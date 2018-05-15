;;; funcs.el --- C/C++ Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//c-toggle-auto-newline ()
  "Toggle auto-newline."
  (c-toggle-auto-newline 1))


;; clang

(defun spacemacs/clang-format-function (&optional style)
  "Format the current function with clang-format according to STYLE."
  (interactive)
  (save-excursion
    (c-mark-function)
    (clang-format (region-beginning) (region-end) style)
    (deactivate-mark) ; If the function is already formatted, then remove the mark
    (message "Formatted function %s" (c-defun-name))))

(defun spacemacs/clang-format-region-or-buffer (&optional style)
  "Format the current region or buffer with clang-format according to STYLE."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (clang-format-region (region-beginning) (region-end) style)
          (message "Formatted region"))
      (progn
        (clang-format-buffer style)
        (message "Formatted buffer %s" (buffer-name))))))

(defun spacemacs//clang-format-on-save ()
  "Format the current buffer with clang-format on save when
`c-c++-enable-clang-format-on-save' is non-nil."
  (when c-c++-enable-clang-format-on-save
    (spacemacs/clang-format-region-or-buffer)))

(defun spacemacs/clang-format-on-save ()
  "Add before-save hook for clang-format."
  (add-hook 'before-save-hook 'spacemacs//clang-format-on-save nil t))

(defun spacemacs/company-more-than-prefix-guesser ()
  (spacemacs/c-c++-load-clang-args)
  (company-clang-guess-prefix))

;; Based on the Sarcasm/irony-mode compilation database code.
(defun spacemacs/company-find-clang-complete-file ()
  (when buffer-file-name
    (let ((dir (locate-dominating-file buffer-file-name ".clang_complete")))
      (when dir
        (concat (file-name-as-directory dir) ".clang_complete")))))

;; Based on the Sarcasm/irony-mode compilation database code.
(defun spacemacs/company-load-clang-complete-file (cc-file)
  "Load the flags from CC-FILE, one flag per line."
  (let ((invocation-dir (expand-file-name (file-name-directory cc-file)))
        (case-fold-search nil)
        (include-regex "\\(-I\\|-isystem\\|-iquote\\|-idirafter\\)\\s-*\\(\\S-+\\)")
        compile-flags)
    (with-temp-buffer
      (insert-file-contents cc-file)
      ;; Replace relative paths with absolute paths (by @trishume)
      ;; (goto-char (point-min))
      (while (re-search-forward include-regex nil t)
        (replace-match (format "%s%s" (match-string 1)
                               (expand-file-name (match-string 2)
                                                 invocation-dir))))
      ;; Turn lines into a list
      (setq compile-flags
            ;; remove whitespaces at the end of each line, if any
            (mapcar #'(lambda (line)
                        (if (string-match "[ \t]+$" line)
                            (replace-match "" t t line)
                          line))
                    (split-string (buffer-string) "\n" t))))
    compile-flags))

(defun spacemacs//c-c++-get-standard-include-paths (lang)
  "Returns the default system header include paths for LANG if gcc is on the
system and supports it, else returns a default set of include paths."
  (let* ((start "#include <...> search starts here:")
         (end "End of search list.")
         (gcc-tmplt "echo | gcc -x%s -E -v - 2>&1")
         (sed-tmplt " | sed -n '/%s/,/%s/{/%s/b;/%s/b;p}' | sed -e 's/^ *//g'")
         (template (concat gcc-tmplt sed-tmplt))
         (inc-dirs-cmd (format template lang start end start end))
         (inc-dirs (split-string (shell-command-to-string inc-dirs-cmd)
                                 "\n" t)))
    (if (and inc-dirs (every 'file-exists-p inc-dirs))
        inc-dirs
      '("/usr/include" "/usr/local/include"))))

(defun spacemacs//filter-and-substring (flags filter-prefix substr-index)
  "Returns all the strings in FLAGS starting with FILTER-PREFIX. The returned
strings are substringed from SUBSTR-INDEX inclusive to the end of the string."
  (mapcar (lambda (f) (substring f substr-index))
          (remove-if-not (lambda (f) (string-prefix-p filter-prefix f))
                         flags)))

(defun spacemacs/c-c++-load-clang-args ()
  "Sets the arguments for company-clang, the system paths for company-c-headers
and the arguments for flyckeck-clang based on a project-specific text file."
  (unless company-clang-arguments
    (let* ((cc-file (spacemacs/company-find-clang-complete-file))
           (flags (if cc-file
                      (spacemacs/company-load-clang-complete-file cc-file)
                    '()))
           (i-paths (spacemacs//filter-and-substring flags
                                                     "-I" 2))
           (iquote-paths (spacemacs//filter-and-substring flags
                                                          "-iquote" 7))
           (isystem-paths (spacemacs//filter-and-substring flags
                                                           "-isystem" 8))
           (idirafter-paths (spacemacs//filter-and-substring flags
                                                             "-idirafter" 10)))
      (setq-local company-clang-arguments flags)
      (setq-local flycheck-clang-args flags)
      (setq-local company-c-headers-path-user
                  (append '(".")
                          iquote-paths))
      (setq-local company-c-headers-path-system
                  (append i-paths
                          isystem-paths
                          (when (string-equal major-mode "c++-mode")
                            (spacemacs//c-c++-get-standard-include-paths "c++"))
                          (when (string-equal major-mode "c-mode")
                            (spacemacs//c-c++-get-standard-include-paths "c"))
                          idirafter-paths)))))


;; rtags

(defun spacemacs/c-c++-use-rtags (&optional useFileManager)
  "Return non-nil if rtags function should be used."
  ;; this function is used to fallback on gtags function if rtags is not
  ;; supported. So if gtags layer is not used we disable the fallback by
  ;; returning always t.
  (or (not (configuration-layer/layer-used-p 'gtags))
      (and (rtags-executable-find "rc")
           (cond ((not (gtags-get-rootpath)) t)
                 ((and (not (eq major-mode 'c++-mode))
                       (not (eq major-mode 'c-mode))) (rtags-has-filemanager))
                 (useFileManager (rtags-has-filemanager))
                 (t (rtags-is-indexed))))))

(defun spacemacs/c-c++-tags-find-symbol-at-point (&optional prefix)
  (interactive "P")
  (if (and (not (rtags-find-symbol-at-point prefix))
           rtags-last-request-not-indexed)
      (gtags-find-tag)))

(defun spacemacs/c-c++-tags-find-references-at-point (&optional prefix)
  (interactive "P")
  (if (and (not (rtags-find-references-at-point prefix))
           rtags-last-request-not-indexed)
      (gtags-find-rtag)))

(defun spacemacs/c-c++-tags-find-symbol ()
  (interactive)
  (call-interactively (if (spacemacs/c-c++-use-rtags)
                          'rtags-find-symbol 'gtags-find-symbol)))

(defun spacemacs/c-c++-tags-find-references ()
  (interactive)
  (call-interactively (if (spacemacs/c-c++-use-rtags)
                          'rtags-find-references 'gtags-find-rtag)))

(defun spacemacs/c-c++-tags-find-file ()
  (interactive)
  (call-interactively (if (spacemacs/c-c++-use-rtags t)
                          'rtags-find-file 'gtags-find-file)))

(defun spacemacs/c-c++-tags-imenu ()
  (interactive)
  (call-interactively (if (spacemacs/c-c++-use-rtags t)
                          'rtags-imenu 'idomenu)))
