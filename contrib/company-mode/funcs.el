; .clang_complete file loading
; Sets the arguments for company-clang based on a project-specific text file.

; START Based on the Sarcasm/irony-mode compilation database code.

(defun company-mode/find-clang-complete-file ()
  (when buffer-file-name
    (let ((dir (locate-dominating-file buffer-file-name ".clang_complete")))
      (when dir
        (concat (file-name-as-directory dir) ".clang_complete")))))

(defun company-mode/load-clang-complete-file (cc-file)
  "Load the flags from CC-FILE, one flag per line."
  (let ((invocation-dir (expand-file-name (file-name-directory cc-file)))
        (case-fold-search nil)
        compile-flags)
    (with-temp-buffer
      (insert-file-contents cc-file)
      ; Replace relative paths with absolute paths (by @trishume)
      ;(goto-char (point-min))
      (while (re-search-forward "\\(-I\\|-isystem\n\\)\\(\\S-\\)" nil t)
        (replace-match (format "%s%s" (match-string 1) (expand-file-name (match-string 2) invocation-dir))))
      ; Turn lines into a list
      (setq compile-flags
            ;; remove whitespaces at the end of each line, if any
            (mapcar #'(lambda (line)
                        (if (string-match "[ \t]+$" line)
                            (replace-match "" t t line)
                          line))
                    (split-string (buffer-string) "\n" t))))
    compile-flags))

; END Back to things written by @trishume

(defun company-mode/more-than-prefix-guesser ()
  (unless company-clang-arguments
    (let* ((cc-file (company-mode/find-clang-complete-file))
           (flags (when cc-file (company-mode/load-clang-complete-file cc-file))))
      (when flags (setq-local company-clang-arguments flags))))
  (company-clang-guess-prefix))

; START Based on the built in flycheck-clang c++ checker
(eval-after-load 'flycheck
  '(progn
     (flycheck-define-checker c/c++-company
       "A C/C++ syntax checker using parameters from clang-complete"
       :command ("clang"
                 "-fsyntax-only"
                 "-fno-color-diagnostics"    ; Do not include color codes in output
                 "-fno-caret-diagnostics"    ; Do not visually indicate the source
                                        ; location
                 "-fno-diagnostics-show-option" ; Do not show the corresponding
                                        ; warning group
                 "-x" (eval
                       (pcase major-mode
                         (`c++-mode "c++")
                         (`c-mode "c")))
                 (option-list "" company-clang-arguments concat)
                 source)
       :error-patterns
       ((error line-start
               (message "In file included from") " " (file-name) ":" line ":"
               line-end)
        (info line-start (file-name) ":" line ":" column
              ": note: " (optional (message)) line-end)
        (warning line-start (file-name) ":" line ":" column
                 ": warning: " (optional (message)) line-end)
        (error line-start (file-name) ":" line ":" column
               ": " (or "fatal error" "error") ": " (optional (message)) line-end))
       :error-filter
       (lambda (errors)
         (let ((errors (flycheck-sanitize-errors errors)))
           (dolist (err errors)
             ;; Clang will output empty messages for #error/#warning pragmas without
             ;; messages.  We fill these empty errors with a dummy message to get
             ;; them past our error filtering
             (setf (flycheck-error-message err)
                   (or (flycheck-error-message err) "no message")))
           (flycheck-fold-include-levels errors "In file included from")))
       :modes (c-mode c++-mode)
       :next-checkers ((warning . c/c++-cppcheck)))
     (add-to-list 'flycheck-checkers 'c/c++-company)))
; END
