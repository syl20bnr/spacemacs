;;; funcs.el --- TypeScript  Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; backend

(defun spacemacs//typescript-backend ()
  "Returns selected backend."
  (if typescript-backend
      typescript-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'tide))))

(defun spacemacs//typescript-setup-backend ()
  "Conditionally setup typescript backend."
  (pcase (spacemacs//typescript-backend)
    (`tide (spacemacs//tide-setup))
    (`lsp (spacemacs//typescript-setup-lsp))))

(defun spacemacs//typescript-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (spacemacs//typescript-backend)
    (`tide (spacemacs//tide-setup-company 'typescript-mode 'typescript-tsx-mode))
    (`lsp (spacemacs//typescript-setup-lsp-company))))

(defun spacemacs//typescript-setup-eldoc ()
  "Conditionally setup eldoc based on backend."
  (pcase (spacemacs//typescript-backend)
    (`tide (spacemacs//tide-setup-eldoc))
    (`lsp (spacemacs//typescript-setup-lsp-eldoc))))


;; lsp

(defun spacemacs//typescript-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (when (not typescript-lsp-linter)
          (setq-local lsp-diagnostic-package :none))
        (lsp))
    (message (concat "`lsp' layer is not installed, "
                     "please add `lsp' layer to your dotfile."))))

(defun spacemacs//typescript-setup-lsp-company ()
  "Setup lsp auto-completion."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (spacemacs|add-company-backends
          :backends company-lsp
          :modes typescript-mode typescript-tsx-mode
          :variables company-minimum-prefix-length 2
          :append-hooks nil
          :call-hooks t)
        (company-mode))
    (message (concat "`lsp' layer is not installed, "
                     "please add `lsp' layer to your dotfile."))))

(defun spacemacs//typescript-setup-lsp-eldoc ()
  "Setup eldoc for LSP."
  (eldoc-mode))


;; Emmet

(defun spacemacs/typescript-emmet-mode ()
  "Configure `emmet-mode' for local buffer."
  (setq-local emmet-expand-jsx-className? t))


;; Others

(defun spacemacs/typescript-tsfmt-format-buffer ()
  "Format buffer with tsfmt."
  (interactive)
  (if (executable-find "tsfmt")
      (let*  ((extension (file-name-extension (or buffer-file-name "tmp.ts") t))
              (tmpfile (make-temp-file "~fmt-tmp" nil extension))
              (coding-system-for-read 'utf-8)
              (coding-system-for-write 'utf-8)
              (outputbuf (get-buffer-create "*~fmt-tmp.ts*")))
        (unwind-protect
            (progn
              (with-current-buffer outputbuf (erase-buffer))
              (write-region nil nil tmpfile)
              (if (zerop (apply 'call-process "tsfmt" nil outputbuf nil
                                (list (format
                                       "--baseDir='%s' --"
                                       default-directory)
                                      tmpfile)))
                  (let ((p (point)))
                    (save-excursion
                      (with-current-buffer (current-buffer)
                        (erase-buffer)
                        (insert-buffer-substring outputbuf)))
                    (goto-char p)
                    (message "formatted.")
                    (kill-buffer outputbuf))
                (progn
                  (message "Formatting failed!")
                  (display-buffer outputbuf)))
              (progn
                (delete-file tmpfile)))))
    (error "tsfmt not found. Run \"npm install -g typescript-formatter\"")))

(defun spacemacs/typescript-format ()
  "Call formatting tool specified in `typescript-fmt-tool'."
  (interactive)
  (cond
   ((eq typescript-fmt-tool 'typescript-formatter)
    (call-interactively 'spacemacs/typescript-tsfmt-format-buffer))
   ((eq typescript-fmt-tool 'tide)
    (call-interactively 'tide-format))
   ((eq typescript-fmt-tool 'prettier)
    (call-interactively 'prettier-js))
   (t (error (concat "%s isn't valid typescript-fmt-tool value."
                     " It should be 'tide, 'typescript-formatter or 'prettier.")
                     (symbol-name typescript-fmt-tool)))))

(defun spacemacs/typescript-fmt-before-save-hook ()
  (add-hook 'before-save-hook 'spacemacs/typescript-format t t))

(defun spacemacs/typescript-open-region-in-playground (start end)
  "Open selected region in http://www.typescriptlang.org/Playground
                 If nothing is selected - open the whole current buffer."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (browse-url (concat "http://www.typescriptlang.org/Playground#src="
                      (url-hexify-string (buffer-substring-no-properties start end)))))

(defun spacemacs/typescript-yasnippet-setup ()
  (yas-activate-extra-mode 'js-mode))

(defun spacemacs/typescript-jump-to-type-def ()
  (interactive)
  (tide-jump-to-definition))

(defun spacemacs/typescript-safe-local-variables (values)
  ;; safe values for backend to be used in directory file variables
  (dolist (value values)
    (add-to-list 'safe-local-variable-values
                 (cons 'typescript-backend value))))

(defun spacemacs//typescript-setup-checkers ()
  (when-let* ((found (executable-find "eslint_d")))
    (set (make-local-variable 'flycheck-javascript-eslint-executable) found)))
