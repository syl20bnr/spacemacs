;;; funcs.el --- TypeScript  Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3


;; backend

(defun space-macs//typescript-backend ()
  "Returns selected backend."
  (if typescript-backend
      typescript-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'tide))))

(defun space-macs//typescript-setup-backend ()
  "Conditionally setup typescript backend."
  (pcase (space-macs//typescript-backend)
    (`tide (space-macs//tide-setup))
    (`lsp (space-macs//typescript-setup-lsp))))

(defun space-macs//typescript-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (space-macs//typescript-backend)
    (`tide (space-macs//tide-setup-company 'typescript-mode 'typescript-tsx-mode))))

(defun space-macs//typescript-setup-eldoc ()
  "Conditionally setup eldoc based on backend."
  (pcase (space-macs//typescript-backend)
    (`tide (space-macs//tide-setup-eldoc))
    (`lsp (space-macs//typescript-setup-lsp-eldoc))))


;; lsp

(defun space-macs//typescript-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (when (not typescript-lsp-linter)
          (setq-local lsp-diagnostics-provider :none))
        (lsp))
    (message (concat "`lsp' layer is not installed, "
                     "please add `lsp' layer to your dotfile."))))

(defun space-macs//typescript-setup-lsp-eldoc ()
  "Setup eldoc for LSP."
  (eldoc-mode))


;; Emmet

(defun space-macs/typescript-emmet-mode ()
  "Configure `emmet-mode' for local buffer."
  (setq-local emmet-expand-jsx-className? t))


;; Others

(defun space-macs/typescript-tsfmt-format-buffer ()
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

(defun space-macs/typescript-format ()
  "Call formatting tool specified in `typescript-fmt-tool'."
  (interactive)
  (cond
   ((eq typescript-fmt-tool 'typescript-formatter)
    (call-interactively 'space-macs/typescript-tsfmt-format-buffer))
   ((eq typescript-fmt-tool 'tide)
    (call-interactively 'tide-format))
   ((eq typescript-fmt-tool 'prettier)
    (call-interactively 'prettier-js))
   (t (error (concat "%s isn't valid typescript-fmt-tool value."
                     " It should be 'tide, 'typescript-formatter or 'prettier.")
                     (symbol-name typescript-fmt-tool)))))

(defun space-macs/typescript-fmt-before-save-hook ()
  (add-hook 'before-save-hook 'space-macs/typescript-format t t))

(defun space-macs/typescript-open-region-in-playground (start end)
  "Open selected region in http://www.typescriptlang.org/Playground
                 If nothing is selected - open the whole current buffer."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point-min) (point-max))))
  (browse-url (concat "http://www.typescriptlang.org/Playground#src="
                      (url-hexify-string (buffer-substring-no-properties start end)))))

(defun space-macs/typescript-yasnippet-setup ()
  (yas-activate-extra-mode 'js-mode))

(defun space-macs/typescript-jump-to-type-def ()
  (interactive)
  (tide-jump-to-definition))

(defun space-macs/typescript-safe-local-variables (values)
  ;; safe values for backend to be used in directory file variables
  (dolist (value values)
    (add-to-list 'safe-local-variable-values
                 (cons 'typescript-backend value))))

(defun space-macs//typescript-setup-checkers ()
  (when-let* ((found (executable-find "eslint_d")))
    (set (make-local-variable 'flycheck-javascript-eslint-executable) found)))


