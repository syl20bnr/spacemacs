;;; funcs.el --- TypeScript  Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.



;; backend

(defun spacemacs//typescript-setup-backend ()
  "Conditionally setup typescript backend."
  (pcase typescript-backend
    ('tide (spacemacs//tide-setup))
    ('lsp (spacemacs//typescript-setup-lsp))))

(defun spacemacs//typescript-setup-company ()
  "Conditionally setup company based on backend."
  (when (eq typescript-backend 'tide)
    (spacemacs//tide-setup-company 'typescript-mode 'typescript-tsx-mode)))

(defun spacemacs//typescript-setup-eldoc ()
  "Conditionally setup eldoc based on backend."
  (pcase typescript-backend
    ('tide (spacemacs//tide-setup-eldoc))
    ('lsp (spacemacs//typescript-setup-lsp-eldoc))))


;; lsp

(defun spacemacs//typescript-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (unless typescript-lsp-linter
          (setq-local lsp-diagnostics-provider :none))
        (lsp-deferred))
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
  (call-interactively
   (pcase typescript-fmt-tool
     ('typescript-formatter 'spacemacs/typescript-tsfmt-format-buffer)
     ('tide 'tide-format)
     ('prettier 'prettier-js)
     (_ (user-error
         "%s isn't a valid typescript formatter. Possible values are 'tide, 'typescript-formatter or 'prettier"
         typescript-fmt-tool)))))

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
    (setq-local flycheck-javascript-eslint-executable found)))

(defun spacemacs/typescript-mode-init (hook)
  (add-hook hook 'spacemacs//typescript-setup-backend)
  (when typescript-fmt-on-save
    (add-hook hook 'spacemacs/typescript-fmt-before-save-hook)))

(defun spacemacs/typescript-mode-config (mode)
  (spacemacs/set-leader-keys-for-major-mode mode
    "p" 'spacemacs/typescript-open-region-in-playground)
  (pcase typescript-backend
    ('lsp (spacemacs/set-leader-keys-for-major-mode mode
            "==" 'spacemacs/typescript-format))
    ('tide (spacemacs/set-leader-keys-for-major-mode mode
             "=" 'spacemacs/typescript-format))))
