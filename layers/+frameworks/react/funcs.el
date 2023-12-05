;;; funcs.el --- react layer funcs file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Muneeb Shaikh <muneeb@reversehack.in>
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



;; Backend
(defun spacemacs//react-setup-backend ()
  "Conditionally setup react backend."
  (pcase javascript-backend
    ('tern (spacemacs/tern-setup-tern))
    ('tide (spacemacs//tide-setup))
    ('lsp (spacemacs//react-setup-lsp))))

(defun spacemacs//react-setup-company ()
  "Conditionally setup company based on backend."
  (when (eq javascript-backend 'tide)
    (spacemacs//tide-setup-company 'rjsx-mode)))

(defun spacemacs//react-setup-next-error-fn ()
  "If the `syntax-checking' layer is enabled, disable `rjsx-mode''s
`next-error-function', and let `flycheck' handle any errors."
  (when (configuration-layer/layer-used-p 'syntax-checking)
    (setq-local next-error-function nil)))

;; LSP
(defun spacemacs//react-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (unless javascript-lsp-linter
          (setq-local lsp-diagnostics-provider :none))
        (lsp-deferred))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))


;; Emmet
(defun spacemacs/react-emmet-mode ()
  "Activate `emmet-mode' and configure it for local buffer."
  (emmet-mode)
  (setq-local emmet-expand-jsx-className? t))


;; Others
(defun spacemacs//javascript-jsx-file-p ()
  "Enable rjsx mode by using magic-mode-alist."
  (when buffer-file-name
    (and (member (file-name-extension buffer-file-name) '("js" "jsx"))
         (re-search-forward "\\(^\\s-*import React\\|\\( from \\|require(\\)[\"']react\\)"
                            magic-mode-regexp-match-limit t)
         (save-excursion
           (goto-char (match-beginning 1))
           (let ((sexp (syntax-ppss)))
             ;; not inside string or comment
             (not (or (nth 3 sexp)
                      (nth 4 sexp))))))))

(defun spacemacs//react-setup-yasnippet ()
  (yas-activate-extra-mode 'js-mode))

;; Format
(defun spacemacs//react-fmt-before-save-hook ()
  (add-hook 'before-save-hook 'spacemacs/javascript-format t t))
