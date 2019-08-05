;;; funcs.el --- react layer funcs file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Muneeb Shaikh <muneeb@reversehack.in>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; Backend
(defun spacemacs//react-setup-backend ()
  "Conditionally setup react backend."
  (pcase javascript-backend
    (`tern (spacemacs/tern-setup-tern))
    (`lsp (spacemacs//react-setup-lsp))))

(defun spacemacs//react-setup-company ()
  "Conditionally setup company based on backend."
  (pcase javascript-backend
    (`tern (spacemacs/tern-setup-tern-company 'rjsx-mode))
    (`lsp (spacemacs//react-setup-lsp-company))))

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
        (when (not javascript-lsp-linter)
          (setq-local lsp-prefer-flymake :none))
        (lsp))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun spacemacs//react-setup-lsp-company ()
  "Setup lsp auto-completion."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (spacemacs|add-company-backends
          :backends company-lsp
          :modes rjsx-mode
          :variables company-minimum-prefix-length 2
          :append-hooks nil
          :call-hooks t)
        (company-mode))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))


;; Emmet
(defun spacemacs/react-emmet-mode ()
  "Activate `emmet-mode' and configure it for local buffer."
  (emmet-mode)
  (setq-local emmet-expand-jsx-className? t))


;; Others
(defun spacemacs//react-inside-string-q ()
  "Returns non-nil if inside string, else nil.
Result depends on syntax table's string quote character."
  (let ((result (nth 3 (syntax-ppss))))
    result))

(defun spacemacs//react-inside-comment-q ()
  "Returns non-nil if inside comment, else nil.
Result depends on syntax table's comment character."
  (let ((result (nth 4 (syntax-ppss))))
    result))

(defun spacemacs//react-inside-string-or-comment-q ()
  "Return non-nil if point is inside string, documentation string or a comment.
If optional argument P is present, test this instead of point."
  (or (spacemacs//react-inside-string-q)
      (spacemacs//react-inside-comment-q)))

(defun spacemacs//react-setup-yasnippet ()
  (yas-activate-extra-mode 'js-mode))
