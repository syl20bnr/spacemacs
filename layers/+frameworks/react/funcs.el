;;; funcs.el --- react layer funcs file for Space-macs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Muneeb Shaikh <muneeb@reversehack.in>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3


;; Backend
(defun space-macs//react-setup-backend ()
  "Conditionally setup react backend."
  (pcase javascript-backend
    (`tern (space-macs/tern-setup-tern))
    (`tide (space-macs//tide-setup))
    (`lsp (space-macs//react-setup-lsp))))

(defun space-macs//react-setup-company ()
  "Conditionally setup company based on backend."
  (pcase javascript-backend
    (`tide (space-macs//tide-setup-company 'rjsx-mode))))

(defun space-macs//react-setup-next-error-fn ()
  "If the `syntax-checking' layer is enabled, disable `rjsx-mode''s
`next-error-function', and let `flycheck' handle any errors."
  (when (configuration-layer/layer-used-p 'syntax-checking)
    (setq-local next-error-function nil)))

;; LSP
(defun space-macs//react-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (when (not javascript-lsp-linter)
          (setq-local lsp-diagnostics-provider :none))
        (lsp))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))


;; Emmet
(defun space-macs/react-emmet-mode ()
  "Activate `emmet-mode' and configure it for local buffer."
  (emmet-mode)
  (setq-local emmet-expand-jsx-className? t))


;; Others
(defun space-macs//react-inside-string-q ()
  "Returns non-nil if inside string, else nil.
Result depends on syntax table's string quote character."
  (let ((result (nth 3 (syntax-ppss))))
    result))

(defun space-macs//react-inside-comment-q ()
  "Returns non-nil if inside comment, else nil.
Result depends on syntax table's comment character."
  (let ((result (nth 4 (syntax-ppss))))
    result))

(defun space-macs//react-inside-string-or-comment-q ()
  "Return non-nil if point is inside string, documentation string or a comment.
If optional argument P is present, test this instead of point."
  (or (space-macs//react-inside-string-q)
      (space-macs//react-inside-comment-q)))

(defun space-macs//react-setup-yasnippet ()
  (yas-activate-extra-mode 'js-mode))

;; Format
(defun space-macs//react-fmt-before-save-hook ()
  (add-hook 'before-save-hook 'space-macs/javascript-format t t))


