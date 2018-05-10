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

(defun spacemacs//setup-rjsx-mode ()
  "Adjust yas and emmet to accommodate rjsx-mode"
  (emmet-mode 0)
  ;; See https://github.com/CestDiego/emmet-mode/commit/3f2904196e856d31b9c95794d2682c4c7365db23
  (setq-local emmet-expand-jsx-className? t)
  ;; Enable js-mode snippets
  (yas-activate-extra-mode 'js-mode)
  ;; See https://github.com/syl20bnr/spacemacs/issues/8222
  (set (make-local-variable 'company-minimum-prefix-length) 2))


;; Backend
(defun spacemacs//react-setup-backend ()
  "Conditionally setup react backend."
  (pcase javascript-backend
    (`tern(spacemacs//react-setup-tern))
    (`lsp (spacemacs//react-setup-lsp))))

(defun spacemacs//react-setup-company ()
  "Conditionally setup company based on backend."
  (pcase javascript-backend
    (`tern (spacemacs//react-setup-tern-company))
    (`lsp (spacemacs//react-setup-lsp-company))))


;; Tern
(defun spacemacs//react-setup-tern ()
  "Setup tern backend."
  (add-hook 'rjsx-mode-hook 'tern-mode)
  (spacemacs//set-tern-key-bindings 'rjsx-mode))

(defun spacemacs//react-setup-tern-company ()
  "Setup tern auto-completion."
  (spacemacs|add-company-backends
    :backends company-tern
    :modes rjsx-mode)
  (company-mode))


;; LSP
(defun spacemacs//react-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (add-hook 'rjsx-mode #'lsp-javascript-typescript-enable)
        (require 'lsp-javascript-flow)
        (add-hook 'rjsx-mode #'lsp-javascript-flow-enable)
        (require 'lsp-typescript)
        (add-hook 'rjsx-mode #'lsp-typescript-enable)
        (spacemacs//setup-lsp-jump-handler 'rjsx-mode))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dofile.")))

(defun spacemacs//react-setup-lsp-company ()
  "Setup lsp auto-completion."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (spacemacs|add-company-backends
          :backends company-lsp
          :modes rjsx-mode)
        (company-mode))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dofile.")))


;; Others
(defun inside-string-q ()
  "Returns non-nil if inside string, else nil.
Result depends on syntax table's string quote character."
  (let ((result (nth 3 (syntax-ppss))))
    result))

(defun inside-comment-q ()
  "Returns non-nil if inside comment, else nil.
Result depends on syntax table's comment character."
  (let ((result (nth 4 (syntax-ppss))))
    result))

(defun inside-string-or-comment-q ()
  "Return non-nil if point is inside string, documentation string or a comment.
If optional argument P is present, test this instead of point."
  (or (inside-string-q)
      (inside-comment-q)))
