;;; packages.el --- react layer packages file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Andrea Moretti <axyzxp@gmail.com>
;; URL: https://github.com/axyz
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst react-packages
  '(
    add-node-modules-path
    company
    emmet-mode
    evil-matchit
    flycheck
    import-js
    js-doc
    prettier-js
    rjsx-mode
    smartparens
    tern
    web-beautify
    yasnippet
    ))

(defun react/post-init-add-node-modules-path ()
  (add-hook 'rjsx-mode-hook #'add-node-modules-path))

(defun react/post-init-company ()
  (add-hook 'rjsx-mode-local-vars-hook #'spacemacs//react-setup-company))

(defun react/post-init-emmet-mode ()
  (add-hook 'rjsx-mode-hook 'spacemacs/react-emmet-mode))

(defun react/post-init-evil-matchit ()
  (add-hook 'rjsx-mode-hook 'turn-on-evil-matchit-mode))

(defun react/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (dolist (checker '(javascript-eslint javascript-standard))
      (flycheck-add-mode checker 'rjsx-mode)))
  (spacemacs/enable-flycheck 'rjsx-mode))

(defun react/pre-init-import-js ()
  (when (eq javascript-import-tool 'import-js)
    (add-to-list 'spacemacs--import-js-modes (cons 'rjsx-mode 'rjsx-mode-hook))))

(defun react/post-init-js-doc ()
  (add-hook 'rjsx-mode-hook 'spacemacs/js-doc-require)
  (spacemacs/js-doc-set-key-bindings 'rjsx-mode))

(defun react/init-rjsx-mode ()
  (use-package rjsx-mode
    :defer t
    :init
    ;; enable rjsx mode by using magic-mode-alist
    (defun +javascript-jsx-file-p ()
      (and buffer-file-name
           (or (equal (file-name-extension buffer-file-name) "js")
               (equal (file-name-extension buffer-file-name) "jsx"))
           (re-search-forward "\\(^\\s-*import React\\|\\( from \\|require(\\)[\"']react\\)"
                              magic-mode-regexp-match-limit t)
           (progn (goto-char (match-beginning 1))
                  (not (spacemacs//react-inside-string-or-comment-q)))))

    (add-to-list 'magic-mode-alist (cons #'+javascript-jsx-file-p 'rjsx-mode))

    ;; setup rjsx backend
    (add-hook 'rjsx-mode-local-vars-hook #'spacemacs//react-setup-backend)
    ;; set next-error-function to nil because we use flycheck
    (add-hook 'rjsx-mode-local-vars-hook #'spacemacs//react-setup-next-error-fn)

    :config
    ;; declare prefix
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mr" "refactor")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mrr" "rename")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mh" "documentation")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mg" "goto")

    (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "rt" 'rjsx-rename-tag-at-point)

    (with-eval-after-load 'rjsx-mode
      (define-key rjsx-mode-map (kbd "C-d") nil))))

(defun react/pre-init-prettier-js ()
  (when (eq javascript-fmt-tool 'prettier)
    (add-to-list 'spacemacs--prettier-modes 'rjsx-mode)))

(defun react/post-init-smartparens ()
  (if dotspacemacs-smartparens-strict-mode
      (add-hook 'react-mode-hook #'smartparens-strict-mode)
    (add-hook 'react-mode-hook #'smartparens-mode)))

(defun react/post-init-tern ()
  (add-to-list 'tern--key-bindings-modes 'rjsx-mode))

(defun react/pre-init-web-beautify ()
  (when (eq javascript-fmt-tool 'web-beautify)
    (add-to-list 'spacemacs--web-beautify-modes
                 (cons 'rjsx-mode 'web-beautify-js))))

(defun react/post-init-yasnippet ()
  (add-hook 'rjsx-mode-hook #'spacemacs//react-setup-yasnippet))
