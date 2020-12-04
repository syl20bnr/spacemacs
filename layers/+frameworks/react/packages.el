;;; packages.el --- react layer packages file for Space-macs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Andrea Moretti <axyzxp@gmail.com>
;; URL: https://github.com/axyz
;;
;; This file is not part of GNU e-macs.
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
  (add-hook 'rjsx-mode-local-vars-hook #'space-macs//react-setup-company))

(defun react/post-init-emmet-mode ()
  (add-hook 'rjsx-mode-hook 'space-macs/react-emmet-mode))

(defun react/post-init-evil-matchit ()
  (add-hook 'rjsx-mode-hook 'turn-on-evil-matchit-mode))

(defun react/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (dolist (checker '(javascript-eslint javascript-standard))
      (flycheck-add-mode checker 'rjsx-mode)))
  (space-macs/enable-flycheck 'rjsx-mode)
  (add-hook 'rjsx-mode-hook #'space-macs//javascript-setup-checkers 'append))

(defun react/pre-init-import-js ()
  (when (eq javascript-import-tool 'import-js)
    (add-to-list 'space-macs--import-js-modes (cons 'rjsx-mode 'rjsx-mode-hook))))

(defun react/post-init-js-doc ()
  (add-hook 'rjsx-mode-hook 'space-macs/js-doc-require)
  (space-macs/js-doc-set-key-bindings 'rjsx-mode))

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
                  (not (space-macs//react-inside-string-or-comment-q)))))

    (add-to-list 'magic-mode-alist (cons #'+javascript-jsx-file-p 'rjsx-mode))

    ;; setup rjsx backend
    (add-hook 'rjsx-mode-local-vars-hook #'space-macs//react-setup-backend)
    ;; set next-error-function to nil because we use flycheck
    (add-hook 'rjsx-mode-local-vars-hook #'space-macs//react-setup-next-error-fn)
    ;; setup fmt on save
    (when javascript-fmt-on-save
      (add-hook 'rjsx-mode-local-vars-hook #'space-macs//react-fmt-before-save-hook))

    ;; set the javascript layers keymap as parent to the react layers keymap
    (set-keymap-parent space-macs-rjsx-mode-map space-macs-js2-mode-map)

    :config
    ;; declare prefixes
    (space-macs/declare-prefix-for-mode 'rjsx-mode "m="  "format")
    (space-macs/declare-prefix-for-mode 'rjsx-mode "mT"  "toggle")
    (space-macs/declare-prefix-for-mode 'rjsx-mode "me"  "eval")
    (space-macs/declare-prefix-for-mode 'rjsx-mode "mg"  "goto")
    (space-macs/declare-prefix-for-mode 'rjsx-mode "mh"  "documentation")
    (space-macs/declare-prefix-for-mode 'rjsx-mode "mr"  "refactor")
    (space-macs/declare-prefix-for-mode 'rjsx-mode "mr3" "ternary")
    (space-macs/declare-prefix-for-mode 'rjsx-mode "mra" "add/args")
    (space-macs/declare-prefix-for-mode 'rjsx-mode "mrb" "barf")
    (space-macs/declare-prefix-for-mode 'rjsx-mode "mrc" "contract")
    (space-macs/declare-prefix-for-mode 'rjsx-mode "mre" "expand/extract")
    (space-macs/declare-prefix-for-mode 'rjsx-mode "mri" "inline/inject/introduct")
    (space-macs/declare-prefix-for-mode 'rjsx-mode "mrl" "localize/log")
    (space-macs/declare-prefix-for-mode 'rjsx-mode "mrr" "rename")
    (space-macs/declare-prefix-for-mode 'rjsx-mode "mrs" "split/slurp")
    (space-macs/declare-prefix-for-mode 'rjsx-mode "mrt" "toggle")
    (space-macs/declare-prefix-for-mode 'rjsx-mode "mru" "unwrap")
    (space-macs/declare-prefix-for-mode 'rjsx-mode "mrv" "var")
    (space-macs/declare-prefix-for-mode 'rjsx-mode "mrw" "wrap")
    (space-macs/declare-prefix-for-mode 'rjsx-mode "ms"  "skewer")
    (space-macs/declare-prefix-for-mode 'rjsx-mode "mx"  "text")
    (space-macs/declare-prefix-for-mode 'rjsx-mode "mxm" "move")
    (space-macs/declare-prefix-for-mode 'rjsx-mode "mz"  "folding")

    (space-macs/set-leader-keys-for-major-mode 'rjsx-mode "rlt" 'js2r-log-this)
    (space-macs/set-leader-keys-for-major-mode 'rjsx-mode "rt" 'rjsx-rename-tag-at-point)

    (with-eval-after-load 'rjsx-mode
      (define-key rjsx-mode-map (kbd "C-d") nil))))

(defun react/pre-init-prettier-js ()
  (when (eq javascript-fmt-tool 'prettier)
    (add-to-list 'space-macs--prettier-modes 'rjsx-mode)))

(defun react/post-init-smartparens ()
  (if dotspace-macs-smartparens-strict-mode
      (add-hook 'rjsx-mode-hook #'smartparens-strict-mode)
    (add-hook 'rjsx-mode-hook #'smartparens-mode)))

(defun react/post-init-tern ()
  (add-to-list 'tern--key-bindings-modes 'rjsx-mode))

(defun react/pre-init-web-beautify ()
  (when (eq javascript-fmt-tool 'web-beautify)
    (add-to-list 'space-macs--web-beautify-modes
                 (cons 'rjsx-mode 'web-beautify-js))))

(defun react/post-init-yasnippet ()
  (add-hook 'rjsx-mode-hook #'space-macs//react-setup-yasnippet))


