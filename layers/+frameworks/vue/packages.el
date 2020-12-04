;;; packages.el --- vue layer packages file for Space-macs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3


(defconst vue-packages
  '(web-mode
    add-node-modules-path
    company
    evil-matchit
    flycheck
    prettier-js
    smartparens
    yasnippet))

(defun vue/post-init-web-mode ()
  (define-derived-mode vue-mode web-mode "Vue")
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode))
  (space-macs/add-to-hook 'vue-mode-hook '(space-macs//vue-setup-editor-style
                                          space-macs//vue-setup-keybindings))
  (add-hook 'vue-mode-local-vars-hook #'space-macs//vue-setup-backend)
  (space-macs//vue-setup-transient-state))

(defun vue/post-init-add-node-modules-path ()
  (add-hook 'vue-mode-hook #'add-node-modules-path))

(defun vue/post-init-company ()
  (add-hook 'vue-mode-local-vars-hook #'space-macs//vue-setup-company))

(defun vue/post-init-evil-matchit ()
  (evilmi-load-plugin-rules '(vue-mode) '(template simple html))
  (add-hook 'vue-mode-hook 'turn-on-evil-matchit-mode))

(defun vue/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'vue-mode))
  (space-macs/enable-flycheck 'vue-mode)
  (add-hook 'vue-mode-hook #'space-macs//javascript-setup-checkers 'append))

(defun vue/pre-init-prettier-js ()
  (add-to-list 'space-macs--prettier-modes 'vue-mode))

(defun vue/post-init-smartparens ()
  (if dotspace-macs-smartparens-strict-mode
      (add-hook 'vue-mode-hook #'smartparens-strict-mode)
    (add-hook 'vue-mode-hook #'smartparens-mode)))

(defun vue/post-init-yasnippet ()
  (add-hook 'vue-mode-hook #'space-macs//vue-setup-yasnippet))


