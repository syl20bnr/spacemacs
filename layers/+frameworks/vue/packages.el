;;; packages.el --- vue layer packages file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg
;;
;; This file is not part of GNU Emacs.
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
  (spacemacs/add-to-hook 'vue-mode-hook '(spacemacs//vue-setup-editor-style
                                          spacemacs//vue-setup-keybindings))
  (add-hook 'vue-mode-local-vars-hook #'spacemacs//vue-setup-backend)
  (spacemacs//vue-setup-transient-state))

(defun vue/post-init-add-node-modules-path ()
  (add-hook 'vue-mode-hook #'add-node-modules-path))

(defun vue/post-init-company ()
  (add-hook 'vue-mode-local-vars-hook #'spacemacs//vue-setup-company))

(defun vue/post-init-evil-matchit ()
  (evilmi-load-plugin-rules '(vue-mode) '(template simple html))
  (add-hook 'vue-mode-hook 'turn-on-evil-matchit-mode))

(defun vue/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'vue-mode))
  (spacemacs/enable-flycheck 'vue-mode)
  (add-hook 'vue-mode-hook #'spacemacs//javascript-setup-checkers 'append))

(defun vue/pre-init-prettier-js ()
  (add-to-list 'spacemacs--prettier-modes 'vue-mode))

(defun vue/post-init-smartparens ()
  (add-hook 'vue-mode-hook #'spacemacs//activate-smartparens))

(defun vue/post-init-yasnippet ()
  (add-hook 'vue-mode-hook #'spacemacs//vue-setup-yasnippet))
