;;; packages.el --- vue layer packages file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg
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
