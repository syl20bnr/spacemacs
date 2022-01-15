;;; packages.el --- svelte layer packages file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Marco Süß <msuess@mailbox.org>
;; URL: https://github.com/msuess
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

(defconst svelte-packages
  '(web-mode
    add-node-modules-path
    company
    evil-matchit
    flycheck
    prettier-js
    smartparens
    yasnippet))

(defun svelte/post-init-web-mode ()
  (define-derived-mode svelte-mode web-mode "Svelte")
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . svelte-mode))
  (spacemacs/add-to-hook 'svelte-mode-hook '(spacemacs//svelte-setup-editor-style
                                             spacemacs//svelte-setup-keybindings))
  (add-hook 'svelte-mode-local-vars-hook #'spacemacs//svelte-setup-backend)
  (spacemacs//svelte-setup-transient-state))

(defun svelte/post-init-add-node-modules-path ()
  (add-hook 'svelte-mode-hook #'add-node-modules-path))

(defun svelte/post-init-company ()
  (add-hook 'svelte-mode-local-vars-hook #'spacemacs//svelte-setup-company))

(defun svelte/post-init-evil-matchit ()
  (evilmi-load-plugin-rules '(svelte-mode) '(template simple html))
  (add-hook 'svelte-mode-hook 'turn-on-evil-matchit-mode))

(defun svelte/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (flycheck-add-mode 'javascript-eslint 'svelte-mode))
  (spacemacs/enable-flycheck 'svelte-mode)
  (add-hook 'svelte-mode-hook #'spacemacs//javascript-setup-checkers 'append))

(defun svelte/pre-init-prettier-js ()
  (add-to-list 'spacemacs--prettier-modes 'svelte-mode))

(defun svelte/post-init-smartparens ()
  (add-hook 'svelte-mode-hook #'spacemacs//activate-smartparens))

(defun svelte/post-init-yasnippet ()
  (add-hook 'svelte-mode-hook #'spacemacs//svelte-setup-yasnippet))
