;;; packages.el --- react layer packages file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Andrea Moretti <axyzxp@gmail.com>
;; URL: https://github.com/axyz
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
    yasnippet))


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
  (spacemacs/enable-flycheck 'rjsx-mode)
  (add-hook 'rjsx-mode-hook #'spacemacs//javascript-setup-checkers 'append))

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

    (add-to-list 'magic-mode-alist (cons #'spacemacs//javascript-jsx-file-p 'rjsx-mode))

    ;; setup rjsx backend
    (add-hook 'rjsx-mode-local-vars-hook #'spacemacs//react-setup-backend)
    ;; set next-error-function to nil because we use flycheck
    (add-hook 'rjsx-mode-local-vars-hook #'spacemacs//react-setup-next-error-fn)
    ;; setup fmt on save
    (when javascript-fmt-on-save
      (add-hook 'rjsx-mode-local-vars-hook #'spacemacs//react-fmt-before-save-hook))

    ;; set the javascript layers keymap as parent to the react layers keymap
    (set-keymap-parent spacemacs-rjsx-mode-map spacemacs-js2-mode-map)

    :config
    ;; declare prefixes
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "m="  "format")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mT"  "toggle")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "me"  "eval")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mg"  "goto")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mh"  "documentation")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mr"  "refactor")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mr3" "ternary")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mra" "add/args")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mrb" "barf")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mrc" "contract")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mre" "expand/extract")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mri" "inline/inject/introduct")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mrl" "localize/log")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mrr" "rename")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mrs" "split/slurp")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mrt" "toggle")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mru" "unwrap")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mrv" "var")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mrw" "wrap")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "ms"  "skewer")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mx"  "text")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mxm" "move")
    (spacemacs/declare-prefix-for-mode 'rjsx-mode "mz"  "folding")

    (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "rlt" 'js2r-log-this)
    (spacemacs/set-leader-keys-for-major-mode 'rjsx-mode "rt" 'rjsx-rename-tag-at-point)

    (with-eval-after-load 'rjsx-mode
      (define-key rjsx-mode-map (kbd "C-d") nil))))

(defun react/pre-init-prettier-js ()
  (when (eq javascript-fmt-tool 'prettier)
    (add-to-list 'spacemacs--prettier-modes 'rjsx-mode)))

(defun react/post-init-smartparens ()
  (add-hook 'rjsx-mode-hook #'spacemacs//activate-smartparens))

(defun react/post-init-tern ()
  (add-to-list 'tern--key-bindings-modes 'rjsx-mode))

(defun react/pre-init-web-beautify ()
  (when (eq javascript-fmt-tool 'web-beautify)
    (add-to-list 'spacemacs--web-beautify-modes
                 (cons 'rjsx-mode 'web-beautify-js))))

(defun react/post-init-yasnippet ()
  (add-hook 'rjsx-mode-hook #'spacemacs//react-setup-yasnippet))
