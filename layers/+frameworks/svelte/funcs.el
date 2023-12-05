;;; funcs.el --- svelte layer funcs file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
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



;; backend
(defun spacemacs//svelte-setup-company ()
  "Conditionally setup company based on backend."
  (pcase svelte-backend
    ('dumb
     (spacemacs|add-company-backends :backends (company-web-html company-css company-files company-dabbrev)
                                     :modes svelte-mode))
    ('lsp
     (spacemacs|add-company-backends
       :backends company-capf
       :modes svelte-mode))))

(defun spacemacs//svelte-setup-backend ()
  "Conditionally setup svelte backend."
  (pcase svelte-backend
    ('dumb (spacemacs//svelte-setup-dumb))
    ('lsp (spacemacs//svelte-setup-lsp))))


;; lsp
(defun spacemacs//svelte-setup-lsp ()
  "Setup lsp backend."
  ;; error checking from lsp langserver sucks, turn it off
  ;; so eslint won't be overriden
  (setq-local lsp-diagnostics-provider :none)
  (lsp-deferred))


;; dumb
(defun spacemacs//svelte-setup-dumb ()
  (setq imenu-generic-expression '(("html" "^<template>$" 0)
                                   ("js" "^<script>$" 0)
                                   ("js" "^\\s-*\\(data\\).*()\\s-?{" 1)
                                   ("js" "^\\s-*\\(mounted\\).*()\\s-?{" 1)
                                   ("js" "^\\s-*\\(beforeMount\\).*()\\s-?{" 1)
                                   ("js" "^\\s-*\\(beforeDestroy\\).*()\\s-?{" 1)
                                   ("js" "^\\s-*\\(created\\).*()\\s-?{" 1)
                                   ("js" "^\\s-*\\(computed\\):\\s-?{" 1)
                                   ("js" "^\\s-*\\(watch\\):\\s-?{" 1)
                                   ("js" "^\\s-*\\(methods\\):\\s-?{" 1)
                                   ("js" "^\\s-*\\(props\\):\\s-?{" 1)
                                   ("css" "^<css>$" 0))
        imenu-create-index-function #'imenu-default-create-index-function))


;; Others
(defun spacemacs//svelte-setup-yasnippet ()
  (spacemacs/load-yasnippet)
  (yas-activate-extra-mode 'js-mode))

(defun spacemacs//svelte-setup-editor-style ()
  "such as indent rules comment style etc"
  ;; https://stackoverflow.com/a/36725155
  (setq web-mode-script-padding 0)
  (setq web-mode-style-padding 0)
  ;; https://emacs.stackexchange.com/a/27714
  (add-to-list 'web-mode-comment-formats '("javascript" . "//")))

(defun spacemacs//svelte-setup-keybindings ()
  "free stuff from `html' layer"
  (spacemacs/declare-prefix-for-mode 'svelte-mode "m=" "format")
  (spacemacs/declare-prefix-for-mode 'svelte-mode "mE" "errors")
  (spacemacs/declare-prefix-for-mode 'svelte-mode "me" "element")
  (spacemacs/declare-prefix-for-mode 'svelte-mode "mg" "goto")
  (unless (equal svelte-backend 'lsp)
    (spacemacs/declare-prefix-for-mode 'svelte-mode "mh" "help")
    (spacemacs/declare-prefix-for-mode 'svelte-mode "mr" "refactor"))
  (spacemacs/set-leader-keys-for-major-mode 'svelte-mode
    "El" 'web-mode-dom-errors-show
    "eb" 'web-mode-element-beginning
    "ec" 'web-mode-element-child
    "ep" 'web-mode-element-parent
    "es" 'web-mode-element-sibling-next
    "hp" 'web-mode-dom-xpath
    "rc" 'web-mode-element-clone
    "rd" 'web-mode-element-vanish
    "rk" 'web-mode-element-kill
    "rn" 'web-mode-element-rename
    "rw" 'web-mode-element-wrap
    "z" 'web-mode-fold-or-unfold))

(defun spacemacs//svelte-setup-transient-state ()
  (defvar spacemacs--svelte-ts-full-hint-toggle nil
    "Toggle the state of the svelte transient state documentation.")

  (defvar spacemacs--svelte-ts-full-hint nil
    "Display full svelte transient state documentation.")

  (defvar spacemacs--svelte-ts-minified-hint nil
    "Display minified svelte transient state documentation.")

  (defun spacemacs//svelte-ts-toggle-hint ()
    "Toggle the full hint docstring for the svelte transient state."
    (interactive)
    (setq spacemacs--svelte-ts-full-hint-toggle
          (not spacemacs--svelte-ts-full-hint-toggle)))

  (defun spacemacs//svelte-ts-hint ()
    "Return a condensed/full hint for the svelte transient state"
    (concat
     " "
     (if spacemacs--svelte-ts-full-hint-toggle
         spacemacs--svelte-ts-full-hint
       (concat "[" (propertize "?" 'face 'hydra-face-red) "] help"
               spacemacs--svelte-ts-minified-hint))))

  (spacemacs|transient-state-format-hint svelte
    spacemacs--svelte-ts-minified-hint "\n
Navigate: _j_ _k_ _J_ _K_ _h_ _l_ Element: _c_ _d_ _D_ _r_ _w_ Other: _p_")

  (spacemacs|transient-state-format-hint svelte
    spacemacs--svelte-ts-full-hint
    (format "\n[_?_] toggle help
Navigate^^^^                 Element^^                    Other
[_j_/_k_] next/prev element  [_c_] clone                  [_p_] xpath (display path)
[_J_/_K_] next/prev sibling  [_d_] vanish (keep content)  [_q_] quit
[_h_/_l_] parent/child       [_D_] kill (inkl. content)
^^^^                         [_r_] rename
^^^^                         [_w_] wrap"))

  (spacemacs|define-transient-state svelte
    :title "Svelte Transient State"
    :hint-is-doc t
    :dynamic-hint (spacemacs//svelte-ts-hint)
    :foreign-keys run
    :evil-leader-for-mode (svelte-mode . ".")
    :bindings
    ("?" spacemacs//svelte-ts-toggle-hint)
    ;; Navigate
    ("j"  web-mode-element-next)
    ("k"  web-mode-element-previous)
    ("J"  web-mode-element-sibling-next)
    ("gj" web-mode-element-sibling-next)
    ("K"  web-mode-element-sibling-previous)
    ("gk" web-mode-element-sibling-previous)
    ("h"  web-mode-element-parent)
    ("l"  web-mode-element-child)
    ;; Element
    ("c" web-mode-element-clone)
    ("d" web-mode-element-vanish)
    ("D" web-mode-element-kill)
    ("r" web-mode-element-rename)
    ("w" web-mode-element-wrap)
    ;; Other
    ("p" web-mode-dom-xpath)
    ("q" nil :exit t)
    ("<escape>" nil :exit t)))
