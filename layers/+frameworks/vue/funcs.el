;;; funcs.el --- vue layer funcs file for Space-macs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3


;; backend
(defun space-macs//vue-setup-backend ()
  "Conditionally setup vue backend."
  (pcase vue-backend
    ('dumb (space-macs//vue-setup-dumb))
    ('lsp (space-macs//vue-setup-lsp))))

(defun space-macs//vue-setup-company ()
  "Conditionally setup company based on backend."
  (pcase vue-backend
    ('dumb (space-macs//vue-setup-dumb-company))))


;; lsp
(defun space-macs//vue-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        ;; error checking from lsp langserver sucks, turn it off
        ;; so eslint won't be overriden
        (setq-local lsp-diagnostics-provider :none)
        (lsp))
    (message (concat "`lsp' layer is not installed, "
                     "please add `lsp' layer to your dotfile."))))


;; dumb

(defun space-macs//vue-setup-dumb-imenu ()
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

(defun space-macs//vue-setup-dumb ()
  (add-to-list 'space-macs-jump-handlers-vue-mode 'dumb-jump-go)
  (space-macs//vue-setup-dumb-imenu))

(defun space-macs//vue-setup-dumb-company ()
  (space-macs|add-company-backends :backends (company-web-html company-css company-files company-dabbrev)
                                  :modes vue-mode
                                  :variables company-minimum-prefix-length 2)
  (company-mode))


;; Others
(defun space-macs//vue-setup-yasnippet ()
  (space-macs/load-yasnippet)
  (yas-activate-extra-mode 'js-mode))

(defun space-macs//vue-setup-editor-style ()
  "such as indent rules comment style etc"
  ;; https://stackoverflow.com/questions/36701024/how-can-i-indent-inline-javascript-in-web-mode
  (setq web-mode-script-padding 0)
  (setq web-mode-style-padding 0)
  ;; https://e-macs.stackexchange.com/questions/27683/change-comment-style-in-web-mode
  (add-to-list 'web-mode-comment-formats '("javascript" . "//")))

(defun space-macs//vue-setup-keybindings ()
  "free stuff from `html' layer"
  (space-macs/declare-prefix-for-mode 'vue-mode "m=" "format")
  (space-macs/declare-prefix-for-mode 'vue-mode "mE" "errors")
  (space-macs/declare-prefix-for-mode 'vue-mode "me" "element")
  (space-macs/declare-prefix-for-mode 'vue-mode "mg" "goto")
  (unless (equal vue-backend 'lsp)
    (space-macs/declare-prefix-for-mode 'vue-mode "mh" "help")
    (space-macs/declare-prefix-for-mode 'vue-mode "mr" "refactor"))
  (space-macs/set-leader-keys-for-major-mode 'vue-mode
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

(defun space-macs//vue-setup-transient-state ()
  (defvar space-macs--vue-ts-full-hint-toggle nil
    "Toggle the state of the vue transient state documentation.")

  (defvar space-macs--vue-ts-full-hint nil
    "Display full vue transient state documentation.")

  (defvar space-macs--vue-ts-minified-hint nil
    "Display minified vue transient state documentation.")

  (defun space-macs//vue-ts-toggle-hint ()
    "Toggle the full hint docstring for the vue transient state."
    (interactive)
    (setq space-macs--vue-ts-full-hint-toggle
          (not space-macs--vue-ts-full-hint-toggle)))

  (defun space-macs//vue-ts-hint ()
    "Return a condensed/full hint for the vue transient state"
    (concat
     " "
     (if space-macs--vue-ts-full-hint-toggle
         space-macs--vue-ts-full-hint
       (concat "[" (propertize "?" 'face 'hydra-face-red) "] help"
               space-macs--vue-ts-minified-hint))))

  (space-macs|transient-state-format-hint vue
    space-macs--vue-ts-minified-hint "\n
Navigate: _j_ _k_ _J_ _K_ _h_ _l_ Element: _c_ _d_ _D_ _r_ _w_ Other: _p_")

  (space-macs|transient-state-format-hint vue
    space-macs--vue-ts-full-hint
    (format "\n[_?_] toggle help
Navigate^^^^                 Element^^                    Other
[_j_/_k_] next/prev element  [_c_] clone                  [_p_] xpath (display path)
[_J_/_K_] next/prev sibling  [_d_] vanish (keep content)  [_q_] quit
[_h_/_l_] parent/child       [_D_] kill (inkl. content)
^^^^                         [_r_] rename
^^^^                         [_w_] wrap"))

  (space-macs|define-transient-state vue
    :title "Vue Transient State"
    :hint-is-doc t
    :dynamic-hint (space-macs//vue-ts-hint)
    :foreign-keys run
    :evil-leader-for-mode (vue-mode . ".")
    :bindings
    ("?" space-macs//vue-ts-toggle-hint)
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


