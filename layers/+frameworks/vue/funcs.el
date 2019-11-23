;;; funcs.el --- vue layer funcs file for Spacemacs. -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; backend
(defun spacemacs//vue-setup-backend ()
  "Conditionally setup vue backend."
  (pcase vue-backend
    ('dumb (spacemacs//vue-setup-dumb))
    ('lsp (spacemacs//vue-setup-lsp))))

(defun spacemacs//vue-setup-company ()
  "Conditionally setup company based on backend."
  (pcase vue-backend
    ('dumb (spacemacs//vue-setup-dumb-company))
    ('lsp (spacemacs//vue-setup-lsp-company))))


;; lsp
(defun spacemacs//vue-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        ;; error checking from lsp langserver sucks, turn it off
        ;; so eslint won't be overriden
        (setq-local lsp-prefer-flymake :none)
        (lsp))
    (message (concat "`lsp' layer is not installed, "
                     "please add `lsp' layer to your dotfile."))))

(defun spacemacs//vue-setup-lsp-company ()
  "Setup lsp auto-completion."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (spacemacs|add-company-backends
          :backends company-lsp
          :modes vue-mode
          :variables company-minimum-prefix-length 2
          :append-hooks nil
          :call-hooks t)
        (company-mode))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))


;; dumb

(defun spacemacs//vue-setup-dumb-imenu ()
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

(defun spacemacs//vue-setup-dumb ()
  (add-to-list 'spacemacs-jump-handlers-vue-mode 'dumb-jump-go)
  (spacemacs//vue-setup-dumb-imenu))

(defun spacemacs//vue-setup-dumb-company ()
  (spacemacs|add-company-backends :backends (company-web-html company-css company-files company-dabbrev)
                                  :modes vue-mode
                                  :variables company-minimum-prefix-length 2)
  (company-mode))


;; Others
(defun spacemacs//vue-setup-yasnippet ()
  (spacemacs/load-yasnippet)
  (yas-activate-extra-mode 'js-mode))

(defun spacemacs//vue-setup-editor-style ()
  "such as indent rules comment style etc"
  ;; https://stackoverflow.com/questions/36701024/how-can-i-indent-inline-javascript-in-web-mode
  (setq web-mode-script-padding 0)
  ;; https://emacs.stackexchange.com/questions/27683/change-comment-style-in-web-mode
  (add-to-list 'web-mode-comment-formats '("javascript" . "//")))

(defun spacemacs//vue-setup-keybindings ()
  "free stuff from `html' layer"
  (spacemacs/declare-prefix-for-mode 'vue-mode "m=" "format")
  (spacemacs/declare-prefix-for-mode 'vue-mode "mE" "errors")
  (spacemacs/declare-prefix-for-mode 'vue-mode "me" "element")
  (spacemacs/declare-prefix-for-mode 'vue-mode "mg" "goto")
  (unless (equal vue-backend 'lsp)
    (spacemacs/declare-prefix-for-mode 'vue-mode "mh" "help")
    (spacemacs/declare-prefix-for-mode 'vue-mode "mr" "refactor"))
  (spacemacs/set-leader-keys-for-major-mode 'vue-mode
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
    "z" 'web-mode-fold-or-unfold)
  (spacemacs|define-transient-state vue-mode
    :title "Web-mode Transient State"
    :columns 4
    :foreign-keys run
    :evil-leader-for-mode (vue-mode . ".")
    :bindings
    ("D" web-mode-element-kill "kill")
    ("J" web-mode-element-sibling-next "next sibling")
    ("K" web-mode-element-sibling-previous "previous sibling")
    ("c" web-mode-element-clone "clone")
    ("d" web-mode-element-vanish "delete")
    ("gj" web-mode-element-sibling-next)
    ("gk" web-mode-element-sibling-previous)
    ("h" web-mode-element-parent "parent")
    ("j" web-mode-element-next "next")
    ("k" web-mode-element-previous "previous")
    ("l" web-mode-element-child "child")
    ("p" web-mode-dom-xpath "xpath")
    ("q" nil "quit" :exit t)
    ("r" web-mode-element-rename "rename" :exit t)
    ("w" web-mode-element-wrap "wrap")
    ("<escape>" nil nil :exit t)))
