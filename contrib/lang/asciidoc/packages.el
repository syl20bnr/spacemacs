;;; packages.el --- adoc layer packages file for Spacemacs
;;
;; Copyright (c) 2015 Torben Hoffmann
;;
;; Author: Torben Hoffmann <torben.lehoff@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar asciidoc-packages
  '(cl
    adoc-mode
    )
   "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun asciidoc/init-adoc-mode ()
  (use-package adoc-mode
    :mode ("\\.\\(txt\\|adoc\\|asciidoc\\)\\'" . adoc-mode)
    :config
    (progn
      (evil-leader/set-key-for-mode 'adoc-mode
        ;; Element insertion
        "mt1"    'tempo-template-adoc-title-1
        "mt2"    'tempo-template-adoc-title-2
        "mt3"    'tempo-template-adoc-title-3
        "mt4"    'tempo-template-adoc-title-4
        "mt5"    'tempo-template-adoc-title-5
        "mtp"    'adoc-promote
        "mtd"    'adoc-denote
        "mss"    'tempo-template-adoc-strong
        "mse"    'tempo-template-adoc-emphasis
        ))
    )
  )

(defun asciidoc/init-cl ()
  (use-package cl))
