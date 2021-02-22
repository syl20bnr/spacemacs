;;; config.el --- Markdown Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables

(defvar markdown-live-preview-engine 'eww
  "Possibe values are `eww' (built-in browser) or `vmd' (installed with `npm').")

(defvar markdown-mmm-auto-modes
  '(
    ;; in alphabetical order, symbols first then lists
    "c"
    "c++"
    "css"
    "java"
    "javascript"
    "python"
    "ruby"
    "rust"
    "scala"

    ("elisp" "emacs-lisp")
    ("ess" "R")
    ("ini" "conf-unix")
    ("html" "web")
    )
  "List of language names or lists of language and mode names for which to
generate mmm classes.")

(defvar markdown--key-bindings-modes '(markdown-mode gfm-mode)
  "Modes using markdown key bindings.")
