;;; config.el --- Markdown Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
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
