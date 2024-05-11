;;; config.el --- OSX Layer config File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
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

(defvar osx-command-as 'hyper
  "Sets the key binding of the `COMMAND' key on macOS.
   Possible values are `super', `meta', `hyper', `alt', and `none'.
   Default: `hyper'.")
;; There are problems setting osx-command-as to `alt' and `super',
;; so we use `hyper' as a default instead because, for example:
;;   - Using `alt':   Command-x or Command-m inserts, respectively: × µ
;;   - Using `super': Control-Command-f produces keycode: <C-s-268632078>
;; Setting to `hyper' seems to avoid both types of the above problems.
;; Also, while it is possible, it is not recommended to set to `meta'
;; since standard macOS shortcuts would overshadow important keys such
;; as M-x.

(defvar osx-option-as 'meta
  "Sets the key binding of the `OPTION' key on macOS.
   Possible values are `super' `meta' `hyper' `alt' `none'.
   Default: `meta'.")
(defvar osx-function-as nil
  "Sets the key binding of the `FUNCTION' key on macOS.
   Possible values are `super' `meta' `hyper' `alt' `nil'.
   Default: `nil'.")
(defvar osx-control-as 'control
  "Sets the key binding of the `CONTROL' key on macOS.
   Possible values are `super' `meta' `hyper' `alt' `none'.
   Default: `control'.")

(defvar osx-right-control-as 'left
  "Sets the key binding of the `RIGHT CONTROL' key on macOS.
   Possible values are `super' `meta' `hyper' `alt' `left' `none'.
   Default: `left'.")
(defvar osx-right-command-as 'left
  "Sets the key binding of the `RIGHT COMMAND' key on macOS.
   Possible values are `super' `meta' `hyper' `alt' `left' `none'.
   Default: `left'.")
(defvar osx-right-option-as 'left
  "Sets the key binding of the `RIGHT OPTION' key on macOS.
   Possible values are `super' `meta' `hyper' `alt' `left' `none'.
   Default: `left'.")

(defvar osx-use-dictionary-app (spacemacs/system-is-mac)
  "Use the macOS dictionary app instead of Wordnet.")

(defvar osx-swap-option-and-command nil
  "If non nil swap option key and command key")

;; Use the macOS Emoji font for Emoticons.
(when (fboundp 'set-fontset-font)
  (set-fontset-font "fontset-default"
                    '(#x1F600 . #x1F64F)
                    (font-spec :name "Apple Color Emoji") nil 'prepend))
