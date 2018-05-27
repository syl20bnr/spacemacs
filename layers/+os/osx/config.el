;;; config.el --- OSX Layer config File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar osx-use-option-as-meta 'deprecated
  "DEPRECATED. See README for OSX layer for new variables. If this
   variable is set it will take precedence (for backwards compatibility).
   If non nil the option key is mapped to meta. Set to `nil` if you need the
   option key to type common characters.
   Default: `deprecated'")

(defvar osx-command-as 'hyper
  "Sets the key binding of the `COMMAND' key on OSX.
   Possible values are `super' `meta' `hyper' `alt' `none'.
   Default: `hyper'.")
;; There are problems setting osx-command-as to `alt' and `super',
;; so we use `hyper' as a default instead because, for example:
;;   - Using `alt':   Command-x or Command-m inserts, respectively: × µ
;;   - Using `super': Control-Command-f produces keycode: <C-s-268632078>
;; Setting to `hyper' seems to avoid both types of the above problems.
;; Also, while it is possible, it is not recommended to set to `meta'
;; since standard OSX shortcuts would overshadow important keys such
;; as M-x.

(defvar osx-option-as 'meta
  "Sets the key binding of the `OPTION' key on OSX.
   Possible values are `super' `meta' `hyper' `alt' `none'.
   Default: `meta'.
   For backwards compatibility the variable `osx-use-option-as-meta'
   takes precedence is set to t.")
(defvar osx-function-as nil
  "Sets the key binding of the `FUNCTION' key on OSX.
   Possible values are `super' `meta' `hyper' `alt' `nil'.
   Default: `nil'.")
(defvar osx-control-as 'control
  "Sets the key binding of the `CONTROL' key on OSX.
   Possible values are `super' `meta' `hyper' `alt' `none'.
   Default: `control'.")

(defvar osx-right-control-as 'left
  "Sets the key binding of the `RIGHT CONTROL' key on OSX.
   Possible values are `super' `meta' `hyper' `alt' `left' `none'.
   Default: `left'.")
(defvar osx-right-command-as 'left
  "Sets the key binding of the `RIGHT COMMAND' key on OSX.
   Possible values are `super' `meta' `hyper' `alt' `left' `none'.
   Default: `left'.")
(defvar osx-right-option-as 'left
  "Sets the key binding of the `RIGHT OPTION' key on OSX.
   Possible values are `super' `meta' `hyper' `alt' `left' `none'.
   Default: `left'.")

(defvar osx-use-dictionary-app t
  "If non nil use osx dictionary app instead of wordnet")

;; Use the OS X Emoji font for Emoticons
(when (fboundp 'set-fontset-font)
  (set-fontset-font "fontset-default"
                    '(#x1F600 . #x1F64F)
                    (font-spec :name "Apple Color Emoji") nil 'prepend))
