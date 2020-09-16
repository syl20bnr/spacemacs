;;; config.el --- C/C++ Layer config File for Spacemacs
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

(spacemacs|define-jump-handlers c++-mode)
(spacemacs|define-jump-handlers c-mode)

(defvar c-c++-backend nil
  "The backend to use for IDE features.
Possible values are `lsp-ccls', `lsp-clangd', `rtags' and `ycmd'.")


;; lsp

(defvar c-c++-lsp-enable-semantic-highlight nil
  "If non-nil then enable semantic highlighting.
If `t' then regular semantic highlighting is enabled.
If `rainbow' then rainbow semantic highlighting is enabled.
Rainbow semantic highlighting gives a unique color to each identifier.")

(defvar c-c++-lsp-semantic-highlight-method 'font-lock
  "Method used to highlight the text when semantic highlighting is enabled.

By default `font-lock' is used to highlight the text, set the variable to
`overlay' if you want to use overlays. Note that overlays can be slower.")


;; rtags

(defvar c-c++-enable-rtags-completion t
  "If `nil', RTags completion is disabled when the RTags backend is enabled.")


;; clang

(defvar c-c++-enable-clang-format-on-save nil
  "If non-nil, automatically format code with ClangFormat on
  save. Clang support has to be enabled for this to work.")


;; style

(defvar c++-enable-organize-includes-on-save nil
  "If non-nil then automatically organize the includes on save C++ buffer.")

(defvar c-c++-enable-auto-newline nil
  "If non nil then enables the `Auto-newline' minor mode.")

(defvar c-c++-enable-google-style nil
  "If non-nil `google-set-c-style' will be added as as
  `c-mode-common-hook'.")

(defvar c-c++-enable-google-newline nil
  "If non-nil `google-make-newline-indent' will be added as as
  `c-mode-common-hook'.")


;; misc

(defvar c-c++-default-mode-for-headers (when (not (functionp 'c-or-c++-mode)) 'c-mode)
  "Default mode to open header files. Can be `c-mode' or `c++-mode', or `c-or-c++-mode' for Emacs > 26+.")

(defvar c-c++-adopt-subprojects nil
  "When non-nil, projectile will remember project root when visiting files in subprojects")


;; internal

(defconst c-c++-modes '(c-mode c++-mode)
  "Primary major modes of the `c-c++' layer.")

(defconst c-c++-mode-hooks '(c-mode-hook c++-mode-hook)
  "Primary hooks of the `c-c++' layer.")
