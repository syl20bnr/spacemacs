;;; config.el --- C/C++ Layer config File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables

(defconst c-c++-modes '(c-mode c++-mode)
  "Primary major modes of the `c-c++' layer.")

(defconst c-c++-mode-hooks '(c-mode-hook c++-mode-hook)
  "Primary hooks of the `c-c++' layer.")

(defconst c-c++-lsp-backends '(lsp-cquery lsp-ccls)
  "Language Server Protocol (LSP) backends supported by the `c-c++' layer.")

(defvar c-c++-backend nil
  "If `lsp-cquery' or `lsp-ccls' then selects language server protocol backend (cquery or ccls).
  If `rtags' then enables rtags support")

(defvar c-c++-enable-auto-newline nil
  "If non nil then enables the `Auto-newline' minor mode.")

(defvar c-c++-enable-clang-support nil
  "If non nil Clang related packages and configuration are enabled.")

(defvar c-c++-enable-google-style nil
  "If non-nil `google-set-c-style' will be added as as
  `c-mode-common-hook'.")

(defvar c-c++-enable-google-newline nil
  "If non-nil `google-make-newline-indent' will be added as as
  `c-mode-common-hook'.")

(defvar c-c++-enable-rtags-completion t
  "If `nil', RTags completion is disabled when the RTags backend is enabled.")

(defvar c-c++-enable-clang-format-on-save nil
  "If non-nil, automatically format code with ClangFormat on
  save. Clang support has to be enabled for this to work.")

(spacemacs|define-jump-handlers c++-mode)
(spacemacs|define-jump-handlers c-mode)

(defvar c-c++-default-mode-for-headers 'c-mode
  "Default mode to open header files. Can be `c-mode' or `c++-mode'.")

(defvar c-c++-adopt-subprojects nil
  "When non-nil, projectile will remember project root when visiting files in subprojects")

;; c-c++-lsp-backend variables
(defvar c-c++-lsp-cache-dir nil
  "Cache directory. Absolute and relative paths supported.")

(defvar c-c++-lsp-executable nil
  "Path to cquery/ccls executable (default value assumes it's in the path)")

(defvar c-c++-lsp-project-whitelist nil
  "A list of project directory patterns for which lsp-c-c++ should be
initialized. This overrides `c-c++-lsp-project-blacklist'.")

(defvar c-c++-lsp-project-blacklist nil
  "A list of project root patterns for which lsp-c-c++ shouldn't be
initialized. `c-c++-lsp-project-whitelist' is checked first, then this,
if no pattern matches the project root, lsp-c-c++ will be initialized.")

(defvar c-c++-lsp-sem-highlight-method nil
  "Set to 'font-lock or 'overlay to enable semantic highlighting")

(defvar c-c++-lsp-sem-highlight-rainbow nil
  "When non-nil, use rainbow semantic highlighting")

;; I've left cquery/ccls -extra-init-params separate for now, as one has defaults while the other doesn't
;; Just to facilitate switching between the two easily
(defvar c-c++-lsp-extra-init-params '(:cacheFormat "msgpack")
  "Extra initialisation parameters to pass to the backend. See
https://github.com/cquery-project/cquery/blob/master/src/config.h or
https://github.com/MaskRay/ccls/blob/master/src/config.h
for details.")
