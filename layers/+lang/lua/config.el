;;; config.el --- Lua Layer packages File for Spacemacs
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

(spacemacs|define-jump-handlers lua-mode)

(defvar lua-backend 'nil
  "The backend to be used for lua must be `lsp-emmy' or nil if `lua-mode' should be used.")

;; lua-lsp-backend variables
(defvar lua-lsp-emmy-java-path "java"
  "Path to java which will be used for running emmy-lua language server.")

(defvar lua-lsp-emmy-jar-path "~/.emacs.d/EmmyLua-LS-all.jar"
  "Path to jar which will be used for running EmmyLua language server.")

(defvar lua-lsp-emmy-enable-file-watchers t
  "Enabled the EmmyLua file watchers.")
