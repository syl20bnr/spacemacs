;;; config.el --- Lua Layer packages File for Spacemacs
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

(spacemacs|define-jump-handlers lua-mode)

(defvar lua-backend 'nil
  "The backend to use for IDE features.
Possible values are `lua-mode' and `lsp'.
If `nil' then `lua-mode' is the default backend unless `lsp' layer is used.")

(defvar lua-lsp-server 'emmy
  "Language server to use for lsp backend.
Possible values are `emmy', `lua-language-server', or `lua-lsp'.")
