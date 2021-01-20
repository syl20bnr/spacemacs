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

(defvar lua-backend nil
  "The backend to be used for Lua.
When nil, use `lua-mode' as the backend.
When non-nil, use one of LSP Lua clients, i.e. `lsp-emmy', `lua-language-server', or `lsp-lua-lsp'.")
