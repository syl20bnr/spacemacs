;;; funcs.el --- Lua Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Lin Sun <sunlin7@yahoo.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//lua-setup-backend ()
  "Conditionally setup lua backend."
  (setq lua-indent-level 2
        lua-indent-string-contents t)
  (spacemacs/declare-prefix-for-mode 'lua-mode "mh" "help")
  (spacemacs/declare-prefix-for-mode 'lua-mode "ms" "REPL")
  (spacemacs/declare-prefix-for-mode 'lua-mode "mg" "goto")
  (spacemacs/set-leader-keys-for-major-mode 'lua-mode
    "hd" 'lua-search-documentation
    "sb" 'lua-send-buffer
    "sf" 'lua-send-defun
    "sl" 'lua-send-current-line
    "sr" 'lua-send-region
    "'" 'lua-show-process-buffer)
  (pcase lua-backend
    ('lsp-emmy-lua (spacemacs//lua-setup-lsp-emmy-lua))
    ('lsp-lua-language-server (spacemacs//lua-setup-lsp-lua-language-server))
    ('lsp-lua-lsp (spacemacs//lua-setup-lsp-lua-lsp))
    (_ (if lua-backend
           (user-error "Unexpected value of `lua-backend', %s" lua-backend)
         (when (configuration-layer/layer-used-p 'lsp)
           (lsp))))))

(defun spacemacs//lua-setup-company ()
  "Conditionally setup company based on backend."
  (pcase lua-backend
    (_ (spacemacs//lua-setup-company-lua))))


;; LSP Lua
(defun spacemacs//lua-setup-lsp-emmy-lua ()
  "Setup emmy Lua."
  (setq-local lsp-disabled-clients '(lua-language-server lsp-lua-lsp))
  (lsp))

(defun spacemacs//lua-setup-lsp-lua-language-server ()
  "Setup lua-language-server."
  (setq-local lsp-disabled-clients '(emmy-lua lsp-lua-lsp))
  (lsp))

(defun spacemacs//lua-setup-lsp-lua-lsp ()
  "Setup lsp-lua-lsp."
  (setq-local lsp-disabled-clients '(emmy-lua lsp-lua-language-server))
  (lsp))


;; Lua mode
(defun spacemacs//lua-setup-company-lua ()
  (spacemacs|add-company-backends
    :backends company-lua
    :modes lua-mode)
  (company-mode))
