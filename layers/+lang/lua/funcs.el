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

(defun spacemacs//lua-backend ()
  "Returns selected backend."
  (if lua-backend
      lua-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'lua-mode))))

(defun spacemacs//lua-setup-backend ()
  "Conditionally setup lua backend."
  (pcase (spacemacs//lua-backend)
    (`lsp (spacemacs//lua-setup-lsp))))

(defun spacemacs//lua-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (spacemacs//lua-backend)
    (`lua-mode (spacemacs|add-company-backends
                 :backends company-lua
                 :modes lua-mode))
    (`lsp (spacemacs|add-company-backends
            :backends company-capf
            :modes lua-mode))))


;; LSP Lua

(defun spacemacs//lua-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (cond ((eq lua-lsp-server 'emmy)  (setq-local lsp-disabled-clients '(lua-language-server lsp-lua-lsp)))
              ((eq lua-lsp-server 'lua-language-server) (setq-local lsp-disabled-clients '(emmy-lua lsp-lua-lsp)))
              ((eq lua-lsp-server 'lua-lsp) (setq-local lsp-disabled-clients '(emmy-lua lsp-lua-language-server))))
        (lsp))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))
