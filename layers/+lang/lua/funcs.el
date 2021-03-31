;;; funcs.el --- Lua Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Lin Sun <sunlin7@yahoo.com>
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
