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


(defun spacemacs//lua-setup-backend ()
  "Conditionally setup lua backend."
  (when (eq lua-backend 'lsp)
    (spacemacs//lua-setup-lsp)))

(defun spacemacs//lua-setup-company ()
  "Conditionally setup company based on backend."
  (pcase lua-backend
    ('lua-mode
     (spacemacs|add-company-backends
       :backends company-lua
       :modes lua-mode))
    ('lsp
     (spacemacs|add-company-backends
       :backends company-capf
       :modes lua-mode))))


;; LSP Lua

(defun spacemacs//lua-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (setq-local lsp-disabled-clients
                    (pcase lua-lsp-server
                      ('emmy '(lua-language-server lsp-lua-lsp))
                      ('lua-language-server '(emmy-lua lsp-lua-lsp))
                      ('lua-lsp '(emmy-lua lsp-lua-language-server))))
        (lsp-deferred))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))
