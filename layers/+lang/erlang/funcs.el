;;; funcs.el --- Erlang Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Carlos F. Clavijo <arkan1313@gmail.com>
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


(defun spacemacs//erlang-setup-backend ()
  "Conditionally setup erlang backend."
  (when (eq erlang-backend 'lsp) (spacemacs//erlang-setup-lsp)))

(defun spacemacs//erlang-setup-company ()
  "Conditionally setup company based on backend."
  ;; Activate lsp company explicitly to activate
  ;; standard backends as well
  (when (eq erlang-backend 'lsp)
    (spacemacs|add-company-backends
      :backends company-capf
      :modes erlang-mode
      :append-hooks t)))

(defun spacemacs//erlang-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (lsp-deferred)
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun spacemacs//erlang-setup-dap ()
  "Conditionally setup erlang DAP integration."
  (if (configuration-layer/layer-used-p 'dap)
      (require 'dap-erlang)
    (message "`dsp' layer is not installed, please add `dap' layer to your dotfile.")))

(defun spacemacs//erlang-default ()
  "Default settings for erlang buffers"

  ;; Use a custom fill-column for erlang buffers
  (set-fill-column erlang-fill-column))
