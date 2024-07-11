;;; config.el --- Elixir Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
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


;; Variables

(defvar elixir-backend (if (configuration-layer/layer-used-p 'lsp) 'lsp 'alchemist)
  "The backend to use for IDE features.
Possible values are `alchemist' and `lsp'.
If `nil' then `alchemist' is the default backend unless `lsp' layer is used.")
(put 'elixir-backend 'safe-local-variable #'symbolp)

(defvar elixir-ls-path "~/elixir-ls/release"
  "The path to the folder that contains the elixir-ls release, start scripts (language_server.sh/language_server.bat).")

(spacemacs|define-jump-handlers elixir-mode)
