;;; config.el --- Purescript Layer config File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Ryan L. Bell
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


;; variables

(spacemacs|define-jump-handlers purescript-mode)

(defvar purescript-fmt-on-save nil
  "Run formatter on buffer save.")

(defvar purescript-fmt-tool 'purs-tidy
  "The name of the tool to be used for Purescript source code formatting.
Possible values are 'purs-tidy (default).")

(defvar purescript-add-import-on-completion t
  "If non-nil adds imports for completed identifiers")

(defvar purescript-enable-rebuild-on-save nil
  "If non-nil rebuild on save is enabled")

(defvar purescript-backend (if (configuration-layer/layer-used-p 'lsp) 'lsp 'psc-ide)
  "The backend to use for IDE features.
Possible values are `lsp' and `psc-ide'.
If `nil' then `psc-ide' is the default backend unless `lsp' layer is used.")
