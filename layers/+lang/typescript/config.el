;;; config.el --- Typescript Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
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


;; variables

(defvar typescript-fmt-on-save nil
  "Run formatter on buffer save.")

(defvar typescript-fmt-tool 'tide
  "The name of the tool to be used for TypeScript source code formatting.
Possible values are 'tide (default), 'typescript-formatter and 'prettier.")

(defvar typescript-backend (if (configuration-layer/layer-used-p 'lsp) 'lsp 'tide)
  "The backend to use for IDE features.
Possible values are `tide' and `lsp'.
If `nil' then `tide' is the default backend unless `lsp' layer is used.")

(defvar typescript-linter 'tslint
  "The linter to use for typescript. Possible values are `tslint' `eslint'")

(defvar typescript-lsp-linter t
  "If the backend is `lsp', and this variable is non-nil, then
use lsp as the linter, otherwise let flycheck choose the best
linter that's available.")

(spacemacs|define-jump-handlers typescript-mode)
(spacemacs|define-jump-handlers typescript-tsx-mode)
