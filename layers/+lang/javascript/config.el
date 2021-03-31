;;; config.el --- Javascript Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
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

(spacemacs|define-jump-handlers js2-mode)

(defvar javascript-backend nil
  "The backend to use for IDE features.
Possible values are `tern', `tide' and `lsp'.
If `nil' then `tern' is the default backend unless `lsp' layer is used.")

(defvar javascript-fmt-tool 'web-beautify
  "The formatter to format a JavaScript file. Possible values are `web-beautify' and `prettier'.")

(defvar javascript-import-tool nil
  "The import backend to import modules. Possible values are `import-js' and `nil' to disable.")

(defvar javascript-fmt-on-save nil
  "Run formatter on buffer save.")

(defvar javascript-repl 'skewer
  "Repl to be configured by the layer, `skewer' for browser based javascript, `nodejs' for server based development.")

(defvar javascript-lsp-linter t
  "If the backend is `lsp', and this variable is non-nil, then
use lsp as the linter, otherwise let flycheck choose the best
linter that's available.")
