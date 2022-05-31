;;; config.el --- sql Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Kepi <kepi@igloonet.cz>
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


(spacemacs|define-jump-handlers sql-mode)

(defvar sql-capitalize-keywords nil
  "Capitalize keywords in SQL mode.")

(defvar sql-capitalize-keywords-disable-interactive nil
  "Do not capitalize keywords in interactive session (e.g. psql).")

(defvar sql-capitalize-keywords-blacklist '("name")
  "List of keywords to ignore during capitalization.")

(defvar sql-auto-indent t
  "If non nil use sql-indent.")

(defvar sql-backend (if (configuration-layer/layer-used-p 'lsp) 'lsp 'company-sql)
  "The backend to use for IDE features.
Possible values are `lsp' and `company-sql'.
If `nil' then 'company-sql` is the default backend unless `lsp' layer is used")

(defvar sql-lsp-sqls-workspace-config-path 'workspace
  "Setup workspace configuration with json file. Possible values are:
`workspace': {workspace directory}/.sqls/config.json
`root': {root directory of workspace}/.sqls/config.json")
