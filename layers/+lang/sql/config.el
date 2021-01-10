;;; config.el --- sql Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Kepi <kepi@igloonet.cz>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs|define-jump-handlers sql-mode)

(defvar sql-capitalize-keywords nil
  "Capitalize keywords in SQL mode.")

(defvar sql-capitalize-keywords-disable-interactive nil
  "Do not capitalize keywords in interactive session (e.g. psql).")

(defvar sql-capitalize-keywords-blacklist '("name")
  "List of keywords to ignore during capitalization.")

(defvar sql-auto-indent t
  "If non nil use sql-indent.")

(defvar sql-backend nil
  "The backend to use for IDE features.
Possible values are `lsp' and `company-sql'.
If `nil' then 'company-sql` is the default backend unless `lsp' layer is used")

(defvar sql-lsp-sqls-workspace-config-path 'workspace
  "Setup workspace configuration with json file. Possible values are:
`workspace': {workspace directory}/.sqls/config.json
`root': {root directory of workspace}/.sqls/config.json")
