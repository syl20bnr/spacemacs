;;; config.el --- Typescript Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables

(defvar typescript-fmt-on-save nil
  "Run formatter on buffer save.")

(defvar typescript-fmt-tool 'tide
  "The name of the tool to be used for TypeScript source code formatting.
Currently available 'tide (default), 'typescript-formatter and 'prettier.")

(defvar typescript-backend nil
  "The backend to use for IDE features.
Possible values are `tide'+and `lsp'.
If `nil' then `tide' is the default backend unless `lsp' layer is used.")

(defvar typescript-linter 'tslint
  "The linter to use for typescript. Possible values are `tslint' `eslint'")

(defvar typescript-lsp-linter t
  "If the backend is `lsp', and this variable is non-nil, then
use lsp as the linter, otherwise let flycheck choose the best
linter that's available.")

(spacemacs|define-jump-handlers typescript-mode)
(spacemacs|define-jump-handlers typescript-tsx-mode)
