;;; config.el --- Typescript Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
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
Currently avaliable 'tide (default), 'typescript-formatter and 'prettier.")

(defvar typescript-backend 'tide
  "The backend to use for IDE features. Possible values are `tide'
+and `lsp'.")

(spacemacs|define-jump-handlers typescript-mode)
(spacemacs|define-jump-handlers typescript-tsx-mode)
