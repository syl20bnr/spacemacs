;;; packages.el --- lsp-python layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Yuan Fu <casouri@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; install pyls-mypy for mypy check
;; (defvar python-enable-mypy nil
;;   "If non-nil, enable mypy syntax checker.")

;; install pyls-isort for import sort
;; (defvar python-enable-import-sort nil
;;   "If non-nil, sort import on save.")

(defvar python-enable-format-on-save nil
  "If non-nil, format by yapf on save")

;; (defcustom lsp-python-server-install-directory
;;   "~/.emacs.d/lsp-python-server"
;;   "Installation directory for lsp python server."
;;   :type 'directory)

(defvar default-python-interpreter "python3")
