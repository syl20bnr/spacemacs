;;; config.el --- Python Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables

(spacemacs|defvar-company-backends python-mode)
(spacemacs|defvar-company-backends inferior-python-mode)
(spacemacs|defvar-company-backends pip-requirements-mode)

(defvar python-enable-yapf-format-on-save nil
  "If non-nil, automatically format code with YAPF on save.")

(defvar python-test-runner 'nose
  "Test runner to use. Possible values are `nose' or `pytest'.")

(defvar python-fill-column 79
  "Fill column value for python buffers")

(defvar python-auto-set-local-pyenv-version 'on-visit
  "Automatically set pyenv version from \".python-version\".

Possible values are `on-visit', `on-project-switch' or `nil'.")
