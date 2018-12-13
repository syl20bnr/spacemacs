;;; config.el --- Python Layer Configuration File for Spacemacs
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

(spacemacs|define-jump-handlers python-mode)
(spacemacs|define-jump-handlers cython-mode anaconda-mode-goto)

(defvar python-backend 'anaconda
  "The backend to use for IDE features. Possible values are `anaconda'
and `lsp'.")

(defvar python-enable-yapf-format-on-save nil
  "If non-nil, automatically format code with YAPF on save.")

(defvar python-test-runner 'nose
  "Test runner to use. Possible values are `nose' or `pytest'.")

(defvar python-save-before-test t
  "If non nil, current buffer will be save before call a test function")

(defvar python-fill-column 79
  "Fill column value for python buffers")

(defvar python-tab-width 4
  "Tab width value for python buffers")

(defvar python-spacemacs-indent-guess t
  "If non-nil, try to guess correct indentation settings for python buffers on opening")

(defvar python-auto-set-local-pyenv-version 'on-visit
  "Automatically set pyenv version from \".python-version\".

Possible values are `on-visit', `on-project-switch' or `nil'.")

(defvar python-auto-set-local-pyvenv-virtualenv 'on-visit
  "Automatically set pyvenv virtualenv from \".venv\".

Possible values are `on-visit', `on-project-switch' or `nil'.")

(defvar python-sort-imports-on-save nil
  "If non-nil, automatically sort imports on save.")

(defvar spacemacs--python-pyenv-modes nil
  "List of major modes where to add pyenv support.")

(defvar spacemacs--python-pyvenv-modes nil
  "List of major modes where to add pyvenv support.")

;; inferior-python-mode needs these variables to be defined.  The python
;; package declares them but does not initialize them.
(defvar python-shell--interpreter nil)
(defvar python-shell--interpreter-args nil)
