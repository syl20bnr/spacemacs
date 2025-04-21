;;; config.el --- Python Layer Configuration File for Spacemacs  -*- lexical-binding: nil; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
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

(spacemacs|define-jump-handlers python-mode)
(spacemacs|define-jump-handlers cython-mode anaconda-mode-goto)

(defvar python-backend (if (configuration-layer/layer-used-p 'lsp) 'lsp 'anaconda)
  "The backend to use for IDE features.
Possible values are `anaconda' and `lsp'.
If `nil' then `anaconda' is the default backend unless the `lsp' layer is used.")
(put 'python-backend 'safe-local-variable #'symbolp)

(defvar python-lsp-server 'pylsp
  "Language server for lsp backend. Possible values are `pylsp', `pyright'")
(put 'python-lsp-server 'safe-local-variable #'symbolp)

(defvar python-lsp-git-root nil
  "If non-nil, use a development version of the language server in this folder")

(defvar python-pipenv-activate nil
  "If non-nil, activate pipenv before enabling backend")

(defvar python-poetry-activate nil
  "If non-nil, activate poetry before enabling backend")

(defvar python-formatter
  (if (and (configuration-layer/layer-used-p 'lsp)
           ;; pyright does not support formatting
           (eq python-lsp-server 'pylsp))
      'lsp
    'yapf)
  "The formatter to use. Possible values are `yapf', `black', `ruff' and `lsp'.
The default formatter is `yapf' unless both the `lsp' layer is used,
and `python-lsp-server' is `pylsp' (pyright does not support formatting).")

(defvar python-format-on-save nil
  "If non-nil, automatically format code with formatter selected
  via `python-formatter' on save.")

(defvar python-test-runner 'pytest
  "Test runner to use. Possible values are `nose' or `pytest'.")
(put 'python-test-runner 'safe-local-variable #'symbolp)

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
(put 'python-sort-imports-on-save 'safe-local-variable 'booleanp)

(defvar python-enable-importmagic nil
  "If non-nil, enable the importmagic feature.")

(defvar spacemacs--python-pyenv-modes nil
  "List of major modes where to add pyenv support.")

(defvar spacemacs--python-pyvenv-modes nil
  "List of major modes where to add pyvenv support.")

(defvar spacemacs--python-pipenv-modes nil
  "List of major modes where to add pipenv support.")

(defvar spacemacs--python-poetry-modes nil
  "List of major modes where to add poetry support.")

(defvar spacemacs--python-shell-interpreter-origin nil
  "Origin python-shell-interpreter value.")
;; inferior-python-mode needs these variables to be defined.  The python
;; package declares them but does not initialize them.
(defvar python-shell--interpreter nil)
(defvar python-shell--interpreter-args nil)
