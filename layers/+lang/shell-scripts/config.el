;;; config.el --- Shell Scripts Layer Configuration File for Spacemacs
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

(spacemacs|define-jump-handlers sh-mode)

(defvar shell-scripts-backend (when (configuration-layer/layer-used-p 'lsp) 'lsp)
  "The backend to use for IDE features.
When `lsp' layer is used, defaults to `lsp'.")

(defvar shell-scripts-format-on-save nil
  "If non-nil, automatically format code with shfmt on save.")

(defvar shell-scripts-mark-executable-after-save
  (not (spacemacs/system-is-mswindows))
  "If non-nil, automatically changes file to executable after buffer saved.")

(defcustom shell-scripts-shfmt-args ()
  "Arguments passed to shfmt."
  :type '(list string))
