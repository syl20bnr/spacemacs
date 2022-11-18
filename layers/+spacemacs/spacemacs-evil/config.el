;;; config.el --- Spacemacs-evil Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
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



(defvar vim-style-visual-feedback nil
  "If non-nil objects are briefly highlighted performing an action.")

(defvar hybrid-style-visual-feedback nil
  "If non-nil objects are briefly highlighted performing an action.")

(defvar evil-lisp-safe-structural-editing-modes '()
  "A list of major mode symbols where safe structural editing is supported.")

(defvar spacemacs-evil-collection-allowed-list '(eww dired quickrun ediff)
  "List of modes Spacemacs will allow to be evilified by ‘evil-collection-init’.")
