;;; config.el --- Scala Layer configuration File for Spacemacs
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


(spacemacs|define-jump-handlers scala-mode)

(defvar scala-enable-gtags nil
  "If non-nil then gtags is enabled in the scala layer.")

(defvar scala-sbt-window-position nil
  "Where to position the SBT window.
If `nil', just let `sbt-mode' figure it out. If `bottom', make a relatively
small window at the bottom of the frame.")

(defvar scala-auto-insert-asterisk-in-comments nil
  "If non-nil automatically insert leading asterisk in multi-line comments.")

(defconst scala-backends '(scala-metals)
  "Backend server implementation to enable advanced IDE language features")

(defvar scala-backend 'scala-metals
  "Backend used to trigger IDE language features.
Only `scala-metals' is currently supported.")

(defvar scala-auto-treeview nil
  "If non-nil automatically show treeview when views are received by metals.")
