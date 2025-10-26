;;; config.el --- Spacemacs Navigation Layer config File  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2025 Sylvain Benner & Contributors
;;
;; Author: Ferdinand Nussbaum <ferdinand.nussbaum@inf.ethz.ch>
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

(defvar spacemacs-default-cycle-forwards-key [tab]
  "Key to cycle forwards after commands determined
by `dotspacemacs-enable-cycling'.")

(defvar spacemacs-default-cycle-backwards-key [backspace]
  "Key to cycle backwards after commands determined
by `dotspacemacs-enable-cycling'.")

(defvar spacemacs-alternate-buffer-cycle-forwards-key nil
  "Key to cycle forwards after an invocation of
`spacemacs/alternate-buffer'.

See `dotspacemacs-enable-cycling'.")

(defvar spacemacs-alternate-buffer-cycle-backwards-key nil
  "Key to cycle backwards after an invocation of
`spacemacs/alternate-buffer'.

See `dotspacemacs-enable-cycling'.")

(defvar spacemacs-alternate-window-cycle-forwards-key nil
  "Key to cycle forwards after an invocation of
`spacemacs/alternate-window'.

See `dotspacemacs-enable-cycling'.")

(defvar spacemacs-alternate-window-cycle-backwards-key nil
  "Key to cycle backwards after an invocation of
`spacemacs/alternate-window'.

See `dotspacemacs-enable-cycling'.")
