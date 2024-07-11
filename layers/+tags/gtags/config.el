;;; config.el --- gtags configuration File
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
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


(defvar gtags-enable-by-default t
  "Whether or not to enable ggtags-mode.")

(defvar spacemacs--counsel-gtags-dwim-success nil
  "Stores the return value of `counsel-gtags-dwim' so it can be
  passed to the jump handler. This is needed because `buffer' and
  `point' are not updated after jumping.")

(spacemacs|define-jump-handlers tcl-mode)
(spacemacs|define-jump-handlers vhdl-mode)
(spacemacs|define-jump-handlers awk-mode)
(spacemacs|define-jump-handlers dired-mode)
(spacemacs|define-jump-handlers compilation-mode)
(spacemacs|define-jump-handlers shell-mode)
