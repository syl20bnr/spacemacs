;;; core-versions.el --- Spacemacs Core File  -*- lexical-binding: t -*-
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

;;; Commentary:
;;
;;    Define Spacemacs version and minimum supported Emacs version.

(defconst spacemacs-version          "0.999.0" "Spacemacs version.")
(defconst spacemacs-emacs-min-version   "28.2" "Minimal version of Emacs.")

(defmacro spacemacs|eval-until-emacs-min-version (version msg &rest body)
  "Evaluate the BODY if `spacemacs-emacs-min-version' < VERSION, otherwise
warn the MSG."
  (declare (indent 1))
  `(if (version< spacemacs-emacs-min-version ,version)
       (progn ,@body)
     (apply 'warn
            (if ,msg '("%s" ,msg)
              '("Minimum emacs version %s is newer than supported version %s"
                ,version spacemacs-emacs-min-version)))))

(provide 'core-versions)
