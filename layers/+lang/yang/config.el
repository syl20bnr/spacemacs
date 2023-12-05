;;; config.el --- yang layer config file for Spacemacs.
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Christian Hopps <chopps@gmail.com>
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


;; (defvar yang-check-using-rules "lint"
;;   "Value indicating which rules to use when checking yang syntax
;;   checking. This value is passed as an double-dash argument to
;;   pyang. As of this writing the following values are supported:

;;     bbf, ieee, ietf, lint, mef

;;   where lint represents RFC6087 rules, and the others correspond
;;   to the respective organization.
;;   ")

(defvar yang-pyang-rules "lint"
  "Rules to use when checking yang syntax. This value is
  prepended with double-dash and passed to pyang. The valid
  values at the time of this writing were as follows:

    bbf, ieee, ietf, mef, and lint

  lint being the default and referring to rules outlined in RFC
  6020.")

(defvar yang-pyang-extra-args nil
  "Any extra arguments to pass to pyang.")
