;;; config.el --- prolog layer config File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Newres Al Haider <newrescode@gmail.com>
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

(defvar prolog-system `swi
  "The type of Prolog system used when setting up the Prolog layer. A number of configuration values are derived from this, notably in prolog-mode. The default value is `swi for SWI-Prolog. The recognized symbol values are:
swi     - SWI Prolog
sicstus - SICStus Prolog
eclipse - Eclipse Prolog
xsb     - XSB <http://xsb.sourceforge.net>
gnu     - GNU Prolog
yap     - YAP Prolog")

;;; config.el ends here
