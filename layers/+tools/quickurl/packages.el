;;; packages.el --- Quickurl dispatch layer.
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Spenser Truex <web@spensertruex.com>
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

(defconst quickurl-packages
  '(quickurl :location built-in))

(defun quickurl/init-quickurl ()
  (spacemacs/declare-prefix "atq" "quickurl")
  (spacemacs/set-leader-keys
    "atql" 'quickurl-list
    "atqq" 'quickurl
    "atqi" 'quickurl-ask
    "atqe" 'quickurl-edit-urls
    "atqa" 'quickurl-add-url
    "atqb" 'quickurl-browse-url-ask))
