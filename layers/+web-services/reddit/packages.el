;;; packages.el --- Reddit Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Benedikt Broich <b.broich@posteo.de>
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


(defconst reddit-packages
  '(reddigg))

(defun reddit/init-reddigg ()
  (use-package reddigg
    :defer t
    :init
    (spacemacs/declare-prefix "awr" "reddit")
    (spacemacs/set-leader-keys
      "awrm" 'reddigg-view-main
      "awrs" 'reddigg-view-sub)))
