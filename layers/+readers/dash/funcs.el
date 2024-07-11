;;; funcs.el --- Dash Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
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


(defun dash//activate-package-docsets (path)
  "Add dash docsets from specified PATH."
  (unless (string-blank-p path)
    (setq dash-docs-docsets-path (expand-file-name path)))
  (setq dash-docs-common-docsets (dash-docs-installed-docsets))
  (message (format "activated %d docsets from: %s"
                   (length dash-docs-common-docsets) path)))
