;; config.el --- Nav-flash Layer Configuration File for Spacemacs.
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
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

(defvar nav-flash-exclude-commands
  '(mouse-set-point mouse-drag-region evil-mouse-drag-region +org/dwim-at-point
                    org-find-file org-find-file-at-mouse)
  "A list of commands that should not trigger nav-flash.")

(defvar nav-flash--last-point nil
  "internal variable for nav-flash.")
