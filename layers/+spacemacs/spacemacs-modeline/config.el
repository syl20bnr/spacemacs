;;; config.el --- Spacemacs Mode-line Visual Layer configuration File
;;
;; Copyright (c) 2020-2023 Sylvain Benner & Contributors
;;
;; Author: Riccardo Murri <riccardo.murri@gmail.com>
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


(defcustom spacemacs-spaceline-additional-segments
  '((new-version :when active))
  "Additional segments for the Spacemacs modeline.

They are inserted in the modeline between `global' and
`buffer-position'.

Must be a list of valid segments; see `spaceline-install' for
more information on what constitutes a valid segment."
  :type '(repeat sexp)
  :group 'spacemacs)
