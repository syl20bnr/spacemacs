;;; packages.el --- csharp Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: William Casarin <bill@casarin.me>
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


(setq csv-packages '(csv-mode))

(defun csv/init-csv-mode ()
  (use-package csv-mode
    :defer t
    :config
    (spacemacs/declare-prefix-for-mode 'csv-mode "ms" "sort")
    (spacemacs/declare-prefix-for-mode 'csv-mode "mv" "yank")
    (spacemacs/set-leader-keys-for-major-mode 'csv-mode
      "a"  'csv-align-fields
      "d"  'csv-kill-fields
      "h"  'csv-header-line
      "i"  'csv-toggle-invisibility
      "n"  'csv-forward-field
      "p"  'csv-backward-field
      "r"  'csv-reverse-region
      "sf" 'csv-sort-fields
      "sn" 'csv-sort-numeric-fields
      "so" 'csv-toggle-descending
      "t"  'csv-transpose
      "u"  'csv-unalign-fields
      "vf" 'csv-yank-fields
      "vt" 'csv-yank-as-new-table)
    (spacemacs/inherit-leader-keys-from-parent-mode 'tsv-mode)))
