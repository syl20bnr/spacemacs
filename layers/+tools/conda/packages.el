;;; packages.el --- Conda Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Zach Pearson <zach@zjp.codes>
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


(setq conda-packages
      '(
        conda
        ))

(defun conda/init-conda ()
  (use-package conda
    :defer t
    :commands (conda-env-list
               conda-env-activate
               conda-env-deactivate
               conda-env-autoactivate-mode
               conda-env-activate-for-buffer)
    :init
    (spacemacs/declare-prefix-for-mode 'python-mode "mn" "anaconda")
    (spacemacs/set-leader-keys-for-major-mode 'python-mode
      "nl" 'conda-env-list
      "na" 'conda-env-activate
      "nd" 'conda-env-deactivate
      "nA" 'conda-env-autoactivate-mode
      "nb" 'conda-env-activate-for-buffer)))
