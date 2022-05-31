;;; packages.el --- Sphinx layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author:  <wwguo@hiGDP>
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


(defconst sphinx-packages
  '(
    rst
    (rst-sphinx :location local)
    ))

(defun sphinx/init-rst-sphinx ()
  (use-package rst-sphinx))

(defun sphinx/pre-init-rst ()
  (spacemacs|use-package-add-hook rst
    :post-config
    (spacemacs/declare-prefix-for-mode 'rst-mode "mc" "compile")
    (spacemacs/declare-prefix-for-mode 'rst-mode "mg" "goto")
    (spacemacs/set-leader-keys-for-major-mode 'rst-mode
      "cc" 'rst-sphinx-compile
      "cC" 'rst-sphinx-clean
      "cr" 'rst-sphinx-rebuild
      "gc" 'rst-sphinx-open-conf
      "o"  'rst-sphinx-target-open)))
