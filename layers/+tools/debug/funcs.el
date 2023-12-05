;;; funcs.el --- Debug layer function file for Spacemacs.
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Troy Hinckley <troy.hinckley@gmail.com>
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


(defun spacemacs/debug-short-key-state (mode-on)
  "Set evil-evilified-state explicitly."
  (if mode-on
      (evil-evilified-state)
    (evil-normal-state)))

(defun spacemacs/debug-generate-symbol (debugger)
  "Create RealGUD interactive function name from DEBUGGER."
  (intern (concat "realgud:" debugger)))

(defun spacemacs/add-realgud-debugger (mode debugger)
  "Add RealGUD DEBUGGER to MODE."
  (let ((dbg-name (spacemacs/debug-generate-symbol debugger)))
    (spacemacs/set-leader-keys-for-major-mode mode
      "dd" dbg-name)
    (autoload dbg-name "realgud" nil t)))
