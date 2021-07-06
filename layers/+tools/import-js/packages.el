;;; packages.el --- import-js Layer packages file for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
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


(setq import-js-packages '(import-js))

(defun import-js/init-import-js ()
  (use-package import-js
    :defer t
    :init
    (dolist (x spacemacs--import-js-modes)
      (add-hook (cdr x) #'run-import-js)
      (spacemacs/declare-prefix-for-mode (car x) "mi" "import")
      (spacemacs/set-leader-keys-for-major-mode (car x)
        "if" #'spacemacs/import-js-fix
        "ii" #'spacemacs/import-js-import
        "ig" #'import-js-goto))))
