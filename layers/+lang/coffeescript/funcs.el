;;; funcs.el --- CoffeeScript Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Muneeb Shaikh <muneeb@reversehack.in>
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


(defun spacemacs//coffeescript-indent-hook ()
  (setq indent-line-function 'spacemacs//coffeescript-indent
        evil-shift-width coffee-tab-width))

(defun spacemacs//coffeescript-indent ()
  (if (coffee-line-wants-indent)
      ;; We need to insert an additional tab because
      ;; the last line was special.
      (coffee-insert-spaces (+ (coffee-previous-indent) coffee-tab-width))
    ;; otherwise keep at the same indentation level
    (coffee-insert-spaces (coffee-previous-indent))))
