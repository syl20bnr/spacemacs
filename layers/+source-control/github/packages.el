;;; packages.el --- Github Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
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

;; Package deprecation notice shown at startup
(warn "`github' layer is deprecated. See layer README.org for details.")


(defconst github-packages
  '(
    grip-mode
    ;; this package does not exits, we need it to wrap
    ;; the call to spacemacs/declare-prefix.
    (spacemacs-github :location built-in)))

(defun github/init-grip-mode ()
  (use-package grip-mode
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys
        "ghp" 'grip-mode))))

(defun github/init-spacemacs-github ()
  (spacemacs/declare-prefix "gh" "github"))
