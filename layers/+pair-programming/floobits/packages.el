;;; packages.el --- floobits Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Rodolfo Hansen <rhansen@kitsd.com>
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


(setq floobits-packages
  '(
    floobits
    ))

(defun floobits/init-floobits ()
  (use-package floobits
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "P" "PP/floobits")

      (defun spacemacs/floobits-rclocation ()
        "Return the absolute path to the floobits dotfile."
        (concat user-home-directory ".floorc.json"))

      (defun spacemacs/floobits-load-rcfile ()
        "Load ~/.floobitsrc if it exists."
        (let ((floobitsrc (spacemacs/floobits-rclocation)))
          (if (file-exists-p floobitsrc) (load floobitsrc))))

      (spacemacs/set-leader-keys
        "Pc" 'floobits-clear-highlights
        "Pd" 'spacemacs/floobits-load-rcfile
        "Pf" 'floobits-follow-user
        "Pj" 'floobits-join-workspace
        "Pl" 'floobits-leave-workspace
        "PR" 'floobits-share-dir-private
        "Ps" 'floobits-summon
        "Pt" 'floobits-follow-mode-toggle
        "PU" 'floobits-share-dir-public))))
