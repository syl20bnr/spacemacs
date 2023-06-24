;;; packages.el --- Perforce Layer packages File for Spacemacs
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


(defconst perforce-packages
  '(p4))

(defun perforce/init-p4 ()
  (use-package p4
    :commands (p4-add
               p4-branch
               p4-branches
               p4-client
               p4-changes
               p4-diff2
               p4-describe
               p4-edit
               p4-reopen
               p4-depot-find-file
               p4-filelog
               p4-files
               p4-get-client-name
               p4-update
               p4-help
               p4-have
               p4-info
               p4-integ
               p4-job
               p4-jobs
               p4-label
               p4-labels
               p4-labelsync
               p4-move
               p4-opened
               p4-print
               p4-set-p4-port
               quit-window
               p4-revert
               p4-refresh
               p4-resolve
               p4-shelve
               p4-status
               p4-submit
               p4-toggle-vc-mode
               p4-unshelve
               p4-user
               p4-users
               p4-version
               p4-annotate
               p4-where
               p4-delete
               p4-fix
               p4-reconcile
               p4-diff
               p4-diff-all-opened
               p4-ediff)
    :init
    (spacemacs/declare-prefix "p4" "perforce")
    (spacemacs/set-leader-keys
      "p4a" 'p4-add
      "p4b" 'p4-branch
      "p4B" 'p4-branches
      "p4c" 'p4-client
      "p4C" 'p4-changes
      "p4d" 'p4-diff2
      "p4D" 'p4-describe
      "p4e" 'p4-edit
      "p4E" 'p4-reopen
      "p4@" 'p4-depot-find-file
      "p4f" 'p4-filelog
      "p4F" 'p4-files
      "p4G" 'p4-get-client-name
      "p4g" 'p4-update
      "p4h" 'p4-help
      "p4H" 'p4-have
      "p4i" 'p4-info
      "p4I" 'p4-integ
      "p4j" 'p4-job
      "p4J" 'p4-jobs
      "p4l" 'p4-label
      "p4L" 'p4-labels
      "p4:" 'p4-labelsync
      "p4m" 'p4-move
      "p4o" 'p4-opened
      "p4p" 'p4-print
      "p4P" 'p4-set-p4-port
      "p4q" 'quit-window
      "p4r" 'p4-revert
      "p4R" 'p4-refresh
      "p4y" 'p4-resolve
      "p4s" 'p4-status
      "p4S" 'p4-submit
      "p4[" 'p4-shelve
      "p4]" 'p4-unshelve
      "p4t" 'p4-toggle-vc-mode
      "p4u" 'p4-user
      "p4U" 'p4-users
      "p4v" 'p4-version
      "p4V" 'p4-annotate
      "p4w" 'p4-where
      "p4x" 'p4-delete
      "p4X" 'p4-fix
      "p4z" 'p4-reconcile
      "p4=" 'p4-diff
      "p4+" 'p4-diff-all-opened
      "p4-" 'p4-ediff)))
