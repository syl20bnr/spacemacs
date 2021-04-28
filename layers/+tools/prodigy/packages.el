;;; packages.el --- Prodigy Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
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


(setq prodigy-packages '(prodigy))

(defun prodigy/init-prodigy ()
  (use-package prodigy
    :init
    (spacemacs/set-leader-keys "atp" 'prodigy)
    :config
    (progn
      (evilified-state-evilify prodigy-mode prodigy-mode-map
        "c" 'prodigy-view-clear-buffer
        "h" 'prodigy-first
        "j" 'prodigy-next
        "k" 'prodigy-prev
        "l" 'prodigy-last
        "H" 'prodigy-display-process
        "J" 'prodigy-next-with-status
        "K" 'prodigy-prev-with-status
        "L" 'prodigy-start
        "d" 'prodigy-jump-file-manager
        "g" 'prodigy-jump-magit
        "Y" 'prodigy-copy-cmd
        "R" 'revert-buffer)
      (evilified-state-evilify prodigy-view-mode prodigy-view-mode-map
        "gf" 'find-file-at-point
        "q" 'quit-window))))
