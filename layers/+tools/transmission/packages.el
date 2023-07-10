;;; packages.el --- Transmission Layer packages File for Spacemacs
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


(setq transmission-packages
      '(transmission))

(defun transmission/init-transmission ()
  (use-package transmission
    :defer t
    :init (spacemacs/set-leader-keys "att" 'transmission)
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'transmission-mode "ma" "add")
      (spacemacs/declare-prefix-for-mode 'transmission-mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode 'transmission-mode "ms" "set")
      (spacemacs/declare-prefix-for-mode 'transmission-mode "mt" "toggle")
      (spacemacs/set-leader-keys-for-major-mode 'transmission-mode
        "D"  'transmission-delete
        "m"  'transmission-toggle-mark
        "q"  'transmission-quit
        "r"  'transmission-move
        "R"  'transmission-remove
        "S"  'tabulated-list-sort
        "v"  'transmission-verify

        ;; add
        "aa" 'transmission-add
        "at" 'transmission-trackers-add

        ;; goto
        "gi" 'transmission-info
        "gf" 'transmission-files
        "gp" 'transmission-peers

        ;; set
        "sd" 'transmission-set-download
        "sl" 'transmission-label
        "sp" 'transmission-set-bandwidth-priority
        "sr" 'transmission-set-ratio
        "su" 'transmission-set-upload

        ;; toggle
        "ts" 'transmission-toggle
        "tt" 'transmission-turtle-mode)

      (evil-define-key 'normal transmission-mode-map
        "H" 'transmission-queue-move-top
        "J" 'transmission-queue-move-down
        "K" 'transmission-queue-move-up
        "L" 'transmission-queue-move-bottom)

      (spacemacs|diminish transmission-turtle-mode " 🐢" " [T]")
      (spacemacs/declare-prefix-for-minor-mode 'transmission-turtle-mode "ms" "set")
      (spacemacs/set-leader-keys-for-minor-mode 'transmission-turtle-mode
        ;; set
        "sD" 'transmission-turtle-set-days
        "sS" 'transmission-turtle-set-speeds
        "sT" 'transmission-turtle-set-times)

      (spacemacs/declare-prefix-for-mode 'transmission-info-mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode 'transmission-info-mode "ms" "set")
      (spacemacs/set-leader-keys-for-major-mode 'transmission-info-mode
        "a"  'transmission-trackers-add
        "c"  'transmission-copy-magnet
        "r"  'transmission-move
        "T"  'transmission-trackers-remove

        ;; goto
        "gf" 'transmission-files
        "gp" 'transmission-peers

        ;; set
        "sd" 'transmission-set-torrent-download
        "sl" 'transmission-label
        "sp" 'transmission-set-bandwidth-priority
        "sr" 'transmission-set-torrent-ratio
        "su" 'transmission-set-torrent-upload)

      (spacemacs/declare-prefix-for-mode 'transmission-files-mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode 'transmission-files-mode "ms" "set")
      (spacemacs/declare-prefix-for-mode 'transmission-files-mode "mm" "toggle")
      (spacemacs/set-leader-keys-for-major-mode 'transmission-files-mode
        "m"  'transmission-toggle-mark
        "X"  'transmission-files-command

        ;; goto
        "gf" 'transmission-find-file
        "gi" 'transmission-info
        "gp" 'transmission-peers

        ;; set
        "sp" 'transmission-files-priority

        ;; toggle
        "tu" 'transmission-files-unwant
        "tw" 'transmission-files-want)

      (spacemacs/set-leader-keys-for-major-mode 'transmission-peers-mode
        "i" 'transmission-info)
      (when transmission-auto-refresh-all
        (setq transmission-refresh-modes '(transmission-mode
                                           transmission-files-mode
                                           transmission-info-mode
                                           transmission-peers-mode))))))
