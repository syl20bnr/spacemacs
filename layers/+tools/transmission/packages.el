;;; packages.el --- Transmission Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq transmission-packages
      '(transmission))

(defun transmission/init-transmission ()
  (use-package transmission
    :defer t
    :init (progn
            (spacemacs/set-leader-keys "at" 'transmission)
            (spacemacs/declare-prefix-for-mode 'transmission-mode "ma" "add")
            (spacemacs/declare-prefix-for-mode 'transmission-mode "mg" "goto")
            (spacemacs/declare-prefix-for-mode 'transmission-mode "ms" "set")
            (spacemacs/declare-prefix-for-mode 'transmission-mode "mt" "toggle")
            (spacemacs/set-leader-keys-for-major-mode 'transmission-mode
              "gt" 'transmission-files
              "at" 'transmission-add
              "sd" 'transmission-set-download
              "gp" 'transmission-peers
              "gi" 'transmission-info
              "sl" 'transmission-set-ratio
              "m"  'transmission-move
              "r"  'transmission-remove
              "ts" 'transmission-toggle
              "aa" 'transmission-trackers-add
              "su" 'transmission-set-upload
              "v"  'transmission-verify
              "q"  'transmission-quit
              "sb" 'transmission-set-bandwidth-priority)
            (spacemacs/declare-prefix-for-mode 'transmission-info-mode "ma" "add")
            (spacemacs/declare-prefix-for-mode 'transmission-info-mode "mg" "goto")
            (spacemacs/declare-prefix-for-mode 'transmission-info-mode "ms" "set")
            (spacemacs/set-leader-keys-for-major-mode 'transmission-info-mode
              "c"  'transmission-copy-magnet
              "sd" 'transmission-set-torrent-download
              "gp" 'transmission-peers
              "sl" 'transmission-set-torrent-ratio
              "m"  'transmission-move
              "aa" 'transmission-trackers-add
              "T"  'transmission-trackers-remove
              "su" 'transmission-set-torrent-upload
              "sp" 'transmission-set-bandwidth-priority)
            (spacemacs/declare-prefix-for-mode 'transmission-files-mode "mg" "goto")
            (spacemacs/declare-prefix-for-mode 'transmission-files-mode "mm" "mark")
            (spacemacs/declare-prefix-for-mode 'transmission-files-mode "ms" "set")
            (spacemacs/set-leader-keys-for-major-mode 'transmission-files-mode
              "gf" 'transmission-find-file
              "r"  'transmission-files-command
              "gp" 'transmission-peers
              "gi" 'transmission-info
              "mm" 'transmission-move
              "mu" 'transmission-files-unwant
              "mw" 'transmission-files-want
              "sp" 'transmission-files-priority)
            (spacemacs/set-leader-keys-for-major-mode 'transmission-peers-mode
              "i" 'transmission-info))
    :config (when transmission-auto-refresh-all
              (setq transmission-refresh-modes '(transmission-mode
                                                 transmission-files-mode
                                                 transmission-info-mode
                                                 transmission-peers-mode)))))
