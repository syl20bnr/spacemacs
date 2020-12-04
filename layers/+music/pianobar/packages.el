;;; packages.el --- pianobar Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Leo Littlebook  <texas.cyberthal@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq pianobar-packages
  '(
    pianobar
    ))

(defun pianobar/init-pianobar ()
  (use-package pianobar
    :defer t
    :init (progn
            (space-macs/declare-prefix "am" "music")
            (space-macs/declare-prefix "amp" "Pianobar")
            (space-macs/set-leader-keys
              "ampp" 'pianobar-play-or-pause
              "ampn" 'pianobar-next-song
              "amp+" 'pianobar-love-current-song
              "amp-" 'pianobar-ban-current-song
              "ampt" 'pianobar-shelve-current-song
              "amps" 'pianobar-change-station
              ))))


