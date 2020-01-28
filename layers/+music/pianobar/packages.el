;;; packages.el --- pianobar Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Leo Littlebook  <texas.cyberthal@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
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
            (spacemacs/declare-prefix "am" "music")
            (spacemacs/declare-prefix "amp" "Pianobar")
            (spacemacs/set-leader-keys
              "ampp" 'pianobar-play-or-pause
              "ampn" 'pianobar-next-song
              "amp+" 'pianobar-love-current-song
              "amp-" 'pianobar-ban-current-song
              "ampt" 'pianobar-shelve-current-song
              "amps" 'pianobar-change-station
              ))))
