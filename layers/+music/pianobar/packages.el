;;; packages.el --- pianobar Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Leo Littlebook  <texas.cyberthal@gmail.com>
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


(defconst pianobar-packages
  '(
    pianobar))


(defun pianobar/init-pianobar ()
  (use-package pianobar
    :defer t
    :init
    (spacemacs/declare-prefix
      "am"  "music"
      "amp" "Pianobar")
    (spacemacs/set-leader-keys
      "ampp" 'pianobar-play-or-pause
      "ampn" 'pianobar-next-song
      "amp+" 'pianobar-love-current-song
      "amp-" 'pianobar-ban-current-song
      "ampt" 'pianobar-shelve-current-song
      "amps" 'pianobar-change-station)))
