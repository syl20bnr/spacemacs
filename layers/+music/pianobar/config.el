;;; config.el --- Pianobar Layer config File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Leo Littlebook  <texas.cyberthal@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar pianobar-config 'nil
  "Set to t if your pianobar config contains your username and
  password.")

(defvar pianobar-command "pianobar"
  "If pianobar is not in your path, set this variable to
  pianobar's path.")

(defvar pianobar-station 'nil
  "Automatically connect to this station on login. Value must be
  a string containing the number used to select the station you
  want through Pianobar. The number is many digits long.")
