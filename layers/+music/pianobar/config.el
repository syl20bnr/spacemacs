;;; config.el --- Pianobar Layer config File for Spacemacs
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
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
