;;; funcs.el --- geolocation functions File for Spacemacs
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Eivind Fonn <evfonn@gmail.com>
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



(defun spacemacs//osx-location-changed ()
  (let ((location-changed-p nil)
        (_longitude (/ (truncate (* osx-location-longitude 10)) 10.0)) ; one decimal point, no rounding
        (_latitdue (/ (truncate (* osx-location-latitude 10)) 10.0)))
    (unless (equal (bound-and-true-p calendar-longitude) _longitude)
      (setq calendar-longitude _longitude
            location-changed-p t))
    (unless (equal (bound-and-true-p  calendar-latitude) _latitdue)
      (setq calendar-latitude _latitdue
            location-changed-p t))
    (when (and (configuration-layer/layer-used-p 'geolocation) location-changed-p)
      (message "Location changed %s %s (restarting rase-timer)" calendar-latitude calendar-longitude)
      (rase-start t))))

(defun spacemacs//osx-location-changed-rase ()
  (setq calendar-latitude osx-location-latitude
        calendar-longitude osx-location-longitude)
  (unless (bound-and-true-p calendar-location-name)
    (setq calendar-location-name
          (format "%s, %s"
                  osx-location-latitude
                  osx-location-longitude))))

(defun geolocation//activate-theme-changer ()
  (unless (bound-and-true-p calendar-longitude)
    (user-error "calendar-longitude is not set"))
  (unless (bound-and-true-p calendar-latitude)
    (user-error "calendar-latitude is not set"))
  (when (> (length dotspacemacs-themes) 1)
    (change-theme (nth 0 dotspacemacs-themes)
                  (nth 1 dotspacemacs-themes))))
