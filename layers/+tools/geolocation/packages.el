;;; packages.el --- geolocation configuration File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Uri Sharf <uri.sharf@me.com>
;; URL: https://github.com/usharf/spacemacs
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


(defconst geolocation-packages
  '(
    (osx-location :toggle (and geolocation-enable-location-service
                               (spacemacs/system-is-mac)))
    popwin
    (rase :toggle (and geolocation-enable-location-service
                       (spacemacs/system-is-mac)))
    (sunshine :toggle geolocation-enable-weather-forecast)
    (theme-changer :toggle geolocation-enable-automatic-theme-changer)))

(defun geolocation/init-osx-location ()
  "Initialize osx-location"
  (use-package osx-location
    :defer t
    :init
    (add-hook 'osx-location-changed-hook 'spacemacs//osx-location-changed)
    (osx-location-watch)))

(defun geolocation/init-rase ()
  (use-package rase
    :defer t
    :init
    (add-hook 'osx-location-changed-hook 'spacemacs//osx-location-changed-rase)
    (osx-location-watch)
    (defadvice rase-start (around test-calendar activate)
      "Don't call `raise-start' if `calendar-latitude' or
`calendar-longitude' are not bound yet, or still nil.

This is setup this way because `rase.el' does not test these
values, and will fail under such conditions, when calling
`solar.el' functions.

Also, it allows users who enabled service such as `osx-location'
to not have to set these variables manually when enabling this layer."
      (if (and (bound-and-true-p calendar-longitude)
               (bound-and-true-p calendar-latitude))
          ad-do-it))
    (rase-start t)))

(defun geolocation/init-sunshine ()
  "Initialize sunshine"
  (use-package sunshine
    :commands (sunshine-forecast sunshine-quick-forecast)
    :init
    (spacemacs/declare-prefix "atg" "geolocation")
    (spacemacs/set-leader-keys
      "atgw" 'sunshine-forecast
      "atgW" 'sunshine-quick-forecast)
    :config
    (evilified-state-evilify-map sunshine-mode-map
      :mode sunshine-mode
      :bindings
      (kbd "q") 'quit-window
      (kbd "i") 'sunshine-toggle-icons)

    ;; just in case location was not set by user, or on macOS,
    ;; if wasn't set up automatically, will not work with Emacs'
    ;; default for `calendar-location-name'
    (unless (boundp 'sunshine-location)
      (setq sunshine-location (format "%s, %s"
                                      calendar-latitude
                                      calendar-longitude)))))

(defun geolocation/init-theme-changer ()
  "Initialize theme-changer"
  (use-package theme-changer
    :init
    (spacemacs/defer-until-after-user-config #'geolocation//activate-theme-changer)))

(defun geolocation/pre-init-popwin ()
  "Pin the weather forecast to the bottom window"
  (spacemacs|use-package-add-hook popwin
    :post-config
    (push '("*Sunshine*" :dedicated t :position bottom)
          popwin:special-display-config)))
