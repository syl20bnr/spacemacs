;;; config.el --- geolocation configuration File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Uri Sharf & Contributors
;;
;; Author: Uri Sharf <uri.sharf@me.com>
;; URL: https://github.com/usharf/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar geolocation-enable-weather-forecast nil
  "If non nil enable the weather forecast service.")

(defvar geolocation-enable-theme-changer nil
  "If non nil, enable the automatic change of theme based on sunset/sunrise in current location.")

(defvar geolocation-enable-location-services nil
  "If non nil, enable platform dependent geolocation service")
