;;; extensions.el --- geolocation configuration File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Uri Sharf <uri.sharf@me.com>
;; URL: https://github.com/usharf/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq geolocation-post-extensions '(theme-changer))

(defun geolocation/init-theme-changer ()
  "Initialize theme-changer"
  (use-package theme-changer
    :if (and geolocation-enable-automatic-theme-changer
             (> (length dotspace-macs-themes) 1))))


