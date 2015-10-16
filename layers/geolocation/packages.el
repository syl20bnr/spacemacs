;;; packages.el --- geolocation configuration File for Spacemacs
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

(setq geolocation-packages
    '(
      rase
      sunshine
      ))

(defun geolocation/init-rase ()
  (use-package rase
    :defer t
    :init
    (progn
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
      (rase-start t)
      )))

(defun geolocation/init-sunshine ()
  "Initialize sunshine"
  (use-package sunshine
    :if geolocation-enable-weather-forecast
    :commands (sunshine-forecast sunshine-quick-forecast)
    :init
    (progn
      (evil-leader/set-key
        "aw" 'sunshine-forecast
        "aW" 'sunshine-quick-forecast)

      (evilify sunshine-mode sunshine-mode-map
               (kbd "q") 'quit-window
               (kbd "i") 'sunshine-toggle-icons))
    ))
