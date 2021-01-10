;;; theme-changer.el --- geolocation configuration File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Uri Sharf <uri.sharf@me.com>
;; URL: https://github.com/usharf/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(require 'rase)

(defun theme-changer/switch-themes (sun-event &optional first-run)
  "Switch first two themes in dotspacemacs-themes on sunrise and sunset."
  (if first-run				                ; set theme on initialization
      (cond ((memq sun-event '(sunrise midday))
             (load-theme (nth 0 dotspacemacs-themes) t))
            ((memq sun-event '(sunset midnight))
             (load-theme (nth 1 dotspacemacs-themes) t)))
    (cond ((eq sun-event 'sunrise)    ; after initialization deal only with
                                        ; sunrise and sunset
           (load-theme (nth 0 dotspacemacs-themes) t))
          ((eq sun-event 'sunset)
           (load-theme (nth 1 dotspacemacs-themes) t))))
  )

(with-eval-after-load 'rase ; probably redaundant because it's a post extension
  (add-hook 'rase-functions 'theme-changer/switch-themes)
  (rase-start t))

(provide 'theme-changer)
