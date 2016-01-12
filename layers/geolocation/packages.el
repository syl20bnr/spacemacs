;;; packages.el --- Geolocation Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq geolocation-packages
    '(
      osx-location
      sunshine
      theme-changer
      popwin
      ))

(defun geolocation/init-osx-location ()
  "Initialize osx-location"
  (use-package osx-location
    :if geolocation-enable-osx-location-service-support
    :init
    (progn
      (add-hook 'osx-location-changed-hook
                (lambda ()
                  (setq calendar-latitude osx-location-latitude
                        calendar-longitude osx-location-longitude)
                  (unless (bound-and-true-p calendar-location-name)
                    (setq calendar-location-name
                          (format "%s, %s"
                                  osx-location-latitude
                                  osx-location-longitude)))))
      (osx-location-watch))))

(defun geolocation/init-sunshine ()
  "Initialize sunshine"
  (use-package sunshine
    :if geolocation-enable-weather-forecast
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys
        "aw" 'sunshine-forecast
        "aW" 'sunshine-quick-forecast)

      (evilified-state-evilify sunshine-mode sunshine-mode-map
        (kbd "q") 'quit-window
        (kbd "i") 'sunshine-toggle-icons))
    :config
    ;; just in case location was not set by user, or on OS X,
    ;; if wasn't set up automatically, will not work with Emac's
    ;; default for ;; `calendar-location-name'
    (when (not (boundp 'sunshine-location))
      (setq sunshine-location (format "%s, %s"
                                      calendar-latitude
                                      calendar-longitude)))))

(defun geolocation/init-theme-changer ()
  "Initialize theme-changer"
  (use-package theme-changer
    :if geolocation-enable-automatic-theme-changer
    :config
    (progn
      (when (> (length dotspacemacs-themes) 1)
        (change-theme (nth 0 dotspacemacs-themes)
                      (nth 1 dotspacemacs-themes))))))

(defun geolocation/post-init-popwin ()
  ;; Pin the weather forecast to the bottom window
  (push '("*Sunshine*" :dedicated t :position bottom)
        popwin:special-display-config))
