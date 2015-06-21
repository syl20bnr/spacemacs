;;; packages.el --- location Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq location-packages
    '(
      osx-location
      sunshine
      theme-changer
      ))

(require 'solar)

(defun location/init-osx-location ()
  "Initialize osx-location"
  (use-package osx-location
    :if enable-osx-location-service-support
    :defer t
    :init
    (progn
      (add-hook 'osx-location-changed-hook
                (lambda ()
                  (setq calendar-latitude osx-location-latitude
                        calendar-longitude osx-location-longitude)
                  (unless calendar-location-name
                    (setq calendar-location-name
                          (format "%s, %s" osx-location-latitude osx-location-longitude)))))
      (osx-location-watch))))

(defun location/init-sunshine ()
  "Initialize sunshine"
  (use-package sunshine
    :if enable-weather-forecast
    :defer t
    :init
    (progn
      (evil-leader/set-key
        "aw" 'sunshine-forecast
        "aW" 'sunshine-quick-forecast)

      (evilify sunshine-mode sunshine-mode-map
               (kbd "q") 'quit-window
               (kbd "i") 'sunshine-toggle-icons))
    :config
    ;; just in case location was not set by user, or on OS X, if wasn't set up
    ;; automatically, will not work with Emac's default for `calendar-location-name'
    (when (not (boundp 'sunshine-location))
      (setq sunshine-location (format "%s, %s" calendar-latitude calendar-longitude)))))

(defun location/init-theme-changer ()
  "Initialize theme-changer"
  (use-package theme-changer
    :if enable-automatic-theme-managment
    :defer t
    :init
    (when (car dotspacemacs-themes) ;; at least one theme defined in user's `dotspacemacs-themes'
        (setq spacemacs/day-theme (nth 0 dotspacemacs-themes))
        (setq spacemacs/night-theme (nth 1 dotspacemacs-themes)))
    :config
    (change-theme spacemacs/day-theme spacemacs/night-theme) ;; otherwise use defaults fron config.el
    ))
