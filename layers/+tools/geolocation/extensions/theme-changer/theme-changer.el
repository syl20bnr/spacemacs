;;; theme-changer.el --- geolocation configuration File for Spacemacs
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
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
