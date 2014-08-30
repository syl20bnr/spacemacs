(use-package weather
  :commands weather-report
  :config
  (progn 
    ;; pinit files come from my dropbox folder
    (require 'pinit-weather nil 'noerror)
    (setq weather-distance-unit "km")))
