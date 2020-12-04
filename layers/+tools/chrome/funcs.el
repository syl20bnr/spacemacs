;;; funcs.el --- Chrome Layer functions File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//flymd-start-browser (browser url)
  (let ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           "flymd" nil
           browser
           (list "--new-window" "--allow-file-access-from-files" url))))

(defun space-macs//flymd-browser-function (url)
  (cond
   (chrome-exec-path
    (space-macs//flymd-start-browser chrome-exec-path url))
   ((executable-find "chromium")
    (space-macs//flymd-start-browser
     (executable-find "chromium") url))
   ((executable-find "google-chrome")
    (space-macs//flymd-start-browser
     (executable-find "google-chrome") url))
   ((executable-find "google-chrome-stable")
    (space-macs//flymd-start-browser
     (executable-find "google-chrome-stable") url))
   (t (message "no useful browser"))))


