;;; funcs.el --- Chrome Layer functions File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//flymd-start-browser (browser url)
  (let ((process-environment (browse-url-process-environment)))
    (apply 'start-process
           "flymd" nil
           browser
           (list "--new-window" "--allow-file-access-from-files" url))))

(defun spacemacs//flymd-browser-function (url)
  (cond
   (chrome-exec-path
    (spacemacs//flymd-start-browser chrome-exec-path url))
   ((executable-find "chromium")
    (spacemacs//flymd-start-browser
     (executable-find "chromium") url))
   ((executable-find "google-chrome")
    (spacemacs//flymd-start-browser
     (executable-find "google-chrome") url))
   ((executable-find "google-chrome-stable")
    (spacemacs//flymd-start-browser
     (executable-find "google-chrome-stable") url))
   (t (message "no useful browser"))))
