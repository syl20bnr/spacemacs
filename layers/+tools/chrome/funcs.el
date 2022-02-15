;;; funcs.el --- Chrome Layer functions File
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
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
