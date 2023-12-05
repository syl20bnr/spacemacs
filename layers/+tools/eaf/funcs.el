;;; funcs.el --- eaf Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Daniel Nicolai <dalanicolai@gmail.com>
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


(defun duckduckgo ()
  (interactive)
  (eaf-open-browser "www.duckduckgo.com"))
(defun wikipedia ()
  (interactive)
  (eaf-open-browser "www.wikipedia.com"))
(defun youtube ()
  (interactive)
  (eaf-open-browser "www.youtube.com"))

(defun eaf-toggle-dark-mode ()
  (interactive)
  (if (not (eq major-mode 'eaf-mode))
      (message "Not in eaf-mode buffer")
    (let* ((app-name eaf--buffer-app-name)
           (app-dark-mode
            (intern
             (format "eaf-%s-dark-mode"
                     (if (string= app-name "pdf-viewer")
                         "pdf"
                       app-name))))
           (current-state (cdr (assoc app-dark-mode eaf-var-list))))
      (let ((browser (string= app-name "browser")))
        (cond ((member current-state '("true" "follow" "ignore"))
               (eaf-set app-dark-mode "false")
               (when browser
                 (eaf-proxy-refresh_page)))
              (t
               (eaf-set app-dark-mode "true")
               (when browser
                 (eaf-proxy-insert_or_dark_mode))))
        (unless browser
          (eaf-restart-process))))))

(defun spacemacs/open-with-eaf ()
  (interactive)
  (eaf-open (buffer-file-name)))
