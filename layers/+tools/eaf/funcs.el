;;; funcs.el --- eaf Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Daniel Nicolai <dalanicolai@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

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
        (when (not browser)
          (eaf-restart-process))))))

(defun spacemacs/open-with-eaf ()
  (interactive)
  (eaf-open (buffer-file-name)))
