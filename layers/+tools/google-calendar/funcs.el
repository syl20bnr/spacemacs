;;; funcs.el --- google-calendar Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Markus Johansson markus.johansson@me.com
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(when (configuration-layer/package-usedp 'org-gcal)
  (defun google-calendar/org-gcal-update ()
    "Refresh OAuth token, fetch and sync calendar"
    (interactive)
    (org-gcal-refresh-token)
    (org-gcal-fetch))

  (defun google-calendar/sync-cal-after-capture ()
    "Sync calendar after a event was added with org-capture.
The function can be run automatically with the 'org-capture-after-finalize-hook'."
    (when-let ((cal-files (mapcar 'f-expand (mapcar 'cdr org-gcal-file-alist)))
               (capture-target (f-expand (car (cdr (org-capture-get :target)))))
               (cal-file-exists (and (mapcar 'f-file? cal-files)))
               (capture-target-isfile (eq (car (org-capture-get :target)) 'file))
               (capture-target-is-cal-file (member capture-target cal-files)))
      (org-gcal-refresh-token)
      (org-gcal-post-at-point))))

(when (configuration-layer/package-usedp 'calfw)
  (defun google-calendar/calfw-view ()
    "Open calfw calendar view."
    (interactive)
    (let ((org-agenda-window-setup calfw-calendar-window-setup))
      (google-calendar/calfw-prepare-window)
      ;;; If non nil, overload agenda window setup with the desired setup for calfw
      (org-agenda nil calfw-org-agenda-view)
      (cfw:open-org-calendar)))

  (defun google-calendar/calfw-prepare-window ()
    "Store current window layout in before opening calfw."
    (when-let ((is-not-cal-buffer (not (member (buffer-name) '("*cfw-calendar*" "*Org Agenda*"))))
               (is-not-conf (not (equal calfw-window-conf '(current-window-configuration)))))
      (setq calfw-window-conf (current-window-configuration))))

  (defun google-calendar/calfw-restore-windows ()
    "Bury current buffer and restore window layout."
    (interactive)
    (bury-buffer)
    (if (and (not (eq calfw-window-conf nil))
             (eq calfw-restore-windows-after-quit 't))
        (progn (set-window-configuration calfw-window-conf)
               (setq calfw-window-conf nil)))))
