;;; packages.el --- google-calendar layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Markus Johansson <markus.johansson@me.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

(defconst google-calendar-packages
  '(org-gcal
    calfw))

(defun google-calendar/init-org-gcal ()
  "Initializes org-gcal and adds keybindings for it's exposed functions"
  (use-package org-gcal
    :init
    (progn
      (spacemacs/set-leader-keys
        "agr" 'org-gcal-refresh-token
        "ags" 'org-gcal-sync
        "agf" 'org-gcal-fetch))
    :config
    (setq org-gcal-down-days 365)   ;; Set org-gcal to download events a year in advance
    (add-hook 'after-init-hook 'org-gcal-fetch)
    (add-hook 'kill-emacs-hook 'org-gcal-sync)
    (add-hook 'org-capture-after-finalize-hook 'google-calendar/sync-cal-after-capture)
    (run-with-idle-timer 600 t 'google-calendar/org-gcal-update)))

(defun google-calendar/init-calfw ()
  "Initialize calfw"

  (use-package calfw
    :init
    (evil-set-initial-state 'cfw:calendar-mode 'normal)
    (defvar calfw-org-agenda-view "a"
      "Key for opening the current week or day view in org-agenda.")

    (defvar calfw-window-conf nil
      "Current window config")

    (defcustom calfw-restore-windows-after-quit 't
      "Non-nil means restore window configuration upon exiting calfw calendar view.
The window configuration is stored when entering calendar view.
When the view is exited and this option is set the previous layout is restored."
      :type 'boolean)

    :config
    (evil-make-overriding-map cfw:calendar-mode-map)
    (define-key cfw:calendar-mode-map "N" 'cfw:navi-next-month-command)
    (define-key cfw:calendar-mode-map "P" 'cfw:navi-previous-month-command)
    (define-key cfw:calendar-mode-map "c" 'cfw:org-capture)
    (define-key cfw:calendar-mode-map "v" 'cfw:org-open-agenda-day))

  (use-package calfw-org
    :init
    (spacemacs/set-leader-keys
      "agc" 'google-calendar/calfw-view)
    (spacemacs/declare-prefix "agc" "open-org-calendar")

    :config
    (define-key cfw:org-schedule-map "q" 'google-calendar/calfw-restore-windows)

    :commands
    (cfw:open-org-calendar)))

;;; packages.el ends here
