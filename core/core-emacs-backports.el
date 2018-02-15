;;; core-emacs-backports.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; nothing for now

(provide 'core-emacs-backports)

(when (version< emacs-version "26")
  ;; backport fix for macOS battery status
  ;; https://github.com/emacs-mirror/emacs/commit/25dca60d5e3b2447352b7c51496baefb4ccd579d#diff-d1b82d59371a01a39cca34f1f64c3447
  (with-eval-after-load 'battery
    (defun battery-pmset ()
      "Get battery status information using `pmset'.

The following %-sequences are provided:
%L Power source (verbose)
%B Battery status (verbose)
%b Battery status, empty means high, `-' means low,
   `!' means critical, and `+' means charging
%p Battery load percentage
%h Remaining time in hours
%m Remaining time in minutes
%t Remaining time in the form `h:min'"
      (let (power-source load-percentage battery-status battery-status-symbol
                         remaining-time hours minutes)
        (with-temp-buffer
          (ignore-errors (call-process "pmset" nil t nil "-g" "ps"))
          (goto-char (point-min))
          (when (re-search-forward "\\(?:Currentl?y\\|Now\\) drawing from '\\(AC\\|Battery\\) Power'" nil t)
            (setq power-source (match-string 1))
            (when (re-search-forward "^ -InternalBattery-0\\([ \t]+(id=[0-9]+)\\)*[ \t]+" nil t)
              (when (looking-at "\\([0-9]\\{1,3\\}\\)%")
                (setq load-percentage (match-string 1))
                (goto-char (match-end 0))
                (cond ((looking-at "; charging")
                       (setq battery-status "charging"
                             battery-status-symbol "+"))
                      ((< (string-to-number load-percentage) battery-load-critical)
                       (setq battery-status "critical"
                             battery-status-symbol "!"))
                      ((< (string-to-number load-percentage) battery-load-low)
                       (setq battery-status "low"
                             battery-status-symbol "-"))
                      (t
                       (setq battery-status "high"
                             battery-status-symbol "")))
                (when (re-search-forward "\\(\\([0-9]+\\):\\([0-9]+\\)\\) remaining"  nil t)
                  (setq remaining-time (match-string 1))
                  (let ((h (string-to-number (match-string 2)))
                        (m (string-to-number (match-string 3))))
                    (setq hours (number-to-string (+ h (if (< m 30) 0 1)))
                          minutes (number-to-string (+ (* h 60) m)))))))))
        (list (cons ?L (or power-source "N/A"))
              (cons ?p (or load-percentage "N/A"))
              (cons ?B (or battery-status "N/A"))
              (cons ?b (or battery-status-symbol ""))
              (cons ?h (or hours "N/A"))
              (cons ?m (or minutes "N/A"))
              (cons ?t (or remaining-time "N/A")))))))
