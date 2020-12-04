;;; core-command-line.el --- Space-macs Core File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defvar space-macs-force-resume-layouts nil
  "If non-nil force the current e-macs instance to resume layouts
  at start time despite the value of `dotspace-macs-auto-resume-layouts'.")

(defvar space-macs-insecure nil
  "If non-nil force Space-macs to operate without secured protocols.")

(defvar space-macs-sync-packages t
  "If non-nil packages are synchronized when the configuration layer system is
loaded.")

(defvar space-macs-force-dump nil
  "If non-nil then force a redump of e-macs.")

(defun space-macs//parse-command-line (args)
  "Handle Space-macs specific command line arguments.
The reason why we don't use the e-macs hooks for processing user defined
arguments is that we want to process these arguments as soon as possible."
  (let ((i 0) new-args)
    (while (< i (length args))
      (let ((arg (nth i args))
            (next-arg-digit
             (when (< (1+ i) (length args))
               (string-to-number (nth (1+ i) args)))))
        (when (or (null next-arg-digit) (= 0 next-arg-digit))
          (setq next-arg-digit nil))
        (pcase arg
          ("--profile"
           (setq space-macs-debug-with-profile t)
           (setq space-macs-debugp t))
          ("--timed-requires"
           (setq space-macs-debug-with-timed-requires t)
           (when next-arg-digit
             (setq space-macs-debug-timer-threshold next-arg-digit
                   i (1+ i)))
           (setq space-macs-debugp t))
          ("--adv-timers"
           (setq space-macs-debug-with-adv-timers t)
           (when next-arg-digit
             (setq space-macs-debug-timer-threshold next-arg-digit
                   i (1+ 1)))
           (setq space-macs-debugp t))
          ("--insecure"
           (setq space-macs-insecure t))
          ("--no-layer"
           (setq configuration-layer-exclude-all-layers t))
          ("--distribution"
           (setq configuration-layer-force-distribution (intern (nth (1+ i) args))
                 i (1+ i)))
          ("--resume-layouts"
           (setq space-macs-force-resume-layouts t))
          ("--no-package-sync"
           (setq space-macs-sync-packages nil))
          ("--force-dump"
           (setq space-macs-force-dump t))
          (_ (push arg new-args))))
      (setq i (1+ i)))
    (nreverse new-args)))

(provide 'core-command-line)


