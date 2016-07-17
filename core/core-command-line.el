;;; core-command-line.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar spacemacs-force-resume-layouts nil
  "If non-nil force the current emacs instance to resume layouts
  at start time despite the value of `dotspacemacs-auto-resume-layouts'.")

(defvar spacemacs-insecure nil
  "If non-nil force Spacemacs to operate without secured protocols.")

(defun spacemacs//parse-command-line (args)
  "Handle Spacemacs specific command line arguments.
The reason why we don't use the Emacs hooks for processing user defined
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
           (setq spacemacs-debug-with-profile t)
           (setq spacemacs-debugp t))
          ("--timed-requires"
           (setq spacemacs-debug-with-timed-requires t)
           (when next-arg-digit
             (setq spacemacs-debug-timer-threshold next-arg-digit
                   i (1+ i)))
           (setq spacemacs-debugp t))
          ("--adv-timers"
           (setq spacemacs-debug-with-adv-timers t)
           (when next-arg-digit
             (setq spacemacs-debug-timer-threshold next-arg-digit
                   i (1+ 1)))
           (setq spacemacs-debugp t))
          ("--insecure"
           (setq spacemacs-insecure t))
          ("--no-layer"
           (setq configuration-layer-exclude-all-layers t))
          ("--distribution"
           (setq configuration-layer-force-distribution (intern (nth (1+ i) args))
                 i (1+ i)))
          ("--resume-layouts"
           (setq spacemacs-force-resume-layouts t))
          (_ (push arg new-args))))
      (setq i (1+ i)))
    (nreverse new-args)))

(setq command-line-args (spacemacs//parse-command-line command-line-args))

(provide 'core-command-line)
