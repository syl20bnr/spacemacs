;;; core-command-line.el --- Spacemacs Core File -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
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


(defvar spacemacs-force-resume-layouts nil
  "If non-nil force the current emacs instance to resume layouts
  at start time despite the value of `dotspacemacs-auto-resume-layouts'.")

(defvar spacemacs-insecure nil
  "If non-nil force Spacemacs to operate without secured protocols.")

(defvar spacemacs-sync-packages t
  "If non-nil packages are synchronized when the configuration layer system is
loaded.")

(defvar spacemacs-force-dump nil
  "If non-nil then force a redump of Emacs.")

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
          ("--no-package-sync"
           (setq spacemacs-sync-packages nil))
          ("--force-dump"
           (setq spacemacs-force-dump t))
          (_ (push arg new-args))))
      (setq i (1+ i)))
    (nreverse new-args)))

(provide 'core-command-line)
