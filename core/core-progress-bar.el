;;; core-progress-bar.el --- Space-macs Core File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3
(require 'core-space-macs-buffer)

(defvar space-macs-loading-char ?â–ˆ)
(defvar space-macs-loading-char-light ?â–‘)
(defvar space-macs-loading-string "")
(defvar space-macs-loading-counter 0)
(defvar space-macs-loading-value 0)
(defvar space-macs-loading-dots-chunk-count 5)
(defvar space-macs-loading-dots-count 0)
(defvar space-macs-loading-dots-chunk-size 0)
(defvar space-macs-loading-dots-chunk-threshold 0)

(defun space-macs/init-progress-bar (max)
  "Initialize the progress bar."
  (setq space-macs-loading-dots-count (window-total-size nil 'width))
  (setq space-macs-loading-dots-chunk-size (/ space-macs-loading-dots-count
                                             space-macs-loading-dots-chunk-count))
  (setq space-macs-loading-dots-chunk-threshold
        (- (/ max space-macs-loading-dots-chunk-count) 5))
  (setq space-macs-loading-counter 0)
  (setq space-macs-loading-value 0)
  (space-macs-buffer/set-mode-line (make-string space-macs-loading-dots-count
                                               space-macs-loading-char-light))
  (space-macs//redisplay))

(defun space-macs/update-progress-bar ()
  "Update progress bar by incrementing its value by 1.
Display the progress bar by chunks of size `space-macs-loading-dots-chunk-threshold'"
  (when (and (not noninteractive)
             (> space-macs-loading-dots-chunk-threshold 0)
             dotspace-macs-loading-progress-bar)
    (setq space-macs-loading-counter (1+ space-macs-loading-counter))
    (setq space-macs-loading-value (1+ space-macs-loading-value))
    (when (>= space-macs-loading-counter
              space-macs-loading-dots-chunk-threshold)
      (let* ((progress
              (max 0 (* space-macs-loading-dots-chunk-size
                        (floor (/ space-macs-loading-value
                                  space-macs-loading-dots-chunk-threshold)))))
             (remain (max 0 (- space-macs-loading-dots-count progress))))
        (setq space-macs-loading-counter 0)
        (setq space-macs-loading-string
              (concat (make-string progress space-macs-loading-char)
                      (make-string remain space-macs-loading-char-light)))
        (space-macs-buffer/set-mode-line space-macs-loading-string))
      (space-macs//redisplay))))

(provide 'core-progress-bar)


