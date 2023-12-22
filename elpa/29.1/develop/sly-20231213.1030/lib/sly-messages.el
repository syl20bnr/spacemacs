;;; sly-messages.el --- Messages, errors, echo-area and visual feedback utils for SLY  -*- lexical-binding: t; -*-

;; Copyright (C) 2014  João Távora

;; Author: João Távora <joaotavora@gmail.com>
;; Keywords: 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

(require 'cl-lib)

(defvar sly--last-message nil)

(defun sly-message (format-string &rest args)
  "Like `message', but use a prefix."
  (let ((body (apply #'format format-string args)))
    (setq sly--last-message (format "[sly] %s" body))
    (message "%s" sly--last-message)))

(add-hook 'echo-area-clear-hook
          'sly--message-clear-last-message)

(defun sly--message-clear-last-message ()
  (setq sly--last-message nil))

(defun sly-temp-message (wait sit-for format &rest args)
  "Wait WAIT seconds then display a message for SIT-FOR seconds.
A nil value for WAIT means \"now\".
SIT-FOR is has the semantincs of `minibuffer-message-timeout', which see."
  (run-with-timer
   wait nil
   #'(lambda ()
       (let ((existing sly--last-message)
             (text (apply #'format format args)))
         (if (minibuffer-window-active-p (minibuffer-window))
             (let ((minibuffer-message-timeout sit-for))
               (minibuffer-message "[sly] %s" text))
           (message "[sly] %s" text) ; don't sly-message here
           (run-with-timer
            sit-for
            nil
            #'(lambda ()
                ;; restore the message
                (when existing
                  (message "%s" existing)))))))))

(defun sly-warning (format-string &rest args)
  (display-warning '(sly warning) (apply #'format format-string args)))

(defun sly-error (format-string &rest args)
  (apply #'error (format "[sly] %s" format-string) args))

(defun sly-user-error (format-string &rest args)
  (apply #'user-error (format "[sly] %s" format-string) args))

(defun sly-display-oneliner (format-string &rest format-args)
  (let* ((msg (apply #'format format-string format-args)))
    (unless (minibuffer-window-active-p (minibuffer-window))
      (sly-message (sly-oneliner msg)))))

(defun sly-oneliner (string)
  "Return STRING truncated to fit in a single echo-area line."
  (substring string 0 (min (length string)
                           (or (cl-position ?\n string) most-positive-fixnum)
                           (1- (window-width (minibuffer-window))))))

(defun sly-y-or-n-p (format-string &rest args)
  (let ((prompt (apply #'format (concat "[sly] "
                                        format-string)
                       args)))
    (y-or-n-p prompt)))


;;; Flashing the region
;;;
(defvar sly-flash-inhibit nil
  "If non-nil `sly-flash-region' does nothing")

(defvar sly--flash-overlay (make-overlay 0 0))
(overlay-put sly--flash-overlay 'priority 1000)

(cl-defun sly-flash-region (start end &key
                                  timeout
                                  face
                                  times
                                  (pattern '(0.2)))
  "Temporarily highlight region from START to END."
  (if pattern
      (cl-assert (and (null times) (null timeout))
                 nil
                 "If PATTERN is supplied, don't supply TIMES or TIMEOUT")
    (setq pattern (make-list (* 2 times) timeout)))
  (unless sly-flash-inhibit
    (let ((buffer (current-buffer)))
      (move-overlay sly--flash-overlay start end buffer)
      (cl-labels
          ((on () (overlay-put sly--flash-overlay 'face (or face 'highlight)))
           (off () (overlay-put sly--flash-overlay 'face nil))
           (relevant-p ()
                       (equal (list start end buffer)
                              (list (overlay-start sly--flash-overlay)
                                    (overlay-end sly--flash-overlay)
                                    (overlay-buffer sly--flash-overlay))))
           (onoff ()
                  (when (and pattern (relevant-p))
                    (on)
                    (run-with-timer (pop pattern)
                                    nil
                                    (lambda ()
                                      (when (relevant-p)
                                        (off)
                                        (when pattern
                                          (run-with-timer
                                           (pop pattern)
                                           nil
                                           (lambda () (onoff))))))))))
        (onoff)))))

(provide 'sly-messages)
;;; sly-messages.el ends here
