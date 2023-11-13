;;; elfeed-log.el --- Elfeed's logging system -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defface elfeed-log-date-face
  '((t :inherit font-lock-type-face))
  "Face for showing the date in the elfeed log buffer."
  :group 'elfeed)

(defface elfeed-log-error-level-face
  '((t :foreground "red"))
  "Face for showing the `error' log level in the elfeed log buffer."
  :group 'elfeed)

(defface elfeed-log-warn-level-face
  '((t :foreground "goldenrod"))
  "Face for showing the `warn' log level in the elfeed log buffer."
  :group 'elfeed)

(defface elfeed-log-info-level-face
  '((t :foreground "deep sky blue"))
  "Face for showing the `info' log level in the elfeed log buffer."
  :group 'elfeed)

(defface elfeed-log-debug-level-face
  '((t :foreground "magenta2"))
  "Face for showing the `debug' log level in the elfeed log buffer."
  :group 'elfeed)

(defvar elfeed-log-buffer-name "*elfeed-log*"
  "Name of buffer used for logging Elfeed events.")

(defvar elfeed-log-level 'info
  "Lowest type of messages to be logged.")

(defun elfeed-log-buffer ()
  "Returns the buffer for `elfeed-log', creating it as needed."
  (let ((buffer (get-buffer elfeed-log-buffer-name)))
    (if buffer
        buffer
      (with-current-buffer (generate-new-buffer elfeed-log-buffer-name)
        (special-mode)
        (current-buffer)))))

(defun elfeed-log--level-number (level)
  "Return a relative level number for LEVEL."
  (cl-case level
    (debug -10)
    (info 0)
    (warn 10)
    (error 20)
    (otherwise -10)))

(defun elfeed-log (level fmt &rest objects)
  "Write log message FMT at LEVEL to Elfeed's log buffer.

LEVEL should be a symbol: debug, info, warn, error.
FMT must be a string suitable for `format' given OBJECTS as arguments."
  (let ((log-buffer (elfeed-log-buffer))
        (log-level-face (cl-case level
                          (debug 'elfeed-log-debug-level-face)
                          (info 'elfeed-log-info-level-face)
                          (warn 'elfeed-log-warn-level-face)
                          (error 'elfeed-log-error-level-face)))
        (inhibit-read-only t))
    (when (>= (elfeed-log--level-number level)
              (elfeed-log--level-number elfeed-log-level))
      (with-current-buffer log-buffer
        (goto-char (point-max))
        (insert
         (format
          (concat "[" (propertize "%s" 'face 'elfeed-log-date-face) "] "
                  "[" (propertize "%s" 'face log-level-face) "]: %s\n")
          (format-time-string "%Y-%m-%d %H:%M:%S")
          level
          (apply #'format fmt objects)))))))

(provide 'elfeed-log)

;;; elfeed-log.el ends here
