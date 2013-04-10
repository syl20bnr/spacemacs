(require 'flymake)

;; from http://stackoverflow.com/questions/2571436/emacs-annoying-flymake-dialog-box
(defun flymake-display-warning (warning)
  "Display a warning to the user in the mini-buffer instead of a dialog box"
  (message warning))

;; from http://www.emacswiki.org/emacs/FlyMake
(defun safer-flymake-find-file-hook ()
  "Don't barf if we can't open this flymake file"
  (let ((flymake-filename
         (flymake-create-temp-inplace (buffer-file-name) "flymake")))
    (if (file-writable-p flymake-filename)
        (flymake-find-file-hook)
      (message
       (format
        "Couldn't enable flymake; permission denied on %s" flymake-filename)))))
(add-hook 'find-file-hook 'safer-flymake-find-file-hook)

;; from http://stackoverflow.com/questions/6110691/is-there-a-way-to-make-flymake-to-compile-only-when-i-save
(eval-after-load "flymake"
  '(progn
    (defun flymake-after-change-function (start stop len)
      "Start syntax check for current buffer if it isn't already running."
      ;; Do nothing, don't want to run checks until I save.
      )))
