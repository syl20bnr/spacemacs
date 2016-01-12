;;; core-display-init.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar spacemacs--after-display-system-init-list '()
  "List of functions to be run after the display system is initialized.")

(defadvice server-create-window-system-frame
    (after spacemacs-init-display activate)
  "After Emacs server creates a frame, run functions queued in
`SPACEMACS--AFTER-DISPLAY-SYSTEM-INIT-LIST' to do any setup that needs to have
the display system initialized."
  (progn
    (dolist (fn (reverse spacemacs--after-display-system-init-list))
      (funcall fn))
    (ad-disable-advice 'server-create-window-system-frame
                       'after
                       'spacemacs-init-display)
    (ad-activate 'server-create-window-system-frame)))

(defmacro spacemacs|do-after-display-system-init (&rest body)
  "If the display-system is initialized, run `BODY', otherwise,
add it to a queue of actions to perform after the first graphical frame is
created."
  `(let ((init (cond ((boundp 'ns-initialized) 'ns-initialized)
                     ((boundp 'w32-initialized) 'w32-initialized)
                     ((boundp 'x-initialized) 'x-initialized)
                     (t 't))))           ; fallback to normal loading behavior
     (if (symbol-value init)
         (progn
           ,@body)
       (push (lambda () ,@body) spacemacs--after-display-system-init-list))))

(provide 'core-display-init)
