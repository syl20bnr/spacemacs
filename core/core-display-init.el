;;; core-display-init.el --- Spacemacs Core File -*- lexical-binding: t -*-
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


(defvar spacemacs--after-display-system-init-list '()
  "List of functions to be run after the display system is initialized.")

(defvar spacemacs--display-system-initialized-p nil)

(defun spacemacs//init-window-frame (frame)
  "After Emacs creates a window frame FRAME, run enqueued functions.

Functions are called with FRAME selected.

Queued functions are added to
`spacemacs--after-display-system-init-list' and are run once,
only after the display system has been initialized."
  (when (display-graphic-p frame)
    (setq spacemacs--display-system-initialized-p t)
    (dolist (f (reverse spacemacs--after-display-system-init-list))
      (with-demoted-errors "spacemacs|do-after-display-system-init: %S"
        (with-selected-frame frame
          (funcall f))))
    (remove-hook 'after-make-frame-functions #'spacemacs//init-window-frame)))

(add-hook 'after-make-frame-functions #'spacemacs//init-window-frame)

(defun spacemacs--call-after-display-system-init (func)
  "Call FUNC with no arguments once the display system is initialized.

See `spacemacs|do-after-display-system-init'."
  (if spacemacs--display-system-initialized-p
      (funcall func)
    (push func spacemacs--after-display-system-init-list)
    ;; `spacemacs--display-system-initialized-p' may be nil even if the initial
    ;; frame is graphical.
    (spacemacs//init-window-frame (selected-frame))))

(defmacro spacemacs|do-after-display-system-init (&rest body)
  "If the display system is initialized, run BODY.

Otherwise, enqueue it until after the first graphical frame is
created.

BODY is evaluated with a graphical frame selected."
  `(spacemacs--call-after-display-system-init (lambda () ,@body)))

(provide 'core-display-init)
