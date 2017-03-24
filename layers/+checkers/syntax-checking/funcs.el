;;; funcs.el --- Syntax Checking Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/enable-flycheck (mode)
  "Use flycheck in MODE by default, if `syntax-checking-enable-by-default' is
true."
  (when (and syntax-checking-enable-by-default
             (listp flycheck-global-modes)
             (not (eq 'not (car flycheck-global-modes))))
    (add-to-list 'flycheck-global-modes mode)))

;; toggle flycheck window
(defun spacemacs/toggle-flycheck-error-list ()
  "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
  (interactive)
  (-if-let (window (flycheck-get-error-list-window))
      (quit-window nil window)
    (flycheck-list-errors)))

(defun spacemacs/goto-flycheck-error-list ()
  "Open and go to the error list buffer."
  (interactive)
  (unless (get-buffer-window (get-buffer flycheck-error-list-buffer))
    (flycheck-list-errors)
    (switch-to-buffer-other-window flycheck-error-list-buffer)))
