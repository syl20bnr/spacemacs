;;; funcs.el --- Spacemacs Editing Visual Layer functions File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/toggle-centered-buffer-mode ()
  "Toggle `spacemacs-centered-buffer-mode'."
  (interactive)
  (when (require 'centered-buffer-mode nil t)
    (call-interactively 'spacemacs-centered-buffer-mode)))

(defun spacemacs/toggle-centered-buffer-mode-frame ()
  "Open current buffer in the new frame centered and without mode-line."
  (interactive)
  (when (require 'centered-buffer-mode nil t)
    (switch-to-buffer-other-frame (current-buffer) t)
    (toggle-frame-fullscreen)
    (run-with-idle-timer
     ;; FIXME: We need this delay to make sure that the
     ;; `toggle-frame-fullscreen' fully "finished"
     ;; it will be better to use something more reliable
     ;; instead :)
     1
     nil
     (lambda ()
       (call-interactively 'spacemacs-centered-buffer-mode)
       (setq mode-line-format nil)))))

(defun spacemacs/centered-buffer-mode-full-width ()
  "Center buffer in the frame."
  ;; FIXME Needs new key-binding.
  (interactive)
  (when (require 'centered-buffer-mode nil t)
    (spacemacs/maximize-horizontally)
    (call-interactively 'spacemacs-centered-buffer-mode)))
