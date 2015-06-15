;;; packages.el --- mwe-log-commands Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: blinkd <tshemeng@live.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq mwe-log-commands-packages
    '(mwe-log-commands))

(defun mwe-log-commands/post-init-mwe-log-commands ()
  (defun spacemacs/mwe-log-commands ()
    (interactive)
    (let ((current-buffer (current-buffer)))
      (mwe:log-keyboard-commands t)
      (mwe:open-command-log-buffer)
      (erase-buffer)
      (linum-mode -1)
      (delete-other-windows)
      (split-window-horizontally -30)
      (other-window 0)
      (switch-to-buffer current-buffer)))

  (define-minor-mode mwe-log-commands-mode
    "log commands")

  (defun mwe-log-commands-on ()
    (unless (minibufferp)
      (mwe:log-keyboard-commands t)))

  (define-globalized-minor-mode global-mwe-log-commands-mode mwe-log-commands-mode mwe-log-commands-on)
  (global-mwe-log-commands-mode))

(defun mwe-log-commands/init-mwe-log-commands ()
  "Initialize my package"
  (use-package mwe-log-commands)
  :defer t
  :init
  (add-hook 'spacemacs-mode-hook (function mwe:log-keyboard-commands)))
