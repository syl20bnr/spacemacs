;;; packages.el --- sml Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Keith Simmons & Contributors
;;
;; Author: Keith Simmons <keith@the-simmons.net>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq sml-packages
  '(
    sml-mode
    ob-sml
    ))

(defun sml/init-sml-mode ()
  (use-package sml-mode
    :mode ("\\.\\(sml\\|sig\\)\\'" . sml-mode)
    :defer t
    :commands run-sml
    :config
    (progn
      (defun spacemacs/sml-prog-proc-send-buffer-and-focus ()
        "Send buffer to REPL and switch to it in `insert state'."
        (interactive)
        (sml-prog-proc-send-buffer t)
        (evil-insert-state))

      (defun spacemacs/sml-prog-proc-send-region-and-focus (start end)
        "Send region to REPL and switch to it in `insert state'."
        (interactive "r")
        (sml-prog-proc-send-region start end t)
        (evil-insert-state))

      (defun spacemacs/sml-send-function-and-focus ()
        "Send function at point to REPL and switch to it in `insert state'."
        (interactive)
        (sml-send-function t)
        (evil-insert-state))

      (evil-leader/set-key-for-mode 'sml-mode
        ;; REPL
        "msb" 'sml-prog-proc-send-buffer
        "msB" 'spacemacs/sml-prog-proc-send-buffer-and-focus
        "msf" 'sml-send-function
        "msF" 'spacemacs/sml-send-function-and-focus
        "msi" 'run-sml
        "msr" 'sml-prog-proc-send-region
        "msR" 'spacemacs/sml-prog-proc-send-region-and-focus
        "mss" 'run-sml)
      (define-key sml-mode-map (kbd "M-<SPC>") 'sml-electric-space)
      (define-key sml-mode-map (kbd "|") 'sml-electric-pipe))))

(defun sml/init-ob-sml ()
  (use-package ob-sml
    :defer t
    :init
    (org-babel-do-load-languages 'org-babel-do-load-languages '(sml . t))))
