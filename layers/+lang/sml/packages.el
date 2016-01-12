;;; packages.el --- sml Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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

      (spacemacs/set-leader-keys-for-major-mode 'sml-mode
        ;; REPL
        "sb" 'sml-prog-proc-send-buffer
        "sB" 'spacemacs/sml-prog-proc-send-buffer-and-focus
        "sf" 'sml-send-function
        "sF" 'spacemacs/sml-send-function-and-focus
        "si" 'run-sml
        "sr" 'sml-prog-proc-send-region
        "sR" 'spacemacs/sml-prog-proc-send-region-and-focus
        "ss" 'run-sml)
      (define-key sml-mode-map (kbd "M-<SPC>") 'sml-electric-space)
      (define-key sml-mode-map (kbd "|") 'sml-electric-pipe))))

(defun sml/init-ob-sml ()
  (use-package ob-sml
    :defer t
    :init
    (org-babel-do-load-languages 'org-babel-do-load-languages '(sml . t))))
