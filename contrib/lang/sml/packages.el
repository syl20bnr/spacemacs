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
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar sml-excluded-packages '()
  "List of packages to exclude.")

(defun sml/init-sml-mode ()
  (use-package sml-mode
    :mode ("\\.\\(sml\\|sig\\)\\'" . sml-mode)
    :defer t
    :commands run-sml
    :config
    (progn
      (evil-leader/set-key-for-mode 'sml-mode
        ;; REPL
        "msb" 'sml-prog-proc-send-buffer
        "msr" 'sml-prog-proc-send-region
        "mss" 'run-sml)
      (define-key sml-mode-map (kbd "M-<SPC>") 'sml-electric-space)
      (define-key sml-mode-map (kbd "|") 'sml-electric-pipe))))

(defun sml/init-ob-sml ()
  (use-package ob-sml
    :config
    (org-babel-do-load-languages
     'org-babel-do-load-languages
     '(sml . t))))
