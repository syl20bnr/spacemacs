;;; packages.el --- sml Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Keith Simmons <keith@the-simmons.net>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq sml-packages
      '(
        sml-mode
        ob-sml
        smartparens
        ))

(defun sml/init-sml-mode ()
  (use-package sml-mode
    :mode ("\\.\\(sml\\|sig\\)\\'" . sml-mode)
    :defer t
    :commands run-sml
    :init (space-macs/register-repl 'sml-mode 'run-sml "sml")
    :config
    (progn
      (defun space-macs/sml-prog-proc-send-buffer-and-focus ()
        "Send buffer to REPL and switch to it in `insert state'."
        (interactive)
        (sml-prog-proc-send-buffer t)
        (evil-insert-state))

      (defun space-macs/sml-prog-proc-send-region-and-focus (start end)
        "Send region to REPL and switch to it in `insert state'."
        (interactive "r")
        (sml-prog-proc-send-region start end t)
        (evil-insert-state))

      (defun space-macs/sml-send-function-and-focus ()
        "Send function at point to REPL and switch to it in `insert state'."
        (interactive)
        (sml-send-function t)
        (evil-insert-state))

      (space-macs/set-leader-keys-for-major-mode 'sml-mode
        ;; REPL
        "'"  'run-sml
        "sb" 'sml-prog-proc-send-buffer
        "sB" 'space-macs/sml-prog-proc-send-buffer-and-focus
        "sf" 'sml-send-function
        "sF" 'space-macs/sml-send-function-and-focus
        "si" 'run-sml
        "sr" 'sml-prog-proc-send-region
        "sR" 'space-macs/sml-prog-proc-send-region-and-focus
        "ss" 'run-sml)
      (define-key sml-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
      (define-key sml-mode-map (kbd "M-SPC") 'sml-electric-space)
      (define-key sml-mode-map (kbd "|") 'sml-electric-pipe))))

(defun sml/post-init-smartparens ()
  (with-eval-after-load 'smartparens
    ;; don't auto-close apostrophes (type 'a = foo) and backticks (`Foo)
    (sp-local-pair 'sml-mode "'" nil :actions nil)
    (sp-local-pair 'sml-mode "`" nil :actions nil)))

(defun sml/pre-init-ob-sml ()
  (space-macs|use-package-add-hook org
    :post-config
    (use-package ob-sml
      :init (add-to-list 'org-babel-load-languages '(sml . t)))))
(defun sml/init-ob-sml ())


