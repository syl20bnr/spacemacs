;;; packages.el --- F# Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq fsharp-packages '(fsharp-mode))

(defun fsharp/init-fsharp-mode ()
  (use-package fsharp-mode
    :defer t
    :init
    (setq fsharp-doc-idle-delay .2)
    :config
    (progn

      (defun spacemacs/fsharp-load-buffer-file-focus ()
        "Send the current buffer to REPL and switch to the REPL in
 `insert state'."
        (interactive)
        (fsharp-load-buffer-file)
        (switch-to-buffer-other-window inferior-fsharp-buffer-name)
        (evil-insert-state))

      (defun spacemacs/fsharp-eval-phrase-focus ()
        "Send the current phrase to REPL and switch to the REPL in
 `insert state'."
        (interactive)
        (fsharp-eval-phrase)
        (switch-to-buffer-other-window inferior-fsharp-buffer-name)
        (evil-insert-state))

      (defun spacemacs/fsharp-eval-region-focus (start end)
        "Send the current phrase to REPL and switch to the REPL in
 `insert state'."
        (interactive "r")
        (fsharp-eval-region start end)
        (switch-to-buffer-other-window inferior-fsharp-buffer-name)
        (evil-insert-state))

      (evil-leader/set-key-for-mode 'fsharp-mode
        ;; Compile
        "mcc" 'compile

        "mfa" 'fsharp-find-alternate-file

        "mgg" 'fsharp-ac/gotodefn-at-point

        "mht" 'fsharp-ac/show-tooltip-at-point

        "msb" 'fsharp-load-buffer-file
        "msB" 'spacemacs/fsharp-load-buffer-file-focus
        "msi" 'fsharp-show-subshell
        "msp" 'fsharp-eval-phrase
        "msP" 'spacemacs/fsharp-eval-phrase-focus
        "msr" 'fsharp-eval-region
        "msR" 'spacemacs/fsharp-eval-region-focus
        "mss" 'fsharp-show-subshell

        "mxf" 'fsharp-run-executable-file))))


