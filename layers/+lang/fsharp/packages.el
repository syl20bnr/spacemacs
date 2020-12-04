;;; packages.el --- F# Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst fsharp-packages
  '(
    company
    counsel-gtags
    flycheck
    fsharp-mode
    ggtags
    helm-gtags
    ))

(defun fsharp/post-init-company ()
  (space-macs//fsharp-setup-company))

(defun fsharp/post-init-flycheck ()
  (space-macs/enable-flycheck 'fsharp-mode))

(defun fsharp/init-fsharp-mode ()
  (use-package fsharp-mode
    :defer t
    :init
    (progn
      (unless (eq (space-macs//fsharp-backend) 'lsp)
        (require 'eglot-fsharp))
      (setq eglot-fsharp-server-install-dir (expand-file-name
                                             (locate-user-e-macs-file (f-join ".cache" "eglot"))))
      (setq fsharp-doc-idle-delay .2)
      (space-macs/register-repl 'fsharp-mode 'fsharp-show-subshell "F#")
      (add-hook 'fsharp-mode-hook #'space-macs//fsharp-setup-backend))
    :config
    (progn
      (defun space-macs/fsharp-load-buffer-file-focus ()
        "Send the current buffer to REPL and switch to the REPL in
 `insert state'."
        (interactive)
        (fsharp-load-buffer-file)
        (switch-to-buffer-other-window inferior-fsharp-buffer-name)
        (evil-insert-state))
      (defun space-macs/fsharp-eval-phrase-focus ()
        "Send the current phrase to REPL and switch to the REPL in
 `insert state'."
        (interactive)
        (fsharp-eval-phrase)
        (switch-to-buffer-other-window inferior-fsharp-buffer-name)
        (evil-insert-state))
      (defun space-macs/fsharp-eval-region-focus (start end)
        "Send the current phrase to REPL and switch to the REPL in
 `insert state'."
        (interactive "r")
        (fsharp-eval-region start end)
        (switch-to-buffer-other-window inferior-fsharp-buffer-name)
        (evil-insert-state))

      (space-macs/declare-prefix-for-mode 'fsharp-mode "ms" "repl")
      (space-macs/declare-prefix-for-mode 'fsharp-mode "mc" "compile")
      (unless (eq (space-macs//fsharp-backend) 'lsp)
        (space-macs/declare-prefix-for-mode 'fsharp-mode "mg" "goto"))

      (space-macs/set-leader-keys-for-major-mode 'fsharp-mode
        "cc" 'compile
        "ga" 'fsharp-find-alternate-file
        "sb" 'fsharp-load-buffer-file
        "sB" 'space-macs/fsharp-load-buffer-file-focus
        "si" 'fsharp-show-subshell
        "sp" 'fsharp-eval-phrase
        "sP" 'space-macs/fsharp-eval-phrase-focus
        "sr" 'fsharp-eval-region
        "sR" 'space-macs/fsharp-eval-region-focus
        "'"  'fsharp-show-subshell))))

(defun fsharp/post-init-ggtags ()
  (add-hook 'fsharp-mode-local-vars-hook #'space-macs/ggtags-mode-enable))

(defun fsharp/post-init-counsel-gtags ()
  (space-macs/counsel-gtags-define-keys-for-mode 'fsharp-mode))

(defun fsharp/post-init-helm-gtags ()
  (space-macs/helm-gtags-define-keys-for-mode 'fsharp-mode))


