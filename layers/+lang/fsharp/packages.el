;;; packages.el --- F# Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst fsharp-packages
  '(
    company
    counsel-gtags
    (eglot-fsharp :toggle (eq (spacemacs//fsharp-backend) 'eglot))
    flycheck
    fsharp-mode
    ggtags
    helm-gtags
    ))

(defun fsharp/post-init-company ()
  (spacemacs//fsharp-setup-company))

(defun fsharp/post-init-flycheck ()
  (spacemacs/enable-flycheck 'fsharp-mode))

(defun fsharp/init-eglot-fsharp ()
  (use-package eglot-fsharp
    :defer t
    :init
    (progn
      (require 'f)
      (setq eglot-fsharp-server-install-dir
            (expand-file-name
             (locate-user-emacs-file (f-join ".cache" "eglot")))))))

(defun fsharp/init-fsharp-mode ()
  (use-package fsharp-mode
    :defer t
    :init
    (progn
      (when (eq (spacemacs//fsharp-backend) 'eglot)
        (require 'eglot-fsharp))
      (setq fsharp-doc-idle-delay .2)
      (spacemacs/register-repl 'fsharp-mode 'fsharp-show-subshell "F#")
      (add-hook 'fsharp-mode-hook #'spacemacs//fsharp-setup-backend))
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

      (spacemacs/declare-prefix-for-mode 'fsharp-mode "ms" "repl")
      (spacemacs/declare-prefix-for-mode 'fsharp-mode "mc" "compile")
      (when (eq (spacemacs//fsharp-backend) 'eglot)
        (spacemacs/declare-prefix-for-mode 'fsharp-mode "mg" "goto"))

      (spacemacs/set-leader-keys-for-major-mode 'fsharp-mode
        "cc" 'compile
        "ga" 'fsharp-find-alternate-file
        "sb" 'fsharp-load-buffer-file
        "sB" 'spacemacs/fsharp-load-buffer-file-focus
        "si" 'fsharp-show-subshell
        "sp" 'fsharp-eval-phrase
        "sP" 'spacemacs/fsharp-eval-phrase-focus
        "sr" 'fsharp-eval-region
        "sR" 'spacemacs/fsharp-eval-region-focus
        "'"  'fsharp-show-subshell))))

(defun fsharp/post-init-ggtags ()
  (add-hook 'fsharp-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun fsharp/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'fsharp-mode))

(defun fsharp/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'fsharp-mode))
