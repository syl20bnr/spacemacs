;;; packages.el --- C# Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq csharp-packages
      '(
        company
        csharp-mode
        evil-matchit
        ggtags
        counsel-gtags
        helm-gtags
        omnisharp
        flycheck
        ))

(defun csharp/init-omnisharp ()
  (use-package omnisharp
    :defer t
    :init
    (space-macs//csharp-setup-backend)
    :config
    (space-macs//csharp-configure)
    ))

(defun csharp/post-init-company ()
  (space-macs//csharp-setup-company))

(defun csharp/init-csharp-mode ()
  (use-package csharp-mode
    :defer t))

(defun csharp/post-init-evil-matchit ()
  (with-eval-after-load 'evil-matchit
    (plist-put evilmi-plugins 'csharp-mode
               '((evilmi-simple-get-tag evilmi-simple-jump)
                 (evilmi-c-get-tag evilmi-c-jump))))
  (add-hook 'csharp-mode-hook 'turn-on-evil-matchit-mode))

(defun csharp/post-init-flycheck ()
  (space-macs/enable-flycheck 'csharp-mode))

(defun csharp/post-init-ggtags ()
  (add-hook 'csharp-mode-local-vars-hook #'space-macs/ggtags-mode-enable))

(defun csharp/post-init-counsel-gtags ()
  (space-macs/counsel-gtags-define-keys-for-mode 'csharp-mode))

(defun csharp/post-init-helm-gtags ()
  (space-macs/helm-gtags-define-keys-for-mode 'csharp-mode))


