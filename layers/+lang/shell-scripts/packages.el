;;; packages.el --- Shell Scripts Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq shell-scripts-packages
      '(
        (company-shell :toggle (configuration-layer/package-usedp 'company))
        fish-mode
        flycheck
        ggtags
        helm-gtags
        insert-shebang
        (sh-script :location built-in)
        ))

(defun shell-scripts/init-company-shell ()
  (use-package company-shell
    :defer t
    :init
    (progn
      (spacemacs|add-company-backends
        :backends company-shell
        :modes sh-mode)
      (spacemacs|add-company-backends
        :backends (company-shell company-fish-shell)
        :modes fish-mode))))

(defun shell-scripts/post-init-flycheck ()
  (spacemacs/enable-flycheck 'sh-mode))

(defun shell-scripts/init-fish-mode ()
  (use-package fish-mode
    :defer t))

(defun shell-scripts/init-sh-script ()
  (use-package sh-script
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'sh-mode
        "\\" 'sh-backslash-region)

      ;; Use sh-mode when opening `.zsh' files, and when opening Prezto runcoms.
      (dolist (pattern '("\\.zsh\\'"
                         "zlogin\\'"
                         "zlogout\\'"
                         "zpreztorc\\'"
                         "zprofile\\'"
                         "zshenv\\'"
                         "zshrc\\'"))
        (add-to-list 'auto-mode-alist (cons pattern 'sh-mode)))

      (defun spacemacs//setup-shell ()
        (when (and buffer-file-name
                   (string-match-p "\\.zsh\\'" buffer-file-name))
          (sh-set-shell "zsh")))
      (add-hook 'sh-mode-hook 'spacemacs//setup-shell))))

(defun shell-scripts/post-init-ggtags ()
  (add-hook 'sh-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun shell-scripts/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'sh-mode))

(defun shell-scripts/init-insert-shebang ()
  (use-package insert-shebang
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "i!" 'spacemacs/insert-shebang)
      ;; we don't want to insert shebang lines automatically
      (remove-hook 'find-file-hook 'insert-shebang))))

