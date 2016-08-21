;;; packages.el --- Shell Scripts Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq shell-scripts-packages
      '(
        company
        (company-shell :toggle (configuration-layer/package-usedp 'company))
        fish-mode
        flycheck
        ggtags
        helm-gtags
        (sh-script :location built-in)
        ))

(defun shell-scripts/post-init-company ()
  (spacemacs|add-company-hook sh-mode)
  (spacemacs|add-company-hook fish-mode))

(defun shell-scripts/init-company-shell ()
  (use-package company-shell
    :defer t
    :init
    (progn
      (push 'company-shell company-backends-sh-mode)
      (push '(company-shell company-fish-shell) company-backends-fish-mode))))

(defun shell-scripts/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'sh-mode))

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
