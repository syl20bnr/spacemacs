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
  '(fish-mode
    (sh-script :location built-in)
    ))

(defun shell-scripts/init-fish-mode ()
  (use-package fish-mode
    :defer t))

(defun shell-scripts/init-sh-script ()
  (use-package sh-script
    :defer t
    :init
    (progn
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
