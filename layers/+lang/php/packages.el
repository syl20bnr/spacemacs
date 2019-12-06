;;; packages.el --- PHP Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst php-packages
  '(
    drupal-mode
    eldoc
    evil-matchit
    flycheck
    ggtags
    counsel-gtags
    helm-gtags
    php-auto-yasnippets
    (php-extras :location (recipe :fetcher github :repo "arnested/php-extras"))
    php-mode
    phpcbf
    phpunit
    (phpactor :toggle (not (eq php-backend 'lsp)))
    (company-phpactor :requires company :toggle (not (eq php-backend 'lsp)))
    (company-php :requires company :toggle (not (eq php-backend 'lsp)))
    (geben :toggle (not (eq php-backend 'lsp)))
    ))

(defun php/init-drupal-mode ()
  (use-package drupal-mode
    :defer t))

(defun php/post-init-eldoc ()
  (add-hook 'php-mode-hook 'eldoc-mode))

(defun php/post-init-flycheck ()
  (spacemacs/enable-flycheck 'php-mode))

(defun php/post-init-ggtags ()
  (add-hook 'php-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun php/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'php-mode))

(defun php/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'php-mode))

(defun php/post-init-evil-matchit ()
  (add-hook 'php-mode-hook 'turn-on-evil-matchit-mode))

(defun php/init-php-auto-yasnippets ()
  (use-package php-auto-yasnippets
    :defer t))

(defun php/init-php-extras ()
  (use-package php-extras
    :defer t))

(defun php/init-php-mode ()
  (use-package php-mode
    :defer t
    :mode ("\\.php\\'" . php-mode)
    :init
    (progn
      (add-hook 'php-mode-hook 'spacemacs//php-setup-backend))
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'php-mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode 'php-mode "mt" "tests")
      (spacemacs/set-leader-keys-for-major-mode 'php-mode
        "tt" 'phpunit-current-test
        "tc" 'phpunit-current-class
        "tp" 'phpunit-current-project))))

(defun php/init-phpactor ()
  (use-package phpactor
    :defer t
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'php-mode "mrg" "generate")
      (spacemacs/declare-prefix-for-mode 'php-mode "mre" "extract")
      (spacemacs/declare-prefix-for-mode 'php-mode "mrm" "methods")
      (spacemacs/declare-prefix-for-mode 'php-mode "mrc" "classes")
      (spacemacs/declare-prefix-for-mode 'php-mode "mrp" "properties")
      (spacemacs/declare-prefix-for-mode 'php-mode "mP" "phpactor")
      (spacemacs/declare-prefix-for-mode 'php-mode "mr" "refactoring")
      (spacemacs/set-leader-keys-for-major-mode 'php-mode
        "ri"  #'phpactor-import-class
        "rr"  #'phpactor-rename-variable-local
        "rR"  #'phpactor-rename-variable-file
        "rn"  #'phpactor-fix-namespace
        "rv"  #'phpactor-change-visibility
        "rga" #'phpactor-generate-accessors
        "rgm" #'phpactor-generate-method
        "rcn" #'phpactor-create-new-class
        "rcc" #'phpactor-copy-class
        "rcm" #'phpactor-move-class
        "rci" #'phpactor-inflect-class
        "rpc" #'phpactor-complete-constructor
        "rpp" #'phpactor-complete-properties
        "rec" #'phpactor-extract-constant
        "ree" #'phpactor-extract-expression
        "rem" #'phpactor-extract-method
        "rmc" #'phpactor-implement-contracts
        "Ps"  #'phpactor-status
        "Pu"  #'phpactor-install-or-update)
      (setq-default phpactor-references-list-col1-width 72))))

(defun php/init-phpcbf ()
  (use-package phpcbf
    :defer t))

(defun php/init-phpunit ()
  (use-package phpunit
    :defer t))

(defun php/init-company-phpactor ()
  (use-package company-phpactor
    :defer t))

(defun php/init-company-php ()
  (use-package company-php
    :defer t
    :init
    (progn
      (add-to-list 'spacemacs-jump-handlers-php-mode 'ac-php-find-symbol-at-point)
      (add-hook 'php-mode-hook 'ac-php-core-eldoc-setup)
      (spacemacs|add-company-backends
        :modes php-mode
        :backends (company-ac-php-backend company-phpactor)))))

(defun php/init-geben ()
  (use-package geben
    :config
    (progn
      (setq geben-temporary-file-directory (concat spacemacs-cache-directory "geben"))

      (spacemacs/declare-prefix-for-mode 'php-mode "md" "debug")
      (spacemacs/set-leader-keys-for-major-mode 'php-mode
        "dx" #'geben
        "dX" #'geben-end
        "db" #'geben-add-current-line-to-predefined-breakpoints
        "dC" #'geben-clear-predefined-breakpoints)
      (evilified-state-evilify-map geben-mode-map
        :mode 'php-mode
        :bindings
        "q"  #'geben-stop
        "n"  #'geben-step-over
        "s"  #'geben-step-into
        "r"  #'geben-step-out
        "L"  #'geben-where
        "v"  #'geben-display-context
        "c"  #'geben-run-to-cursor
        "bb" #'geben-set-breakpoint-line
        "bc" #'geben-set-breakpoint-conditional
        "be" #'geben-set-breakpoint-exception
        "w"  #'geben-show-backtrace
        "gf" #'geben-find-file)
      (add-hook 'geben-mode-hook 'evil-evilified-state)
      (evil-set-initial-state 'geben-context-mode 'evilified)
      (evilified-state-evilify-map geben-context-mode-map
        :mode geben-context-mode
        :bindings
        "q"  #'geben-quit-window
        "j"  #'widget-forward
        "k"  #'widget-backward
        (kbd "<tab>") 'widget-button-press)
      (evilified-state-evilify-map geben-backtrace-mode-map
        :mode geben-backtrace-mode))))
