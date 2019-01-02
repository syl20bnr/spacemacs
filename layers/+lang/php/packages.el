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

(setq php-packages
      '(
        drupal-mode
        eldoc
        flycheck
        ggtags
        counsel-gtags
        helm-gtags
        php-auto-yasnippets
        (php-extras :location (recipe :fetcher github :repo "arnested/php-extras"))
        php-mode
        phpcbf
        phpunit
        (company-php :requires company)
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

(defun php/init-php-auto-yasnippets ()
  (use-package php-auto-yasnippets
    :defer t))

(defun php/init-php-extras ()
  (use-package php-extras
    :defer t))

(defun php/init-php-mode ()
  (use-package php-mode
    :defer t
    :mode ("\\.php\\'" . php-mode))
    :init
    (progn
      (add-hook 'php-mode-hook 'spacemacs//php-setup-backend)))

(defun php/init-phpcbf ()
  (use-package phpcbf
    :defer t))

(defun php/init-phpunit ()
  (use-package phpunit
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
        :backends company-ac-php-backend))))
