;;; packages.el --- PHP Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq php-packages
      '(
        ac-php
        company
        feature-mode
        drupal-mode
        eldoc
        flycheck
        ggtags
        helm-gtags
        php-auto-yasnippets
        (php-extras :location (recipe :fetcher github :repo "arnested/php-extras"))
        php-mode
        phpcbf
        phpunit
        ))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun php/post-init-company ()
    (spacemacs|add-company-hook php-mode)))

(add-hook 'php-mode-hook
         '(lambda ()
            (require 'company-php)
            (company-mode t)
            (add-to-list 'company-backends 'company-ac-php-backend )))

(defun php/post-init-php-extras ()
    (push 'php-extras-company company-backends-php-mode))

(defun php/post-init-ac-php ()
    (push 'company-ac-php-backend company-backends-php-mode))

(defun php/init-feature-mode ()
  "Initialize feature mode for Behat"
  (use-package feature-mode
    :mode (("\\.feature\\'" . feature-mode))))

(defun php/init-ac-php ()
  (use-package ac-php
    :defer t
    :init (progn
            (use-package ac-php-company
              :defer t)
            )))

(defun php/init-drupal-mode ()
  (use-package drupal-mode
    :defer t))

(defun php/post-init-eldoc ()
  (add-hook 'php-mode-hook 'eldoc-mode))

(defun php/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'php-mode))

(defun php/post-init-ggtags ()
  (add-hook 'php-mode-hook #'spacemacs/ggtags-mode-enable))

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
    :mode ("\\.php\\'" . php-mode)))

(defun php/init-phpcbf ()
  (use-package phpcbf
    :defer t))

(defun php/init-phpunit ()
  (use-package phpunit
    :defer t))
