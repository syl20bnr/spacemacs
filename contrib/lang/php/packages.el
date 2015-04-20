;;; packages.el --- PHP Layer packages File for Spacemacs
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

(defvar php-packages '(
                       php-mode
                       phpcbf
                       helm-gtags
                       ggtags
                       php-extras
                       flycheck
                       company
                       php-auto-yasnippets
                       phpunit
                       drupal-mode
                       )
  )

(defun php/init-php-mode ()
  (use-package php-mode
    :defer t
    :mode ("\\.php\\'" . php-mode)
    :config
    (progn
        (add-hook 'php-mode-hook 'eldoc-mode)
        (add-hook 'php-mode-hook 'setq-local eldoc-documentation-function #'ggtags-eldoc-function)
        (add-hook 'php-mode-hook
            (lambda ()
                (set (make-local-variable 'company-backends)
                    '((company-yasnippet company-gtags company-capf company-dabbrev-code company-keywords company-files php-extras-company)))))

      )))

(defun php/init-php-extras ()
  (use-package php-extras
    :defer t
    )
  )

(defun php/init-phpcbf ()
  (use-package phpcbf
    :defer t
    )
  )

(defun php/init-php-auto-yasnippets ()
  (use-package php-auto-yasnippets
    :defer t))

(defun php/init-phpunit ()
  (use-package phpunit
    :defer t))

(defun php/init-drupal-mode ()
  (use-package drupal-mode
    :defer t
  )
)

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun php/post-init-company ()
    (spacemacs|add-company-hook php-mode))
  )

(defun php/post-init-helm-gtags ()
  (spacemacs/gtags-define-keys-for-mode 'php-mode))
