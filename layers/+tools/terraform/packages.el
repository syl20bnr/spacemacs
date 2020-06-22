;;; packages.el --- terraform Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Brian Hicks <brian@brianthicks.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq terraform-packages
      '(
        company
        (company-terraform :requires company)
        terraform-mode
        ))

(defun terraform/post-init-company ()
  (spacemacs//terraform-setup-company))

(defun terraform/init-company-terraform ()
  (use-package company-terraform
    :defer t))

(defun terraform/init-terraform-mode ()
  (use-package terraform-mode
    :defer t
    :init (add-hook 'terraform-mode-hook
                    'spacemacs//terraform-setup-backend)
    :config (when terraform-auto-format-on-save
              (add-hook 'terraform-mode-hook
                        'terraform-format-on-save-mode))))
