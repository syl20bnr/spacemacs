;;; packages.el --- terraform Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Brian Hicks <brian@brianthicks.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq terraform-packages '(terraform-mode))

(defun terraform/init-terraform-mode ()
  (use-package terraform-mode
    :defer t
    :config (when terraform-auto-format-on-save
              (add-hook 'terraform-mode-hook 'terraform-format-on-save-mode))))
