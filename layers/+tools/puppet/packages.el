;;; layers.el --- Puppet layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq puppet-packages
  '(
    company
    flycheck
    puppet-mode
    ))

(defun puppet/init-puppet-mode ()
  (use-package puppet-mode
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'puppet-mode
        "{" 'beginning-of-defun
        "}" 'end-of-defun
        "$" 'puppet-interpolate
        "a" 'puppet-align-block
        "'" 'puppet-toggle-string-quotes
        ";" 'puppet-clear-string
        "j" 'imenu
        "c" 'puppet-apply
        "v" 'puppet-validate
        "l" 'puppet-lint
      ))))

(defun puppet/post-init-company ()
  (spacemacs|add-company-hook puppet-mode))

(defun puppet/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'puppet-mode))
