;;; packages.el --- Puppet layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
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
      (space-macs/set-leader-keys-for-major-mode 'puppet-mode
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
  (space-macs|add-company-backends :modes puppet-mode))

(defun puppet/post-init-flycheck ()
  (space-macs/enable-flycheck 'puppet-mode))


