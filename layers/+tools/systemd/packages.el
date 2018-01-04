;;; packages.el --- systemd layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Fabien Dubosson <fabien.dubosson@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst systemd-packages
  '(
    flycheck
    systemd
    )
  "The list of Lisp packages required by the systemd layer.")

(defun systemd/post-init-flycheck ()
  (spacemacs/enable-flycheck 'systemd-mode))

(defun systemd/init-systemd ()
  (use-package systemd
    :defer t
    :init (setq systemd-use-company-p
                (configuration-layer/package-used-p 'company))
    :config (spacemacs/set-leader-keys-for-major-mode 'systemd-mode
              "hd" 'systemd-doc-directives
              "ho" 'systemd-doc-open)))

;;; packages.el ends here
