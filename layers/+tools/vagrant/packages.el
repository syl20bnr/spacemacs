;;; packages.el --- Vagrant Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Brian Hicks <brian@brianthicks.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(setq vagrant-packages '(vagrant
                         vagrant-tramp))

(defun vagrant/init-vagrant ()
  (use-package vagrant
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "atv" "vagrant")
      (spacemacs/set-leader-keys
        "atvD" 'vagrant-destroy
        "atve" 'vagrant-edit
        "atvH" 'vagrant-halt
        "atvp" 'vagrant-provision
        "atvr" 'vagrant-resume
        "atvR" 'vagrant-reload
        "atvs" 'vagrant-status
        "atvS" 'vagrant-suspend
        "atvV" 'vagrant-up))))

(defun vagrant/init-vagrant-tramp ()
  (use-package vagrant-tramp
    :defer t
    :init
    (progn
      (defvar spacemacs--vagrant-tramp-loaded nil)
      (defadvice vagrant-tramp-term (before spacemacs//load-vagrant activate)
        "Lazy load vagrant-tramp."
        (unless spacemacs--vagrant-tramp-loaded
          (vagrant-tramp-add-method)
          (setq spacemacs--vagrant-tramp-loaded t)))
      (spacemacs/set-leader-keys "atvt" 'vagrant-tramp-term))))
