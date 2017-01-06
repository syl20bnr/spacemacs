;;; packages.el --- Vagrant Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
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
      (spacemacs/declare-prefix "V" "vagrant")
      (spacemacs/set-leader-keys
        "VD" 'vagrant-destroy
        "Ve" 'vagrant-edit
        "VH" 'vagrant-halt
        "Vp" 'vagrant-provision
        "Vr" 'vagrant-resume
        "VR" 'vagrant-reload
        "Vs" 'vagrant-status
        "VS" 'vagrant-suspend
        "VV" 'vagrant-up))))

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
      (spacemacs/set-leader-keys "Vt" 'vagrant-tramp-term))))
