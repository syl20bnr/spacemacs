;;; packages.el --- Vagrant Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2015 Brian Hicks & Contributors
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
      (evil-leader/set-key
        "VD" 'vagrant-destroy
        "Ve" 'vagrant-edit
        "VH" 'vagrant-halt
        "Vp" 'vagrant-provision
        "Vr" 'vagrant-resume
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
          (vagrant-tramp-enable)
          (setq spacemacs--vagrant-tramp-loaded t)))
      (evil-leader/set-key "Vt" 'vagrant-tramp-term))))
