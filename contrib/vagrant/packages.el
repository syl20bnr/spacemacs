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
(defvar vagrant-packages '(vagrant
                           vagrant-tramp))

(defun vagrant/init-vagrant ()
    (use-package vagrant
      :defer t
      :init (progn
                (spacemacs/declare-prefix "V" "vagrant")
                (evil-leader/set-key
                   "VV" 'vagrant-up
                   "Vp" 'vagrant-provision
                   "VD" 'vagrant-destroy
                   "Vs" 'vagrant-status
                   "VS" 'vagrant-suspend
                   "Vr" 'vagrant-resume
                   "VH" 'vagrant-halt
                   "Ve" 'vagrant-edit))))

(defun vagrant/init-vagrant-tramp ()
  (use-package vagrant-tramp
    :defer t
    :init (progn
            (vagrant-tramp-enable)
            (evil-leader/set-key
               "Vt" 'vagrant-tramp-term))))
