;;; packages.el --- Ansible Layer extensions File for Spacemacs
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
(setq ansible-packages '(yaml-mode
                           ansible
                           ansible-doc))

(defun ansible/init-yaml-mode ()
  (use-package yaml-mode
    :defer t))

(defun ansible/init-ansible ()
  (use-package ansible
    :defer t
    :init (progn
            (eval-after-load 'yaml-mode
              '(add-hook 'yaml-mode-hook 'ansible/ansible-maybe-enable))

            ;; ansible-mode requires ac-user-dictionary-files. If the
            ;; config is using company-mode this variable will not be
            ;; set, so we set it to a dummy value.
            ;;
            ;; Tracking here:
            ;; https://github.com/k1LoW/emacs-ansible/issues/2
            (when (member 'company-mode dotspacemacs-configuration-layers)
              (setq ac-user-dictionary-files '())))))

(defun ansible/init-ansible-doc ()
  (use-package ansible-doc
    :defer t
    :init (eval-after-load 'yaml-mode
            '(add-hook 'yaml-mode-hook 'ansible/ansible-doc-maybe-enable))))
