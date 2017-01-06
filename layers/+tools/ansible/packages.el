;;; packages.el --- Ansible Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Brian Hicks <brian@brianthicks.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(setq ansible-packages
      '(ansible
        ansible-doc
        company
        (company-ansible :toggle (configuration-layer/package-usedp 'company))
        jinja2-mode
        yaml-mode))

(defun ansible/init-ansible ()
  (use-package ansible
    :defer t
    :init
    (progn
      (add-hook 'yaml-mode-hook 'spacemacs/ansible-maybe-enable)
      (if ansible-auto-encrypt-descrypt
          (add-hook 'ansible-hook 'ansible::auto-decrypt-encrypt)
        (remove-hook 'ansible-hook 'ansible::auto-decrypt-encrypt))
      (spacemacs/set-leader-keys-for-minor-mode 'ansible
        "bd" 'ansible::decrypt-buffer
        "be" 'ansible::encrypt-buffer))
    :config
    ;; TODO to remove when fixed upstream
    (advice-add 'ansible::decrypt-buffer
                :after 'spacemacs//ansible-reset-buffer-modified)))

(defun ansible/init-ansible-doc ()
  (use-package ansible-doc
    :defer t
    :init
    (progn
      (add-hook 'yaml-mode-hook 'spacemacs/ansible-doc-maybe-enable)
      (spacemacs/set-leader-keys-for-minor-mode 'ansible-doc-mode
        "ha" 'ansible-doc))
    :config (spacemacs|hide-lighter ansible-doc-mode)))

(defun ansible/post-init-company ()
  ;; ansible-mode requires ac-user-dictionary-files. If the
  ;; config is using company-mode this variable will not be
  ;; set, so we set it to a dummy value.
  ;;
  ;; Tracking here:
  ;; https://github.com/k1LoW/emacs-ansible/issues/2
  (defvar ac-user-dictionary-files nil))

(defun ansible/init-company-ansible ()
  (use-package company-ansible
    :defer t
    :init
    ;; append this hook at the end to execute it last so `company-backends'
    ;; variable is buffer local
    (add-hook 'yaml-mode-hook 'spacemacs/ansible-company-maybe-enable t)))

(defun ansible/init-jinja2-mode ()
  (use-package jinja2-mode
    :mode ("\\.j2\\'" . jinja2-mode)
    :defer t))

(defun ansible/post-init-yaml-mode ()
  ;; maybe move it to the layer owning `yaml-mode' at some point
  (spacemacs/declare-prefix-for-mode 'yaml-mode "mh" "help")
  (spacemacs/declare-prefix-for-mode 'yaml-mode "mb" "buffer")
  (add-to-list 'auto-mode-alist
               '("\\(group_vars/.+\\|host_vars/.+\\)" . yaml-mode)))
