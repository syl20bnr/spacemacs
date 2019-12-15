;;; packages.el --- Ansible Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
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
        (company-ansible :requires company)
        jinja2-mode
        yaml-mode))

(defun ansible/init-ansible ()
  (use-package ansible
    :defer t
    :commands ansible-auto-decrypt-encrypt
    :init
    (progn
      (add-hook 'yaml-mode-hook 'spacemacs/ansible-maybe-enable)
      (put 'ansible-vault-password-file 'safe-local-variable #'stringp)
      (if ansible-auto-encrypt-decrypt
          ;; add this hook to local-vars-hook to allow users to specify
          ;; a password file in directory local variables
          (add-hook 'yaml-mode-local-vars-hook
                    'spacemacs/ansible-auto-decrypt-encrypt-vault)
        (remove-hook 'yaml-mode-local-vars-hook
                     'spacemacs/ansible-auto-decrypt-encrypt-vault))
      )
    :config
    (progn
      (spacemacs/set-leader-keys-for-minor-mode 'ansible
       "bd" 'ansible-decrypt-buffer
       "be" 'ansible-encrypt-buffer))))

(defun ansible/init-ansible-doc ()
  (use-package ansible-doc
    :defer t
    :init
    (progn
      (add-hook 'yaml-mode-hook 'spacemacs/ansible-doc-maybe-enable))
    :config
    (progn
      (spacemacs/set-leader-keys-for-minor-mode 'ansible-doc-mode
        "ha" 'ansible-doc)
      (spacemacs|hide-lighter ansible-doc-mode))))

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
    (unless yaml-enable-lsp
      ;; append this hook at the end to execute it last so `company-backends'
      ;; variable is buffer local
      (add-hook 'yaml-mode-hook 'spacemacs/ansible-company-maybe-enable t)
      (spacemacs|add-company-backends :backends company-ansible
                                      :modes ansible))))

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
