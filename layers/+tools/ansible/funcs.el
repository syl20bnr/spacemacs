;;; funcs.el --- Ansible Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Brian Hicks <brian@brianthicks.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//ansible-should-enable? ()
  "Return non-nil if `ansible' should be enabled for the current file."
  (and (stringp buffer-file-name)
       (string-match spacemacs--ansible-filename-re buffer-file-name)))

(defun spacemacs/ansible-maybe-enable ()
  "Enable `ansible-mode' if required."
  (when (spacemacs//ansible-should-enable?)
    (ansible 1)))

(defun spacemacs/ansible-auto-decrypt-encrypt-vault ()
  "Auto decrypt/encrypt Vault files."
  (when (spacemacs//ansible-should-enable?)
    (ansible-auto-decrypt-encrypt)))

(defun spacemacs/ansible-company-maybe-enable ()
  "Add the ansible company backend only for when ansible mode is active."
  (when (spacemacs//ansible-should-enable?)
    (add-to-list 'company-backends 'company-ansible)))

(defun spacemacs/ansible-doc-maybe-enable ()
  "Enable `ansible-doc-mode` if possible.'"
  (when (spacemacs//ansible-should-enable?)
    (ansible-doc-mode 1)))
