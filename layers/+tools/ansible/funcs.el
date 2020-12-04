;;; funcs.el --- Ansible Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Brian Hicks <brian@brianthicks.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//ansible-should-enable? ()
  "Return non-nil if `ansible' should be enabled for the current file."
  (and (stringp buffer-file-name)
       (string-match space-macs--ansible-filename-re buffer-file-name)))

(defun space-macs/ansible-maybe-enable ()
  "Enable `ansible-mode' if required."
  (when (space-macs//ansible-should-enable?)
    (ansible 1)))

(defun space-macs/ansible-auto-decrypt-encrypt-vault ()
  "Auto decrypt/encrypt Vault files."
  (when (space-macs//ansible-should-enable?)
    (ansible-auto-decrypt-encrypt)))

(defun space-macs/ansible-company-maybe-enable ()
  "Add the ansible company backend only for when ansible mode is active."
  (when (space-macs//ansible-should-enable?)
    (add-to-list 'company-backends 'company-ansible)))

(defun space-macs/ansible-doc-maybe-enable ()
  "Enable `ansible-doc-mode` if possible.'"
  (when (space-macs//ansible-should-enable?)
    (ansible-doc-mode 1)))


