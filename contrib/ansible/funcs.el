;;; funcs.el --- Ansible Layer extensions File for Spacemacs
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
(defun ansible/ansible-should-enable? ()
  (and (stringp buffer-file-name)
       (string-match ansible/ansible-filename-re buffer-file-name)))

(defun ansible/ansible-maybe-enable ()
  (when (ansible/ansible-should-enable?)
    (ansible 1)))

(defun ansible/ansible-doc-maybe-enable ()
  (when (ansible/ansible-should-enable?)
    (ansible-doc-mode 1)
    (evil-leader/set-key-for-mode 'yaml-mode
      "ma?" 'ansible-doc)))
