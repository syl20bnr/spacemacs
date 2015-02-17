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

;; detect filenames compatible with Ansible's recommended layout.
;; http://docs.ansible.com/playbooks_best_practices.html#directory-layout
(setq ansible/ansible-filename-re
      "\\(site\.yml\\|roles/.+\.yml\\|group_vars/.+\\|host_vars/.+\\)")
