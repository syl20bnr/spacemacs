;;; config.el --- Ansible Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Brian Hicks <brian@brianthicks.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables

(defvar ansible-auto-encrypt-decrypt t
  "Set it to non-nil to seamlessly edit `ansible-vault' encrypted files.
If non-nil then encrypted files are automatically decrypted when opened and
 encrypted when saved.")

;; detect filenames compatible with Ansible's recommended layout.
;; http://docs.ansible.com/playbooks_best_practices.html#directory-layout
(setq spacemacs--ansible-filename-re
      ".*\\(\\(main\\|site\\|encrypted\\|roles/.+\\)\.yml\\|group_vars/.+\\|host_vars/.+\\)")
