;;; packages.el --- Protocol Buffers Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Amol Mandhane <https://github.com/amol-mandhane>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq protobuf-packages
      '(
        flycheck
        protobuf-mode
        ))

(defun protobuf/init-protobuf-mode ()
  (use-package protobuf-mode
    :init (progn
            (defun spacemacs//setup-protobuf-imenu ()
              "Setup imenu regex for protocol buffers."
              (setq imenu-generic-expression '((nil "^[[:space:]]*\\(message\\|service\\|enum\\)[[:space:]]+\\([[:alnum:]]+\\)"
                                                    2))))
            (add-hook 'protobuf-mode-hook 'spacemacs//setup-protobuf-imenu))))

(defun protobuf/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'protobuf-mode))
