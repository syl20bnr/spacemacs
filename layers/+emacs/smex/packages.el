;;; packages.el --- smex Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq smex-packages '(smex))

(defun smex/init-smex ()
  (use-package smex
    :defer t
    :init
    (progn
      (setq-default smex-history-length 32
                    smex-save-file (concat spacemacs-cache-directory
                                           ".smex-items"))
      ;; define the key binding at the very end in order to allow the user
      ;; to overwrite any key binding
      (add-hook 'emacs-startup-hook
                (lambda () (spacemacs/set-leader-keys
                             dotspacemacs-emacs-command-key 'spacemacs/smex)))
      (spacemacs/set-leader-keys ":" 'spacemacs/smex-major-mode-commands)
      (global-set-key (kbd "M-x") 'spacemacs/smex))))
