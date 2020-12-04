;;; packages.el --- smex Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq smex-packages '(smex))

(defun smex/init-smex ()
  (use-package smex
    :defer t
    :init
    (progn
      (setq-default smex-history-length 32
                    smex-save-file (concat space-macs-cache-directory
                                           ".smex-items"))
      ;; define the key binding at the very end in order to allow the user
      ;; to overwrite any key binding
      (add-hook 'e-macs-startup-hook
                (lambda () (space-macs/set-leader-keys
                             dotspace-macs-e-macs-command-key 'space-macs/smex)))
      (space-macs/set-leader-keys "m:" 'space-macs/smex-major-mode-commands)
      (global-set-key (kbd "M-x") 'space-macs/smex))))


