;;; packages.el --- smex Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar smex-packages '(smex)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun smex/init-smex ()
  (use-package smex
    :defer t
    :init
    (progn
      (setq-default smex-history-length 32
                    smex-save-file (concat spacemacs-cache-directory
                                           ".smex-items"))

      (defun spacemacs/smex ()
        "Execute smex with a better prompt."
        (interactive)
        (let ((smex-prompt-string "Emacs commands:"))
          (smex)))

      (defun spacemacs/smex-major-mode-commands ()
        "Reexecute smex with major mode commands only."
        (interactive)
        (let ((smex-prompt-string (format "%s commands:" major-mode)))
          (smex-major-mode-commands)))

      (defun spacemacs/smex-define-keys ()
        (when dotspacemacs-feature-toggle-leader-on-jk
          (evil-leader/set-key-for-mode 'ido-mode
            "g"   'smex-find-function
            "hdf" 'smex-describe-function
            "hw"  'smex-where-is))
        ;; (key-chord-define ido-completion-map (kbd "jk")
        ;;                   (cdr (assoc 'ido-mode evil-leader--mode-maps)))
        )

      (evil-leader/set-key dotspacemacs-command-key 'spacemacs/smex)
      (evil-leader/set-key "m:" 'spacemacs/smex-major-mode-commands)
      (global-set-key (kbd "M-x") 'smex)
      (global-set-key (kbd "M-X") 'smex)
      (add-to-list 'ido-setup-hook 'spacemacs/smex-define-keys)
      )
    ))
