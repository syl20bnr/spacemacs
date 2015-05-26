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

(setq smex-packages '(smex))

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
        (let ((smex-prompt-string "Emacs commands: "))
          (smex)))

      (defun spacemacs/smex-major-mode-commands ()
        "Reexecute smex with major mode commands only."
        (interactive)
        (let ((smex-prompt-string (format "%s commands: " major-mode)))
          (smex-major-mode-commands)))

      ;; define the key binding at the very end in order to allow the user
      ;; to overwrite any key binding
      (add-hook 'after-init-hook
                (lambda () (evil-leader/set-key dotspacemacs-command-key
                             'spacemacs/smex)))
      (evil-leader/set-key "m:" 'spacemacs/smex-major-mode-commands)
      (global-set-key (kbd "M-x") 'spacemacs/smex)))) 
