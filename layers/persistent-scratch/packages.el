;;; packages.el --- persistent-scratch Layer packages File for Spacemacs
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

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq persistent-scratch-packages
      '(persistent-scratch
        helm))

;; List of packages to exclude.
(setq persistent-scratch-excluded-packages '())

(defun persistent-scratch/init-persistent-scratch ()
  (use-package persistent-scratch
    :defer t
    :init
    (progn
      ;; add binding for temp scratch buffer
      (evil-leader/set-key
        "b t" 'spacemacs/new-empty-buffer)
      ;; clean up which-key
      (add-to-list 'which-key-description-replacement-alist '("persistent-scratch\/" . ""))
      (setq persistent-scratch-backup-directory
            (concat spacemacs-cache-directory ".scratch/")
            persistent-scratch-save-file
            (concat spacemacs-cache-directory ".persistent-scratch")
            persistent-scratch-scratch-buffer-p-function
            'persistent-scratch-persistent-file-match-regexp-p)
      (add-hook 'after-init-hook 'persistent-scratch-setup-default))))

(defun persistent-scratch/post-init-helm ()

  (defun persistent-scratch/scratch-buffers-list ()
    "Show list of buffers recognized as scratch buffers."
    (interactive)
    (advice-add 'helm-skip-boring-buffers :filter-return 'filter-scratch-buffers)
    (call-interactively 'helm-buffers-list)
    (advice-remove 'helm-skip-boring-buffers 'filter-scratch-buffers))

  (evil-leader/set-key
    "h s" 'persistent-scratch/scratch-buffers-list)
  )

