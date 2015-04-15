;;; packages.el --- deft Layer packages File for Spacemacs
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

(defvar deft-packages
  '(deft))

(defun deft/init-deft ()
  (use-package deft
    :defer t
    :init
    (progn
      (defun spacemacs/deft ()
        "Helper to call deft and then fix things so that it is nice and works"
        (interactive)
        (deft)
        ;; Hungry delete wrecks deft's DEL override
        (when (fboundp 'hungry-delete-mode)
          (hungry-delete-mode -1))
        ;; When opening it you always want to filter right away
        (evil-insert-state nil))
      (evil-leader/set-key "an" 'spacemacs/deft)
      )
    :config
    (progn
      (evil-leader/set-key-for-mode 'deft-mode
        "m d" 'deft-delete-file
        "m r" 'deft-rename-file
        "m i" 'deft-toggle-incremental-search
        "m n" 'deft-new-file)
      (setq deft-extension "txt"
            deft-text-mode 'org-mode
            deft-use-filename-as-title t))))
