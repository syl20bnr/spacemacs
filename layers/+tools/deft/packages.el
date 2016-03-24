;;; packages.el --- deft Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq deft-packages
  '(deft))

(defun deft/init-deft ()
  (use-package deft
    :defer t
    :init
    (progn
      (setq deft-extensions '("org" "md" "txt")
            deft-text-mode 'org-mode
            deft-use-filename-as-title t)
      (spacemacs/set-leader-keys "an" 'spacemacs/deft)

      (defun spacemacs/deft ()
        "Helper to call deft and then fix things so that it is nice and works"
        (interactive)
        (deft)
        ;; Hungry delete wrecks deft's DEL override
        (when (fboundp 'hungry-delete-mode)
          (hungry-delete-mode -1))
        ;; When opening it you always want to filter right away
        (evil-insert-state nil)))
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'deft-mode
        "d" 'deft-delete-file
        "i" 'deft-toggle-incremental-search
        "n" 'deft-new-file
        "r" 'deft-rename-file))))
