;;; packages.el --- ibuffer Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Aleksandr Guljajev & Contributors
;;
;; Author: Sylvain Benner <aleksandr.guljajev@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar ibuffer-packages
  '(
    ibuffer
    ibuffer-projectile
    ))

(defun ibuffer/init-ibuffer()
  (use-package ibuffer
    :defer t
    :init
    (progn
      (evilify ibuffer-mode ibuffer-mode-map)
      (evil-leader/set-key "bB" 'ibuffer)
      (global-set-key (kbd "C-x C-b") 'ibuffer)

      (defun spacemacs//ibuffer-group-by-modes ()
        "Group buffers by modes."
        (when (eq 'modes ibuffer-group-buffers-by)
          (spacemacs//ibuffer-create-buffs-group)))
      (add-hook 'ibuffer-hook 'spacemacs//ibuffer-group-by-modes))))

(defun ibuffer/init-ibuffer-projectile()
    (use-package ibuffer-projectile
      :defer t
      :init
      (progn
        (defun spacemacs//ibuffer-group-by-projects ()
          "Group buffers by projects."
          (when (eq 'projects ibuffer-group-buffers-by)
            (ibuffer-projectile-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))
        (add-hook 'ibuffer-hook 'spacemacs//ibuffer-group-by-projects))))
