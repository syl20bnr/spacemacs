;;; packages.el --- ibuffer Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Aleksandr Guljajev <aleksandr.guljajev@gmail.com>
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
      (spacemacs/set-leader-keys "bB" 'ibuffer)
      (global-set-key (kbd "C-x C-b") 'ibuffer)
      (defun spacemacs//ibuffer-group-by-modes ()
        "Group buffers by modes."
        (when (eq 'modes ibuffer-group-buffers-by)
          (spacemacs//ibuffer-create-buffs-group)))
      (add-hook 'ibuffer-hook 'spacemacs//ibuffer-group-by-modes)

      ;; Use ibuffer to provide :ls
      (evil-ex-define-cmd "buffers" 'ibuffer))
    :config
    (evilified-state-evilify-map ibuffer-mode-map
      :mode ibuffer-mode)))

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
