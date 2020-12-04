;;; packages.el --- ibuffer Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Aleksandr Guljajev <aleksandr.guljajev@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq ibuffer-packages
      '(
        ibuffer
        ibuffer-projectile
        ))

(defun ibuffer/init-ibuffer()
  (use-package ibuffer
    :defer t
    :init
    (progn
      (space-macs/set-leader-keys "bI" 'ibuffer)
      (global-set-key (kbd "C-x C-b") 'ibuffer)
      (defun space-macs//ibuffer-group-by-modes ()
        "Group buffers by modes."
        (when (eq 'modes ibuffer-group-buffers-by)
          (space-macs//ibuffer-create-buffs-group)))
      (add-hook 'ibuffer-hook 'space-macs//ibuffer-group-by-modes)

      ;; Use ibuffer to provide :ls
      (evil-ex-define-cmd "buffers" 'ibuffer))
    :config
    (evilified-state-evilify-map ibuffer-mode-map
      :mode ibuffer-mode
      :bindings
      "gr" 'ibuffer-update
      "gj" 'ibuffer-forward-filter-group
      "]"  'ibuffer-forward-filter-group
      "gk" 'ibuffer-backward-filter-group
      "["  'ibuffer-backward-filter-group)))

(defun ibuffer/init-ibuffer-projectile()
    (use-package ibuffer-projectile
      :defer t
      :init
      (progn
        (defun space-macs//ibuffer-group-by-projects ()
          "Group buffers by projects."
          (when (eq 'projects ibuffer-group-buffers-by)
            (ibuffer-projectile-set-filter-groups)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))
        (add-hook 'ibuffer-hook 'space-macs//ibuffer-group-by-projects))))


