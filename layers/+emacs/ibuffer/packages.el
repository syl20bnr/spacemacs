;;; packages.el --- ibuffer Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Aleksandr Guljajev <aleksandr.guljajev@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(setq ibuffer-packages
      '(
        ibuffer
        ibuffer-projectile
        ))

(defun ibuffer/init-ibuffer ()
  (use-package ibuffer
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys "bI" 'ibuffer)
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
      :mode ibuffer-mode
      :bindings
      "gr" 'ibuffer-update
      "gj" 'ibuffer-forward-filter-group
      "]"  'ibuffer-forward-filter-group
      "gk" 'ibuffer-backward-filter-group
      "["  'ibuffer-backward-filter-group)))

(defun ibuffer/init-ibuffer-projectile ()
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
