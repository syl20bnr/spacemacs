;;; packages.el --- vinegar Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
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


(setq vinegar-packages
      '(
        diff-hl
        ;; dired+
        (dired :location built-in)
        ))

(defun vinegar/init-dired+ ()
  (use-package dired+
    :defer t
    :init
    (progn
      (setq diredp-hide-details-initially-flag t)
      (setq diredp-hide-details-propagate-flag t)
      ;; use single buffer for all dired navigation
      ;; disable font themeing from dired+
      (setq font-lock-maximum-decoration (quote ((dired-mode . 1) (t . t))))
      (toggle-diredp-find-file-reuse-dir 1)
      )))

(defun vinegar/post-init-diff-hl ()
  (use-package diff-hl
    :defer t
    :init
    (progn
      (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
      )))

(defun vinegar/post-init-dired ()
  (use-package dired
    :defer t
    :config
    (evilified-state-evilify dired-mode dired-mode-map
      "j"         'vinegar/move-down
      "k"         'vinegar/move-up
      "-"         'vinegar/up-directory
      "0"         'dired-back-to-start-of-files
      "="         'vinegar/dired-diff
      (kbd "C-j") 'dired-next-subdir
      (kbd "C-k") 'dired-prev-subdir
      "I"         'vinegar/dotfiles-toggle
      (kbd "~")   '(lambda ()(interactive) (find-alternate-file "~/"))
      (kbd "RET") (if vinegar-reuse-dired-buffer
                      'dired-find-alternate-file
                    'dired-find-file)
      "f"         (if (configuration-layer/layer-used-p 'ivy)
                      'counsel-find-file
                    'helm-find-files)
      "J"         'dired-goto-file
      (kbd "C-f") 'find-name-dired
      "H"         'diredp-dired-recent-dirs
      "T"         'dired-tree-down
      "K"         'dired-do-kill-lines
      "r"         'revert-buffer
      (kbd "C-r") 'dired-do-redisplay
      "gg"        'vinegar/back-to-top
      "G"         'vinegar/jump-to-bottom)))
