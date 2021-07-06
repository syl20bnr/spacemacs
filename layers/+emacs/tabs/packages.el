;;; packages.el --- tabs layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018, 2020 Sylvain Benner & Contributors
;;
;; Author: Deepu Puthrote <git@deepumohan.com>
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


(defconst tabs-packages
  '(centaur-tabs))

(defun tabs/init-centaur-tabs ()
  (use-package centaur-tabs
    :demand
    :config
    (setq tabs-show-icons t
          tabs-set-modified-marker t
          tabs-modified-marker "⚠"
          tabs-set-bar 'left)
    (when tabs-headline-match
      (centaur-tabs-headline-match))
    (if tabs-group-by-project
        (centaur-tabs-group-by-projectile-project)
      (centaur-tabs-group-buffer-groups))
    (centaur-tabs-mode t)

    (when tabs-auto-hide
      (add-hook 'window-setup-hook 'spacemacs//tabs-timer-hide)
      (add-hook 'find-file-hook 'spacemacs//tabs-timer-hide)
      (add-hook 'change-major-mode-hook 'spacemacs//tabs-timer-hide))
    :bind
    ("C-{" . spacemacs/tabs-backward)
    ("C-}" . spacemacs/tabs-forward)
    ("C-M-{" . centaur-tabs-move-current-tab-to-left)
    ("C-M-}" . centaur-tabs-move-current-tab-to-right)
    ("C-c t s" . centaur-tabs-counsel-switch-group)
    ("C-c t p" . centaur-tabs-group-by-projectile-project)
    ("C-c t g" . centaur-tabs-group-buffer-groups)))
