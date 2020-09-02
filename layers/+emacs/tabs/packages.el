;;; packages.el --- tabs layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018, 2020 Sylvain Benner & Contributors
;;
;; Author: Deepu Puthrote <git@deepumohan.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst tabs-packages
  '(centaur-tabs))

(defun tabs/init-centaur-tabs ()
  (use-package centaur-tabs
    :demand
    :config
    (setq centaur-tabs-cycle-scope tabs-navigation
          centaur-tabs-gray-out-icons tabs-gray-out-unselected
          centaur-tabs-height tabs-height
          centaur-tabs-modified-marker tabs-modified-marker
          centaur-tabs-set-bar tabs-set-bar
          centaur-tabs-set-icons tabs-show-icons
          centaur-tabs-set-modified-marker tabs-set-modified-marker
          centaur-tabs-show-navigation-buttons t
          centaur-tabs-style tabs-style)
    (when tabs-headline-match
      (centaur-tabs-headline-match))
    (if tabs-group-by-project
        (centaur-tabs-group-by-projectile-project)
      (centaur-tabs-group-buffer-groups))
    (centaur-tabs-mode t)
    :bind
    ("C-{" . centaur-tabs-backward)
    ("C-}" . centaur-tabs-forward)
    ("C-M-{" . centaur-tabs-move-current-tab-to-left)
    ("C-M-}" . centaur-tabs-move-current-tab-to-right)
    ("C-c t s" . centaur-tabs-counsel-switch-group)
    ("C-c t p" . centaur-tabs-group-by-projectile-project)
    ("C-c t g" . centaur-tabs-group-buffer-groups)))
