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
