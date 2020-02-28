;;; config.el --- mu4e Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar mu4e-installation-path nil
  "Installation path for mu4e.")

(defvar mu4e-spacemacs-layout-name "@Mu4e"
  "Name used in the setup for `spacemacs-layouts' micro-state")

(defvar mu4e-spacemacs-layout-binding "m"
  "Binding used in the setup for `spacemacs-layouts' micro-state")

(defvar mu4e-spacemacs-kill-layout-on-exit t
  "When `t` exiting mu4e app will automatically kill its layout")

(defvar mu4e-enable-async-operations nil
  "Prefer async operations when sending emails.")

(defvar mu4e-enable-notifications nil
  "If non-nil, enable desktop notifications for unread emails.")

(defvar mu4e-enable-mode-line nil
  "If non-nil, enable display of unread emails in mode-line.")

(defvar mu4e-use-maildirs-extension nil
  "Use mu4e-maildirs-extension package if value is non-nil.")

(defvar mu4e-list-modes
  '(mu4e-main-mode mu4e-headers-mode)
  "Modes that are associated with mu4e buffers.")

(defvar mu4e-view-modes
  '(mu4e-view-mode mu4e-compose-mode mu4e-loading-mode)
  "Modes that are associated with mu4e buffers.")

(when mu4e-installation-path
  (add-to-list 'load-path mu4e-installation-path))
