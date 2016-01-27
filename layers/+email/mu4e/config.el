;;; config.el --- mu4e Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
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

(defvar mu4e-enable-notifications nil
  "If non-nil, enable desktop notifications for unread emails.")

(defvar mu4e-enable-mode-line nil
  "If non-nil, enable display of unread emails in mode-line.")

(when mu4e-installation-path
  (push mu4e-installation-path load-path))
