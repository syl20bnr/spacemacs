;;; config.el --- Circe Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Aidan Nyquist <contact@aidannyquist.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(defvar circe-spacemacs-layout-name "@circe"
  "Name used in the setup for `spacemacs-layouts' micro-state")

(defvar circe-spacemacs-layout-binding "i"
  "Binding used in the setup for `spacemacs-layouts' micro-state")

(defvar circe-modes '(circe-channel-mode circe-query-mode circe-server-mode)
  "List of modes that are used by circe")

(defvar circe-helm-for-username-completion 'nil
  "Whether or not to use helm for username completion")
