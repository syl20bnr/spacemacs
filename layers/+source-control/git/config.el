;;; config.el --- Git Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(defvar git-enable-magit-delta-plugin nil
  "If non nil, enable `magit-delta' plugin.")

(defvar git-enable-magit-gitflow-plugin nil
  "If non nil, enable `magit-gitflow' plugin.")

(defvar git-enable-magit-svn-plugin nil
  "If non nil, enable `magit-svn' plugin.")

(defvar git-enable-magit-todos-plugin nil
  "If non nil, enable `magit-todos' plugin.")

(defvar git-magit-status-fullscreen nil
  "If non nil magit-status buffer is displayed in fullscreen.")

(defvar spacemacs--git-blame-ts-full-hint-toggle nil
  "Display git blame transient state documentation.")
