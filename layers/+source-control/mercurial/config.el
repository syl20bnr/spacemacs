;;; config.el --- Mercurial Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2016 Joseph Benden
;;
;; Author: Joseph Benden <joe@benden.us>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(defvar mercurial-monky-process-type nil
  "How Monky spawns Mercurial process for each request or use
Mercurial's command server feature to run several commands in
a single process instance. See `monky-process-type'.")

(defvar mercurial-monky-status-fullscreen nil
  "If non nil monky-status buffer is displayed in fullscreen.")
