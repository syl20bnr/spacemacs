;;; config.el --- Auto-completion configuration File for Spacemacs
;;
;; Copyright (c) 2012-2014 Aleksandr Guljajev
;; Copyright (c) 2014-2015 Aleksandr Guljajev & Contributors
;;
;; Author: Aleksandr Guljajev <gulj.aleks@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar ibuffer-group-by-major-mode t
  "If non nil ibuffer will start with buffers grouped by major mode.")

(defvar ibuffer-group-by-projectile nil
  "If non nil ibuffer will start with buffers grouped by projects.
  Will be ignored if ibuffer-group-by-major-mode is non nil")
