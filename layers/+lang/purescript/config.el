;;; config.el --- Purescript Layer config File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Ryan L. Bell
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables

(spacemacs|define-jump-handlers purescript-mode)

(defvar purescript-add-import-on-completion t
  "If non-nil adds imports for completed identifiers")

(defvar purescript-enable-rebuild-on-save nil
  "If non-nil rebuild on save is enabled")
