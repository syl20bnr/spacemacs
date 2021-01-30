;;; config.el --- Spacemacs-evil Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(defvar vim-style-visual-feedback nil
  "If non-nil objects are briefly highlighted performing an action.")

(defvar hybrid-style-visual-feedback nil
  "If non-nil objects are briefly highlighted performing an action.")

(defvar evil-lisp-safe-structural-editing-modes '()
  "A list of major mode symbols where safe structural editing is supported.")

(defvar spacemacs-evil-collection-allowed-list '(eww dired)
  "List of modes Spacemacs will allow to be evilified by ‘evil-collection-init’.")
