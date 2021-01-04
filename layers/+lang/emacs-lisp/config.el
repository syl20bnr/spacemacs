;;; config.el --- Emacs Lisp Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Dumper

(defun emacs-lisp/pre-dump ()
  (spacemacs/dump-modes '(emacs-lisp-mode)))

;; Variables

(spacemacs|define-jump-handlers emacs-lisp-mode)
(spacemacs|define-jump-handlers lisp-interaction-mode)
(spacemacs|define-jump-handlers inferior-emacs-lisp-mode)

(defvar emacs-lisp-hide-namespace-prefix nil
  "If non-nil, hide namespace prefixes using nameless-mode.")
