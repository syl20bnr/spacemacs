;;; config.el --- e-macs Lisp Layer configuration File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

;; Dumper

(defun e-macs-lisp/pre-dump ()
  (space-macs/dump-modes '(e-macs-lisp-mode)))

;; Variables

(space-macs|define-jump-handlers e-macs-lisp-mode)
(space-macs|define-jump-handlers lisp-interaction-mode)

(defvar e-macs-lisp-hide-namespace-prefix nil
  "If non-nil, hide namespace prefixes using nameless-mode.")


