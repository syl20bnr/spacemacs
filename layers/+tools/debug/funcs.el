;;; funcs.el --- Debug layer function file for Space-macs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Troy Hinckley <troy.hinckley@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs/debug-short-key-state (mode-on)
  "Set evil-evilified-state explicitly."
  (if mode-on
      (evil-evilified-state)
    (evil-normal-state)))

(defun space-macs/debug-generate-symbol (debugger)
  "Create RealGUD interactive function name from DEBUGGER."
  (intern (concat "realgud:" debugger)))

(defun space-macs/add-realgud-debugger (mode debugger)
  "Add RealGUD DEBUGGER to MODE."
  (let ((dbg-name (space-macs/debug-generate-symbol debugger)))
    (space-macs/set-leader-keys-for-major-mode mode
      "dd" dbg-name)
    (autoload dbg-name "realgud" nil t)))


