;;; funcs.el --- Debug layer function file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Troy Hinckley <troy.hinckley@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/debug-short-key-state (mode-on)
  "Set evil-evilified-state explicitly."
  (if mode-on
      (evil-evilified-state)
    (evil-normal-state)))

(defun spacemacs/debug-generate-symbol (debugger)
  "Create RealGUD interactive function name from DEBUGGER."
  (intern (concat "realgud:" debugger)))

(defun spacemacs/add-realgud-debugger (mode debugger)
  "Add RealGUD DEBUGGER to MODE."
  (let ((dbg-name (spacemacs/debug-generate-symbol debugger)))
    (spacemacs/set-leader-keys-for-major-mode mode
      "dd" dbg-name)
    (autoload dbg-name "realgud" nil t)))
