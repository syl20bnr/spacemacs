;;; funcs.el --- Debug layer function file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: troy.j.hinckley <troy.hinckley@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun debug-short-key-state (modeon)
  "Set evil-evilified-state explicitly."
  (if modeon
      (evil-evilified-state)
    (evil-normal-state)))

(defun debug-generate-symbol (debugger)
  "create realgud interactive function name from debugger"
  (intern (concat "realgud:" debugger)))

(defun spacemacs|add-realgud-debugger (mode debugger)
  "added a deubbger to major mode.
Note that this function MUST be called BEFORE init-realgud.
Therefore you should add it to a pre-init-realgud definition"
  (let ((dbg-name (debug-generate-symbol debugger)))
    (add-to-list 'debug-autoload-debuggers dbg-name)
    (spacemacs/set-leader-keys-for-major-mode mode
      "dd" dbg-name)))


