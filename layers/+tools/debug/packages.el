;;; packages.el --- Debug Layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Troy Hinckley <troy.hinckley@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst debug-packages
  '(realgud))

(defun debug/init-realgud ()
  (use-package realgud
    :defer t
    :init
    (progn
      (dolist (debugger (mapcar 'spacemacs/debug-generate-symbol
                                debug-additional-debuggers))
        (autoload debugger "realgud" nil t))
      (advice-add 'realgud-short-key-mode-setup
                  :before #'spacemacs/debug-short-key-state)
      (evilified-state-evilify-map realgud:shortkey-mode-map
        :eval-after-load realgud
        :mode realgud-short-key-mode
        :bindings
        "bb" 'realgud:cmd-break
        "bc" 'realgud:cmd-clear
        "bd" 'realgud:cmd-delete
        "bs" 'realgud:cmd-disable
        "be" 'realgud:cmd-enable
        "c" 'realgud:cmd-continue
        "i" 'realgud:cmd-step
        "J" 'realgud:cmd-jump
        "o" 'realgud:cmd-finish
        "q" 'realgud:cmd-quit
        "r" 'realgud:cmd-restart
        "s" 'realgud:cmd-next
        "S" 'realgud-window-cmd-undisturb-src
        "v" 'realgud:cmd-eval-dwim))))
