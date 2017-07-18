;;; packages.el --- Debug Layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Troy Hinckley <troy.hinckley@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst debug-packages
  '(realgud))

(defun debug/init-realgud()
  (use-package realgud
    :defer t
    :init

    (dolist (debugger (mapcar 'debug-generate-symbol debug-additional-debuggers))
      (autoload debugger "realgud" nil t))

    (advice-add 'realgud-short-key-mode-setup
                :before #'debug-short-key-state)

    (evilified-state-evilify-map realgud:shortkey-mode-map
      :eval-after-load realgud
      :mode realgud-short-key-mode
      :bindings
      "s" 'realgud:cmd-next
      "i" 'realgud:cmd-step
      "o" 'realgud:cmd-finish
      "c" 'realgud:cmd-continue
      "J" 'realgud:cmd-jump

      "bb" 'realgud:cmd-break
      "bc" 'realgud:cmd-clear
      "bd" 'realgud:cmd-delete
      "bs" 'realgud:cmd-disable
      "be" 'realgud:cmd-enable

      "v" 'realgud:cmd-eval-dwim
      "r" 'realgud:cmd-restart
      "q" 'realgud:cmd-quit
      "S" 'realgud-window-cmd-undisturb-src)))
