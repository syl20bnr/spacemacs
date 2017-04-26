;;; packages.el --- debug layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: hodge <hodge@5.local>
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
    :commands (realgud:gdb)
    :init
    (when c-c++-enable-debug
      (spacemacs/set-leader-keys-for-major-mode 'c++-mode
        "dd" 'realgud:gdb
        "de" 'realgud:cmd-eval-dwim)
      (spacemacs/set-leader-keys-for-major-mode 'c-mode
        "dd" 'realgud:gdb
        "de" 'realgud:cmd-eval-dwim)
      (evilified-state-evilify-map realgud:shortkey-mode-map
        :eval-after-load realgud
        :mode realgud-short-key-mode
        :bindings
        "s" 'realgud:cmd-next
        "i" 'realgud:cmd-step
        "b" 'realgud:cmd-break
        "B" 'realgud:cmd-clear
        "o" 'realgud:cmd-finish
        "c" 'realgud:cmd-continue
        "e" 'realgud:cmd-eval
        "r" 'realgud:cmd-restart
        "q" 'realgud:cmd-quit
        "S" 'realgud-window-cmd-undisturb-src)
      )
    ))
