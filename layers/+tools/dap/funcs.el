;;; packages.el --- DAP mode functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Ivan Yonchovski (yyoncho@gmail.com)
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/dap-bind-keys-for-mode (mode)
  "Define key bindings for the specific MODE."

  (spacemacs/declare-prefix-for-mode mode "md" "debug")
  (spacemacs/declare-prefix-for-mode mode "mdd" "debuging")
  (spacemacs/declare-prefix-for-mode mode "mdb" "breakpoints")
  (spacemacs/declare-prefix-for-mode mode "mdw" "debug windows")
  (spacemacs/declare-prefix-for-mode mode "mdS" "switch")
  (spacemacs/declare-prefix-for-mode mode "mdI" "inspect")
  (spacemacs/declare-prefix-for-mode mode "mde" "eval")

  (spacemacs/set-leader-keys-for-major-mode mode
    ;; debuging/running
    "ddd" #'dap-debug
    "ddl" #'dap-debug-last
    "ddr" #'dap-debug-recent
    ;; stepping
    "dc" #'dap-continue
    "di" #'dap-step-in
    "do" #'dap-step-out
    "ds" #'dap-next
    "dv" #'dap-ui-inspect-thing-at-point
    "dr" #'dap-restart-frame
    ;; transient state
    "d." #'dap-hydra
    ;; abandon
    "da" #'dap-disconnect
    "dA" #'dap-delete-all-sessions
    ;; eval
    "dee" #'dap-eval
    "der" #'dap-eval-region
    "det" #'dap-eval-thing-at-point
    ;; switching
    "dSs" #'dap-switch-session
    "dSt" #'dap-switch-thread
    "dSf" #'dap-switch-frame
    ;; inspect
    "dIi" #'dap-ui-inspect
    "dIr" #'dap-ui-inspect-region
    "dIt" #'dap-ui-inspect-thing-at-point
    ;; breakpoints
    "dbb" #'dap-breakpoint-toggle
    "dbc" #'dap-breakpoint-condition
    "dbl" #'dap-breakpoint-log-message
    "dbh" #'dap-breakpoint-hit-condition
    "dba" #'dap-breakpoint-add
    "dbd" #'dap-breakpoint-delete
    "dbD"  #'dap-breakpoint-delete-all
    ;; repl
    "d'"  #'dap-ui-repl
    ;; windows
    "dwo" #'dap-go-to-output-buffer
    "dwl" #'dap-ui-locals
    "dws" #'dap-ui-sessions
    "dwb" #'dap-ui-breakpoints))
