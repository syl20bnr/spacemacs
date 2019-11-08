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

(defconst dap-packages
  '(dap-mode))

(defun dap/init-dap-mode ()
  (use-package dap-mode
    :after (lsp-mode)
    :hook ((dap-ui-sessions-mode . evil-evilified-state)
           (dap-ui-breakpoints-ui-list-mode . evil-evilified-state)
           (dap-ui-locals-mode . evil-evilified-state)
           (dap-ui-inspect-mode . evil-evilified-state))
    :config
    (progn
      (dap-mode 1)
      (dap-ui-mode 1)
      ;; mouse support
      (spacemacs|add-toggle dap-mouse
        :status dap-tooltip-mode
        :on (progn (dap-tooltip-mode)
                   (tooltip-mode))
        :off (progn (dap-tooltip-mode -1)
                    (tooltip-mode -1))
        :documentation "Enable mouse support in DAP mode.")
      (when dap-enable-mouse-support
        (spacemacs/toggle-dap-mouse-on))
      ;; key bindings
      (dolist (mode spacemacs--dap-supported-modes)

        ;; avoid clash with other debug key bindings
        (spacemacs/set-leader-keys-for-major-mode mode "db" nil)

        (spacemacs/declare-prefix-for-mode mode "md" "debug")
        (spacemacs/declare-prefix-for-mode mode "mdb" "breakpoints")
        (spacemacs/declare-prefix-for-mode mode "mdd" "debugging")
        (spacemacs/declare-prefix-for-mode mode "mde" "eval")
        (spacemacs/declare-prefix-for-mode mode "mdI" "inspect")
        (spacemacs/declare-prefix-for-mode mode "mdS" "switch")
        (spacemacs/declare-prefix-for-mode mode "mdT" "toggles")
        (spacemacs/declare-prefix-for-mode mode "mdw" "debug windows")

        (spacemacs/set-leader-keys-for-major-mode mode
          ;; transient state
          "d."  #'dap-hydra
          ;; repl
          "d'"  #'dap-ui-repl
          ;; abandon
          "da"  #'dap-disconnect
          "dA"  #'dap-delete-all-sessions
          ;; breakpoints
          "dbb" #'dap-breakpoint-toggle
          "dbc" #'dap-breakpoint-condition
          "dbl" #'dap-breakpoint-log-message
          "dbh" #'dap-breakpoint-hit-condition
          "dba" #'dap-breakpoint-add
          "dbd" #'dap-breakpoint-delete
          "dbD" #'dap-breakpoint-delete-all
          ;; debuging/running
          "ddd" #'dap-debug
          "dde" #'dap-debug-edit-template
          "ddl" #'dap-debug-last
          "ddr" #'dap-debug-recent
          ;; eval
          "dee" #'dap-eval
          "der" #'dap-eval-region
          "det" #'dap-eval-thing-at-point
          ;; inspect
          "dIi" #'dap-ui-inspect
          "dIr" #'dap-ui-inspect-region
          "dIt" #'dap-ui-inspect-thing-at-point
          ;; stepping
          "dc"  #'dap-continue
          "di"  #'dap-step-in
          "do"  #'dap-step-out
          "dr"  #'dap-restart-frame
          "ds"  #'dap-next
          "dv"  #'dap-ui-inspect-thing-at-point
          ;; switching
          "dSs" #'dap-switch-session
          "dSt" #'dap-switch-thread
          "dSf" #'dap-switch-frame
          ;; toggles
          "dTm" 'spacemacs/toggle-dap-mouse
          ;; windows
          "dwo" #'dap-go-to-output-buffer
          "dwl" #'dap-ui-locals
          "dws" #'dap-ui-sessions
          "dwb" #'dap-ui-breakpoints)))))
