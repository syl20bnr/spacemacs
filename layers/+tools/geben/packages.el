;;; packages.el --- geben layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Franz Luther Neulist Carroll <franzneulistcarroll@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst geben-packages '(geben))


(defun geben/init-geben ()
  (use-package geben
    :init
    (progn
      (spacemacs/set-leader-keys
        "Ga" 'geben
        "Gr" 'geben-run
        "Gx" 'geben-stop
        "Ge" 'geben-end
        "Gp" 'geben-proxy
        "Gn" 'geben-proxy-end
        "Gw" 'geben-where
        "Gq" 'geben-quit-window
        "Gv" 'geben-set-redirect
        "Gh" 'geben-mode-help
        "Gm" 'geben-toggle-pause-at-entry-line-flag
        "Gj" 'geben-eval-expression
        "Gk" 'geben-eval-current-word
        "Gi" 'geben-run-to-cursor
        "Gg" 'geben-full-frame-mode
        "Go" 'geben-open-file
        "Gf" 'geben-find-file

        "Gcd" 'geben-display-context
        "Gcm" 'geben-context-mode
        "Gch" 'geben-context-mode-help
        "Gcr" 'geben-context-mode-refresh

        "Gtt" 'geben-show-backtrace
        "Gtm" 'geben-backtrace-mode
        "Gth" 'geben-backtrace-mode-help
        "Gtg" 'geben-backtrace-mode-goto
        "Gth" 'geben-backtrace-mode-mouse-goto
        "Gtc" 'geben-backtrace-mode-mode-context

        "Gsi" 'geben-step-into
        "Gso" 'geben-step-over
        "Gsu" 'geben-step-out
        "Gsa" 'geben-step-again

        "Gbm" 'geben-breakpoint-menu
        "Gbi" 'geben-show-breakpoint-list
        "Gbc" 'geben-clear-breakpoints
        "Gbu" 'geben-unset-breakpoint-line

        "Gble" 'geben-breakpoint-list-execute
        "Gblr" 'geben-breakpoint-list-refresh
        "Gblu" 'geben-breakpoint-list-unmark
        "Gblh" 'geben-breakpoint-list-mode-help
        "Gblg" 'geben-breakpoint-list-mode-goto
        "Gbld" 'geben-breakpoint-list-mark-delete

        "Gbsi" 'geben-set-breakpoint-conditional
        "Gbsr" 'geben-set-breakpoint-return
        "Gbsl" 'geben-set-breakpoint-line
        "Gbse" 'geben-set-breakpoint-exception
        "Gbsw" 'geben-set-breakpoint-watch
        "Gbsc" 'geben-set-breakpoint-call))))
