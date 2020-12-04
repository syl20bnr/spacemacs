;;; packages.el --- Django Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq django-packages '(pony-mode))

(defun django/init-pony-mode ()
  (use-package pony-mode
    :defer t
    :init (progn
            (space-macs/declare-prefix-for-mode 'python-mode "mj" "django")
            (space-macs/declare-prefix-for-mode 'python-mode "mja" "fabric")
            (space-macs/declare-prefix-for-mode 'python-mode "mjf" "files")
            (space-macs/declare-prefix-for-mode 'python-mode "mji" "interactive")
            (space-macs/declare-prefix-for-mode 'python-mode "mjr" "runserver")
            (space-macs/declare-prefix-for-mode 'python-mode "mjs" "south/syncdb")
            (space-macs/declare-prefix-for-mode 'python-mode "mjt" "test")

            (space-macs/set-leader-keys-for-major-mode 'python-mode
              "jaf" 'pony-fabric
              "jad" 'pony-fabric-deploy

              "jfs" 'pony-goto-settings
              "jfc" 'pony-setting
              "jft" 'pony-goto-template
              "jfr" 'pony-resolve

              "jid" 'pony-db-shell
              "jis" 'pony-shell

              ; not including one-off management commands like "flush" and
              ; "startapp" even though they're implemented in pony-mode,
              ; because this is much handier
              "jm" 'pony-manage

              "jrd" 'pony-stopserver
              "jro" 'pony-browser
              "jrr" 'pony-restart-server
              "jru" 'pony-runserver
              "jrt" 'pony-temp-server

              "jsc" 'pony-south-convert
              "jsh" 'pony-south-schemamigration
              "jsi" 'pony-south-initial
              "jsm" 'pony-south-migrate
              "jss" 'pony-syncdb

              "jtd" 'pony-test-down
              "jte" 'pony-test-goto-err
              "jto" 'pony-test-open
              "jtt" 'pony-test
              "jtu" 'pony-test-up))))


