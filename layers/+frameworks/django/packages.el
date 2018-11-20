;;; packages.el --- Django Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq django-packages '(pony-mode))

(defun django/init-pony-mode ()
  (use-package pony-mode
    :defer t
    :init (progn
            (spacemacs/declare-prefix-for-mode 'python-mode "mj" "django")
            (spacemacs/declare-prefix-for-mode 'python-mode "mja" "fabric")
            (spacemacs/declare-prefix-for-mode 'python-mode "mjf" "files")
            (spacemacs/declare-prefix-for-mode 'python-mode "mji" "interactive")
            (spacemacs/declare-prefix-for-mode 'python-mode "mjr" "runserver")
            (spacemacs/declare-prefix-for-mode 'python-mode "mjs" "south/syncdb")
            (spacemacs/declare-prefix-for-mode 'python-mode "mjt" "test")

            (spacemacs/set-leader-keys-for-major-mode 'python-mode
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
