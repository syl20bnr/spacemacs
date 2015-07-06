;;; packages.el --- Django Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq django-packages
  '(
    pony-mode
    ))

(defun django/init-pony-mode ()
  (use-package pony-mode
    :defer t
    :init (progn
            (evil-leader/set-key-for-mode 'python-mode
              ; d*j*ango f*a*bric
              "mjaf" 'pony-fabric
              "mjad" 'pony-fabric-deploy
              ; d*j*ango *f*iles
              "mjfs" 'pony-goto-settings
              "mjfc" 'pony-setting
              "mjft" 'pony-goto-template
              "mjfr" 'pony-resolve
              ; d*j*ango *i*nteractive
              "mjid" 'pony-db-shell
              "mjis" 'pony-shell
              ; d*j*ango *m*anage
              ; not including one-off management commands like "flush" and
              ; "startapp" even though they're implemented in pony-mode,
              ; because this is much handier
              "mjm" 'pony-manage
              ; d*j*ango *r*unserver
              "mjrd" 'pony-stopserver
              "mjro" 'pony-browser
              "mjrr" 'pony-restart-server
              "mjru" 'pony-runserver
              "mjrt" 'pony-temp-server
              ; d*j*ango *s*outh/*s*yncdb
              "mjsc" 'pony-south-convert
              "mjsh" 'pony-south-schemamigration
              "mjsi" 'pony-south-initial
              "mjsm" 'pony-south-migrate
              "mjss" 'pony-syncdb
              ; d*j*ango *t*est
              "mjtd" 'pony-test-down
              "mjte" 'pony-test-goto-err
              "mjto" 'pony-test-open
              "mjtt" 'pony-test
              "mjtu" 'pony-test-up))))
