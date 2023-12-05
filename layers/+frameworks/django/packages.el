;;; packages.el --- Django Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(setq django-packages '(pony-mode))

(defun django/init-pony-mode ()
  (use-package pony-mode
    :defer t
    :init 
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
      "jtu" 'pony-test-up)))
