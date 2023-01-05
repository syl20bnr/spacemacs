;;; core-use-package-ext.el --- Spacemacs Core File -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
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
(require 'core-fonts-support)
(require 'core-spacebind)

(defconst spacemacs--use-package-add-hook-keywords '(:pre-init
                                                     :post-init
                                                     :pre-config
                                                     :post-config))

(defvar spacemacs--use-package-extended nil)

(defmacro spacemacs|use-package-add-hook (name &rest plist)
  "Add post hooks to `:init' or `:config' arguments of an existing
configuration.

In order to use this macro the variable `use-package-inject-hooks'
must be non-nil. If it is not a warning will be issued.

This is useful in the dotfile to override the default configuration
of a package.

Usage:

  (spacemacs|use-package-add-hook package-name
     [:keyword [option]]...)

:pre-init      Code to run before the default `:init' configuration.
:post-init     Code to run after the default `:init' configuration.
:pre-config    Code to run before the default `:config' configuration.
:post-config   Code to run after the default `:config' configuration.

In practice the most useful hook is the `:post-config' where you can
override lazy-loaded settings."
  (declare (indent 1))
  (let ((name-symbol (if (stringp name) (intern name) name))
        (expanded-forms '()))
    (dolist (keyword spacemacs--use-package-add-hook-keywords)
      (let ((body (spacemacs/mplist-get-values plist keyword)))
        (when body
          (let ((hook (intern (format "use-package--%S--%s-hook"
                                      name-symbol
                                      (substring (format "%s" keyword) 1)))))
            (push `(add-hook ',hook (lambda nil ,@body t)) expanded-forms)))))
    `(progn ,@expanded-forms)))



(defun spacemacs/use-package-extend ()
  "Extend use-package with custom keywords."
  (when (and (require 'use-package nil t) (not spacemacs--use-package-extended))
    (setq use-package-verbose init-file-debug
          ;; inject use-package hooks for easy customization of stock package
          ;; configuration
          spacemacs--use-package-extended t
          use-package-inject-hooks t)
    (add-to-list 'use-package-keywords :spacebind t)
    (add-to-list 'use-package-keywords :spacediminish t)))

(defun use-package-normalize/:spacebind (name-symbol keyword args)
  (use-package-only-one (symbol-name keyword) args
    (lambda (label arg)
      (if (and (listp arg) (keywordp (car arg)))
          arg
        (use-package-error
         ":spacebind wants an arg list compatible with `spacebind' macro")))))

(defun use-package-handler/:spacebind (name-symbol keyword args rest state)
  (let ((body (use-package-process-keywords name-symbol rest state)))
    (if (null args)
        body
      (use-package-concat
       body
       `((spacemacs|spacebind ,@args))))))

(defun use-package-normalize/:spacediminish (name keyword args)
  (use-package-as-one (symbol-name keyword) args
    (apply-partially #'use-package-normalize-spacediminish name) t))

(defun use-package-handler/:spacediminish (name _keyword arg rest state)
  (let ((body (use-package-process-keywords name rest state)))
    (use-package-concat
     `((when (fboundp 'spacemacs|diminish)
         ,@(if (consp (car arg)) ;; e.g. ((MODE FOO BAR) ...)
               (mapcar (lambda (var) `(spacemacs|diminish ,@var))
                       arg)
             `((spacemacs|diminish ,@arg))))) ;; e.g. (MODE FOO BAR)
     body)))

(defun use-package-normalize-spacediminish (name label arg &optional recursed)
  "Normalize the arguments to `spacemacs|diminish' to a list of one of six forms:
     t
     SYMBOL
     STRING
     (SYMBOL STRING)
     (STRING STRING)
     (SYMBOL STRING STRING)"
  (let ((default-mode (use-package-as-mode name)))
    (pcase arg
      ;; (PATTERN ..) when not recursive -> go to recursive case
      ((and (or `(,x . ,y) `(,x ,y))
            (guard (and (not recursed)
                        (listp x)
                        (listp y))))
       (mapcar (lambda (var) (use-package-normalize-spacediminish name label var t))
               arg))
      ;; t -> (<PKG>-mode)
      ('t
       (list default-mode))
      ;; SYMBOL -> (SYMBOL)
      ((pred use-package-non-nil-symbolp)
       (list arg))
      ;; STRING -> (<PKG>-mode STRING)
      ((pred stringp)
       (list default-mode arg))
      ;; (SYMBOL) when recursed -> (SYMBOL)
      ((and `(,x)
            (guard (and recursed (use-package-non-nil-symbolp x))))
       arg)
      ;; (STRING) when recursed -> (<PKG>-mode STRING))
      ((and `(,x)
            (guard (and recursed (stringp x))))
       (cons default-mode arg))
      ;; (SYMBOL STRING) -> (SYMBOL STRING)
      ((and `(,x ,y)
            (guard (and (use-package-non-nil-symbolp x) (stringp y))))
       arg)
      ;; (STRING STRING) -> (<PKG>-mode STRING STRING)
      ((and `(,x ,y)
            (guard (and (stringp x) (stringp y))))
       (cons default-mode arg))
      ;; (SYMBOL STRING STRING) -> (SYMBOL STRING STRING)
      ((and `(,x ,y ,z)
            (guard (and (use-package-non-nil-symbolp x)
                        (stringp y)
                        (stringp z))))
       arg)
      (_
       (use-package-error
        (format
         "%s wants a symbol, string, (symbol string), (string string), (symbol string string) or list of these: %S"
         label arg))))))

(provide 'core-use-package-ext)
