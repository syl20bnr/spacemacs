;;; config.el --- Python Layer Configuration File for Spacemacs
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

;; variables

(spacemacs|defvar-company-backends python-mode)
(spacemacs|defvar-company-backends inferior-python-mode)
(spacemacs|defvar-company-backends pip-requirements-mode)

(defvar python-enable-yapf-format-on-save nil
  "If non-nil, automatically format code with YAPF on save.")

(defvar python-test-runner 'nose
  "Test runner to use. Possible values are `nose' or `pytest'.")

;; Command prefixes

;; not supported for now
;; (setq python/key-binding-prefixes
;;       '(; django/pony-mode
;;         ("mj" . "django")
;;         ("mjr" . "django-runserver")
;;         ("mji" . "django-shells")
;;         ("mjs" . "django-south/syncdb")
;;         ("mjt" . "django-test")
;;         ("mjf" . "django-files")
;;         ("mja" . "django-fabric")
;;         ; pyenv/pyenv-mode
;;         ("mp" . "pyenv")))
;; (mapc (lambda (x) (spacemacs/declare-prefix (car x) (cdr x)))
;;       python/key-binding-prefixes)
