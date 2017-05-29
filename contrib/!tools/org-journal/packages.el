;;; packages.el --- org-journal Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Mattias Lundell & Contributors
;;
;; Author: Mattias Lundell <mattias@lundell.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar org-journal-packages '(org-journal)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun org-journal/init-org-journal ()
  (use-package org-journal))
