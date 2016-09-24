;;; config.el --- Org configuration File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(defvar org-enable-bootstrap-support nil
  "If non-nil Twitter Bootstrap related packages are configured.")

(defvar org-enable-github-support nil
  "If non-nil Github related packages are configured.")

(defvar org-enable-reveal-js-support nil
  "If non-nil, enable export to reveal.js.")

(defvar org-projectile-file "TODOs.org"
  "The file to store project TODOs in. If this is a relative
path, one file per project is used (and the path is relative to
the project root). If it an absolute path, one global file is
used.")

(spacemacs|defvar-company-backends org-mode)
