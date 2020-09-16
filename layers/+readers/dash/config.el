;;; config.el --- Dash Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar dash-autoload-common-docsets t
  "If non nil, autoload all installed docsets as common docsets")

(defvar dash-docs-docset-newpath "~/.docsets"
  "Path containing dash docsets.")

(defvaralias 'helm-dash-docset-newpath 'dash-docs-docset-newpath)
