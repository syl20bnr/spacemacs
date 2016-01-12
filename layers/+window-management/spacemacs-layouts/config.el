;;; config.el --- Spacemacs Layouts Layer configuration File for Spacemacs
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

(defvar spacemacs-layouts-directory
  (expand-file-name (concat spacemacs-cache-directory "layouts/"))
  "Save layouts in this directory.")

(defvar layouts-enable-autosave nil
  "If true, saves perspectives to file per `layouts-autosave-delay'")

(defvar layouts-autosave-delay 900
  "Delay in seconds between each layouts auto-save.")
