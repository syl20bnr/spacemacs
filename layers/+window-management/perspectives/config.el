;;; config.el --- Git Layer configuration File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(defvar spacemacs-layouts-save-dir
  (expand-file-name (concat spacemacs-cache-directory "perspectives/"))
  "Save perspectives in this directory.")

(defvar layouts-enable-autosave nil
  "If true, saves perspectives to file per `layouts-autosave-delay'")

(defvar layouts-autosave-delay 900
  "Delay in seconds between each layouts auto-save.")
