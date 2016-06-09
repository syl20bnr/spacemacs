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

(defvar spacemacs--ts-full-hint-toggle 0
  "Toggle display of transient states documentations.")

(defvar spacemacs--last-selected-layout dotspacemacs-default-layout-name
  "Previously selected layout.")

(defvar spacemacs--custom-layout-alist nil
  "List of custom layouts with their bound keys.
 Do not modify directly, use provided `spacemacs|define-custom-layout'")

(defvar spacemacs--layouts-autosave-timer nil
  "Timer for layouts auto-save.")
