;;; config.el --- Git Layer configuration File for Spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables
(defvar perspectives-display-help t
  "If non-nil additional help is displayed when selecting a perspective.")

(defvar spacemacs-persp-nil-name "@Home"
  "Name o the `nil' perspective.")

(defvar spacemacs-persp-show-home-at-startup t
  "Whether to show the Spacemacs Home Buffer at startup")

(defvar spacemacs-persp-save-dir (expand-file-name "perspectives/"
                                                   spacemacs-cache-directory)
  "Save perspectives in this directory.")

(defvar persp-mode-autosave nil
  "If true, saves perspectives to file per `persp-autosave-interval'")

(defvar persp-autosave-interval 900
  "Delay in seconds between `persp-autosave'.")

(defvar persp-autosave-timer nil
  "Stores `persp-autosave' for removal on exit.")
