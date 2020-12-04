;;; config.el --- Helm Configuration File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

;; Dumper

(defun helm/pre-dump ()
  (space-macs/dump-modes '(helm-mode)))

;; variables

;; TODO: remove dotspace-macs variables backward compatbility in version
;;       0.400 or later

(defvar helm-no-header (space-macs|dotspace-macs-backward-compatibility
                        dotspace-macs-helm-no-header nil)
  "if non nil, the helm header is hidden when there is only one source.")

(defvar helm-position (space-macs|dotspace-macs-backward-compatibility
                       dotspace-macs-helm-position bottom)
  "Position in which to show the `helm' mini-buffer.")

(defvar space-macs-helm-rg-max-column-number 512
  "Controls the maximum number of columns to display with ripgrep (otherwise
  omits a line)")

;; internals

;; for Helm Window position
(defvar space-macs-helm-display-help-buffer-regexp '("*.*Helm.*Help.**"))
(defvar space-macs-helm-display-buffer-regexp
  `("*.*helm.**"
    (display-buffer-in-side-window)
    (inhibit-same-window . t)
    (side . ,helm-position)
    (window-width . 0.6)
    (window-height . 0.4)))
(defvar space-macs-display-buffer-alist nil)


