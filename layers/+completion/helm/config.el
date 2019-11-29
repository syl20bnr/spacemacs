;;; config.el --- Helm Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Dumper

(defun helm/pre-dump ()
  (spacemacs/dump-modes '(helm-mode)))

;; variables

;; TODO: remove dotspacemacs variables backward compatbility in version
;;       0.400 or later

(defvar helm-no-header (spacemacs|dotspacemacs-backward-compatibility
                        dotspacemacs-helm-no-header nil)
  "if non nil, the helm header is hidden when there is only one source.")

(defvar helm-position (spacemacs|dotspacemacs-backward-compatibility
                       dotspacemacs-helm-position bottom)
  "Position in which to show the `helm' mini-buffer.")

(defvar spacemacs-helm-rg-max-column-number 512
  "Controls the maximum number of columns to display with ripgrep (otherwise
  omits a line)")

;; internals

;; for Helm Window position
(defvar spacemacs-helm-display-help-buffer-regexp '("*.*Helm.*Help.**"))
(defvar spacemacs-helm-display-buffer-regexp
  `("*.*helm.**"
    (display-buffer-in-side-window)
    (inhibit-same-window . t)
    (side . ,helm-position)
    (window-width . 0.6)
    (window-height . 0.4)))
(defvar spacemacs-display-buffer-alist nil)
