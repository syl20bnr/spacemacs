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

;; variables

;; TODO: remove dotspacemacs variables backward compatbility in version
;;       0.400 or later

(defmacro spacemacs|helm-backward-compatibility (variable default)
  "Return `or' sexp for backward compatibility with old dotspacemacs
values."
  `(or (and (boundp ',variable) ,variable) ',default))

(defvar helm-enable-auto-resize (spacemacs|helm-backward-compatibility
                                 dotspacemacs-helm-resize nil)
  "If non nil, `helm' will try to minimize the space it uses.")

(defvar helm-no-header (spacemacs|helm-backward-compatibility
                        dotspacemacs-helm-no-header nil)
  "if non nil, the helm header is hidden when there is only one source.")

(defvar helm-position (spacemacs|helm-backward-compatibility
                       dotspacemacs-helm-position bottom)
  "Position in which to show the `helm' mini-buffer.")

(defvar helm-use-fuzzy (spacemacs|helm-backward-compatibility
                        dotspacemacs-helm-use-fuzzy always)
  "Controls fuzzy matching in helm. If set to `always', force fuzzy matching
  in all non-asynchronous sources. If set to `source', preserve individual
  source settings. Else, disable fuzzy matching in all sources.")

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
