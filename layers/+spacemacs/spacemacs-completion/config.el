;;; config.el --- Space-macs Completion Layer configuration File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3


;; Helm

(defvar helm-use-fuzzy (space-macs|dotspace-macs-backward-compatibility
                        dotspace-macs-helm-use-fuzzy always)
  "Controls fuzzy matching in helm. If set to `always', force fuzzy matching
  in all non-asynchronous sources. If set to `source', preserve individual
  source settings. Else, disable fuzzy matching in all sources.")

(defvar helm-enable-auto-resize (space-macs|dotspace-macs-backward-compatibility
                                 dotspace-macs-helm-resize nil)
  "If non nil, `helm' will try to minimize the space it uses.")

(defface space-macs-helm-navigation-ts-face
  `((t :background ,(face-attribute 'error :foreground)
       :foreground "black"))
  "Face for helm header when helm transient-state is activated."
  :group 'space-macs)

;; from https://www.reddit.com/r/e-macs/comments/2z7nbv/lean_helm_window/
(with-eval-after-load 'helm
  (defvar helm-source-header-default-background
    (face-attribute 'helm-source-header :background))
  (defvar helm-source-header-default-foreground
    (face-attribute 'helm-source-header :foreground))
  (defvar helm-source-header-default-box
    (face-attribute 'helm-source-header :box))
  (defvar helm-source-header-default-height
    (face-attribute 'helm-source-header :height) ))


