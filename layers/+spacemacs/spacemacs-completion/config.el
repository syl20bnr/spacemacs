;;; config.el --- Spacemacs Completion Layer configuration File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; Helm

(defface spacemacs-helm-navigation-ms-face
  `((t :background ,(face-attribute 'error :foreground)
       :foreground "black"))
  "Face for helm header when helm transient-state is activated."
  :group 'spacemacs)

;; from https://www.reddit.com/r/emacs/comments/2z7nbv/lean_helm_window/
(with-eval-after-load 'helm
  (defvar helm-source-header-default-background
    (face-attribute 'helm-source-header :background))
  (defvar helm-source-header-default-foreground
    (face-attribute 'helm-source-header :foreground))
  (defvar helm-source-header-default-box
    (face-attribute 'helm-source-header :box))
  (defvar helm-source-header-default-height
    (face-attribute 'helm-source-header :height) ))
