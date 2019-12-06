;;; packages.el --- kubernetes layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Matt Bray <mattjbray@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst kubernetes-packages
  '(
    kubernetes
    kubernetes-evil
    ))

(defun kubernetes/init-kubernetes ()
  (use-package kubernetes
    :defer t
    ;; Autoload for 'kubernetes-overview is defined in "kubernetes-overview.el".
    ;; Add an autoload for the whole 'kubernetes package when kubernetes-overview is called.
    :commands (kubernetes-overview)
    :init (spacemacs/set-leader-keys "aK" 'kubernetes-overview)))

(defun kubernetes/init-kubernetes-evil ()
  (use-package kubernetes-evil
    :after kubernetes-overview))
