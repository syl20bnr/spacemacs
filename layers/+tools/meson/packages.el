;;; packages.el --- meson layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Matthew Leach <dev@mattleach.net>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst meson-packages
  '(meson-mode))

(defun meson/init-meson-mode ()
  (use-package meson-mode
    :defer t
    :mode (("meson\\.build\\'" . meson-mode))
    :config
    (spacemacs/declare-prefix-for-mode 'meson-mode "mh" "help")
    (spacemacs/set-leader-keys-for-major-mode 'meson-mode "hh" 'meson-lookup-doc-at-point)

    ;; Make sure that standard spacemacs f1 help bindings prevail
    (define-key meson-mode-map [f1] nil)))
