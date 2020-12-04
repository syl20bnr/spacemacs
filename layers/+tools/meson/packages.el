;;; packages.el --- meson layer packages file for Space-macs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Matthew Leach <dev@mattleach.net>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst meson-packages
  '(meson-mode))

(defun meson/init-meson-mode ()
  (use-package meson-mode
    :defer t
    :mode (("meson\\.build\\'" . meson-mode))
    :config
    (space-macs/declare-prefix-for-mode 'meson-mode "mh" "help")
    (space-macs/set-leader-keys-for-major-mode 'meson-mode "hh" 'meson-lookup-doc-at-point)

    ;; Make sure that standard space-macs f1 help bindings prevail
    (define-key meson-mode-map [f1] nil)))


