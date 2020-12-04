;;; packages.el --- faust layer packages file for Space-macs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author:  Bart Brouns <bart@magnetophon.nl>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

;;; Code:

(defconst faust-packages
  '(
    company
    faust-mode
    yasnippet
    ))

(defun faust/post-init-company ()
  (space-macs|add-company-backends :modes faust-mode))

(defun faust/init-faust-mode ()
  (use-package faust-mode
    :defer t
    :mode "\\.\\(dsp\\|lib\\)\\'"
    :init
    (progn
      (space-macs/set-leader-keys-for-major-mode 'faust-mode
        "cf" 'space-macs/faust-to-firefox
        "cg" 'space-macs/faust-to-jack-gtk
        "cq" 'space-macs/faust-to-jack-qt))))

(defun faust/post-init-yasnippet ()
  (add-hook 'faust-mode-hook 'space-macs/load-yasnippet))


