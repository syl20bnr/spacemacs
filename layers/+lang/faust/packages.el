;;; packages.el --- faust layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author:  Bart Brouns <bart@magnetophon.nl>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
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
  (spacemacs|add-company-backends :modes faust-mode))

(defun faust/init-faust-mode ()
  (use-package faust-mode
    :defer t
    :mode "\\.\\(dsp\\|lib\\)\\'"
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'faust-mode
        "cf" 'spacemacs/faust-to-firefox
        "cg" 'spacemacs/faust-to-jack-gtk
        "cq" 'spacemacs/faust-to-jack-qt))))

(defun faust/post-init-yasnippet ()
  (add-hook 'faust-mode-hook 'spacemacs/load-yasnippet))
