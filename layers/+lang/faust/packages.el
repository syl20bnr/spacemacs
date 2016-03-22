;;; packages.el --- faust layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author:  Bart Brouns <bart@magnetophon.nl>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst faust-packages
  '(company
    dumb-jump
    faust-mode
    yasnippet))

(defun faust/post-init-company ()
  (spacemacs|add-company-hook faust-mode))

;; Consider to move this package somewhere else if we use it for other languages.
(defun faust/init-dumb-jump ()
  (use-package dumb-jump
    :defer t
    :init
    (progn
      (add-hook 'faust-mode-hook 'dumb-jump-mode)
      (spacemacs/set-leader-keys-for-major-mode 'faust-mode
        "gb" 'dumb-jump-back
        "gg" 'dumb-jump-go))))

(defun faust/init-faust-mode ()
  (use-package faust-mode
    :defer t
    :mode "\\.\\(dsp\\|lib\\)\\'"
    :init (spacemacs/set-leader-keys-for-major-mode 'faust-mode
            "cf" 'spacemacs/faust-to-firefox
            "cg" 'spacemacs/faust-to-jack-gtk
            "cq" 'spacemacs/faust-to-jack-qt)))

(defun faust/post-init-yasnippet ()
  (add-hook 'faust-mode-hook 'spacemacs/load-yasnippet))
