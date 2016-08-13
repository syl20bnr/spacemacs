;;; packages.el --- Spacemacs Misc. Layer packages File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq spacemacs-misc-packages
      '(
        dumb-jump
        request
        ))

(defun spacemacs-misc/init-dumb-jump ()
  (use-package dumb-jump
    :defer t
    :init
    (progn
      ;; Since it's dumb, we add it to the end of the default jump handlers. At
      ;; the time of writing it is the only default jump handler. (gtags remains
      ;; mode-local)
      (add-to-list 'spacemacs-default-jump-handlers 'dumb-jump-go 'append)
      (add-hook 'prog-mode-hook 'dumb-jump-mode))))

(defun spacemacs-misc/init-request ()
  (setq request-storage-directory
        (concat spacemacs-cache-directory "request/")))
