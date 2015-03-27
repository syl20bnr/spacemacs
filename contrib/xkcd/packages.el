;;; packages.el --- xkcd Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar xkcd-packages '(xkcd))

(defun xkcd/init-xkcd ()
  (use-package xkcd-mode
    :defer t
    :init
    (progn
      (setq xkcd-cache-dir (concat spacemacs-cache-directory "xkcd/"))
      (when (not (file-directory-p xkcd-cache-dir))
        (make-directory xkcd-cache-dir))
      (evil-leader/set-key
        "ax" 'xkcd)
      (add-to-list 'evil-emacs-state-modes 'xkcd-mode))
    :config
    (progn
      (define-key xkcd-mode-map (kbd "j") 'xkcd-next)
      (define-key xkcd-mode-map (kbd "k") 'xkcd-prev))))
