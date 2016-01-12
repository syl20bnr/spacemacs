;;; extensions.el --- Colors Layer Extensions File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq colors-post-extensions '(nyan-mode))

(defun colors/init-nyan-mode ()
  (use-package nyan-mode
    :if colors-enable-nyan-cat-progress-bar
    :config
    (progn
      (setq nyan-wavy-trail t)
      (setq nyan-animate-nyancat t)
      (nyan-mode)
      ;; explicitly re-enable the cat for the first GUI client
      (spacemacs|do-after-display-system-init
       (nyan-mode -1)
       (nyan-mode))

      (spacemacs|add-toggle nyan-cat-progress-bar
        :status nyan-mode
        :on (nyan-mode)
        :off (nyan-mode -1)
        :documentation "Show a nyan cat progress bar in the mode-line."
        :evil-leader "tmn"))))
