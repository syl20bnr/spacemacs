;;; packages.el --- Spacemacs Misc. Layer packages File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
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
      ;; not activating `dumb-jump-mode' because it only adds key bindings, and
      ;; they conflict with existing bindings (see
      ;; https://github.com/syl20bnr/spacemacs/issues/7107)

      (spacemacs/set-leader-keys "jq" #'dumb-jump-quick-look)

      ;; Use Helm or Ivy as the selector for dumb-jump.
      (cond
       ((configuration-layer/layer-used-p 'ivy)
        (setq dumb-jump-selector 'ivy))
       ((configuration-layer/layer-used-p 'helm)
        (setq dumb-jump-selector 'helm)))

      ;; Since it's dumb, we add it to the end of the default jump handlers. At
      ;; the time of writing it is the only default jump handler. (gtags remains
      ;; mode-local)
      (add-to-list 'spacemacs-default-jump-handlers 'dumb-jump-go 'append))))

(defun spacemacs-misc/init-request ()
  (setq request-storage-directory
        (concat spacemacs-cache-directory "request/")))
