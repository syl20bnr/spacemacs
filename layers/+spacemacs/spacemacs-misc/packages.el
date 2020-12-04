;;; packages.el --- Space-macs Misc. Layer packages File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq space-macs-misc-packages
      '(
        devdocs
        dumb-jump
        request
        ))

(defun space-macs-misc/init-dumb-jump ()
  (use-package dumb-jump
    :defer t
    :init
    (progn
      ;; not activating `dumb-jump-mode' because it only adds key bindings, and
      ;; they conflict with existing bindings (see
      ;; https://github.com/syl20bnr/space-macs/issues/7107)

      (space-macs/set-leader-keys "jq" #'dumb-jump-quick-look)

      ;; Use Helm or Ivy as the selector for dumb-jump.
      (cond
       ((configuration-layer/layer-used-p 'ivy)
        (setq dumb-jump-selector 'ivy))
       ((configuration-layer/layer-used-p 'helm)
        (setq dumb-jump-selector 'helm)))

      ;; Since it's dumb, we add it to the end of the default jump handlers. At
      ;; the time of writing it is the only default jump handler. (gtags remains
      ;; mode-local)
      (add-to-list 'space-macs-default-jump-handlers 'dumb-jump-go 'append))))

(defun space-macs-misc/init-request ()
  (setq request-storage-directory
        (concat space-macs-cache-directory "request/")))

(defun space-macs-misc/init-devdocs ()
  (use-package devdocs
    :defer t
    :init
    (progn
      (defalias 'space-macs/browse-docs-online-at-point 'devdocs-search)
      (space-macs/set-leader-keys "hbd" #'space-macs/browse-docs-online-at-point))))


