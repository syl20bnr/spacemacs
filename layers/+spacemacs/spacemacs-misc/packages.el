;;; packages.el --- Spacemacs Misc. Layer packages File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq spacemacs-misc-packages
      '(
        devdocs
        dumb-jump
        request))


(defun spacemacs-misc/init-dumb-jump ()
  (use-package dumb-jump
    :defer t
    :init
    (progn
      ;; Use Helm or Ivy as the selector for dumb-jump.
      (cond
       ((configuration-layer/layer-used-p 'ivy)
        (setq dumb-jump-selector 'ivy))
       ((configuration-layer/layer-used-p 'helm)
        (setq dumb-jump-selector 'helm)))

      ;; Enable xref-backend of dumb-jump. It's chosen only when no better
      ;; options is available
      (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))))

(defun spacemacs-misc/init-request ()
  (setq request-storage-directory
        (concat spacemacs-cache-directory "request/")))

(defun spacemacs-misc/init-devdocs ()
  (use-package devdocs
    :defer t
    :init
    (progn
      (defalias 'spacemacs/browse-docs-online-at-point 'devdocs-search)
      (spacemacs/set-leader-keys "hbd" #'spacemacs/browse-docs-online-at-point))))
