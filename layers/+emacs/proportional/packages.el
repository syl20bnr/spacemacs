;;; packages.el --- typography Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Johannes Goslar <swiesner@lunaryorn.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst proportional-packages
  '((proportional :location local)))

(defun proportional/init-proportional ()
  (add-to-list 'default-frame-alist `(font . ,proportional-font))
  (set-frame-font proportional-font)
  (set-fontset-font "fontset-default" 'symbol proportional-font)
  (setq variable-pitch `((t :family ,proportional-font)))

  (add-hook 'dired-mode-hook 'proportional/use-monospace)
  (add-hook 'helm-mode 'proportional/use-monospace)
  (add-hook 'spacemacs-buffer-mode-hook 'proportional/use-monospace)
  (add-hook 'tabulated-list-mode 'proportional/use-monospace)
  (add-hook 'magit-popup-mode-hook 'proportional/use-monospace)
  (add-hook 'which-key-init-buffer-hook 'proportional/use-monospace)
  (add-hook 'mu4e-headers-mode-hook 'proportional/use-monospace))

(defun proportional/post-init-which-key ()
  (edebug)
  (spacemacs|use-package-add-hook which-key
    :post-init
    (with-current-buffer which-key--buffer
      (proportional/use-monospace))))
