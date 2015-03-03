;;; packages.el --- ace-window Layer packages File for Spacemacs
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

(defvar ace-window-packages '(ace-window)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar ace-window-excluded-packages '()
  "List of packages to exclude.")

(defun ace-window/init-ace-window ()
  (use-package ace-window
    :defer t
    :init
    (evil-leader/set-key
      "bmm" 'ace-swap-window
      "wc"  'ace-delete-window
      "wm"  'ace-maximize-window
      "ww"  'ace-window)
    :config
    (progn
      ;; add support for golden-ratio
      (eval-after-load 'golden-ratio
        '(setq golden-ratio-extra-commands
               (append golden-ratio-extra-commands
                       '(ace-window
                         ace-delete-window
                         ace-select-window
                         ace-swap-window
                         ace-maximize-window)))))))
