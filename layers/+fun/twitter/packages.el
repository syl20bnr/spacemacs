;;; packages.el --- twitter Layer packages File for Spacemacs
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

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq twitter-packages
    '(twittering-mode))

(defun twitter/init-twittering-mode ()
  (use-package twittering-mode
    :commands twit
    :init
    (evil-leader/set-key
      "at" 'twit)
    (when (configuration-layer/package-usedp 'flyspell)
      (add-hook 'twittering-edit-mode-hook (lambda () (flyspell-mode 1))))
    (push 'twittering-edit-mode evil-insert-state-modes)
    :config
    (setq twitter-images-directory
          (expand-file-name
           (concat spacemacs-cache-directory "twitter-images")))
    (unless (file-exists-p twitter-images-directory)
      (make-directory twitter-images-directory))
    (setq twittering-icon-mode t)
    (setq twittering-url-show-status nil)
    (setq twittering-use-master-password t)
    (setq twittering-use-icon-storage 1)))
