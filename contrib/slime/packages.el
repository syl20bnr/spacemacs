;;; packages.el --- slime Layer packages File for Spacemacs
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

(defvar slime-packages
  '(slime)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar slime-excluded-packages '()
  "List of packages to exclude.")

(defun slime/init-slime ()
  (use-package slime
    :commands slime-mode
    :init
    (progn
      (setq slime-contribs '(slime-fancy)
            inferior-lisp-program "sbcl")
      (add-to-hooks 'slime-mode '(lisp-mode-hook
                                  emacs-lisp-mode-hook
                                  scheme-mode-hook)))
    :config
    (message "loading slime...")
    (slime-setup)))
