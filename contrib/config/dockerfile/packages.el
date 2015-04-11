;;; packages.el --- dockerfile Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2015 Sylvain Benner
;; Copyright (c) 2015 Alan Zimmerman & Contributors
;;
;; Author: Alan Zimmerman <alan.zimm@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar dockerfile-packages
  '(
    dockerfile-mode
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun dockerfile/init-dockerfile-mode ()
  (use-package dockerfile-mode
    :defer t
    :config
    (progn
      (evil-leader/set-key-for-mode 'dockerfile-mode
         "mcb" 'dockerfile-build-buffer
       )))
 )
