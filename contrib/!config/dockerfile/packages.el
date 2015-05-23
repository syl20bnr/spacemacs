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

(setq dockerfile-packages
  '(
    dockerfile-mode
    ))

(defun dockerfile/init-dockerfile-mode ()
  (use-package dockerfile-mode
    :defer t
    :config
    (progn
      (evil-leader/set-key-for-mode 'dockerfile-mode
         "mcb" 'dockerfile-build-buffer
       )))
 )
