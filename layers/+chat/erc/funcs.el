;;; funcs.el --- Spacemacs ERC Layer functions File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun erc//servers (server-list)
  (dolist (s server-list)
    (setq s (copy-list s))
    (apply (if
               (plist-get (cdr s) :ssl)
               (progn
                 (remf (cdr s) :ssl)
                 'erc-tls)
             'erc)
           :server s)))

(defun erc/default-servers ()
  (interactive)
  (if erc-server-list
      (erc//servers erc-server-list)
    (message "You must define erc-server-list")))
