;;; packages.el --- autohotkey Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Author: Rich Alesi <https://github.com/ralesi>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar autohotkey-packages
  '(ahk-mode)
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.") 

(defun autohotkey/init-ahk-mode ()
  (use-package ahk-mode
    :mode ".ahk"
    :defer t
    :init
    (evil-leader/set-key-for-mode 'ahk-mode
      "mcc" 'ahk-comment-dwim
      "mcb" 'ahk-comment-block-dwim
      "mh" 'ahk-lookup-web
      "meb" 'ahk-run-script)))
