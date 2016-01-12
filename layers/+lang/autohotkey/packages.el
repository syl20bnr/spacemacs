;;; packages.el --- autohotkey Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Author: Rich Alesi <https://github.com/ralesi>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq autohotkey-packages
  '(ahk-mode)) 

(defun autohotkey/init-ahk-mode ()
  (use-package ahk-mode
    :mode "\\.ahk$"
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'ahk-mode
      "cc" 'ahk-comment-dwim
      "cb" 'ahk-comment-block-dwim
      "h" 'ahk-lookup-web
      "eb" 'ahk-run-script)))
