;;; packages.el --- autohotkey Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
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
    :mode "\\.ahk\\'"
    :defer t
    :init
    (spacemacs/set-leader-keys-for-major-mode 'ahk-mode
      "cb" 'ahk-comment-block-dwim
      "cc" 'ahk-comment-dwim
      "eb" 'ahk-run-script
      "hh" 'ahk-lookup-web
      "hH" 'ahk-lookup-chm)))
