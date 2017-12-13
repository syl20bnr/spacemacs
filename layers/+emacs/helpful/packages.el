;;; packages.el --- helpful layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Johnson Denen <johnson@johnsons-macbook-pro.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(defconst helpful-packages
  '(helpful))

(defun helpful/init-helpful ()
  (use-package helpful
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "hh" "helpful")
      (spacemacs/set-leader-keys
        "hhh" #'helpful-at-point
        "hhc" #'helpful-command
        "hhf" #'helpful-callable
        "hhk" #'helpful-key
        "hhm" #'helpful-macro
        "hhv" #'helpful-variable))))

;;; packages.el ends here
