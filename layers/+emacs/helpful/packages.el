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
        "hhk" #'helpful-key
        "hhf" #'helpful-callable
        "hhv" #'helpful-variable)
      (evil-set-initial-state 'helpful-mode 'normal)
      (evil-make-overriding-map helpful-mode-map 'normal)
      (evil-define-key 'normal helpful-mode-map (kbd "h") 'evil-backward-char))))
