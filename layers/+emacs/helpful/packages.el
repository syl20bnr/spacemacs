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
    (spacemacs/declare-prefix-for-mode 'helpful-mode "mg" "goto")
    (add-hook 'emacs-startup-hook
              (lambda ()
                (spacemacs/set-leader-keys
                  "hdk" #'helpful-key
                  "hdf" #'helpful-callable
                  "hdv" #'helpful-variable))
              'append)
    :config
    (evil-set-initial-state 'helpful-mode 'normal)
    (evil-define-key 'normal helpful-mode-map (kbd "q") 'quit-window)))
