;;; packages.el --- helpful layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Johnson Denen <johnson@johnsons-macbook-pro.local>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst helpful-packages
  '(
    helpful
    link-hint
    popwin))

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
                  "hdv" #'helpful-variable)
                (global-set-key (kbd "C-h k") 'helpful-key)
                (global-set-key (kbd "C-h f") 'helpful-callable)
                (global-set-key (kbd "C-h v") 'helpful-variable))
              'append)
    :config
    (evil-set-initial-state 'helpful-mode 'normal)
    (spacemacs/set-leader-keys-for-major-mode 'helpful-mode
      (kbd "q") 'helpful-kill-buffers)
    (evil-define-key 'normal helpful-mode-map (kbd "gr") 'helpful-update)
    (evil-define-key 'normal helpful-mode-map (kbd "q") 'quit-window)))

(defun helpful/post-init-link-hint ()
  (with-eval-after-load 'helpful
    (evil-define-key 'normal helpful-mode-map (kbd "o") 'link-hint-open-link)))

(defun helpful/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :post-config
    (push '(helpful-mode :dedicated t :position bottom :stick t :noselect t :height 0.4) popwin:special-display-config)))
