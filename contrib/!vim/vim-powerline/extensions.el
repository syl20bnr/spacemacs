;;; extensions.el --- vim-powerline Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq vim-powerline-post-extensions '(vim-powerline))

(defun vim-powerline/init-vim-powerline ()
  (require 'powerline)
  (if (display-graphic-p)
      (setq powerline-default-separator 'arrow)
    (setq powerline-default-separator 'utf-8))
  (powerline-vimish-theme)

  (defun spacemacs//set-vimish-powerline-for-startup-buffers ()
    "Set the powerline for buffers created when Emacs starts."
    (unless configuration-layer-error-count
      (dolist (buffer '("*Messages*" "*spacemacs*" "*Compile-Log*"))
        (when (get-buffer buffer)
          (with-current-buffer buffer
            (setq-local mode-line-format (default-value 'mode-line-format))
            (powerline-set-selected-window)
            (powerline-reset))))))
  (add-hook 'emacs-startup-hook
            'spacemacs//set-vimish-powerline-for-startup-buffers))
