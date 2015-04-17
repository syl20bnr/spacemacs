;;; packages.el --- erc Layer packages File for Spacemacs
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

(setq erc-packages '(erc))

(when (system-is-mac)
  (push 'erc-terminal-notifier erc-packages))

(defun erc/init-erc ()
  "Initialize ERC"
  (use-package erc
    :defer t
    :init
    (evil-leader/set-key
      "aie" 'erc
      "aiE" 'erc-tls
      "ais" 'erc-track-switch-buffer)
    :config
    (progn

      (defun erc-list-command ()
        "execute the list command"
        (interactive)
        (insert "/list")
        (erc-send-current-line))

      (setq erc-kill-buffer-on-part t
            erc-kill-queries-on-quit t
            erc-kill-server-buffer-on-quit t)

      (erc-track-mode t)
      (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE")
            erc-server-coding-system '(utf-8 . utf-8))
      ;; keybindings
      (evil-leader/set-key-for-mode 'erc-mode
        "mb" 'erc-iswitchb
        "md" 'erc-input-action
        "mj" 'erc-join-channel
        "mn" 'erc-channel-names
        "ml" 'erc-list-command
        "mp" 'erc-part-from-channel
        "mq" 'erc-quit-server))))

(defun erc/init-erc-terminal-notifier ())
