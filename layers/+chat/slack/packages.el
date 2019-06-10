;;; packages.el --- slack layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Kosta Harlan <kosta@kostaharlan.net>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

;; TODO: Integrate company-emoji.

(defconst slack-packages
  '(
    alert
    emoji-cheat-sheet-plus
    flyspell
    linum
    persp-mode
    slack
    window-purpose
    ))

(defun slack/init-alert ()
  (use-package alert
    :defer t
    :init (setq alert-default-style 'notifier)))

(defun slack/post-init-emoji-cheat-sheet-plus ()
  (add-hook 'slack-mode-hook 'emoji-cheat-sheet-plus-display-mode))

(defun slack/post-init-flyspell ()
  (add-hook 'lui-mode-hook 'flyspell-mode))

(defun slack/post-init-linum ()
  (add-hook 'slack-mode-hook 'spacemacs/no-linum))

(defun slack/pre-init-persp-mode ()
  (spacemacs|use-package-add-hook persp-mode
    :post-config
    (progn
      (add-to-list 'persp-filter-save-buffers-functions
                   'spacemacs//slack-persp-filter-save-buffers-function)
      (spacemacs|define-custom-layout slack-spacemacs-layout-name
        :binding slack-spacemacs-layout-binding
        :body
        (progn
          (add-hook 'slack-mode #'spacemacs//slack-buffer-to-persp)
          ;; TODO: We don't want to slack-start every time someone types `SPC l o s`
          (call-interactively 'slack-start)
          (call-interactively 'slack-channel-select))))))

(defun slack/init-slack ()
  "Initialize Slack"
  (use-package slack
    :commands (slack-start)
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "aC" "slack")
      (spacemacs/set-leader-keys
        "aCs" 'slack-start
        "aCj" 'slack-channel-select
        "aCg" 'slack-group-select
        "aCr" 'slack-select-rooms
        "aCd" 'slack-im-select
        "aCq" 'slack-ws-close)
      (setq slack-enable-emoji t))
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'slack-mode
        "j" 'slack-channel-select
        "g" 'slack-group-select
        "r" 'slack-select-rooms
        "d" 'slack-im-select
        "p" 'slack-room-load-prev-messages
        "e" 'slack-message-edit
        "t" 'slack-thread-show-or-create
        "q" 'slack-ws-close
        "mm" 'slack-message-embed-mention
        "mc" 'slack-message-embed-channel
        "k" 'slack-select-rooms
        "@" 'slack-message-embed-mention
        "#" 'slack-message-embed-channel
        ")" 'slack-message-add-reaction
        "(" 'slack-message-remove-reaction)
      (evil-define-key 'insert slack-mode-map
        (kbd "@") 'slack-message-embed-mention
        (kbd "#") 'slack-message-embed-channel))))

(defun slack/pre-init-window-purpose ()
  (spacemacs|use-package-add-hook window-purpose
    :pre-config
    (add-to-list 'purpose-user-mode-purposes '(slack-mode . chat))))

;;; packages.el ends here
