;;; packages.el --- slack layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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

(defun slack/post-init-persp-mode ()
  (spacemacs|define-custom-layout "@Slack"
    :binding "s"
    :body
    (progn
      (add-hook 'slack-mode #'(lambda ()
                                (persp-add-buffer (current-buffer))))
      ;; TODO: We don't want to slack-start every time someone types `<SPC> l o s`
      (call-interactively 'slack-start)
      (call-interactively 'slack-channel-select)))
  ;; Do not save slack buffers
  (spacemacs|use-package-add-hook persp-mode
    :post-config
    (push (lambda (b) (with-current-buffer b (eq major-mode 'slack-mode)))
          persp-filter-save-buffers-functions)))

(defun slack/init-slack ()
  "Initialize Slack"
  (use-package slack
    :commands (slack-start)
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys
        "aCs" 'slack-start
        "aCj" 'slack-channel-select
        "aCd" 'slack-im-select
        "aCq" 'slack-ws-close)
      (setq slack-enable-emoji t))
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'slack-mode
        "j" 'slack-channel-select
        "d" 'slack-im-select
        "p" 'slack-room-load-prev-messages
        "e" 'slack-message-edit
        "q" 'slack-ws-close
        "mm" 'slack-message-embed-mention
        "mc" 'slack-message-embed-channel
        "k" 'slack-channel-select
        "@" 'slack-message-embed-mention
        "#" 'slack-message-embed-channel)
      (evil-define-key 'insert slack-mode-map
        (kbd "@") 'slack-message-embed-mention
        (kbd "#") 'slack-message-embed-channel))))

;;; packages.el ends here
