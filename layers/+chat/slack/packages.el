;;; packages.el --- slack layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Kosta Harlan <kosta@kostaharlan.net>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;; TODO: Integrate company-emoji.

(defconst slack-packages
  '(
    alert
    emoji-cheat-sheet-plus
    flyspell
    persp-mode
    slack
    window-purpose))

(defun slack/init-alert ()
  (use-package alert
    :defer t
    :init (setq alert-default-style 'notifier)))

(defun slack/post-init-emoji-cheat-sheet-plus ()
  (add-hook 'slack-mode-hook 'emoji-cheat-sheet-plus-display-mode))

(defun slack/post-init-flyspell ()
  (add-hook 'lui-mode-hook 'flyspell-mode))

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
    (spacemacs/declare-prefix "acs" "slack")
    (spacemacs/set-leader-keys
      "acsT" 'slack-all-threads
      "acsd" 'slack-im-select
      "acsg" 'slack-group-select
      "acsj" 'slack-channel-select
      "acsq" 'slack-ws-close
      "acsr" 'slack-select-rooms
      "acss" 'slack-start
      "acsu" 'slack-all-unreads)
    (setq slack-enable-emoji t)
    :config
    (dolist (mode '(slack-mode slack-message-buffer-mode slack-thread-message-buffer-mode))
      (spacemacs/set-leader-keys-for-major-mode mode
        "#" 'slack-message-embed-channel
        "(" 'slack-message-remove-reaction
        ")" 'slack-message-add-reaction
        "@" 'slack-message-embed-mention
        "T" 'slack-all-threads
        "d" 'slack-im-select
        "e" 'slack-message-edit
        "g" 'slack-group-select
        "j" 'slack-channel-select
        "k" 'slack-select-rooms
        "mc" 'slack-message-embed-channel
        "mm" 'slack-message-embed-mention
        "p" 'slack-room-load-prev-messages
        "q" 'slack-ws-close
        "r" 'slack-select-rooms
        "t" 'slack-thread-show-or-create
        "u" 'slack-all-unreads)
      (let ((keymap (symbol-value (intern (concat (symbol-name mode) "-map")))))
        (evil-define-key 'insert keymap
          (kbd "#") 'slack-message-embed-channel
          (kbd ":") 'slack-insert-emoji
          (kbd "@") 'slack-message-embed-mention)))))

(defun slack/post-init-window-purpose ()
  (purpose-set-extension-configuration
   :slack-layer
   (purpose-conf :mode-purposes '((slack-mode . chat)))))
