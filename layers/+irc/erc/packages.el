;;; packages.el --- erc Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq erc-packages
      '(
        company
        company-emoji
        emoji-cheat-sheet-plus
        erc
        (erc-gitter :location (recipe
                               :fetcher github
                               :repo "jleechpe/erc-gitter")
                    :excluded t)
        erc-hl-nicks
        erc-image
        (erc-sasl :location local)
        erc-social-graph
        (erc-tex :location local)
        erc-view-log
        (erc-yank :location local :excluded t)
        erc-yt
        persp-mode
        smooth-scrolling
        ))

(when (spacemacs/system-is-mac)
  (push 'erc-terminal-notifier erc-packages))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun erc/post-init-company ()
    (spacemacs|add-company-hook erc-mode)
    (push 'company-capf company-backends-erc-mode))

  (defun erc/post-init-company-emoji ()
    (push 'company-emoji company-backends-erc-mode)))

(defun erc/post-init-emoji-cheat-sheet-plus ()
  (add-hook 'erc-mode-hook 'emoji-cheat-sheet-plus-display-mode))

(defun erc/init-erc ()
  "Initialize ERC"
  (use-package erc
    :defer t
    :init
    (spacemacs/set-leader-keys
      "aie" 'erc
      "aiE" 'erc-tls
      "aii" 'erc-track-switch-buffer)
    ;; utf-8 always and forever
    (setq erc-server-coding-system '(utf-8 . utf-8))
    ;; disable linum mode in erc
    ;; check if this will not be efficient
    (defun no-linum (&rest ignore)
      (when (or 'linum-mode global-linum-mode)
        (linum-mode 0)))
    (spacemacs/add-to-hooks 'no-linum '(erc-hook
                                        erc-mode-hook
                                        erc-insert-pre-hook))
    :config
    (progn
      (use-package erc-autoaway
        :config
        (setq erc-auto-discard-away t
              erc-autoaway-idle-seconds 600
              erc-autoaway-use-emacs-idle t))
      (erc-services-mode 1)
      (defun erc-list-command ()
        "execute the list command"
        (interactive)
        (insert "/list")
        (erc-send-current-line))
      (setq erc-kill-buffer-on-part t
            erc-kill-queries-on-quit t
            erc-kill-server-buffer-on-quit t)
      (add-hook 'erc-connect-pre-hook (lambda (x) (erc-update-modules)))
      (erc-track-mode t)
      (setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE")
            erc-server-coding-system '(utf-8 . utf-8))
      (setq erc-prompt (lambda () (concat "[" (buffer-name) "]")))

      (require 'notifications)
      (defun erc-global-notify (match-type nick message)
        "Notify when a message is recieved."
        (notifications-notify
         :title nick
         :body message
         :app-icon (concat spacemacs-assets-directory "spacemacs.svg")
         :urgency 'low))

      ;; osx doesn't have dbus support
      (when (boundp 'dbus-compiled-version)
        (add-hook 'erc-text-matched-hook 'erc-global-notify))

      ;; keybindings
      (spacemacs/set-leader-keys-for-major-mode 'erc-mode
        "d" 'erc-input-action
        "j" 'erc-join-channel
        "n" 'erc-channel-names
        "l" 'erc-list-command
        "p" 'erc-part-from-channel
        "q" 'erc-quit-server))))

(defun erc/init-erc-gitter ()
  (use-package erc-gitter
    :config
    (add-to-list 'erc-modules 'gitter)))


(defun erc/init-erc-hl-nicks ()
  (use-package erc-hl-nicks
    :defer t
    ;; may need a hook ?
    ))

(defun erc/init-erc-sasl ()
  (use-package erc-sasl
    :if erc-enable-sasl-auth
    ;; Following http://www.emacswiki.org/emacs/ErcSASL
    ;; Maybe an advice would be better?
    :config
    (progn
      ;; Add any server like this
      ;; (add-to-list 'erc-sasl-server-regexp-list "host\\.server\\.com")
      (add-to-list 'erc-sasl-server-regexp-list "irc\\.freenode\\.net")
      (defun erc-login ()
        "Perform user authentication at the IRC server."
        (erc-log (format "login: nick: %s, user: %s %s %s :%s"
                         (erc-current-nick)
                         (user-login-name)
                         (or erc-system-name (system-name))
                         erc-session-server
                         erc-session-user-full-name))
        (if erc-session-password
            (erc-server-send (format "PASS %s" erc-session-password))
          (message "Logging in without password"))
        (when (and (featurep 'erc-sasl) (erc-sasl-use-sasl-p))
          (erc-server-send "CAP REQ :sasl"))
        (erc-server-send (format "NICK %s" (erc-current-nick)))
        (erc-server-send
         (format "USER %s %s %s :%s"
                 ;; hacked - S.B.
                 (if erc-anonymous-login erc-email-userid (user-login-name))
                 "0" "*"
                 erc-session-user-full-name))
        (erc-update-mode-line)))))

(defun erc/init-erc-social-graph ()
  (use-package erc-social-graph
    :init
    (progn
      ;; does not exist ?
      ;; (erc-social-graph-enable)
      (setq erc-social-graph-dynamic-graph t)
      (spacemacs/set-leader-keys-for-major-mode 'erc-mode
        "D" 'erc-social-graph-draw))))

(defun erc/init-erc-tex ()
  (require 'erc-tex))

(defun erc/init-erc-yt ()
  (use-package erc-yt
    :init (with-eval-after-load 'erc (add-to-list 'erc-modules 'youtube))))

(defun erc/init-erc-yank ()
  (use-package erc-yank
    :if (configuration-layer/package-usedp 'gist)
    :init
    (evil-define-key 'normal erc-mode-map
      "p" 'erc-yank)))

(defun erc/init-erc-view-log ()
  (use-package erc-view-log
    :init
    (progn
      (with-eval-after-load 'erc (add-to-list 'erc-modules 'log))
      (setq erc-log-channels-directory
            (expand-file-name
             (concat spacemacs-cache-directory
                     "erc-logs")))
      (unless (file-exists-p erc-log-channels-directory)
        (make-directory erc-log-channels-directory)))

    :config
    ;; ERC Logging
    (progn
      (add-to-list 'auto-mode-alist
                   `(,(format "%s/.*\\.[log|txt]"
                              (regexp-quote
                               (expand-file-name
                                erc-log-channels-directory))) . erc-view-log-mode))
      ;; Following https://raw.githubusercontent.com/Niluge-KiWi/erc-view-log/master/erc-view-log.el
      ;; installation instructions
      (add-hook 'erc-view-log-mode-hook 'turn-on-auto-revert-tail-mode)

      (spacemacs|define-transient-state erc-log
        :title "ERC Log Transient State"
        :doc "\n[_r_] reload the log file  [_>_/_<_] go to the next/prev mention"
        :bindings
        ("r" erc-view-log-reload-file)
        (">" erc-view-log-next-mention)
        ("<" erc-view-log-previous-mention))
      (spacemacs/set-leader-keys-for-major-mode 'erc-mode
        "." 'spacemacs/erc-log-transient-state/body))))

(defun erc/init-erc-image ()
  (use-package erc-image
    :init (with-eval-after-load 'erc (add-to-list 'erc-modules 'image))))

(defun erc/init-erc-terminal-notifier ()
  (use-package erc-terminal-notifier
    :if (executable-find "terminal-notifier")))

(defun erc/post-init-persp-mode ()
  (spacemacs|define-custom-layout "@ERC"
    :binding "E"
    :body
    (progn
      (add-hook 'erc-mode #'(lambda ()
                              (persp-add-buffer (current-buffer))))
      (call-interactively 'erc)))
  ;; do not save erc buffers
  (spacemacs|use-package-add-hook persp-mode
    :post-config
    (push (lambda (b) (with-current-buffer b (eq major-mode 'erc-mode)))
          persp-filter-save-buffers-functions)))

(defun erc/post-init-smooth-scrolling ()
  (add-hook 'erc-mode-hook 'spacemacs//unset-scroll-margin))
