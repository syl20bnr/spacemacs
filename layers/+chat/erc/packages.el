;;; packages.el --- erc Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
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
        linum
        persp-mode
        ))

(when (spacemacs/system-is-mac)
  (push 'erc-terminal-notifier erc-packages))

(defun erc/post-init-company ()
  (spacemacs|add-company-backends :backends company-capf :modes erc-mode))

(defun erc/post-init-company-emoji ()
  (spacemacs|add-company-backends :backends company-emoji :modes erc-mode))

(defun erc/post-init-emoji-cheat-sheet-plus ()
  (add-hook 'erc-mode-hook 'emoji-cheat-sheet-plus-display-mode))

(defun erc/init-erc ()
  "Initialize ERC"
  (use-package erc
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys
        "aie" 'erc
        "aiE" 'erc-tls
        "aii" 'erc-track-switch-buffer
        "aiD" 'erc/default-servers)
      ;; utf-8 always and forever
      (setq erc-server-coding-system '(utf-8 . utf-8)))
    :config
    (progn
      (use-package erc-autoaway
        :defer t
        :init
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
  (spacemacs|use-package-add-hook erc
    :post-config
    (use-package erc-hl-nicks)))

(defun erc/init-erc-sasl ()
  (spacemacs|use-package-add-hook erc
    :post-config
    (use-package erc-sasl
      :defer t
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
          (erc-update-mode-line))))))

(defun erc/init-erc-social-graph ()
  (spacemacs|use-package-add-hook erc
    :post-config
    (use-package erc-social-graph
      :init
      (progn
        ;; does not exist ?
        ;; (erc-social-graph-enable)
        (setq erc-social-graph-dynamic-graph t)
        (spacemacs/set-leader-keys-for-major-mode 'erc-mode
          "D" 'erc-social-graph-draw)))))

(defun erc/init-erc-tex ()
  (spacemacs|use-package-add-hook erc
    :post-config
    (require 'erc-tex)))

(defun erc/init-erc-yt ()
  (spacemacs|use-package-add-hook erc
    :post-config
    (use-package erc-yt
      :init (with-eval-after-load 'erc
              (add-to-list 'erc-modules 'youtube)))))

(defun erc/init-erc-yank ()
  (spacemacs|use-package-add-hook erc
    :post-config
    (use-package erc-yank
      :if (configuration-layer/package-usedp 'gist)
      :init (evil-define-key 'normal erc-mode-map "p" 'erc-yank))))

(defun erc/init-erc-view-log ()
  (use-package erc-view-log
    :defer t
    :init
    (progn
      (setq erc-log-channels-directory
            (expand-file-name
             (concat spacemacs-cache-directory
                     "erc-logs")))
      (unless (file-exists-p erc-log-channels-directory)
        (make-directory erc-log-channels-directory))
      (add-to-list 'auto-mode-alist
                   `(,(format "%s/.*\\.[log|txt]"
                              (regexp-quote
                               (expand-file-name
                                erc-log-channels-directory))) . erc-view-log-mode))
      (with-eval-after-load 'erc (add-to-list 'erc-modules 'log)))
    :config
    ;; ERC Logging
    (progn
      ;; Following https://raw.githubusercontent.com/Niluge-KiWi/erc-view-log/master/erc-view-log.el
      ;; installation instructions
      (add-hook 'erc-view-log-mode-hook 'turn-on-auto-revert-tail-mode)

      (spacemacs|define-transient-state erc-log
        :title "ERC Log Transient State"
        :doc "\n[_r_] reload the log file  [_>_/_<_] go to the next/prev mention"
        :evil-leader-for-mode (erc-mode . ".")
        :bindings
        ("r" erc-view-log-reload-file)
        (">" erc-view-log-next-mention)
        ("<" erc-view-log-previous-mention)))))

(defun erc/init-erc-image ()
  (use-package erc-image
    :defer t
    :init (with-eval-after-load 'erc
            (require 'erc-image)
            (add-to-list 'erc-modules 'image))))

(defun erc/init-erc-terminal-notifier ()
  (use-package erc-terminal-notifier
    :if (executable-find "terminal-notifier")))

(defun erc/post-init-linum ()
  (spacemacs/add-to-hooks 'spacemacs/no-linum '(erc-mode-hook
                                                erc-insert-pre-hook)))

(defun erc/post-init-persp-mode ()
  ;; do not save erc buffers
  (with-eval-after-load 'persp-mode
    (push (lambda (b) (with-current-buffer b (eq major-mode 'erc-mode)))
          persp-filter-save-buffers-functions))

  (spacemacs|define-custom-layout erc-spacemacs-layout-name
    :binding erc-spacemacs-layout-binding
    :body
    (progn
      (defun spacemacs-layouts/add-erc-buffer-to-persp ()
        (persp-add-buffer (current-buffer)
                          (persp-get-by-name
                           erc-spacemacs-layout-name)))
      (add-hook 'erc-mode-hook #'spacemacs-layouts/add-erc-buffer-to-persp)
      (if erc-server-list
          (erc/default-servers)
        (call-interactively 'erc)))))
