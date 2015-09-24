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
        erc-social-graph
        erc-view-log
        erc-yt
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
    (evil-leader/set-key
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
         :app-icon "/home/io/.emacs.d/assets/spacemacs.svg"
         :urgency 'low))

      ;; osx doesn't have dbus support
      (when (boundp 'dbus-compiled-version)
        (add-hook 'erc-text-matched-hook 'erc-global-notify))

      ;; keybindings
      (evil-leader/set-key-for-mode 'erc-mode
        "md" 'erc-input-action
        "mj" 'erc-join-channel
        "mn" 'erc-channel-names
        "ml" 'erc-list-command
        "mp" 'erc-part-from-channel
        "mq" 'erc-quit-server))))

(defun erc/init-erc-gitter ()
  (use-package erc-gitter
    :config
    (add-to-list 'erc-modules 'gitter)))


(defun erc/init-erc-hl-nicks ()
  (use-package erc-hl-nicks
    :defer t
    ;; may need a hook ?
    ))

(defun erc/init-erc-social-graph ()
  (use-package erc-social-graph
    :init
    (progn
      ;; does not exist ?
      ;; (erc-social-graph-enable)
      (setq erc-social-graph-dynamic-graph t)
      (evil-leader/set-key-for-mode 'erc-mode
        "mD" 'erc-social-graph-draw))))

(defun erc/init-erc-yt ()
  (use-package erc-yt
    :init (eval-after-load 'erc '(add-to-list 'erc-modules 'youtube))))

(defun erc/init-erc-view-log ()
  (use-package erc-view-log
    :init
    (progn
      (eval-after-load 'erc '(add-to-list 'erc-modules 'log))
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


      (defun spacemacs//erc-log-ms-documentation ()
        "Return the docstring for the workspaces micro-state."
        (concat
         "\n[r]  to reload the log file"
         "[>], [<] to go to the next/prev mention"))

      (spacemacs|define-micro-state erc-log
        :doc (spacemacs//erc-log-ms-documentation)
        :use-minibuffer t
        :evil-leader "m."
        :bindings
        ("r" erc-view-log-reload-file)
        (">" erc-view-log-next-mention)
        ("<" erc-view-log-previous-mention)))))

(defun erc/init-erc-image ()
  (use-package erc-image
    :init (eval-after-load 'erc '(add-to-list 'erc-modules 'image))))

(defun erc/init-erc-terminal-notifier ())
