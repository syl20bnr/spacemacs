;;; packages.el --- Circe layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Chris Barrett <chris.d.barrett@me.com>
;; Author: Aidan Nyquist <contact@aidannyquist.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst circe-packages
  '(circe
    circe-notifications
    persp-mode
    helm
    helm-circe
    ))

(defun circe/init-circe ()
  (use-package circe
    :commands circe
    :config

    (set-face-background 'circe-prompt-face nil)
    (setq circe-reduce-lurker-spam t)
    (setq circe-use-cycle-completion t)
    (setq circe-active-users-timeout (* 60 30)) ; 30 minutes
    (setq circe-format-say "{nick}> {body}")
    (setq circe-format-self-say (concat (propertize ">>>" 'face 'circe-self-say-face) " {body}"))
    (setq circe-prompt-string (concat (propertize ">>>" 'face 'circe-prompt-face) " "))
    (setq circe-highlight-nick-type 'all)

    ;; Add circe-color-nicks
    (enable-circe-color-nicks)
    (setq circe-color-nicks-everywhere 't)

    ;; Show channel name in prompt.
    (add-hook 'circe-chat-mode-hook
              (lambda ()
                (-let* (
                        ;; Get buffername
                        (s (buffer-name))
                        ((_ gitter-fmt) (s-match (rx (+ nonl) "/" (group (+ nonl)) eos) s))
                        ((_ standard-fmt) (s-match (rx "#" (group (+ nonl)) eos) s))
                        ;; Buffername to channel name
                        (chan (or gitter-fmt standard-fmt))
                        ;; Format prompt
                        (prompt (concat "#" (propertize chan 'face 'circe-prompt-face) " > ")))
                  (lui-set-prompt prompt))))

    ;; Timestamps in margins.
    (setq lui-time-stamp-position 'right-margin)
    (setq lui-time-stamp-format "%H:%M")
    (setq lui-fill-type nil)
    (add-hook 'lui-mode-hook (lambda ()
                               (setq fringes-outside-margins t)
                               (setq right-margin-width 5)
                               (setq word-wrap t)
                               (setq wrap-prefix "    ")))

    (add-hook 'circe-channel-mode-hook #'enable-lui-autopaste)

    ;; Keybindings
    (spacemacs/set-leader-keys "aic" 'circe)

    (dolist (mode circe-modes)
      (spacemacs/declare-prefix-for-mode mode "mc" "command")
      (spacemacs/set-leader-keys-for-major-mode mode
        "r" 'circe-reconnect
        "R" 'circe-reconnect-all
        "v" 'circe-version
        "c n" 'circe-command-NAMES
        "c q" 'circe-command-QUERY
        "c h" 'circe-command-HELP
        "c t" 'circe-command-CHTOPIC
        "c s" 'circe-command-SAY
        ))))

(defun circe/post-init-helm ()
  (with-eval-after-load 'helm
    (if (not circe-helm-for-username-completion)
        (dolist (mode circe-modes)
          (add-to-list 'helm-mode-no-completion-in-region-in-modes mode)))))

(defun circe/init-helm-circe ()
  (use-package helm-circe
    :commands (helm-circe
               helm-circe-servers
               helm-circe-queries
               helm-circe-by-server
               helm-circe-new-activity)
    :after circe
    :config
    ;; Helm keybindings
    (dolist (mode circe-modes)
      (spacemacs/declare-prefix-for-mode mode "mh" "helm")
      (spacemacs/set-leader-keys-for-major-mode mode
        "h h" 'helm-circe
        "h s" 'helm-circe-servers
        "h q" 'helm-circe-queries
        "h b" 'helm-circe-by-server
        "h n" 'helm-circe-new-activity
        ))))

(defun circe/init-circe-notifications ()
  (use-package circe-notifications
    :commands enable-circe-notifications
    :after circe
    :init
    (add-hook 'circe-server-connected-hook #'enable-circe-notifications)
    :config
    ;;; HACK: Fix broken implementation of internal function.
    (defun circe-notifications-nicks-on-all-networks ()
      "Get a list of all nicks in use according to `circe-network-options'."
      (delete-dups (mapcar (lambda (opt)
                             (plist-get (cdr opt) :nick))
                           circe-network-options)))

    (when (eq system-type 'darwin)
      (setq circe-notifications-alert-style 'osx-notifier))))

(defun circe/post-init-persp-mode ()
  ;; do not save circe buffers
  (with-eval-after-load 'persp-mode
    (push (lambda (b) (with-current-buffer b (eq major-mode 'circe-mode)))
          persp-filter-save-buffers-functions))

  (spacemacs|define-custom-layout circe-spacemacs-layout-name
    :binding circe-spacemacs-layout-binding
    :body
    (defun spacemacs-layouts/add-circe-buffer-to-persp ()
      (persp-add-buffer (current-buffer)
                        (persp-get-by-name
                         circe-spacemacs-layout-name)))
    (add-hook 'circe-mode-hook #'spacemacs-layouts/add-circe-buffer-to-persp)
    (call-interactively 'circe)))

;;; packages.el ends here
