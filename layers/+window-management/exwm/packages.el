;;; packages.el --- exwm Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(defconst exwm-packages
    '(cl-generic
      (helm-exwm :requires helm)
      (evil-exwm-state :location (recipe :fetcher github
                                         :repo "domenzain/evil-exwm-state"))
      (xelb :location (recipe :fetcher github
                              :repo "ch11ng/xelb")
            :step pre)
      (exwm :location (recipe :fetcher github
                              :repo "ch11ng/exwm")
            :step pre)))

(defun exwm/init-helm-exwm ()
  ;; when helm is used activate extra EXWM features
  (use-package helm-exwm
    :config
    (progn
      ;; Add EXWM buffers to a specific section in helm mini
      (setq exwm-helm-exwm-emacs-buffers-source
            (helm-exwm-build-emacs-buffers-source))
      (setq exwm-helm-exwm-source (helm-exwm-build-source))
      (setq helm-mini-default-sources `(exwm-helm-exwm-emacs-buffers-source
                                        exwm-helm-exwm-source
                                        helm-source-recentf
                                        helm-source-buffer-not-found))
      ;; Add a prefix command to choose among EXWM buffers only
      (spacemacs/set-leader-keys "WW" 'helm-exwm))))

(defun exwm/init-evil-exwm-state ()
  (use-package evil-exwm-state
    :init
    (spacemacs/define-evil-state-face "exwm" "firebrick1")
    (spacemacs/define-evil-state-face "exwm-insert" "chartreuse3")))

(defun exwm/init-cl-generic ()
  (use-package cl-generic
    :demand))

(defun exwm/init-xelb ()
  (use-package xelb))

(defun exwm/init-exwm ()
  (use-package exwm-randr)
  (use-package exwm-systemtray)
  (use-package exwm-config)
  (use-package exwm
    :init
    ;; Disable dialog boxes since they are unusable in EXWM
    (setq use-dialog-box nil)
    ;; Use as many workspaces as there are connected displays
    ;; TODO: Is there a way of doing this with xelb?
    (defvar exwm--randr-displays (split-string
                            (shell-command-to-string
                             "xrandr | grep ' connected' | cut -d' ' -f1 "))
      "The list of connected RandR displays")
    ;; Set at least as many workspaces as there are connected displays.
    ;; At the user's option, begin with even more workspaces
    (setq exwm-workspace-number
          (if exwm-workspace-number
              (max exwm-workspace-number (list-length exwm--randr-displays))
              (list-length exwm--randr-displays)))
    ;; The first workspaces will match the order in RandR
    (setq exwm-randr-workspace-output-plist
          (exwm//flatenum 0 exwm--randr-displays))

    ;; You may want Emacs to show you the time
    (display-time-mode t)
    (when exwm-hide-tiling-modeline
      (add-hook 'exwm-mode-hook #'hidden-mode-line-mode))

    :config
    (add-hook 'exwm-update-title-hook
              (lambda ()
                (exwm-workspace-rename-buffer exwm-title)))

    (when 'dotspacemacs-use-ido
      (exwm-config-ido)
      ;; Only rename windows intelligently when using ido.
      ;; When using helm-exwm, the source distinguishes title and class.
      (add-hook 'exwm-update-class-hook
                (lambda ()
                  (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                              (string= "gimp" exwm-instance-name))
                    (exwm-workspace-rename-buffer exwm-class-name))))
      (add-hook 'exwm-update-title-hook
                (lambda ()
                  (when (or (not exwm-instance-name)
                            (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                            (string= "gimp" exwm-instance-name))
                    (exwm-workspace-rename-buffer exwm-title)))))

    (exwm/exwm-bind-command "s-'"  exwm-terminal-command)
    (exwm/exwm-bind-command "<XF86MonBrightnessUp>"
                                 "light -A 5")
    (exwm/exwm-bind-command "<XF86MonBrightnessDown>"
                                 "light -U 5")
    (exwm/exwm-bind-command "<XF86AudioLowerVolume>"
                                 "amixer -D pulse -- sset Master unmute 3%-")
    (exwm/exwm-bind-command "<XF86AudioRaiseVolume>"
                                 "amixer -D pulse -- sset Master unmute 3%+")
    (exwm/exwm-bind-command "<XF86AudioMute>"
                                 "amixer -D pulse -- sset Master toggle")
    (exwm/exwm-bind-command "<XF86AudioMicMute>"
                                 "amixer -D pulse -- sset Capture toggle")

    ;; Pass all keypresses to emacs in line mode.
    (setq exwm-input-line-mode-passthrough t)


    ;; `exwm-input-set-key' allows you to set a global key binding (available in
    ;; any case). Following are a few examples.

    (exwm-input-set-key (kbd "s-i") 'exwm-input-toggle-keyboard)
    (exwm-input-set-key (kbd "M-m") 'spacemacs-cmds)
    (exwm-input-set-key (kbd "C-q") 'exwm-input-send-next-key)
    ;; + We always need a way to go back to line-mode from char-mode
    (exwm-input-set-key (kbd "s-r") 'exwm-reset)

    (exwm-input-set-key (kbd "s-f") #'exwm/exwm-layout-toggle-fullscreen)
    (exwm-input-set-key (kbd "<s-tab>") #'exwm/jump-to-last-exwm)
    ;; + Bind a key to switch workspace interactively
    (exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)
    (exwm-input-set-key (kbd "s-SPC") #'exwm/exwm-app-launcher)
    (exwm-input-set-key (kbd "s-l") 'exwm/exwm-lock)

    ;; set up evil escape
    (exwm-input-set-key [escape] 'evil-escape)

    ;; Undo window configurations
    (exwm-input-set-key (kbd "s-u") #'winner-undo)
    (exwm-input-set-key (kbd "S-s-U") #'winner-redo)
    ;; Workspaces
    (exwm-input-set-key (kbd "s-]") #'exwm/exwm-workspace-next)
    (exwm-input-set-key (kbd "s-[") #'exwm/exwm-workspace-prev)

    ;; Bindings available everywhere
    (spacemacs/declare-prefix "W" "EXWM")
    (spacemacs/set-leader-keys
      "Wp" 'exwm/exwm-workspace-prev
      "Wn" 'exwm/exwm-workspace-next
      "WA" 'exwm-workspace-add
      "Wd" 'exwm-workspace-delete
      "WR" 'exwm-restart
      "Wl" 'exwm/exwm-lock
      "Wa" 'exwm/exwm-app-launcher)

    ;; Bindings for use only on EXWM buffers
    (spacemacs/declare-prefix-for-mode 'exwm-mode
      "mT" "toggle")
    (spacemacs/set-leader-keys-for-major-mode 'exwm-mode
      "a" 'exwm/exwm-app-launcher
      "r" 'exwm-reset
      "Tf" 'exwm-layout-toggle-fullscreen
      "Tt" 'exwm-floating-toggle-floating
      "Tm" 'exwm-layout-toggle-mode-line)

    (exwm-randr-enable)
    (exwm-systemtray-enable)

    (if exwm-randr-command
     (start-process-shell-command
      "xrandr" nil exwm-randr-command))
    ;; The following example demonstrates how to use simulation keys to mimic the
    ;; behavior of Emacs. The argument to `exwm-input-set-simulation-keys' is a
    ;; list of cons cells (SRC . DEST), where SRC is the key sequence you press and
    ;; DEST is what EXWM actually sends to application. Note that SRC must be a key
    ;; sequence (of type vector or string), while DEST can also be a single key.

    ;; (exwm-input-set-simulation-keys
    ;;  '(([?\C-b] . left)
    ;;    ([?\C-f] . right)
    ;;    ([?\C-p] . up)
    ;;    ([?\C-n] . down)
    ;;    ([?\M-v] . prior)
    ;;    ))
    (exwm-init)
    (server-start)
    ))
