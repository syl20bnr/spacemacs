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
      helm-exwm
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
  (spacemacs|use-package-add-hook helm
    :post-config
    (use-package helm-exwm
      :config
      (progn
        ;; Add EXWM buffers to a specific section in helm mini
        (setq exwm/helm-exwm-emacs-buffers-source
              (helm-exwm-build-emacs-buffers-source))
        (setq exwm/helm-exwm-source (helm-exwm-build-source))
        (setq helm-mini-default-sources `(exwm/helm-exwm-emacs-buffers-source
                                          exwm/helm-exwm-source
                                          helm-source-recentf
                                          helm-source-buffer-not-found))
        ;; Add a prefix command to choose among EXWM buffers only
        (spacemacs/set-leader-keys "WW" 'helm-exwm)))))

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
    (setq exwm-workspace-number
          (list-length
           (split-string
            (shell-command-to-string
             "xrandr | grep ' connected' | cut -d' ' -f1 "))))
    ;; The workspaces will match the order in randr
    (setq exwm-randr-workspace-output-plist '())
    ;; You may want Emacs to show you the time
    (display-time-mode t)
    (when exwm/hide-tiling-modeline
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

    (exwm/exwm-bind-command "s-'"  exwm/terminal-command)
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
    ;; + We always need a way to go back to line-mode from char-mode
    (exwm-input-set-key (kbd "s-r") 'exwm-reset)

    (exwm-input-set-key (kbd "s-f") #'exwm/exwm-layout-toggle-fullscreen)
    (exwm-input-set-key (kbd "<s-tab>") #'exwm/jump-to-last-exwm)
    ;; + Bind a key to switch workspace interactively
    (exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)
    ;; + Set shortcuts to switch to a certain workspace.
    (exwm-input-set-key (kbd "s-1")
                        (lambda () (interactive) (exwm-workspace-switch 0)))
    (exwm-input-set-key (kbd "s-2")
                        (lambda () (interactive) (exwm-workspace-switch 1)))
    (exwm-input-set-key (kbd "s-3")
                        (lambda () (interactive) (exwm-workspace-switch 2)))
    (exwm-input-set-key (kbd "s-4")
                        (lambda () (interactive) (exwm-workspace-switch 3)))
    (exwm-input-set-key (kbd "s-5")
                        (lambda () (interactive) (exwm-workspace-switch 4)))
    (exwm-input-set-key (kbd "s-6")
                        (lambda () (interactive) (exwm-workspace-switch 5)))
    (exwm-input-set-key (kbd "s-7")
                        (lambda () (interactive) (exwm-workspace-switch 6)))
    (exwm-input-set-key (kbd "s-8")
                        (lambda () (interactive) (exwm-workspace-switch 7)))
    (exwm-input-set-key (kbd "s-9")
                        (lambda () (interactive) (exwm-workspace-switch 8)))
    (exwm-input-set-key (kbd "s-0")
                        (lambda () (interactive) (exwm-workspace-switch 9)))
    ;; The following example demonstrates how to set a key binding only available
    ;; in line mode. It's simply done by first push the prefix key to
    ;; `exwm-input-prefix-keys' and then add the key sequence to `exwm-mode-map'.
    ;; The example shorten 'C-c q' to 'C-q'.
    (push ?\C-q exwm-input-prefix-keys)
    (define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)
    (exwm-input-set-key (kbd "s-SPC") #'exwm/exwm-app-launcher)
    (exwm-input-set-key (kbd "s-l") 'exwm/exwm-lock)

    ;; M-m leader, sorry Space Folks
    (push ?\M-m exwm-input-prefix-keys)
    ;; Universal Get-me-outta-here
    (push ?\C-g exwm-input-prefix-keys)
    ;; Universal Arguments
    (push ?\C-u exwm-input-prefix-keys)
    (push ?\C-0 exwm-input-prefix-keys)
    (push ?\C-1 exwm-input-prefix-keys)
    (push ?\C-2 exwm-input-prefix-keys)
    (push ?\C-3 exwm-input-prefix-keys)
    (push ?\C-4 exwm-input-prefix-keys)
    (push ?\C-5 exwm-input-prefix-keys)
    (push ?\C-6 exwm-input-prefix-keys)
    (push ?\C-7 exwm-input-prefix-keys)
    (push ?\C-8 exwm-input-prefix-keys)
    (push ?\C-9 exwm-input-prefix-keys)
    ;; C-c, C-x are needed for copying and pasting
    (delete ?\C-x exwm-input-prefix-keys)
    (delete ?\C-c exwm-input-prefix-keys)
    ;; We can use `M-m h' to access help
    (delete ?\C-h exwm-input-prefix-keys)
    ;; set up evil escape
    (exwm-input-set-key [escape] 'evil-escape)

    ;; Preserve the habit
    (exwm-input-set-key (kbd "s-:") 'helm-M-x)
    (exwm-input-set-key (kbd "s-;") 'evil-ex)
    ;; Shell (not a real one for the moment)
    (exwm-input-set-key (kbd "C-'") #'spacemacs/default-pop-shell)
    ;; Undo window configurations
    (exwm-input-set-key (kbd "s-u") #'winner-undo)
    (exwm-input-set-key (kbd "S-s-U") #'winner-redo)
    ;; Focusing windows
    (exwm-input-set-key (kbd "s-h") #'evil-window-left)
    (exwm-input-set-key (kbd "s-j") #'evil-window-down)
    (exwm-input-set-key (kbd "s-k") #'evil-window-up)
    (exwm-input-set-key (kbd "s-l") #'evil-window-right)
    ;; Moving Windows
    (exwm-input-set-key (kbd "s-H") #'evil-window-move-far-left)
    (exwm-input-set-key (kbd "s-J") #'evil-window-move-very-bottom)
    (exwm-input-set-key (kbd "s-K") #'evil-window-move-very-top)
    (exwm-input-set-key (kbd "s-L") #'evil-window-move-far-right)
    ;; Resize
    (exwm-input-set-key (kbd "M-s-h") #'spacemacs/shrink-window-horizontally)
    (exwm-input-set-key (kbd "M-s-j") #'spacemacs/shrink-window)
    (exwm-input-set-key (kbd "M-s-k") #'spacemacs/enlarge-window)
    (exwm-input-set-key (kbd "M-s-l") #'spacemacs/enlarge-window-horizontally)
    ;; Workspaces
    (exwm-input-set-key (kbd "s-]") #'exwm/exwm-workspace-next)
    (exwm-input-set-key (kbd "s-[") #'exwm/exwm-workspace-prev)

    (spacemacs/declare-prefix "W" "EXWM")
    (exwm-randr-enable)
    (exwm-systemtray-enable)
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

    ;; Do not forget to enable EXWM. It will start by itself when things are ready.
    ;; (exwm-enable)
    (server-start)
    ))
