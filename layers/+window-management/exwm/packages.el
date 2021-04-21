;;; packages.el --- EXWM Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
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


(defconst exwm-packages
    '((helm-exwm :requires helm)
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
              (max exwm-workspace-number (length exwm--randr-displays))
              (length exwm--randr-displays)))
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

    ;; Remove ALL bindings
    (define-key exwm-mode-map "\C-c\C-f" nil)
    (define-key exwm-mode-map "\C-c\C-h" nil)
    (define-key exwm-mode-map "\C-c\C-k" nil)
    (define-key exwm-mode-map "\C-c\C-m" nil)
    (define-key exwm-mode-map "\C-c\C-q" nil)
    (define-key exwm-mode-map "\C-c\C-t\C-f" nil)
    (define-key exwm-mode-map "\C-c\C-t\C-m" nil)
    ;; Let easy-menu figure out the keys
    (easy-menu-add-item exwm-mode-menu '()
                        ["Toggle mode-line" exwm-layout-toggle-mode-line])
    (easy-menu-add-item exwm-mode-menu '()
                        ["Move X window to" exwm-workspace-move-window])

    (exwm/exwm-bind-command
     "s-'"  exwm-terminal-command
     "<s-return>"  exwm-terminal-command
     "<XF86MonBrightnessUp>"   "light -A 5"
     "<XF86MonBrightnessDown>" "light -U 5"
     "<XF86AudioLowerVolume>" "amixer -D pulse -- sset Master unmute 3%-"
     "<XF86AudioRaiseVolume>" "amixer -D pulse -- sset Master unmute 3%+"
     "<XF86AudioMute>"        "amixer -D pulse -- sset Master toggle"
     "<XF86AudioMicMute>"     "amixer -D pulse -- sset Capture toggle")

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

    ;; Bindings available everywhere
    (spacemacs/declare-prefix "W" "EXWM")
    (spacemacs/set-leader-keys
      "Wp" 'exwm/exwm-workspace-prev
      "Wn" 'exwm/exwm-workspace-next
      "WA" 'exwm-workspace-add
      "Wd" 'exwm-workspace-delete
      "Wr" 'exwm-restart
      "Wl" 'exwm/exwm-lock
      "Wa" 'exwm/exwm-app-launcher)

    ;; Bindings for use only on EXWM buffers
    (spacemacs/declare-prefix-for-mode 'exwm-mode
      "mT" "toggle")
    (spacemacs/set-leader-keys-for-major-mode 'exwm-mode
      "r" 'exwm-reset
      "Tf" 'exwm-layout-toggle-fullscreen
      "Tt" 'exwm-floating-toggle-floating
      "Tm" 'exwm-layout-toggle-mode-line)

    (exwm-randr-enable)
    (exwm-systemtray-enable)

    (if exwm-randr-command
     (start-process-shell-command
      "xrandr" nil exwm-randr-command))

    (exwm-init)
    (server-start)
    ))
