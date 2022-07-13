;;; packages.el --- EXWM Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
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
  '((xdg :location built-in)
    desktop-environment
    (helm-exwm :toggle (configuration-layer/package-used-p 'helm))
    (evil-exwm-state :toggle (configuration-layer/package-used-p 'evil)
                     :location (recipe :fetcher github
                                       :repo "domenzain/evil-exwm-state"))
    (xelb :location (recipe :fetcher github
                            :repo "ch11ng/xelb")
          :step pre)
    (exwm :location (recipe :fetcher github
                            :repo "ch11ng/exwm")
          :step pre)))

(defun exwm/init-xdg ()
  (use-package xdg
    :defer t
    :commands (xdg-config-dirs xdg-config-home xdg-desktop-read-file)))

(defun exwm/init-desktop-environment ()
  (use-package desktop-environment
    :after exwm
    :spacediminish t
    :defer t
    :init (spacemacs|add-toggle desktop-environment
            :mode desktop-environment-mode
            :documentation "Keybindings for Desktop Environment functionality."
            :evil-leader "TD")
    :config
    (progn
      (setq desktop-environment-update-exwm-global-keys :prefix)
      (define-key desktop-environment-mode-map (kbd "s-l") nil)
      ;; If we don't enable this, exwm/switch-to-buffer-or-run won't move an X window to the current frame
      (setq exwm-layout-show-all-buffers t))))

(defun exwm/init-helm-exwm ()
  ;; when helm is used activate extra EXWM features
  (use-package helm-exwm
    :config
    (progn
      ;; Add EXWM buffers to a specific section in helm mini
      (setq exwm-helm-exwm-emacs-buffers-source (helm-exwm-build-emacs-buffers-source))
      (setq exwm-helm-exwm-source (helm-exwm-build-source))
      (setq helm-mini-default-sources `(exwm-helm-exwm-emacs-buffers-source
                                        exwm-helm-exwm-source
                                        helm-source-recentf
                                        helm-source-buffer-not-found))
      ;; Add a prefix command to choose among EXWM buffers only
      (spacemacs/set-leader-keys "WW" 'helm-exwm))))

(defun exwm/init-evil-exwm-state ()
  (use-package evil-exwm-state
    :config
    (progn
      (spacemacs/define-evil-state-face "exwm" "firebrick1")
      (spacemacs/define-evil-state-face "exwm-insert" "chartreuse3"))))

(defun exwm/init-xelb ()
  (use-package xelb))

(defun exwm/init-exwm ()
  (use-package exwm-systemtray)
  (use-package exwm
    :custom
    (use-dialog-box nil "Disable dialog boxes since they are unusable in EXWM")
    (exwm-input-line-mode-passthrough t "Pass all keypresses to emacs in line mode.")
    :init
    (progn
      (require 'exwm-config)
      ;; "Number of workspaces. Defaults to the number of connected displays."
      (unless exwm-workspace-number
        (custom-set-variables '(exwm-workspace-number (/ (length exwm--randr-displays) 2))))

      (exwm//randr-setup)
      (exwm//systray-setup))
    :config
    (progn
      ;; You may want Emacs to show you the time
      (display-time-mode t)

      (when exwm-hide-tiling-modeline
        (exwm-layout-hide-mode-line))

      (add-hook 'exwm-update-title-hook
                (lambda ()
                  (exwm-workspace-rename-buffer exwm-title)))

      ;; Remove ALL bindings
      (define-key exwm-mode-map "\C-c\C-f" nil)
      (define-key exwm-mode-map "\C-c\C-h" nil)
      (define-key exwm-mode-map "\C-c\C-k" nil)
      (define-key exwm-mode-map "\C-c\C-m" nil)
      (define-key exwm-mode-map "\C-c\C-q" nil)
      (define-key exwm-mode-map "\C-c\C-t\C-f" nil)
      (define-key exwm-mode-map "\C-c\C-t\C-m" nil)


      ;; `exwm-input-set-key' allows you to set a global key binding (available in
      ;; any case). Following are a few examples.

      (exwm-input-set-key (kbd "M-m")     'spacemacs-cmds)
      (exwm-input-set-key (kbd "C-q")     #'exwm-input-send-next-key)
      (exwm-input-set-key (kbd "s-i")     #'exwm-input-toggle-keyboard)
      (exwm-input-set-key (kbd "s-l")     #'exwm/exwm-lock)
      (exwm-input-set-key (kbd "s-r")     #'exwm-reset)

      (exwm-input-set-key (kbd "s-w")     #'exwm-workspace-switch)
      (exwm-input-set-key (kbd "s-TAB")   #'exwm/jump-to-last-exwm)

      (exwm-input-set-key (kbd "s-SPC")   #'exwm/exwm-app-launcher)
      (exwm-input-set-key (kbd "s-RET")   #'exwm-terminal-command)

      ;; set up evil escape
      (when (configuration-layer/package-used-p 'evil-escape)
        (exwm-input-set-key [escape] 'evil-escape))

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

      ;; autostart
      (when exwm-autostart-xdg-applications
        (add-hook 'exwm-init-hook 'exwm//autostart-xdg-applications t))

      (exwm-init)
      (server-start))))
