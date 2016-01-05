;;; core-spacemacs.el --- Spacemacs Core File
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
(setq message-log-max 16384)
(defconst emacs-start-time (current-time))

(require 'subr-x nil 'noerror)
(require 'core-dotspacemacs)
(require 'core-emacs-backports)
(require 'core-release-management)
(require 'core-auto-completion)
(require 'core-display-init)
(require 'core-themes-support)
(require 'core-fonts-support)
(require 'core-spacemacs-buffer)
(require 'core-keybindings)
(require 'core-toggle)
(require 'core-funcs)
(require 'core-micro-state)
(require 'core-use-package-ext)

(defgroup spacemacs nil
  "Spacemacs customizations."
  :group 'starter-kit
  :prefix 'spacemacs-)

;; loading progress bar variables
(defvar spacemacs-loading-char ?█)
(defvar spacemacs-loading-string "")
(defvar spacemacs-loading-counter 0)
;; (defvar spacemacs-loading-text "Loading")
;; (defvar spacemacs-loading-done-text "Ready!")
(defvar spacemacs-loading-dots-chunk-count 3)
(defvar spacemacs-loading-dots-count (window-total-size nil 'width))
(defvar spacemacs-loading-dots-chunk-size
  (/ spacemacs-loading-dots-count spacemacs-loading-dots-chunk-count))
(defvar spacemacs-loading-dots-chunk-threshold 0)

(defvar spacemacs--default-mode-line mode-line-format
  "Backup of default mode line format.")

(defun spacemacs/init ()
  "Perform startup initialization."
  ;; silence ad-handle-definition about advised functions getting redefined
  (setq ad-redefinition-action 'accept)
  ;; this is for a smoother UX at startup (i.e. less graphical glitches)
  (hidden-mode-line-mode)
  (spacemacs//removes-gui-elements)
  ;; explicitly set the prefered coding systems to avoid annoying prompt
  ;; from emacs (especially on Microsoft Windows)
  (prefer-coding-system 'utf-8)
  ;; TODO move evil-want-C-u-scroll when evil is removed from the bootstrapped
  ;; packages.
  (setq-default evil-want-C-u-scroll t)
  (dotspacemacs/load-file)
  (require 'core-configuration-layer)
  (dotspacemacs|call-func dotspacemacs/init "Calling dotfile init...")
  (dotspacemacs|call-func dotspacemacs/user-init "Calling dotfile user init...")
  (configuration-layer/initialize)
  ;; default theme
  (let ((default-theme (car dotspacemacs-themes)))
    (spacemacs/load-theme default-theme)
    ;; protect used themes from deletion as orphans
    (setq configuration-layer--protected-packages
          (append
           (delq nil (mapcar 'spacemacs//get-theme-package
                             dotspacemacs-themes))
           configuration-layer--protected-packages))
    (setq-default spacemacs--cur-theme default-theme)
    (setq-default spacemacs--cycle-themes (cdr dotspacemacs-themes)))
  ;; font
  (spacemacs|do-after-display-system-init
   (if (find-font (font-spec :name (car dotspacemacs-default-font)))
       (spacemacs/set-default-font dotspacemacs-default-font)
     (spacemacs-buffer/warning "Cannot find font \"%s\"!"
                               (car dotspacemacs-default-font))))
  ;; spacemacs init
  (spacemacs-buffer/goto-buffer)
  ;; explicitly recreate the home buffer for the first GUI client
  (spacemacs|do-after-display-system-init
   (kill-buffer (get-buffer spacemacs-buffer-name))
   (spacemacs-buffer/goto-buffer))
  (setq initial-buffer-choice (lambda () (get-buffer spacemacs-buffer-name)))
  ;; mandatory dependencies
  ;; dash is required to prevent a package.el bug with f on 24.3.1
  ;; (spacemacs/load-or-install-protected-package 'dash t)
  ;; (spacemacs/load-or-install-protected-package 's t)

  ;; evil is required by bind-map
  (spacemacs/load-or-install-protected-package 'evil t)
  (spacemacs/load-or-install-protected-package 'bind-map t)
  ;; bind-key is required by use-package
  (spacemacs/load-or-install-protected-package 'bind-key t)
  (spacemacs/load-or-install-protected-package 'which-key t)
  (spacemacs/load-or-install-protected-package 'use-package t)
  (setq use-package-verbose init-file-debug)
  ;; package-build is required by quelpa
  (spacemacs/load-or-install-protected-package 'package-build t)
  (setq quelpa-verbose init-file-debug
        quelpa-dir (concat spacemacs-cache-directory "quelpa/")
        quelpa-build-dir (expand-file-name "build" quelpa-dir)
        quelpa-persistent-cache-file (expand-file-name "cache" quelpa-dir)
        quelpa-update-melpa-p nil)
  (spacemacs/load-or-install-protected-package 'quelpa t)
  ;; inject use-package hooks for easy customization of stock package
  ;; configuration
  (setq use-package-inject-hooks t)
  (require 'core-keybindings)
  ;; for convenience and user support
  (unless (fboundp 'tool-bar-mode)
    (spacemacs-buffer/message (concat "No graphical support detected, "
                                      "you won't be able to launch a "
                                      "graphical instance of Emacs"
                                      "with this build.")))
  ;; check for new version
  (if dotspacemacs-mode-line-unicode-symbols
      (setq-default spacemacs-version-check-lighter "[⇪]"))
  (spacemacs/set-new-version-lighter-mode-line-faces))

(defun spacemacs//removes-gui-elements ()
  "Remove the menu bar, tool bar and scroll bars."
  ;; removes the GUI elements
  (unless (eq window-system 'mac)
    (when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
      (menu-bar-mode -1)))
  (when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
    (scroll-bar-mode -1))
  (when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
    (tool-bar-mode -1))
  ;; tooltips in echo-aera
  (when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
    (tooltip-mode -1)))

(defun spacemacs/maybe-install-dotfile ()
  "Install the dotfile if it does not exist."
  (unless (file-exists-p dotspacemacs-filepath)
    (spacemacs-buffer/set-mode-line "Dotfile wizard installer")
    (spacemacs//redisplay)
    (when (dotspacemacs/install 'with-wizard)
      (dotspacemacs/sync-configuration-layers '(16)))))

(defun spacemacs/display-and-copy-version ()
  "Echo the current spacemacs version and copy it."
  (interactive)
  (let ((msg (format "Spacemacs v.%s" spacemacs-version)))
    (message msg) (kill-new msg)))

(defun display-startup-echo-area-message ()
  "Change the default welcome message of minibuffer to another one."
  (message "Spacemacs is ready."))

(defun spacemacs/setup-startup-hook ()
  "Add post init processing."
  (add-hook
   'emacs-startup-hook
   (lambda ()
     ;; Ultimate configuration decisions are given to the user who can defined
     ;; them in his/her ~/.spacemacs file
     ;; TODO remove support for dotspacemacs/config in 0.106
     (if (fboundp 'dotspacemacs/user-config)
         (dotspacemacs|call-func dotspacemacs/user-config
                                 "Calling dotfile user config...")
       (spacemacs-buffer/warning (concat "`dotspacemacs/config' is deprecated, "
                                         "please rename your function to "
                                         "`dotspacemacs/user-config'"))
       (dotspacemacs|call-func dotspacemacs/config
                               "Calling dotfile user config..."))
     ;; from jwiegley
     ;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
     (let ((elapsed (float-time
                     (time-subtract (current-time) emacs-start-time))))
       (spacemacs-buffer/append
        (format "\n[%s packages loaded in %.3fs]\n"
                (configuration-layer/configured-packages-count)
                elapsed)))
     (spacemacs/check-for-new-version spacemacs-version-check-interval))))

(defun spacemacs/describe-system-info ()
  "Gathers info about your Spacemacs setup and copies to clipboard."
  (interactive)
  (let ((sysinfo (format
                  (concat "#### System Info\n"
                          "- OS: %s\n"
                          "- Emacs: %s\n"
                          "- Spacemacs: %s\n"
                          "- Spacemacs branch: %s (rev. %s)\n"
                          "- Distribution: %s\n"
                          "- Layers:\n```elisp\n%s```\n")
                  system-type
                  emacs-version
                  spacemacs-version
                  (spacemacs/git-get-current-branch)
                  (spacemacs/git-get-current-branch-rev)
                  dotspacemacs-distribution
                  (pp dotspacemacs-configuration-layers))))
    (kill-new sysinfo)
    (message sysinfo)
    (message (concat "Information has been copied to clipboard.\n"
                     "You can paste it in the gitter chat.\n"
                     "Check the *Messages* buffer if you need to review it"))))

(provide 'core-spacemacs)
