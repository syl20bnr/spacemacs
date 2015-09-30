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
(require 'core-emacs-backports)
(require 'core-release-management)
(require 'core-auto-completion)
(require 'core-themes-support)
(require 'core-fonts-support)
(require 'core-spacemacs-buffer)
(require 'core-toggle)
(require 'core-micro-state)
(require 'core-use-package-ext)
(require 'core-keybindings)

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

(defvar spacemacs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] 'widget-forward)
    (define-key map (kbd "C-i") 'widget-forward)
    (define-key map [backtab] 'widget-backward)
    (define-key map (kbd "RET") 'widget-button-press)
    (define-key map [down-mouse-1] 'widget-button-click)
    map)
  "Keymap for spacemacs mode.")

(defvar spacemacs--default-mode-line mode-line-format
  "Backup of default mode line format.")

(define-derived-mode spacemacs-mode special-mode "Spacemacs"
  "Spacemacs major mode for startup screen.

\\<spacemacs-mode-map>
"
  :group 'spacemacs
  :syntax-table nil
  :abbrev-table nil
  (setq truncate-lines t)
  ;; needed to make tab work correctly in terminal
  (evil-define-key 'motion spacemacs-mode-map (kbd "C-i") 'widget-forward)
  ;; motion state since this is a special mode
  (add-to-list 'evil-motion-state-modes 'spacemacs-mode))

(defun spacemacs/init ()
  "Create the special buffer for `spacemacs-mode' and perform startup
initialization."
  ;; explicitly set the prefered coding systems to avoid annoying prompt
  ;; from emacs (especially on Microsoft Windows)
  (prefer-coding-system 'utf-8)
  ;; dotfile init
  (dotspacemacs/load-file)
  ;; TODO remove evil-want-C-u-scroll and document it, we should not
  ;; shadow the universal argument
  (setq-default evil-want-C-u-scroll t)
  (dotspacemacs|call-func dotspacemacs/init "Calling dotfile init...")
  (dotspacemacs|call-func dotspacemacs/user-init "Calling dotfile user init...")
  ;; spacemacs init
  (switch-to-buffer (get-buffer-create spacemacs-buffer-name))
  (spacemacs-buffer/set-mode-line "")
  ;; no welcome buffer
  (setq inhibit-startup-screen t)
  ;; silence ad-handle-definition about advised functions getting redefined
  (setq ad-redefinition-action 'accept)
  ;; default theme
  (let ((default-theme (car dotspacemacs-themes)))
    (spacemacs/load-theme default-theme)
    ;; used to prevent automatic deletion of used packages
    (setq spacemacs-used-theme-packages
          (delq nil (mapcar 'spacemacs//get-theme-package
                            dotspacemacs-themes)))
    (setq-default spacemacs--cur-theme default-theme)
    (setq-default spacemacs--cycle-themes (cdr dotspacemacs-themes)))
  ;; removes the GUI elements
  (when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
    (tool-bar-mode -1))
  (when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
    (scroll-bar-mode -1))
  ;; tooltips in echo-aera
  (when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
    (tooltip-mode -1))
  (unless (eq window-system 'mac)
    (when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
      (menu-bar-mode -1)))
  ;; for convenience and user support
  (unless (fboundp 'tool-bar-mode)
    (spacemacs-buffer/message (concat "No graphical support detected, you won't be"
                                      "able to launch a graphical instance of Emacs"
                                      "with this build.")))
  ;; font
  (if (find-font (font-spec :name (car dotspacemacs-default-font)))
      (spacemacs/set-default-font dotspacemacs-default-font)
    (spacemacs-buffer/warning "Cannot find font \"%s\"!"
                              (car dotspacemacs-default-font)))
  ;; banner
  (spacemacs-buffer/insert-banner-and-buttons)
  ;; mandatory dependencies
  ;; dash is required to prevent a package.el bug with f on 24.3.1
  (spacemacs/load-or-install-package 'dash t)
  (spacemacs/load-or-install-package 's t)
  ;; bind-key is required by use-package
  (spacemacs/load-or-install-package 'bind-key t)
  (spacemacs/load-or-install-package 'use-package t)
  (setq use-package-verbose dotspacemacs-verbose-loading)
  ;; package-build is required by quelpa
  (spacemacs/load-or-install-package 'package-build t)
  (setq quelpa-verbose dotspacemacs-verbose-loading
        quelpa-dir (concat spacemacs-cache-directory "quelpa/")
        quelpa-build-dir (expand-file-name "build" quelpa-dir)
        quelpa-persistent-cache-file (expand-file-name "cache" quelpa-dir)
        quelpa-update-melpa-p nil)
  (spacemacs/load-or-install-package 'quelpa t)
  ;; inject use-package hooks for easy customization of
  ;; stock package configuration
  (setq use-package-inject-hooks t)
  ;; which-key
  (spacemacs/load-or-install-package 'which-key t)
  ;; evil and evil-leader must be installed at the beginning of the
  ;; boot sequence.
  ;; Use C-u as scroll-up (must be set before actually loading evil)
  (spacemacs/load-or-install-package 'evil t)
  (spacemacs/load-or-install-package 'evil-leader t)
  (require 'core-evilified-state)
  ;; check for new version
  (if dotspacemacs-mode-line-unicode-symbols
      (setq-default spacemacs-version-check-lighter "[⇪]"))
  (spacemacs/set-new-version-lighter-mode-line-faces)
  (add-hook 'emacs-startup-hook 'spacemacs-buffer/goto-link-line)
  (spacemacs-mode))

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
     ;; TODO remove support for dotspacemacs/config in 0.105
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
     ;; Display useful lists of items
     (when dotspacemacs-startup-lists
       (spacemacs-buffer/insert-startupify-lists))
     (if configuration-layer-error-count
         (spacemacs-buffer/set-mode-line
          (format (concat "%s error(s) at startup! "
                          "Spacemacs may not be able to operate properly.")
                  configuration-layer-error-count))
       (spacemacs-buffer/set-mode-line spacemacs--default-mode-line))
     (force-mode-line-update)
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
