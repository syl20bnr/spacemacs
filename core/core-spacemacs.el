;;; core-space-macs.el --- Space-macs Core File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3
(setq message-log-max 16384)

(require 'subr-x nil 'noerror)
(require 'core-e-macs-backports)
(require 'core-env)
(require 'page-break-lines)
(require 'core-hooks)
(require 'core-debug)
(require 'core-command-line)
(require 'core-configuration-layer)
(require 'core-dotspace-macs)
(require 'core-custom-settings)
(require 'core-release-management)
(require 'core-jump)
(require 'core-display-init)
(require 'core-themes-support)
(require 'core-fonts-support)
(require 'core-space-macs-buffer)
(require 'core-keybindings)
(require 'core-toggle)
(require 'core-funcs)
(require 'core-micro-state)
(require 'core-transient-state)
(require 'core-use-package-ext)
(require 'core-spacebind)

(defgroup space-macs nil
  "Space-macs customizations."
  :group 'starter-kit
  :prefix 'space-macs-)

(defvar space-macs-post-user-config-hook nil
  "Hook run after dotspace-macs/user-config")
(defvar space-macs-post-user-config-hook-run nil
  "Whether `space-macs-post-user-config-hook' has been run")
(defvar space-macs-scratch-mode-hook nil
  "Hook run on buffer *scratch* after `dotspace-macs-scratch-mode' is invoked.")

(defvar space-macs--default-mode-line mode-line-format
  "Backup of default mode line format.")
(defvar space-macs-initialized nil
  "Whether or not space-macs has finished initializing by completing
the final step of executing code in `e-macs-startup-hook'.")

(defun space-macs/init ()
  "Perform startup initialization."
  (setq command-line-args (space-macs//parse-command-line command-line-args))
  (when space-macs-debugp (space-macs/init-debug))
  ;; silence ad-handle-definition about advised functions getting redefined
  (setq ad-redefinition-action 'accept)
  ;; this is for a smoother UX at startup (i.e. less graphical glitches)
  (hidden-mode-line-mode)
  (space-macs//removes-gui-elements)
  (space-macs//setup-ido-vertical-mode)
  ;; explicitly set the preferred coding systems to avoid annoying prompt
  ;; from e-macs (especially on Microsoft Windows)
  (prefer-coding-system 'utf-8)
  ;; TODO move these variables when evil is removed from the bootstrapped
  ;; packages.
  (setq-default evil-want-C-u-scroll t
                ;; `evil-want-C-i-jump' is set to nil to avoid `TAB' being
                ;; overlapped in terminal mode. The GUI specific `<C-i>' is used
                ;; instead.
                evil-want-C-i-jump nil)
  (dotspace-macs/load-file)
  (dotspace-macs|call-func dotspace-macs/init "Calling dotfile init...")
  (when dotspace-macs-undecorated-at-startup
    ;; this should be called before toggle-frame-maximized
    (set-frame-parameter nil 'undecorated t)
    (add-to-list 'default-frame-alist '(undecorated . t)))
  (when dotspace-macs-maximized-at-startup
    (unless (frame-parameter nil 'fullscreen)
      (toggle-frame-maximized))
    (add-to-list 'default-frame-alist '(fullscreen . maximized)))
  (space-macs|unless-dumping
    (dotspace-macs|call-func dotspace-macs/user-init "Calling dotfile user init..."))
  ;; Given the loading process of Space-macs we have no choice but to set the
  ;; custom settings twice:
  ;; - once at the very beginning of startup (here)
  ;; - once at the very end of loading (in `space-macs/setup-startup-hook')
  ;; The first application of custom settings is to be sure that e-macs knows all
  ;; the defined settings before saving them to a file (otherwise we loose all
  ;; the settings that e-macs does not know of).
  ;; The second application is to override any setting set in dotfile functions
  ;; like `dotspace-macs/user-config`, users expect the custom settings to be the
  ;; effective ones.
  ;; Note: Loading custom-settings twice is not ideal since they can have side
  ;; effects! Maybe an inhibit variable in e-macs can suppress these side effects?
  (space-macs/initialize-custom-file)
  ;; Commenting the first load although it is mentioned above that we must do it
  ;; I don't recall why we must load the custom settings twice and my experiment
  ;; seems to show that we don't need this double loading process anymore.
  ;; related issue: https://github.com/syl20bnr/space-macs/issues/9736
  ;; (dotspace-macs|call-func dotspace-macs/e-macs-custom-settings
  ;;                         "Calling dotfile e-macs custom settings...")
  (setq dotspace-macs-editing-style (dotspace-macs//read-editing-style-config
                                    dotspace-macs-editing-style))
  (configuration-layer/initialize)
  ;; frame title init
  (when dotspace-macs-frame-title-format
    (require 'format-spec)
    (setq frame-title-format '((:eval (space-macs/title-prepare dotspace-macs-frame-title-format))))
    (if dotspace-macs-icon-title-format
        (setq icon-title-format '((:eval (space-macs/title-prepare dotspace-macs-icon-title-format))))
      (setq icon-title-format frame-title-format)))
  ;; theme
  (space-macs/load-default-theme space-macs--fallback-theme 'disable)
  ;; font
  (space-macs|do-after-display-system-init
   ;; If you are thinking to remove this call to `message', think twice. You'll
   ;; break the life of several Space-macser using e-macs in daemon mode. Without
   ;; this, their chosen font will not be set on the *first* instance of
   ;; e-macsclient, at least if different than their system font. You don't
   ;; believe me? Go ahead, try it. After you'll have notice that this was true,
   ;; increase the counter bellow so next people will give it more confidence.
   ;; Counter = 1
   (space-macs-buffer/message "Setting the font...")
   (unless (space-macs/set-default-font dotspace-macs-default-font)
     (space-macs-buffer/warning
      "Cannot find any of the specified fonts (%s)! Font settings may not be correct."
      (if (listp (car dotspace-macs-default-font))
          (mapconcat 'car dotspace-macs-default-font ", ")
        (car dotspace-macs-default-font)))))
  ;; space-macs init
  (setq inhibit-startup-screen t)
  (space-macs-buffer/goto-buffer)
  (unless (display-graphic-p)
    ;; explicitly recreate the home buffer for the first GUI client
    ;; in order to correctly display the logo
    (space-macs|do-after-display-system-init
     (kill-buffer (get-buffer space-macs-buffer-name))
     (space-macs-buffer/goto-buffer)))
  ;; This is set to nil during startup to allow Space-macs to show buffers opened
  ;; as command line arguments.
  (setq initial-buffer-choice nil)
  (setq inhibit-startup-screen t)
  (require 'core-keybindings)
  ;; for convenience and user support
  (unless (fboundp 'tool-bar-mode)
    (space-macs-buffer/message (concat "No graphical support detected, "
                                      "you won't be able to launch a "
                                      "graphical instance of e-macs"
                                      "with this build.")))
  ;; check for new version
  (if dotspace-macs-mode-line-unicode-symbols
      (setq-default space-macs-version-check-lighter "[â‡ª]"))
  ;; load environment variables
  (if (fboundp 'dotspace-macs/user-env)
      (dotspace-macs/call-user-env)
    (space-macs/load-space-macs-env))
  ;; install the dotfile if required
  (dotspace-macs/maybe-install-dotfile))

(defun space-macs//removes-gui-elements ()
  "Remove the menu bar, tool bar and scroll bars."
  ;; removes the GUI elements
  (when (and (fboundp 'scroll-bar-mode) (not (eq scroll-bar-mode -1)))
    (scroll-bar-mode -1))
  (when (and (fboundp 'tool-bar-mode) (not (eq tool-bar-mode -1)))
    (tool-bar-mode -1))
  (unless (space-macs/window-system-is-mac)
    (when (and (fboundp 'menu-bar-mode) (not (eq menu-bar-mode -1)))
      (menu-bar-mode -1)))
  ;; tooltips in echo-aera
  (when (and (fboundp 'tooltip-mode) (not (eq tooltip-mode -1)))
    (tooltip-mode -1)))

(defun space-macs//setup-ido-vertical-mode ()
  "Setup `ido-vertical-mode'."
  (require 'ido-vertical-mode)
  (ido-vertical-mode t)
  (add-hook
   'ido-setup-hook
   ;; think about hacking directly `ido-vertical-mode' source in libs instead.
   (defun space-macs//ido-vertical-natural-navigation ()
     ;; more natural navigation keys: up, down to change current item
     ;; left to go up dir
     ;; right to open the selected item
     (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
     (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
     (define-key ido-completion-map (kbd "<left>") 'ido-delete-backward-updir)
     (define-key ido-completion-map (kbd "<right>") 'ido-exit-minibuffer))))

(defun display-startup-echo-area-message ()
  "Change the default welcome message of minibuffer to another one."
  (message "Space-macs is ready."))

(defun space-macs/defer-until-after-user-config (func)
  "Call FUNC if dotspace-macs/user-config has been called. Otherwise,
defer call using `space-macs-post-user-config-hook'."
  (if space-macs-post-user-config-hook-run
      (funcall func)
    (add-hook 'space-macs-post-user-config-hook func)))

(defun space-macs/setup-startup-hook ()
  "Add post init processing.
Note: the hooked function is not executed when in dumped mode."
  (add-hook
   'e-macs-startup-hook
   (defun space-macs/startup-hook ()
     ;; This is set here so that e-macsclient will show the startup buffer (and
     ;; so that it can be changed in user-config if necessary). It was set to
     ;; nil earlier in the startup process to properly handle command line
     ;; arguments.
     (setq initial-buffer-choice (lambda () (get-buffer space-macs-buffer-name)))

     ;; Activate winner-mode for non dumped e-macs sessions. Do this prior to
     ;; user-config to allow users to disable the feature and patch ediff
     ;; themselves. See issue 12582 for details.
     (winner-mode t)

     ;; Ultimate configuration decisions are given to the user who can defined
     ;; them in his/her ~/.space-macs file
     (dotspace-macs|call-func dotspace-macs/user-config
                             "Calling dotfile user config...")
     (dotspace-macs|call-func dotspace-macs/e-macs-custom-settings
                             "Calling dotfile e-macs custom settings...")
     ;; don't write custom settings into the dotfile before loading them,
     ;; otherwise https://github.com/syl20bnr/space-macs/issues/10504 happens
     (space-macs/initialize-custom-file-sync)
     (run-hooks 'space-macs-post-user-config-hook)
     (setq space-macs-post-user-config-hook-run t)
     (when (fboundp dotspace-macs-scratch-mode)
       (with-current-buffer "*scratch*"
         (funcall dotspace-macs-scratch-mode)
         (run-hooks 'space-macs-scratch-mode-hook)))
     (when space-macs--delayed-user-theme
       (space-macs/load-theme space-macs--delayed-user-theme
                             space-macs--fallback-theme t))
     (configuration-layer/display-summary e-macs-start-time)
     (space-macs-buffer//startup-hook)
     (space-macs/check-for-new-version nil space-macs-version-check-interval)
     (setq space-macs-initialized t)
     (setq gc-cons-threshold (car dotspace-macs-gc-cons)
           gc-cons-percentage (cadr dotspace-macs-gc-cons))
     (unless (version< e-macs-version "27")
       (setq read-process-output-max dotspace-macs-read-process-output-max)))))

(provide 'core-space-macs)


