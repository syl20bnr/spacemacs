;;; core-spacemacs.el --- Spacemacs Core File -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
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

;; Increase the message log size to avoid losing startup messages.
(setq message-log-max 16384)

;; Define the main customization group for Spacemacs.
(defgroup spacemacs nil
  "Spacemacs customizations."
  :group 'emacs
  :prefix 'spacemacs-)

;; Load required core modules and libraries.
(require 'subr-x nil 'noerror) ;; Useful string and list functions.
(require 'core-versions)
(require 'core-load-paths)
(require 'core-emacs-backports)
(require 'core-env)
(require 'core-hooks)
(require 'core-debug)
(require 'core-command-line)
(require 'core-configuration-layer)
(require 'core-dotspacemacs)
(require 'core-custom-settings)
(require 'core-release-management)
(require 'core-jump)
(require 'core-display-init)
(require 'core-themes-support)
(require 'core-fonts-support)
(require 'core-spacemacs-buffer)
(require 'core-keybindings)
(require 'core-toggle)
(require 'core-early-funcs)
(require 'core-funcs)
(require 'core-micro-state)
(require 'core-transient-state)
(require 'core-use-package-ext)
(require 'core-spacebind)
(require 'core-compilation)

;; Hooks for post-user configuration and scratch buffer customization.
(defvar spacemacs-post-user-config-hook nil
  "Hook run after dotspacemacs/user-config")
(defvar spacemacs-post-user-config-hook-run nil
  "Whether `spacemacs-post-user-config-hook' has been run")
(defvar spacemacs-scratch-mode-hook nil
  "Hook run on buffer *scratch* after `dotspacemacs-scratch-mode' is invoked.")

;; Backup of the default mode line format, so it can be restored if needed.
(defvar spacemacs--default-mode-line mode-line-format
  "Backup of default mode line format.")

;; Flag indicating whether Spacemacs has finished initializing.
(defvar spacemacs-initialized nil
  "Whether or not spacemacs has finished initializing by completing
the final step of executing code in `emacs-startup-hook'.")

;; Utility to regenerate autoloads for installed packages.
(defun spacemacs//package-regenerate-autoloads (&optional path)
  "Regenerate the autoloads for installed packages.
If PATH is provided, use it as the package directory, otherwise use `package-user-dir'."
  (interactive "P")
  (dolist (dir (or path (list package-user-dir)))
    (when (file-directory-p dir)
      (dolist (pkg-dir (directory-files dir t "\\`[^.]"))
        (when-let* (((file-directory-p pkg-dir))
                    (pkg-desc (package-load-descriptor pkg-dir)))
          (let ((default-directory pkg-dir))
            ;; Remove existing autoload files before regenerating.
            (mapc 'delete-file (file-expand-wildcards "*-autoloads.el" )))
          (package-generate-autoloads
           (package-desc-name pkg-desc) pkg-dir))))))

;; Lookup load hints for a given file.
(defsubst spacemacs//lookup-load-hints (file)
  "Find out the `load-hints' item for the FILE.
Returns the directory path from load-hints where FILE is found."
  (unless (file-name-absolute-p file)
    (car-safe (seq-find (lambda (row) (member file (cdr row))) load-hints))))

;; Activate load hints support for Spacemacs.
(defun spacemacs//activate-load-hints ()
  "Enable the `load-hints' support for Spacemacs.
This helps Emacs locate files more efficiently by maintaining a mapping
of directories to file basenames."
  (setq package-enable-load-hints dotspacemacs-enable-load-hints
        load-hints
        (mapcar
         (lambda (path)
           (when-let* (((file-directory-p path))
                       (files (seq-difference
                               (directory-files path) '("." "..")
                               #'string=))
                       ;; Remove load-suffixes from file basenames.
                       (bases
                        (mapcar
                         (lambda (f)
                           (seq-some
                            (lambda (s)
                              (if-let* ((n (length s))
                                        ((length> f n))
                                        ((string= s (substring f (- n)))))
                                  (substring f 0 (- n))))
                            (get-load-suffixes)))
                         files)))
             (cons path (seq-uniq (remove nil bases) 'string-equal))))
         load-path))

  ;; Advice for `require' to use load-hints when loading features.
  (defun require@LOAD-HINTS (args)
    "Advice for `require' to use load-hints for locating files."
    (let ((feature (nth 0 args))
          (filename (nth 1 args))
          (noerror (nth 2 args)))
      (when-let* (((not filename))
                  (name (symbol-name feature))
                  (path (spacemacs//lookup-load-hints name)))
        (setq filename (expand-file-name name path)))
      (list feature filename noerror)))

  (advice-add #'require :filter-args #'require@LOAD-HINTS '((depth . -99)))

  ;; Advice to update load-hints after autoload generation.
  (define-advice package-generate-autoloads (:after (name pkg-dir) LOAD-HINTS)
    ;; If load-hints are enabled, collect loadable files in pkg-dir and update load-hints.
    (when-let* (dotspacemacs-enable-load-hints
                (auto-name (format "%s-autoloads.el" name))
                (output-file (expand-file-name auto-name pkg-dir))
                (name (symbol-name name))
                (files (seq-difference
                        (directory-files pkg-dir)
                        `("." ".." ,(concat name "-pkg.el") ,auto-name)
                        #'string=))
                ;; Remove load-suffixes from file basenames.
                (bases
                 (remove nil
                         (mapcar
                          (lambda (f)
                            (seq-some
                             (lambda (s)
                               (if-let* ((n (length s))
                                         ((length> f n))
                                         ((string= s (substring f (- n)))))
                                   (substring f 0 (- n))))
                             (get-load-suffixes)))
                          files))))
      (with-current-buffer (find-file-noselect output-file)
        (goto-char (point-min))
        (when (re-search-forward "add-to-list 'load-path" nil t)
          (forward-line 0)
          (insert (format "(add-to-list 'load-hints (cons %S '%S))\n"
                          '(or (and load-file-name
                                    (directory-file-name
                                     (file-name-directory load-file-name)))
                               (car load-path))
                          (seq-uniq bases 'string-equal))))
        (save-buffer)
        (kill-buffer)))))

;; Main initialization function for Spacemacs startup.
(defun spacemacs/init ()
  "Perform startup initialization for Spacemacs."
  ;; Parse command line arguments for Spacemacs-specific options.
  (setq command-line-args (spacemacs//parse-command-line command-line-args))
  ;; Enable debug mode if requested.
  (when spacemacs-debugp (spacemacs/init-debug))
  ;; Accept redefinition of advised functions to silence warnings.
  (setq ad-redefinition-action 'accept)
  ;; Hide mode line for smoother startup visuals.
  (hidden-mode-line-mode)
  ;; Disable GUI elements (toolbars, scrollbars, etc.) for a cleaner look.
  (spacemacs//toggle-gui-elements 0)
  ;; Setup vertical ido mode for the setup wizard.
  (spacemacs//setup-ido-vertical-mode)
  ;; Set preferred coding system to UTF-8 to avoid prompts.
  (prefer-coding-system 'utf-8)
  ;; Extend use-package if installed.
  (spacemacs/use-package-extend)
  ;; Evil mode settings for scrolling and jump behavior.
  (setq-default evil-want-C-u-scroll t
                evil-want-C-i-jump nil)
  ;; Load the user's dotspacemacs file.
  (dotspacemacs/load-file)
  ;; Call the user's initialization function.
  (dotspacemacs|call-func dotspacemacs/init "Calling dotfile init...")
  ;; Handle undecorated and maximized frame settings at startup.
  (when dotspacemacs-undecorated-at-startup
    (set-frame-parameter nil 'undecorated t)
    (set-frame-parameter nil 'internal-border-width 0)
    (add-to-list 'default-frame-alist '(undecorated . t))
    (add-to-list 'default-frame-alist '(internal-border-width . 0)))
  (when dotspacemacs-maximized-at-startup
    (unless (frame-parameter nil 'fullscreen)
      (toggle-frame-maximized))
    (add-to-list 'default-frame-alist '(fullscreen . maximized)))
  ;; Call the user's user-init function.
  (dotspacemacs|call-func dotspacemacs/user-init "Calling dotfile user init...")
  ;; Initialize custom settings early to ensure Emacs knows about them.
  (spacemacs/initialize-custom-file)
  ;; Set editing style from config.
  (setq dotspacemacs-editing-style (dotspacemacs//read-editing-style-config
                                    dotspacemacs-editing-style))
  ;; Initialize the configuration layer (layers, packages, etc.).
  (configuration-layer/initialize)
  ;; Set frame and icon title formats if specified.
  (when dotspacemacs-frame-title-format
    (require 'format-spec)
    (setq frame-title-format '((:eval (spacemacs/title-prepare dotspacemacs-frame-title-format))))
    (if dotspacemacs-icon-title-format
        (setq icon-title-format '((:eval (spacemacs/title-prepare dotspacemacs-icon-title-format))))
      (setq icon-title-format frame-title-format)))
  ;; Activate load hints if enabled and not already available.
  (when (and dotspacemacs-enable-load-hints (not (boundp 'load-hints)))
    (spacemacs//activate-load-hints))
  ;; Ensure load-hints variable exists for autoloads.
  (unless (boundp 'load-hints)
    (defvar load-hints '()))
  ;; Load the default theme.
  (spacemacs/load-default-theme)
  ;; Set the default font after display system is initialized.
  (spacemacs|do-after-display-system-init
    (unless (spacemacs/set-default-font dotspacemacs-default-font)
      (spacemacs-buffer/warning
       "Cannot find any of the specified fonts (%s)! Font settings may not be correct."
       (if (listp (car dotspacemacs-default-font))
           (mapconcat 'car dotspacemacs-default-font ", ")
         (car dotspacemacs-default-font)))))
  ;; Inhibit the default Emacs startup screen.
  (setq inhibit-startup-screen t)
  ;; Go to the Spacemacs buffer on startup.
  (spacemacs-buffer/goto-buffer t)
  ;; Allow buffers opened via command line arguments to be shown.
  (setq initial-buffer-choice nil)
  (require 'core-keybindings)
  ;; Warn if graphical support is missing.
  (unless (fboundp 'tool-bar-mode)
    (spacemacs-buffer/message (concat "No graphical support detected, "
                                      "you won't be able to launch a "
                                      "graphical instance of Emacs"
                                      "with this build.")))
  ;; Set mode line lighter for version check if unicode symbols are enabled.
  (if dotspacemacs-mode-line-unicode-symbols
      (setq-default spacemacs-version-check-lighter "[⇪]"))
  ;; Load environment variables from user config or default env.
  (if (fboundp 'dotspacemacs/user-env)
      (dotspacemacs/call-user-env)
    (spacemacs/load-spacemacs-env))
  ;; Install the dotfile if required.
  (dotspacemacs/maybe-install-dotfile))

;; Setup ido-vertical-mode for the setup wizard.
(defun spacemacs//setup-ido-vertical-mode ()
  "Setup `ido-vertical-mode' for the setup wizard.
Only activates after ido is loaded, for use in the dotfile setup wizard."
  (with-eval-after-load 'ido
    (require 'ido-vertical-mode)
    (ido-vertical-mode t)
    (add-hook
     'ido-setup-hook
     ;; Natural navigation keys for ido vertical mode.
     (defun spacemacs//ido-vertical-natural-navigation ()
       (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
       (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
       (define-key ido-completion-map (kbd "<left>") 'ido-delete-backward-updir)
       (define-key ido-completion-map (kbd "<right>") 'ido-exit-minibuffer)))))

;; Override the default startup echo area message.
(defun display-startup-echo-area-message ()
  "Change the default welcome message of minibuffer to another one."
  (message "Spacemacs is ready."))

;; Utility to defer function execution until after user config.
(defun spacemacs/defer-until-after-user-config (func)
  "Call FUNC if dotspacemacs/user-config has been called. Otherwise,
defer call using `spacemacs-post-user-config-hook'."
  (if spacemacs-post-user-config-hook-run
      (funcall func)
    (add-hook 'spacemacs-post-user-config-hook func)))

;; Remove byte-compiled versions of files listed in spacemacs-compiled-files.
(defun spacemacs//byte-compile-cleanup ()
  "Remove byte-compiled versions of `spacemacs-compiled-files'."
  (let ((default-directory spacemacs-start-directory))
    (spacemacs//remove-byte-compiled-files
     spacemacs-compiled-files)))

;; Setup the startup hook for post-initialization processing.
(defun spacemacs/setup-startup-hook ()
  "Add post init processing to `emacs-startup-hook'."
  (add-hook
   'emacs-startup-hook
   (defun spacemacs/startup-hook ()
     ;; Set initial buffer choice to the Spacemacs buffer.
     (setq initial-buffer-choice (lambda () (get-buffer spacemacs-buffer-name)))
     ;; Enable winner-mode for window configuration undo/redo.
     (winner-mode t)
     ;; Call user config and custom settings from dotfile.
     (dotspacemacs|call-func dotspacemacs/user-config
                             "Calling dotfile user config...")
     (dotspacemacs|call-func dotspacemacs/emacs-custom-settings
                             "Calling dotfile Emacs custom settings...")
     ;; Sync custom file settings.
     (spacemacs/initialize-custom-file-sync)
     ;; Run post-user-config hooks.
     (run-hooks 'spacemacs-post-user-config-hook)
     (setq spacemacs-post-user-config-hook-run t)
     ;; Setup scratch buffer mode if defined.
     (when (fboundp dotspacemacs-scratch-mode)
       (when (get-buffer "*scratch*")
         (with-current-buffer "*scratch*"
           (funcall dotspacemacs-scratch-mode)
           (run-hooks 'spacemacs-scratch-mode-hook))))
     ;; Load delayed user theme if set.
     (when spacemacs--delayed-user-theme
       (spacemacs/load-theme spacemacs--delayed-user-theme
                             spacemacs--fallback-theme t))
     ;; Check for new Spacemacs version.
     (spacemacs/check-for-new-version nil spacemacs-version-check-interval)
     ;; Move cursor to link line in Spacemacs buffer.
     (spacemacs-buffer/goto-link-line)
     ;; Mark Spacemacs as initialized.
     (setq spacemacs-initialized t)
     ;; Set process output max for LSP and other features.
     (setq read-process-output-max dotspacemacs-read-process-output-max)
     ;; Redraw Spacemacs buffer to ensure it displays correctly.
     (spacemacs-buffer//startup-hook)
     ;; Display configuration layer summary.
     (configuration-layer/display-summary)
     ;; Set garbage collection settings for performance.
     (setq gc-cons-threshold (car dotspacemacs-gc-cons)
           gc-cons-percentage (cadr dotspacemacs-gc-cons))))

  ;; Byte-compile or clean up core libs as needed.
  (if dotspacemacs-byte-compile
      (when (> 1 (spacemacs//dir-byte-compile-state
                  (concat spacemacs-core-directory "libs/")))
        (byte-recompile-directory (concat spacemacs-core-directory "libs/") 0))
    (spacemacs//remove-byte-compiled-files-in-dir spacemacs-core-directory))

  ;; Check if Spacemacs revision has changed.
  (spacemacs//revision-check))

;; Provide the core-spacemacs feature for require.
(provide 'core-spacemacs)
