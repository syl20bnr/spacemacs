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
(require 'core-evilify-keymap)
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

(defvar spacemacs-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [tab] 'widget-forward)
    (define-key map (kbd "C-i") 'widget-forward)
    (define-key map [backtab] 'widget-backward)
    (define-key map [return] 'widget-button-press)
    (define-key map [down-mouse-1] 'widget-button-click)
    map)
  "Keymap for spacemacs mode.")

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
  (dotspacemacs|call-func dotspacemacs/init "Calling dotfile init...")
  ;; spacemacs init
  (switch-to-buffer (get-buffer-create spacemacs-buffer-name))
  (spacemacs-buffer/set-mode-line "")
  ;; no welcome buffer
  (setq inhibit-startup-screen t)
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
  (setq tooltip-use-echo-area t)
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
  (setq-default evil-want-C-u-scroll t)
  ;; Initializing configuration from ~/.spacemacs
  (dotspacemacs|call-func dotspacemacs/init "Executing user init...")
  ;; dash is required to prevent a package.el bug with f on 24.3.1
  (spacemacs/load-or-install-package 'dash t)
  ;; bind-key is required by use-package
  (spacemacs/load-or-install-package 'bind-key t)
  (spacemacs/load-or-install-package 'use-package t)
  (setq use-package-verbose dotspacemacs-verbose-loading)
  ;; inject use-package hooks for easy customization of
  ;; stock package configuration
  (setq use-package-inject-hooks t)
  ;; evil and evil-leader must be installed at the beginning of the
  ;; boot sequence.
  ;; Use C-u as scroll-up (must be set before actually loading evil)
  (spacemacs/load-or-install-package 'evil t)
  (spacemacs/load-or-install-package 'evil-leader t)
  ;; check for new version
  (if dotspacemacs-mode-line-unicode-symbols
      (setq-default spacemacs-version-check-lighter "[⇪]"))
  (spacemacs/set-new-version-lighter-mode-line-faces)
  (add-hook 'after-init-hook 'spacemacs-buffer/goto-link-line)
  (spacemacs-mode))

(defun spacemacs//get-package-directory (pkg)
  "Return the directory of PKG. Return nil if not found."
  (let ((elpa-dir (concat user-emacs-directory "elpa/")))
    (when (file-exists-p elpa-dir)
      (let ((dir (reduce (lambda (x y) (if x x y))
                         (mapcar (lambda (x)
                                   (if (string-match
                                        (concat "/"
                                                (symbol-name pkg)
                                                "-[0-9]+") x) x))
                                 (directory-files elpa-dir 'full))
                         :initial-value nil)))
        (if dir (file-name-as-directory dir))))))

(defun spacemacs/load-or-install-package (pkg &optional log file-to-load)
  "Load PKG package. PKG will be installed if it is not already installed.
Whenever the initial require fails the absolute path to the package
directory is returned.
If LOG is non-nil a message is displayed in spacemacs-mode buffer.
FILE-TO-LOAD is an explicit file to load after the installation."
  (condition-case nil
      (require pkg)
    (error
     ;; not installed, we try to initialize package.el only if required to
     ;; precious seconds during boot time
     (require 'cl)
     (let ((pkg-elpa-dir (spacemacs//get-package-directory pkg)))
       (if pkg-elpa-dir
           (add-to-list 'load-path pkg-elpa-dir)
         ;; install the package
         (when log
           (spacemacs-buffer/append
            (format "(Bootstrap) Installing %s...\n" pkg))
           (spacemacs//redisplay))
         (package-refresh-contents)
         (package-install pkg)
         (setq pkg-elpa-dir (spacemacs//get-package-directory pkg)))
       (require pkg nil 'noerror)
       (when file-to-load
         (load-file (concat pkg-elpa-dir file-to-load)))
       pkg-elpa-dir))))

(defun spacemacs/maybe-install-dotfile ()
  "Install the dotfile if it does not exist."
  (unless (file-exists-p dotspacemacs-filepath)
    (spacemacs-buffer/set-mode-line "Dotfile wizard installer")
    (spacemacs//redisplay)
    (when (dotspacemacs/install 'with-wizard)
      (dotspacemacs/sync-configuration-layers)
      (spacemacs-buffer/append
       "The dotfile has been installed.\n"))))

(defun spacemacs/display-and-copy-version ()
  "Echo the current spacemacs version and copy it."
  (interactive)
  (let ((msg (format "Spacemacs v.%s" spacemacs-version)))
    (message msg) (kill-new msg)))

(defun display-startup-echo-area-message ()
  "Change the default welcome message of minibuffer to another one."
  (message "Spacemacs is ready."))

(defun spacemacs/setup-after-init-hook ()
  "Add post init processing."
  (add-hook
   'after-init-hook
   (lambda ()
     ;; Ultimate configuration decisions are given to the user who can defined
     ;; them in his/her ~/.spacemacs file
     (dotspacemacs|call-func dotspacemacs/config "Calling dotfile config...")
     ;; from jwiegley
     ;; https://github.com/jwiegley/dot-emacs/blob/master/init.el
     (let ((elapsed (float-time
                     (time-subtract (current-time) emacs-start-time))))
       (spacemacs-buffer/append
        (format "\n[%s packages loaded in %.3fs]\n"
                (configuration-layer//initialized-packages-count)
                elapsed)))
     ;; Display useful lists of items
     (when dotspacemacs-startup-lists
       (spacemacs-buffer/insert-startupify-lists))
     (when configuration-layer-error-count
       ;; ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position evil-mode-line-tag
        ;; (vc-mode vc-mode)
       ;; "  " mode-line-modes mode-line-misc-info mode-line-end-spaces
       (spacemacs-buffer/set-mode-line
        (format (concat "%s error(s) at startup! "
                        "Spacemacs may not be able to operate properly.")
                configuration-layer-error-count))
       (force-mode-line-update))
     (spacemacs/check-for-new-version spacemacs-version-check-interval))))

(provide 'core-spacemacs)
