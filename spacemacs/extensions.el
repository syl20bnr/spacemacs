;;; extensions.el --- Spacemacs Layer extensions File
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

;; Extensions are in emacs_paths/extensions
;; Pre extensions are loaded *before* the packages
(setq spacemacs-pre-extensions
  '(
    evil-evilified-state
    holy-mode
    ))

;; Post extensions are loaded *after* the packages
(setq spacemacs-post-extensions
  '(
    centered-cursor
    helm-spacemacs
    solarized-theme
    spacemacs-theme
    spray
    zoom-frm
    ;; hack to be able to wrap built-in emacs modes in an init function
    emacs-builtin-process-menu
    ))

;; use the last 24.3 compatible version of paradox as
;; an extension.
;; There is no corresponding init-paradox function here
;; the init-paradox from packages.el will be called
;; automatically.
(when (version< emacs-version "24.4")
    (push 'paradox spacemacs-post-extensions))

;; Initialize the extensions

(defun spacemacs/init-evil-evilified-state ()
  (require 'evil-evilified-state))

(defun spacemacs/init-centered-cursor ()
  (use-package centered-cursor-mode
    :commands (centered-cursor-mode
               global-centered-cursor-mode)
    :init
    (progn
      (spacemacs|add-toggle centered-point
                            :status centered-cursor-mode
                            :on (centered-cursor-mode)
                            :off (centered-cursor-mode -1)
                            :documentation
                            "Keep point always at the center of the window."
                            :evil-leader "t-")
      (spacemacs|add-toggle centered-point-globally
                            :status centered-cursor-mode
                            :on (global-centered-cursor-mode)
                            :off (global-centered-cursor-mode -1)
                            :documentation
                            "Globally keep point always at the center of the window."
                            :evil-leader "t C--"))
    :config
    (progn
      (custom-set-variables
       '(ccm-recenter-at-end-of-file t)
       '(ccm-ignored-commands (quote (mouse-drag-region
                                      mouse-set-point
                                      widget-button-click
                                      scroll-bar-toolkit-scroll
                                      evil-mouse-drag-region))))
      (spacemacs|diminish centered-cursor-mode " ⊝" " -"))))

(defun spacemacs/init-holy-mode ()
  (use-package holy-mode
    :commands holy-mode
    :init
    (progn
      (when (eq 'emacs dotspacemacs-editing-style)
        (holy-mode))
      (spacemacs|add-toggle holy-mode
                            :status holy-mode
                            :on (holy-mode)
                            :off (holy-mode -1)
                            :documentation "Globally toggle the holy mode."
                            :evil-leader "P <tab>" "P C-i"))))

(defun spacemacs/init-helm-spacemacs ()
  (use-package helm-spacemacs
    :commands helm-spacemacs
    :init
    (evil-leader/set-key "feh" 'helm-spacemacs)))

(defun spacemacs/init-revive ()
  (use-package revive
    :disabled t
    :init
    (require 'revive-mode-config)
    :config
    (progn
      ;; save and restore layout
      (add-hook 'kill-emacs-hook 'emacs-save-layout)
      (add-hook 'after-init-hook 'emacs-load-layout t))))

(defun spacemacs/init-spray ()
  (use-package spray
    :commands spray-mode
    :init
    (progn
      (defun spacemacs/start-spray ()
        "Start spray speed reading on current buffer at current point."
        (interactive)
        (evil-insert-state)
        (spray-mode t)
        (evil-insert-state-cursor-hide))
      (evil-leader/set-key "asr" 'spacemacs/start-spray)

      (defadvice spray-quit (after spacemacs//quit-spray activate)
        "Correctly quit spray."
        (set-default-evil-insert-state-cursor)
        (evil-normal-state)))
    :config
    (progn
      (define-key spray-mode-map (kbd "h") 'spray-backward-word)
      (define-key spray-mode-map (kbd "l") 'spray-forward-word)
      (define-key spray-mode-map (kbd "q") 'spray-quit))))

(defun spacemacs/init-solarized-theme ()
  (use-package solarized
    :init
    (progn
      (deftheme solarized-dark "The dark variant of the Solarized colour theme")
      (deftheme solarized-light "The light variant of the Solarized colour theme"))))

(defun spacemacs/init-spacemacs-theme ()
  (let ((path (configuration-layer/get-layer-property 'spacemacs :ext-dir)))
    (add-to-list 'custom-theme-load-path (concat path "spacemacs-theme"))))

(defun spacemacs/init-zoom-frm ()
  (use-package zoom-frm
    :commands (zoom-frm-unzoom
               zoom-frm-out
               zoom-frm-in)
    :init
    (progn
      (spacemacs|define-micro-state zoom-frm
        :doc "[+] zoom frame in [-] zoom frame out [=] reset zoom"
        :evil-leader "zf"
        :use-minibuffer t
        :bindings
        ("+" spacemacs/zoom-frm-in :post (spacemacs//zoom-frm-powerline-reset))
        ("-" spacemacs/zoom-frm-out :post (spacemacs//zoom-frm-powerline-reset))
        ("=" spacemacs/zoom-frm-unzoom :post (spacemacs//zoom-frm-powerline-reset)))

      (defun spacemacs//zoom-frm-powerline-reset ()
        (when (fboundp 'powerline-reset)
          (setq-default powerline-height (spacemacs/compute-powerline-height))
          (powerline-reset)))

      (defun spacemacs//zoom-frm-do (arg)
        "Perform a zoom action depending on ARG value."
        (let ((zoom-action (cond ((eq arg 0) 'zoom-frm-unzoom)
                                 ((< arg 0) 'zoom-frm-out)
                                 ((> arg 0) 'zoom-frm-in)))
              (fm (cdr (assoc 'fullscreen (frame-parameters))))
              (fwp (* (frame-char-width) (frame-width)))
              (fhp (* (frame-char-height) (frame-height))))
          (when (equal fm 'maximized)
            (toggle-frame-maximized))
          (funcall zoom-action)
          (set-frame-size nil fwp fhp t)
          (when (equal fm 'maximized)
            (toggle-frame-maximized))))

      (defun spacemacs/zoom-frm-in ()
        "zoom in frame, but keep the same pixel size"
        (interactive)
        (spacemacs//zoom-frm-do 1))

      (defun spacemacs/zoom-frm-out ()
        "zoom out frame, but keep the same pixel size"
        (interactive)
        (spacemacs//zoom-frm-do -1))

      (defun spacemacs/zoom-frm-unzoom ()
        "Unzoom current frame, keeping the same pixel size"
        (interactive)
        (spacemacs//zoom-frm-do 0))

      ;; Font size, either with ctrl + mouse wheel
      (global-set-key (kbd "<C-wheel-up>") 'spacemacs/zoom-frm-in)
      (global-set-key (kbd "<C-wheel-down>") 'spacemacs/zoom-frm-out))))

(defun spacemacs/init-emacs-builtin-process-menu ()
  (evilify process-menu-mode process-menu-mode-map))
