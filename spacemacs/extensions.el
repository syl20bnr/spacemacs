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
(defvar spacemacs-pre-extensions '())

;; Post extensions are loaded *after* the packages
(defvar spacemacs-post-extensions
  '(
    centered-cursor
    emoji-cheat-sheet
    evil-escape
    evil-lisp-state
    helm-rcirc
    helm-spacemacs
    solarized-theme
    spray
    zoom-frm
    ))

;; use the last 24.3 compatible version of paradox as
;; an extension.
;; There is no corresponding init-paradox function here
;; the init-paradox from packages.el will be called
;; automatically.
(when (version< emacs-version "24.4")
    (push 'paradox spacemacs-post-extensions))

;; Initialize the extensions

(defun spacemacs/init-centered-cursor ()
  (use-package centered-cursor-mode
    :commands global-centered-cursor-mode
    :init
    (evil-leader/set-key "zz" 'global-centered-cursor-mode)
    :config
    (progn
      (custom-set-variables
       '(ccm-recenter-at-end-of-file t)
       '(ccm-ignored-commands (quote (mouse-drag-region
                                      mouse-set-point
                                      widget-button-click
                                      scroll-bar-toolkit-scroll
                                      evil-mouse-drag-region))))
      (spacemacs|diminish centered-cursor-mode " Ⓒ" " C"))))

(defun spacemacs/init-emoji-cheat-sheet ()
  (use-package emoji-cheat-sheet
    :commands emoji-cheat-sheet))

(defun spacemacs/init-evil-escape ()
  (use-package evil-escape
    :init
    (evil-escape-mode)
    :config
    (spacemacs|hide-lighter evil-escape-mode)))

(defun spacemacs/init-evil-lisp-state ()
  (require 'evil-lisp-state))

(defun spacemacs/init-helm-rcirc ()
  (use-package helm-rcirc
    :commands helm-rcirc-auto-join-channels
    :init
    (evil-leader/set-key "irc" 'helm-rcirc-auto-join-channels)))

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
      (evil-leader/set-key "asr"
        (lambda ()
          (interactive)
          (evil-insert-state)
          (spray-mode t)
          (evil-insert-state-cursor-hide))))
    :config
    (progn
      (define-key spray-mode-map (kbd "h") 'spray-backward-word)
      (define-key spray-mode-map (kbd "l") 'spray-forward-word)
      (define-key spray-mode-map (kbd "q")
        (lambda ()
          (interactive)
          (spray-quit)
          (set-default-evil-insert-state-cursor)
          (evil-normal-state))))))

(defun spacemacs/init-solarized-theme ()
  (use-package solarized
    :init
    (progn
      (deftheme solarized-dark "The dark variant of the Solarized colour theme")
      (deftheme solarized-light "The light variant of the Solarized colour theme"))))

(defun spacemacs/init-zoom-frm ()
  (use-package zoom-frm
    :commands (zoom-frm-unzoom
               zoom-frm-out
               zoom-frm-in)
    :init
    (progn
      (defun spacemacs/zoom-frame-overlay-map ()
        "Set a temporary overlay map to easily change the font size."
        (set-temporary-overlay-map
         (let ((map (make-sparse-keymap)))
           (define-key map (kbd "+") 'spacemacs/zoom-in-frame)
           (define-key map (kbd "-") 'spacemacs/zoom-out-frame)
           (define-key map (kbd "=") 'spacemacs/reset-zoom)
           map) t))

      (defun spacemacs/zoom-frame-micro-state-doc ()
        "Display a short documentation in the mini buffer."
        (echo "Zoom Frame micro-state
  + to zoom frame in
  - to zoom frame out
  = to reset zoom
Press any other key to exit."))

      (defun spacemacs/zoom-in-frame ()
        "Zoom in frame."
        (interactive)
        (spacemacs/zoom-in-or-out 1))

      (defun spacemacs/zoom-out-frame ()
        "Zoom out frame."
        (interactive)
        (spacemacs/zoom-in-or-out -1))

      (defun spacemacs/reset-zoom ()
        "Reset the zoom."
        (interactive)
        (spacemacs/zoom-in-or-out 0))

      (defun spacemacs/zoom-in-or-out (direction)
        "Zoom the buffer in/out. If DIRECTION is positive or zero the frame text is enlarged,
otherwise it is reduced."
        (interactive)
        (if (eq direction 0)
            (zoom-frm-unzoom)
          (if (< direction 0)
              (zoom-frm-out)
            (zoom-frm-in)))
        (spacemacs/zoom-frame-overlay-map)
        (spacemacs/zoom-frame-micro-state-doc))
      (evil-leader/set-key
        "zf+"  'spacemacs/zoom-in-frame
        "zf-"  'spacemacs/zoom-out-frame
        "zf="  'spacemacs/reset-zoom))))
