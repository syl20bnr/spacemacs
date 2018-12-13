;;; packages.el --- Colors Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq colors-packages
  '(
    ;; not working well for now
    ;; rainbow-blocks
    (nyan-mode :location local)
    color-identifiers-mode
    rainbow-identifiers
    rainbow-mode
    ))

;; (defun colors/init-rainbow-blocks ()
;;   (use-package rainbow-blocks
;;     :disabled t
;;     :init (add-hook 'emacs-lisp-mode-hook 'rainbow-blocks-mode)))

(defun colors/init-nyan-mode ()
  (use-package nyan-mode
    :if colors-enable-nyan-cat-progress-bar
    :config
    (progn
      (setq nyan-wavy-trail t)
      (setq nyan-animate-nyancat t)
      (nyan-mode)
      ;; explicitly re-enable the cat for the first GUI client
      (spacemacs|do-after-display-system-init
       (nyan-mode -1)
       (nyan-mode))

      (spacemacs|add-toggle nyan-cat-progress-bar
        :mode nyan-mode
        :documentation "Show a nyan cat progress bar in the mode-line."
        :evil-leader "tmn"))))

(defun colors/init-color-identifiers-mode ()
  (use-package color-identifiers-mode
    :defer t
    :init
    (progn
      (when (eq 'variables colors-colorize-identifiers)
        (add-hook 'prog-mode-hook 'color-identifiers-mode))
      (spacemacs/declare-prefix "Ci" "colors-identifiers")
      (spacemacs|add-toggle color-identifiers-mode
        :status color-identifiers-mode
        :on (progn
              (when (bound-and-true-p rainbow-identifiers-mode)
                (rainbow-identifiers-mode -1))
              (color-identifiers-mode))
        :off (color-identifiers-mode -1)
        :documentation "Colorize variables."
        :evil-leader "tCv")
      (spacemacs|add-toggle global-color-identifiers-mode
        :status global-color-identifiers-mode
        :on (progn
              (when (bound-and-true-p global-rainbow-identifiers-mode)
                (global-rainbow-identifiers-mode -1))
              (global-color-identifiers-mode))
        :off (global-color-identifiers-mode -1)
        :documentation "Colorize variables globally."
        :evil-leader "tC C-v"))
    :config (spacemacs|hide-lighter color-identifiers-mode)))

(defun colors/init-rainbow-identifiers ()
  (use-package rainbow-identifiers
    :commands (global-rainbow-identifiers-mode
               rainbow-identifiers-mode)
    :init
    (progn
      (setq rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face
            ;; defaults to use before we've loaded per-theme settings
            rainbow-identifiers-cie-l*a*b*-saturation colors-default-rainbow-identifiers-sat
            rainbow-identifiers-cie-l*a*b*-lightness colors-default-rainbow-identifiers-light
            ;; override theme faces
            rainbow-identifiers-faces-to-override '(highlight-quoted-symbol
                                                    font-lock-keyword-face
                                                    font-lock-function-name-face
                                                    font-lock-variable-name-face))
      (defadvice spacemacs/post-theme-init (after colors/post-theme-init activate)
        "Adjust lightness and brightness of rainbow-identifiers on post theme init."
        (colors//tweak-theme-colors spacemacs--cur-theme))
      ;; key bindings
      (spacemacs/declare-prefix "Ci" "colors-identifiers")
      (spacemacs|add-toggle rainbow-identifier
        :status rainbow-identifiers-mode
        :on (progn
              (when (bound-and-true-p color-identifiers-mode)
                (color-identifiers-mode -1))
              (rainbow-identifiers-mode))
        :off (rainbow-identifiers-mode -1)
        :documentation "Colorize all identifiers."
        :evil-leader "tCa")
      (with-eval-after-load 'rainbow-identifiers
        (define-global-minor-mode global-rainbow-identifiers-mode
          rainbow-identifiers-mode colors//rainbow-identifiers-mode-maybe))
      (spacemacs|add-toggle global-rainbow-identifiers-mode
        :status global-rainbow-identifiers-mode
        :on (progn
              (when (bound-and-true-p global-color-identifiers-mode)
                (global-color-identifiers-mode -1))
              (global-rainbow-identifiers-mode))
        :off (global-rainbow-identifiers-mode -1)
        :documentation "Colorize identifiers globally."
        :evil-leader "tC C-a")
      (spacemacs/set-leader-keys "Cis" 'colors/start-change-color-saturation)
      (spacemacs/set-leader-keys "Cil" 'colors/start-change-color-lightness)
      ;; tweak colors of current theme
      (colors//tweak-theme-colors spacemacs--cur-theme)
      (when (eq 'all colors-colorize-identifiers)
        (global-rainbow-identifiers-mode)))))

(defun colors/init-rainbow-mode ()
  (use-package rainbow-mode
    :defer t
    :init (spacemacs/set-leader-keys "tCc" 'rainbow-mode)
    :config (spacemacs|hide-lighter rainbow-mode)))
