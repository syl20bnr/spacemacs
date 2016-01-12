;;; packages.el --- Syntax Checking Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq syntax-checking-packages
  '(
    flycheck
    flycheck-pos-tip
    popwin
    ))

(defun syntax-checking/init-flycheck ()
  (use-package flycheck
    :defer t
    :init
    (progn
      (setq flycheck-standard-error-navigation nil)
      (spacemacs|add-toggle syntax-checking
        :status flycheck-mode
        :on (flycheck-mode)
        :off (flycheck-mode -1)
        :documentation "Enable error and syntax checking."
        :evil-leader "ts"))
    :config
    (progn
      (spacemacs|diminish flycheck-mode " ⓢ" " s")

      ;; Custom fringe indicator
      (when (fboundp 'define-fringe-bitmap)
        (define-fringe-bitmap 'my-flycheck-fringe-indicator
          (vector #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00011100
                  #b00111110
                  #b00111110
                  #b00111110
                  #b00011100
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000)))

      (flycheck-define-error-level 'error
        :overlay-category 'flycheck-error-overlay
        :fringe-bitmap 'my-flycheck-fringe-indicator
        :fringe-face 'flycheck-fringe-error)

      (flycheck-define-error-level 'warning
        :overlay-category 'flycheck-warning-overlay
        :fringe-bitmap 'my-flycheck-fringe-indicator
        :fringe-face 'flycheck-fringe-warning)

      (flycheck-define-error-level 'info
        :overlay-category 'flycheck-info-overlay
        :fringe-bitmap 'my-flycheck-fringe-indicator
        :fringe-face 'flycheck-fringe-info)

      ;; toggle flycheck window
      (defun spacemacs/toggle-flycheck-error-list ()
        "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
        (interactive)
        (-if-let (window (flycheck-get-error-list-window))
            (quit-window nil window)
          (flycheck-list-errors)))

      (evilified-state-evilify-map flycheck-error-list-mode-map
        :mode flycheck-error-list-mode
        :bindings
        "RET" 'flycheck-error-list-goto-error
        "j" 'flycheck-error-list-next-error
        "k" 'flycheck-error-list-previous-error)

      ;; key bindings
      (spacemacs/set-leader-keys
        "ec" 'flycheck-clear
        "eh" 'flycheck-describe-checker
        "el" 'spacemacs/toggle-flycheck-error-list
        "es" 'flycheck-select-checker
        "eS" 'flycheck-set-checker-executable
        "ev" 'flycheck-verify-setup))))

(defun syntax-checking/init-flycheck-pos-tip ()
  (use-package flycheck-pos-tip
    :if syntax-checking-enable-tooltips
    :defer t
    :init
    (with-eval-after-load 'flycheck
      (flycheck-pos-tip-mode))))

(defun syntax-checking/post-init-popwin ()
  (push '("^\\*Flycheck.+\\*$"
          :regexp t
          :dedicated t
          :position bottom
          :stick t
          :noselect t)
        popwin:special-display-config))
