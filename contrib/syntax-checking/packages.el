;;; packages.el --- Syntax Checking Layer packages File for Spacemacs
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

(setq syntax-checking-packages
  '(
    flycheck
    flycheck-pos-tip
    flyspell
    helm-flyspell
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
      ;; Make flycheck recognize packages in loadpath
      ;; i.e (require 'company) will not give an error now
      (setq flycheck-emacs-lisp-load-path 'inherit)

      (spacemacs|diminish flycheck-mode " ⓢ" " s")

      ;; color mode line faces
      (defun spacemacs/defface-flycheck-mode-line-color (state)
        "Define a face for the given Flycheck STATE."
        (let* ((fname (intern (format "spacemacs-mode-line-flycheck-%s-face"
                                      (symbol-name state))))
              (foreground (face-foreground
                           (intern (format "flycheck-fringe-%s" state)))))
          (eval `(defface ,fname '((t ()))
                   ,(format "Color for Flycheck %s feedback in mode line."
                            (symbol-name state))
                   :group 'spacemacs))
          (set-face-attribute fname nil
                              :foreground foreground
                              :box (face-attribute 'mode-line :box))))

      (defun spacemacs/set-flycheck-mode-line-faces ()
        "Define or set the flycheck info mode-line faces."
        (mapcar 'spacemacs/defface-flycheck-mode-line-color
                '(error warning info)))
      (spacemacs/set-flycheck-mode-line-faces)

      (defmacro spacemacs|custom-flycheck-lighter (error)
        "Return a formatted string for the given ERROR (error, warning, info)."
        `(let* ((error-counts (flycheck-count-errors
                               flycheck-current-errors))
                (errorp (flycheck-has-current-errors-p ',error))
                (err (or (cdr (assq ',error error-counts)) "?"))
                (running (eq 'running flycheck-last-status-change)))
           (if (or errorp running) (format "•%s " err))))

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
                  #b01111111)))

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

      ;; key bindings
      (evil-leader/set-key
        "ec" 'flycheck-clear
        "el" 'flycheck-list-errors))))

(defun syntax-checking/init-flycheck-pos-tip ()
  (use-package flycheck-pos-tip
    :if syntax-checking-enable-tooltips
    :defer t
    :init
    (setq flycheck-display-errors-function 'flycheck-pos-tip-error-messages)))

(defun syntax-checking/init-flyspell ()
  (use-package flyspell
    :defer t
    :init
    (progn
      (setq-default ispell-program-name "aspell")
      (setq-default ispell-dictionary "english")
      (add-hook 'markdown-mode-hook '(lambda () (flyspell-mode 1)))
      (add-hook 'text-mode-hook '(lambda () (flyspell-mode 1)))
      (spacemacs|add-toggle spelling-checking
        :status flyspell-mode
        :on (flyspell-mode)
        :off (flyspell-mode -1)
        :documentation
        "Enable flyspell for automatic spelling checking."
        :evil-leader "tS"))
    :config
    (progn
      (flyspell-prog-mode)
      (spacemacs|diminish flyspell-mode " Ⓢ" " S"))))

(defun syntax-checking/init-helm-flyspell ()
  (use-package helm-flyspell
    :commands helm-flyspell-correct
    :init (evil-leader/set-key "Sc" 'helm-flyspell-correct)))

(defun syntax-checking/post-init-popwin ()
  (push '("^\*Flycheck.+\*$" :regexp t :dedicated t :position bottom :stick t :noselect t) popwin:special-display-config))
