;;; packages.el --- Eyebrowse Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq eyebrowse-packages '(eyebrowse))

(defun eyebrowse/init-eyebrowse ()
  (use-package eyebrowse
    :init
    (progn
      (setq eyebrowse-new-workspace #'spacemacs/home
            eyebrowse-wrap-around t)
      (eyebrowse-mode)

      ;; vim-style tab switching
      (define-key evil-motion-state-map "gt" 'eyebrowse-next-window-config)
      (define-key evil-motion-state-map "gT" 'eyebrowse-prev-window-config)

      (defun spacemacs/workspaces-ms-rename ()
        "Rename a workspace and get back to micro-state."
        (interactive)
        (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) nil)
        (spacemacs/workspaces-micro-state))

      (defun spacemacs//workspaces-ms-get-slot-name (window-config)
        "Return the name for the given window-config"
        (let ((slot (car window-config))
              (caption (eyebrowse-format-slot window-config)))
          (if (= slot current-slot)
              (format "[%s]" caption)
            caption)))

      (defun spacemacs//workspaces-ms-get-window-configs ()
        "Return the list of window configs."
        (--sort (if (eq (car other) 0)
                    t
                  (< (car it) (car other)))
                (eyebrowse--get 'window-configs)))

      (defun spacemacs//workspaces-ms-documentation ()
        "Return the docstring for the workspaces micro-state."
        (let* ((current-slot (eyebrowse--get 'current-slot))
               (window-configs (spacemacs//workspaces-ms-get-window-configs)))
          (concat
           "<" (if window-configs
                   (concat
                    (mapconcat 'spacemacs//workspaces-ms-get-slot-name
                               window-configs "> <") ">")
                 (when eyebrowse-display-help
                   (concat
                    "\n[0-9] to create/switch to a workspace, "
                    "[n] next, [p/N] previous, [TAB] back and forth, [c] close, "
                    "[r] rename"))))))

      (global-set-key (kbd "<C-tab>")
                      'spacemacs//workspaces-eyebrowse-next-window-config-l)
      (global-set-key (kbd "<C-S-tab>")
                      'spacemacs//workspaces-eyebrowse-prev-window-config-h)
      (global-set-key (kbd "<C-S-iso-lefttab>")
                      'spacemacs//workspaces-eyebrowse-prev-window-config-h)

      (global-set-key (kbd "M-s-1") 'spacemacs//workspaces-eyebrowse-switch-to-window-config-1-1)
      (global-set-key (kbd "M-s-2") 'spacemacs//workspaces-eyebrowse-switch-to-window-config-2-2)
      (global-set-key (kbd "M-s-3") 'spacemacs//workspaces-eyebrowse-switch-to-window-config-3-3)
      (global-set-key (kbd "M-s-4") 'spacemacs//workspaces-eyebrowse-switch-to-window-config-4-4)
      (global-set-key (kbd "M-s-5") 'spacemacs//workspaces-eyebrowse-switch-to-window-config-5-5)
      (global-set-key (kbd "M-s-6") 'spacemacs//workspaces-eyebrowse-switch-to-window-config-6-6)
      (global-set-key (kbd "M-s-7") 'spacemacs//workspaces-eyebrowse-switch-to-window-config-7-7)
      (global-set-key (kbd "M-s-8") 'spacemacs//workspaces-eyebrowse-switch-to-window-config-8-8)
      (global-set-key (kbd "M-s-9") 'spacemacs//workspaces-eyebrowse-switch-to-window-config-9-9)
      (global-set-key (kbd "M-s-0") 'spacemacs//workspaces-eyebrowse-switch-to-window-config-0-0)

      (spacemacs|define-micro-state workspaces
        :doc (spacemacs//workspaces-ms-documentation)
        :use-minibuffer t
        :bindings
        ("0" eyebrowse-switch-to-window-config-0)
        ("1" eyebrowse-switch-to-window-config-1)
        ("2" eyebrowse-switch-to-window-config-2)
        ("3" eyebrowse-switch-to-window-config-3)
        ("4" eyebrowse-switch-to-window-config-4)
        ("5" eyebrowse-switch-to-window-config-5)
        ("6" eyebrowse-switch-to-window-config-6)
        ("7" eyebrowse-switch-to-window-config-7)
        ("8" eyebrowse-switch-to-window-config-8)
        ("9" eyebrowse-switch-to-window-config-9)
        ("<tab>" eyebrowse-last-window-config)
        ("C-i" eyebrowse-last-window-config)
        ("c" eyebrowse-close-window-config)
        ("h" eyebrowse-prev-window-config)
        ("l" eyebrowse-next-window-config)
        ("n" eyebrowse-next-window-config)
        ("N" eyebrowse-prev-window-config)
        ("p" eyebrowse-prev-window-config)
        ("r" spacemacs/workspaces-ms-rename :exit t)
        ("w" eyebrowse-switch-to-window-config :exit t))

      ;; The layouts layer defines this keybinding inside a microstate
      ;; thus this is only needed if that layer is not used
      (unless (configuration-layer/layer-usedp 'spacemacs-layouts)
        (spacemacs/set-leader-keys "lw" 'spacemacs/workspaces-micro-state)))))
