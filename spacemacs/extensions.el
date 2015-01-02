;; Extensions are in emacs_paths/extensions

;; Pre extensions are loaded *before* the packages
(defvar spacemacs-pre-extensions
  '(
    key-chord
    ))

;; Post extensions are loaded *after* the packages
(defvar spacemacs-post-extensions
  '(
    centered-cursor
    emoji-cheat-sheet
    evil-plugins
    evil-tutor
    helm-rcirc
    helm-spacemacs
    neotree
    solarized-theme
    spray
    ))

;; Initialize the extensions

(defun spacemacs/init-evil-tutor ()
  (use-package evil-tutor
    :commands (evil-tutor/start
               evil-tutor/resume)
    :init
    (progn
      (setq evil-tutor-working-directory
            (concat spacemacs-cache-directory ".tutor/"))
      (evil-leader/set-key "hT" 'evil-tutor/start))))

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

(defun spacemacs/init-evil-plugins ()
  (use-package evil-little-word))

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

(defun spacemacs/init-neotree ()
  (use-package neotree
    :commands (neotree-toggle)
    :init
    (progn
      (add-to-list 'evil-motion-state-modes 'neotree-mode)
      (setq neo-window-width 32
            neo-create-file-auto-open t
            neo-banner-message nil
            neo-show-updir-line nil
            neo-mode-line-type 'neotree
            neo-smart-open t
            neo-dont-be-alone t
            neo-persist-show nil
            neo-show-hidden-files t
            neo-auto-indent-point t)

      (defun spacemacs/neotree-expand-or-open ()
        "Collapse a neotree node."
        (interactive)
        (let ((node (neo-buffer--get-filename-current-line)))
          (when node
            (if (file-directory-p node)
                (progn
                  (neo-buffer--set-expand node t)
                  (neo-buffer--refresh t)
                  (when neo-auto-indent-point
                    (next-line)
                    (neo-point-auto-indent)))
              (call-interactively 'neotree-enter)))))

      (defun spacemacs/neotree-collapse ()
        "Collapse a neotree node."
        (interactive)
        (let ((node (neo-buffer--get-filename-current-line)))
          (when node
            (when (file-directory-p node)
              (neo-buffer--set-expand node nil)
              (neo-buffer--refresh t))
            (when neo-auto-indent-point
              (neo-point-auto-indent)))))

      (defun spacemacs//neotree-key-bindings ()
        "Set the key bindings for a neotree buffer."
        (define-key evil-motion-state-local-map (kbd "TAB") 'neotree-stretch-toggle)
        (define-key evil-motion-state-local-map (kbd "RET") 'neotree-enter)
        (define-key evil-motion-state-local-map (kbd "|")   'neotree-enter-vertical-split)
        (define-key evil-motion-state-local-map (kbd "-")   'neotree-enter-horizontal-split)
        (define-key evil-motion-state-local-map (kbd "?")   'evil-search-backward)
        (define-key evil-motion-state-local-map (kbd "c")   'neotree-create-node)
        (define-key evil-motion-state-local-map (kbd "d")   'neotree-delete-node)
        (define-key evil-motion-state-local-map (kbd "g")   'neotree-refresh)
        (define-key evil-motion-state-local-map (kbd "h")   'spacemacs/neotree-collapse)
        (define-key evil-motion-state-local-map (kbd "H")   'neotree-select-previous-sibling-node)
        (define-key evil-motion-state-local-map (kbd "J")   'neotree-select-down-node)
        (define-key evil-motion-state-local-map (kbd "K")   'neotree-select-up-node)
        (define-key evil-motion-state-local-map (kbd "l")   'spacemacs/neotree-expand-or-open)
        (define-key evil-motion-state-local-map (kbd "L")   'neotree-select-next-sibling-node)
        (define-key evil-motion-state-local-map (kbd "q")   'neotree-hide)
        (define-key evil-motion-state-local-map (kbd "r")   'neotree-rename-node)
        (define-key evil-motion-state-local-map (kbd "s")   'neotree-hidden-file-toggle))

      (evil-leader/set-key "ft" 'neotree-toggle))
    :config
    (add-hook 'neotree-mode-hook 'spacemacs//neotree-key-bindings)))

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
