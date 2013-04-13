(require 'cl)

;; Locations ==================================================================
(defvar user-home-directory
  (expand-file-name (concat user-emacs-directory "../"))
  "Emacs home directory.")

(defvar user-org-directory
  (expand-file-name (concat user-emacs-directory "my-org/"))
  "Org files directory.")

(defvar user-config-directory
  (expand-file-name (concat user-emacs-directory "config/"))
  "Configuration scripts.")

(defvar user-extensions-directory
  (expand-file-name (concat user-emacs-directory "extensions/"))
  "Additional extensions.")

(defvar user-init-extension-directory
  (expand-file-name (concat user-emacs-directory "init-extension/"))
  "Extension initialization.")

(defvar user-init-package-directory
  (expand-file-name (concat user-emacs-directory "init-package/"))
  "Package initialization.")

(defvar host-directory
  (expand-file-name (concat user-emacs-directory "host/" system-name "/"))
  "Host specific configurations")

(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path user-extensions-directory)

;; Emacs built-ins configuration ==============================================
(progn (when (file-exists-p user-config-directory)
    (dolist (l (directory-files user-config-directory nil "^[^#].*el$"))
      (load (concat user-config-directory l)))))

;; Setup ======================================================================
(require 'my-funcs)
(require 'my-macros)
(require 'pre-extensions)
(require 'packages)
(require 'post-extensions)
(require 'my-keybindings)
(require 'my-keychords)

;; Host specific configuration ================================================
(progn (when (file-exists-p host-directory)
    (dolist (l (directory-files host-directory nil "^[^#].*el$"))
      (load (concat host-directory l)))))

;; Customization settings =====================================================
(require 'custom-settings)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ahs-case-fold-search nil)
 '(ahs-default-range (quote ahs-range-whole-buffer))
 '(ahs-idle-interval 0.01)
 '(expand-region-contract-fast-key "V")
 '(expand-region-reset-fast-key "r")
 '(haskell-notify-p t)
 '(haskell-process-type (quote cabal-dev))
 '(haskell-stylish-on-save t)
 '(haskell-tags-on-save t)
 '(safe-local-variable-values (quote ((eval when (and (buffer-file-name) (file-regular-p (buffer-file-name)) (string-match-p "^[^.]" (buffer-file-name))) (emacs-lisp-mode) (unless (featurep (quote package-build)) (let ((load-path (cons ".." load-path))) (require (quote package-build)))) (package-build-minor-mode)))))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#93a1a1"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(popup-face ((t (:background "#586e75" :foreground "#000000"))))
 '(popup-menu-face ((t (:background "#586e75" :foreground "#000000"))))
 '(popup-menu-mouse-face ((t (:background "#073642" :foreground "#dc322f" :box (:line-width -1 :style pressed-button)))))
 '(popup-menu-selection-face ((t (:background "#073642" :foreground "#b58900" :box (:line-width -1 :style pressed-button) :overline "black" :weight bold))))
 '(popup-scroll-bar-background-face ((t (:background "#586e75"))))
 '(popup-scroll-bar-foreground-face ((t (:background "#002b36"))))
 '(popup-tip-face ((t (:background "#586e75" :foreground "#000000" :slant italic)))))
