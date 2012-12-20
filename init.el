(require 'cl)

;; Locations ===================================================================
(defvar user-home-directory
  (expand-file-name (concat user-emacs-directory "../"))
  "The user's home directory.")

(defvar user-custom-modes-dir
  (expand-file-name (concat user-emacs-directory "custom-modes/"))
  "The directory containing the user's custom modes.")

(add-to-list 'load-path user-emacs-directory)

;; Config files ================================================================
(progn
  (setq user-emacs-config-dir (concat user-emacs-directory "config/"))
  (when (file-exists-p user-emacs-config-dir)
    (dolist (l (directory-files user-emacs-config-dir nil "^[^#].*el$"))
      (load (concat user-emacs-config-dir l)))))

(require 'my-funcs)
(require 'my-packages)
(require 'my-keybindings)
(require 'my-keybindings-work)

;; Python ======================================================================
;; clone git repository https://github.com/gabrielelanaro/emacs-for-python
;; =============================================================================
(setq ropemacs-global-prefix "C-x /") ;; avoid conflict with p4 global prefix
(load-file (concat user-emacs-directory "emacs-for-python/epy-init.el"))
(require 'epy-setup)
(require 'epy-python)
(require 'epy-completion)
(require 'epy-editing)
(require 'epy-bindings)
(require 'epy-nose)
(epy-setup-checker "pyflakes %f")
(epy-setup-ipython)
;; line hightlighting
(global-hl-line-mode t)
(set-face-background 'hl-line "#073642")
;; identation highlighting
;; (require 'highlight-indentation)
;; (add-hook 'python-mode-hook 'highlight-indentation)
;; disable auto-pairing
;;(setq skeleton-pair nil)
(add-hook 'python-mode-hook (lambda ()
                              (local-set-key "\C-c\C-c" 'syl-python-compile)))

;; Custom modes ================================================================
(load-file (concat user-custom-modes-dir "heartbeat-cursor.el"))
