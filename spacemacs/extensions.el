;; Extensions are in emacs_paths/extensions

;; Pre extensions are loaded *before* the packages
(defvar spacemacs-pre-extensions
  '(
    use-package
    ))

;; Pre extensions are loaded *after* the packages
(defvar spacemacs-post-extensions
  '(
    centered-cursor
    dos
    emoji-cheat-sheet
    evil-org-mode
    evil-plugins
    nose
    o-blog
    pylookup
    solarized-theme
    ))

;; Initialize the extensions

(defun spacemacs/init-use-package ()
  (require 'use-package))

(defun spacemacs/init-centered-cursor ()
  (use-package centered-cursor-mode
    :commands global-centered-cursor-mode
    :init
    (evil-leader/set-key "zz" 'global-centered-cursor-mode)
    :config
    (custom-set-variables
     '(ccm-recenter-at-end-of-file t)
     '(ccm-ignored-commands (quote (mouse-drag-region
                                    mouse-set-point
                                    widget-button-click
                                    scroll-bar-toolkit-scroll
                                    evil-mouse-drag-region))))))

(defun spacemacs/init-dos ()
  (use-package dos
    :mode ("\\.bat$" . dos-mode)))

(defun spacemacs/init-emoji-cheat-sheet ()
  (use-package emoji-cheat-sheet
    :commands emoji-cheat-sheet))

(defun spacemacs/init-evil-org-mode ()
  (use-package evil-org
    :commands evil-org-mode
    :init (add-hook 'org-mode-hook 'evil-org-mode)))

(defun spacemacs/init-evil-plugins ()
  (use-package evil-little-word)
  (use-package evil-operator-comment
    :init
    (global-evil-operator-comment-mode 1)))

(defun spacemacs/init-nose ()
  (use-package nose
    :commands (nosetests-one
               nosetests-pdb-one
               nosetests-all
               nosetests-pdb-all
               nosetests-module
               nosetests-pdb-module
               nosetests-suite
               nosetests-pdb-suite)
    :config
    (progn
      (add-to-list 'nose-project-root-files "setup.cfg")
      (setq nose-use-verbose nil))))

(defun spacemacs/init-o-blog ()
  (use-package o-blog
    :defer t))

(defun spacemacs/init-pylookup ()
  (use-package pylookup
    :commands pylookup-lookup
    :config
    (progn
      (setq pylookup-dir (concat spacemacs-extensions-directory "/pylookup"))
      ;; set executable file and db file
      (setq pylookup-program (concat pylookup-dir "/pylookup.py"))
      (setq pylookup-db-file (concat pylookup-dir "/pylookup.db")))))

(defun spacemacs/init-solarized-theme ()
  ;; different method used than the documented one in order to speed up the
  ;; loading of emacs
  (use-package solarized
    :init
    (progn 
      (deftheme solarized-light "The light variant of the Solarized colour theme")
      (create-solarized-theme 'light 'solarized-light)
      (deftheme solarized-dark "The dark variant of the Solarized colour theme")
      (create-solarized-theme 'dark 'solarized-dark)
      (spacemacs/set-flycheck-custom-face)
      )))
