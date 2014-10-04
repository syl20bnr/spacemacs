;; Extensions are in emacs_paths/extensions

;; Pre extensions are loaded *before* the packages
(defvar spacemacs-pre-extensions
  '(
    use-package
    solarized-theme
    auto-highlight-symbol
    ))

;; Post extensions are loaded *after* the packages
(defvar spacemacs-post-extensions
  '(
    centered-cursor
    dos
    emoji-cheat-sheet
    evil-org-mode
    evil-plugins
    helm-rcirc
    nose
    o-blog
    pylookup
    spray
    ))

;; Initialize the extensions

(defun spacemacs/init-use-package ()
  (require 'use-package))

(defun spacemacs/init-auto-highlight-symbol ()
  (use-package auto-highlight-symbol
    :commands auto-highlight-symbol-mode
    :init
    (add-to-hooks 'auto-highlight-symbol-mode '(erlang-mode-hook
                                                prog-mode-hook
                                                org-mode-hook
                                                markdown-mode-hook))
    :config
    (progn
      (custom-set-variables
       '(ahs-case-fold-search nil)
       '(ahs-default-range (quote ahs-range-whole-buffer))
       '(ahs-idle-interval 0.25))
      (eval-after-load "evil-leader"
        '(evil-leader/set-key
           "he" 'ahs-edit-mode
           "hh" 'ahs-forward-definition
           "hn" 'ahs-forward
           "hN" 'ahs-backward
           "hR"  (lambda () (interactive) (eval '(ahs-change-range ahs-default-range) nil))
           "hrb" (lambda () (interactive) (eval '(ahs-change-range 'ahs-range-whole-buffer) nil))
           "hrd" (lambda () (interactive) (eval '(ahs-change-range 'ahs-range-display) nil))
           "hrf" (lambda () (interactive) (eval '(ahs-change-range 'ahs-range-beginning-of-defun) nil))
           "th" 'auto-highlight-symbol-mode))
      (spacemacs//diminish auto-highlight-symbol-mode " Ⓗ")
      ;; mini-mode to easily jump from a highlighted symbol to the others
      (dolist (sym '(ahs-forward
                     ahs-forward-definition
                     ahs-backward
                     ahs-backward-definition
                     ahs-back-to-start))
        (let* ((advice (intern (format "spacemacs/%s" (symbol-name sym)))))
          (eval `(defadvice ,sym (after ,advice activate)
                   (spacemacs/auto-highlight-symbol-overlay-map t)))))
      (defun spacemacs/auto-highlight-symbol-overlay-map (&optional display)
        "Set a temporary overlay map to easily jump from highlighted symbols to
 the nexts. If DISPLAY is true a documentation is displayed in the mini-buffer."
        (interactive)
        (set-temporary-overlay-map
         (let ((map (make-sparse-keymap)))
           (define-key map (kbd "c")
             '(lambda () (interactive)
                (eval '(ahs-change-range) nil)
                ;; we tolerate a recursive call here
                (spacemacs/auto-highlight-symbol-overlay-map)))
           (define-key map (kbd "d") 'ahs-forward-definition)
           (define-key map (kbd "D") 'ahs-backward-definition)
           (define-key map (kbd "e") 'ahs-edit-mode)
           (define-key map (kbd "n") 'ahs-forward)
           (define-key map (kbd "N") 'ahs-backward)
           (define-key map (kbd "h") 'ahs-back-to-start)
           map) nil)
        (let* ((i 0)
               (overlay-count (length ahs-overlay-list))
               (overlay (format "%s" (nth i ahs-overlay-list)))
               (current-overlay (format "%s" ahs-current-overlay)))
          (while (not (string= overlay current-overlay))
            (setq i (1+ i))
            (setq overlay (format "%s" (nth i ahs-overlay-list))))
          (unless (not display)
            (if (or (< (point) (window-start)) (> (point) (window-end)))
                ;; redisplay only if point leave the window
                ;; this way the next call to ahs-stat will compute up to
                ;; date values
                (redisplay))
            (let* ((st (ahs-stat))
                   (x/y (format "[%s/%s]" (- overlay-count i) overlay-count))
                   (propx/y (propertize x/y 'face ahs-plugin-whole-buffer-face))
                   (hidden (if (< 0 (- overlay-count (nth 4 st))) "*" ""))
                   (prophidden (propertize hidden 'face '(:weight bold))))
              (message "%s%s press (n) or (N) to navigate, (h) for home symbol, (c) to change scope"
                       propx/y prophidden))))))))

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
      (spacemacs//diminish centered-cursor-mode " Ⓒ"))))

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

(defun spacemacs/init-helm-rcirc ()
  (use-package helm-rcirc
    :commands helm-rcirc-auto-join-channels
    :init
    (evil-leader/set-key "irc" 'helm-rcirc-auto-join-channels)))

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
    :init
    (evil-leader/set-key-for-mode 'python-mode
      "mTf" 'nosetests-pdb-one
      "mtf" 'nosetests-one
      "mTa" 'nosetests-pdb-all
      "mta" 'nosetests-all
      "mTm" 'nosetests-pdb-module
      "mtm" 'nosetests-module
      "mTs" 'nosetests-pdb-suite
      "mts" 'nosetests-suite)
    :config
    (progn
      (add-to-list 'nose-project-root-files "setup.cfg")
      (setq nose-use-verbose nil)
      )))

(defun spacemacs/init-o-blog ()
  (use-package o-blog
    :defer t))

(defun spacemacs/init-pylookup ()
  (use-package pylookup
    :commands pylookup-lookup
    :config
    (progn
      (add-to-list 'evil-emacs-state-modes 'pylookup-mode)
      (evil-add-hjkl-bindings pylookup-mode-map 'emacs)
      (let* ((layer (assq 'spacemacs spacemacs-config-layers))
             (dir (plist-get (cdr layer) :ext-dir)))
        (setq pylookup-dir (concat dir "/pylookup")
              pylookup-program (concat pylookup-dir "/pylookup.py")
              pylookup-db-file (concat pylookup-dir "/pylookup.db"))))))

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

;; solarized theme dependencies
(unless (package-installed-p 'dash)
  (package-refresh-contents)
  (package-install 'dash))
(defun spacemacs/init-solarized-theme ()
  ;; different method used than the documented one in order to speed up the
  ;; loading of emacs
  (use-package solarized
    :init
    (progn
      (deftheme solarized-dark "The dark variant of the Solarized colour theme")
      (create-solarized-theme 'dark 'solarized-dark)
      (deftheme solarized-light "The light variant of the Solarized colour theme")
      (create-solarized-theme 'light 'solarized-light)
      (spacemacs/post-theme-init 'solarized-light)
      (redisplay))))
