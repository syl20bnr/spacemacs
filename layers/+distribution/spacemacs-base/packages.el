;;; packages.el --- Spacemacs Base Layer packages File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq spacemacs-base-packages
      '(
        (abbrev :location built-in)
        (bookmark :location built-in)
        (dired :location built-in)
        (dired-x :location built-in)
        (electric-indent-mode :location built-in)
        (ediff :location built-in)
        (eldoc :location built-in)
        evil-escape
        (evil-evilified-state :location local :step pre :protected t)
        evil-visualstar
        exec-path-from-shell
        help-fns+
        (hi-lock :location built-in)
        (holy-mode :location local :step pre)
        (hybrid-mode :location local :step pre)
        (package-menu :location built-in)
        (process-menu :location built-in)
        (recentf :location built-in)
        (savehist :location built-in)
        (saveplace :location built-in)
        spacemacs-theme
        (subword :location built-in)
        (uniquify :location built-in)
        (url :location built-in)
        (visual-line-mode :location built-in)
        (whitespace :location built-in)
        (winner :location built-in)
        ))

;; Initialization of packages

(defun spacemacs-base/init-abbrev ()
  (spacemacs|hide-lighter abbrev-mode))

(defun spacemacs-base/init-bookmark ()
  (use-package bookmark
    :defer t
    :init
    (progn
      (setq bookmark-default-file (concat spacemacs-cache-directory "bookmarks")
            ;; autosave each change
            bookmark-save-flag 1)
      (spacemacs/set-leader-keys "fb" 'bookmark-jump))))

(defun spacemacs-base/init-dired ()
  (spacemacs/set-leader-keys
    "ad" 'dired
    "fj" 'dired-jump
    "jd" 'dired-jump
    "jD" 'dired-jump-other-window))

(defun spacemacs-base/init-dired-x ()
  (use-package dired-x
    :commands (dired-jump
               dired-jump-other-window
               dired-omit-mode)))

(defun spacemacs-base/init-electric-indent-mode ()
  (electric-indent-mode))

(defun spacemacs-base/init-visual-line-mode ()
  (spacemacs|diminish visual-line-mode " Ⓛ" " L"))

;; notes from mijoharas
;; We currently just set a few variables to make it look nicer.
;; Here is my first attempt at evilifying the buffer, does not work correctly, help is very much welcome.

;; ```
;; (defun ediff/setup-ediff-keymaps ()
;;   "setup the evil ediff keymap"
;;     (progn
;;      (add-to-list 'evil-emacs-state-modes 'Ediff)
;;      (evilified-state-evilify ediff-mode-map)
;;      (spacemacs/activate-evil-leader-for-map 'ediff-mode-map)
;;       )
;;   )

;; ;; inside the use-package function
;; (add-hook 'ediff-keymap-setup-hook 'ediff/setup-ediff-keymaps)
;; ```
(defun spacemacs-base/init-ediff ()
  (use-package ediff
    :defer t
    :init
    (progn
      ;; first we set some sane defaults
      (setq-default
       ediff-window-setup-function 'ediff-setup-windows-plain
       ;; emacs is evil and decrees that vertical shall henceforth be horizontal
       ediff-split-window-function 'split-window-horizontally
       ediff-merge-split-window-function 'split-window-horizontally)
      ;; restore window layout when done
      (add-hook 'ediff-quit-hook #'winner-undo))))

(defun spacemacs-base/init-eldoc ()
  (use-package eldoc
    :defer t
    :config
    (progn
      ;; enable eldoc in `eval-expression'
      (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
      ;; enable eldoc in IELM
      (add-hook 'ielm-mode-hook #'eldoc-mode)
      ;; don't display eldoc on modeline
      (spacemacs|hide-lighter eldoc-mode))))

(defun spacemacs-base/init-evil-escape ()
  (use-package evil-escape
    :init (evil-escape-mode)
    :config (spacemacs|hide-lighter evil-escape-mode)))

(defun spacemacs-base/init-evil-evilified-state ()
  (use-package evil-evilified-state)
  (define-key evil-evilified-state-map (kbd dotspacemacs-leader-key)
    spacemacs-default-map))

(defun spacemacs-base/init-evil-visualstar ()
  (use-package evil-visualstar
    :commands (evil-visualstar/begin-search-forward
               evil-visualstar/begin-search-backward)
    :init
    (progn
      (define-key evil-visual-state-map (kbd "*")
        'evil-visualstar/begin-search-forward)
      (define-key evil-visual-state-map (kbd "#")
        'evil-visualstar/begin-search-backward))))

(defun spacemacs-base/init-exec-path-from-shell ()
  (use-package exec-path-from-shell
    :init (when (memq window-system '(mac ns x))
            (exec-path-from-shell-initialize))))

(defun spacemacs-base/init-help-fns+ ()
  (use-package help-fns+
    :commands (describe-keymap)
    :init (spacemacs/set-leader-keys "hdK" 'describe-keymap)))

(defun spacemacs-base/init-hi-lock ()
  (spacemacs|hide-lighter hi-lock-mode))

(defun spacemacs-base/init-holy-mode ()
  (use-package holy-mode
    :commands holy-mode
    :init
    (progn
      (when (eq 'emacs dotspacemacs-editing-style)
        (holy-mode))
      (spacemacs|add-toggle holy-mode
        :status holy-mode
        :on (progn (when (bound-and-true-p hybrid-mode)
                     (hybrid-mode -1))
                   (holy-mode))
        :off (holy-mode -1)
        :documentation "Globally toggle holy mode."
        :evil-leader "tEe")
      (spacemacs|diminish holy-mode " Ⓔe" " Ee"))))

(defun spacemacs-base/init-hybrid-mode ()
  (use-package hybrid-mode
    :config
    (progn
      (when (eq 'hybrid dotspacemacs-editing-style) (hybrid-mode))
      (spacemacs|add-toggle hybrid-mode
        :status hybrid-mode
        :on (progn (when (bound-and-true-p holy-mode)
                     (holy-mode -1))
                   (hybrid-mode))
        :off (hybrid-mode -1)
        :documentation "Globally toggle hybrid mode."
        :evil-leader "tEh")
      (spacemacs|diminish hybrid-mode " Ⓔh" " Eh"))))

(defun spacemacs-base/init-package-menu ()
  (evilified-state-evilify-map package-menu-mode-map
    :mode package-menu-mode))

(defun spacemacs-base/init-process-menu ()
  (evilified-state-evilify process-menu-mode process-menu-mode-map))

(defun spacemacs-base/init-recentf ()
  (use-package recentf
    :defer t
    :init
    (progn
      ;; lazy load recentf
      (add-hook 'find-file-hook (lambda () (unless recentf-mode
                                             (recentf-mode)
                                             (recentf-track-opened-file))))
      (setq recentf-save-file (concat spacemacs-cache-directory "recentf")
            recentf-max-saved-items 1000
            recentf-auto-cleanup 'never
            recentf-auto-save-timer (run-with-idle-timer 600 t
                                                         'recentf-save-list)))
    :config
    (progn
      (add-to-list 'recentf-exclude
                   (expand-file-name spacemacs-cache-directory))
      (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
      (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'"))))

(defun spacemacs-base/init-savehist ()
  (use-package savehist
    :init
    (progn
      ;; Minibuffer history
      (setq savehist-file (concat spacemacs-cache-directory "savehist")
            enable-recursive-minibuffers t ; Allow commands in minibuffers
            history-length 1000
            savehist-additional-variables '(mark-ring
                                            global-mark-ring
                                            search-ring
                                            regexp-search-ring
                                            extended-command-history)
            savehist-autosave-interval 60)
      (savehist-mode t))))

(defun spacemacs-base/init-saveplace ()
  (use-package saveplace
    :init
    (progn
      (if (fboundp 'save-place-mode)
          ;; Emacs 25 has a proper mode for `save-place'
          (save-place-mode)
        (setq save-place t))
      ;; Save point position between sessions
      (setq save-place-file (concat spacemacs-cache-directory "places")))))

(defun spacemacs-base/init-spacemacs-theme ()
  (use-package spacemacs-theme
    :defer t
    :init
    (progn
      (setq spacemacs-theme-comment-bg t)
      (setq spacemacs-theme-org-height t))))

(defun spacemacs-base/init-subword ()
  (unless (version< emacs-version "24.4")
    (use-package subword
      :defer t
      :init
      (progn
        (unless (category-docstring ?U)
          (define-category ?U "Uppercase")
          (define-category ?u "Lowercase"))
        (modify-category-entry (cons ?A ?Z) ?U)
        (modify-category-entry (cons ?a ?z) ?u)
        (make-variable-buffer-local 'evil-cjk-word-separating-categories)
        (defun spacemacs//subword-enable-camel-case ()
          "Add support for camel case to subword."
          (if subword-mode
              (push '(?u . ?U) evil-cjk-word-separating-categories)
            (setq evil-cjk-word-separating-categories
                  (default-value 'evil-cjk-word-separating-categories))))
        (add-hook 'subword-mode-hook 'spacemacs//subword-enable-camel-case)
        (spacemacs|add-toggle camel-case-motion
          :status subword-mode
          :on (subword-mode +1)
          :off (subword-mode -1)
          :documentation "Toggle CamelCase motions."
          :evil-leader "tc")
        (spacemacs|add-toggle camel-case-motion-globally
          :status subword-mode
          :on (global-subword-mode +1)
          :off (global-subword-mode -1)
          :documentation "Globally toggle CamelCase motions."
          :evil-leader "t C-c"))
      :config
      (spacemacs|diminish subword-mode " ⓒ" " c"))))

(defun spacemacs-base/init-uniquify ()
  (require 'uniquify)
  ;; When having windows with repeated filenames, uniquify them
  ;; by the folder they are in rather those annoying <2>,<3>,.. etc
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        ;; don't screw special buffers
        uniquify-ignore-buffers-re "^\\*"))

(defun spacemacs-base/init-url ()
  ;; gravatars from magit use this to store their cache
  (setq url-configuration-directory (concat spacemacs-cache-directory "url/")))

(defun spacemacs-base/init-whitespace ()
  (use-package whitespace
    :defer t
    :init
    (progn
      (setq spacemacs-show-trailing-whitespace t)
      (defun spacemacs//show-trailing-whitespace ()
        (when spacemacs-show-trailing-whitespace
          (set-face-attribute 'trailing-whitespace nil
                              :background
                              (face-attribute 'font-lock-comment-face
                                              :foreground))
          (setq show-trailing-whitespace 1)))
      (add-hook 'prog-mode-hook 'spacemacs//show-trailing-whitespace)

      (spacemacs|add-toggle whitespace
        :status whitespace-mode
        :on (whitespace-mode)
        :off (whitespace-mode -1)
        :documentation "Display whitespace."
        :evil-leader "tw")
      (spacemacs|add-toggle whitespace-globally
        :status global-whitespace-mode
        :on (global-whitespace-mode)
        :off (global-whitespace-mode -1)
        :documentation "Display whitespace globally."
        :evil-leader "t C-w")

      (defun spacemacs//set-whitespace-style-for-diff ()
        "Whitespace configuration for `diff-mode'"
        (setq-local whitespace-style '(face
                                       tabs
                                       tab-mark
                                       spaces
                                       space-mark
                                       trailing
                                       indentation::space
                                       indentation::tab
                                       newline
                                       newline-mark)))
      (add-hook 'diff-mode-hook 'whitespace-mode)
      (add-hook 'diff-mode-hook 'spacemacs//set-whitespace-style-for-diff))
    :config
    (progn
      (set-face-attribute 'whitespace-space nil
                          :background nil
                          :foreground (face-attribute 'font-lock-warning-face
                                                      :foreground))
      (set-face-attribute 'whitespace-tab nil
                          :background nil)
      (set-face-attribute 'whitespace-indentation nil
                          :background nil)
      (spacemacs|diminish whitespace-mode " ⓦ" " w")
      (spacemacs|diminish global-whitespace-mode " Ⓦ" " W"))))

(defun spacemacs-base/init-winner ()
  (use-package winner
    :init
    (progn
      (winner-mode t)
      (setq spacemacs/winner-boring-buffers '("*Completions*"
                                              "*Compile-Log*"
                                              "*inferior-lisp*"
                                              "*Fuzzy Completions*"
                                              "*Apropos*"
                                              "*Help*"
                                              "*cvs*"
                                              "*Buffer List*"
                                              "*Ibuffer*"
                                              "*esh command on file*"
                                              ))
      (setq winner-boring-buffers
            (append winner-boring-buffers spacemacs/winner-boring-buffers))
      (winner-mode t))))
