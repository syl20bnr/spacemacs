;;; packages.el --- Spacemacs Defaults Layer packages File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq spacemacs-defaults-packages
      '(
        (abbrev :location built-in)
        (archive-mode :location built-in)
        (bookmark :location built-in)
        (conf-mode :location built-in)
        (dired :location built-in)
        (dired-x :location built-in)
        (electric-indent-mode :location built-in)
        (ediff :location built-in)
        (eldoc :location built-in)
        (help-fns+ :location local)
        (hi-lock :location built-in)
        (image-mode :location built-in)
        (imenu :location built-in)
        (linum :location built-in)
        (occur-mode :location built-in)
        (package-menu :location built-in)
        ;; page-break-lines is shipped with spacemacs core
        (page-break-lines :location built-in)
        (process-menu :location built-in)
        (recentf :location built-in)
        (savehist :location built-in)
        (saveplace :location built-in)
        (subword :location built-in)
        (tar-mode :location built-in)
        (uniquify :location built-in)
        (url :location built-in)
        (visual-line-mode :location built-in)
        (whitespace :location built-in)
        (winner :location built-in)
        (zone :location built-in)
        ))

;; Initialization of packages

(defun spacemacs-defaults/init-abbrev ()
  (spacemacs|hide-lighter abbrev-mode))

(defun spacemacs-defaults/init-archive-mode ()
  (evilified-state-evilify-map archive-mode-map
    :mode archive-mode
    :eval-after-load archive-mode))

(defun spacemacs-defaults/init-bookmark ()
  (use-package bookmark
    :defer t
    :init
    (progn
      (setq bookmark-default-file (concat spacemacs-cache-directory "bookmarks")
            ;; autosave each change
            bookmark-save-flag 1)
      (spacemacs/set-leader-keys "fb" 'bookmark-jump))))

(defun spacemacs-defaults/init-conf-mode ()
  :init
  ;; explicitly derive conf-mode from text-mode major-mode
  (add-hook 'conf-mode-hook 'spacemacs/run-text-mode-hooks))

(defun spacemacs-defaults/init-dired ()
  (spacemacs/set-leader-keys
    "ad" 'dired
    "fj" 'dired-jump
    "jd" 'dired-jump
    "jD" 'dired-jump-other-window))

(defun spacemacs-defaults/init-dired-x ()
  (use-package dired-x
    :commands (dired-jump
               dired-jump-other-window
               dired-omit-mode)))

(defun spacemacs-defaults/init-electric-indent-mode ()
  (electric-indent-mode))

(defun spacemacs-defaults/init-visual-line-mode ()
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
(defun spacemacs-defaults/init-ediff ()
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
      ;; show org ediffs unfolded
      (require 'outline)
      (add-hook 'ediff-prepare-buffer-hook #'show-all)
      ;; restore window layout when done
      (add-hook 'ediff-quit-hook #'winner-undo))))

(defun spacemacs-defaults/init-eldoc ()
  (use-package eldoc
    :defer (spacemacs/defer)
    :init (spacemacs|require 'eldoc)
    :config
    (progn
      ;; enable eldoc in `eval-expression'
      (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
      ;; enable eldoc in IELM
      (add-hook 'ielm-mode-hook #'eldoc-mode)
      ;; don't display eldoc on modeline
      (spacemacs|hide-lighter eldoc-mode))))

(defun spacemacs-defaults/init-help-fns+ ()
  (use-package help-fns+
    :commands (describe-keymap)
    :init (spacemacs/set-leader-keys "hdK" 'describe-keymap)))

(defun spacemacs-defaults/init-hi-lock ()
  (with-eval-after-load 'hi-lock
    (spacemacs|hide-lighter hi-lock-mode)))

(defun spacemacs-defaults/init-image-mode ()
  (use-package image-mode
    :init
    (progn
      (setq image-animate-loop t)
      (spacemacs/declare-prefix-for-mode 'image-mode "ma" "animate")
      (spacemacs/declare-prefix-for-mode 'image-mode "mg" "goto file")
      (spacemacs/declare-prefix-for-mode 'image-mode "mt" "transform/resize")
      (spacemacs/set-leader-keys-for-major-mode 'image-mode
        "aa" 'image-toggle-animation
        "a+" 'image-increase-speed
        "a-" 'image-decrease-speed
        "ar" 'image-reset-speed
        "gn" 'image-next-file
        "gN" 'image-previous-file
        "t+" 'image-increase-size
        "t-" 'image-decrease-size
        "tf" 'image-mode-fit-frame
        "tr" 'image-transform-reset
        "th" 'image-transform-fit-to-height
        "tw" 'image-transform-fit-to-width
        "ts" 'image-transform-set-scale
        "tr" 'image-transform-rotation))
    :config (evilified-state-evilify-map image-mode-map
              :mode image-mode
              :bindings
              "h" 'image-backward-hscroll
              "j" 'image-next-line
              "k" 'image-previous-line
              "l" 'image-forward-hscroll)))

(defun spacemacs-defaults/init-imenu ()
  (use-package imenu
    :defer t
    :init (spacemacs/set-leader-keys "ji" 'imenu)))

(defun spacemacs-defaults/init-linum ()
  (use-package linum
    :init
    (progn
      (setq linum-format "%4d")
      (spacemacs|add-toggle line-numbers
        :mode linum-mode
        :documentation "Show the line numbers."
        :evil-leader "tn")
      (advice-add #'linum-update-window
                  :after #'spacemacs//linum-update-window-scale-fix)
      (advice-add #'linum-on
                  :around #'spacemacs//linum-on))
    :config
    (progn
      (when (spacemacs//linum-backward-compabitility)
        (add-hook 'prog-mode-hook 'linum-mode)
        (add-hook 'text-mode-hook 'linum-mode))
      (when dotspacemacs-line-numbers
        (global-linum-mode)))))

(defun spacemacs-defaults/init-occur-mode ()
  (evilified-state-evilify-map occur-mode-map
    :mode occur-mode))

(defun spacemacs-defaults/init-package-menu ()
  (evilified-state-evilify-map package-menu-mode-map
    :mode package-menu-mode))

(defun spacemacs-defaults/init-page-break-lines ()
  (require 'page-break-lines)
  (global-page-break-lines-mode t)
  (spacemacs|hide-lighter page-break-lines-mode))

(defun spacemacs-defaults/init-process-menu ()
  (evilified-state-evilify process-menu-mode process-menu-mode-map))

(defun spacemacs-defaults/init-recentf ()
  (use-package recentf
    :defer (spacemacs/defer)
    :init
    (progn
      (spacemacs|require 'recentf)
      (when (spacemacs/defer)
        (add-hook 'find-file-hook (lambda () (unless recentf-mode
                                               (recentf-mode)
                                               (recentf-track-opened-file)))))
      (setq recentf-save-file (concat spacemacs-cache-directory "recentf")
            recentf-max-saved-items 1000
            recentf-auto-cleanup 'never
            recentf-auto-save-timer (run-with-idle-timer 600 t
                                                         'recentf-save-list)))
    :config
    (progn
      (add-to-list 'recentf-exclude
                   (recentf-expand-file-name spacemacs-cache-directory))
      (add-to-list 'recentf-exclude (recentf-expand-file-name package-user-dir))
      (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'"))))

(defun spacemacs-defaults/init-savehist ()
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

(defun spacemacs-defaults/init-saveplace ()
  (use-package saveplace
    :init
    (progn
      (if (fboundp 'save-place-mode)
          ;; Emacs 25 has a proper mode for `save-place'
          (save-place-mode)
        (setq save-place t))
      ;; Save point position between sessions
      (setq save-place-file (concat spacemacs-cache-directory "places")))))

(defun spacemacs-defaults/init-subword ()
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
        :mode subword-mode
        :documentation "Toggle CamelCase motions."
        :evil-leader "tc")
      (spacemacs|add-toggle camel-case-motion-globally
        :mode global-subword-mode
        :documentation "Globally toggle CamelCase motions."
        :evil-leader "t C-c"))
    :config
    (spacemacs|diminish subword-mode " ⓒ" " c")))

(defun spacemacs-defaults/init-tar-mode ()
  (evilified-state-evilify-map tar-mode-map
    :mode tar-mode
    :eval-after-load tar-mode))

(defun spacemacs-defaults/init-uniquify ()
  (require 'uniquify)
  ;; When having windows with repeated filenames, uniquify them
  ;; by the folder they are in rather those annoying <2>,<3>,.. etc
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets
        ;; don't screw special buffers
        uniquify-ignore-buffers-re "^\\*"))

(defun spacemacs-defaults/init-url ()
  ;; gravatars from magit use this to store their cache
  (setq url-configuration-directory (concat spacemacs-cache-directory "url/")))

(defun spacemacs-defaults/init-whitespace ()
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
        :mode whitespace-mode
        :documentation "Display whitespace."
        :evil-leader "tw")
      (spacemacs|add-toggle whitespace-globally
        :mode global-whitespace-mode
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
      (spacemacs|diminish global-whitespace-mode " ⓦ" " w"))))

(defun spacemacs-defaults/init-winner ()
  (use-package winner
    :defer t
    :init
    (with-eval-after-load 'winner
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
            (append winner-boring-buffers spacemacs/winner-boring-buffers)))))

(defun spacemacs-defaults/init-zone ()
  (require 'zone)
  (when (and dotspacemacs-zone-out-when-idle
             (numberp dotspacemacs-zone-out-when-idle))
    (zone-when-idle dotspacemacs-zone-out-when-idle))
  ;; remove not interesting programs
  (setq zone-programs [
                       ;; zone-pgm-jitter
                       zone-pgm-putz-with-case
                       zone-pgm-dissolve
                       ;; zone-pgm-explode
                       zone-pgm-whack-chars
                       zone-pgm-rotate
                       zone-pgm-rotate-LR-lockstep
                       zone-pgm-rotate-RL-lockstep
                       zone-pgm-rotate-LR-variable
                       zone-pgm-rotate-RL-variable
                       zone-pgm-drip
                       ;; zone-pgm-drip-fretfully
                       ;; zone-pgm-five-oclock-swan-dive
                       ;; zone-pgm-martini-swan-dive
                       zone-pgm-rat-race
                       zone-pgm-paragraph-spaz
                       ;; zone-pgm-stress
                       ;; zone-pgm-stress-destress
                       ;; zone-pgm-random-life
                       ])
  (spacemacs/set-leader-keys "TZ" 'zone))
