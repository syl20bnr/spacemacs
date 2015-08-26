;;; packages.el --- Spacemacs Layer packages File
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

(setq spacemacs-packages
      '(
        ;; pre
        (holy-mode :location local :step pre)
        ;; default
        ace-link
        ace-window
        adaptive-wrap
        aggressive-indent
        auto-dictionary
        auto-highlight-symbol
        avy
        bind-key
        bookmark
        buffer-move
        (centered-cursor :location local)
        clean-aindent-mode
        define-word
        desktop
        diminish
        doc-view
        ediff
        eldoc
        ;; hack to be able to wrap built-in emacs modes in an init function
        (emacs-builtin-process-menu :location local)
        eval-sexp-fu
        evil
        evil-anzu
        evil-args
        evil-escape
        evil-exchange
        evil-iedit-state
        (evil-indent-textobject :location (recipe :fetcher github :repo "TheBB/evil-indent-textobject"))
        evil-jumper
        evil-leader
        evil-lisp-state
        evil-nerd-commenter
        evil-matchit
        evil-numbers
        evil-search-highlight-persist
        evil-surround
        evil-terminal-cursor-changer
        evil-tutor
        evil-visualstar
        exec-path-from-shell
        expand-region
        fancy-battery
        fill-column-indicator
        flx-ido
        fringe-helper
        gh-md
        golden-ratio
        google-translate
        ;; guide-key-tip
        helm
        helm-ag
        helm-descbinds
        helm-make
        helm-mode-manager
        ;; not working for now
        ;; helm-proc
        helm-projectile
        (helm-spacemacs :location local)
        helm-swoop
        helm-themes
        highlight-indentation
        highlight-numbers
        highlight-parentheses
        ;; waiting for an overlay bug to be fixed
        ;; see https://github.com/syl20bnr/spacemacs/issues/2529
        (hl-anything :excluded t)
        hungry-delete
        (hybrid-mode :location local)
        ido-vertical-mode
        info+
        iedit
        indent-guide
        open-junk-file
        leuven-theme
        linum-relative
        move-text
        neotree
        page-break-lines
        pcre2el
        popup
        popwin
        powerline
        projectile
        quelpa
        rainbow-delimiters
        recentf
        rfringe
        savehist
        saveplace
        smartparens
        smooth-scrolling
        (solarized-theme :location local)
        spacemacs-theme
        spray
        subword
        undo-tree
        use-package
        vi-tilde-fringe
        volatile-highlights
        whitespace
        which-key
        window-numbering
        winner
        (zoom-frm :location local)
        ))
;; Paradox from MELPA is not compatible with 24.3, so we use
;; a local paradox with 24.3
(if  (version< emacs-version "24.4")
    (push '(paradox :location local) spacemacs-packages)
  (push 'paradox spacemacs-packages))

;; Initialization of packages

(defun spacemacs/init-ace-link ()
  (use-package ace-link
    :commands spacemacs/ace-buffer-links
    :init
    (progn
      (define-key spacemacs-mode-map "o" 'spacemacs/ace-buffer-links)
      (eval-after-load "info"
        '(define-key Info-mode-map "o" 'ace-link-info))
      (eval-after-load "help-mode"
        '(define-key help-mode-map "o" 'ace-link-help))
      (eval-after-load "eww"
        '(progn
           (define-key eww-link-keymap "o" 'ace-link-eww)
           (define-key eww-mode-map "o" 'ace-link-eww))))
    :config
    (progn
      (defvar spacemacs--link-pattern "~?/.+\\|\s\\[")
      (defun spacemacs//collect-spacemacs-buffer-links ()
        (let ((end (window-end))
              points)
          (save-excursion
            (goto-char (window-start))
            (while (re-search-forward spacemacs--link-pattern end t)
              (push (+ (match-beginning 0) 1) points))
            (nreverse points))))
      (defun spacemacs/ace-buffer-links ()
        "Ace jump to links in `spacemacs' buffer."
        (interactive)
        (let ((res (avy--with-avy-keys spacemacs/ace-buffer-links
                    (avy--process
                        (spacemacs//collect-spacemacs-buffer-links)
                        #'avy--overlay-pre))))
            (when res
            (goto-char (1+ res))
            (widget-button-press (point))))))))


(defun spacemacs/init-ace-window ()
  (use-package ace-window
    :defer t
    :init
    (progn
      (evil-leader/set-key
        "bM"  'ace-swap-window
        "wC"  'ace-delete-window
        "w <SPC>"  'ace-window)
      ;; set ace-window keys to home-row
      (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))))

(defun spacemacs/init-adaptive-wrap ()
  (use-package adaptive-wrap
    :config
    (progn
      (add-hook 'visual-line-mode-hook 'adaptive-wrap-prefix-mode))))

(defun spacemacs/init-aggressive-indent ()
  (use-package aggressive-indent
    :defer t
    :init
    (progn
      (spacemacs|add-toggle aggressive-indent
        :status aggressive-indent-mode
        :on (aggressive-indent-mode)
        :off (aggressive-indent-mode -1)
        :documentation "Keep code always indented."
        :evil-leader "tI")
      (spacemacs|add-toggle aggressive-indent-globally
        :status aggressive-indent-mode
        :on (global-aggressive-indent-mode)
        :off (global-aggressive-indent-mode -1)
        :documentation "Globally keep code always indented."
        :evil-leader "t C-I"))
    :config
    (progn
      (add-hook 'diff-auto-refine-mode-hook 'spacemacs/toggle-aggressive-indent-off)
      (spacemacs|diminish aggressive-indent-mode " Ⓘ" " I"))))

(defun spacemacs/init-auto-dictionary ()
  (use-package auto-dictionary
    :disabled t
    :defer t
    :init
    (progn
      (add-hook 'flyspell-mode-hook '(lambda () (auto-dictionary-mode 1)))
      (evil-leader/set-key
        "Sd" 'adict-change-dictionary))))

(defun spacemacs/init-auto-highlight-symbol ()
  (use-package auto-highlight-symbol
    :defer t
    :init
    (spacemacs/add-to-hooks 'auto-highlight-symbol-mode '(prog-mode-hook
                                                          markdown-mode-hook))
    :config
    (progn
      (setq ahs-case-fold-search nil
            ahs-default-range 'ahs-range-whole-buffer
            ;; disable auto-highlight of symbol
            ;; current symbol should be highlight on demand with <SPC> s h
            ahs-idle-timer 0
            ahs-idle-interval 0.25
            ahs-inhibit-face-list nil)

      (defvar spacemacs-last-ahs-highlight-p nil
        "Info on the last searched highlighted symbol.")
      (make-variable-buffer-local 'spacemacs-last-ahs-highlight-p)

      (defun spacemacs/goto-last-searched-ahs-symbol ()
        "Go to the last known occurrence of the last symbol searched with
`auto-highlight-symbol'."
        (interactive)
        (if spacemacs-last-ahs-highlight-p
            (progn (goto-char (nth 1 spacemacs-last-ahs-highlight-p))
                   (eval '(progn (spacemacs/ahs-highlight-now-wrapper) (ahs-back-to-start)) nil))
          (message "No symbol has been searched for now.")))

      (defun spacemacs/integrate-evil-search (forward)
        ;; isearch-string is last searched item.  Next time
        ;; "n" is hit we will use this.
        (setq isearch-string (concat "\\<" (evil-find-thing forward 'symbol) "\\>"))
        (setq isearch-regexp (concat "\\<" (evil-find-thing forward 'symbol) "\\>"))
        ;; Next time "n" is hit, go the correct direction.
        (setq isearch-forward forward)
        ;; ahs does a case sensitive search.  We could set
        ;; this, but it would break the user's current
        ;; sensitivity settings.  We could save the setting,
        ;; then next time the user starts a search we could
        ;; restore the setting.
        ;;(setq case-fold-search nil)
        ;; Place the search term into the search rings.
        (isearch-update-ring isearch-string t)
        (evil-push-search-history isearch-string forward)
        ;; Use this search term for empty pattern "%s//replacement/"
        ;; Append case sensitivity
        (setq evil-ex-last-was-search nil
              evil-ex-substitute-pattern `(,(concat isearch-string "\\C") nil (0 0)))
        )

      (defun spacemacs/ensure-ahs-enabled-locally ()
        "Ensures ahs is enabled for the local buffer."
        (unless
            (bound-and-true-p ahs-mode-line)
          (auto-highlight-symbol-mode)
          ))

      (defun spacemacs/ahs-highlight-now-wrapper ()
        "Safe wrapper for ahs-highlight-now"
        (eval '(progn
                 (spacemacs/ensure-ahs-enabled-locally)
                 (ahs-highlight-now)) nil))

      (defun spacemacs/quick-ahs-forward ()
        "Go to the next occurrence of symbol under point with
`auto-highlight-symbol'"
        (interactive)
        (spacemacs/integrate-evil-search t)
        (spacemacs/ahs-highlight-now-wrapper)
        (when (configuration-layer/package-usedp 'evil-jumper)
          (evil-set-jump))
        (spacemacs/highlight-symbol-micro-state)
        (ahs-forward))

      (defun spacemacs/quick-ahs-backward ()
        "Go to the previous occurrence of symbol under point with
`auto-highlight-symbol'"
        (interactive)
        (spacemacs/integrate-evil-search nil)
        (spacemacs/ahs-highlight-now-wrapper)
        (when (configuration-layer/package-usedp 'evil-jumper)
          (evil-set-jump))
        (spacemacs/highlight-symbol-micro-state)
        (ahs-backward))

      (eval-after-load 'evil
        '(progn
           (define-key evil-motion-state-map (kbd "*") 'spacemacs/quick-ahs-forward)
           (define-key evil-motion-state-map (kbd "#") 'spacemacs/quick-ahs-backward)))

      (defun spacemacs/symbol-highlight ()
        "Highlight the symbol under point with `auto-highlight-symbol'."
        (interactive)
        (spacemacs/ahs-highlight-now-wrapper)
        (setq spacemacs-last-ahs-highlight-p (ahs-highlight-p))
        (spacemacs/highlight-symbol-micro-state)
        (spacemacs/integrate-evil-search nil))

      (defun spacemacs/symbol-highlight-reset-range ()
        "Reset the range for `auto-highlight-symbol'."
        (interactive)
        (ahs-change-range ahs-default-range))

      (evil-leader/set-key
        "sh" 'spacemacs/symbol-highlight
        "sH" 'spacemacs/goto-last-searched-ahs-symbol
        "sR" 'spacemacs/symbol-highlight-reset-range)

      (spacemacs|hide-lighter auto-highlight-symbol-mode)

      ;; micro-state to easily jump from a highlighted symbol to the others
      (dolist (sym '(ahs-forward
                     ahs-forward-definition
                     ahs-backward
                     ahs-backward-definition
                     ahs-back-to-start
                     ahs-change-range))
        (let* ((advice (intern (format "spacemacs/%s" (symbol-name sym)))))
          (eval `(defadvice ,sym (around ,advice activate)
                   (spacemacs/ahs-highlight-now-wrapper)
                   ad-do-it
                   (spacemacs/ahs-highlight-now-wrapper)
                   (setq spacemacs-last-ahs-highlight-p (ahs-highlight-p))))))

      (spacemacs|define-micro-state highlight-symbol
        :doc (let* ((i 0)
                    (overlay-count (length ahs-overlay-list))
                    (overlay (format "%s" (nth i ahs-overlay-list)))
                    (current-overlay (format "%s" ahs-current-overlay))
                    (st (ahs-stat))
                    (plighter (ahs-current-plugin-prop 'lighter))
                    (plugin (format " <%s> " (cond ((string= plighter "HS") "D")
                                                   ((string= plighter "HSA") "B")
                                                   ((string= plighter "HSD") "F"))))
                    (propplugin (propertize plugin 'face
                                            `(:foreground "#ffffff"
                                                          :background ,(face-attribute
                                                                        'ahs-plugin-defalt-face :foreground)))))
               (while (not (string= overlay current-overlay))
                 (setq i (1+ i))
                 (setq overlay (format "%s" (nth i ahs-overlay-list))))
               (let* ((x/y (format "(%s/%s)" (- overlay-count i) overlay-count))
                      (propx/y (propertize x/y 'face ahs-plugin-whole-buffer-face))
                      (hidden (if (< 0 (- overlay-count (nth 4 st))) "*" ""))
                      (prophidden (propertize hidden 'face '(:weight bold))))
                 (format "%s %s%s [n/N] move [e] edit [r] range [R] reset [d/D] definition [/] find in project [f] find in files [b] find in opened buffers [q] exit"
                         propplugin propx/y prophidden)))
        :bindings
        ("d" ahs-forward-definition)
        ("D" ahs-backward-definition)
        ("e" nil
         :post (if (configuration-layer/package-usedp 'evil-iedit-state)
                   (evil-iedit-state/iedit-mode)
                 (ahs-edit-mode))
         :exit t)
        ("n" spacemacs/quick-ahs-forward)
        ("N" spacemacs/quick-ahs-backward)
        ("R" ahs-back-to-start)
        ("r" ahs-change-range)
        ("/" spacemacs/helm-project-smart-do-search-region-or-symbol :exit t)
        ("b" spacemacs/helm-buffers-smart-do-search-region-or-symbol :exit t)
        ("f" spacemacs/helm-files-smart-do-search-region-or-symbol :exit t)
        ("q" nil :exit t)))))

(defun spacemacs/init-avy ()
  (use-package avy
    :defer t
    :init
    (progn
      (setq avy-keys (number-sequence ?a ?z))
      (setq avy-all-windows 'all-frames)
      (setq avy-background t)
      (evil-leader/set-key
        "SPC" 'avy-goto-word-or-subword-1
        "l" 'avy-goto-line))
    :config
    (evil-leader/set-key "`" 'avy-pop-mark)))

(defun spacemacs/init-bind-key ())

(defun spacemacs/init-bookmark ()
  (use-package bookmark
    :defer t
    :init
    (setq bookmark-default-file (concat spacemacs-cache-directory "bookmarks")
          ;; autosave each change
          bookmark-save-flag 1)))

(defun spacemacs/init-buffer-move ()
  (use-package buffer-move
    :defer t
    :init
    (evil-leader/set-key
      "bmh" 'buf-move-left
      "bmj" 'buf-move-down
      "bmk" 'buf-move-up
      "bml" 'buf-move-right)))

(defun spacemacs/init-centered-cursor ()
  (use-package centered-cursor-mode
    :commands (centered-cursor-mode
               global-centered-cursor-mode)
    :init
    (progn
      (spacemacs|add-toggle centered-point
        :status centered-cursor-mode
        :on (centered-cursor-mode)
        :off (centered-cursor-mode -1)
        :documentation
        "Keep point always at the center of the window."
        :evil-leader "t-")
      (spacemacs|add-toggle centered-point-globally
        :status centered-cursor-mode
        :on (global-centered-cursor-mode)
        :off (global-centered-cursor-mode -1)
        :documentation
        "Globally keep point always at the center of the window."
        :evil-leader "t C--"))
    :config
    (progn
      (setq ccm-recenter-at-end-of-file t
            ccm-ignored-commands '(mouse-drag-region
                                   mouse-set-point
                                   widget-button-click
                                   scroll-bar-toolkit-scroll
                                   evil-mouse-drag-region))
      (spacemacs|diminish centered-cursor-mode " ⊝" " -"))))

(defun spacemacs/init-clean-aindent-mode ()
  (use-package clean-aindent-mode
    :defer t
    :init
    (add-hook 'prog-mode-hook 'clean-aindent-mode)))

(defun spacemacs/init-desktop ()
  (use-package desktop
    :defer t
    :config
    (progn
      (setq desktop-dirname spacemacs-cache-directory)
      (push spacemacs-cache-directory desktop-path))))

(defun spacemacs/init-define-word ()
  (use-package define-word
    :defer t
    :init
    (evil-leader/set-key
      "xwd" 'define-word-at-point)))

(defun spacemacs/init-diminish ()
  (use-package diminish
    :init
    (progn
      ;; Minor modes abbrev --------------------------------------------------------
      (when (display-graphic-p)
        (eval-after-load "eproject"
          '(diminish 'eproject-mode " eⓅ"))
        (eval-after-load "flymake"
          '(diminish 'flymake-mode " Ⓕ2")))
      ;; Minor Mode (hidden) ------------------------------------------------------
      (eval-after-load 'elisp-slime-nav
        '(diminish 'elisp-slime-nav-mode))
      (eval-after-load "hi-lock"
        '(diminish 'hi-lock-mode))
      (eval-after-load "abbrev"
        '(diminish 'abbrev-mode))
      (eval-after-load "subword"
        '(when (eval-when-compile (version< "24.3.1" emacs-version))
           (diminish 'subword-mode))))))

(defun spacemacs/init-dired+ ()
  (use-package dired+
    :defer t))

(defun spacemacs/init-doc-view ()
  (use-package doc-view
    :defer t
    :init
    (evilify doc-view-mode doc-view-mode-map
             "/"  'spacemacs/doc-view-search-new-query
             "?"  'spacemacs/doc-view-search-new-query-backward
             "gg" 'doc-view-first-page
             "G"  'doc-view-last-page
             "gt" 'doc-view-goto-page
             "h"  'doc-view-previous-page
             "j"  'doc-view-next-line-or-next-page
             "k"  'doc-view-previous-line-or-previous-page
             "K"  'doc-view-kill-proc-and-buffer
             "l"  'doc-view-next-page
             "n"  'doc-view-search
             "N"  'doc-view-search-backward
             (kbd "C-d") 'doc-view-scroll-up-or-next-page
             (kbd "C-k") 'doc-view-kill-proc
             (kbd "C-u") 'doc-view-scroll-down-or-previous-page)
    :config
    (progn
      (defun spacemacs/doc-view-search-new-query ()
        "Initiate a new query."
        (interactive)
        (doc-view-search 'newquery))

      (defun spacemacs/doc-view-search-new-query-backward ()
        "Initiate a new query."
        (interactive)
        (doc-view-search 'newquery t))

      ;; fixed a weird issue where toggling display does not
      ;; swtich to text mode
      (defadvice doc-view-toggle-display
          (around spacemacs/doc-view-toggle-display activate)
        (if (eq major-mode 'doc-view-mode)
            (progn
              ad-do-it
              (text-mode)
              (doc-view-minor-mode))
          ad-do-it)))))

;; notes from mijoharas
;; We currently just set a few variables to make it look nicer.
;; Here is my first attempt at evilifying the buffer, does not work correctly, help is very much welcome.

;; ```
;; (defun ediff/setup-ediff-keymaps ()
;;   "setup the evil ediff keymap"
;;     (progn
;;      (add-to-list 'evil-emacs-state-modes 'Ediff)
;;      (spacemacs|evilify ediff-mode-map)
;;      (spacemacs/activate-evil-leader-for-map 'ediff-mode-map)
;;       )
;;   )

;; ;; inside the use-package function
;; (add-hook 'ediff-keymap-setup-hook 'ediff/setup-ediff-keymaps)
;; ```
(defun spacemacs/init-ediff ()
  (use-package ediff
    :defer t
    :init
    (progn
      ;; first we set some sane defaults
      (setq-default
       ediff-window-setup-function 'ediff-setup-windows-plain
       ;; emacs is evil and decrees that vertical shall henceforth be horizontal
       ediff-split-window-function 'split-window-horizontally
       ediff-merge-split-window-function 'split-window-horizontally))))

(defun spacemacs/init-eldoc ()
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

(defun spacemacs/init-emacs-builtin-process-menu ()
  (evilify process-menu-mode process-menu-mode-map))

(defun spacemacs/init-eval-sexp-fu ()
  ;; ignore obsolete function warning generated on startup
  (let ((byte-compile-not-obsolete-funcs (append byte-compile-not-obsolete-funcs '(preceding-sexp))))
    (require 'eval-sexp-fu)))

(defun spacemacs/init-evil ()
  (use-package evil
    :init
    (progn
      (defvar spacemacs-evil-cursor-colors '((normal . "DarkGoldenrod2")
                                             (insert . "chartreuse3")
                                             (emacs  . "SkyBlue2")
                                             (replace . "chocolate")
                                             (evilified . "LightGoldenrod3")
                                             (visual . "gray")
                                             (motion . "plum3")
                                             (lisp   . "HotPink1")
                                             (iedit  . "firebrick1")
                                             (iedit-insert  . "firebrick1"))
        "Colors assigned to evil states.")

      ;; put back refresh of the cursor on post-command-hook see status of:
      ;; https://bitbucket.org/lyro/evil/issue/502/cursor-is-not-refreshed-in-some-cases
      (add-hook 'post-command-hook 'evil-refresh-cursor)

      ;; allow the point to go past the end of line so we can
      ;; consisently evaluate expression with eval-last-sexp in
      ;; all modes
      (setq evil-move-beyond-eol t)

      (defun spacemacs/state-color-face (state)
        "Return the symbol of the face for the given STATE."
        (intern (format "spacemacs-%s-face" (symbol-name state))))

      (defun spacemacs/defface-state-color (state color)
        "Define a face for the given STATE and background COLOR."
        (eval `(defface ,(spacemacs/state-color-face state) '((t ()))
                 ,(format "%s state face." (symbol-name state))
                 :group 'spacemacs))
        (set-face-attribute (spacemacs/state-color-face state) nil
                            :background color
                            :foreground (face-background 'mode-line)
                            :box (face-attribute 'mode-line :box)
                            :inherit 'mode-line))

      (defun spacemacs/state-color (state)
        "Return the color string associated to STATE."
        (face-background (spacemacs/state-color-face state)))

      (defun spacemacs/current-state-color ()
        "Return the color string associated to the current state."
        (face-background (spacemacs/state-color-face evil-state)))

      (defun spacemacs/state-face (state)
        "Return the face associated to the STATE."
        (spacemacs/state-color-face state))

      (defun spacemacs/current-state-face ()
        "Return the face associated to the current state."
        (let ((state (if (eq evil-state 'operator)
                         evil-previous-state
                       evil-state)))
          (spacemacs/state-color-face state)))

      (defun spacemacs/set-state-faces ()
        "Define or set the state faces."
        (mapcar (lambda (x) (spacemacs/defface-state-color (car x) (cdr x)))
                spacemacs-evil-cursor-colors))
      (spacemacs/set-state-faces)

      (defun set-default-evil-emacs-state-cursor ()
        (let ((c (when dotspacemacs-colorize-cursor-according-to-state
                   (spacemacs/state-color 'emacs))))
          (setq evil-emacs-state-cursor `(,c box))))
      (defun set-default-evil-evilified-state-cursor ()
        (let ((c (when dotspacemacs-colorize-cursor-according-to-state
                   (spacemacs/state-color 'evilified))))
          (setq evil-evilified-state-cursor `(,c box))))
      (defun set-default-evil-normal-state-cursor ()
        (let ((c (when dotspacemacs-colorize-cursor-according-to-state
                   (spacemacs/state-color 'normal))))
          (setq evil-normal-state-cursor `(,c box))))
      (defun set-default-evil-insert-state-cursor ()
        (let ((c (when dotspacemacs-colorize-cursor-according-to-state
                   (spacemacs/state-color 'insert))))
          (setq evil-insert-state-cursor `(,c (bar . 2)))))
      (defun set-default-evil-visual-state-cursor ()
        (let ((c (when dotspacemacs-colorize-cursor-according-to-state
                   (spacemacs/state-color 'visual))))
          (setq evil-visual-state-cursor `(,c (hbar . 2)))))
      (defun set-default-evil-motion-state-cursor ()
        (let ((c (when dotspacemacs-colorize-cursor-according-to-state
                   (spacemacs/state-color 'motion))))
          (setq evil-motion-state-cursor `(,c box))))
      (defun set-default-evil-lisp-state-cursor ()
        (let ((c (when dotspacemacs-colorize-cursor-according-to-state
                   (spacemacs/state-color 'lisp))))
          (setq evil-lisp-state-cursor `(,c box))))
      (defun set-default-evil-iedit-state-cursor ()
        (let ((c (when dotspacemacs-colorize-cursor-according-to-state
                   (spacemacs/state-color 'iedit))))
          (setq evil-iedit-state-cursor `(,c box))))
      (defun set-default-evil-iedit-insert-state-cursor ()
        (let ((c (when dotspacemacs-colorize-cursor-according-to-state
                   (spacemacs/state-color 'iedit-insert))))
          (setq evil-iedit-insert-state-cursor `(,c (bar . 2)))))
      (defun set-default-evil-replace-state-cursor ()
        (let ((c (when dotspacemacs-colorize-cursor-according-to-state
                   (spacemacs/state-color 'replace))))
          (setq evil-replace-state-cursor `(,c (hbar . 2)))))
      (defun evil-insert-state-cursor-hide ()
        (setq evil-insert-state-cursor '((hbar . 0))))
      (set-default-evil-emacs-state-cursor)
      (set-default-evil-evilified-state-cursor)
      (set-default-evil-normal-state-cursor)
      (set-default-evil-insert-state-cursor)
      (set-default-evil-visual-state-cursor)
      (set-default-evil-motion-state-cursor)
      (set-default-evil-lisp-state-cursor)
      (set-default-evil-iedit-state-cursor)
      (set-default-evil-iedit-insert-state-cursor)
      (set-default-evil-replace-state-cursor)

      (evil-mode 1))
    :config
    (progn
      ;; evil ex-command key
      (define-key evil-normal-state-map (kbd dotspacemacs-command-key) 'evil-ex)
      (define-key evil-visual-state-map (kbd dotspacemacs-command-key) 'evil-ex)
      (define-key evil-motion-state-map (kbd dotspacemacs-command-key) 'evil-ex)
      ;; Make the current definition and/or comment visible.
      (define-key evil-normal-state-map "zf" 'reposition-window)
      ;; toggle maximize buffer
      (define-key evil-window-map (kbd "o") 'spacemacs/toggle-maximize-buffer)
      (define-key evil-window-map (kbd "C-o") 'spacemacs/toggle-maximize-buffer)

      (evil-leader/set-key "re" 'evil-show-registers)

      (defmacro evil-map (state key seq)
        "Map for a given STATE a KEY to a sequence SEQ of keys.

Can handle recursive definition only if KEY is the first key of SEQ.
Example: (evil-map visual \"<\" \"<gv\")"
        (let ((map (intern (format "evil-%S-state-map" state))))
          `(define-key ,map ,key
             (lambda ()
               (interactive)
               ,(if (string-equal key (substring seq 0 1))
                    `(progn
                       (call-interactively ',(lookup-key evil-normal-state-map key))
                       (execute-kbd-macro ,(substring seq 1)))
                  (execute-kbd-macro ,seq))))))
      ;; Keep the region active when shifting
      (evil-map visual "<" "<gv")
      (evil-map visual ">" ">gv")

      (defun spacemacs/evil-smart-doc-lookup ()
        "Version of `evil-lookup' that attempts to use
        the mode specific goto-definition binding,
        i.e. `SPC m h h`, to lookup the source of the definition,
        while falling back to `evil-lookup'"
        (interactive)
        (condition-case nil
            (execute-kbd-macro (kbd (concat dotspacemacs-leader-key " mhh")))
          (error (evil-lookup))))
      (define-key evil-normal-state-map (kbd "K") 'spacemacs/evil-smart-doc-lookup)

      (defun spacemacs/evil-smart-goto-definition ()
        "Version of `evil-goto-definition' that attempts to use
        the mode specific goto-definition binding,
        i.e. `SPC m g g`, to lookup the source of the definition,
        while falling back to `evil-goto-definition'"
        (interactive)
        (condition-case nil
            (execute-kbd-macro (kbd (concat dotspacemacs-leader-key " mgg")))
          (error (evil-goto-definition))))
      (define-key evil-normal-state-map
        (kbd "gd") 'spacemacs/evil-smart-goto-definition)

      ;; scrolling micro state
      (defun spacemacs/scroll-half-page-up ()
        "Scroll half a page up while keeping cursor in middle of page."
        (interactive)
        (evil-window-top)
        (let ((recenter-redisplay nil))
          (recenter nil)))
      (defun spacemacs/scroll-half-page-down ()
        "Scroll half a page down while keeping cursor in middle of page."
        (interactive)
        (evil-window-bottom)
        ;; required to make repeated presses idempotent
        (evil-next-visual-line)
        (let ((recenter-redisplay nil))
          (recenter nil)))
      (spacemacs|define-micro-state scroll
        :doc "[,] page up [.] page down [<] half page up [>] half page down"
        :execute-binding-on-enter t
        :evil-leader "n." "n," "n<" "n>"
        :bindings
        ;; page
        ("," evil-scroll-page-up)
        ("." evil-scroll-page-down)
        ;; half page
        ("<" spacemacs/scroll-half-page-up)
        (">" spacemacs/scroll-half-page-down))

      ;; support for auto-indentation inhibition on universal argument
      (spacemacs|advise-commands
       "handle-indent" (evil-paste-before evil-paste-after) around
       "Handle the universal prefix argument for auto-indentation."
       (let ((prefix (ad-get-arg 0)))
         (ad-set-arg 0 (unless (equal '(4) prefix) prefix))
         ad-do-it
         (ad-set-arg 0 prefix)))

      ;; pasting micro-state
      (spacemacs|advise-commands
       "paste-micro-state"
       (evil-paste-before evil-paste-after evil-visual-paste) after
       "Initate the paste micro-state."
       (unless (or (evil-ex-p)
                   (eq 'evil-paste-from-register this-command))
         (spacemacs/paste-micro-state)))
      (defun spacemacs//paste-ms-doc ()
        "The documentation for the paste micro-state."
        (format (concat "[%s/%s] Type [p] or [P] to paste the previous or "
                        "next copied text, [.] to paste the same text")
                (length kill-ring-yank-pointer) (length kill-ring)))
      (spacemacs|define-micro-state paste
        :doc (spacemacs//paste-ms-doc)
        :use-minibuffer t
        :bindings
        ("p" evil-paste-pop)
        ("P" evil-paste-pop-next))
      (unless dotspacemacs-enable-paste-micro-state
        (ad-disable-advice 'evil-paste-before 'after
                           'evil-paste-before-paste-micro-state)
        (ad-activate 'evil-paste-before)
        (ad-disable-advice 'evil-paste-after 'after
                           'evil-paste-after-paste-micro-state)
        (ad-activate 'evil-paste-after)
        (ad-disable-advice 'evil-visual-paste 'after
                           'evil-visual-paste-paste-micro-state)
        (ad-activate 'evil-visual-paste))

      ;; define text objects
      (defmacro spacemacs|define-text-object (key name start end)
        (let ((inner-name (make-symbol (concat "evil-inner-" name)))
              (outer-name (make-symbol (concat "evil-outer-" name)))
              (start-regex (regexp-opt (list start)))
              (end-regex (regexp-opt (list end))))
          `(progn
             (evil-define-text-object ,inner-name (count &optional beg end type)
               (evil-select-paren ,start-regex ,end-regex beg end type count nil))
             (evil-define-text-object ,outer-name (count &optional beg end type)
               (evil-select-paren ,start-regex ,end-regex beg end type count t))
             (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
             (define-key evil-outer-text-objects-map ,key (quote ,outer-name))
             (when (configuration-layer/package-usedp 'evil-surround)
               (push (cons (string-to-char ,key)
                           (if ,end
                               (cons ,start ,end)
                             ,start))
                     evil-surround-pairs-alist)))))

      (defun spacemacs//standard-text-objects ()
        ;; between dollars sign:
        (spacemacs|define-text-object "$" "dollar" "$" "$")
        ;; define stars
        (spacemacs|define-text-object "*" "star" "*" "*")
        ;; define block star text object
        (spacemacs|define-text-object "8" "block-star" "/*" "*/")
        ;; between pipe characters:
        (spacemacs|define-text-object "|" "bar" "|" "|")
        ;; between percent signs:
        (spacemacs|define-text-object "%" "percent" "%" "%"))

      (spacemacs/add-to-hook 'prog-mode-hook '(spacemacs//standard-text-objects))

      ;; support smart 1parens-strict-mode
      (when (configuration-layer/package-usedp 'smartparens)
          (defadvice evil-delete-backward-char-and-join
              (around spacemacs/evil-delete-backward-char-and-join activate)
            (defvar smartparens-strict-mode)
            ;; defadvice compiles this sexp generating a compiler warning for a
            ;; free variable reference. The line above fixes this
            (if smartparens-strict-mode
                (call-interactively 'sp-backward-delete-char)
              ad-do-it))))))

(defun spacemacs/init-evil-anzu ()
  (use-package evil-anzu
    :init
    (global-anzu-mode t)
    :config
    (progn
      (spacemacs|hide-lighter anzu-mode)
      (setq anzu-search-threshold 1000
            anzu-cons-mode-line-p nil)
      ;; powerline integration
      (when (configuration-layer/package-usedp 'powerline)
        (defun spacemacs/anzu-update-mode-line (here total)
          "Custom update function which does not propertize the status."
          (when anzu--state
            (let ((status (cl-case anzu--state
                            (search (format "(%s/%d%s)"
                                            (anzu--format-here-position here total)
                                            total (if anzu--overflow-p "+" "")))
                            (replace-query (format "(%d replace)" total))
                            (replace (format "(%d/%d)" here total)))))
              status)))
        (setq anzu-mode-line-update-function 'spacemacs/anzu-update-mode-line)))))

(defun spacemacs/init-evil-args ()
  (use-package evil-args
    :init
    (progn
      ;; bind evil-args text objects
      (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
      (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))))

(defun spacemacs/init-evil-escape ()
  (use-package evil-escape
    :init
    (evil-escape-mode)
    :config
    (spacemacs|hide-lighter evil-escape-mode)))

(defun spacemacs/init-evil-exchange ()
  (use-package evil-exchange
    :init (evil-exchange-install)))

(defun spacemacs/init-evil-iedit-state ()
  (use-package evil-iedit-state
    :commands (evil-iedit-state evil-iedit-state/iedit-mode)
    :init
    (progn
      (evil-leader/set-key "se" 'evil-iedit-state)
      (evil-leader/set-key "sE" 'evil-iedit-state/iedit-mode))
    :config
    (progn
      ;; activate leader in iedit and iedit-insert states
      (define-key evil-iedit-state-map
        (kbd evil-leader/leader) evil-leader--default-map))))

(defun spacemacs/init-evil-indent-textobject ()
  (use-package evil-indent-textobject))

(defun spacemacs/init-evil-jumper ()
  (use-package evil-jumper
    :init
    (progn
      (setq evil-jumper-file (concat spacemacs-cache-directory "evil-jumps")
            evil-jumper-auto-save-interval 3600)
      (evil-jumper-mode t)
      )))

(defun spacemacs/init-evil-leader ()
  (use-package evil-leader
    :init
    (progn
      (setq evil-leader/leader dotspacemacs-leader-key)
      (global-evil-leader-mode))
    :config
    (progn
      ;; Unset shortcuts which shadow evil leader
      (eval-after-load "compile"
        '(progn
           ;; (define-key compilation-mode-map (kbd dotspacemacs-leader-key) nil)
           (define-key compilation-mode-map (kbd "h") nil)))
      ;; (eval-after-load "dired"
      ;;   '(define-key dired-mode-map (kbd dotspacemacs-leader-key) nil))
      ;; make leader available in visual and motion states
      (mapc (lambda (s)
              (eval `(define-key
                       ,(intern (format "evil-%S-state-map" s))
                       ,(kbd dotspacemacs-leader-key)
                       evil-leader--default-map)))
            '(motion visual))
      ;; emacs and insert states (make it also available in other states
      ;; for consistency and POLA.)
      (mapc (lambda (s)
              (eval `(define-key
                       ,(intern (format "evil-%S-state-map" s))
                       ,(kbd dotspacemacs-emacs-leader-key)
                       evil-leader--default-map)))
            '(emacs insert normal visual motion))
      ;; experimental: map SPC m to ,
      (when dotspacemacs-major-mode-leader-key
        (add-hook 'after-change-major-mode-hook
                  'spacemacs/activate-major-mode-leader)))))

(defun spacemacs/init-evil-lisp-state ()
  (use-package evil-lisp-state
    :init
    (progn
      (setq evil-lisp-state-global t)
      (setq evil-lisp-state-leader-prefix "k"))))

(defun spacemacs/init-evil-nerd-commenter ()
  (use-package evil-nerd-commenter
    :commands (evilnc-comment-operator
               evilnc-comment-or-uncomment-lines
               evilnc-toggle-invert-comment-line-by-line
               evilnc-comment-or-uncomment-paragraphs
               evilnc-quick-comment-or-uncomment-to-the-line
               evilnc-copy-and-comment-lines)
    :init
    (progn
      (evil-leader/set-key
        ";"  'evilnc-comment-operator
        "cl" 'evilnc-comment-or-uncomment-lines
        "ci" 'evilnc-toggle-invert-comment-line-by-line
        "cp" 'evilnc-comment-or-uncomment-paragraphs
        "ct" 'evilnc-quick-comment-or-uncomment-to-the-line
        "cy" 'evilnc-copy-and-comment-lines))))

(defun spacemacs/init-evil-matchit ()
  (use-package evil-matchit
    :defer t))

(defun spacemacs/init-evil-numbers ()
  (use-package evil-numbers
    :config
    (progn
      (defun spacemacs/evil-numbers-micro-state-doc ()
        "Display a short documentation in the mini buffer."
        (spacemacs/echo "+/= to increase the value or - to decrease it"))

      (defun spacemacs/evil-numbers-micro-state-overlay-map ()
        "Set a temporary overlay map to easily increase or decrease a number"
        (set-temporary-overlay-map
         (let ((map (make-sparse-keymap)))
           (define-key map (kbd "+") 'spacemacs/evil-numbers-increase)
           (define-key map (kbd "=") 'spacemacs/evil-numbers-increase)
           (define-key map (kbd "-") 'spacemacs/evil-numbers-decrease)
           map) t)
        (spacemacs/evil-numbers-micro-state-doc))

      (defun spacemacs/evil-numbers-increase (amount &optional no-region)
        "Increase number at point."
        (interactive "p*")
        (evil-numbers/inc-at-pt amount no-region)
        (spacemacs/evil-numbers-micro-state-overlay-map))
      (defun spacemacs/evil-numbers-decrease (amount)
        "Decrease number at point."
        (interactive "p*")
        (evil-numbers/dec-at-pt amount)
        (spacemacs/evil-numbers-micro-state-overlay-map))
      (evil-leader/set-key "n+" 'spacemacs/evil-numbers-increase)
      (evil-leader/set-key "n=" 'spacemacs/evil-numbers-increase)
      (evil-leader/set-key "n-" 'spacemacs/evil-numbers-decrease))))

(defun spacemacs/init-evil-search-highlight-persist ()
  (use-package evil-search-highlight-persist
    :init
    (progn
      (global-evil-search-highlight-persist)
      ;; (set-face-attribute )
      (evil-leader/set-key "sc" 'evil-search-highlight-persist-remove-all)
      (define-key evil-search-highlight-persist-map (kbd "C-x SPC") 'rectangle-mark-mode)
      (evil-ex-define-cmd "nohlsearch"
                          'evil-search-highlight-persist-remove-all)
      (defun spacemacs/adaptive-evil-highlight-persist-face ()
        (set-face-attribute 'evil-search-highlight-persist-highlight-face nil
                            :inherit 'region
                            :background nil
                            :foreground nil))
      (spacemacs/adaptive-evil-highlight-persist-face))))

(defun spacemacs/init-evil-surround ()
  (use-package evil-surround
    :init
    (progn
      (global-evil-surround-mode 1)
      ;; `s' for surround instead of `substitute'
      ;; see motivation for this change in the documentation
      (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
      (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute))))

(defun spacemacs/init-evil-terminal-cursor-changer ()
  (use-package evil-terminal-cursor-changer
    :if (not (display-graphic-p))
    :init (setq evil-visual-state-cursor 'box
                evil-insert-state-cursor 'bar
                evil-emacs-state-cursor 'hbar)))

(defun spacemacs/init-evil-tutor ()
  (use-package evil-tutor
    :commands (evil-tutor-start
               evil-tutor-resume)
    :init
    (progn
      (setq evil-tutor-working-directory
            (concat spacemacs-cache-directory ".tutor/"))
      (evil-leader/set-key "hT" 'evil-tutor-start))))

(defun spacemacs/init-evil-visualstar ()
  (use-package evil-visualstar
    :commands (evil-visualstar/begin-search-forward
               evil-visualstar/begin-search-backward)
    :init
    (progn
      (define-key evil-visual-state-map (kbd "*")
        'evil-visualstar/begin-search-forward)
      (define-key evil-visual-state-map (kbd "#")
        'evil-visualstar/begin-search-backward))))

(defun spacemacs/init-exec-path-from-shell ()
  (use-package exec-path-from-shell
    :init (when (memq window-system '(mac ns x))
            (exec-path-from-shell-initialize))))

(defun spacemacs/init-expand-region ()
  (use-package expand-region
    :defer t
    :init (evil-leader/set-key "v" 'er/expand-region)
    :config
    (progn
      ;; add search capability to expand-region
      (when (configuration-layer/package-usedp 'helm-ag)
        (defadvice er/prepare-for-more-expansions-internal
            (around helm-ag/prepare-for-more-expansions-internal activate)
          ad-do-it
          (let ((new-msg (concat (car ad-return-value)
                                 ", / to search in project, "
                                 "f to search in files, "
                                 "b to search in opened buffers"))
                (new-bindings (cdr ad-return-value)))
            (cl-pushnew
             '("/" (lambda ()
                     (call-interactively
                      'spacemacs/helm-project-smart-do-search-region-or-symbol)))
             new-bindings)
            (cl-pushnew
             '("f" (lambda ()
                     (call-interactively
                      'spacemacs/helm-files-smart-do-search-region-or-symbol)))
             new-bindings)
            (cl-pushnew
             '("b" (lambda ()
                     (call-interactively
                      'spacemacs/helm-buffers-smart-do-search-region-or-symbol)))
             new-bindings)
            (setq ad-return-value (cons new-msg new-bindings)))))
      (setq expand-region-contract-fast-key "V"
            expand-region-reset-fast-key "r"))))

(defun spacemacs/init-fancy-battery ()
  (use-package fancy-battery
    :defer t
    :init
    (progn
      (spacemacs|add-toggle mode-line-battery
        :status fancy-battery-mode
        :on (fancy-battery-mode)
        :off (fancy-battery-mode -1)
        :documentation "Display battery info in mode-line."
        :evil-leader "tmb")

      (push 'fancy-battery-mode-line spacemacs--global-mode-line-excludes)

      (defun spacemacs/mode-line-battery-percentage ()
        "Return the load percentage or an empty string."
        (let ((p (cdr (assq ?p fancy-battery-last-status))))
          (if (and fancy-battery-show-percentage
                   p (not (string= "N/A" p))) (concat " " p "%%") "")))

      (defun spacemacs/mode-line-battery-time ()
        "Return the remaining time complete load or discharge."
        (let ((time (cdr (assq ?t fancy-battery-last-status))))
          (cond
           ((string= "0:00" time) "")
           ((string= "N/A" time) "")
           ((string-empty-p time) "")
           (t (concat " (" time ")")))))

      (setq-default fancy-battery-show-percentage t))
    :config
    (progn
      ;; redefine this function for Spacemacs,
      ;; basically remove all faces and properties.
      (defun fancy-battery-default-mode-line ()
        "Assemble a mode line string for Fancy Battery Mode."
        (when fancy-battery-last-status
          (let* ((type (cdr (assq ?L fancy-battery-last-status)))
                 (percentage (spacemacs/mode-line-battery-percentage))
                 (time (spacemacs/mode-line-battery-time)))
            (cond
             ((string= "on-line" type) " No Battery")
             ((string-empty-p type) " No Battery")
             (t (concat (if (string= "AC" type) " AC" "") percentage time))))))

      (defun fancy-battery-powerline-face ()
        "Return a face appropriate for powerline"
        (let ((type (cdr (assq ?L fancy-battery-last-status))))
          (if (and type (string= "AC" type))
              'fancy-battery-charging
            (pcase (cdr (assq ?b fancy-battery-last-status))
              ("!"  'fancy-battery-critical)
              ("+"  'fancy-battery-charging)
              ("-"  'fancy-battery-discharging)
              (_ 'fancy-battery-discharging))))))
    ))

(defun spacemacs/init-fill-column-indicator ()
  (use-package fill-column-indicator
    :defer t
    :init
    (progn
      (setq fci-rule-width 1)
      (setq fci-rule-color "#D0BF8F")
      ;; manually register the minor mode since it does not define any
      ;; lighter
      (push '(fci-mode "") minor-mode-alist)
      (spacemacs|add-toggle fill-column-indicator
        :status fci-mode
        :on (turn-on-fci-mode)
        :off (turn-off-fci-mode)
        :documentation "Display the fill column indicator."
        :evil-leader "tf"))
    :config
    (spacemacs|hide-lighter fci-mode)))

(defun spacemacs/init-flx-ido ()
  (use-package flx-ido
    :init (flx-ido-mode 1)))

(defun spacemacs/init-fringe-helper ())

(defun spacemacs/init-gh-md ())

(defun spacemacs/init-golden-ratio ()
  (use-package golden-ratio
    :defer t
    :init
    (spacemacs|add-toggle golden-ratio
      :status golden-ratio-mode
      :on (golden-ratio-mode) (golden-ratio)
      :off (golden-ratio-mode -1) (balance-windows)
      :documentation
      (concat "Dynamically resize the focused window using "
              "the golden ratio.")
      :evil-leader "tg")
    :config
    (progn
      (setq golden-ratio-exclude-modes '("bs-mode"
                                         "calc-mode"
                                         "ediff-mode"
                                         "dired-mode"
                                         "gud-mode"
                                         "gdb-locals-mode"
                                         "gdb-registers-mode"
                                         "gdb-breakpoints-mode"
                                         "gdb-threads-mode"
                                         "gdb-frames-mode"
                                         "gdb-inferior-io-mode"
                                         "gud-mode"
                                         "gdb-inferior-io-mode"
                                         "gdb-disassembly-mode"
                                         "gdb-memory-mode"
                                         "restclient-mode"
                                         "speedbar-mode"
                                         ))

      (setq golden-ratio-extra-commands
            (append golden-ratio-extra-commands
                    '(ace-window
                      ace-delete-window
                      ace-select-window
                      ace-swap-window
                      ace-maximize-window
                      avy-pop-mark
                      evil-avy-goto-word-or-subword-1
                      evil-avy-goto-line
                      windmove-left
                      windmove-right
                      windmove-up
                      windmove-down
                      evil-window-delete
                      evil-window-split
                      evil-window-vsplit
                      evil-window-left
                      evil-window-right
                      evil-window-up
                      evil-window-down
                      evil-window-bottom-right
                      evil-window-top-left
                      evil-window-mru
                      evil-window-next
                      evil-window-prev
                      evil-window-new
                      evil-window-vnew
                      evil-window-rotate-upwards
                      evil-window-rotate-downwards
                      evil-window-move-very-top
                      evil-window-move-far-left
                      evil-window-move-far-right
                      evil-window-move-very-bottom
                      select-window-0
                      select-window-1
                      select-window-2
                      select-window-3
                      select-window-4
                      select-window-5
                      select-window-6
                      select-window-7
                      select-window-8
                      select-window-9
                      buf-move-left
                      buf-move-right
                      buf-move-up
                      buf-move-down
                      ess-eval-buffer-and-go
                      ess-eval-function-and-go
                      ess-eval-line-and-go)))

      ;; Disable auto-resizing for some buffers
      (defun spacemacs/no-golden-ratio-for-buffers (bufname)
        "Disable golden-ratio if BUFNAME is the name of a visible buffer."
        (and (get-buffer bufname) (get-buffer-window bufname 'visible)))
      (defun spacemacs/no-golden-ratio-guide-key ()
        "Disable golden-ratio for guide-key popwin buffer."
        (or (spacemacs/no-golden-ratio-for-buffers " *guide-key*")
            (spacemacs/no-golden-ratio-for-buffers " *popwin-dummy*")))
      (add-to-list 'golden-ratio-inhibit-functions
                   'spacemacs/no-golden-ratio-guide-key)
      (add-to-list 'golden-ratio-exclude-buffer-names " *NeoTree*")
      (add-to-list 'golden-ratio-exclude-buffer-names "*LV*")
      (add-to-list 'golden-ratio-exclude-buffer-names " *which-key*")

      (spacemacs|diminish golden-ratio-mode " ⓖ" " g"))))

(defun spacemacs/init-google-translate ()
  (use-package google-translate
    :commands (google-translate-query-translate
               google-translate-at-point
               google-translate-query-translate-reverse
               google-translate-at-point-reverse)
    :init
    (evil-leader/set-key
      "xgQ" 'google-translate-query-translate-reverse
      "xgq" 'google-translate-query-translate
      "xgT" 'google-translate-at-point-reverse
      "xgt" 'google-translate-at-point)
    :config
    (progn
      (require 'google-translate-default-ui)
      (setq google-translate-enable-ido-completion t)
      (setq google-translate-show-phonetic t)
      (setq google-translate-default-source-language "En")
      (setq google-translate-default-target-language "Fr"))))

;; (defun spacemacs/init-guide-key-tip ()
;;   (use-package guide-key-tip
;;     :init
;;     (progn
;;       (defun spacemacs/toggle-guide-key ()
;;         "Toggle golden-ratio mode on and off."
;;         (interactive)
;;         (if (symbol-value guide-key-mode)
;;             (guide-key-mode -1)
;;           (guide-key-mode)))

;;       (defadvice guide-key/popup-guide-buffer-p
;;           (around spacemacs/inhibit-guide-buffer activate)
;;         "Prevent the popup of the guide-key buffer in some case."
;;         ;; a micro-state is running
;;         ;; or
;;         ;; bzg-big-fringe-mode is on
;;         (unless (or overriding-terminal-local-map
;;                     bzg-big-fringe-mode)
;;           ad-do-it))

;;       (spacemacs|add-toggle guide-key
;;                       :status guide-key-mode
;;                       :on (guide-key-mode)
;;                       :off (guide-key-mode -1)
;;                       :documentation
;;                       "Display a buffer with available key bindings."
;;                       :evil-leader "tG")

;;       (setq guide-key/guide-key-sequence `("C-x"
;;                                            "C-c"
;;                                            "C-w"
;;                                            ,dotspacemacs-leader-key
;;                                            ,dotspacemacs-emacs-leader-key
;;                                            ,dotspacemacs-major-mode-leader-key
;;                                            ,dotspacemacs-major-mode-emacs-leader-key
;;                                            ;; M-m in terminal
;;                                            "<ESC>m"
;;                                            ;; C-M-m in terminal
;;                                            "<ESC><RET>"
;;                                            "g"
;;                                            "\["
;;                                            "\]"
;;                                            "z"
;;                                            "C-h")
;;             guide-key/recursive-key-sequence-flag t
;;             guide-key/popup-window-position 'bottom
;;             guide-key/idle-delay dotspacemacs-guide-key-delay
;;             guide-key/text-scale-amount 0
;;             ;; use this in your ~/.spacemacs file to enable tool tip in a
;;             ;; graphical envrionment
;;             ;; guide-key-tip/enabled (if window-system t)
;;             guide-key-tip/enabled nil)
;;       (setq guide-key/highlight-command-regexp
;;                    (cons spacemacs/prefix-command-string font-lock-warning-face))
;;       (guide-key-mode 1)
;;       (spacemacs|diminish guide-key-mode " Ⓖ" " G"))))

(defun spacemacs/init-helm ()
  (use-package helm
    :defer 1
    :commands spacemacs/helm-find-files
    :config
    (progn
      (defun spacemacs/helm-find-files (arg)
        "Custom spacemacs implementation for calling helm-find-files-1.

Removes the automatic guessing of the initial value based on thing at point. "
        (interactive "P")
        (let* ((hist          (and arg helm-ff-history (helm-find-files-history)))
                (default-input hist )
                (input         (cond ((and (eq major-mode 'dired-mode) default-input)
                                    (file-name-directory default-input))
                                    ((and (not (string= default-input ""))
                                            default-input))
                                    (t (expand-file-name (helm-current-directory))))))
            (set-text-properties 0 (length input) nil input)
            (helm-find-files-1 input )))
      )
    :init
    (progn
      (setq helm-prevent-escaping-from-minibuffer t
            helm-bookmark-show-location t
            helm-display-header-line nil
            helm-split-window-in-side-p t
            helm-always-two-windows t
            helm-echo-input-in-header-line t
            helm-imenu-execute-action-at-once-if-one nil)

      ;; hide minibuffer in Helm session, since we use the header line already
      (defun helm-hide-minibuffer-maybe ()
        (when (with-helm-buffer helm-echo-input-in-header-line)
          (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
            (overlay-put ov 'window (selected-window))
            (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                    `(:background ,bg-color :foreground ,bg-color)))
            (setq-local cursor-type nil))))
      (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

      ;; fuzzy matching setting
      (setq helm-M-x-fuzzy-match t
            helm-apropos-fuzzy-match t
            helm-file-cache-fuzzy-match t
            helm-imenu-fuzzy-match t
            helm-lisp-fuzzy-completion t
            helm-locate-fuzzy-match t
            helm-recentf-fuzzy-match t
            helm-semantic-fuzzy-match t
            helm-buffers-fuzzy-matching t)

      (defun spacemacs/helm-find-files-navigate-back (orig-fun &rest args)
        )
      (defadvice helm-ff-delete-char-backward
          (around spacemacs/helm-find-files-navigate-back activate)
        (if (= (length helm-pattern) (length (helm-find-files-initial-input)))
            (helm-find-files-up-one-level 1)
          ad-do-it))

      (defun spacemacs//helm-do-grep-region-or-symbol (&optional targs use-region-or-symbol-p)
        "Version of `helm-do-grep' with a default input."
        (interactive)
        (require 'helm)
        (cl-letf*
            (((symbol-function 'this-fn) (symbol-function 'helm-do-grep-1))
             ((symbol-function 'helm-do-grep-1)
              (lambda (targets &optional recurse zgrep exts default-input region-or-symbol-p)
                (let* ((new-input (when region-or-symbol-p
                                   (if (region-active-p)
                                       (buffer-substring-no-properties
                                        (region-beginning) (region-end))
                                     (thing-at-point 'symbol t))))
                      (quoted-input (when new-input (rxt-quote-pcre new-input))))
                  (this-fn targets recurse zgrep exts default-input quoted-input))))
             (preselection (or (dired-get-filename nil t)
                               (buffer-file-name (current-buffer))))
             (targets   (if targs
                            targs
                          (helm-read-file-name
                          "Search in file(s): "
                          :marked-candidates t
                          :preselect (and helm-do-grep-preselect-candidate
                                          (if helm-ff-transformer-show-only-basename
                                              (helm-basename preselection)
                                            preselection))))))
          (helm-do-grep-1 targets nil nil nil nil use-region-or-symbol-p)))

      (defun helm-find-contrib-file ()
        "Runs helm find files on spacemacs contrib folder"
        (interactive)
        (helm-find-files-1
         (expand-file-name (concat user-emacs-directory "contrib/"))))

      (defun helm-find-spacemacs-file ()
        "Runs helm find files on spacemacs directory"
        (interactive)
        (helm-find-files-1
         (expand-file-name (concat user-emacs-directory "spacemacs/"))))

      (defun spacemacs/helm-file-do-grep ()
        "Search in current file with `grep' using a default input."
        (interactive)
        (spacemacs//helm-do-grep-region-or-symbol
         (list (buffer-file-name (current-buffer))) nil))

      (defun spacemacs/helm-file-do-grep-region-or-symbol ()
        "Search in current file with `grep' using a default input."
        (interactive)
        (spacemacs//helm-do-grep-region-or-symbol
         (list (buffer-file-name (current-buffer))) t))

      (defun spacemacs/helm-files-do-grep ()
        "Search in files with `grep'."
        (interactive)
        (spacemacs//helm-do-grep-region-or-symbol nil nil))

      (defun spacemacs/helm-files-do-grep-region-or-symbol ()
        "Search in files with `grep' using a default input."
        (interactive)
        (spacemacs//helm-do-grep-region-or-symbol nil t))

      (defun spacemacs/helm-buffers-do-grep ()
        "Search in opened buffers with `grep'."
        (interactive)
        (let ((buffers (cl-loop for buffer in (buffer-list)
                                when (buffer-file-name buffer)
                                collect (buffer-file-name buffer))))
          (spacemacs//helm-do-grep-region-or-symbol buffers nil)))

      (defun spacemacs/helm-buffers-do-grep-region-or-symbol ()
        "Search in opened buffers with `grep' with a default input."
        (interactive)
        (let ((buffers (cl-loop for buffer in (buffer-list)
                                when (buffer-file-name buffer)
                                collect (buffer-file-name buffer))))
          (spacemacs//helm-do-grep-region-or-symbol buffers t)))

      (defun spacemacs/last-search-buffer ()
        "open last helm-ag or hgrep buffer."
        (interactive)
        (if (get-buffer "*helm ag results*")
            (switch-to-buffer-other-window "*helm ag results*")
            (if (get-buffer "*hgrep*")
                (switch-to-buffer-other-window "*hgrep*")
                (message "No previous search buffer found"))))

      ;; use helm by default for contrib and spacemacs layers
      (unless dotspacemacs-use-ido
        (define-key global-map [remap find-spacemacs-file]
          'helm-find-spacemacs-file)
        (define-key global-map [remap find-contrib-file]
          'helm-find-contrib-file))

      ;; use helm by default for M-x
      (unless (configuration-layer/package-usedp 'smex)
        (global-set-key (kbd "M-x") 'helm-M-x))

      (evil-leader/set-key
        "<f1>" 'helm-apropos
        "bb"   'helm-mini
        "Cl"   'helm-colors
        "ff"   'spacemacs/helm-find-files
        "fF"   'helm-find-files
        "fr"   'helm-recentf
        "hb"   'helm-filtered-bookmarks
        "hi"   'helm-info-at-point
        "hl"   'helm-resume
        "hm"   'helm-man-woman
        "iu"   'helm-ucs
        "ry"   'helm-show-kill-ring
        "rr"   'helm-register
        "rm"   'helm-all-mark-rings
        "sL"   'spacemacs/last-search-buffer
        "sl"   'spacemacs/jump-in-buffer)

      ;; search with grep
      (evil-leader/set-key
        "sgb"  'spacemacs/helm-buffers-do-grep
        "sgB"  'spacemacs/helm-buffers-do-grep-region-or-symbol
        "sgf"  'spacemacs/helm-files-do-grep
        "sgF"  'spacemacs/helm-files-do-grep-region-or-symbol
        "sgg"  'spacemacs/helm-file-do-grep
        "sgG"  'spacemacs/helm-file-do-grep-region-or-symbol)

      ;; define the key binding at the very end in order to allow the user
      ;; to overwrite any key binding
      (add-hook 'emacs-startup-hook
                (lambda ()
                  (unless (configuration-layer/package-usedp 'smex)
                    (evil-leader/set-key dotspacemacs-command-key 'helm-M-x))))

      (defvar spacemacs-helm-display-help-buffer-regexp '("*.*Helm.*Help.**"))
      (defvar spacemacs-helm-display-buffer-regexp `("*.*helm.**"
                                                     (display-buffer-in-side-window)
                                                     (inhibit-same-window . t)
                                                     (window-height . 0.4)))
      (defvar spacemacs-display-buffer-alist nil)
      (defun spacemacs//helm-prepare-display ()
        "Prepare necessary settings to make Helm display properly."
        ;; avoid Helm buffer being diplaye twice when user
        ;; sets this variable to some function that pop buffer to
        ;; a window. See https://github.com/syl20bnr/spacemacs/issues/1396
        (let ((display-buffer-base-action '(nil)))
          (setq spacemacs-display-buffer-alist display-buffer-alist)
          ;; the only buffer to display is Helm, nothing else we must set this
          ;; otherwise Helm cannot reuse its own windows for copyinng/deleting
          ;; etc... because of existing popwin buffers in the alist
          (setq display-buffer-alist nil)
          (popwin-mode -1)))

      (defun spacemacs//display-helm-at-bottom (buffer)
        (let ((display-buffer-alist (list spacemacs-helm-display-help-buffer-regexp
                                          ;; this or any specialized case of Helm buffer must be added AFTER
                                          ;; `spacemacs-helm-display-buffer-regexp'. Otherwise,
                                          ;; `spacemacs-helm-display-buffer-regexp' will be used before
                                          ;; `spacemacs-helm-display-help-buffer-regexp' and display
                                          ;; configuration for normal Helm buffer is applied for helm help
                                          ;; buffer, making the help buffer unable to be displayed.
                                          spacemacs-helm-display-buffer-regexp)))
          (helm-default-display-buffer buffer)))

      (setq helm-display-function 'spacemacs//display-helm-at-bottom)

      (defun spacemacs//restore-previous-display-config ()
        (popwin-mode 1)
        ;; we must enable popwin-mode first then restore `display-buffer-alist'
        ;; Otherwise, popwin keeps adding up its own buffers to `display-buffer-alist'
        ;; and could slow down Emacs as the list grows
        (setq display-buffer-alist spacemacs-display-buffer-alist))

      (add-hook 'helm-after-initialize-hook 'spacemacs//helm-prepare-display)
      ;;  Restore popwin-mode after a Helm session finishes.
      (add-hook 'helm-cleanup-hook 'spacemacs//restore-previous-display-config)

      ;; Add minibuffer history with `helm-minibuffer-history'
      (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

      (defun spacemacs//helm-cleanup ()
        "Cleanup some helm related states when quitting."
        ;; deactivate any running transient map (micro-state)
        (setq overriding-terminal-local-map nil))
      (add-hook 'helm-cleanup-hook 'spacemacs//helm-cleanup)

      (defface spacemacs-helm-navigation-ms-face
        `((t :background ,(face-attribute 'error :foreground) :foreground "black"))
        "Face for helm heder when helm micro-state is activated."
        :group 'spacemacs))

    :config
    (progn
      (helm-mode +1)
      (defun spacemacs//set-dotted-directory ()
        "Set the face of diretories for `.' and `..'"
        (set-face-attribute 'helm-ff-dotted-directory
                            nil
                            :foreground nil
                            :background nil
                            :inherit 'helm-ff-directory))
      (add-hook 'helm-find-files-before-init-hook 'spacemacs//set-dotted-directory)

      ;; alter helm-bookmark key bindings to be simpler
      (defun simpler-helm-bookmark-keybindings ()
        (define-key helm-bookmark-map (kbd "C-d") 'helm-bookmark-run-delete)
        (define-key helm-bookmark-map (kbd "C-e") 'helm-bookmark-run-edit)
        (define-key helm-bookmark-map (kbd "C-f") 'helm-bookmark-toggle-filename)
        (define-key helm-bookmark-map (kbd "C-o") 'helm-bookmark-run-jump-other-window)
        (define-key helm-bookmark-map (kbd "C-/") 'helm-bookmark-help))
      (add-hook 'helm-mode-hook 'simpler-helm-bookmark-keybindings)

      ;; helm navigation on hjkl
      (defun spacemacs//helm-hjkl-navigation (&optional arg)
        "Set navigation in helm on `jklh'.
ARG non nil means that the editing style is `vim'."
        (cond
         (arg
          (define-key helm-map (kbd "C-j") 'helm-next-line)
          (define-key helm-map (kbd "C-k") 'helm-previous-line)
          (define-key helm-map (kbd "C-h") 'helm-next-source)
          (define-key helm-map (kbd "C-l") 'helm-previous-source))
         (t
          (define-key helm-map (kbd "C-j") 'helm-execute-persistent-action)
          (define-key helm-map (kbd "C-k") 'helm-delete-minibuffer-contents)
          (define-key helm-map (kbd "C-h") nil)
          (define-key helm-map (kbd "C-l") 'helm-recenter-top-bottom-other-window))))
      (spacemacs//helm-hjkl-navigation (member dotspacemacs-editing-style '(vim hybrid)))

      (defun spacemacs/helm-edit ()
        "Switch in edit mode depending on the current helm buffer."
        (interactive)
        (cond
         ((string-equal "*helm-ag*" helm-buffer)
          (helm-ag-edit))))

      (defun spacemacs//helm-navigation-ms-on-enter ()
        "Initialization of helm micro-state."
        ;; faces
        (spacemacs//helm-navigation-ms-set-face)
        (setq spacemacs--helm-navigation-ms-face-cookie-minibuffer
              (face-remap-add-relative
               'minibuffer-prompt
               'spacemacs-helm-navigation-ms-face))
        ;; bind actions on numbers starting from 1 which executes action 0
        (dotimes (n 10)
          (define-key helm-map (number-to-string n)
            `(lambda () (interactive) (helm-select-nth-action
                                       ,(% (+ n 9) 10))))))

      (defun spacemacs//helm-navigation-ms-set-face ()
        "Set the face for helm header in helm navigation micro-state"
        (with-helm-window
          (setq spacemacs--helm-navigation-ms-face-cookie-header
                (face-remap-add-relative
                 'helm-header
                 'spacemacs-helm-navigation-ms-face))))

      (defun spacemacs//helm-navigation-ms-on-exit ()
        "Action to perform when exiting helm micro-state."
        ;; restore helm key map
        (dotimes (n 10) (define-key helm-map (number-to-string n) nil))
        ;; restore faces
        (with-helm-window
          (face-remap-remove-relative
           spacemacs--helm-navigation-ms-face-cookie-header))
        (face-remap-remove-relative
         spacemacs--helm-navigation-ms-face-cookie-minibuffer))

      (defun spacemacs//helm-navigation-ms-full-doc ()
        "Full documentation for helm navigation micro-state."
        "
  [?]          display this help
  [a]          toggle action selection page
  [e]          edit occurrences if supported
  [j] [k]      next/previous candidate
  [h] [l]      previous/next source
  [t]          toggle visible mark
  [T]          toggle all mark
  [v]          persistent action
  [q]          quit")

      (spacemacs|define-micro-state helm-navigation
        :persistent t
        :disable-evil-leader t
        :define-key (helm-map . "M-SPC") (helm-map . "s-M-SPC")
        :on-enter (spacemacs//helm-navigation-ms-on-enter)
        :on-exit  (spacemacs//helm-navigation-ms-on-exit)
        :bindings
        ("<tab>" helm-select-action :exit t)
        ("C-i" helm-select-action :exit t)
        ("<RET>" helm-maybe-exit-minibuffer :exit t)
        ("?" nil :doc (spacemacs//helm-navigation-ms-full-doc))
        ("a" helm-select-action :post (spacemacs//helm-navigation-ms-set-face))
        ("e" spacemacs/helm-edit)
        ("h" helm-previous-source)
        ("j" helm-next-line)
        ("k" helm-previous-line)
        ("l" helm-next-source)
        ("q" nil :exit t)
        ("t" helm-toggle-visible-mark)
        ("T" helm-toggle-all-marks)
        ("v" helm-execute-persistent-action))

      ;; Swap default TAB and C-z commands.
      ;; For GUI.
      (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
      ;; For terminal.
      (define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
      (define-key helm-map (kbd "C-z") 'helm-select-action)

      (eval-after-load "helm-mode" ; required
        '(spacemacs|hide-lighter helm-mode)))))

(defun spacemacs/init-helm-ag ()
  (use-package helm-ag
    :defer t
    :init
    (progn
      (defun spacemacs//helm-do-ag-region-or-symbol (func &optional dir)
        "Search with `ag' with a default input."
        (require 'helm-ag)
        (cl-letf* (((symbol-value 'helm-ag-insert-at-point) 'symbol)
                   ;; make thing-at-point choosing the active region first
                   ((symbol-function 'this-fn) (symbol-function 'thing-at-point))
                   ((symbol-function 'thing-at-point)
                    (lambda (thing)
                      (let ((res (if (region-active-p)
                          (buffer-substring-no-properties
                           (region-beginning) (region-end))
                          (this-fn thing))))
                        (when res (rxt-quote-pcre res))))))
          (funcall func dir)))

      (defun spacemacs//helm-do-search-find-tool (base tools default-inputp)
        "Create a cond form given a TOOLS string list and evaluate it."
        (eval
         `(cond
           ,@(mapcar
              (lambda (x)
                `((executable-find ,x)
                  ',(let ((func
                           (intern
                            (format (if default-inputp
                                        "spacemacs/%s-%s-region-or-symbol"
                                      "spacemacs/%s-%s")
                                    base x))))
                      (if (fboundp func)
                          func
                        (intern (format "%s-%s"  base x))))))
                     tools)
           (t 'helm-do-grep))))

      ;; Search in current file ----------------------------------------------

      (defun spacemacs/helm-file-do-ag (&optional _)
        "Wrapper to execute `helm-ag-this-file.'"
        (interactive)
        (helm-ag-this-file))

      (defun spacemacs/helm-file-do-ag-region-or-symbol ()
        "Search in current file with `ag' using a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-file-do-ag))

      (defun spacemacs/helm-file-smart-do-search (&optional default-inputp)
        "Search in current file using `dotspacemacs-search-tools'.
Search for a search tool in the order provided by `dotspacemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
        (interactive)
        (call-interactively
         (spacemacs//helm-do-search-find-tool "helm-file-do"
                                              dotspacemacs-search-tools
                                              default-inputp)))

      (defun spacemacs/helm-file-smart-do-search-region-or-symbol ()
        "Search in current file using `dotspacemacs-search-tools' with
 default input.
Search for a search tool in the order provided by `dotspacemacs-search-tools'."
        (interactive)
        (spacemacs/helm-file-smart-do-search t))

      ;; Search in files -----------------------------------------------------

      (defun spacemacs/helm-files-do-ag (&optional dir)
        "Search in files with `ag' using a default input."
        (interactive)
        (helm-do-ag dir))

      (defun spacemacs/helm-files-do-ag-region-or-symbol ()
        "Search in files with `ag' using a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-ag))

      (defun spacemacs/helm-files-do-ack (&optional dir)
        "Search in files with `ack'."
        (interactive)
        (let ((helm-ag-base-command "ack --nocolor --nogroup"))
          (helm-do-ag dir)))

      (defun spacemacs/helm-files-do-ack-region-or-symbol ()
        "Search in files with `ack' using a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-ack))

      (defun spacemacs/helm-files-do-pt (&optional dir)
        "Search in files with `pt'."
        (interactive)
        (let ((helm-ag-base-command "pt -e --nocolor --nogroup"))
          (helm-do-ag dir)))

      (defun spacemacs/helm-files-do-pt-region-or-symbol ()
        "Search in files with `pt' using a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-pt))

      (defun spacemacs/helm-files-smart-do-search (&optional default-inputp)
        "Search in opened buffers using `dotspacemacs-search-tools'.
Search for a search tool in the order provided by `dotspacemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
        (interactive)
        (call-interactively
         (spacemacs//helm-do-search-find-tool "helm-files-do"
                                              dotspacemacs-search-tools
                                              default-inputp)))

      (defun spacemacs/helm-files-smart-do-search-region-or-symbol ()
        "Search in opened buffers using `dotspacemacs-search-tools'.
with default input.
Search for a search tool in the order provided by `dotspacemacs-search-tools'."
        (interactive)
        (spacemacs/helm-files-smart-do-search t))

      ;; Search in buffers ---------------------------------------------------

      (defun spacemacs/helm-buffers-do-ag (&optional _)
        "Wrapper to execute `helm-ag-buffers.'"
        (interactive)
        (helm-do-ag-buffers))

      (defun spacemacs/helm-buffers-do-ag-region-or-symbol ()
        "Search in opened buffers with `ag' with a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-buffers-do-ag))

      (defun spacemacs/helm-buffers-do-ack (&optional _)
        "Search in opened buffers with `ack'."
        (interactive)
        (let ((helm-ag-base-command "ack --nocolor --nogroup"))
          (helm-do-ag-buffers)))

      (defun spacemacs/helm-buffers-do-ack-region-or-symbol ()
        "Search in opened buffers with `ack' with a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-buffers-do-ack))

      (defun spacemacs/helm-buffers-do-pt (&optional _)
        "Search in opened buffers with `pt'."
        (interactive)
        (let ((helm-ag-base-command "pt -e --nocolor --nogroup"))
          (helm-do-ag-buffers)))

      (defun spacemacs/helm-buffers-do-pt-region-or-symbol ()
        "Search in opened buffers with `pt' using a default input."
        (interactive)
        (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-buffers-do-pt))

      (defun spacemacs/helm-buffers-smart-do-search (&optional default-inputp)
        "Search in opened buffers using `dotspacemacs-search-tools'.
Search for a search tool in the order provided by `dotspacemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
        (interactive)
        (call-interactively
         (spacemacs//helm-do-search-find-tool "helm-buffers-do"
                                              dotspacemacs-search-tools
                                              default-inputp)))

      (defun spacemacs/helm-buffers-smart-do-search-region-or-symbol ()
        "Search in opened buffers using `dotspacemacs-search-tools' with
default input.
Search for a search tool in the order provided by `dotspacemacs-search-tools'."
        (interactive)
        (spacemacs/helm-buffers-smart-do-search t))

      ;; Search in project ---------------------------------------------------

      (defun spacemacs/helm-project-do-ag ()
        "Search in current project with `ag'."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (helm-do-ag dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-do-ag-region-or-symbol ()
        "Search in current project with `ag' using a default input."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (spacemacs//helm-do-ag-region-or-symbol 'helm-do-ag dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-do-ack ()
        "Search in current project with `ack'."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (spacemacs/helm-files-do-ack dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-do-ack-region-or-symbol ()
        "Search in current project with `ack' using a default input."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-ack dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-do-pt ()
        "Search in current project with `pt'."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (spacemacs/helm-files-do-pt dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-do-pt-region-or-symbol ()
        "Search in current project with `pt' using a default input."
        (interactive)
        (let ((dir (projectile-project-root)))
          (if dir
              (spacemacs//helm-do-ag-region-or-symbol 'spacemacs/helm-files-do-pt dir)
            (message "error: Not in a project."))))

      (defun spacemacs/helm-project-smart-do-search (&optional default-inputp)
        "Search in current project using `dotspacemacs-search-tools'.
Search for a search tool in the order provided by `dotspacemacs-search-tools'
If DEFAULT-INPUTP is non nil then the current region or symbol at point
are used as default input."
        (interactive)
        (let ((projectile-require-project-root nil))
         (call-interactively
          (spacemacs//helm-do-search-find-tool "helm-project-do"
                                               dotspacemacs-search-tools
                                               default-inputp))))

      (defun spacemacs/helm-project-smart-do-search-region-or-symbol ()
        "Search in current project using `dotspacemacs-search-tools' with
 default input.
Search for a search tool in the order provided by `dotspacemacs-search-tools'."
        (interactive)
        (spacemacs/helm-project-smart-do-search t))

      ;; evilify the helm-grep buffer
      (evilify helm-grep-mode helm-grep-mode-map
               (kbd "RET") 'helm-grep-mode-jump-other-window
               (kbd "q") 'quit-window)

      (evil-leader/set-key
        ;; opened buffers scope
        "sb"  'spacemacs/helm-buffers-smart-do-search
        "sB"  'spacemacs/helm-buffers-smart-do-search-region-or-symbol
        "sab" 'helm-do-ag-buffers
        "saB" 'spacemacs/helm-buffers-do-ag-region-or-symbol
        "skb" 'spacemacs/helm-buffers-do-ack
        "skB" 'spacemacs/helm-buffers-do-ack-region-or-symbol
        "stb" 'spacemacs/helm-buffers-do-pt
        "stB" 'spacemacs/helm-buffers-do-pt-region-or-symbol
        ;; current file scope
        "ss"  'spacemacs/helm-file-smart-do-search
        "sS"  'spacemacs/helm-file-smart-do-search-region-or-symbol
        "saa" 'helm-ag-this-file
        "saA" 'spacemacs/helm-file-do-ag-region-or-symbol
        ;; files scope
        "sf"  'spacemacs/helm-files-smart-do-search
        "sF"  'spacemacs/helm-files-smart-do-search-region-or-symbol
        "saf" 'helm-do-ag
        "saF" 'spacemacs/helm-files-do-ag-region-or-symbol
        "skf" 'spacemacs/helm-files-do-ack
        "skF" 'spacemacs/helm-files-do-ack-region-or-symbol
        "stf" 'spacemacs/helm-files-do-pt
        "stF" 'spacemacs/helm-files-do-pt-region-or-symbol
        ;; current project scope
        "/"   'spacemacs/helm-project-smart-do-search-region-or-symbol
        "sp"  'spacemacs/helm-project-smart-do-search
        "sP"  'spacemacs/helm-project-smart-do-search-region-or-symbol
        "sap" 'spacemacs/helm-project-do-ag
        "saP" 'spacemacs/helm-project-do-ag-region-or-symbol
        "skp" 'spacemacs/helm-project-do-ack
        "skP" 'spacemacs/helm-project-do-ack-region-or-symbol
        "stp" 'spacemacs/helm-project-do-pt
        "stP" 'spacemacs/helm-project-do-pt-region-or-symbol))
    :config
    (progn
      (evil-define-key 'normal helm-ag-map "SPC" evil-leader--default-map)
      (evilify helm-ag-mode helm-ag-mode-map
               (kbd "RET") 'helm-ag-mode-jump-other-window
               (kbd "q") 'quit-window))))

(defun spacemacs/init-helm-descbinds ()
  (use-package helm-descbinds
    :defer t
    :init
    (progn
      (setq helm-descbinds-window-style 'split)
      (add-hook 'helm-mode-hook 'helm-descbinds-mode)
      (evil-leader/set-key "?" 'helm-descbinds))))

(defun spacemacs/init-helm-make ()
  (use-package helm-make
    :defer t
    :init
    (evil-leader/set-key "hk" 'helm-make)))

(defun spacemacs/init-helm-mode-manager ()
  (use-package helm-mode-manager
    :defer t
    :init
    (evil-leader/set-key
      "hM"    'helm-switch-major-mode
      ;; "hm"    'helm-disable-minor-mode
      "h C-m" 'helm-enable-minor-mode)))

(defun spacemacs/init-helm-projectile ()
  (use-package helm-projectile
    :commands (helm-projectile-switch-to-buffer
               helm-projectile-find-dir
               helm-projectile-dired-find-dir
               helm-projectile-recentf
               helm-projectile-find-file
               helm-projectile-grep
               helm-projectile
               helm-projectile-switch-project)
    :init
    (progn
      (setq projectile-switch-project-action 'helm-projectile)

      (defconst spacemacs-use-helm-projectile t
        "This variable is only defined if helm-projectile is used.")

      ;; needed for smart search if user's default tool is grep
      (defalias 'spacemacs/helm-project-do-grep 'helm-projectile-grep)
      (defalias 'spacemacs/helm-project-do-grep-region-or-symbol 'helm-projectile-grep)

      (evil-leader/set-key
        "pb"  'helm-projectile-switch-to-buffer
        "pd"  'helm-projectile-find-dir
        "pf"  'helm-projectile-find-file
        "ph"  'helm-projectile
        "pp"  'helm-projectile-switch-project
        "pr"  'helm-projectile-recentf
        "pv"  'projectile-vc
        "sgp" 'helm-projectile-grep))))

(defun spacemacs/init-helm-spacemacs ()
  (use-package helm-spacemacs
    :commands helm-spacemacs
    :init
    (evil-leader/set-key "feh" 'helm-spacemacs)))

(defun spacemacs/init-helm-swoop ()
  (use-package helm-swoop
    :defer t
    :init
    (progn
      (setq helm-swoop-split-with-multiple-windows t
            helm-swoop-split-direction 'split-window-vertically
            helm-swoop-speed-or-color t
            helm-swoop-split-window-function 'helm-default-display-buffer
            helm-swoop-pre-input-function (lambda () ""))

      (defun spacemacs/helm-swoop-region-or-symbol ()
        "Call `helm-swoop' with default input."
        (interactive)
        (let ((helm-swoop-pre-input-function
               (lambda ()
                 (if (region-active-p)
                     (buffer-substring-no-properties (region-beginning)
                                                     (region-end))
                   (let ((thing (thing-at-point 'symbol t)))
                     (if thing thing ""))))))
          (call-interactively 'helm-swoop)))

      (evil-leader/set-key
        "ss"    'helm-swoop
        "sS"    'spacemacs/helm-swoop-region-or-symbol
        "s C-s" 'helm-multi-swoop-all)
      (defadvice helm-swoop (before add-evil-jump activate)
        (when (configuration-layer/package-usedp 'evil-jumper)
          (evil-set-jump))))))

(defun spacemacs/init-helm-themes ()
  (use-package helm-themes
    :defer t
    :init
    (evil-leader/set-key
      "Th" 'helm-themes)))

(defun spacemacs/init-highlight-indentation ()
  (use-package highlight-indentation
    :defer t
    :init
    (progn
      (spacemacs|add-toggle highlight-indentation
        :status highlight-indentation-mode
        :on (highlight-indentation-mode)
        :off (highlight-indentation-mode -1)
        :documentation "Highlight indentation levels."
        :evil-leader "thi")
      (spacemacs|add-toggle highlight-indentation-current-column
        :status highlight-indentation-current-column-mode
        :on (highlight-indentation-current-column-mode)
        :off (highlight-indentation-current-column-mode -1)
        :documentation "Highlight indentation level at point."
        :evil-leader "thc"))
    :config
    (progn
      (spacemacs|diminish highlight-indentation-mode " ⓗi" " hi")
      (spacemacs|diminish highlight-indentation-current-column-mode " ⓗc" " hc"))))

(defun spacemacs/init-highlight-numbers ()
  (use-package highlight-numbers
    :defer t
    :init
    (progn
      (add-hook 'prog-mode-hook 'highlight-numbers-mode)
      (add-hook 'asm-mode-hook (lambda () (highlight-numbers-mode -1))))))

(defun spacemacs/init-highlight-parentheses ()
  (use-package highlight-parentheses
    :defer t
    :init
    (progn
      (when (member dotspacemacs-highlight-delimiters '(all current))
        (add-hook 'prog-mode-hook #'highlight-parentheses-mode))
      (setq hl-paren-delay 0.2)
      (evil-leader/set-key "tCp" 'highlight-parentheses-mode)
      (setq hl-paren-colors '("Springgreen3"
                              "IndianRed1"
                              "IndianRed3"
                              "IndianRed4")))
    :config
    (spacemacs|hide-lighter highlight-parentheses-mode)
    (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)))

(defun spacemacs/init-hl-anything ()
  (use-package hl-anything
    :init
    (progn
      (hl-highlight-mode)
      (setq-default hl-highlight-save-file
                    (concat spacemacs-cache-directory ".hl-save"))
      (evil-leader/set-key
        "hc"  'hl-unhighlight-all-local
        "hC"  'hl-unhighlight-all-global
        "hh"  'hl-highlight-thingatpt-local
        "hH"  'hl-highlight-thingatpt-global
        "hn"  'hl-find-next-thing
        "hN"  'hl-find-prev-thing
        "hr"  'hl-restore-highlights
        "hs"  'hl-save-highlights))
    :config (spacemacs|hide-lighter hl-highlight-mode)))

(defun spacemacs/init-holy-mode ()
  (use-package holy-mode
    :commands holy-mode
    :init
    (progn
      (when (eq 'emacs dotspacemacs-editing-style)
        (holy-mode))
      (spacemacs|add-toggle holy-mode
        :status holy-mode
        :on (holy-mode)
        :off (holy-mode -1)
        :documentation "Globally toggle the holy mode."
        :evil-leader "E H"))))

(defun spacemacs/init-hungry-delete ()
  (use-package hungry-delete
    :defer t
    :init
    (spacemacs|add-toggle hungry-delete
      :status hungry-delete-mode
      :on (hungry-delete-mode)
      :off (hungry-delete-mode -1)
      :documentation "Delete consecutive horizontal whitespace with a single key.
Put (global-hungry-delete-mode) in dotspacemacs/config to enable by default."
      :evil-leader "td")
    :config
    (progn
      (setq-default hungry-delete-chars-to-skip " \t\f\v") ; only horizontal whitespace
      (define-key hungry-delete-mode-map (kbd "DEL") 'hungry-delete-backward)
      (define-key hungry-delete-mode-map (kbd "S-DEL") 'delete-backward-char))))

(defun spacemacs/init-hybrid-mode ()
  (use-package hybrid-mode
    :commands hybrid-mode
    :init
    (progn
      (add-to-list 'spacemacs-evil-cursor-colors
                   `(hybrid . ,(spacemacs/state-color 'emacs)))
      (spacemacs/set-state-faces)
      (setq hybrid-mode-insert-state-cursor
            (if (and (display-graphic-p)
                     dotspacemacs-colorize-cursor-according-to-state)
                `(,(spacemacs/state-color 'hybrid) (bar . 2))
              'bar))
      (copy-face 'spacemacs-insert-face 'spacemacs-insert-face-backup)
      (add-hook 'hybrid-mode-hook
                (lambda ()
                  (copy-face (if hybrid-mode
                                 'spacemacs-emacs-face
                               'spacemacs-insert-face-backup)
                             'spacemacs-insert-face)))
      (when (eq 'hybrid dotspacemacs-editing-style)
        (hybrid-mode))
      (spacemacs|add-toggle hybrid-mode
        :status hybrid-mode
        :on (hybrid-mode)
        :off (hybrid-mode -1)
        :documentation "Globally toggle hybrid mode."
        :evil-leader "E Y"))))

(defun spacemacs/init-ido-vertical-mode ()
  (use-package ido-vertical-mode
    :init
    (progn
      (ido-vertical-mode t)
      (when dotspacemacs-use-ido
        (evil-leader/set-key "ff" 'ido-find-file))
      (defun spacemacs//ido-minibuffer-setup ()
        "Setup the minibuffer."
        ;; Since ido is implemented in a while loop where each
        ;; iteration setup a whole new minibuffer, we have to keep
        ;; track of any activated ido navigation micro-state and force
        ;; the reactivation at each iteration.
        (when spacemacs--ido-navigation-ms-enabled
          (spacemacs/ido-navigation-micro-state)))
      (add-hook 'ido-minibuffer-setup-hook 'spacemacs//ido-minibuffer-setup)

      (defun spacemacs//ido-setup ()
        (when spacemacs--ido-navigation-ms-face-cookie-minibuffer
          (face-remap-remove-relative
           spacemacs--ido-navigation-ms-face-cookie-minibuffer))
        ;; be sure to wipe any previous micro-state flag
        (setq spacemacs--ido-navigation-ms-enabled nil)
        ;; overwrite the key bindings for ido vertical mode only
        (define-key ido-completion-map (kbd "C-<return>") 'ido-select-text)
        ;; use M-RET in terminal
        (define-key ido-completion-map "\M-\r" 'ido-select-text)
        (define-key ido-completion-map (kbd "C-h") 'ido-delete-backward-updir)
        (define-key ido-completion-map (kbd "C-j") 'ido-next-match)
        (define-key ido-completion-map (kbd "C-k") 'ido-prev-match)
        (define-key ido-completion-map (kbd "C-l") 'ido-exit-minibuffer)
        (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
        (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
        (define-key ido-completion-map (kbd "C-S-h") 'ido-prev-match-dir)
        (define-key ido-completion-map (kbd "C-S-j") 'next-history-element)
        (define-key ido-completion-map (kbd "C-S-k") 'previous-history-element)
        (define-key ido-completion-map (kbd "C-S-l") 'ido-next-match-dir)
        (define-key ido-completion-map (kbd "C-S-n") 'next-history-element)
        (define-key ido-completion-map (kbd "C-S-p") 'previous-history-element)
        ;; ido-other window maps
        (define-key ido-completion-map (kbd "C-o") 'spacemacs/ido-invoke-in-other-window)
        (define-key ido-completion-map (kbd "C-s") 'spacemacs/ido-invoke-in-vertical-split)
        (define-key ido-completion-map (kbd "C-t") 'spacemacs/ido-invoke-in-new-frame)
        (define-key ido-completion-map (kbd "C-v") 'spacemacs/ido-invoke-in-horizontal-split)
        ;; more natural navigation keys: up, down to change current item
        ;; left to go up dir
        ;; right to open the selected item
        (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
        (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
        (define-key ido-completion-map (kbd "<left>") 'ido-delete-backward-updir)
        (define-key ido-completion-map (kbd "<right>") 'ido-exit-minibuffer)
        ;; initiate micro-state
        (define-key ido-completion-map (kbd "M-SPC") 'spacemacs/ido-navigation-micro-state)
        (define-key ido-completion-map (kbd "s-M-SPC") 'spacemacs/ido-navigation-micro-state)
        )
      (add-hook 'ido-setup-hook 'spacemacs//ido-setup)

      (defvar spacemacs--ido-navigation-ms-enabled nil
        "Flag which is non nil when ido navigation micro-state is enabled.")

      (defvar spacemacs--ido-navigation-ms-face-cookie-minibuffer nil
        "Cookie pointing to the local face remapping.")

      (defface spacemacs-ido-navigation-ms-face
        `((t :background ,(face-attribute 'error :foreground)
             :foreground "black"
             :weight bold))
        "Face for ido minibuffer prompt when ido micro-state is activated."
        :group 'spacemacs)

      (defun spacemacs//ido-navigation-ms-set-face ()
        "Set faces for ido navigation micro-state."
        (setq spacemacs--ido-navigation-ms-face-cookie-minibuffer
              (face-remap-add-relative
               'minibuffer-prompt
               'spacemacs-ido-navigation-ms-face)))

      (defun spacemacs//ido-navigation-ms-on-enter ()
        "Initialization of ido micro-state."
        (setq spacemacs--ido-navigation-ms-enabled t)
        (spacemacs//ido-navigation-ms-set-face))

      (defun spacemacs//ido-navigation-ms-on-exit ()
        "Action to perform when exiting ido micro-state."
        (face-remap-remove-relative
         spacemacs--ido-navigation-ms-face-cookie-minibuffer))

      (defun spacemacs//ido-navigation-ms-full-doc ()
        "Full documentation for ido navigation micro-state."
        "
  [?]          display this help
  [e]          enter dired
  [j] [k]      next/previous match
  [J] [K]      sub/parent directory
  [h]          delete backward or parent directory
  [l]          select match
  [n] [p]      next/previous directory in history
  [o]          open in other window
  [s]          open in a new horizontal split
  [t]          open in other frame
  [v]          open in a new vertical split
  [q]          quit")

      (spacemacs|define-micro-state ido-navigation
        :persistent t
        :disable-evil-leader t
        :on-enter (spacemacs//ido-navigation-ms-on-enter)
        :on-exit  (spacemacs//ido-navigation-ms-on-exit)
        :bindings
        ("?" nil :doc (spacemacs//ido-navigation-ms-full-doc))
        ("<RET>" ido-exit-minibuffer :exit t)
        ("<escape>" nil :exit t)
        ("e" ido-select-text :exit t)
        ("h" ido-delete-backward-updir)
        ("j" ido-next-match)
        ("J" ido-next-match-dir)
        ("k" ido-prev-match)
        ("K" ido-prev-match-dir)
        ("l" ido-exit-minibuffer :exit t)
        ("n" ido-next-match-dir)
        ("o" spacemacs/ido-invoke-in-other-window :exit t)
        ("p" ido-prev-match-dir)
        ("q" nil :exit t)
        ("s" spacemacs/ido-invoke-in-vertical-split :exit t)
        ("t" spacemacs/ido-invoke-in-new-frame :exit t)
        ("v" spacemacs/ido-invoke-in-horizontal-split :exit t)))))

(defun spacemacs/init-iedit ()
  (use-package iedit
    :defer t
    :init
    (progn
      (setq iedit-current-symbol-default t
            iedit-only-at-symbol-boundaries t
            iedit-toggle-key-default nil))
    :config
    (progn
      (eval-after-load 'evil-iedit-state
        '(progn
           (define-key evil-iedit-state-map (kbd "gj")
             'ex-iedit-toggle-point-overlay-next-line)))

      (defun ex-iedit-toggle-point-overlay-next-line (count)
        (interactive "p")
        (unless (iedit-find-current-occurrence-overlay)
          (iedit-toggle-selection))
        (let ((i (when count count 1)))
          (save-excursion
            (evil-next-line)
            (while (and (> i 0)
                        (iedit-find-current-occurrence-overlay))
              (1- i)
              (evil-next-line))
            (when (not (iedit-find-current-occurrence-overlay))
              (dotimes (j (1+(- count i)))
                (iedit-toggle-selection)
                (evil-next-line))))))

      (defun iedit-toggle-selection ()
        "Override default iedit function to be able to add arbitrary overlays.

It will toggle the overlay under point or create an overlay of one character."
        (interactive)
        (iedit-barf-if-buffering)
        (let ((ov (iedit-find-current-occurrence-overlay)))
          (if ov
              (iedit-restrict-region (overlay-start ov) (overlay-end ov) t)
            (save-excursion
              (push (iedit-make-occurrence-overlay (point) (1+ (point)))
                    iedit-occurrences-overlays))
            (setq iedit-mode
                  (propertize
                   (concat " Iedit:" (number-to-string
                                      (length iedit-occurrences-overlays)))
                   'face 'font-lock-warning-face))
            (force-mode-line-update)))))))

(defun spacemacs/init-indent-guide ()
  (use-package indent-guide
    :defer t
    :init
    (progn
      (setq indent-guide-delay 0.3)
      (spacemacs|add-toggle indent-guide
        :status indent-guide-mode
        :on (indent-guide-mode)
        :off (indent-guide-mode -1)
        :documentation
        (concat "Enbale a guide to highlight "
                "the current indentation (alternative "
                "to the toggle"
                "highlight-indentation-current-column).")
        :evil-leader "ti")
      (spacemacs|add-toggle indent-guide-globally
        :status indent-guide-mode
        :on (indent-guide-global-mode)
        :off (indent-guide-global-mode -1)
        :documentation
        (concat "Enbale globally a guide to highlight "
                "the current indentation (alternative "
                "to the toggle"
                "highlight-indentation-current-column).")
        :evil-leader "t C-i"))
    :config
    (spacemacs|diminish indent-guide-mode " ⓘ" " i")))

(defun spacemacs/init-open-junk-file ()
  (use-package open-junk-file
    :defer t
    :commands (open-junk-file)
    :init
    (evil-leader/set-key "fJ" 'open-junk-file)
    (setq open-junk-file-directory (concat spacemacs-cache-directory "junk/"))))

(defun spacemacs/init-info+ ()
  (use-package info+
    :defer t
    :init
    (progn
      (eval-after-load 'info
        '(require 'info+))
      (setq Info-fontify-angle-bracketed-flag nil))))

(defun spacemacs/init-leuven-theme ()
  (use-package leuven-theme
    :defer t
    :init (setq org-fontify-whole-heading-line t)))

(defun spacemacs/init-spacemacs-theme ()
  (use-package spacemacs-theme
    :defer t
    :init
    (progn
      (setq spacemacs-theme-comment-bg t)
      (setq spacemacs-theme-org-height t))))

(defun spacemacs/init-linum-relative ()
  (use-package linum-relative
    :commands linum-relative-toggle
    :init
    (evil-leader/set-key "tr" 'linum-relative-toggle)
    :config
    (progn
      (setq linum-format 'linum-relative)
      (setq linum-relative-current-symbol "")
      (linum-relative-toggle))))

(defun spacemacs/init-move-text ()
  (use-package move-text
    :defer t
    :init
    (spacemacs|define-micro-state move-text
      :doc "[J] move down [K] move up"
        :use-minibuffer t
      :execute-binding-on-enter t
      :evil-leader "xJ" "xK"
      :bindings
      ("J" move-text-down)
      ("K" move-text-up))))

(defun spacemacs/init-neotree ()
  (use-package neotree
    :defer t
    :commands neo-global--window-exists-p
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

      (defun spacemacs//init-neotree ()
        "Initialize the neotree mode."
        )

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

      (defun spacemacs/neotree-collapse-or-up ()
        "Collapse an expanded directory node or go to the parent node."
        (interactive)
        (let ((node (neo-buffer--get-filename-current-line)))
          (when node
            (if (file-directory-p node)
                (if (neo-buffer--expanded-node-p node)
                    (spacemacs/neotree-collapse)
                  (neotree-select-up-node))
              (neotree-select-up-node)))))

      (defun neotree-find-project-root ()
        (interactive)
        (if (neo-global--window-exists-p)
            (neotree-hide)
          (neotree-find (projectile-project-root))))

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
        (define-key evil-motion-state-local-map (kbd "h")   'spacemacs/neotree-collapse-or-up)
        (define-key evil-motion-state-local-map (kbd "H")   'neotree-select-previous-sibling-node)
        (define-key evil-motion-state-local-map (kbd "J")   'neotree-select-down-node)
        (define-key evil-motion-state-local-map (kbd "K")   'neotree-select-up-node)
        (define-key evil-motion-state-local-map (kbd "l")   'spacemacs/neotree-expand-or-open)
        (define-key evil-motion-state-local-map (kbd "L")   'neotree-select-next-sibling-node)
        (define-key evil-motion-state-local-map (kbd "q")   'neotree-hide)
        (define-key evil-motion-state-local-map (kbd "r")   'neotree-rename-node)
        (define-key evil-motion-state-local-map (kbd "R")   'neotree-change-root)
        (define-key evil-motion-state-local-map (kbd "s")   'neotree-hidden-file-toggle))

      (evil-leader/set-key
        "ft" 'neotree-toggle
        "pt" 'neotree-find-project-root))

    :config
    (spacemacs/add-to-hook 'neotree-mode-hook '(spacemacs//init-neotree
                                                spacemacs//neotree-key-bindings))))

(defun spacemacs/init-page-break-lines ()
  (use-package page-break-lines
    :init
    (global-page-break-lines-mode t)
    (spacemacs|hide-lighter page-break-lines-mode)))

(defun spacemacs/init-pcre2el ()
  (use-package pcre2el
    :defer t
    :commands rxt-fontify-regexp-at-point
    :init
    (progn
      (spacemacs/declare-prefix "R" "pcre2el")
      (evil-leader/set-key
        "R/"  'rxt-explain
        "Rc"  'rxt-convert-syntax
        "Rx"  'rxt-convert-to-rx
        "R'"  'rxt-convert-to-strings
        "Rpe" 'rxt-pcre-to-elisp
        "R%"  'pcre-query-replace-regexp
        "Rpx" 'rxt-pcre-to-rx
        "Rps" 'rxt-pcre-to-sre
        "Rp'" 'rxt-pcre-to-strings
        "Rp/" 'rxt-explain-pcre
        "Re/" 'rxt-explain-elisp
        "Rep" 'rxt-elisp-to-pcre
        "Rex" 'rxt-elisp-to-rx
        "Res" 'rxt-elisp-to-sre
        "Re'" 'rxt-elisp-to-strings
        "Ret" 'rxt-toggle-elisp-rx
        "Rt"  'rxt-toggle-elisp-rx
        "Rh"  'rxt-fontify-regexp-at-point))))

(defun spacemacs/init-paradox ()
  (use-package paradox
    :commands paradox-list-packages
    :init
    (progn
      (setq paradox-execute-asynchronously nil)
      (defun spacemacs/paradox-list-packages ()
        "Load depdendencies for auth and open the package list."
        (interactive)
        (require 'epa-file)
        (require 'auth-source)
        (when (and (not (boundp 'paradox-github-token))
                   (file-exists-p "~/.authinfo.gpg"))
          (let ((authinfo-result (car (auth-source-search
                                       :max 1
                                       :host "github.com"
                                       :port "paradox"
                                       :user "paradox"
                                       :require '(:secret)))))
            (let ((paradox-token (plist-get authinfo-result :secret)))
              (setq paradox-github-token (if (functionp paradox-token)
                                             (funcall paradox-token)
                                           paradox-token)))))
        (paradox-list-packages nil))

      (evilify paradox-menu-mode paradox-menu-mode-map
               "H" 'paradox-menu-quick-help
               "J" 'paradox-next-describe
               "K" 'paradox-previous-describe
               "L" 'paradox-menu-view-commit-list
               "o" 'paradox-menu-visit-homepage)
      (evil-leader/set-key
        "aP" 'spacemacs/paradox-list-packages))))

(defun spacemacs/init-popup ()
  (use-package popup
    :defer t))

(defun spacemacs/init-popwin ()
  (use-package popwin
    :config
    (progn
      (popwin-mode 1)
      (evil-leader/set-key "wpm" 'popwin:messages)
      (evil-leader/set-key "wpp" 'popwin:close-popup-window)

      ;; don't use default value but manage it ourselves
      (setq popwin:special-display-config nil)

      ;; buffers that we manage
      (push '("*Help*"                 :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
      (push '("*compilation*"          :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
      (push '("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("*Async Shell Command*"  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '(" *undo-tree*"           :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
      (push '("*ert*"                  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("*grep*"                 :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("*nosetests*"            :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("^\*WoMan.+\*$" :regexp t             :position bottom                                   ) popwin:special-display-config)

      (defun spacemacs/remove-popwin-display-config (str)
        "Removes the popwin display configurations that matches the passed STR"
        (setq popwin:special-display-config
              (-remove (lambda (x) (if (and (listp x) (stringp (car x)))
                                       (string-match str (car x))))
                       popwin:special-display-config))))))

(defun spacemacs/init-powerline ()
  (use-package powerline
    :init
    (progn
      ;; Custom format of minor mode lighters, they are separated by a pipe.
      (defpowerline spacemacs-powerline-minor-modes
        (mapconcat (lambda (mm)
                     (propertize
                      mm
                      'mouse-face 'mode-line-highlight
                      'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes"
                      'local-map (let ((map (make-sparse-keymap)))
                                   (define-key map
                                     [mode-line down-mouse-1]
                                     (powerline-mouse 'minor 'menu mm))
                                   (define-key map
                                     [mode-line mouse-2]
                                     (powerline-mouse 'minor 'help mm))
                                   (define-key map
                                     [mode-line down-mouse-3]
                                     (powerline-mouse 'minor 'menu mm))
                                   (define-key map
                                     [header-line down-mouse-3]
                                     (powerline-mouse 'minor 'menu mm))
                                   map)))
                   (split-string (format-mode-line minor-mode-alist))
                   (concat (propertize
                            (if (dotspacemacs|symbol-value
                                 dotspacemacs-mode-line-unicode-symbols) " " "") 'face face)
                           (unless (dotspacemacs|symbol-value
                                    dotspacemacs-mode-line-unicode-symbols) "|"))))

      (defpowerline spacemacs-powerline-new-version
        (propertize
         spacemacs-version-check-lighter
         'mouse-face 'mode-line-highlight
         'help-echo (format "New version %s | Click with mouse-1 to update"
                            spacemacs-new-version)
         'local-map (let ((map (make-sparse-keymap)))
                      (define-key map
                        [mode-line down-mouse-1]
                        (lambda (event)
                          (interactive "@e")
                          (if (yes-or-no-p
                               (format (concat "Do you want to update to the newest "
                                               "version %s ?") spacemacs-new-version))
                              (progn
                                (spacemacs/switch-to-version spacemacs-new-version)
                                (setq spacemacs-mode-line-new-version-lighterp nil))
                            (message "Update aborted."))))
                      map)))

      (defvar spacemacs-mode-line-minor-modesp t
        "If not nil, minor modes lighter are displayed in the mode-line.")
      (spacemacs|add-toggle mode-line-minor-modes
        :status spacemacs-mode-line-minor-modesp
        :on (setq spacemacs-mode-line-minor-modesp t)
        :off (setq spacemacs-mode-line-minor-modesp nil)
        :documentation "Show minor modes in mode-line."
        :evil-leader "tmm")

      (defvar spacemacs-mode-line-major-modep t
        "If not nil, major mode is displayed in the mode-line.")
      (spacemacs|add-toggle mode-line-major-mode
        :status spacemacs-mode-line-major-modep
        :on (setq spacemacs-mode-line-major-modep t)
        :off (setq spacemacs-mode-line-major-modep nil)
        :documentation "Show major mode in mode-line."
        :evil-leader "tmM")

      (defvar spacemacs-mode-line-version-controlp t
        "If not nil, version control info is displayed in the mode-line.")
      (spacemacs|add-toggle mode-line-version-control
        :status spacemacs-mode-line-version-controlp
        :on (setq spacemacs-mode-line-version-controlp t)
        :off (setq spacemacs-mode-line-version-controlp nil)
        :documentation "Show version control info in mode-line."
        :evil-leader "tmv")

      (defvar spacemacs-mode-line-new-version-lighterp t
        "If not nil, new version lighter is displayed in the mode-line.")
      (spacemacs|add-toggle mode-line-new-version
        :status spacemacs-mode-line-new-version-lighterp
        :on (setq spacemacs-mode-line-new-version-lighterp t)
        :off (setq spacemacs-mode-line-new-version-lighterp nil)
        :documentation "Show new version in the mode-line."
        :evil-leader "tmV")

      (defvar spacemacs-mode-line-display-point-p nil
        "If not nil, display point alongside row/column in the mode-line.")
      (spacemacs|add-toggle mode-line-display-point
        :status spacemacs-mode-line-display-point-p
        :on (setq spacemacs-mode-line-display-point-p t)
        :off (setq spacemacs-mode-line-display-point-p nil)
        :documentation "Show point in the mode-line."
        :evil-leader "tmp")

      (defvar spacemacs-mode-line-org-clock-current-taskp nil
        "If not nil, the currently clocked org-mode task will be
displayed in the mode-line.")
      (defvar spacemacs-mode-line-org-clock-format-function
        'org-clock-get-clock-string
        "Function used to render the currently clocked org-mode task.")
      (spacemacs|add-toggle mode-line-org-clock-current-task
        :status spacemacs-mode-line-org-clock-current-taskp
        :on (setq spacemacs-mode-line-org-clock-current-taskp t)
        :off (setq spacemacs-mode-line-org-clock-current-taskp nil)
        :documentation "Show org clock in mode-line."
        :evil-leader "tmc")

      (defvar spacemacs-mode-line-left
        '(((workspace-number window-number)
           :fallback state-tag
           :separator "|"
           :face state-face)
          anzu
          (buffer-modified buffer-size buffer-id remote-host)
          major-mode
          ((flycheck-errors flycheck-warnings flycheck-infos)
           :when active)
          ((minor-modes process)
           :when active)
          (erc-track :when active)
          (version-control :when active)
          (org-pomodoro :when active)
          (org-clock :when active)
          nyan-cat)
        "List of modeline segments to render on the left. Each element
must be a valid segment specification, see documentation for
`spacemacs//eval-mode-line-segment'.")

      (defvar spacemacs-mode-line-right
        '((battery :when active)
          selection-info
          ((buffer-encoding-abbrev
            point-position
            line-column)
           :separator " | ")
          ((global-mode new-version)
           :when active)
          buffer-position
          hud)
        "List of modeline segments to render on the right. Each element
must be a valid segment specification, see documentation for
`spacemacs//eval-mode-line-segment'.")

      (defun spacemacs//mode-line-file-encoding ()
        "Return the file encoding to be displayed in the mode-line."
        (let ((buf-coding (format "%s" buffer-file-coding-system)))
          (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
              (match-string 1 buf-coding)
            buf-coding)))

      (if (display-graphic-p)
          (setq-default powerline-default-separator 'wave)
        (setq-default powerline-default-separator 'utf-8))

      (defun spacemacs/customize-powerline-faces ()
        "Alter powerline face to make them work with more themes."
        (set-face-attribute 'powerline-inactive2 nil
                            :inherit 'font-lock-comment-face))
      (spacemacs/customize-powerline-faces)

      (defmacro spacemacs|define-mode-line-segment (name value &rest props)
        "Defines a modeline segment called `NAME' whose value is
computed by the form `VALUE'. The optional keyword argument `WHEN'
defines a condition required for the segment to be shown.

This macro defines a function `spacemacs//mode-line-NAME' which
returns a list of modeline objects (strings or images). If the
form `VALUE' does not result in a list, the return value will be
wrapped as a singleton list.

All properties are stored in a plist attached to the symbol, to be
inspected at evaluation time by `spacemacs//eval-mode-line-segment'."
        (declare (indent 1))
        (let* ((wrapper-func (intern (format "spacemacs//mode-line-%S" name)))
               (wrapper-func-available (intern (format "%S-available" wrapper-func)))
               (condition (or (plist-get props :when) t)))
          `(progn
             (defun ,wrapper-func ()
               (when ,condition
                 (let ((value ,value))
                   (cond ((spacemacs//imagep value)
                          (list value))
                         ((listp value) value)
                         ((and (stringp value)
                               (= 0 (length value)))
                          nil)
                         (t (list value))))))
             (setplist ',wrapper-func ',props))))

      ;; An intermediate representation of the value of a modeline segment.
      (defstruct segment
        objects face-left face-right tight-left tight-right)

      (defun column-number-at-pos (pos)
        "Analog to line-number-at-pos."
        (save-excursion (goto-char pos) (current-column)))

      (defun selection-info ()
        "Info on the current selection for the mode-line.

It is a string holding:
- the number of columns in the selection if it covers only one line,
- the number of lines in the selection if if covers several full lines
- or rowsxcols if it's a block selection."
        (let* ((lines (count-lines (region-beginning) (min (1+ (region-end)) (point-max))))
               (chars (- (1+ (region-end)) (region-beginning)))
               (cols (1+ (abs (- (column-number-at-pos (region-end))
                                 (column-number-at-pos (region-beginning)))))))
          (if (eq evil-visual-selection 'block)
              (format "%d×%d block" lines cols)
            (if (> lines 1) (format "%d lines" lines)
              (format "%d chars" chars)))))

      ;; BEGIN define modeline segments

      (spacemacs|define-mode-line-segment workspace-number
        (spacemacs/workspace-number)
        :when (and (bound-and-true-p eyebrowse-mode)
                   (spacemacs/workspace-number)))

      (spacemacs|define-mode-line-segment window-number
        (spacemacs/window-number)
        :when (and (bound-and-true-p window-numbering-mode)
                   (spacemacs/window-number)))

      (spacemacs|define-mode-line-segment state-tag
        (s-trim (evil-state-property evil-state :tag t)))

      (spacemacs|define-mode-line-segment anzu
        (anzu--update-mode-line)
        :when (and active (bound-and-true-p anzu--state)))

      (spacemacs|define-mode-line-segment buffer-modified "%*")
      (spacemacs|define-mode-line-segment buffer-size
        (powerline-buffer-size))
      (spacemacs|define-mode-line-segment buffer-id
        (powerline-buffer-id))
      (spacemacs|define-mode-line-segment remote-host
        (concat "@" (file-remote-p default-directory 'host))
        :when (file-remote-p default-directory 'host))

      (spacemacs|define-mode-line-segment major-mode
        (powerline-major-mode)
        :when spacemacs-mode-line-major-modep)
      (spacemacs|define-mode-line-segment minor-modes
        (spacemacs-powerline-minor-modes)
        :when spacemacs-mode-line-minor-modesp)
      (spacemacs|define-mode-line-segment process
        (powerline-raw mode-line-process)
        :when (spacemacs//mode-line-nonempty mode-line-process))

      (spacemacs|define-mode-line-segment erc-track
        (let* ((buffers (mapcar 'car erc-modified-channels-alist))
               (long-names (mapconcat (lambda (buf)
                                        (or (buffer-name buf) ""))
                                      buffers " ")))
          long-names)
        :when (bound-and-true-p erc-track-mode))

      (spacemacs|define-mode-line-segment version-control
        (s-trim (powerline-vc))
        :when (and (powerline-vc)
                   spacemacs-mode-line-version-controlp))

      (spacemacs|define-mode-line-segment selection-info
        (selection-info)
        :when (evil-visual-state-p))

      (spacemacs|define-mode-line-segment buffer-encoding
        (format "%s" buffer-file-coding-system))
      (spacemacs|define-mode-line-segment buffer-encoding-abbrev
        (spacemacs//mode-line-file-encoding))

      (spacemacs|define-mode-line-segment point-position
        (format "%d" (point))
        :when spacemacs-mode-line-display-point-p)
      (spacemacs|define-mode-line-segment line-column "%l:%2c")
      (spacemacs|define-mode-line-segment buffer-position "%p")

      (spacemacs|define-mode-line-segment hud
        (powerline-hud state-face default-face)
        :tight t
        :when (string-match "\%" (format-mode-line "%p")))

      (spacemacs|define-mode-line-segment nyan-cat
        (powerline-raw (nyan-create) default-face)
        :when (bound-and-true-p nyan-mode))

      (spacemacs|define-mode-line-segment global-mode
        (powerline-raw (-difference global-mode-string
                                    spacemacs--global-mode-line-excludes))
        :when (spacemacs//mode-line-nonempty global-mode-string))

      (spacemacs|define-mode-line-segment battery
        (powerline-raw (s-trim (fancy-battery-default-mode-line))
                       (fancy-battery-powerline-face))
        :when (bound-and-true-p fancy-battery-mode))

      (spacemacs|define-mode-line-segment new-version
        (spacemacs-powerline-new-version
         (spacemacs/get-new-version-lighter-face
          spacemacs-version spacemacs-new-version))
        :when (and spacemacs-new-version
                   spacemacs-mode-line-new-version-lighterp))

      ;; flycheck-errors, flycheck-warnings, flycheck-infos
      (dolist (type '(error warning info))
        (let ((segment-name (intern (format "flycheck-%ss" type)))
              (face (intern (format "spacemacs-mode-line-flycheck-%s-face" type))))
          (eval
           `(spacemacs|define-mode-line-segment ,segment-name
              (powerline-raw (s-trim (spacemacs|custom-flycheck-lighter ,type)) ',face)
              :when (and (bound-and-true-p flycheck-mode)
                         (or flycheck-current-errors
                             (eq 'running flycheck-last-status-change))
                         (spacemacs|custom-flycheck-lighter ,type))))))

      (spacemacs|define-mode-line-segment org-clock
        (substring-no-properties (funcall spacemacs-mode-line-org-clock-format-function))
        :when (and spacemacs-mode-line-org-clock-current-taskp
                   (fboundp 'org-clocking-p)
                   (org-clocking-p)))
      (push 'org-mode-line-string spacemacs--global-mode-line-excludes)

      (spacemacs|define-mode-line-segment org-pomodoro
        (nth 1 org-pomodoro-mode-line)
        :when (and (fboundp 'org-pomodoro-active-p)
                   (org-pomodoro-active-p)))
      (push 'org-pomodoro-mode-line spacemacs--global-mode-line-excludes)

      ;; END define modeline segments

      (defun spacemacs//eval-mode-line-segment (segment-spec &rest outer-props)
        "Evaluates a modeline segment given by `SEGMENT-SPEC' with
additional properties given by `OUTER-PROPS'.

`SEGMENT-SPEC' may be either:
- A literal value (number or string, for example)
- A symbol previously defined by `spacemacs|define-mode-line-segment'
- A list whose car is a segment-spec and whose cdr is a plist of properties
- A list of segment-specs

The properties applied are, in order of priority:
- Those given by `SEGMENT-SPEC', if applicable
- The properties attached to the segment symbol, if applicable
- `OUTER-PROPS'

Valid properties are:
- `:tight-left' => if true, the segment should be rendered with no padding
  or separator on its left side
- `:tight-right' => corresponding option for the right side
- `:tight' => shorthand option to set both `:tight-left' and `:tight-right'
- `:when' => condition that determines whether this segment is shown
- `:fallback' => segment to evaluate if this segment produces no output
- `:separator' => string with which to separate nested segments
- `:face' => the face with which to render the segment

When calling nested or fallback segments, the full property list is passed
as `OUTER-PROPS', with the exception of `:fallback'. This means that more
deeply specified properties, as a rule, override the higher level ones.
The exception is `:when', which must be true at all levels.

The return vaule is a `segment' struct. Its `OBJECTS' list may be nil."

        ;; We get a property list from `SEGMENT-SPEC' if it's a list
        ;; with more than one element whose second element is a symbol
        ;; starting with a colon
        (let* ((input (if (and (listp segment-spec)
                               (cdr segment-spec)
                               (keywordp (cadr segment-spec)))
                          segment-spec
                        (cons segment-spec nil)))
               (segment (car input))
               (segment-symbol (when (symbolp segment)
                                 (intern (format "spacemacs//mode-line-%S" segment))))

               ;; Assemble the properties in the correct order
               (props (append (cdr input)
                              (when (symbolp segment) (symbol-plist segment-symbol))
                              outer-props))

               ;; Property list to be passed to nested or fallback segments
               (nest-props (append '(:fallback nil) (cdr input) outer-props))

               ;; Parse property list
               (condition (if (plist-member props :when)
                              (eval (plist-get props :when))
                            t))
               (face (eval (or (plist-get props :face) 'default-face)))
               (separator (powerline-raw (or (plist-get props :separator) " ") face))
               (tight-left (or (plist-member props :tight)
                               (plist-member props :tight-left)))
               (tight-right (or (plist-member props :tight)
                                (plist-member props :tight-right)))

               ;; Final output
               (result (make-segment :objects nil
                                     :face-left face
                                     :face-right face
                                     :tight-left tight-left
                                     :tight-right tight-right)))

          ;; Evaluate the segment based on its type
          (when condition
            (cond
             ;; A list of segments
             ((listp segment)
              (let ((results (remove-if-not
                              'segment-objects
                              (mapcar (lambda (s)
                                        (apply 'spacemacs//eval-mode-line-segment
                                               s nest-props))
                                      segment))))
                (when results
                  (setf (segment-objects result)
                        (apply 'append (spacemacs//intersperse
                                        (mapcar 'segment-objects results)
                                        (list separator))))
                  (setf (segment-face-left result)
                        (segment-face-left (car results)))
                  (setf (segment-face-right result)
                        (segment-face-right (car (last results))))
                  (setf (segment-tight-left result)
                        (segment-tight-left (car results)))
                  (setf (segment-tight-right result)
                        (segment-tight-right (car (last results)))))))
             ;; A single symbol
             ((symbolp segment)
              (setf (segment-objects result)
                    (mapcar (lambda (s)
                              (if (spacemacs//imagep s) s (powerline-raw s face)))
                            (funcall segment-symbol))))
             ;; A literal value
             (t (setf (segment-objects result)
                      (list (powerline-raw (format "%s" segment) face))))))

          (cond
           ;; This segment produced output, so return it
           ((segment-objects result) result)
           ;; Return the fallback segment, if any
           ((plist-get props :fallback)
            (apply 'spacemacs//eval-mode-line-segment
                   (plist-get props :fallback) nest-props))
           ;; No output (objects = nil)
           (t result))))

      (defun spacemacs//mode-line-prepare-any (spec side)
        "Prepares one side of the modeline. `SPEC' is a list of segment
specifications (see `spacemacs//eval-mode-line-segment'), and `SIDE' is
one of `l' or `r'."
        (let* ((active (powerline-selected-window-active))
               (line-face (if active 'powerline-active2 'powerline-inactive2))
               (default-face (if active 'powerline-active1 'powerline-inactive1))
               (other-face (if active 'mode-line 'mode-line-inactive))
               (state-face (if active (spacemacs/current-state-face) line-face))

               ;; Loop through the segments and collect the results
               (segments (loop with result
                               for s in spec
                               do (setq result (spacemacs//eval-mode-line-segment s))
                               if (segment-objects result)
                                 collect result
                                 and do (rotatef default-face other-face)))

               (dummy (make-segment :face-left line-face :face-right line-face))
               (separator-style (format "powerline-%S" powerline-default-separator))
               (default-separator (intern (format "%s-%S" separator-style
                                                  (car powerline-default-separator-dir))))
               (other-separator (intern (format "%s-%S" separator-style
                                                (cdr powerline-default-separator-dir)))))

          ;; Collect all segment values and add separators
          (apply 'append
                 (mapcar
                  (lambda (pair)
                    (let* ((lhs (car pair))
                           (rhs (cdr pair))
                           (objs (if (eq 'l side) lhs rhs))
                           (add-sep (not (or (segment-tight-right lhs)
                                             (segment-tight-left rhs)))))
                      (rotatef default-separator other-separator)
                      (append
                       (when (and (eq 'r side) add-sep)
                         (list (funcall default-separator
                                        (segment-face-right lhs)
                                        (segment-face-left rhs))))
                       (unless (segment-tight-left objs)
                         (list (powerline-raw " " (segment-face-left objs))))
                       (segment-objects objs)
                       (unless (segment-tight-right objs)
                         (list (powerline-raw " " (segment-face-right objs))))
                       (when (and (eq 'l side) add-sep)
                         (list (funcall default-separator
                                        (segment-face-right lhs)
                                        (segment-face-left rhs)))))))
                  (-zip (if (eq 'l side) segments (cons dummy segments))
                        (if (eq 'l side) (append (cdr segments) (list dummy)) segments))))))

      (defun spacemacs//mode-line-prepare-left ()
        (spacemacs//mode-line-prepare-any spacemacs-mode-line-left 'l))

      (defun spacemacs//mode-line-prepare-right ()
        (spacemacs//mode-line-prepare-any spacemacs-mode-line-right 'r))

      (defun spacemacs//mode-line-prepare ()
        ;; diminish the lighters
        (when spacemacs-mode-line-minor-modesp
          (let ((unicodep (dotspacemacs|symbol-value
                           dotspacemacs-mode-line-unicode-symbols)))
            (dolist (mm spacemacs--diminished-minor-modes)
              (let ((mode (car mm)))
                (when (and (boundp mode) (symbol-value mode))
                  (let* ((unicode (cadr mm))
                         (ascii (caddr mm))
                         (dim (if unicodep
                                  unicode
                                (if ascii ascii unicode))))
                    (diminish mode dim)))))))
        (let* ((active (powerline-selected-window-active))
               (lhs (spacemacs//mode-line-prepare-left))
               (rhs (spacemacs//mode-line-prepare-right))
               (line-face (if active 'powerline-active2 'powerline-inactive2)))
          ;; create the line
          (concat (powerline-render lhs)
                  (powerline-fill line-face (powerline-width rhs))
                  (powerline-render rhs))))

      (setq-default mode-line-format
                    '("%e" (:eval (spacemacs//mode-line-prepare))))

      (defun spacemacs//restore-powerline (buffer)
        "Restore the powerline in buffer"
        (with-current-buffer buffer
              (setq-local mode-line-format
                          '("%e" (:eval (spacemacs//mode-line-prepare))))
              (powerline-set-selected-window)
              (powerline-reset)))

      (defun spacemacs//set-powerline-for-startup-buffers ()
        "Set the powerline for buffers created when Emacs starts."
        (unless configuration-layer-error-count
          (dolist (buffer '("*Messages*" "*spacemacs*" "*Compile-Log*"))
            (when (and (get-buffer buffer)
                       (configuration-layer/package-usedp 'powerline))
              (spacemacs//restore-powerline buffer)))))
      (add-hook 'emacs-startup-hook
                'spacemacs//set-powerline-for-startup-buffers))))

(defun spacemacs/init-projectile ()
  (use-package projectile
    :commands (projectile-ack
               projectile-ag
               projectile-compile-project
               projectile-dired
               projectile-grep
               projectile-find-dir
               projectile-find-file
               projectile-find-tag
               projectile-find-test-file
               projectile-invalidate-cache
               projectile-kill-buffers
               projectile-multi-occur
               projectile-project-root
               projectile-recentf
               projectile-regenerate-tags
               projectile-replace
               projectile-run-async-shell-command-in-root
               projectile-run-shell-command-in-root
               projectile-switch-project
               projectile-switch-to-buffer
               projectile-vc)
    :init
    (progn
      ;; note for Windows: GNU find or Cygwin find must be in path
      ;; default parameters are not supported on Windows, we default
      ;; to simplest call to find.
      (when (spacemacs/system-is-mswindows)
        (setq projectile-generic-command "find . -type f"))
      (setq projectile-enable-caching t
            projectile-indexing-method 'alien
            projectile-sort-order 'recentf
            projectile-cache-file (concat spacemacs-cache-directory
                                          "projectile.cache")
            projectile-known-projects-file (concat spacemacs-cache-directory
                                                   "projectile-bookmarks.eld"))
      (unless (configuration-layer/package-usedp 'helm-projectile)
        (evil-leader/set-key
          "pb" 'projectile-switch-to-buffer
          "pd" 'projectile-find-dir
          "pf" 'projectile-find-file
          "ph" 'helm-projectile
          "pr" 'projectile-recentf
          "ps" 'projectile-switch-project))
      (evil-leader/set-key
        "p!" 'projectile-run-shell-command-in-root
        "p&" 'projectile-run-async-shell-command-in-root
        "pa" 'projectile-toggle-between-implementation-and-test
        "pc" 'projectile-compile-project
        "pD" 'projectile-dired
        "pG" 'projectile-regenerate-tags
        "pI" 'projectile-invalidate-cache
        "pk" 'projectile-kill-buffers
        "po" 'projectile-multi-occur
        "pR" 'projectile-replace
        "pT" 'projectile-find-test-file
        "py" 'projectile-find-tag))
    :config
    (progn
      (projectile-global-mode)
      (spacemacs|hide-lighter projectile-mode))))

(defun spacemacs/init-quelpa ())

(defun spacemacs/init-rainbow-delimiters ()
  (use-package rainbow-delimiters
    :defer t
    :init
    (progn
      (evil-leader/set-key "tCd" 'rainbow-delimiters-mode)
      (when (member dotspacemacs-highlight-delimiters '(any all))
        (spacemacs/add-to-hooks 'rainbow-delimiters-mode '(prog-mode-hook))))))

(defun spacemacs/init-recentf ()
  (use-package recentf
    :defer t
    :init
    ;; lazy load recentf
    (add-hook 'find-file-hook (lambda () (unless recentf-mode
                                           (recentf-mode)
                                           (recentf-track-opened-file))))
    :config
    (add-to-list 'recentf-exclude (expand-file-name spacemacs-cache-directory))
    (add-to-list 'recentf-exclude (expand-file-name package-user-dir))
    (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
    (setq recentf-save-file (concat spacemacs-cache-directory "recentf"))
    (setq recentf-max-saved-items 100)
    (setq recentf-auto-cleanup 'never)
    (setq recentf-auto-save-timer (run-with-idle-timer 600 t 'recentf-save-list))))

(defun spacemacs/init-rfringe ()
  (use-package rfringe
    :defer t))

(defun spacemacs/init-savehist ()
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

(defun spacemacs/init-saveplace ()
  (use-package saveplace
    :init
    (progn
      ;; Save point position between sessions
      (setq save-place t
            save-place-file (concat spacemacs-cache-directory "places")))))

(defun spacemacs/init-smartparens ()
  (use-package smartparens
    :defer t
    :init
    (progn
      (spacemacs/add-to-hooks (if dotspacemacs-smartparens-strict-mode
                                  'smartparens-strict-mode
                                'smartparens-mode)
                              '(prog-mode-hook))

      ;; enable smartparens-mode in `eval-expression'
      (defun conditionally-enable-smartparens-mode ()
        "Enable `smartparens-mode' in the minibuffer, during `eval-expression'."
        (if (eq this-command 'eval-expression)
            (smartparens-mode)))

      (add-hook 'minibuffer-setup-hook 'conditionally-enable-smartparens-mode)

      (spacemacs|add-toggle smartparens
        :status smartparens-mode
        :on (smartparens-mode)
        :off (smartparens-mode -1)
        :documentation "Enable smartparens."
        :evil-leader "tp")

      (spacemacs|add-toggle smartparens-globally
        :status smartparens-mode
        :on (smartparens-global-mode)
        :off (smartparens-global-mode -1)
        :documentation "Enable smartparens globally."
        :evil-leader "t C-p")

      (setq sp-show-pair-delay 0.2
            ;; fix paren highlighting in normal mode
            sp-show-pair-from-inside (not evil-move-beyond-eol)
            sp-cancel-autoskip-on-backward-movement nil))
    :config
    (progn
      (require 'smartparens-config)
      (spacemacs|diminish smartparens-mode " ⓟ" " p")

      (show-smartparens-global-mode +1)

      (defun spacemacs/smartparens-pair-newline (id action context)
        (save-excursion
          (newline)
          (indent-according-to-mode)))

      (defun spacemacs/smartparens-pair-newline-and-indent (id action context)
        (spacemacs/smartparens-pair-newline id action context)
        (indent-according-to-mode))

      ;; don't create a pair with single quote in minibuffer
      (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

      (sp-pair "{" nil :post-handlers
               '(:add (spacemacs/smartparens-pair-newline-and-indent "RET")))
      (sp-pair "[" nil :post-handlers
               '(:add (spacemacs/smartparens-pair-newline-and-indent "RET"))))))

(defun spacemacs/init-smooth-scrolling ()
  (if dotspacemacs-smooth-scrolling
      (use-package smooth-scrolling
        :init
        (setq smooth-scroll-margin 5
              scroll-conservatively 101
              scroll-preserve-screen-position t
              auto-window-vscroll nil)
        :config
        (setq scroll-margin 5))

    ;; deactivate the defadvice's
    (ad-disable-advice 'previous-line 'after 'smooth-scroll-down)
    (ad-activate 'previous-line)
    (ad-disable-advice 'next-line 'after 'smooth-scroll-up)
    (ad-activate 'next-line)
    (ad-disable-advice 'isearch-repeat 'after 'isearch-smooth-scroll)
    (ad-activate 'isearch-repeat)))

(defun spacemacs/init-solarized-theme ()
  (use-package solarized
    :init
    (progn
      (deftheme solarized-dark "The dark variant of the Solarized colour theme")
      (deftheme solarized-light "The light variant of the Solarized colour theme"))))

(defun spacemacs/init-spray ()
  (use-package spray
    :commands spray-mode
    :init
    (progn
      (defun spacemacs/start-spray ()
        "Start spray speed reading on current buffer at current point."
        (interactive)
        (evil-insert-state)
        (spray-mode t)
        (evil-insert-state-cursor-hide))
      (evil-leader/set-key "asr" 'spacemacs/start-spray)

      (defadvice spray-quit (after spacemacs//quit-spray activate)
        "Correctly quit spray."
        (set-default-evil-insert-state-cursor)
        (evil-normal-state)))
    :config
    (progn
      (define-key spray-mode-map (kbd "h") 'spray-backward-word)
      (define-key spray-mode-map (kbd "l") 'spray-forward-word)
      (define-key spray-mode-map (kbd "q") 'spray-quit))))

(defun spacemacs/init-subword ()
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
          :documentation "Toggle CamelCase motion."
          :evil-leader "tc")
        (spacemacs|add-toggle camel-case-motion-globally
          :status subword-mode
          :on (global-subword-mode +1)
          :off (global-subword-mode -1)
          :documentation "Globally toggle CamelCase motion."
          :evil-leader "t C-c"))
      :config
      (spacemacs|diminish subword-mode " ⓒ" " c"))))

(defun spacemacs/init-undo-tree ()
  (use-package undo-tree
    :init
    (global-undo-tree-mode)
    ;; (setq undo-tree-auto-save-history t
    ;;       undo-tree-history-directory-alist
    ;;       `(("." . ,(concat spacemacs-cache-directory "undo"))))
    ;; (unless (file-exists-p (concat spacemacs-cache-directory "undo"))
    ;;     (make-directory (concat spacemacs-cache-directory "undo")))
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)
    :config
    (spacemacs|hide-lighter undo-tree-mode)))

(defun spacemacs/init-use-package ())

(defun spacemacs/init-vi-tilde-fringe ()
  (use-package vi-tilde-fringe
    :if window-system
    :init
    (progn
      (global-vi-tilde-fringe-mode)
      (spacemacs|add-toggle vi-tilde-fringe
        :status vi-tilde-fringe-mode
        :on (global-vi-tilde-fringe-mode)
        :off (global-vi-tilde-fringe-mode -1)
        :documentation
        (concat "Globally display a ~ on "
                "empty lines in the fringe.")
        :evil-leader "t~")
      ;; don't enable it on spacemacs home buffer
      (with-current-buffer  "*spacemacs*"
        (vi-tilde-fringe-mode -1))
      ;; after a major mode is loaded, check if the buffer is read only
      ;; if so, disable vi-tilde-fringe-mode
      (add-hook 'after-change-major-mode-hook (lambda ()
                                                (when buffer-read-only
                                                  (vi-tilde-fringe-mode -1)))))
    :config
    (spacemacs|hide-lighter vi-tilde-fringe-mode)))

(defun spacemacs/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :defer t
    ;; no shortcut for now (used by registers)
    ;; :init
    ;; (evil-leader/set-key
    ;;   "rR" 'vr/query-replace
    ;;   "rr" 'vr/replace)
    ))

(defun spacemacs/init-volatile-highlights ()
  (use-package volatile-highlights
    :config
    (progn
      (volatile-highlights-mode t)
      (spacemacs|hide-lighter volatile-highlights-mode))))

(defun spacemacs/init-wand ()
  (use-package wand
    :disabled t
    :init
    (progn
      (require 'wand)
      (wand:add-rule (wand:create-rule :match "https?://"
                                       :capture :whole
                                       :action message))
      (evil-leader/set-key "RET" 'wand:execute))))

(defun spacemacs/init-whitespace ()
  (use-package whitespace
    :defer t
    :init
    (progn
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
        :documentation "Globally display whitespace."
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
                          :foreground (face-attribute 'font-lock-warning-face :foreground))
      (set-face-attribute 'whitespace-tab nil
                          :background nil)
      (set-face-attribute 'whitespace-indentation nil
                          :background nil)
      (spacemacs|diminish whitespace-mode " ⓦ" " w")
      (spacemacs|diminish global-whitespace-mode " Ⓦ" " W"))))

(defun spacemacs/init-which-key ()
  (use-package which-key
    :init
    (progn
      (spacemacs|add-toggle which-key
        :status which-key-mode
        :on (which-key-mode)
        :off (which-key-mode -1)
        :documentation
        "Display a buffer with available key bindings."
        :evil-leader "tK")
      ;; (defadvice which-key--update
      ;;     (around spacemacs/inhibit-which-key-buffer activate)
      ;;   "Prevent the popup of the which-key buffer in some case."
      ;;   ;; a micro-state is running
      ;;   ;; or
      ;;   ;; bzg-big-fringe-mode is on
      ;;   (if (or overriding-terminal-local-map
      ;;           bzg-big-fringe-mode)
      ;;       (let ((which-key-inhibit t)) ad-do-it)
      ;;     ad-do-it))
      (let ((new-descriptions
             ;; being higher in this list means the replacement is applied later
             '(
               ("spacemacs/\\(.+\\)" . "\\1")
               ("spacemacs/toggle-\\(.+\\)" . "\\1")
               ("select-window-\\([0-9]\\)" . "window \\1")
               ("spacemacs/alternate-buffer" . "last buffer")
               ("avy-goto-word-or-subword-1" . "avy word")
               ("shell-command" . "shell cmd")
               ("spacemacs/default-pop-shell" . "open shell")
               ("spacemacs/helm-project-smart-do-search-region-or-symbol" . "smart search")
               ("helm-descbinds" . "show keybindings")
               ("sp-split-sexp" . "split sexp")
               ("avy-goto-line" . "avy line")
               ("universal-argument" . "universal arg")
               ("er/expand-region" . "expand region")
               ("helm-apropos" . "apropos"))))
        (dolist (nd new-descriptions)
          ;; ensure the target matches the whole string
          (push (cons (concat "\\`" (car nd) "\\'") (cdr nd))
                which-key-description-replacement-alist)))
      (dolist (leader-key `(,dotspacemacs-leader-key ,dotspacemacs-emacs-leader-key))
        (which-key-add-key-based-replacements
         (concat leader-key " m")    "major mode commands"
         (concat leader-key " " dotspacemacs-command-key) "M-x"))
      ;; disable special key handling for spacemacs, since it can be
      ;; disorienting if you don't understand it
      (setq which-key-prefix-title-alist
            `((,(listify-key-sequence
                 (kbd (concat dotspacemacs-leader-key " m"))) . "Major mode commands")
              (,(listify-key-sequence
                 (kbd (concat dotspacemacs-emacs-leader-key " m"))) . "Major mode commands")
              (,(listify-key-sequence
                 (kbd dotspacemacs-leader-key)) . "Spacemacs root")
              (,(listify-key-sequence
                 (kbd dotspacemacs-emacs-leader-key)) . "Spacemacs root")))
      (nconc which-key-prefix-title-alist spacemacs/prefix-titles)
      (setq which-key-special-keys nil
            which-key-use-C-h-for-paging t
            which-key-echo-keystrokes 0.02
            which-key-max-description-length 32)
      (which-key-mode)
      (spacemacs|diminish which-key-mode " Ⓚ" " K"))))

(defun spacemacs/init-window-numbering ()
  (use-package window-numbering
    :config
    (progn
      (when (configuration-layer/package-usedp 'powerline)
        (defun window-numbering-install-mode-line (&optional position)
          "Do nothing, the display is handled by the powerline."))
      (setq window-numbering-auto-assign-0-to-minibuffer nil)
      (evil-leader/set-key
        "0" 'select-window-0
        "1" 'select-window-1
        "2" 'select-window-2
        "3" 'select-window-3
        "4" 'select-window-4
        "5" 'select-window-5
        "6" 'select-window-6
        "7" 'select-window-7
        "8" 'select-window-8
        "9" 'select-window-9)
      (window-numbering-mode 1))

    (defun spacemacs/window-number ()
      "Return the number of the window."
      (let* ((num (window-numbering-get-number))
             (str (if num (int-to-string num))))
        (cond
         ((not (dotspacemacs|symbol-value
                dotspacemacs-mode-line-unicode-symbols)) str)
         ((equal str "1")  "➊")
         ((equal str "2")  "➋")
         ((equal str "3")  "➌")
         ((equal str "4")  "➍")
         ((equal str "5")  "➎")
         ((equal str "6")  "❻")
         ((equal str "7")  "➐")
         ((equal str "8")  "➑")
         ((equal str "9")  "➒")
         ((equal str "0")  "➓"))))

    (defun spacemacs//window-numbering-assign (windows)
      "Custom number assignment for special buffers."
      (mapc (lambda (w)
              (when (and (boundp 'neo-global--window)
                         (eq w neo-global--window))
                (window-numbering-assign w 0)))
            windows))
    (add-hook 'window-numbering-before-hook 'spacemacs//window-numbering-assign)))

(defun spacemacs/init-winner ()
  (use-package winner
    :init
    (progn
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

(defun spacemacs/init-zoom-frm ()
  (use-package zoom-frm
    :commands (zoom-frm-unzoom
               zoom-frm-out
               zoom-frm-in)
    :init
    (progn
      (spacemacs|define-micro-state zoom-frm
        :doc "[+] zoom frame in [-] zoom frame out [=] reset zoom"
        :evil-leader "zf"
        :use-minibuffer t
        :bindings
        ("+" spacemacs/zoom-frm-in :post (spacemacs//zoom-frm-powerline-reset))
        ("-" spacemacs/zoom-frm-out :post (spacemacs//zoom-frm-powerline-reset))
        ("=" spacemacs/zoom-frm-unzoom :post (spacemacs//zoom-frm-powerline-reset)))

      (defun spacemacs//zoom-frm-powerline-reset ()
        (when (fboundp 'powerline-reset)
          (setq-default powerline-height (spacemacs/compute-powerline-height))
          (powerline-reset)))

      (defun spacemacs//zoom-frm-do (arg)
        "Perform a zoom action depending on ARG value."
        (let ((zoom-action (cond ((eq arg 0) 'zoom-frm-unzoom)
                                 ((< arg 0) 'zoom-frm-out)
                                 ((> arg 0) 'zoom-frm-in)))
              (fm (cdr (assoc 'fullscreen (frame-parameters))))
              (fwp (* (frame-char-width) (frame-width)))
              (fhp (* (frame-char-height) (frame-height))))
          (when (equal fm 'maximized)
            (toggle-frame-maximized))
          (funcall zoom-action)
          (set-frame-size nil fwp fhp t)
          (when (equal fm 'maximized)
            (toggle-frame-maximized))))

      (defun spacemacs/zoom-frm-in ()
        "zoom in frame, but keep the same pixel size"
        (interactive)
        (spacemacs//zoom-frm-do 1))

      (defun spacemacs/zoom-frm-out ()
        "zoom out frame, but keep the same pixel size"
        (interactive)
        (spacemacs//zoom-frm-do -1))

      (defun spacemacs/zoom-frm-unzoom ()
        "Unzoom current frame, keeping the same pixel size"
        (interactive)
        (spacemacs//zoom-frm-do 0))

      ;; Font size, either with ctrl + mouse wheel
      (global-set-key (kbd "<C-wheel-up>") 'spacemacs/zoom-frm-in)
      (global-set-key (kbd "<C-wheel-down>") 'spacemacs/zoom-frm-out))))
