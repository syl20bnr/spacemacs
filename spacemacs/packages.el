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

(defvar spacemacs-packages
  '(
    ace-jump-mode
    ace-link
    ace-window
    adaptive-wrap
    aggressive-indent
    auto-dictionary
    auto-highlight-symbol
    base16-theme
    bind-key
    bookmark
    buffer-move
    diminish
    doc-view
    ediff
    elisp-slime-nav
    eldoc
    eval-sexp-fu
    evil
    evil-anzu
    evil-args
    evil-escape
    evil-exchange
    evil-iedit-state
    evil-indent-textobject
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
    flyspell
    fringe-helper
    golden-ratio
    google-translate
    guide-key-tip
    helm
    helm-ag
    helm-c-yasnippet
    helm-descbinds
    helm-flyspell
    helm-make
    helm-mode-manager
    ;; not working for now
    ;; helm-proc
    helm-projectile
    helm-swoop
    helm-themes
    highlight-indentation
    highlight-numbers
    hippie-exp
    hl-anything
    hungry-delete
    ido-vertical-mode
    info+
    iedit
    indent-guide
    leuven-theme
    linum-relative
    monokai-theme
    move-text
    multi-term
    neotree
    page-break-lines
    popup
    popwin
    powerline
    projectile
    rainbow-delimiters
    recentf
    rfringe
    shell
    smartparens
    smooth-scrolling
    subword
    undo-tree
    use-package
    vi-tilde-fringe
    volatile-highlights
    whitespace
    window-numbering
    winner
    yasnippet
    zenburn-theme
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar spacemacs-excluded-packages '()
  "List of packages to exclude.")

;; Paradox from MELPA is not compatible with 24.3 anymore
(unless  (version< emacs-version "24.4")
  (push 'paradox spacemacs-packages))

;; Initialization of packages

(defun spacemacs/init-ace-jump-mode ()
  (use-package ace-jump-mode
    :defer t
    :init
    (progn
      (add-hook 'ace-jump-mode-end-hook 'golden-ratio)
      (evil-leader/set-key "SPC" 'evil-ace-jump-word-mode)
      (evil-leader/set-key "l" 'evil-ace-jump-line-mode))
    :config
    (progn
      (setq ace-jump-mode-scope 'global)
      (evil-leader/set-key "`" 'ace-jump-mode-pop-mark))))

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
        (ali-generic
         (spacemacs//collect-spacemacs-buffer-links)
         (forward-char 1)
         (widget-button-press (point)))))))

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
      (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))
    :config
    (progn
      ;; add support for golden-ratio
      (eval-after-load 'golden-ratio
        '(setq golden-ratio-extra-commands
               (append golden-ratio-extra-commands
                       '(ace-window
                         ace-delete-window
                         ace-select-window
                         ace-swap-window
                         ace-maximize-window
                         )))))))

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
      (defun spacemacs/toggle-aggressive-indent ()
        "Toggle the aggressive indent mode for the current buffer."
        (interactive)
        (require 'aggressive-indent)
        (if (symbol-value aggressive-indent-mode)
            (global-aggressive-indent-mode -1)
          (global-aggressive-indent-mode)))

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
      (defun spacemacs/disable-aggressive-indent ()
        (aggressive-indent-mode -1))

      (add-hook 'diff-auto-refine-mode-hook 'spacemacs/disable-aggressive-indent)

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

(defun spacemacs/init-base16-theme ())

(defun spacemacs/init-auto-highlight-symbol ()
  (use-package auto-highlight-symbol
    :defer t
    :init
    (add-to-hooks 'auto-highlight-symbol-mode '(prog-mode-hook
                                                markdown-mode-hook))
    :config
    (progn
      (custom-set-variables
       '(ahs-case-fold-search nil)
       '(ahs-default-range (quote ahs-range-whole-buffer))
       ;; disable auto-highlight of symbol
       ;; current symbol should be highlight on demand with <SPC> s h
       '(ahs-idle-timer 0)
       '(ahs-idle-interval 0.25)
       '(ahs-inhibit-face-list nil))

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
                 (ahs-highlight-now)
                 ) nil))

      (defun spacemacs/quick-ahs-forward ()
        "Go to the next occurrence of symbol under point with
`auto-highlight-symbol'"
        (interactive)
        (eval '(progn (spacemacs/integrate-evil-search t)
                      (spacemacs/ahs-highlight-now-wrapper)
                      (when (configuration-layer/package-usedp 'evil-jumper)
                        (evil-set-jump))
                      (ahs-forward)) nil))

      (defun spacemacs/quick-ahs-backward ()
        "Go to the previous occurrence of symbol under point with
`auto-highlight-symbol'"
        (interactive)
        (eval '(progn (spacemacs/integrate-evil-search nil)
                      (spacemacs/ahs-highlight-now-wrapper)
                      (when (configuration-layer/package-usedp 'evil-jumper)
                        (evil-set-jump))
                      (ahs-backward)) nil))

      (eval-after-load 'evil
        '(progn
           (define-key evil-motion-state-map (kbd "*") 'spacemacs/quick-ahs-forward)
           (define-key evil-motion-state-map (kbd "#") 'spacemacs/quick-ahs-backward)))

      (defun spacemacs/symbol-highlight ()
        "Highlight the symbol under point with `auto-highlight-symbol'."
        (interactive)
        (eval '(progn
                 (spacemacs/ahs-highlight-now-wrapper)
                 (setq spacemacs-last-ahs-highlight-p (ahs-highlight-p))
                 (spacemacs/auto-highlight-symbol-overlay-map)
                 (spacemacs/integrate-evil-search nil)
                 ) nil))

      (defun spacemacs/symbol-highlight-reset-range ()
        "Reset the range for `auto-highlight-symbol'."
        (interactive)
        (eval '(ahs-change-range ahs-default-range) nil))

      (evil-leader/set-key
        "sb"  'spacemacs/goto-last-searched-ahs-symbol
        "sh"  'spacemacs/symbol-highlight
        "sR"  'spacemacs/symbol-highlight-reset-range)

      (spacemacs|hide-lighter auto-highlight-symbol-mode)
      ;; micro-state to easily jump from a highlighted symbol to the others
      (dolist (sym '(ahs-forward
                     ahs-forward-definition
                     ahs-backward
                     ahs-backward-definition
                     ahs-back-to-start
                     ahs-change-range))
        (let* ((advice (intern (format "spacemacs/%s" (symbol-name sym)))))
          (eval `(defadvice ,sym (after ,advice activate)
                   (spacemacs/ahs-highlight-now-wrapper)
                   (setq spacemacs-last-ahs-highlight-p (ahs-highlight-p))
                   (spacemacs/auto-highlight-symbol-overlay-map)))))
      (defun spacemacs/auto-highlight-symbol-overlay-map ()
        "Set a temporary overlay map to easily jump from highlighted symbols to
 the nexts."
        (interactive)
        (set-temporary-overlay-map
         (let ((map (make-sparse-keymap)))
           (define-key map (kbd "d") 'ahs-forward-definition)
           (define-key map (kbd "D") 'ahs-backward-definition)
           (if (ht-contains? configuration-layer-all-packages 'evil-iedit-state)
               (define-key map (kbd "e") 'evil-iedit-state/iedit-mode)
             (define-key map (kbd "e") 'ahs-edit-mode))
           (define-key map (kbd "n") 'ahs-forward)
           (define-key map (kbd "N") 'ahs-backward)
           (define-key map (kbd "R") 'ahs-back-to-start)
           (define-key map (kbd "r") (lambda () (interactive)
                                       (eval '(ahs-change-range) nil)))
           map) nil)
        (let* ((i 0)
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
          (let* ((x/y (format "[%s/%s]" (- overlay-count i) overlay-count))
                 (propx/y (propertize x/y 'face ahs-plugin-whole-buffer-face))
                 (hidden (if (< 0 (- overlay-count (nth 4 st))) "*" ""))
                 (prophidden (propertize hidden 'face '(:weight bold))))
            (echo "%s %s%s press (n/N) to navigate, (e) to edit, (r) to change range or (R) for reset (d) to go to next definition (D) to go to previous definition"
                  propplugin propx/y prophidden)))))))

(defun spacemacs/init-bind-key ())

(defun spacemacs/init-bookmark ()
  (use-package bookmark
    :defer t
    :init
    (setq bookmark-default-file (concat user-emacs-directory "bookmarks")
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

(defun spacemacs/init-diminish ()
  (require 'diminish)
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
       (diminish 'subword-mode))))

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


(defun spacemacs/init-elisp-slime-nav ()
  ;; Elisp go-to-definition with M-. and back again with M-,
  (use-package elisp-slime-nav
    :defer t
    :init
    (progn
      (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)
      (evil-leader/set-key-for-mode 'emacs-lisp-mode
        "mgg" 'elisp-slime-nav-find-elisp-thing-at-point
        "mhh" 'elisp-slime-nav-describe-elisp-thing-at-point))))

(defun spacemacs/init-eldoc ()
  (use-package eldoc
    :defer t
    :init (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
    :config
    (progn
      ;; enable eldoc in `eval-expression'
      (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)

      ;; enable eldoc in IELM
      (add-hook 'ielm-mode-hook #'eldoc-mode)

      ;; don't display eldoc on modeline
      (spacemacs|hide-lighter eldoc-mode))))

(defun spacemacs/init-eval-sexp-fu ()
  (require 'eval-sexp-fu))

(defun spacemacs/init-evil ()
  (use-package evil
    :init
    (progn
      (defvar spacemacs-evil-cursor-colors '((normal . "DarkGoldenrod2")
                                             (insert . "chartreuse3")
                                             (emacs  . "SkyBlue2")
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

      (evil-mode 1))
    :config
    (progn
      ;; evil ex-command key
      (define-key evil-normal-state-map (kbd dotspacemacs-command-key) 'evil-ex)
      (define-key evil-visual-state-map (kbd dotspacemacs-command-key) 'evil-ex)
      (define-key evil-motion-state-map (kbd dotspacemacs-command-key) 'evil-ex)
      ;; Make evil-mode up/down operate in screen lines instead of logical lines
      (define-key evil-normal-state-map "j" 'evil-next-visual-line)
      (define-key evil-normal-state-map "k" 'evil-previous-visual-line)
      ;; Also in visual mode
      (define-key evil-visual-state-map "j" 'evil-next-visual-line)
      (define-key evil-visual-state-map "k" 'evil-previous-visual-line)
      ;; Make the current definition and/or comment visible.
      (define-key evil-normal-state-map "zf" 'reposition-window)

      (evil-leader/set-key "re" 'evil-show-registers)

      (defun spacemacs/smart-doc-lookup ()
        "Bind K to SPC m h h and fall back to `evil-lookup'"
        (interactive)
        (condition-case nil
            (execute-kbd-macro (kbd "SPC m h h"))
          (error (evil-lookup))))
      (define-key evil-normal-state-map (kbd "K") 'spacemacs/smart-doc-lookup)

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
        :use-minibuffer t
        :execute-binding-on-enter t
        :evil-leader "n." "n," "n<" "n>"
        :bindings
        ;; page
        ("," evil-scroll-page-up)
        ("." evil-scroll-page-down)
        ;; half page
        ("<" spacemacs/scroll-half-page-up)
        (">" spacemacs/scroll-half-page-down))

      ;; pasting micro-state
      (defadvice evil-paste-before (after spacemacs/evil-paste-before activate)
        "Initate the paste micro-state after the execution of evil-paste-before"
        (unless (evil-ex-p)
          (spacemacs/paste-micro-state)))
      (defadvice evil-paste-after (after spacemacs/evil-paste-after activate)
        "Initate the paste micro-state after the execution of evil-paste-after"
        (unless (evil-ex-p)
          (spacemacs/paste-micro-state)))
      (defadvice evil-visual-paste (after spacemacs/evil-visual-paste activate)
        "Initate the paste micro-state after the execution of evil-visual-paste"
        (spacemacs/paste-micro-state))
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
        (ad-disable-advice 'evil-paste-before 'after 'spacemacs/evil-paste-before)
        (ad-activate 'evil-paste-before)
        (ad-disable-advice 'evil-paste-after 'after 'spacemacs/evil-paste-after)
        (ad-activate 'evil-paste-after)
        (ad-disable-advice 'evil-visual-paste 'after 'spacemacs/evil-visual-paste)
        (ad-activate 'evil-visual-paste))


      ;; define text objects
      (defmacro spacemacs|define-and-bind-text-object (key name start-regex end-regex)
        (let ((inner-name (make-symbol (concat "evil-inner-" name)))
              (outer-name (make-symbol (concat "evil-outer-" name))))
          `(progn
             (evil-define-text-object ,inner-name (count &optional beg end type)
               (evil-select-paren ,start-regex ,end-regex beg end type count nil))
             (evil-define-text-object ,outer-name (count &optional beg end type)
               (evil-select-paren ,start-regex ,end-regex beg end type count t))
             (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
             (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))
      ;; between dollars sign:
      (spacemacs|define-and-bind-text-object "$" "dollar" "\\$" "\\$")
      ;; between pipe characters:
      (spacemacs|define-and-bind-text-object "|" "bar" "|" "|")
      ;; between percent signs:
      (spacemacs|define-and-bind-text-object "%" "percent" "%" "%")

      ;; add star block
      (spacemacs|define-and-bind-text-object "*" "star-block" "/*" "*/")
      ;; add slash block
      (spacemacs|define-and-bind-text-object "/" "slash-block" "//" "//")

      ;; support smart 1parens-strict-mode
      (if (ht-contains? configuration-layer-all-packages 'smartparens)
          (defadvice evil-delete-backward-char-and-join
              (around spacemacs/evil-delete-backward-char-and-join activate)
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

  (defun spacemacs/evil-state-lazy-loading ()
    (require 'evil-iedit-state)
    ;; activate leader in iedit and iedit-insert states
    (define-key evil-iedit-state-map
      (kbd evil-leader/leader) evil-leader--default-map))

  (evil-leader/set-key "se" 'evil-iedit-state/iedit-mode)
  (add-to-hooks 'spacemacs/evil-state-lazy-loading '(find-file-hook)))

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
  (setq evil-lisp-state-global t)
  (setq evil-lisp-state-leader-prefix "k")
  (require 'evil-lisp-state))

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
        (echo "+ to increase the value or - to decrease it"))

      (defun spacemacs/evil-numbers-micro-state-overlay-map ()
        "Set a temporary overlay map to easily increase or decrease a number"
        (set-temporary-overlay-map
         (let ((map (make-sparse-keymap)))
           (define-key map (kbd "+") 'spacemacs/evil-numbers-increase)
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
                          'evil-search-highlight-persist-remove-all))))

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
  (unless (display-graphic-p)
    (require 'evil-terminal-cursor-changer)
     (setq evil-visual-state-cursor 'box) ; █
     (setq evil-insert-state-cursor 'bar) ; ⎸
     (setq evil-emacs-state-cursor 'hbar) ; _
    ))

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
    :init
    (evil-leader/set-key "v" 'er/expand-region)
    :config
    (custom-set-variables
     '(expand-region-contract-fast-key "V")
     '(expand-region-reset-fast-key "r"))))

(defun spacemacs/init-fancy-battery ()
  (use-package fancy-battery
    :defer t
    :init
    (progn

      (defun spacemacs/mode-line-battery-info-toggle ()
        "Toggle display of battery info."
        (interactive)
        (if fancy-battery-mode
            (fancy-battery-mode -1)
          (fancy-battery-mode)
          (spacemacs/mode-line-battery-remove-from-global)))

      (defun spacemacs/mode-line-battery-remove-from-global ()
        "Remove the battery info from the `global-mode-string'."
        (setq global-mode-string (delq 'fancy-battery-mode-line
                                       global-mode-string)))

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

      (setq-default fancy-battery-show-percentage t)
      (evil-leader/set-key "tmb" 'spacemacs/mode-line-battery-info-toggle))
    :config
    (progn
      ;; redefine this function for Spacemacs,
      ;; basically remove all faces and properties.
      (defun fancy-battery-default-mode-line ()
        "Assemble a mode line string for Fancy Battery Mode."
        (spacemacs/mode-line-battery-remove-from-global)
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
      ;; manually register the minor mode since it does not define any
      ;; lighter
      (push '(fci-mode "") minor-mode-alist)
      (spacemacs|add-toggle fill-column-indicator
                            :status fci-mode
                            :on (turn-on-fci-mode)
                            :off (turn-off-fci-mode)
                            :documentation "Display the fill column indicator."
                            :evil-leader "tc"))
    :config
    (spacemacs|diminish fci-mode " ⓒ" " c")))

(defun spacemacs/init-flx-ido ()
  (use-package flx-ido
    :init (flx-ido-mode 1)))

(defun spacemacs/init-flyspell ()
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
                            :evil-leader "ts"))
    :config
    (progn
      (flyspell-prog-mode)
      (spacemacs|diminish flyspell-mode " ⓢ" " s"))))

(defun spacemacs/init-fringe-helper ())

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
      (setq golden-ratio-extra-commands
            (append golden-ratio-extra-commands
                    '(windmove-left
                      windmove-right
                      windmove-up
                      windmove-down
                      evil-window-left
                      evil-window-right
                      evil-window-up
                      evil-window-down
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
                      ace-jump-mode-pop-mark
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

(defun spacemacs/init-guide-key-tip ()
  (use-package guide-key-tip
    :init
    (progn
      (defun spacemacs/toggle-guide-key ()
        "Toggle golden-ratio mode on and off."
        (interactive)
        (if (symbol-value guide-key-mode)
            (guide-key-mode -1)
          (guide-key-mode)))

      (defadvice guide-key/popup-guide-buffer-p
          (around spacemacs/inhibit-guide-buffer activate)
        "Prevent the popup of the guide-key buffer in some case."
        ;; a micro-state is running
        ;; or
        ;; bzg-big-fringe-mode is on
        (unless (or overriding-terminal-local-map
                    bzg-big-fringe-mode)
          ad-do-it))

      (spacemacs|add-toggle guide-key
                      :status guide-key-mode
                      :on (guide-key-mode)
                      :off (guide-key-mode -1)
                      :documentation
                      "Display a buffer with available key bindings."
                      :evil-leader "tG")

      (setq guide-key/guide-key-sequence `("C-x"
                                           "C-c"
                                           "C-w"
                                           ,dotspacemacs-leader-key
                                           ,dotspacemacs-emacs-leader-key
                                           ,dotspacemacs-major-mode-leader-key
                                           ,dotspacemacs-major-mode-emacs-leader-key
                                           ;; M-m in terminal
                                           "<ESC>m"
                                           ;; C-M-m in terminal
                                           "<ESC><RET>"
                                           "g"
                                           "\["
                                           "\]"
                                           "z"
                                           "C-h")
            guide-key/recursive-key-sequence-flag t
            guide-key/popup-window-position 'right
            guide-key/idle-delay dotspacemacs-guide-key-delay
            guide-key/text-scale-amount 0
            ;; use this in your ~/.spacemacs file to enable tool tip in a
            ;; graphical envrionment
            ;; guide-key-tip/enabled (if window-system t)
            guide-key-tip/enabled nil)
      (setq guide-key/highlight-command-regexp
                   (cons spacemacs/prefix-command-string font-lock-warning-face))
      (guide-key-mode 1)
      (spacemacs|diminish guide-key-mode " Ⓖ" " G"))))

(defun spacemacs/init-helm ()
  (use-package helm
    :defer t
    :init
    (progn

      (setq helm-prevent-escaping-from-minibuffer t
            helm-split-window-in-side-p nil
            helm-bookmark-show-location t
            helm-buffers-fuzzy-matching t
            helm-always-two-windows     t)

      (defun spacemacs/helm-do-ack ()
        "Perform a search with ack using `helm-ag.'"
        (interactive)
        (if (configuration-layer/package-usedp 'helm-ag)
            (let ((helm-ag-base-command "ack --nocolor --nogroup"))
              (call-interactively 'helm-do-ag))
          (message "error: helm-ag not found.")))

      (defun spacemacs/helm-do-pt ()
        "Perform a search with the platinum searcher using `helm-ag.'"
        (interactive)
        (if (configuration-layer/package-usedp 'helm-ag)
            (let ((helm-ag-base-command "pt --nocolor --nogroup"))
              (call-interactively 'helm-do-ag))
          (message "error: helm-ag not found.")))

      (defun spacemacs/helm-do-search-dwim (&optional arg)
        "Execute the first found search tool.

Search for a search tool in the following order pt > ag > ack > grep.
If ARG is non nil then the search order is changed to ack > grep (ag and pt are
ignored)."
        (interactive "P")
        (call-interactively
         (if arg
             (cond (((executable-find "ack") 'spacemacs/helm-do-ack)
                    (t 'helm-do-grep)))
           (cond ((executable-find "pt") 'spacemacs/helm-do-pt)
                 ((executable-find "ag") 'helm-do-ag)
                 ((executable-find "ack") 'spacemacs/helm-do-ack)
                 (t 'helm-do-grep)))))

      ;; use helm by default for M-x
      (unless (configuration-layer/package-usedp 'smex)
        (global-set-key (kbd "M-x") 'helm-M-x))

      (evil-leader/set-key
        "<f1>" 'helm-apropos
        "bs"  'helm-mini
        "Cl"  'helm-colors
        "fh"  'helm-find-files
        "fr"  'helm-recentf
        "hb"  'helm-bookmarks
        "hi"  'helm-info-at-point
        "hl"  'helm-resume
        "hm"  'helm-man-woman
        "ry"  'helm-show-kill-ring
        "rr"  'helm-register
        "rm"  'helm-all-mark-rings
        "s/"  'spacemacs/helm-do-search-dwim
        "sa"  'helm-do-ag
        "sg"  'helm-do-grep
        "sk"  'spacemacs/helm-do-ack
        "sp"  'spacemacs/helm-do-pt
        "sl"  'helm-semantic-or-imenu)

      ;; define the key binding at the very end in order to allow the user
      ;; to overwrite any key binding
      (add-hook 'after-init-hook
                (lambda ()
                  (unless (configuration-layer/package-usedp 'smex)
                    (evil-leader/set-key dotspacemacs-command-key 'helm-M-x))))

      ;; disable popwin-mode in an active Helm session It should be disabled
      ;; otherwise it will conflict with other window opened by Helm persistent
      ;; action, such as *Help* window.
      (add-hook 'helm-after-initialize-hook (lambda () (popwin-mode -1)))

      ;;  Restore popwin-mode after a Helm session finishes.
      (add-hook 'helm-cleanup-hook (lambda () (popwin-mode 1)))

      ;; Add minibuffer history with `helm-minibuffer-history'
      (define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

      (defun spacemacs//helm-before-initialize ()
        "Stuff to do before helm initializes."
        ;; be sure that any previous micro-state face override are
        ;; wiped out
        (setq face-remapping-alist nil))
      (add-hook 'helm-before-initialize-hook 'spacemacs//helm-before-initialize)

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
      (spacemacs//helm-hjkl-navigation (eq 'vim dotspacemacs-editing-style))

      ;; eshell
      (defun spacemacs/helm-eshell-history ()
        "Correctly revert to insert state after selection."
        (interactive)
        (helm-eshell-history)
        (evil-insert-state))
      (defun spacemacs/helm-shell-history ()
        "Correctly revert to insert state after selection."
        (interactive)
        (helm-comint-input-ring)
        (evil-insert-state))
      (defun spacemacs/init-helm-eshell ()
        "Initialize helm-eshell."
        ;; this is buggy for now
        ;; (define-key eshell-mode-map (kbd "<tab>") 'helm-esh-pcomplete)
        (evil-leader/set-key-for-mode 'eshell-mode "mH" 'spacemacs/helm-eshell-history))
      (add-hook 'eshell-mode-hook 'spacemacs/init-helm-eshell)
      ;;shell
      (evil-leader/set-key-for-mode 'shell-mode "mH" 'spacemacs/helm-shell-history)

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
        :define-key (helm-map . "C-SPC") (helm-map . "C-@")
        :on-enter (spacemacs//helm-navigation-ms-on-enter)
        :on-exit  (spacemacs//helm-navigation-ms-on-exit)
        :bindings
        ("C-SPC" nil :exit t)
        ("C-@" nil :exit t)
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

      (eval-after-load "helm-mode" ; required
        '(spacemacs|hide-lighter helm-mode)))))

(defun spacemacs/init-helm-ag ()
  (use-package helm-ag
    :defer t
    :init
    (progn
      (defun spacemacs/helm-do-ack ()

        )
      )
    :config
    (evil-define-key 'normal helm-ag-map "SPC" evil-leader--default-map)))

(defun spacemacs/init-helm-c-yasnippet ()
  (use-package helm-c-yasnippet
    :defer t
    :init
    (progn
      (defun spacemacs/helm-yas ()
        "Properly lazy load helm-c-yasnipper."
        (interactive)
        (spacemacs/load-yasnippet)
        (require 'helm-c-yasnippet)
        (call-interactively 'helm-yas-complete))
      (evil-leader/set-key "is" 'spacemacs/helm-yas)
      (setq helm-c-yas-space-match-any-greedy t))))

(defun spacemacs/init-helm-descbinds ()
  (use-package helm-descbinds
    :defer t
    :init
    (progn
      (setq helm-descbinds-window-style 'split)
      (add-hook 'helm-mode-hook 'helm-descbinds-mode)
      (evil-leader/set-key "?" 'helm-descbinds))))

(defun spacemacs/init-helm-flyspell ()
  (use-package helm-flyspell
    :commands helm-flyspell-correct
    :init (evil-leader/set-key "Sc" 'helm-flyspell-correct)))

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
    :commands (helm-projectile-ack
               helm-projectile-ag
               helm-projectile-switch-to-buffer
               helm-projectile-find-dir
               helm-projectile-dired-find-dir
               helm-projectile-recentf
               helm-projectile-find-file
               helm-projectile-grep
               helm-projectile
               helm-projectile-switch-project
               helm-projectile-vc)
    :init
    (progn
      (setq projectile-switch-project-action 'helm-projectile)

      (defconst spacemacs-use-helm-projectile t
        "This variable is only defined if helm-projectile is used.")

      (defun spacemacs/helm-projectile-pt ()
        "Perform a search with the platinum searcher using `helm-projectile.'"
        (interactive)
        (if (configuration-layer/package-usedp 'helm-ag)
            (let ((helm-ag-base-command "pt -e --nocolor --nogroup"))
              (call-interactively 'helm-projectile-ag))
          (message "error: helm-ag not found.")))

      (defun spacemacs/helm-projectile-search-dwim (&optional arg)
        "Execute the first found search tool.

Search for a search tool in the following order pt > ag > ack > grep.
If ARG is non nil then the search order is changed to ack > grep (ag and pt are
ignored)."
        (interactive "P")
        (call-interactively
         (if arg
             (cond (((executable-find "ack") 'helm-projectile-ack)
                    (t 'helm-projectile-grep)))
           (cond ((executable-find "pt") 'spacemacs/helm-projectile-pt)
                 ((executable-find "ag") 'helm-projectile-ag)
                 ((executable-find "ack") 'spacemacs/helm-projectile-ack)
                 (t 'helm-projectile-grep)))))

      (evil-leader/set-key
        "/"   'spacemacs/helm-projectile-search-dwim
        "pb"  'helm-projectile-switch-to-buffer
        "pd"  'helm-projectile-find-dir
        "pe"  'helm-projectile-recentf
        "pf"  'helm-projectile-find-file
        "ph"  'helm-projectile
        "pp"  'helm-projectile-switch-project
        "psa" 'helm-projectile-ag
        "psg" 'helm-projectile-grep
        "psk" 'helm-projectile-ack
        "psp" 'helm-projectile-pt
        "pv"  'helm-projectile-vc))))

(defun spacemacs/init-helm-swoop ()
  (use-package helm-swoop
    :defer t
    :init
    (setq helm-swoop-split-with-multiple-windows t
          helm-swoop-split-direction 'split-window-vertically
          helm-swoop-split-window-function 'helm-default-display-buffer)
    (evil-leader/set-key
      "sS"    'helm-multi-swoop
      "ss"    'helm-swoop
      "s C-s" 'helm-multi-swoop-all)
    (defadvice helm-swoop (before add-evil-jump activate)
      (when (configuration-layer/package-usedp 'evil-jumper)
        (evil-set-jump)))))

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
                            :evil-leader "thc"))))

(defun spacemacs/init-highlight-numbers ()
  (use-package highlight-numbers
    :defer t
    :init
    (progn
      (add-hook 'prog-mode-hook 'highlight-numbers-mode)
      (add-hook 'asm-mode-hook (lambda () (highlight-numbers-mode -1))))))

(defun spacemacs/init-hippie-exp ()
  (global-set-key (kbd "M-/") 'hippie-expand) ;; replace dabbrev-expand
  (setq hippie-expand-try-functions-list
        '(
          ;; Try to expand word "dynamically", searching the current buffer.
          try-expand-dabbrev
          ;; Try to expand word "dynamically", searching all other buffers.
          try-expand-dabbrev-all-buffers
          ;; Try to expand word "dynamically", searching the kill ring.
          try-expand-dabbrev-from-kill
          ;; Try to complete text as a file name, as many characters as unique.
          try-complete-file-name-partially
          ;; Try to complete text as a file name.
          try-complete-file-name
          ;; Try to expand word before point according to all abbrev tables.
          try-expand-all-abbrevs
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-list
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-line
          ;; Try to complete as an Emacs Lisp symbol, as many characters as
          ;; unique.
          try-complete-lisp-symbol-partially
          ;; Try to complete word as an Emacs Lisp symbol.
          try-complete-lisp-symbol)))

(defun spacemacs/init-hl-anything ()
  (use-package hl-anything
    :defer t
    :init
    (progn
      (setq-default hl-highlight-save-file (concat spacemacs-cache-directory
                                                   ".hl-save"))
      (evil-leader/set-key
        "hc"  'hl-unhighlight-all-local
        "hgc" 'hl-unhighlight-all-global
        "hgh" 'hl-highlight-thingatpt-global
        "hh"  'hl-highlight-thingatpt-local
        "hn"  'hl-find-next-thing
        "hN"  'hl-find-prev-thing
        "hp"  'hl-paren-mode
        "hr"  'hl-restore-highlights
        "hs"  'hl-save-highlights))
    :config
    (progn
      (spacemacs|diminish hl-paren-mode " (Ⓗ)" " (H)")
      (spacemacs|hide-lighter hl-highlight-mode))))

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

(defun spacemacs/init-ido-vertical-mode ()
  (use-package ido-vertical-mode
    :init
    (progn
      (ido-vertical-mode t)

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
        (when face-remapping-alist
          (setq face-remapping-alist nil))
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
        (define-key ido-completion-map (kbd "C-o") 'ido-invoke-in-other-window)
        (define-key ido-completion-map (kbd "C-s") 'ido-invoke-in-vertical-split)
        (define-key ido-completion-map (kbd "C-t") 'ido-invoke-in-new-frame)
        (define-key ido-completion-map (kbd "C-v") 'ido-invoke-in-horizontal-split)
        ;; more natural navigation keys: up, down to change current item
        ;; left to go up dir
        ;; right to open the selected item
        (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
        (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
        (define-key ido-completion-map (kbd "<left>") 'ido-delete-backward-updir)
        (define-key ido-completion-map (kbd "<right>") 'ido-exit-minibuffer)
        ;; initiate micro-state
        (define-key ido-completion-map (kbd "C-SPC") 'spacemacs/ido-navigation-micro-state)
        (define-key ido-completion-map (kbd "C-@") 'spacemacs/ido-navigation-micro-state)
        )
      (add-hook 'ido-setup-hook 'spacemacs//ido-setup)

      (defvar spacemacs--ido-navigation-ms-enabled nil
        "Flag which is non nil when ido navigation micro-state is enabled.")

      (defface spacemacs-ido-navigation-ms-face
        `((t :background ,(face-attribute 'error :foreground)
             :foreground "black"
             :weight bold))
        "Face for ido minibuffer prompt when ido micro-state is activated."
        :group 'spacemacs)

      (defun spacemacs//ido-navigation-ms-set-face ()
        "Set faces for ido navigation micro-state."
        (push '(minibuffer-prompt . spacemacs-ido-navigation-ms-face)
              face-remapping-alist))

      (defun spacemacs//ido-navigation-ms-on-enter ()
        "Initialization of ido micro-state."
        (setq spacemacs--ido-navigation-ms-enabled t)
        (spacemacs//ido-navigation-ms-set-face)
        )
      (defun spacemacs//ido-navigation-ms-on-exit ()
        "Action to perform when exiting ido micro-state."
        (setq face-remapping-alist nil))

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
        ("C-SPC" nil :exit t)
        ("C-@" nil :exit t)
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
        ("o" ido-invoke-in-other-window :exit t)
        ("p" ido-prev-match-dir)
        ("q" nil :exit t)
        ("s" ido-invoke-in-vertical-split :exit t)
        ("t" ido-invoke-in-new-frame :exit t)
        ("v" ido-invoke-in-horizontal-split :exit t)))))

(defun spacemacs/init-iedit ()
  (use-package iedit
    :defer t))

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

(defun spacemacs/init-info+ ()
  (use-package info+
    :defer t
    :init
    (add-hook 'Info-mode-hook (lambda () (require 'info+)))
    :config
    (setq Info-fontify-angle-bracketed-flag nil)))

(defun spacemacs/init-leuven-theme ()
  (use-package leuven-theme
    :defer t
    :init (setq org-fontify-whole-heading-line t)))

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

(defun spacemacs/init-monokai-theme ())

(defun spacemacs/init-move-text ()
  (use-package move-text
    :defer t
    :init
    (evil-leader/set-key
      "xmj" 'move-text-down
      "xmk" 'move-text-up)))

(defun spacemacs/init-multi-term ()
  (use-package multi-term
    :defer t
    :init
    (evil-leader/set-key "ast" 'multi-term)
    :config
    (progn
      (defun term-send-tab ()
        "Send tab in term mode."
        (interactive)
        (term-send-raw-string "\t"))

      (add-to-list 'term-bind-key-alist '("<tab>" . term-send-tab)))))

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
    (add-to-hook 'neotree-mode-hook '(spacemacs//init-neotree
                                      spacemacs//neotree-key-bindings))))

(defun spacemacs/init-page-break-lines ()
  (use-package page-break-lines
    :init
    (global-page-break-lines-mode t)
    (spacemacs|hide-lighter page-break-lines-mode)))

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
    :init
    (progn
      (popwin-mode 1)
      (evil-leader/set-key "wpm" 'popwin:messages)
      (evil-leader/set-key "wpp" 'popwin:close-popup-window)
      (push '("*ert*"                      :dedicated t :position bottom :stick t :noselect t) popwin:special-display-config)
      (push '("*grep*"                     :dedicated t :position bottom :stick t :noselect t) popwin:special-display-config)
      (push '("*nosetests*"                :dedicated t :position bottom :stick t :noselect t) popwin:special-display-config)
      (push '("^\*WoMan.+\*$"    :regexp t              :position bottom                     ) popwin:special-display-config)
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
                            (if dotspacemacs-mode-line-unicode-symbols " " "") 'face face)
                           (unless dotspacemacs-mode-line-unicode-symbols "|"))))

      (defpowerline spacemacs-powerline-new-version
        (propertize
         spacemacs-version-check-lighter
         'mouse-face 'mode-line-highlight
         'help-echo (format "New version %s | Click with mouse-1 to update (Not Yet Implemented)"
                            spacemacs-new-version)
         'local-map (let ((map (make-sparse-keymap)))
                      (define-key map
                        [mode-line down-mouse-1]
                        (lambda (event) (interactive "@e") (message "TODO: update"))
                        )
                      map)))

      (defvar spacemacs-mode-line-minor-modesp t
        "If not nil, minor modes lighter are displayed in the mode-line.")
      (defun spacemacs/mode-line-minor-modes-toggle ()
        "Toggle display of minor modes."
        (interactive)
        (if spacemacs-mode-line-minor-modesp
            (setq spacemacs-mode-line-minor-modesp nil)
          (setq spacemacs-mode-line-minor-modesp t)))
      (evil-leader/set-key "tmm" 'spacemacs/mode-line-minor-modes-toggle)

      (defvar spacemacs-mode-line-new-version-lighterp t
        "If not nil, new version lighter is displayed in the mode-line.")
      (defun spacemacs/mode-line-new-version-lighter-toggle ()
        "Toggle display of new version lighter."
        (interactive)
        (if spacemacs-mode-line-new-version-lighterp
            (setq spacemacs-mode-line-new-version-lighterp nil)
          (setq spacemacs-mode-line-new-version-lighterp t)))
      (evil-leader/set-key "tmv" 'spacemacs/mode-line-new-version-lighter-toggle)

      (defvar spacemacs-mode-line-display-point-p nil
        "If not nil, display point alongside row/column in the mode-line.")
      (defun spacemacs/mode-line-display-point-toggle ()
        (interactive)
        (if spacemacs-mode-line-display-point-p
            (setq spacemacs-mode-line-display-point-p nil)
          (setq spacemacs-mode-line-display-point-p t)))
      (evil-leader/set-key "tmp" 'spacemacs/mode-line-display-point-toggle)

      (defvar spacemacs-mode-line-org-clock-current-taskp nil
        "If not nil, the currently clocked org-mode task will be
displayed in the mode-line.")
      (defvar spacemacs-mode-line-org-clock-format-function
        'org-clock-get-clock-string
        "Function used to render the currently clocked org-mode task.")
      (defun spacemacs/mode-line-org-clock-current-task-toggle ()
        (interactive)
        (if spacemacs-mode-line-org-clock-current-taskp
            (setq spacemacs-mode-line-org-clock-current-taskp nil)
          (setq spacemacs-mode-line-org-clock-current-taskp t)))
      (evil-leader/set-key "tmc" 'spacemacs/mode-line-org-clock-current-task-toggle)

      (if (display-graphic-p)
          (setq-default powerline-default-separator 'wave)
        (setq-default powerline-default-separator 'utf-8))

      (defun spacemacs/mode-line-prepare-left ()
        (let* ((active (powerline-selected-window-active))
               (line-face (if active 'mode-line 'mode-line-inactive))
               (face1 (if active 'powerline-active1 'powerline-inactive1))
               (face2 (if active 'powerline-active2 'powerline-inactive2))
               (state-face (if active (spacemacs/current-state-face) face2))
               (window-numberingp (and (boundp 'window-numbering-mode)
                                       (symbol-value window-numbering-mode)))
               (anzup (and (boundp 'anzu--state) anzu--state))
               (flycheckp (and (boundp 'flycheck-mode)
                               (symbol-value flycheck-mode)
                               (or flycheck-current-errors
                                   (eq 'running flycheck-last-status-change))))
               (vc-face (if (or flycheckp spacemacs-mode-line-minor-modesp)
                            face1 line-face))
               (separator-left (intern (format "powerline-%s-%s"
                                               powerline-default-separator
                                               (car powerline-default-separator-dir))))
               (separator-right (intern (format "powerline-%s-%s"
                                                powerline-default-separator
                                                (cdr powerline-default-separator-dir)))))
          (append
           ;; window number
           (if (and window-numberingp (spacemacs/window-number))
               (list (powerline-raw (spacemacs/window-number) state-face))
             (list (powerline-raw (evil-state-property evil-state :tag t) state-face)))
           (if (and active anzup)
               (list (funcall separator-right state-face face1)
                     (powerline-raw (anzu--update-mode-line) face1)
                     (funcall separator-right face1 line-face))
             (list (funcall separator-right state-face line-face)))
           ;; evil state
           ;; (powerline-raw evil-mode-line-tag state-face)
           ;; (funcall separator-right state-face line-face)
           ;; buffer name
           (list
            (powerline-raw "%*" line-face 'l)
            (powerline-buffer-size line-face 'l)
            (powerline-buffer-id line-face 'l)
            (powerline-raw " " line-face)
            ;; major mode
            (funcall separator-left line-face face1)
            (powerline-major-mode face1 'l)
            (powerline-raw " " face1)
            (when active
              (funcall separator-right face1 line-face)))
           ;; flycheck
           (when (and active flycheckp)
               (list (powerline-raw " " line-face)
                     (powerline-raw (spacemacs|custom-flycheck-lighter error)
                                    'spacemacs-mode-line-flycheck-error-face)
                     (powerline-raw (spacemacs|custom-flycheck-lighter warning)
                                    'spacemacs-mode-line-flycheck-warning-face)
                     (powerline-raw (spacemacs|custom-flycheck-lighter info)
                                    'spacemacs-mode-line-flycheck-info-face)))
           ;; separator between flycheck and minor modes
           (when (and active flycheckp spacemacs-mode-line-minor-modesp)
             (list (funcall separator-left line-face face1)
                   (powerline-raw "  " face1)
                   (funcall separator-right face1 line-face)))
           ;; minor modes
           (when (and active spacemacs-mode-line-minor-modesp)
             (list (spacemacs-powerline-minor-modes line-face 'l)
                   (powerline-raw mode-line-process line-face 'l)
                   (powerline-raw " " line-face)))
           ;; version control
           (when (and active (or flycheckp spacemacs-mode-line-minor-modesp))
             (list (funcall separator-left (if vc-face line-face face1) vc-face)))
           (if active
               (list (powerline-vc vc-face)
                     (powerline-raw " " vc-face)
                     (funcall separator-right vc-face face2))
             (list (funcall separator-right face1 face2)))
           ;; org clocked task
           (when (and active
                      spacemacs-mode-line-org-clock-current-taskp
                      (fboundp 'org-clocking-p)
                      (org-clocking-p))
             (list (powerline-raw " " face2)
                   (funcall spacemacs-mode-line-org-clock-format-function)
                   (powerline-raw " " face2))))))

      (defun column-number-at-pos (pos)
        "Analog to line-number-at-pos."
        (save-excursion (goto-char pos) (current-column)))

      (defun selection-info ()
        "Info on the current selection for the mode-line.

It is a string holding:
- the number of columns in the selection if it covers only one line,
- the number of lines in the selection if if covers several full lines
- or rowsxcols if it's a block selection."
        (let* ((lines (count-lines (region-beginning) (1+ (region-end))))
               (chars (- (1+ (region-end)) (region-beginning)))
               (cols (1+ (abs (- (column-number-at-pos (region-end))
                                 (column-number-at-pos (region-beginning)))))))
          (if (eq evil-visual-selection 'block)
              (format "%dx%d block" lines cols)
            (if (> lines 1) (format "%d lines" lines)
              (format "%d chars" chars)))))

      (defun spacemacs/mode-line-prepare-right ()
        (let* ((active (powerline-selected-window-active))
               (line-face (if active 'mode-line 'mode-line-inactive))
               (face1 (if active 'powerline-active1 'powerline-inactive1))
               (face2 (if active 'powerline-active2 'powerline-inactive2))
               (state-face (if active (spacemacs/current-state-face) face2))
               (nyancatp (and (boundp 'nyan-mode) nyan-mode))
               (batteryp (and (boundp 'fancy-battery-mode)
                              (symbol-value fancy-battery-mode)))
               (battery-face (if batteryp (fancy-battery-powerline-face)))
               (separator-left (intern (format "powerline-%s-%s"
                                               powerline-default-separator
                                               (car powerline-default-separator-dir))))
               (separator-right (intern (format "powerline-%s-%s"
                                                powerline-default-separator
                                                (cdr powerline-default-separator-dir)))))
          (append
           ;; battery
           (if (and active batteryp)
               (list (funcall separator-left face2 battery-face)
                     (powerline-raw (fancy-battery-default-mode-line)
                                    battery-face 'r)
                     (funcall separator-right battery-face face1))
             (list (funcall separator-right face2 face1)))
           (if (evil-visual-state-p)
               ;; selection info, if there is a selection.
               (list
                (powerline-raw " " face1)
                (powerline-raw (selection-info) face1)
                (powerline-raw " " face1)
                (funcall separator-left face1 face2)
                (powerline-raw " " face2)
                (funcall separator-right face2 face1)))
           (list
            ;; row:column
            (powerline-raw " " face1)
            (powerline-raw (if spacemacs-mode-line-display-point-p
                               (concat (format "%d | " (point)) "%l:%2c" )
                             "%l:%2c")
                           face1 'r)
            (funcall separator-left face1 line-face)
            (powerline-raw " " line-face))
           (list
            ;; global-mode
            (unless (equal '("") global-mode-string)
              (powerline-raw global-mode-string)
              (powerline-raw " " line-face))
            ;; new version
            (if (and active
                     spacemacs-new-version
                     spacemacs-mode-line-new-version-lighterp)
                (spacemacs-powerline-new-version
                 (spacemacs/get-new-version-lighter-face
                  spacemacs-version spacemacs-new-version) 'r)))
           (when (and active (not nyancatp))
             (let ((progress (format-mode-line "%p")))
               (list
                ;; percentage in the file
                (powerline-raw "%p" line-face 'r)
                ;; display hud
                (powerline-chamfer-left line-face face1)
                (if (string-match "\%" progress)
                    (powerline-hud state-face face1))))))))

      (defun spacemacs/mode-line-prepare ()
        (let* ((active (powerline-selected-window-active))
               (face2 (if active 'powerline-active2 'powerline-inactive2))
               (lhs (spacemacs/mode-line-prepare-left))
               (rhs (spacemacs/mode-line-prepare-right))
               (nyancatp (and (boundp 'nyan-mode) nyan-mode)))
          (concat (powerline-render lhs)
                  (when (and active nyancatp)
                    (powerline-render (spacemacs/powerline-nyan-cat)))
                  (powerline-fill face2 (powerline-width rhs))
                  (powerline-render rhs))))

      (setq-default mode-line-format
                    '("%e" (:eval (spacemacs/mode-line-prepare))))

      (defun spacemacs//restore-powerline (buffer)
        "Restore the powerline in buffer"
        (with-current-buffer buffer
              (setq-local mode-line-format
                          '("%e" (:eval (spacemacs/mode-line-prepare))))
              (powerline-set-selected-window)
              (powerline-reset)))

      (defun spacemacs//set-powerline-for-startup-buffers ()
        "Set the powerline for buffers created when Emacs starts."
        (unless configuration-layer-error-count
          (dolist (buffer '("*Messages*" "*spacemacs*" "*Compile-Log*"))
            (when (get-buffer buffer)
              (spacemacs//restore-powerline buffer)))))
      (add-hook 'after-init-hook
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
               projectile-vc
               )
    :init
    (progn
      (setq-default projectile-enable-caching t)
      (setq projectile-sort-order 'recentf)
      (setq projectile-cache-file (concat spacemacs-cache-directory
                                          "projectile.cache"))
      (setq projectile-known-projects-file (concat spacemacs-cache-directory
                                                   "projectile-bookmarks.eld"))
      (unless (boundp 'spacemacs-use-helm-projectile)
        (evil-leader/set-key
          "pa" 'projectile-ack
          "pA" 'projectile-ag
          "pb" 'projectile-switch-to-buffer
          "pd" 'projectile-find-dir
          "pe" 'projectile-recentf
          "pf" 'projectile-find-file
          "pg" 'projectile-grep
          "ph" 'helm-projectile
          "ps" 'projectile-switch-project))
      (evil-leader/set-key
        "p!" 'projectile-run-shell-command-in-root
        "p&" 'projectile-run-async-shell-command-in-root
        "pc" 'projectile-compile-project
        "pD" 'projectile-dired
        "pI" 'projectile-invalidate-cache
        "pk" 'projectile-kill-buffers
        "po" 'projectile-multi-occur
        "pr" 'projectile-replace
        "pR" 'projectile-regenerate-tags
        "py" 'projectile-find-tag
        "pT" 'projectile-find-test-file))
    :config
    (progn
      (projectile-global-mode)
      (spacemacs|hide-lighter projectile-mode))))

(defun spacemacs/init-rainbow-delimiters ()
  (use-package rainbow-delimiters
    :defer t
    :init
    (progn
      (evil-leader/set-key "tCd" 'rainbow-delimiters-mode)
      (add-to-hooks 'rainbow-delimiters-mode '(prog-mode-hook)))))

(defun spacemacs/init-recentf ()
  (use-package recentf
    :defer t
    :init
    ;; lazy load recentf
    (add-hook 'find-file-hook (lambda () (unless recentf-mode
                                           (recentf-mode)
                                           (recentf-track-opened-file))))
    :config
    (progn
      (setq recentf-exclude '(spacemacs-cache-directory))
      (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
      (setq recentf-save-file (concat spacemacs-cache-directory "recentf"))
      (setq recentf-max-saved-items 100)
      (setq recentf-auto-cleanup 'never)
      (setq recentf-auto-save-timer (run-with-idle-timer 600 t 'recentf-save-list)))))

(defun spacemacs/init-rfringe ()
  (use-package rfringe
    :defer t))

(defun spacemacs/init-shell ()
  (defun shell-comint-input-sender-hook ()
    "Check certain shell commands.
 Executes the appropriate behavior for certain commands."
    (setq comint-input-sender
          (lambda (proc command)
            (cond
             ;; Check for clear command and execute it.
             ((string-match "^[ \t]*clear[ \t]*$" command)
              (comint-send-string proc "\n")
              (erase-buffer))
             ;; Check for man command and execute it.
             ((string-match "^[ \t]*man[ \t]*" command)
              (comint-send-string proc "\n")
              (setq command (replace-regexp-in-string "^[ \t]*man[ \t]*" "" command))
              (setq command (replace-regexp-in-string "[ \t]+$" "" command))
              (funcall 'man command))
             ;; Send other commands to the default handler.
             (t (comint-simple-send proc command))))))
  (defun eshell/clear ()
    "Clear contents in eshell."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (add-hook 'shell-mode-hook 'shell-comint-input-sender-hook)
  (add-hook 'eshell-mode-hook (lambda ()
                                (setq pcomplete-cycle-completions nil))))

(defun spacemacs/init-smartparens ()
  (use-package smartparens
    :defer t
    :init
    (progn
      (add-to-hooks (if dotspacemacs-smartparens-strict-mode
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

      ;; don't create a pair with single quote in minibuffer
      (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)

      ;; don't create a pair with single quote in dotspacemacs-mode
      (sp-local-pair 'dotspacemacs-mode "'" nil :actions nil)

      (setq sp-cancel-autoskip-on-backward-movement nil))
    :config
    (progn
      (require 'smartparens-config)
      (spacemacs|diminish smartparens-mode " ⓟ" " p")

      ;; disabled for now because it does not play well with evil
      ;; the point has to be on behind a closing parenthesis to
      ;; trigger the highlight.
      ;; When point is on a pair, highlight the matching one
      ;; (show-smartparens-global-mode +1)

      (defun spacemacs/smartparens-pair-newline (id action context)
        (save-excursion
          (newline)
          (indent-according-to-mode)))

      (defun spacemacs/smartparens-pair-newline-and-indent (id action context)
        (spacemacs/smartparens-pair-newline id action context)
        (indent-according-to-mode))

      (sp-pair "{" nil :post-handlers
               '(:add (spacemacs/smartparens-pair-newline-and-indent "RET")))
      (sp-pair "[" nil :post-handlers
               '(:add (spacemacs/smartparens-pair-newline-and-indent "RET"))))))


(defun spacemacs/init-smooth-scrolling ()
  ;; this is not a conventional package
  ;; no require are needed for this package everything is auto-loaded
  (if dotspacemacs-smooth-scrolling
      ;; enable smooth scrolling
      (setq scroll-step 1
            scroll-conservatively 10000
            auto-window-vscroll nil
            smooth-scroll-margin 5)

    ;; deactivate the defadvice's
    (ad-disable-advice 'previous-line 'after 'smooth-scroll-down)
    (ad-activate 'previous-line)
    (ad-disable-advice 'next-line 'after 'smooth-scroll-up)
    (ad-activate 'next-line)
    (ad-disable-advice 'isearch-repeat 'after 'isearch-smooth-scroll)
    (ad-activate 'isearch-repeat)))

(defun spacemacs/init-subword ()
  (unless (version< emacs-version "24.4")
    (use-package subword
      :defer t
      :init
      (add-hook 'prog-mode-hook 'subword-mode))))

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
      (spacemacs|add-toggle whitespaces
                            :status whitespace-mode
                            :on (whitespace-mode)
                            :off (whitespace-mode -1)
                            :documentation "Display the whitespaces."
                            :evil-leader "tw")
      (spacemacs|add-toggle whitespaces-globally
                            :status whitespace-mode
                            :on (global-whitespace-mode)
                            :off (global-whitespace-mode -1)
                            :documentation "Globally display the whitespaces."
                            :evil-leader "t C-w"))
    :config
    (spacemacs|diminish whitespace-mode " ⓦ" " w")))

(defun spacemacs/init-window-numbering ()
  (use-package window-numbering
    ;; not deferred on puprose
    :init (require 'window-numbering)
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
         ((not dotspacemacs-mode-line-unicode-symbols) (concat " " str " "))
         ((equal str "1")  " ➊ ")
         ((equal str "2")  " ➋ ")
         ((equal str "3")  " ➌ ")
         ((equal str "4")  " ➍ ")
         ((equal str "5")  " ➎ ")
         ((equal str "6")  " ❻ ")
         ((equal str "7")  " ➐ ")
         ((equal str "8")  " ➑ ")
         ((equal str "9")  " ➒ ")
         ((equal str "0")  " ➓ "))))

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

(defun spacemacs/init-yasnippet ()
  (use-package yasnippet
    :commands yas-global-mode
    :init
    (progn
      (defun spacemacs/load-yasnippet ()
          (if (not (boundp 'yas-minor-mode))
              (progn
                (let* ((dir (configuration-layer/get-layer-property 'spacemacs :ext-dir))
                       (private-yas-dir (concat configuration-layer-private-directory "snippets"))
                       (yas-dir (concat dir "yasnippet-snippets")))
                  (setq yas-snippet-dirs
                        (append (when (boundp 'yas-snippet-dirs)
                                  yas-snippet-dirs)
                                (list  private-yas-dir yas-dir)))
                  (yas-global-mode 1)))))
      (add-to-hooks 'spacemacs/load-yasnippet '(prog-mode-hook
                                                markdown-mode-hook
                                                org-mode-hook))

      (spacemacs|add-toggle yasnippet
                            :status yas-minor-mode
                            :on (yas-minor-mode)
                            :off (yas-minor-mode -1)
                            :documentation "Enable yasnippet."
                            :evil-leader "ty")

      (defun spacemacs/force-yasnippet-off ()
        (yas-minor-mode -1)
        (setq yas-dont-activate t))

      (add-to-hooks 'spacemacs/force-yasnippet-off '(term-mode-hook
                                                     shell-mode-hook)))
    :config
    (progn
      (spacemacs|diminish yas-minor-mode " ⓨ" " y"))))

(defun spacemacs/init-zenburn-theme ()
  (use-package zenburn-theme
    :defer t))
