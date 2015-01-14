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
    ac-ispell
    ace-jump-mode
    ag
    aggressive-indent
    anzu
    async
    auto-complete
    auto-complete-clang
    auto-dictionary
    auto-highlight-symbol
    base16-theme
    bookmark
    buffer-move
    dash
    diminish
    dired+
    elisp-slime-nav
    eldoc
    evil
    evil-args
    evil-escape
    evil-exchange
    evil-iedit-state
    evil-indent-textobject
    evil-jumper
    evil-leader
    evil-nerd-commenter
    evil-numbers
    evil-org
    evil-search-highlight-persist
    evil-surround
    evil-terminal-cursor-changer
    evil-tutor
    evil-visualstar
    exec-path-from-shell
    expand-region
    fancy-battery
    fancy-narrow
    fill-column-indicator
    fish-mode
    flx-ido
    flycheck
    flycheck-ledger
    flyspell
    ;; required for update
    gh
    golden-ratio
    google-translate
    guide-key-tip
    helm
    helm-ag
    helm-c-yasnippet
    helm-descbinds
    helm-make
    helm-mode-manager
    ;; not working for now
    ;; helm-proc
    helm-projectile
    helm-swoop
    helm-themes
    highlight
    hl-anything
    ido-vertical-mode
    iedit
    ledger-mode
    let-alist
    leuven-theme
    linum-relative
    markdown-mode
    markdown-toc
    monokai-theme
    move-text
    multi-term
    neotree
    noflet
    org
    org-bullets
    ;; annoying error message, disable it for now
    ;; org-trello
    page-break-lines
    popup
    popwin
    powerline
    projectile
    ;; not working well for now
    ;; rainbow-blocks
    rainbow-delimiters
    ;; install fail on windows
    ;; rainbow-mode
    rcirc
    rcirc-color
    recentf
    rfringe
    s
    shell
    smartparens
    smooth-scrolling
    string-edit
    subword
    undo-tree
    use-package
    vi-tilde-fringe
    visual-regexp-steroids
    volatile-highlights
    wdired
    whitespace
    window-numbering
    yasnippet
    zenburn-theme
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar spacemacs-excluded-packages
  '(
    fancy-narrow     ; too much bugs and bad side effects
    )
  "List of packages to exclude.")

;; Paradox from MELPA is not compatible with 24.3 anymore
(unless  (version< emacs-version "24.4")
    (push 'paradox spacemacs-packages))

;; Initialization of packages

(defun spacemacs/init-ac-ispell ()
  (use-package ac-ispell
    :defer t
    :init
    (progn
      (custom-set-variables
       '(ac-ispell-requires 4))
      (eval-after-load "auto-complete"
        '(progn
           (ac-ispell-setup)))
      (add-hook 'markdown-mode-hook 'ac-ispell-ac-setup))))

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
      (evil-leader/set-key "ti" 'spacemacs/toggle-aggressive-indent))
    :config
    (spacemacs|diminish aggressive-indent-mode " Ⓘ" " I")))

(defun spacemacs/init-anzu ()
  (use-package anzu
    :init
    (global-anzu-mode t)
    :config
    (progn
      (spacemacs|hide-lighter anzu-mode)

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

      (defvar spacemacs-anzu-timer nil
        "The current timer for ephemeral anzu display.")
      (defun spacemacs/anzu-ephemeral-display ()
        "Show anzu status for a limited amount of time."
        (interactive)
        (setq spacemacs-anzu-timer nil)
        (anzu--reset-mode-line))
      (defun spacemacs/anzu-evil-search (arg func)
        "Show anzu status when pressing `n` or `N`"
        (anzu--cons-mode-line-search)
        (funcall func arg)
        (anzu--update)
        (if spacemacs-anzu-timer (cancel-timer spacemacs-anzu-timer))
        (setq spacemacs-anzu-timer
              (run-at-time "2 sec" nil 'spacemacs/anzu-ephemeral-display)))
      (evil-define-command spacemacs/anzu-evil-search-next (arg)
        "Show anzu status when executing evil-search-next"
        :repeat ignore
        (interactive "P")
        (spacemacs/anzu-evil-search arg 'evil-search-next))
      (evil-define-command spacemacs/anzu-evil-search-previous (arg)
        "Show anzu status when executing evil-search-previous"
        :repeat ignore
        (interactive "P")
        (spacemacs/anzu-evil-search arg 'evil-search-previous))
      (define-key evil-normal-state-map "n" 'spacemacs/anzu-evil-search-next)
      (define-key evil-normal-state-map "N" 'spacemacs/anzu-evil-search-previous)
      (define-key evil-motion-state-map "n" 'spacemacs/anzu-evil-search-next)
      (define-key evil-motion-state-map "N" 'spacemacs/anzu-evil-search-previous)

      (setq anzu-search-threshold 1000
            anzu-cons-mode-line-p nil
            anzu-mode-line-update-function 'spacemacs/anzu-update-mode-line))))

(defun spacemacs/init-auto-complete ()
  (use-package auto-complete
    :commands global-auto-complete-mode
    :init
    (add-to-hooks 'auto-complete-mode '(org-mode-hook
                                        prog-mode-hook))
    :idle (global-auto-complete-mode)
    :idle-priority 1
    :config
    (progn
      (require 'auto-complete-config)
      (ac-config-default)
      (when (configuration-layer/package-declaredp 'yasnippet)
        (push 'ac-source-yasnippet ac-sources))
      (add-to-list 'completion-styles 'initials t)
      (evil-leader/set-key "ta" 'auto-complete-mode)
      (define-key ac-completing-map (kbd "C-j") 'ac-next)
      (define-key ac-completing-map (kbd "C-k") 'ac-previous)
      (define-key ac-completing-map (kbd "<S-tab>") 'ac-previous)
      ;; customization
      (setq ac-auto-start 2
            ac-delay 0.
            ac-quick-help-delay 1.
            ac-use-fuzzy t
            ac-fuzzy-enable t
            ac-comphist-file (concat spacemacs-cache-directory "ac-comphist.dat")
            tab-always-indent 'complete ; use 'complete when auto-complete is disabled
            ac-dwim t)
      (spacemacs|diminish auto-complete-mode " Ⓐ" " A"))))

(defun spacemacs/init-auto-complete-clang ()
  (use-package auto-complete-clang
    :defer t
    :config
    (progn
      (setq ac-clang-flags
            (mapcar (lambda (item)(concat "-I" item))
                    (split-string
                     "
 /usr/include/c++/4.7
 /usr/include/i386-linux-gnu/c++/4.7/.
 /usr/include/c++/4.7/backward
 /usr/lib/gcc/i686-linux-gnu/4.7/include
 /usr/local/include
 /usr/lib/gcc/i686-linux-gnu/4.7/include-fixed
 /usr/include/i386-linux-gnu
 /usr/include
 "
                     ))))))

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
    :commands auto-highlight-symbol-mode
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
                   (eval '(progn (ahs-highlight-now) (ahs-back-to-start)) nil))
          (message "No symbol has been searched for now.")))

      (defun spacemacs/integrate-evil-search (forward)
        (eval '(progn
                 ;; isearch-string is last searched item.  Next time
                 ;; "n" is hit we will use this.
                 (setq isearch-string (concat "\\<" (evil-find-thing forward 'symbol) "\\>"))
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
                 ) nil))

      (defun spacemacs/quick-ahs-forward ()
        "Go to the next occurrence of symbol under point with
`auto-highlight-symbol'"
        (interactive)
        (eval '(progn (spacemacs/integrate-evil-search t) (ahs-highlight-now) (ahs-forward)) nil))

      (defun spacemacs/quick-ahs-backward ()
        "Go to the previous occurrence of symbol under point with
`auto-highlight-symbol'"
        (interactive)
        (eval '(progn (spacemacs/integrate-evil-search nil) (ahs-highlight-now) (ahs-backward)) nil))

      (eval-after-load 'evil
        '(progn
           (define-key evil-motion-state-map (kbd "*") 'spacemacs/quick-ahs-forward)
           (define-key evil-motion-state-map (kbd "#") 'spacemacs/quick-ahs-backward)))

      (defun spacemacs/symbol-highlight ()
        "Highlight the symbol under point with `auto-highlight-symbol'."
        (interactive)
        (eval '(progn
                 (ahs-highlight-now)
                 (setq spacemacs-last-ahs-highlight-p (ahs-highlight-p))
                 (spacemacs/auto-highlight-symbol-overlay-map)) nil))

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
                   (ahs-highlight-now)
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
               (propplugin (propertize plugin 'face `(
                                                      :foreground "#ffffff"
                                                      :background ,(face-attribute
                                                                    'ahs-plugin-defalt-face :foreground)))))
          (while (not (string= overlay current-overlay))
            (setq i (1+ i))
            (setq overlay (format "%s" (nth i ahs-overlay-list))))
          (let* ((x/y (format "[%s/%s]" (- overlay-count i) overlay-count))
                 (propx/y (propertize x/y 'face ahs-plugin-whole-buffer-face))
                 (hidden (if (< 0 (- overlay-count (nth 4 st))) "*" ""))
                 (prophidden (propertize hidden 'face '(:weight bold))))
            (echo "%s %s%s press (n/N) to navigate, (e) to edit, (r) to change range or (R) for reset"
                  propplugin propx/y prophidden)))))))

(defun spacemacs/init-bookmark ()
  (use-package bookmark
    :commands (bookmark-delete
               bookmark-jump
               bookmark-rename
               bookmark-set)
    :config
    (setq
     bookmark-default-file "~/.emacs.d/bookmarks" ; keep my ~/ clean
     bookmark-save-flag 1)))                      ; autosave each change

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
    '(diminish 'subword-mode)))

(defun spacemacs/init-dired+ ()
  (use-package dired+
    :defer t))

(defun spacemacs/init-elisp-slime-nav ()
  ;; Elisp go-to-definition with M-. and back again with M-,
  (use-package elisp-slime-nav
    :defer t
    :init
    (add-hook 'emacs-lisp-mode-hook 'elisp-slime-nav-mode)))

(defun spacemacs/init-eldoc ()
  (use-package eldoc
    :defer t
    :init (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
    :config
    (progn
      (spacemacs|hide-lighter eldoc-mode))))

(defun spacemacs/init-evil ()
  (use-package evil
    :init
    (progn
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
                '((normal . "DarkGoldenrod2")
                  (insert . "chartreuse3")
                  (emacs  . "SkyBlue2")
                  (visual . "gray")
                  (motion . "plum3")
                  (lisp   . "HotPink1"))))
      (spacemacs/set-state-faces)

      (defun set-default-evil-emacs-state-cursor ()
        (setq evil-emacs-state-cursor `(,(spacemacs/state-color 'emacs) box)))
      (defun set-default-evil-normal-state-cursor ()
        (setq evil-normal-state-cursor `(,(spacemacs/state-color 'normal) box)))
      (defun set-default-evil-insert-state-cursor ()
        (setq evil-insert-state-cursor `(,(spacemacs/state-color 'insert) (bar . 2))))
      (defun set-default-evil-visual-state-cursor ()
        (setq evil-visual-state-cursor `(,(spacemacs/state-color 'visual) (hbar . 2))))
      (defun set-default-evil-motion-state-cursor ()
        (setq evil-motion-state-cursor `(,(spacemacs/state-color 'motion) box)))
      (defun set-default-evil-lisp-state-cursor ()
        (setq evil-lisp-state-cursor `(,(spacemacs/state-color 'lisp) box)))
      (defun evil-insert-state-cursor-hide ()
        (setq evil-insert-state-cursor `(,(spacemacs/state-color 'insert) (hbar . 0))))
      (set-default-evil-emacs-state-cursor)
      (set-default-evil-normal-state-cursor)
      (set-default-evil-insert-state-cursor)
      (set-default-evil-visual-state-cursor)
      (set-default-evil-motion-state-cursor)
      (set-default-evil-lisp-state-cursor)
      (evil-mode 1))
    :config
    (progn
      ;; evil ex-command key
      (define-key evil-motion-state-map (kbd dotspacemacs-command-key) 'evil-ex)
      ;; Make evil-mode up/down operate in screen lines instead of logical lines
      (define-key evil-normal-state-map "j" 'evil-next-visual-line)
      (define-key evil-normal-state-map "k" 'evil-previous-visual-line)
      ;; Make the current definition and/or comment visible.
      (define-key evil-normal-state-map "zf" 'reposition-window)
      ;; quick navigation
      (define-key evil-normal-state-map (kbd "L")
        (lambda () (interactive)
          (evil-window-bottom)
          (let ((recenter-redisplay nil))
            (recenter nil))))
      (define-key evil-normal-state-map (kbd "H")
        (lambda () (interactive)
          (evil-window-top)
          (let ((recenter-redisplay nil))
            (recenter nil))))
      (evil-leader/set-key "re" 'evil-show-registers)
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
  (spacemacs/defface-state-color 'iedit "firebrick1")
  (spacemacs/defface-state-color 'iedit-insert "firebrick1")

  (defun spacemacs/evil-state-lazy-loading ()
    (require 'evil-iedit-state)
    (setq evil-iedit-state-cursor `(,(spacemacs/state-color 'iedit) box))   
    (setq evil-iedit-insert-state-cursor `((spacemacs/state-color 'iedit-insert) (bar . 2)))
    ;; activate leader in iedit and iedit-insert states
    (define-key evil-iedit-state-map
      (kbd evil-leader/leader) evil-leader--default-map)
    ;; evil-escape support
    (when (and (boundp 'evil-escape-mode)
               (symbol-value evil-escape-mode))
      (key-chord-define evil-iedit-state-map
                        evil-escape-key-sequence
                        'evil-iedit-state/quit-iedit-mode)
      (key-chord-define evil-iedit-insert-state-map
                        evil-escape-key-sequence
                        'evil-iedit-state/quit-iedit-mode)))

  (evil-leader/set-key "se" 'evil-iedit-state/iedit-mode)
  (add-to-hooks 'spacemacs/evil-state-lazy-loading '(prog-mode-hook
                                                     markdown-mode-hook)))

(defun spacemacs/init-evil-indent-textobject ()
  (use-package evil-indent-textobject))

(defun spacemacs/init-evil-jumper ()
  (use-package evil-jumper
    :init
    (setq evil-jumper-auto-center t)
    (setq evil-jumper-file (concat spacemacs-cache-directory "evil-jumps"))
    (setq evil-jumper-auto-save-interval 3600)))

(defun spacemacs/init-evil-leader ()
  (use-package evil-leader
    :init
    (progn
      (setq evil-leader/in-all-states t
            evil-leader/leader dotspacemacs-leader-key
            evil-leader/non-normal-prefix "s-")
      (global-evil-leader-mode))
    :config
    (progn
      ;; Unset shortcuts which shadow evil leader
      (eval-after-load "compile"
        '(progn
           (define-key compilation-mode-map (kbd dotspacemacs-leader-key) nil)
           (define-key compilation-mode-map (kbd "h") nil)))
      (eval-after-load "dired"
        '(define-key dired-mode-map (kbd dotspacemacs-leader-key) nil))
      ;; make leader available in visual mode
      (define-key evil-visual-state-map (kbd dotspacemacs-leader-key)
        evil-leader--default-map)
      (define-key evil-motion-state-map (kbd dotspacemacs-leader-key)
        evil-leader--default-map)
      ;; experimental: invoke leader with "jk" in insert mode
      (when dotspacemacs-feature-toggle-leader-on-jk
        (key-chord-define evil-insert-state-map (kbd "jk")
                          evil-leader--default-map))
      ;; experimental: map SPC m to ,
      (when dotspacemacs-major-mode-leader-key
        (add-hook 'after-change-major-mode-hook 'spacemacs/activate-major-mode-leader))
        )))

(defun spacemacs/init-evil-nerd-commenter ()
  (use-package evil-nerd-commenter
    :init
    (progn
      (evil-leader/set-key
        ";"  'evilnc-comment-operator
        "cl" 'evilnc-comment-or-uncomment-lines
        "ci" 'evilnc-toggle-invert-comment-line-by-line
        "cp" 'evilnc-comment-or-uncomment-paragraphs
        "ct" 'evilnc-quick-comment-or-uncomment-to-the-line
        "cy" 'evilnc-copy-and-comment-lines))))

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

(defun spacemacs/init-evil-org ()
  (use-package evil-org
    :commands evil-org-mode
    :init
    (add-hook 'org-mode-hook 'evil-org-mode)
    :config
    (progn
      ;; to gather all the bindings at the same place the bindnings
      ;; for evil-org have been moved to `spacemacs/init-org'
      (spacemacs|diminish evil-org-mode " Ⓔ" " E"))))

(defun spacemacs/init-evil-search-highlight-persist ()
  (use-package evil-search-highlight-persist
    :init
    (progn
      (global-evil-search-highlight-persist)
      (evil-leader/set-key "sc" 'evil-search-highlight-persist-remove-all)
      (evil-ex-define-cmd "noh" 'evil-search-highlight-persist-remove-all))))

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
    (require 'evil-terminal-cursor-changer)))

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
    :init (when (memq window-system '(mac ns))
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

(defun spacemacs/init-fancy-narrow ()
  (use-package fancy-narrow
    :defer t
    :init
    (evil-leader/set-key
      "nr" 'fancy-narrow-to-region
      "np" 'fancy-narrow-to-page
      "nf" 'fancy-narrow-to-defun
      "nw" 'fancy-widen)
    ))

(defun spacemacs/init-fill-column-indicator ()
  (setq fci-rule-width 1)
  (setq fci-enabled 0)

  (defun toggle-fill-column-indicator ()
    (interactive)
    (make-local-variable 'fci-enabled)
    (if (> fci-enabled 0) (deactivate-fci) (activate-fci)))

  (defun activate-fci ()
    (setq fci-rule-column 79)
    (setq fci-enabled 1)
    (fci-mode 1))

  (defun deactivate-fci ()
    (setq fci-enabled 0)
    (fci-mode 0))

  (use-package fill-column-indicator
    :commands toggle-fill-column-indicator))

(defun spacemacs/init-flx-ido ()
  (use-package flx-ido
    :init (flx-ido-mode 1)))

(defun spacemacs/init-flycheck ()
  (use-package flycheck
    :defer t
    :config
    (progn
      (spacemacs|diminish flycheck-mode " Ⓕ" " F")

      (setq flycheck-check-syntax-automatically '(save mode-enabled)
            flycheck-standard-error-navigation nil)

      (defun spacemacs/mode-line-flycheck-info-toggle ()
        "Toggle display of flycheck info."
        (interactive)
        (if flycheck-mode
            (flycheck-mode -1)
          (flycheck-mode)))
      (evil-leader/set-key "tmf" 'spacemacs/mode-line-flycheck-info-toggle)

      ;; color mode line faces
      (defun spacemacs/defface-flycheck-mode-line-color (state)
        "Define a face for the given Flycheck STATE."
        (let* ((fname (intern (format "spacemacs-mode-line-flycheck-%s-face"
                                      (symbol-name state))))
              (foreground (face-foreground
                           (intern (format "flycheck-fringe-%s" state)))))
          (eval `(defface ,fname '((t ()))
                   ,(format "Color for Flycheck %s feedback in mode line."
                            (symbol-name state))
                   :group 'spacemacs))
          (set-face-attribute fname nil
                              :foreground foreground
                              :box (face-attribute 'mode-line :box))))

      (defun spacemacs/set-flycheck-mode-line-faces ()
        "Define or set the flycheck info mode-line faces."
        (mapcar 'spacemacs/defface-flycheck-mode-line-color
                '(error warning info)))
      (spacemacs/set-flycheck-mode-line-faces)

      (defmacro spacemacs|custom-flycheck-lighter (error)
        "Return a formatted string for the given ERROR (error, warning, info)."
        `(let* ((error-counts (flycheck-count-errors
                               flycheck-current-errors))
                (errorp (flycheck-has-current-errors-p ',error))
                (err (or (cdr (assq ',error error-counts)) "?"))
                (running (eq 'running flycheck-last-status-change)))
           (if (or errorp running) (format "•%s " err))))

      ;; Custom fringe indicator
      (when (fboundp 'define-fringe-bitmap)
        (define-fringe-bitmap 'my-flycheck-fringe-indicator
          (vector #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00011100
                  #b00111110
                  #b00111110
                  #b00111110
                  #b00011100
                  #b00000000
                  #b00000000
                  #b00000000
                  #b00000000
                  #b01111111)))

      (flycheck-define-error-level 'error
        :overlay-category 'flycheck-error-overlay
        :fringe-bitmap 'my-flycheck-fringe-indicator
        :fringe-face 'flycheck-fringe-error)

      (flycheck-define-error-level 'warning
        :overlay-category 'flycheck-warning-overlay
        :fringe-bitmap 'my-flycheck-fringe-indicator
        :fringe-face 'flycheck-fringe-warning)

      (flycheck-define-error-level 'info
        :overlay-category 'flycheck-info-overlay
        :fringe-bitmap 'my-flycheck-fringe-indicator
        :fringe-face 'flycheck-fringe-info)

      (defun spacemacs/next-error (&optional n reset)
        "Dispatch to flycheck or standard emacs error."
        (interactive "P")
        (if (and (boundp 'flycheck-mode)
                 (symbol-value flycheck-mode))
            (call-interactively 'flycheck-next-error)
          (call-interactively 'next-error)))

      (defun spacemacs/previous-error (&optional n reset)
        "Dispatch to flycheck or standard emacs error."
        (interactive "P")
        (if (and (boundp 'flycheck-mode)
                 (symbol-value flycheck-mode))
            (call-interactively 'flycheck-previous-error)
          (call-interactively 'previous-error)))

      ;; key bindings
      (evil-leader/set-key
        "ec" 'flycheck-clear
        "ef" 'flycheck-mode
        "el" 'flycheck-list-errors
        "en" 'spacemacs/next-error
        "eN" 'spacemacs/previous-error))))

(defun spacemacs/init-flycheck-ledger ()
  (eval-after-load 'flycheck
    '(require 'flycheck-ledger)))

(defun spacemacs/init-flyspell ()
  (use-package flyspell
    :defer t
    :init
    (progn
      (setq-default ispell-program-name "aspell")
      (setq-default ispell-dictionary "english")
      (add-hook 'markdown-mode-hook '(lambda () (flyspell-mode 1)))
      (add-hook 'text-mode-hook '(lambda () (flyspell-mode 1))))
    :config
    (spacemacs|diminish flyspell-mode " Ⓢ" " S")))

(defun spacemacs/init-golden-ratio ()
  (use-package golden-ratio
    :defer t
    :init
    (progn
      (defun spacemacs/toggle-golden-ratio ()
        "Toggle golden-ratio mode on and off."
        (interactive)
        (if (symbol-value golden-ratio-mode)
            (progn (golden-ratio-mode -1)(balance-windows))
          (golden-ratio-mode)
          (golden-ratio)))
      (evil-leader/set-key "tg" 'spacemacs/toggle-golden-ratio))
    :config
    (progn
      (setq golden-ratio-extra-commands
            (append golden-ratio-extra-commands
                    '(windmove-left
                      windmove-right
                      windmove-up
                      windmove-down
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

      (spacemacs|diminish golden-ratio-mode " ⊞" " G"))))

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
      (evil-leader/set-key "tk" 'spacemacs/toggle-guide-key)
      (setq guide-key/guide-key-sequence `("C-x"
                                           "C-c"
                                           ,dotspacemacs-leader-key
                                           ,dotspacemacs-major-mode-leader-key
                                           "g"
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
      (spacemacs|diminish guide-key-mode " Ⓚ" " K"))))

(defun spacemacs/init-helm ()
  (use-package helm
    :defer t
    :init
    (setq helm-split-window-in-side-p nil
          helm-bookmark-show-location t
          helm-buffers-fuzzy-matching t
          helm-always-two-windows     t)
    (evil-leader/set-key
        dotspacemacs-command-key 'helm-M-x
        "bs"  'helm-mini
        "sl"  'helm-semantic-or-imenu
        "hb"  'helm-bookmarks
        "hl"  'helm-resume
        "ry"  'helm-show-kill-ring
        "rr"  'helm-register
        "rm"  'helm-all-mark-rings
        "fh"  'helm-find-files
        "fr"  'helm-recentf
        "<f1>" 'helm-apropos
        )
    :config
    (progn
      (helm-mode +1)

      ;; alter helm-bookmark key bindings to be simpler
      (defun simpler-helm-bookmark-keybindings ()
        (define-key helm-bookmark-map (kbd "C-d") 'helm-bookmark-run-delete)
        (define-key helm-bookmark-map (kbd "C-e") 'helm-bookmark-run-edit)
        (define-key helm-bookmark-map (kbd "C-f") 'helm-bookmark-toggle-filename)
        (define-key helm-bookmark-map (kbd "C-o") 'helm-bookmark-run-jump-other-window)
        (define-key helm-bookmark-map (kbd "C-/") 'helm-bookmark-help))
      (add-hook 'helm-mode-hook 'simpler-helm-bookmark-keybindings)
      ;; helm navigation on hjkl
      (define-key helm-map (kbd "C-j") 'helm-next-line)
      (define-key helm-map (kbd "C-k") 'helm-previous-line)
      (define-key helm-map (kbd "C-h") 'helm-next-source)
      (define-key helm-map (kbd "C-l") 'helm-previous-source)
      ;; experimental: toggle evil-leader with "jk" with helm specific commands
      (when dotspacemacs-feature-toggle-leader-on-jk
        (evil-leader/set-key-for-mode 'helm-mode
          "1" (lambda () (interactive) (helm-select-nth-action 0))
          "2" (lambda () (interactive) (helm-select-nth-action 1))
          "3" (lambda () (interactive) (helm-select-nth-action 2))
          "4" (lambda () (interactive) (helm-select-nth-action 3))
          "5" (lambda () (interactive) (helm-select-nth-action 4))
          "6" (lambda () (interactive) (helm-select-nth-action 5))
          "7" (lambda () (interactive) (helm-select-nth-action 6))
          "8" (lambda () (interactive) (helm-select-nth-action 7))
          "9" (lambda () (interactive) (helm-select-nth-action 8))
          "0" (lambda () (interactive) (helm-select-nth-action 9))
          "a" 'helm-select-action)
        (key-chord-define helm-map (kbd "jk") (cdr (assoc 'helm-mode evil-leader--mode-maps))))

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

      (eval-after-load "helm-mode" ; required
        '(spacemacs|hide-lighter helm-mode)))))

(defun spacemacs/init-helm-ag ()
  (use-package helm-ag
    :defer t))

(defun spacemacs/init-helm-descbinds ()
  (use-package helm-descbinds
    :defer t
    :init
    (evil-leader/set-key "?" 'helm-descbinds)))

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
    (defconst spacemacs-use-helm-projectile t
      "This variable is only defined if helm-projectile is used.")
    (evil-leader/set-key
      "/" 'helm-projectile-ag
      "pa" 'helm-projectile-ag
      "pA" 'helm-projectile-ack
      "pb" 'helm-projectile-switch-to-buffer
      "pd" 'helm-projectile-find-dir
      "pe" 'helm-projectile-recentf
      "pf" 'helm-projectile-find-file
      "pg" 'helm-projectile-grep
      "ph" 'helm-projectile
      "ps" 'helm-projectile-switch-project
      "pv" 'helm-projectile-vc)))

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
      "s C-s" 'helm-multi-swoop-all)))

(defun spacemacs/init-helm-themes ()
  (use-package helm-themes
    :defer t
    :init
    (evil-leader/set-key "Th" 'helm-themes)))

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
        "hn"  'hl-find-thing-forwardly
        "hN"  'hl-find-thing-backwardly
        "hp"  'hl-paren-mode
        "hr"  'hl-restore-highlights
        "hs"  'hl-save-highlights))
    :config
    (progn
      (spacemacs|diminish hl-paren-mode " (Ⓗ)" " (H)")
      (spacemacs|hide-lighter hl-highlight-mode))))

(defun spacemacs/init-ido-vertical-mode ()
  (use-package ido-vertical-mode
    :init
    (progn
      (ido-vertical-mode t)
      (defun spacemacs//ido-vertical-define-keys ()
        ;; overwrite the key bindings for ido vertical mode only
        (define-key ido-completion-map (kbd "C-d") 'ido-delete-file-at-head)
        (define-key ido-completion-map (kbd "C-k") 'ido-prev-match)
        (define-key ido-completion-map (kbd "C-<return>") 'ido-select-text)
        (define-key ido-completion-map (kbd "M-<RET>") 'ido-select-text)
        (define-key ido-completion-map (kbd "C-h") 'ido-delete-backward-updir)
        (define-key ido-completion-map (kbd "C-j") 'ido-next-match)
        (define-key ido-completion-map (kbd "C-l") 'ido-exit-minibuffer)
        (define-key ido-completion-map (kbd "C-S-j") 'ido-next-match-dir)
        (define-key ido-completion-map (kbd "C-S-k") 'ido-prev-match-dir)
        ;; history navigation
        (define-key ido-completion-map (kbd "C-n") 'next-history-element)
        (define-key ido-completion-map (kbd "C-p") 'previous-history-element)
        ;; ido-other window maps
        (define-key ido-completion-map (kbd "C-o") 'ido-invoke-in-other-window)
        (define-key ido-completion-map (kbd "C-v") 'ido-invoke-in-vertical-split)
        (define-key ido-completion-map (kbd "C-b") 'ido-invoke-in-horizontal-split)
        (define-key ido-completion-map (kbd "C-t") 'ido-invoke-in-new-frame)
        ;; more natural navigation keys: up, down to change current item
        ;; left to go up dir
        ;; right to open the selected item
        (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
        (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
        (define-key ido-completion-map (kbd "<left>") 'ido-delete-backward-updir)
        (define-key ido-completion-map (kbd "<right>") 'ido-exit-minibuffer)
        (when dotspacemacs-feature-toggle-leader-on-jk
          (evil-leader/set-key-for-mode 'ido-mode
            "b" 'ido-invoke-in-horizontal-split
            "t" 'ido-invoke-in-new-frame
            "v" 'ido-invoke-in-vertical-split
            "x" 'ido-invoke-in-other-window)
          (key-chord-define ido-completion-map (kbd "jk")
                            (cdr (assoc 'ido-mode evil-leader--mode-maps)))))
      (add-to-list 'ido-setup-hook 'spacemacs//ido-vertical-define-keys))
      ))

(defun spacemacs/init-iedit ()
  (use-package iedit
    :defer t))

(defun spacemacs/init-ledger-mode ()
  (use-package ledger-mode
    :mode ("\\.\\(ledger\\|ldg\\)\\'" . ledger-mode)
    :defer t
    :init
    (progn
      (setq ledger-post-amount-alignment-column 62)
      (evil-leader/set-key-for-mode 'ledger-mode
        "mhd" 'ledger-delete-current-transaction
        "ma"  'ledger-add-transaction))))

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

(defun spacemacs/init-markdown-mode ()
  (use-package markdown-mode
    :mode ("\\.md" . markdown-mode)
    :defer t
    :init
    (eval-after-load 'smartparens
      '(add-hook 'markdown-mode-hook 'smartparens-mode))
    :config
    ;; Don't do terrible things with Github code blocks (```)
    (when (fboundp 'sp-local-pair)
      (sp-local-pair 'markdown-mode "`" nil :actions '(:rem autoskip))
      (sp-local-pair 'markdown-mode "'" nil :actions nil))))

(defun spacemacs/init-markdown-toc ()
  (use-package markdown-toc
    :defer t))

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
      (setq multi-term-program "/bin/zsh")

      (defun term-send-tab ()
        "Send tab in term mode."
        (interactive)
        (term-send-raw-string "\t"))

      (add-to-list 'term-bind-key-alist '("<tab>" . term-send-tab)))))

(defun spacemacs/init-neotree ()
  (use-package neotree
    :defer t
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
        (if (fboundp 'global-vi-tilde-fringe-mode)
            (vi-tilde-fringe-mode -1)))

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
        (define-key evil-motion-state-local-map (kbd "s")   'neotree-hidden-file-toggle))

      (evil-leader/set-key "ft" 'neotree-toggle))
    :config
    (add-to-hook 'neotree-mode-hook '(spacemacs//init-neotree
                                      spacemacs//neotree-key-bindings))))

(defun spacemacs/init-org ()
  (use-package org
    :mode ("\\.org$" . org-mode)
    :defer t
    :init
    (progn
      (setq org-log-done t)
      (add-hook 'org-mode-hook 'org-indent-mode)
      (evil-leader/set-key-for-mode 'org-mode
        "mc" 'org-capture
        "md" 'org-deadline
        "me" 'org-export-dispatch
        "mi" 'org-clock-in
        "mo" 'org-clock-out
        "mm" 'org-ctrl-c-ctrl-c
        "mr" 'org-refile
        "ms" 'org-schedule)
      (eval-after-load 'evil-org
        ;; move the leader bindings to `m` prefix to be consistent with
        ;; the rest of spacemacs bindings
        '(evil-leader/set-key-for-mode 'org-mode
           "a" nil "ma" 'org-agenda
           "c" nil "mA" 'org-archive-subtree
           "o" nil "mC" 'evil-org-recompute-clocks
           "l" nil "ml" 'evil-org-open-links
           "t" nil "mt" 'org-show-todo-tree)))
    :config
    (progn
      (require 'org-install)
      (define-key global-map "\C-cl" 'org-store-link)
      (define-key global-map "\C-ca" 'org-agenda)
      (use-package org-bullets
        :config
        (defun spacemacs//org-mode-hook ()
          (org-bullets-mode 1))
        (add-hook 'org-mode-hook 'spacemacs//org-mode-hook))
      ;; (use-package org-trello
      ;;   :config
      ;;   (add-hook 'org-mode-hook 'org-trello-mode))
      ))

  (eval-after-load "org-agenda"
    '(progn
       (define-key org-agenda-mode-map "j" 'org-agenda-next-line)
       (define-key org-agenda-mode-map "k" 'org-agenda-previous-line))))

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

      (add-to-list 'evil-emacs-state-modes 'paradox-menu-mode)
      (spacemacs|evilify paradox-menu-mode-map
        "H" 'paradox-menu-quick-help
        "J" 'paradox-next-describe
        "K" 'paradox-previous-describe
        "L" 'paradox-menu-view-commit-list
        "o" 'paradox-menu-visit-homepage)
      (evil-leader/set-key
        "aP" 'spacemacs/paradox-list-packages))
    :config
    (setq paradox-execute-asynchronously nil)
    (spacemacs/activate-evil-leader-for-map 'paradox-menu-mode-map)
    ))

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
      (push '("^\*Flycheck.+\*$" :regexp t :dedicated t :position bottom :stick t :noselect t) popwin:special-display-config)
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

      ;; for now we hardcode the height value of powerline depending on the
      ;; window system, a better solution would be to compute it correctly
      ;; in powerline package.
      (let ((height (if (eq 'w32 window-system) 18 17)))
        (setq-default powerline-height height))
      (setq-default powerline-default-separator 'wave)

      (defun spacemacs/mode-line-prepare-left ()
        (let* ((active (powerline-selected-window-active))
               (line-face (if active 'mode-line 'mode-line-inactive))
               (face1 (if active 'powerline-active1 'powerline-inactive1))
               (face2 (if active 'powerline-active2 'powerline-inactive2))
               (state-face (if active (spacemacs/current-state-face) face2))
               (window-numberingp (and (boundp 'window-numbering-mode)
                                       (symbol-value window-numbering-mode)))
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
           (if (and active anzu--state)
               (list
                (funcall separator-right state-face face1)
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
            (if active (funcall separator-right face1 line-face)))
           ;; flycheck
           (if (and active flycheckp)
               (list
                (powerline-raw " " line-face)
                (powerline-raw (spacemacs|custom-flycheck-lighter error)
                               'spacemacs-mode-line-flycheck-error-face)
                (powerline-raw (spacemacs|custom-flycheck-lighter warning)
                               'spacemacs-mode-line-flycheck-warning-face)
                (powerline-raw (spacemacs|custom-flycheck-lighter info)
                               'spacemacs-mode-line-flycheck-info-face)))
           ;; separator between flycheck and minor modes
           (if (and active flycheckp spacemacs-mode-line-minor-modesp)
               (list
                (funcall separator-left line-face face1)
                (powerline-raw "  " face1)
                (funcall separator-right face1 line-face)))
           ;; minor modes
           (if (and active spacemacs-mode-line-minor-modesp)
               (list
                (spacemacs-powerline-minor-modes line-face 'l)
                (powerline-raw mode-line-process line-face 'l)
                (powerline-raw " " line-face)))
           ;; version control
           (if (and active (or flycheckp spacemacs-mode-line-minor-modesp))
               (list (funcall separator-left (if vc-face line-face face1) vc-face)))
           (if active (list (powerline-vc vc-face)
                            (powerline-raw " " vc-face)
                            (funcall separator-right vc-face face2))
             (list (funcall separator-right face1 face2))))))

      (defun spacemacs/mode-line-prepare-right ()
        (let* ((active (powerline-selected-window-active))
               (line-face (if active 'mode-line 'mode-line-inactive))
               (face1 (if active 'powerline-active1 'powerline-inactive1))
               (face2 (if active 'powerline-active2 'powerline-inactive2))
               (state-face (if active (spacemacs/current-state-face) face2))
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
           (list
            ;; row:column
            (powerline-raw " " face1)
            (powerline-raw "%l:%2c" face1 'r)
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
           (list
            ;; percentage in the file
            (powerline-raw "%p" line-face 'r)
            (when active
              ;; display hud only if necessary
              (powerline-chamfer-left line-face face1)
              (let ((progress (format-mode-line "%p")))
                (if (string-match "\%" progress)
                    (powerline-hud state-face face1))))))))

      (defun spacemacs/mode-line-prepare ()
        (let* ((active (powerline-selected-window-active))
               (face2 (if active 'powerline-active2 'powerline-inactive2))
               (lhs (spacemacs/mode-line-prepare-left))
               (rhs (spacemacs/mode-line-prepare-right)))
          (concat (powerline-render lhs)
                  (powerline-fill face2 (powerline-width rhs))
                  (powerline-render rhs))))

      (setq-default mode-line-format
                    '("%e" (:eval (spacemacs/mode-line-prepare)))))))

(defun spacemacs/init-projectile ()
  (use-package projectile
    :commands (projectile-ack
               projectile-ag
               projectile-find-file
               projectile-find-test-file
               projectile-switch-to-buffer
               projectile-find-dir
               projectile-dired
               projectile-vc
               projectile-replace
               projectile-regenerate-tags
               projectile-grep
               projectile-switch-project
               projectile-multi-occur
               projectile-find-tag
               projectile-kill-buffers
               projectile-recentf
               projectile-invalidate-cache)
    :init
    (progn
      (setq-default projectile-enable-caching t)
      (setq projectile-cache-file (concat spacemacs-cache-directory
                                          "projectile.cache"))
      (setq projectile-known-projects-file (concat spacemacs-cache-directory
                                                   "projectile-bookmarks.eld"))
      (unless (boundp spacemacs-use-helm-projectile)
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
        "pD" 'projectile-dired
        "pI" 'projectile-invalidate-cache
        "pk" 'projectile-kill-buffers
        "po" 'projectile-multi-occur
        "pr" 'projectile-replace
        "pR" 'projectile-regenerate-tags
        "pt" 'projectile-find-tag
        "pT" 'projectile-find-test-file))
      :config
      (progn
        (projectile-global-mode)
        (spacemacs|hide-lighter projectile-mode))))

(defun spacemacs/init-rainbow-blocks ()
  (use-package rainbow-blocks
    :disabled t
    :init
    (progn (add-hook 'emacs-lisp-mode-hook 'rainbow-blocks-mode))))

(defun spacemacs/init-rainbow-delimiters ()
  (use-package rainbow-delimiters
    :defer t
    :init
    (progn
      (defun turn-on-rainbow-delimiters-mode ()
        (interactive)
        (rainbow-delimiters-mode 1))
      (add-to-hooks
       'turn-on-rainbow-delimiters-mode '(prog-mode-hook)))))

(defun spacemacs/init-rainbow-mode ()
  (use-package rainbow-mode
    :defer t
    :init (evil-leader/set-key "tc" 'rainbow-mode)
    :config
    (spacemacs|hide-lighter rainbow-mode)))

(defun spacemacs/init-rcirc ()
  (use-package rcirc
    :commands irc
    :init
    (progn
      (add-to-hook 'rcirc-mode-hook '(rcirc-track-minor-mode
                                      rcirc-omit-mode
                                      ;; rcirc-reconnect-mode
                                      flyspell-mode))
      (setq evil-normal-state-modes
            (cons 'rcirc-mode evil-normal-state-modes)))
    :config
    (progn
      (setq rcirc-fill-column 80
            rcirc-buffer-maximum-lines 2048
            rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY")
            rcirc-omit-threshold 20)
      (require 'rcirc-color)
      (let ((dir (configuration-layer/get-layer-property 'spacemacs :ext-dir)))
        (require 'rcirc-reconnect
                 (concat dir "rcirc-reconnect/rcirc-reconnect.el")))
      ;; identify info are stored in a separate location, skip errors
      ;; if the feature cannot be found.
      (require 'pinit-rcirc nil 'noerror)
      (define-key rcirc-mode-map (kbd "C-j") 'rcirc-insert-prev-input)
      (define-key rcirc-mode-map (kbd "C-k") 'rcirc-insert-next-input)
      )))

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
      (setq recentf-exclude '("~/.emacs.d/.cache"))
      (add-to-list 'recentf-exclude "COMMIT_EDITMSG\\'")
      (setq recentf-save-file (concat spacemacs-cache-directory "/recentf"))
      (setq recentf-max-saved-items 100)
      (setq recentf-auto-cleanup 'never)
      (setq recentf-auto-save-timer (run-with-idle-timer 600 t 'recentf-save-list)))))

(defun spacemacs/init-rfringe ()
  (use-package rfringe
    :defer t))

(defun spacemacs/init-ruby-mode ()
  (use-package ruby-mode
    :defer t
    :mode (("\\(rake\\|thor\\|guard\\|gem\\|cap\\|vagrant\\)file\\'" . ruby-mode)
           ("\\.\\(rb\\|ru\\|builder\\|rake\\|thor\\|gemspec\\)\\'" . ruby-mode))))

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
  (add-hook 'shell-mode-hook 'shell-comint-input-sender-hook)

  (defun eshell/clear ()
    "Clear contents in eshell."
    (interactive)
    (let ((inhibit-read-only t))
      (erase-buffer))))

(defun spacemacs/init-smartparens ()
  (use-package smartparens
    :defer t
    :init
    (progn
      (add-to-hooks (if dotspacemacs-smartparens-strict-mode
                        'smartparens-strict-mode
                      'smartparens-mode)
                    '(prog-mode-hook)))
    :config
    (progn
      (require 'smartparens-config)
      (spacemacs|diminish smartparens-mode " (Ⓢ)" " (S)")
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
      (setq scroll-margin 5
            scroll-conservatively 9999
            scroll-step 1)
    ;; deactivate the defadvice's
    (ad-disable-advice 'previous-line 'after 'smooth-scroll-down)
    (ad-activate 'previous-line)
    (ad-disable-advice 'next-line 'after 'smooth-scroll-up)
    (ad-activate 'next-line)
    (ad-disable-advice 'isearch-repeat 'after 'isearch-smooth-scroll)
    (ad-activate 'isearch-repeat)))

(defun spacemacs/init-string-edit ()
  (use-package string-edit
    :defer t
    :init
    (evil-leader/set-key "eds" 'string-edit-at-point)))

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

(defun spacemacs/init-vi-tilde-fringe ()
  (use-package vi-tilde-fringe
    :if window-system
    :init
    (global-vi-tilde-fringe-mode)
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
    :init
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
    :config (spacemacs|diminish whitespace-mode " Ⓦ" " W")))

(defun spacemacs/init-window-numbering ()
  (use-package window-numbering
    ;; not deferred on puprose
    :init
    (progn
      (when (configuration-layer/package-declaredp 'powerline)
        (defun window-numbering-install-mode-line (&optional position)
          "Do nothing, the display is handled by the powerline."))
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
         ((equal str "0")  " ⓿ "))))

    (defun spacemacs//window-numbering-assign ()
      "Custom number assignment for special buffers."
      (when (equal (buffer-name) " *NeoTree*") 0))
    (setq window-numbering-assign-func ''spacemacs//window-numbering-assign)
    ))

(defun spacemacs/init-yasnippet ()
  (use-package yasnippet
    :commands yas-global-mode
    :init
    (progn
      (defun spacemacs/load-yasnippet ()
          (if (not (boundp 'yas-minor-mode))
              (progn
                (let* ((dir (configuration-layer/get-layer-property 'spacemacs :ext-dir))
                       (yas-dir (list (concat dir "yasnippet-snippets"))))
                  (setq yas-snippet-dirs yas-dir)
                  (yas-global-mode 1)))))
      (add-to-hooks 'spacemacs/load-yasnippet '(prog-mode-hook
                                                org-mode-hook)))
    :config
    (progn
      (spacemacs|diminish yas-minor-mode " Ⓨ" " Y")
      (require 'helm-c-yasnippet)
      (evil-leader/set-key "is" 'helm-yas-complete)
      (setq helm-c-yas-space-match-any-greedy t))))

(defun spacemacs/init-zenburn-theme ()
  (use-package zenburn-theme
    :defer t))
