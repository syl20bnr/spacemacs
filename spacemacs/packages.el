(defvar spacemacs-packages
  '(
    ac-ispell
    ace-jump-mode
    ag
    anzu
    auto-complete
    auto-complete-clang
    auto-dictionary
    auto-highlight-symbol
    bookmark
    buffer-move
    cc-mode
    cmake-mode
    csharp-mode
    coffee-mode
    dash
    diminish
    dired+
    elisp-slime-nav
    ensime
    evil
    evil-args
    evil-escape
    evil-exchange
    evil-search-highlight-persist
    evil-jumper
    evil-leader
    evil-lisp-state
    evil-nerd-commenter
    evil-numbers
    evil-org
    evil-surround
    evil-terminal-cursor-changer
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
    fringe-helper
    git-gutter-fringe
    git-messenger
    git-timemachine
    ghc
    golden-ratio
    google-translate
    guide-key-tip
    helm
    helm-ag
    helm-css-scss
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
    hy-mode
    ido-vertical-mode
    js2-mode
    json-mode
    ledger-mode
    less-css-mode
    leuven-theme
    linum-relative
    key-chord
    magit
    magit-gitflow
    markdown-mode
    markdown-toc
    monokai-theme
    move-text
    multi-term
    neotree
    org
    org-bullets
    ;; annoying error message, disable it for now
    ;; org-trello
    p4
    page-break-lines
    paradox
    popup
    popwin
    powerline
    powershell
    powershell-mode
    projectile
    puppet-mode
    ;; not working well for now
    ;; rainbow-blocks
    rainbow-delimiters
    ;; install fail on windows
    ;; rainbow-mode
    rcirc
    rcirc-color
    recentf
    rfringe
    ruby-end
    ruby-mode
    ruby-test-mode
    s
    sbt-mode
    scala-mode2
    scss-mode
    smartparens
    smeargle
    smooth-scrolling
    string-edit
    subword
    tagedit
    tern-auto-complete
    undo-tree
    vi-tilde-fringe
    visual-regexp-steroids
    volatile-highlights
    wand
    web-mode
    wdired
    window-numbering
    yasnippet
    zenburn-theme
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar spacemacs-excluded-packages
  '(evil-plugins     ; crashed spacemacs on startup (will wait before reactivating it)
    fancy-narrow     ; too much bugs and bad side effects
    )
  "List of packages to exclude.")


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
      (eval-after-load 'evil-lisp-state
        '(progn
           (define-key evil-lisp-state-map "n" 'spacemacs/anzu-evil-search-next)
           (define-key evil-lisp-state-map "N" 'spacemacs/anzu-evil-search-previous)))

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
      (spacemacs|diminish auto-complete-mode " Ⓐ"))))

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

      (defun spacemacs/quick-ahs-forward ()
        "Go to the next occurrence of symbol under point with
`auto-highlight-symbol'"
        (interactive)
        (eval '(progn (ahs-highlight-now) (ahs-forward)) nil))

      (defun spacemacs/quick-ahs-backward ()
        "Go to the previous occurrence of symbol under point with
`auto-highlight-symbol'"
        (interactive)
        (eval '(progn (ahs-highlight-now) (ahs-backward)) nil))

      (eval-after-load 'evil
        '(progn
           (define-key evil-normal-state-map (kbd "*") 'spacemacs/quick-ahs-forward)
           (define-key evil-normal-state-map (kbd "#") 'spacemacs/quick-ahs-backward)
           (define-key evil-motion-state-map (kbd "*") 'spacemacs/quick-ahs-forward)
           (define-key evil-motion-state-map (kbd "#") 'spacemacs/quick-ahs-backward)
           (eval-after-load 'evil-lisp-state
             '(progn
                (define-key evil-normal-state-map (kbd "*") 'spacemacs/quick-ahs-forward)
                (define-key evil-normal-state-map (kbd "#") 'spacemacs/quick-ahs-backward)))))

      (evil-leader/set-key
        "se"  'ahs-edit-mode
        "sb"  'spacemacs/goto-last-searched-ahs-symbol
        "sh"  (lambda () (interactive)
                (eval '(progn
                         (ahs-highlight-now)
                         (setq spacemacs-last-ahs-highlight-p (ahs-highlight-p))
                         (spacemacs/auto-highlight-symbol-overlay-map)) nil))
        "sn"  (lambda () (interactive) (eval '(progn (ahs-highlight-now) (ahs-forward)) nil))
        "sN"  (lambda () (interactive) (eval '(progn (ahs-highlight-now) (ahs-backward)) nil))
        "srb" (lambda () (interactive) (eval '(ahs-change-range 'ahs-range-whole-buffer) nil))
        "srd" (lambda () (interactive) (eval '(ahs-change-range 'ahs-range-display) nil))
        "srf" (lambda () (interactive) (eval '(ahs-change-range 'ahs-range-beginning-of-defun) nil))
        "sR"  (lambda () (interactive) (eval '(ahs-change-range ahs-default-range) nil)))

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
           (define-key map (kbd "e") 'ahs-edit-mode)
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

(defun spacemacs/init-cc-mode ()
  (use-package cc-mode
    :defer t
    :config
    (progn
      (add-hook 'c-mode-hook '(lambda () (c-toggle-auto-state t)))
      (add-hook 'c++-mode-hook '(lambda () (c-toggle-auto-state t))))))

(defun spacemacs/init-cmake-mode ()
(use-package cmake-mode
  :defer t
  :init
  (setq auto-mode-alist
        (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                  ("\\.cmake\\'" . cmake-mode))
                auto-mode-alist))))

(defun spacemacs/init-coffee-mode ()
  (use-package coffee-mode
    :defer t
    :init
    (progn
      (defun spacemacs/coffee-indent ()
        (if (coffee-line-wants-indent)
            ;; We need to insert an additional tab because the last line was special.
            (coffee-insert-spaces (+ (coffee-previous-indent) coffee-tab-width))
          ;; otherwise keep at the same indentation level
          (coffee-insert-spaces (coffee-previous-indent)))
        )
      ;; indent to right position after `evil-open-blow' and `evil-open-above'
      (add-hook 'coffee-mode-hook '(lambda ()
                                     (setq indent-line-function 'spacemacs/coffee-indent
                                           evil-shift-width coffee-tab-width))))))

(defun spacemacs/init-csharp-mode ()
  (use-package csharp-mode
    :defer t))

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
    :config
    (add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))))

(defun spacemacs/init-ensime ()
  (use-package ensime
    :defer t))

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
      (defvar spacemacs-last-base-state 'normal
        "Last base state, the current value of this variable is used to
determine the state to enable when escaping from the insert state.")
      (make-variable-buffer-local 'spacemacs-last-base-state)
      (defadvice evil-normal-state (before spacemacs/evil-normal-state activate)
        "Advice to keep track of the last base state."
        (setq spacemacs-last-base-state 'normal))
      (defadvice evil-lisp-state (before spacemacs/evil-lisp-state activate)
        "Advice to keep track of the last base state."
        (setq spacemacs-last-base-state 'lisp))
      ;; manage the base state target when leaving the insert state
      (define-key evil-insert-state-map [escape]
        (lambda () (interactive)
          (let ((state (intern (format "evil-%s-state" spacemacs-last-base-state))))
            (funcall state))))

      ;; Make evil-mode up/down operate in screen lines instead of logical lines
      (define-key evil-normal-state-map "j" 'evil-next-visual-line)
      (define-key evil-normal-state-map "k" 'evil-previous-visual-line)
      ;; quick navigation
      (define-key evil-normal-state-map (kbd "L")
        (lambda () (interactive)
          (evil-window-bottom)
          (evil-scroll-line-to-center nil)))
      (define-key evil-normal-state-map (kbd "H")
        (lambda () (interactive)
          (evil-window-top)
          (evil-scroll-line-to-center nil)))
      (evil-leader/set-key "re" 'evil-show-registers)
      ;; define text objects
      (defmacro define-and-bind-text-object (key start-regex end-regex)
        (let ((inner-name (make-symbol "inner-name"))
              (outer-name (make-symbol "outer-name")))
          `(progn
             (evil-define-text-object ,inner-name (count &optional beg end type)
               (evil-regexp-range count beg end type ,start-regex ,end-regex t))
             (evil-define-text-object ,outer-name (count &optional beg end type)
               (evil-regexp-range count beg end type ,start-regex ,end-regex nil))
             (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
             (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))
      ;; between dollars sign:
      (define-and-bind-text-object "$" "\\$" "\\$")
      ;; between pipe characters:
      (define-and-bind-text-object "|" "|" "|")
      ;; between percent signs:
      (define-and-bind-text-object "%" "%" "%")
      )))

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
            evil-leader/leader "SPC"
            evil-leader/non-normal-prefix "s-")
      ;; give name to spacemacs prefixes
      (mapc (lambda (x) (spacemacs/declare-prefix (car x) (cdr x)))
           spacemacs/key-binding-prefixes)
      (global-evil-leader-mode))
    :config
    (progn
      ;; Unset shortcuts which shadow evil leader
      (eval-after-load "compile"
        '(progn
           (define-key compilation-mode-map (kbd "SPC") nil)
           (define-key compilation-mode-map (kbd "h") nil)))
      (eval-after-load "dired"
        '(define-key dired-mode-map (kbd "SPC") nil))
      ;; make leader available in visual mode
      (define-key evil-visual-state-map (kbd "SPC")
        evil-leader--default-map)
      (define-key evil-motion-state-map (kbd "SPC")
        evil-leader--default-map)
      ;; experimental: invoke leader with "jk" in insert mode
      (when dotspacemacs-feature-toggle-leader-on-jk
        (key-chord-define evil-insert-state-map (kbd "jk")
                          evil-leader--default-map)))))

(defun spacemacs/init-evil-lisp-state ()
  (use-package evil-lisp-state
    :init
    (evil-leader/set-key-for-mode 'emacs-lisp-mode "ml" 'evil-lisp-state)))

(defun spacemacs/init-evil-nerd-commenter ()
  (use-package evil-nerd-commenter
    :init
    (progn
      (evil-leader/set-key
        ";"  'evilnc-comment-operator
        "cl" 'evilnc-comment-or-uncomment-lines
        "ci" 'evilnc-toggle-invert-comment-line-by-line
        "cp" 'evilnc-comment-or-uncomment-paragraphs
        "cr" 'comment-or-uncomment-region
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
      ;; move the leader bindings to `m` prefix to be consistent with
      ;; the rest of spacemacs bindings
      (evil-leader/set-key-for-mode 'org-mode
        "t" nil "mt" 'org-show-todo-tree
        "a" nil "ma" 'org-agenda
        "c" nil "mA" 'org-archive-subtree
        "l" nil "ml" 'evil-org-open-links
        "o" nil "mC" 'evil-org-recompute-clocks)
      (spacemacs|diminish evil-org-mode " Ⓔ"))))

(defun spacemacs/init-evil-search-highlight-persist ()
  (use-package evil-search-highlight-persist
    :init
    (progn
      (global-evil-search-highlight-persist)
      (evil-leader/set-key "sc" 'evil-search-highlight-persist-remove-all)
      (evil-ex-define-cmd "noh" 'evil-search-highlight-persist-remove-all))))

(defun spacemacs/init-evil-surround ()
  (use-package evil-surround
    :init (global-evil-surround-mode 1)))

(defun spacemacs/init-evil-terminal-cursor-changer ()
  (unless (display-graphic-p)
    (require 'evil-terminal-cursor-changer)))

(defun spacemacs/init-evil-visualstar ()
  (use-package evil-visualstar))

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
          (fancy-battery-mode)))
      (setq-default fancy-battery-show-percentage t)
      (evil-leader/set-key "tmb" 'spacemacs/mode-line-battery-info-toggle))
    :config
    (progn
      ;; redefine this function for Spacemacs,
      ;; basically remove all faces and properties.
      (defun fancy-battery-default-mode-line ()
        "Assemble a mode line string for Fancy Battery Mode."
        (when fancy-battery-last-status
          (let* ((time (cdr (assq ?t fancy-battery-last-status)))
                 (percentage (cdr (assq ?p fancy-battery-last-status)))
                 (status (if (or fancy-battery-show-percentage
                                 (string= time "N/A"))
                             (and percentage (concat (concat percentage "%%")
                                                     " (" time ")"))
                           time)))
            (if status (concat " " status)
              ;; Battery status is not available
              "N/A"))))

      (defun fancy-battery-powerline-face ()
        "Return a face appropriate for powerline"
        (pcase (cdr (assq ?b fancy-battery-last-status))
                   ("!"  'fancy-battery-critical)
                   ("+"  ' fancy-battery-charging)
                   (_ 'fancy-battery-discharging))))
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
    :init
    (progn
      (dolist (mode '(c
                      coffee
                      js
                      json
                      ruby
                      scss
                      web))
        (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
                  'flycheck-mode)))
    :config
    (progn
      (spacemacs|diminish flycheck-mode " Ⓕ")

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

      ;; key bindings
      (evil-leader/set-key
        "fc" 'flycheck-clear
        "fl" 'flycheck-list-errors
        "fn" 'flycheck-next-error
        "fp" 'flycheck-previous-error))))

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
    (spacemacs|diminish flyspell-mode " Ⓢ")))

(defun spacemacs/init-git-gutter-fringe ()
  (use-package git-gutter-fringe
    :commands git-gutter-mode
    :init
    (add-to-hooks 'git-gutter-mode '(markdown-mode-hook
                                     org-mode-hook
                                     prog-mode-hook
                                     ))
    :config
    (progn
      (setq git-gutter:hide-gutter t)
      ;; Don't need log/message.
      (setq git-gutter:verbosity 0)
      (setq git-gutter-fr:side 'right-fringe)
      ;; (setq git-gutter:update-hooks '(after-save-hook after-revert-hook))
      ;; custom graphics that works nice with half-width fringes
      (fringe-helper-define 'git-gutter-fr:added nil
        "..X...."
        "..X...."
        "XXXXX.."
        "..X...."
        "..X...."
        )
      (fringe-helper-define 'git-gutter-fr:deleted nil
        "......."
        "......."
        "XXXXX.."
        "......."
        "......."
        )
      (fringe-helper-define 'git-gutter-fr:modified nil
        "..X...."
        ".XXX..."
        "XXXXX.."
        ".XXX..."
        "..X...."
        )
      (spacemacs|hide-lighter git-gutter-mode))))

(defun spacemacs/init-git-messenger ()
  (use-package git-messenger
    :defer t
    :init
    (evil-leader/set-key
      "gm" 'git-messenger:popup-message)))

(defun spacemacs/init-git-timemachine ()
  (use-package git-timemachine
    :defer t
    :init
    (evil-leader/set-key
      "gt" 'git-timemachine)))

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
                    '(evil-window-left
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

      (spacemacs|diminish golden-ratio-mode " ⊞"))))

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
      (evil-leader/set-key "tG" 'spacemacs/toggle-guide-key)
      (setq guide-key/guide-key-sequence '("C-x" "C-c" "SPC" "g" "z" "C-h")
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
      (spacemacs|diminish guide-key-mode " Ⓖ"))))

(defun spacemacs/init-helm ()
  (use-package helm
    :defer t
    :init
    (setq helm-split-window-in-side-p nil
          helm-bookmark-show-location t
          helm-buffers-fuzzy-matching t
          helm-always-two-windows     t)
    (evil-leader/set-key
        ":"   'helm-M-x
        "bs"  'helm-mini
        "sl"  'helm-semantic-or-imenu
        "hb"  'helm-bookmarks
        "hl"  'helm-resume
        "ry"  'helm-show-kill-ring
        "rr"  'helm-register
        "rm"  'helm-all-mark-rings
        "fh"  'helm-find-files
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
      (eval-after-load "helm-mode" ; required
        '(spacemacs|hide-lighter helm-mode)))))

(defun spacemacs/init-helm-ag ()
  (use-package helm-ag
    :defer t))

(defun spacemacs/init-helm-css-scss ()
  (use-package helm-css-scss
    :defer t
    :init
    (eval-after-load 'scss-mode
      '(evil-leader/set-key-for-mode 'scss-mode "mh" 'helm-css-scss))))

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
      "hm"    'helm-disable-minor-mode
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
      (spacemacs|diminish hl-paren-mode "(Ⓗ)")
      (spacemacs|hide-lighter hl-highlight-mode))))

(defun spacemacs/init-hy-mode ()
  (use-package hy-mode
    :defer t))

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
        (define-key ido-completion-map (kbd "C-h") 'ido-delete-backward-updir)
        (define-key ido-completion-map (kbd "C-j") 'ido-next-match)
        (define-key ido-completion-map (kbd "C-l") 'ido-exit-minibuffer)
        (define-key ido-completion-map (kbd "C-S-j") 'ido-next-match-dir)
        (define-key ido-completion-map (kbd "C-S-k") 'ido-prev-match-dir)
        ;; history navigation
        (define-key ido-completion-map (kbd "C-n") 'next-history-element)
        (define-key ido-completion-map (kbd "C-p") 'previous-history-element)
        ;; ido-other window maps
        (define-key ido-completion-map (kbd "C-x") 'ido-invoke-in-other-window)
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

(defun spacemacs/init-js2-mode ()
  (use-package js2-mode
    :commands (js2-minor-mode)
    :init
    (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))))

(defun spacemacs/init-json-mode ()
  (use-package json-mode
    :defer t))

(defun spacemacs/init-ledger-mode ()
  (use-package ledger-mode
    :mode ("\\.\\(ledger\\|ldg\\)\\'" . ledger-mode)
    :defer t
    :init
    (progn
      (setq ledger-post-amount-alignment-column 62)
      (evil-leader/set-key-for-mode 'ledger-mode
        "md" 'ledger-delete-current-transaction
        "ma" 'ledger-add-transaction))))

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

(defun spacemacs/init-magit ()
  (use-package magit
    :defer t
    :init
    (evil-leader/set-key "gs" 'magit-status)
    :config
    (progn
      (spacemacs|hide-lighter magit-auto-revert-mode)
      ;; full screen magit-status
      (defadvice magit-status (around magit-fullscreen activate)
        (window-configuration-to-register :magit-fullscreen)
        ad-do-it
        (delete-other-windows))

      ;; hjkl key bindings
      (spacemacs|evilify magit-commit-mode-map
        "C-v" 'magit-revert-item)
      (spacemacs|evilify magit-log-mode-map
        "C-v" 'magit-revert-item)
      (spacemacs|evilify magit-process-mode-map
        "C-v" 'magit-revert-item)
      (spacemacs|evilify magit-branch-manager-mode-map
        "K" 'magit-discard-item
        "L" 'magit-key-mode-popup-logging
        "C-v" 'magit-revert-item)
      (spacemacs|evilify magit-status-mode-map
        "K" 'magit-discard-item
        "L" 'magit-key-mode-popup-logging
        "H" 'magit-key-mode-popup-diff-options
        "C-v" 'magit-revert-item)
      ;; remove conflicts with evil leader
      (spacemacs/activate-evil-leader-for-maps '(magit-mode-map
                                                 magit-commit-mode-map
                                                 magit-diff-mode-map))


      (defun magit-quit-session ()
        "Restores the previous window configuration and kills the magit buffer"
        (interactive)
        (kill-buffer)
        (jump-to-register :magit-fullscreen))
      (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

      (defun magit-toggle-whitespace ()
        (interactive)
        (if (member "-w" magit-diff-options)
            (magit-dont-ignore-whitespace)
          (magit-ignore-whitespace)))

      (defun magit-ignore-whitespace ()
        (interactive)
        (add-to-list 'magit-diff-options "-w")
        (magit-refresh))

      (defun magit-dont-ignore-whitespace ()
        (interactive)
        (setq magit-diff-options (remove "-w" magit-diff-options))
        (magit-refresh))
      (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace))))

(defun spacemacs/init-magit-gitflow ()
  (use-package magit-gitflow
    :commands turn-on-magit-gitflow
    :init (add-hook 'magit-mode-hook 'turn-on-magit-gitflow)
    :config (spacemacs|diminish magit-gitflow-mode "Flow")))

(defun spacemacs/init-markdown-mode ()
  (use-package markdown-mode
    :mode ("\\.md" . markdown-mode)
    :defer t))

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
      (setq neo-create-file-auto-open t
            neo-dont-be-alone t
            neo-banner-message "File Tree browser"
            neo-smart-open t
            neo-persist-show nil)
      (evil-leader/set-key "ft" 'neotree-toggle))
    :config
    (add-hook 'neotree-mode-hook
              (lambda ()
                (define-key evil-motion-state-local-map (kbd "TAB") 'neotree-enter)
                (define-key evil-motion-state-local-map (kbd "RET") 'neotree-enter)
                (define-key evil-motion-state-local-map (kbd "?") 'evil-search-backward)
                (define-key evil-motion-state-local-map (kbd "a") 'neotree-stretch-toggle)
                (define-key evil-motion-state-local-map (kbd "c") 'neotree-create-node)
                (define-key evil-motion-state-local-map (kbd "d") 'neotree-delete-node)
                (define-key evil-motion-state-local-map (kbd "g") 'neotree-refresh)
                (define-key evil-motion-state-local-map (kbd "H") 'neotree-hidden-file-toggle)
                (define-key evil-motion-state-local-map (kbd "K") 'kill-this-buffer)
                (define-key evil-motion-state-local-map (kbd "q") 'neotree-hide)
                (define-key evil-motion-state-local-map (kbd "r") 'neotree-rename-node)
                ))
    ))

(defun spacemacs/init-org ()
  (use-package org
    :mode ("\\.org$" . org-mode)
    :defer t
    :init
    (progn
      (setq org-log-done t)
      (add-hook 'org-mode-hook 'org-indent-mode))
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

(defun spacemacs/init-p4 ()
  (use-package p4
    :commands (p4-add
               p4-delete
               p4-describe
               p4-edit
               p4-revert)
    :init
    (evil-leader/set-key
      "p4a" 'p4-add
      "p4d" 'p4-delete
      "p4D" 'p4-describe
      "p4e" 'p4-edit
      "p4R" 'p4-revert
      "p4r" 'p4-rename
      "p4S" 'p4-submit)))

(defun spacemacs/init-page-break-lines ()
  (use-package page-break-lines
    :init
    (global-page-break-lines-mode t)
    (spacemacs|hide-lighter page-break-lines-mode)))

(defun spacemacs/init-paradox ()
  (use-package paradox
    :defer t
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
      (use-package window-numbering
        :ensure window-numbering
        :init
        (progn
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
          (window-numbering-mode 1)))

      (defun spacemacs/window-number ()
        "Return the number of the window."
        (let ((num (window-numbering-get-number-string)))
          (cond ((not (display-graphic-p)) (concat "(" num ")"))
                ((equal num "1")  " ➊ ")
                ((equal num "2")  " ➋ ")
                ((equal num "3")  " ➌ ")
                ((equal num "4")  " ➍ ")
                ((equal num "5")  " ➎ ")
                ((equal num "6")  " ❻ ")
                ((equal num "7")  " ➐ ")
                ((equal num "8")  " ➑ ")
                ((equal num "9")  " ➒ ")
                ((equal num "0")  " ➓ ")
                (t (concat " (" num ") ")))))

      (defvar spacemacs-mode-line-minor-modesp t
        "If not nil, minor modes lighter are displayed in the mode-line.")
      (defun spacemacs/mode-line-minor-modes-toggle ()
        "Toggle display of minor modes."
        (interactive)
        (if spacemacs-mode-line-minor-modesp
            (setq spacemacs-mode-line-minor-modesp nil)
          (setq spacemacs-mode-line-minor-modesp t)))
      (evil-leader/set-key "tmm" 'spacemacs/mode-line-minor-modes-toggle)

      ;; for now we hardcode the height value of powerline depending on the
      ;; window system, a better solution would be to compute it correctly
      ;; in powerline package.
      (let ((height (if (eq 'w32 window-system) 18 17)))
        (setq-default powerline-height height))
      (setq-default powerline-default-separator 'wave)

      (setq-default mode-line-format '("%e" (:eval
          (let* ((active (powerline-selected-window-active))
                 (line-face (if active 'mode-line 'mode-line-inactive))
                 (face1 (if active 'powerline-active1 'powerline-inactive1))
                 (face2 (if active 'powerline-active2 'powerline-inactive2))
                 (state-face (if active (spacemacs/current-state-face) face2))
                 (batteryp (and (boundp 'fancy-battery-mode)
                                (symbol-value fancy-battery-mode)))
                 (battery-face (if batteryp (fancy-battery-powerline-face)))
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
                                                  (cdr powerline-default-separator-dir))))
                 (lhs (append
                       (list
                        ;; window number
                        ;; (funcall separator-left state-face face1)
                        (powerline-raw (spacemacs/window-number) state-face))
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
                                           'spacemacs-mode-line-flycheck-info-face)
                            ))
                       ;; separator between flycheck and minor modes
                       (if (and active flycheckp spacemacs-mode-line-minor-modesp)
                           (list
                            (funcall separator-left line-face face1)
                            (powerline-raw "  " face1)
                            (funcall separator-right face1 line-face)))
                       ;; minor modes
                       (if (and active spacemacs-mode-line-minor-modesp)
                           (list
                            (powerline-minor-modes line-face 'l)
                            (powerline-raw mode-line-process line-face 'l)
                            (powerline-raw " " line-face)))
                       ;; version control
                       (if (and active (or flycheckp spacemacs-mode-line-minor-modesp))
                           (list (funcall separator-left (if vc-face line-face face1) vc-face)))
                       (if active (list (powerline-vc vc-face)
                                        (powerline-raw " " vc-face)
                                        (funcall separator-right vc-face face2))
                         (list (funcall separator-right face1 face2)))))
                 (rhs (append
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
                        (powerline-raw " " line-face)
                        ;; percentage in the file
                        (powerline-raw "%p" line-face 'r)
                        (when active
                          ;; display hud only if necessary
                          (powerline-chamfer-left line-face face1)
                          (let ((progress (format-mode-line "%p")))
                            (if (string-match "\%" progress)
                                (powerline-hud state-face face1))))))))
            (concat (powerline-render lhs)
                    (powerline-fill face2 (powerline-width rhs))
                    (powerline-render rhs))))))
      )))

(defun spacemacs/init-powershell ()
  (use-package powershell
    :commands powershell))

(defun spacemacs/init-powershell-mode ()
  (use-package powershell-mode
    :defer t
    :mode ("\\.ps1\\'" . powershell-mode)))

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
      (let ((dir (config-system/get-layer-property 'spacemacs :ext-dir)))
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

(defun spacemacs/init-scss-mode ()
  (use-package scss-mode
    :defer t
    :mode ("\\.scss\\'" . scss-mode)))

(defun spacemacs/init-smartparens ()
  (use-package smartparens
    :defer t
    :init
    (progn
      (add-to-hooks 'smartparens-mode '(markdown-mode-hook
                                        prog-mode-hook)))
    :config
    (progn
      (require 'smartparens-config)
      (spacemacs|diminish smartparens-mode " (Ⓢ)")
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
               '(:add (spacemacs/smartparens-pair-newline-and-indent "RET")))

      ;; Don't do terrible things with Github code blocks (```)
      (sp-local-pair 'markdown-mode "`" nil :actions '(:rem autoskip))
      (sp-local-pair 'markdown-mode "'" nil :actions nil))))

(defun spacemacs/init-smeargle ()
  (use-package smeargle
    :defer t
    :init
    (evil-leader/set-key
      "gcC" 'smeargle-clear
      "gcc" 'smeargle-commits
      "gct" 'smeargle)))


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

(defun spacemacs/init-tagedit ()
  (use-package tagedit
    :defer t
    :config
    (progn
      (tagedit-add-experimental-features)
      (add-hook 'html-mode-hook (lambda () (tagedit-mode 1)))
      (spacemacs|diminish tagedit-mode " Ⓣ"))))

(defun spacemacs/init-tern-auto-complete ()
  (use-package tern-auto-complete
    :defer t
    :init
    (add-hook 'js2-mode-hook (lambda () (tern-mode t)))
    :config
    (tern-ac-setup)))

(defun spacemacs/init-undo-tree ()
  (use-package undo-tree
    :idle (global-undo-tree-mode)
    :defer t
    :init
    (setq undo-tree-history-directory-alist
          `(("." . ,(concat spacemacs-cache-directory "undo"))))
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)
    :config
    (spacemacs|hide-lighter undo-tree-mode)))

(defun spacemacs/init-vi-tilde-fringe ()
  (use-package vi-tilde-fringe
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

(defun spacemacs/init-web-mode ()
  (use-package web-mode
    :defer t
    :mode (("\\.phtml\\'"     . web-mode)
           ("\\.tpl\\.php\\'" . web-mode)
           ("\\.html\\'"      . web-mode)
           ("\\.htm\\'"       . web-mode)
           ("\\.[gj]sp\\'"    . web-mode)
           ("\\.as[cp]x\\'"   . web-mode)
           ("\\.erb\\'"       . web-mode)
           ("\\.mustache\\'"  . web-mode)
           ("\\.djhtml\\'"    . web-mode))))

(defun spacemacs/init-yasnippet ()
  (use-package yasnippet
    :commands yas-global-mode
    :init
    (progn
      (defun spacemacs/load-yasnippet ()
          (if (not (boundp 'yas-minor-mode))
              (progn
                (let* ((dir (config-system/get-layer-property 'spacemacs :ext-dir))
                       (yas-dir (list (concat dir "yasnippet-snippets"))))
                  (setq yas-snippet-dirs yas-dir)
                  (yas-global-mode 1)))))
      (add-to-hooks 'spacemacs/load-yasnippet '(prog-mode-hook
                                                org-mode-hook)))
    :config
    (progn
      (spacemacs|diminish yas-minor-mode " Ⓨ")
      (require 'helm-c-yasnippet)
      (evil-leader/set-key "is" 'helm-yas-complete)
      (setq helm-c-yas-space-match-any-greedy t))))

(defun spacemacs/init-zenburn-theme ()
  (use-package zenburn-theme
    :defer t))
