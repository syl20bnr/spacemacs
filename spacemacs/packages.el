;; installed packages as well as build-in packages with a corresponding
;; init-xxx file
(defvar spacemacs-packages
  '(
    ;; must be initialized first
    evil
    evil-exchange
    evil-leader
    evil-visualstar
    window-numbering
    powerline
    ;; the rest is in alphabetical order
    ac-ispell
    ac-js2
    ace-jump-mode
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
    deferred
    diminish
    dired+
    edts
    elisp-slime-nav
    elixir-mix
    elixir-mode
    epc
    erlang
    ess
    ess-R-data-view
    ess-R-object-popup
    ess-smart-underscore
    exec-path-from-shell
    expand-region
    fill-column-indicator
    fish-mode
    flx-ido
    flycheck
    flycheck-color-mode-line
    flycheck-ledger
    flyspell
    fringe-helper
    gist
    git-gutter-fringe
    git-messenger
    git-timemachine
    ghc
    golden-ratio
    google-translate
    haskell-mode
    helm
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
    hy-mode
    jedi
    js2-mode
    js2-refactor
    json-mode
    ledger-mode
    less-css-mode
    magit
    markdown-mode
    monokai-theme
    move-text
    multi-term
    org
    org-bullets
    ;; annoying error message, disable it for now
    ;; org-trello
    p4
    page-break-lines
    paredit
    popup
    popwin
    powershell
    powershell-mode
    projectile
    puppet-mode
    python
    ;; not working well for now
    ;; rainbow-blocks
    rainbow-delimiters
    rainbow-identifiers
    rainbow-mode
    rcirc
    rcirc-color
    recentf
    rfringe
    ruby-end
    ruby-mode
    ruby-test-mode
    s
    scss-mode
    smartparens
    smeargle
    string-edit
    subword
    surround
    tagedit
    visual-regexp-steroids
    volatile-highlights
    wand
    weather
    web-mode
    wdired
    yasnippet
    zenburn-theme
    )
  "List of all packages to install and/or initialized. Built-in packages
which require an initialization must be listed explicitly in the list."
)

;; Initialization of packages

(defun spacemacs/init-evil ()
  ;; relative line number for operator state
  ;; inspired by https://github.com/cofi/dotfiles/blob/master/emacs.d/config/cofi-evil.el
  (defvar cofi/current-line 0
    "Stores the current line before linum numbers the lines.")
  (defadvice linum-update (before set-current-line activate)
    (setq cofi/current-line (line-number-at-pos)))
  (defun cofi/relative-line (line-number)
    (let ((relative (abs (- line-number cofi/current-line))))
      (propertize (format "%2d" relative) 'face (if (= relative 0)
                                                    'linum-current-line
                                                  'linum))))
  (defun cofi/evil-toggle-relative-lines ()
    (interactive)
    (if (eq linum-format #'cofi/relative-line)
        (progn
          (linum-mode -1)
          (setq linum-format #'cofi/linum-dynamic-lines))
      (progn
        (linum-mode t)
        (setq linum-format #'cofi/relative-line)))
    (linum-update-current))
  (defun cofi/linum-dynamic-lines (line-number)
    (let ((width (ceiling (log (count-lines (point-min) (point-max)) 10))))
      (propertize (format (format "%%%dd" width) line-number)
                  'face (if (= cofi/current-line line-number)
                            'linum-current-line
                          'linum))))
  (setq linum-format #'cofi/linum-dynamic-lines)

  ;; evil mode init ------------------------------------------------------------

  (use-package evil
    :init
    (progn
      (setq evil-mode-line-format 'before)
      (setq evil-emacs-state-cursor  '("red" box))
      (setq evil-normal-state-cursor '("orange" box))
      (setq evil-visual-state-cursor '("black" box))
      (setq evil-insert-state-cursor '("green3" box))
      (setq evil-motion-state-cursor '("purple" box))
      (add-to-hooks #'cofi/evil-toggle-relative-lines
                    '(evil-operator-state-entry-hook
                      evil-operator-state-exit-hook))
      ;; I prefer to stay on the original character when leaving insert mode
      ;; (initiated with 'i').
      (setq evil-move-cursor-back nil)
      (evil-mode 1)
      ;; replace motion state by normal state and add some other modes
      (setq evil-normal-state-modes (append '(rcirc-mode) evil-motion-state-modes)))
    :config
    (progn
      ;; inspired from:
      ;; https://github.com/roman/emacs.d/blob/master/zoo/zoo-evil.el
      (evil-define-command fd-trigger (callback)
        "Allows to execute the passed function using 'fd'."
        :repeat change
        (let ((modified (buffer-modified-p)))
          (insert "f")
          (let ((evt (read-event
                      (format "Insert %c to exit insert state" ?d)
                      nil 0.1)))
            (cond
             ((null evt)
              (message ""))
             ((and (integerp evt)
                   (char-equal evt ?d))
              ;; remove the f character
              (delete-char -1)
              (set-buffer-modified-p modified)
              (funcall callback))
             (t                         ; otherwise
              (setq unread-command-events (append unread-command-events
                                                  (list evt))))))))
      ;; load evil-leader
      (use-package evil-leader
        :init
        (progn
          (setq evil-leader/in-all-states t
                evil-leader/leader "SPC"
                evil-leader/non-normal-prefix "s-")
          (global-evil-leader-mode))
        :config
        (progn
          ;; Unset shortcuts which shadow evil leader
          (eval-after-load "compile"
            '(define-key compilation-mode-map (kbd "SPC") nil))
          (eval-after-load "dired"
            '(define-key dired-mode-map (kbd "SPC") nil))
          ;; make leader available in visual mode
          (define-key evil-visual-state-map (kbd "SPC") evil-leader--default-map)
          (define-key evil-motion-state-map (kbd "SPC") evil-leader--default-map)
          (define-key evil-emacs-state-map  (kbd "SPC") evil-leader--default-map)))
      ;; load surround
      (use-package surround
        :init (global-surround-mode 1)) 
      ;; load evil-exchange
      (use-package evil-exchange
        :init (evil-exchange-install))
      ;; initiate a search of the selected text
      (use-package evil-visualstar))))

(defun spacemacs/init-window-numbering ()
  (use-package window-numbering
    :init (window-numbering-mode 1)))

(defun spacemacs/init-powerline ()
  (use-package powerline
    :init
    (progn
      (evil-leader/set-key "tm" 'powerline-minor-modes-toggle)
      ;; Setup modeline items
      (defun gcs-propertized-evil-mode-tag ()
        (propertize evil-mode-line-tag 'font-lock-face
                    ;; Don't propertize if we're not in the selected buffer
                    (cond ((not (eq (current-buffer) (car (buffer-list)))) '())
                          ((evil-insert-state-p) '(:background "green3" :foreground "black"))
                          ((evil-emacs-state-p)  '(:background "red" :foreground "black"))
                          ((evil-motion-state-p) '(:background "purple" :foreground "black"))
                          ((evil-visual-state-p) '(:background "gray" :foreground "black"))
                          ((evil-normal-state-p)  '(:background "orange" :foreground "black"))
                          (t '()))))

      (defpowerline powerline-window-number 
        (let ((num (window-numbering-get-number-string)))
          (cond ((not (display-graphic-p)) (concat "(" num ")"))
                ((equal num "1")  "➊")
                ((equal num "2")  "➋")
                ((equal num "3")  "➌")
                ((equal num "4")  "➍")
                ((equal num "5")  "➎")
                ((equal num "6")  "❻")
                ((equal num "7")  "➐")
                ((equal num "8")  "➑")
                ((equal num "9")  "➒")
                ((equal num "0")  "➓")
                (t (concat "(" num ")")))))

      (defpowerline powerline-evil-mode
        (gcs-propertized-evil-mode-tag))

      (defvar powerline-minor-modesp nil)
      (defun powerline-minor-modes-toggle ()
        "Toggle display of minor modes."
        (interactive)
        (if powerline-minor-modesp
            (setq powerline-minor-modesp nil)
          (setq powerline-minor-modesp t)))

      (setq-default mode-line-format
                    '("%e"
                      (:eval
                       (let* ((active (eq (frame-selected-window) (selected-window)))
                              (face1 (if active 'powerline-active1 'powerline-inactive1))
                              (face2 (if active 'powerline-active2 'powerline-inactive2))
                              (lhs (append (list
                                            (powerline-window-number face1 'l)
                                            (powerline-evil-mode face1 'l)

                                            (powerline-raw "%*" nil 'l)
                                            (powerline-buffer-size nil 'l)
                                            (powerline-buffer-id nil 'l)
                                            (powerline-raw " " nil)

                                            (powerline-arrow-right nil face1)
                                            (powerline-major-mode face1 'l)
                                            (powerline-raw " " face1))

                                           (if powerline-minor-modesp
                                               (list (powerline-arrow-right face1 nil)
                                                     (powerline-minor-modes nil 'l)
                                                     (powerline-raw mode-line-process nil 'l)
                                                     (powerline-raw " " nil)
                                                     (powerline-arrow-right nil face2))
                                             (list (powerline-raw " " face1)
                                                   (powerline-arrow-right face1 face2)))

                                           (list (powerline-vc face2))))
                              (rhs (list
                                    (powerline-raw global-mode-string face2 'r)
                                    (powerline-raw " " face2)

                                    (powerline-arrow-left face2 face1)
                                    (powerline-raw " " face1)
                                    (powerline-raw "%l:%2c" face1 'r)
                                    (powerline-arrow-left face1 nil)
                                    (powerline-raw " " nil)
                                    (powerline-raw "%p" nil 'r)

                                    (powerline-hud face2 face1))))
                         (concat
                          (powerline-render lhs)
                          (powerline-fill face2 (powerline-width rhs))
                          (powerline-render rhs))))))
      )))

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
      (define-key evil-normal-state-map "," 'ace-jump-mode)
      (define-key evil-normal-state-map (kbd "C-,") 'ace-jump-word-mode))
    :config
    (progn 
      ;; ace-jump quick access
      (evil-leader/set-key "," 'ace-jump-mode-pop-mark))))

(defun spacemacs/init-auto-complete ()
  (use-package auto-complete
    :commands auto-complete-mode
    :init
    (progn 
      (add-to-hooks 'auto-complete-mode '(org-mode-hook
                                          prog-mode-hook
                                          erlang-mode-hook))
      (evil-leader/set-key "ta" 'auto-complete-mode))
    :config
    (progn
      (require 'auto-complete-config)
      (ac-config-default)
      (add-to-list 'completion-styles 'initials t)
      ;; customization
      (setq ac-auto-start 2
            ac-delay 0.
            ac-quick-help-delay 1.
            ac-use-fuzzy t
            ac-fuzzy-enable t
            tab-always-indent 'complete ; use 'complete when auto-complete is disabled
            ac-dwim t))))

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
        "sd" 'adict-change-dictionary))))

(defun spacemacs/init-auto-highlight-symbol ()
  (use-package auto-highlight-symbol
    :commands auto-highlight-symbol-mode
    :init
    (add-to-hooks
     'auto-highlight-symbol-mode '(erlang-mode-hook
                                   prog-mode-hook
                                   org-mode-hook
                                   markdown-mode-hook
                                   ))
    :config
    (progn 
      (custom-set-variables
       '(ahs-case-fold-search nil)
       '(ahs-default-range (quote ahs-range-whole-buffer))
       '(ahs-idle-interval 0.5))
      (evil-leader/set-key
        "he" 'ahs-edit-mode
        "hn" 'ahs-forward
        "hp" 'ahs-backward
        "th" 'auto-highlight-symbol-mode))))

(defun spacemacs/init-bookmark ()
  (use-package bookmark
    :commands (bookmark-delete
               bookmark-jump
               bookmark-rename
               bookmark-set)
    :config
    (setq 
     bookmark-default-file "~/.emacs.d/bookmarks" ; keep my ~/ clean
     bookmark-save-flag 1)              ; autosave each change
    ))

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

(defun spacemacs/init-csharp-mode ()
  (use-package csharp-mode
    :defer t))

(defun spacemacs/init-diminish ()
  (require 'diminish)

  ;; Major modes abbrev --------------------------------------------------------

  (add-hook 'emacs-lisp-mode-hook
            (lambda () (setq mode-name "Elisp")))

  (add-hook 'erlang-mode-hook
            (lambda () (setq mode-name "Erlang")))

  (add-hook 'python-mode-hook
            (lambda () (setq mode-name "Python")))

  ;; Minor modes abbrev --------------------------------------------------------

  (when (display-graphic-p)
    (eval-after-load "auto-complete"
      '(diminish 'auto-complete-mode " Ⓐ"))
    (eval-after-load "auto-highlight-symbol"
      '(diminish 'auto-highlight-symbol-mode " Ⓗ"))
    (eval-after-load "centered-cursor-mode"
      '(diminish 'centered-cursor-mode " Ⓒ"))
    (eval-after-load "eproject"
      '(diminish 'eproject-mode " eⓅ"))
    (eval-after-load "flymake"
      '(diminish 'flymake-mode " Ⓕ"))
    (eval-after-load "projectile"
      '(diminish 'projectile-mode " Ⓟ"))
    (eval-after-load "flyspell"
      '(diminish 'flyspell-mode " Ⓢ"))
    (eval-after-load "smartparens"
      '(diminish 'smartparens-mode " (Ⓢ)"))
    (eval-after-load "paredit"
      '(diminish 'paredit-mode " (Ⓟ)"))
    (eval-after-load "tagedit"
      '(diminish 'tagedit-mode " Ⓣ"))
    (eval-after-load "yasnippet"
      '(diminish 'yas-minor-mode " Ⓨ"))
    )

  ;; Minor Mode (hidden) ------------------------------------------------------

  (eval-after-load 'elisp-slime-nav
    '(diminish 'elisp-slime-nav-mode))

  (eval-after-load "hi-lock"
    '(diminish 'hi-lock-mode))

  (eval-after-load "page-break-lines"
    '(diminish 'page-break-lines-mode))

  (eval-after-load "rainbow-mode"
    '(diminish 'rainbow-mode))

  (eval-after-load "ruby-end"
    '(diminish 'ruby-end-mode))

  (eval-after-load "undo-tree"
    '(diminish 'undo-tree-mode))

  (eval-after-load "helm-mode"
    '(diminish 'helm-mode))

  (eval-after-load "golden-ratio"
    '(diminish 'golden-ratio-mode))

  (eval-after-load "git-gutter"
    '(diminish 'git-gutter-mode))

  (eval-after-load "abbrev"
    '(diminish 'abbrev-mode))

  (eval-after-load "volatile-highlights"
    '(diminish 'volatile-highlights-mode)))

(defun spacemacs/init-dired+ ()
  (use-package dired+
    :defer t))

(defun spacemacs/init-elisp-slime-nav ()
  ;; Elisp go-to-definition with M-. and back again with M-,
  (use-package elisp-slime-nav
    :defer t
    :config
    (add-hook 'emacs-lisp-mode-hook (lambda () (elisp-slime-nav-mode t)))))

(defun spacemacs/init-elixir-mix ()
  (use-package elixir-mix
    :defer t
    :init
    (global-elixir-mix-mode)))

(defun spacemacs/init-elixir-mode ()
  (use-package elixir-mode
    :defer t
    :config
    (progn
      (require 'ruby-end)
      (add-to-list 'elixir-mode-hook
                   (defun auto-activate-ruby-end-mode-for-elixir-mode ()
                     (set (make-variable-buffer-local 'ruby-end-expand-keywords-before-re)
                          "\\(?:^\\|\\s-+\\)\\(?:do\\)")
                     (set (make-variable-buffer-local 'ruby-end-check-statement-modifiers) nil)
                     (ruby-end-mode +1))))))

(defun spacemacs/init-erlang ()
  (use-package erlang
    :mode (("\\.erl?$" . erlang-mode)
           ("\\.hrl?$" . erlang-mode)
           ("\\.spec?$" . erlang-mode))
    :config
    (progn
      (setq erlang-root-dir "/usr/lib/erlang/erts-5.10.3")
      (add-to-list 'exec-path "/usr/lib/erlang/erts-5.10.3/bin")
      (setq erlang-man-root-dir "/usr/lib/erlang/erts-5.10.3/man")
      (setq erlang-compile-extra-opts '(debug_info))
      (require 'erlang-start)
      (add-hook 'erlang-mode-hook
                (lambda ()
                  ;; when starting an Erlang shell in Emacs, with a custom node name
                  (setq inferior-erlang-machine-options '("-sname" "syl20bnr"))
                  ))
      (require 'edts-start)
      ;; (setq edts-log-level 'debug)
      ;; (setq edts-face-inhibit-mode-line-updates t)
      (evil-leader/set-key-for-mode 'erlang-mode
        "md" 'edts-find-doc
        "me" 'edts-code-next-issue
        "mG" 'edts-find-global-function
        "mg" 'edts-find-source-under-point
        "mh" 'edts-find-header-source
        "ml" 'edts-find-local-function
        "mm" 'edts-find-macro-source
        "mr" 'edts-find-record-source)))

  ;; not needed using EDTS
  ;; (require 'erlang-flymake)
  ;; (erlang-flymake-only-on-save)
)
  
(defun spacemacs/init-ess ()
  ;; ESS is not quick to load so we just load it when
  ;; we need it (see my-keybindings.el for the associated
  ;; keybinding)
  (defun load-ess-on-demand ()
    (interactive)
    (use-package ess-site)
    (use-package ess-smart-underscore)
    (use-package ess-R-object-popup)
    (use-package ess-R-data-view)
    )
  (evil-leader/set-key "le" 'load-ess-on-demand)

  ;; R --------------------------------------------------------------------------
  (eval-after-load "ess-site"
    '(progn
       (evil-leader/set-key-for-mode 'ess-mode
         "mB" 'ess-eval-buffer-and-go
         "mb" 'ess-eval-buffer
         "mF" 'ess-eval-function-and-go
         "mf" 'ess-eval-function
         "mi" 'R
         "mL" 'ess-eval-line-and-go
         "ml" 'ess-eval-line
         "mp" 'ess-R-object-popup
         "mR" 'ess-eval-region-and-go
         "mr" 'ess-eval-region
         "mS" 'ess-eval-function-or-paragraph-and-step
         "ms" 'ess-eval-region-or-line-and-step
         "mvp" 'ess-R-dv-pprint
         "mvt" 'ess-R-dv-ctable
         )
       (define-key inferior-ess-mode-map (kbd "C-j") 'comint-next-input)
       (define-key inferior-ess-mode-map (kbd "C-k") 'comint-previous-input))))

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
    :init (flx-ido-mode 1)
    :config
    ;; disable ido faces to see flx highlights.
    ;; (setq ido-use-faces nil)
    ))

(defun spacemacs/init-flycheck ()
  (use-package flycheck
    :defer t
    :init
    (progn
      (dolist (mode '(c
                      coffee
                      elixir
                      js
                      json
                      python
                      ruby
                      scss
                      web))
        (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
                  'flycheck-mode))
      (use-package flycheck-color-mode-line
        :defer t
        :init
        (add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode)))
    :config
    (progn
      (setq flycheck-check-syntax-automatically '(save mode-enabled))
      (setq flycheck-standard-error-navigation nil)
      ;; Custom fringe indicator
      (when (fboundp 'define-fringe-bitmap)
        (define-fringe-bitmap 'my-flycheck-fringe-indicator
          (vector #b00000000
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
      (add-hook 'markdown-mode-hook '(lambda () (flyspell-mode 1)))
      (add-hook 'text-mode-hook '(lambda () (flyspell-mode 1))))))

(defun spacemacs/init-gist ()
  (use-package gist
    :defer t))

(defun spacemacs/init-git-gutter-fringe ()
  (use-package git-gutter-fringe
    :commands git-gutter-mode
    :init
    (add-to-hooks 'git-gutter-mode '(erlang-mode-hook
                                     markdown-mode-hook
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
        ))))

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
    :init
    (golden-ratio-mode)
    :config
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
                    ess-eval-line-and-go)))))

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

(defun spacemacs/init-haskell-mode ()
  (use-package haskell-mode
    :commands haskell-mode
    :config
    (progn
      ;; Customization
      (custom-set-variables
       ;; Use cabal-dev for the GHCi session. Ensures our dependencies are in scope.
       '(haskell-process-type 'cabal-dev)

       ;; Use notify.el (if you have it installed) at the end of running
       ;; Cabal commands or generally things worth notifying.
       '(haskell-notify-p t)

       ;; To enable tags generation on save.
       '(haskell-tags-on-save t)

       ;; To enable stylish on save.
       '(haskell-stylish-on-save t))

      (add-hook 'haskell-mode-hook 'haskell-hook)
      (add-hook 'haskell-cabal-mode-hook 'haskell-cabal-hook)

      ;; Haskell main editing mode key bindings.
      (defun haskell-hook ()
        ;; Use simple indentation.
        (turn-on-haskell-simple-indent)
        (define-key haskell-mode-map (kbd "<return>") 'haskell-simple-indent-newline-same-col)
        (define-key haskell-mode-map (kbd "C-<return>") 'haskell-simple-indent-newline-indent)

        ;; Load the current file (and make a session if not already made).
        (define-key haskell-mode-map [?\C-c ?\C-l] 'haskell-process-load-file)
        (define-key haskell-mode-map [f5] 'haskell-process-load-file)

        ;; Switch to the REPL.
        (define-key haskell-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch)
        ;; “Bring” the REPL, hiding all other windows apart from the source
        ;; and the REPL.
        (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)

        ;; Build the Cabal project.
        (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
        ;; Interactively choose the Cabal command to run.
        (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)

        ;; Get the type and info of the symbol at point, print it in the
        ;; message buffer.
        (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
        (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)

        ;; Contextually do clever things on the space key, in particular:
        ;;   1. Complete imports, letting you choose the module name.
        ;;   2. Show the type of the symbol after the space.
        (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

        ;; Jump to the imports. Keep tapping to jump between import
        ;; groups. C-u f8 to jump back again.
        (define-key haskell-mode-map [f8] 'haskell-navigate-imports)

        ;; Jump to the definition of the current symbol.
        (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)

        ;; Indent the below lines on columns after the current column.
        (define-key haskell-mode-map (kbd "C-<right>")
          (lambda ()
            (interactive)
            (haskell-move-nested 1)))
        ;; Same as above but backwards.
        (define-key haskell-mode-map (kbd "C-<left>")
          (lambda ()
            (interactive)
            (haskell-move-nested -1))))

      ;; Useful to have these keybindings for .cabal files, too.
      (defun haskell-cabal-hook ()
        (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
        (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
        (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
        (define-key haskell-cabal-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch)))))

(defun spacemacs/init-helm ()
  (use-package helm
    :defer t
    :init
    (progn 
      (add-to-hooks 'helm-mode '(erlang-mode-hook
                                 markdown-mode-hook
                                 org-mode-hook
                                 prog-mode-hook
                                 ))
      (evil-leader/set-key
        ":"   'helm-M-x
        "bs"  'helm-mini
        "kil"  'helm-how-kill-ring
        "hg"  'helm-bookmarks))
    :config
    (progn
      ;; helm keybindings tweaks
      ;; use home row keys
      ;; the original hot key of helm-keyboard-quit is "C-g"
      (define-key helm-map (kbd "f")
        (lambda () (interactive) (fd-trigger 'helm-keyboard-quit)))
      ;; helm navigation on hjkl
      (define-key helm-map (kbd "C-j") 'helm-next-line)
      (define-key helm-map (kbd "C-k") 'helm-previous-line)
      (define-key helm-map (kbd "C-h") 'helm-next-source)
      (define-key helm-map (kbd "C-l") 'helm-previous-source))))

(defun spacemacs/init-helm-css-scss ()
  (use-package helm-css-scss
    :defer t
    :init
    (evil-leader/set-key
      "hc"    'helm-css-scss)))

(defun spacemacs/init-helm-c-yasnippet ()
  (use-package helm-c-yasnippet
    :commands helm-c-yas-complete
    :init
    (evil-leader/set-key "hy" 'helm-c-yas-complete)
    :config
    (progn
      (setq helm-c-yas-space-match-any-greedy t)
      (use-package yasnippet
        :config
        (progn
          (setq yas-snippet-dirs (list (concat spacemacs-config-directory "snippets")))
          (yas-global-mode 1))))))

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

(defun spacemacs/init-helm-swoop ()
  (use-package helm-swoop
    :defer t
    :init
    (evil-leader/set-key
      "hS"    'helm-multi-swoop
      "hs"    'helm-swoop
      "h C-s" 'helm-multi-swoop-all)))

(defun spacemacs/init-helm-themes ()
  (use-package helm-themes
    :defer t
    :init
    (evil-leader/set-key
      "ht"    'helm-themes)))

(defun spacemacs/init-hy-mode ()
  (use-package hy-mode
    :defer t))

(defun spacemacs/init-js2-mode ()
  (use-package js2-mode
    :commands (js2-minor-mode
               ac-js2-mode)
    :init
    (progn (add-hook 'js-mode-hook 'js2-minor-mode)
           (add-hook 'js2-mode-hook 'ac-js2-mode))))

(defun spacemacs/init-json-mode ()
  (use-package json-mode
    :defer t))

(defun spacemacs/init-ledger-mode ()
  (use-package ledger-mode
    :mode ("\\.ledger\\'" . ledger-mode)
    :init
    (progn 
      (setq ledger-post-amount-alignment-column 62)
      (evil-leader/set-key-for-mode 'ledger-mode
        "md" 'ledger-delete-current-transaction
        "ma" 'ledger-add-transaction))))

(defun spacemacs/init-magit ()
  (use-package magit
    :defer t
    :init
    (evil-leader/set-key "gs" 'magit-status)
    :config
    (progn
      ;; full screen magit-status
      (defadvice magit-status (around magit-fullscreen activate)
        (window-configuration-to-register :magit-fullscreen)
        ad-do-it
        (delete-other-windows))

      (evil-add-hjkl-bindings magit-branch-manager-mode-map 'emacs
        "K" 'magit-discard-item
        "L" 'magit-key-mode-popup-logging)
      (evil-add-hjkl-bindings magit-commit-mode-map 'emacs)
      (evil-add-hjkl-bindings magit-log-mode-map 'emacs)
      (evil-add-hjkl-bindings magit-process-mode-map 'emacs)
      (evil-add-hjkl-bindings magit-status-mode-map 'emacs
        "f" 'magit-key-mode-popup-fetching
        "K" 'magit-discard-item
        "l" 'magit-key-mode-popup-logging
        "h" 'magit-toggle-diff-refine-hunk)

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

(defun spacemacs/init-markdown-mode ()
  (use-package markdown-mode
    :mode ("\\.md" . markdown-mode)))

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

(defun spacemacs/init-org ()
  (use-package org
    :mode ("\\.org$" . org-mode)
    :config
    (progn
      (require 'org-install)
      (define-key global-map "\C-cl" 'org-store-link)
      (define-key global-map "\C-ca" 'org-agenda)
      (setq org-log-done t)
      (setq org-agenda-files '("~/Dropbox/org"))
      (use-package org-bullets
        :config
        (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
      (use-package org-trello
        :config
        (add-hook 'org-mode-hook 'org-trello-mode))))

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
    (global-page-break-lines-mode t)))

(defun spacemacs/init-paredit ()
  (use-package paredit
    :defer t
    :config
    (progn
      (dolist (mode '(scheme emacs-lisp lisp clojure clojurescript))
        (when (> (display-color-cells) 8)
          (font-lock-add-keywords (intern (concat (symbol-name mode) "-mode"))
                                  '(("(\\|)" . 'esk-paren-face))))
        (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
                  'paredit-mode)))))

(defun spacemacs/init-popup ()
  (use-package popup
    :defer t))

(defun spacemacs/init-popwin ()
  (use-package popwin
    :init
    (progn
      (popwin-mode 1)
      (evil-leader/set-key "wp" 'popwin:close-popup-window)
      (push '("*ert*"                      :dedicated t :position bottom :stick t :noselect t) popwin:special-display-config)
      (push '("*grep*"                     :dedicated t :position bottom :stick t :noselect t) popwin:special-display-config)
      (push '("*nosetests*"                :dedicated t :position bottom :stick t :noselect t) popwin:special-display-config)
      (push '("^\*Flycheck.+\*$" :regexp t :dedicated t :position bottom :stick t :noselect t) popwin:special-display-config)
      (push '("^\*WoMan.+\*$"    :regexp t              :position bottom                     ) popwin:special-display-config)
      (push '("^\*helm.+\*$"     :regexp t              :position bottom                     ) popwin:special-display-config)
      (push '("^\*helm-.+\*$"    :regexp t              :position bottom                     ) popwin:special-display-config))))

(defun spacemacs/init-powershell ()
  (use-package powershell
    :commands powershell))

(defun spacemacs/init-powershell-mode ()
  (use-package powershell-mode
    :defer t
    :mode ("\\.ps1\\'" . powershell-mode)))

(defun spacemacs/init-projectile ()
  (use-package projectile
    :commands (projectile-switch-to-buffer
               projectile-invalidate-cache
               projectile-dired
               projectile-find-file
               helm-projectile
               projectile-kill-buffers
               projectile-grep
               projectile-replace)
    :init
    (evil-leader/set-key
      "pb" 'projectile-switch-to-buffer
      "pC" 'projectile-invalidate-cache
      "pd" 'projectile-dired
      "pF" 'projectile-find-file
      "pf" 'helm-projectile
      "pk" 'projectile-kill-buffers
      "pg" 'projectile-grep
      "pr" 'projectile-replace)))

(defun spacemacs/init-python ()
  (use-package python
    :defer t
    :init
    (progn
      (add-hook 'python-mode-hook '(lambda() (setq tab-width 4)))
      ;; from http://pedrokroger.net/2010/07/configuring-emacs-as-a-python-ide-2/
      (defun annotate-pdb ()
        "Highlight break point lines."
        (interactive)
        (highlight-lines-matching-regexp "import pdb")
        (highlight-lines-matching-regexp "pdb.set_trace()"))
      (add-hook 'python-mode-hook 'annotate-pdb)
      (setq
       python-shell-interpreter "ipython"
       ;; python-shell-interpreter-args (if (system-is-mac)
       ;;                                   "--gui=osx --matplotlib=osx --colors=Linux"
       ;;                                 (if (system-is-linux)
       ;;                                     "--gui=wx --matplotlib=wx --colors=Linux"))
       python-shell-prompt-regexp "In \\[[0-9]+\\]: "
       python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
       python-shell-completion-setup-code "from IPython.core.completerlib import module_completion"
       python-shell-completion-module-string-code "';'.join(module_completion('''%s'''))\n"
       python-shell-completion-string-code "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
      (use-package jedi
        :defer t
        :init
        (progn
          (setq jedi:setup-keys t)
          (add-hook 'python-mode-hook 'jedi:setup))
        :config
        (progn
          (setq jedi:complete-on-dot t))))
    :config
    (progn
      ;; from http://pedrokroger.net/2010/07/configuring-emacs-as-a-python-ide-2/
      (defun python-add-breakpoint ()
        "Add a break point, highlight it and save the buffer."
        (interactive)
        (evil-end-of-line)
        (newline-and-indent)
        (insert "import pdb; pdb.set_trace()")
        (save-buffer))
      (evil-leader/set-key-for-mode 'python-mode
        "mB"  (lambda ()
                " Send buffer content to shell and switch to it in insert mode."
                (interactive)
                (python-shell-send-buffer)
                (python-shell-switch-to-shell)
                (evil-insert-state))
        "mb"  'python-shell-send-buffer
        "md"  'pylookup-lookup
        "mF"  (lambda ()
                " Send function content to shell and switch to it in insert mode."
                (interactive)
                (python-shell-send-defun nil)
                (python-shell-switch-to-shell)
                (evil-insert-state))
        "mf"  'python-shell-send-defun
        "mg"  'jedi:goto-definition
        "mi"  (lambda ()
                " Switch to shell in insert mode."
                (interactive)
                (python-shell-switch-to-shell)
                (evil-insert-state))
        "mp"  'python-add-breakpoint
        "mR"  (lambda (start end)
                " Send region content to shell and switch to it in insert mode."
                (interactive "r")
                (python-shell-send-region start end)
                (python-shell-switch-to-shell)
                (evil-insert-state))
        "mr"  'python-shell-send-region
        "mTf" 'nosetests-pdb-one
        "mtf" 'nosetests-one
        "mTa" 'nosetests-pdb-all
        "mta" 'nosetests-all
        "mTm" 'nosetests-pdb-module
        "mtm" 'nosetests-module
        "mTs" 'nosetests-pdb-suite
        "mts" 'nosetests-suite
        "m RET" 'quickrun)
      (define-key inferior-python-mode-map (kbd "C-j") 'comint-next-input)
      (define-key inferior-python-mode-map (kbd "C-k") 'comint-previous-input))))

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

      (setq-default frame-background-mode 'dark)
      (add-to-hooks
       'turn-on-rainbow-delimiters-mode '(prog-mode-hook
                                          erlang-mode-hook
                                          )))))

(defun spacemacs/init-rainbow-identifiers ()
  (use-package rainbow-identifiers
    :commands rainbow-identifiers-mode
    :init
    (add-to-hooks 'rainbow-identifiers-mode '(prog-mode-hook
                                              erlang-mode-hook))))

(defun spacemacs/init-rainbow-mode ()
  (use-package rainbow-mode
    :defer t
    :init (evil-leader/set-key "tc" 'rainbow-mode)))

(defun spacemacs/init-rcirc ()
  (use-package rcirc
    :commands irc
    :init
    (progn
      (add-to-hook 'rcirc-mode-hook '(rcirc-track-minor-mode
                                      rcirc-omit-mode
                                      ;; rcirc-reconnect-mode
                                      flyspell-mode)))
    :config
    (progn
      (setq rcirc-fill-column 160)
      (setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))
      (setq rcirc-omit-threshold 20)
      (setq rcirc-server-alist
            '(("chat.freenode.net" :port 6697 :encryption tls
               :nick "syl20bnr"
               :full-name "Sylvain Benner"
               :channels ("#emacs" "#nupic" "#python"))))
      (require 'rcirc-color)
      (require 'rcirc-reconnect
               (concat spacemacs-extensions-directory "rcirc-reconnect/rcirc-reconnect.el"))
      ;; identify info are stored in a separate location, skip errors
      ;; if the feature cannot be found.
      (require 'pinit-rcirc nil 'noerror))))

(defun spacemacs/init-recentf ()
  (use-package recentf
    :defer t
    :config
    (progn
      (setq recentf-exclude '("~/.emacs.d/.recentf"))
      (setq recentf-save-file (concat user-emacs-directory "/.recentf"))
      (setq recentf-max-saved-items 100)
      (setq recentf-auto-cleanup 'never)
      (setq recentf-auto-save-timer (run-with-idle-timer 600 t 'recentf-save-list)))))

(defun spacemacs/init-rfringe ()
  (use-package rfringe
    :defer t))

(defun spacemacs/init-ruby-end ()
  (use-package ruby-end
    :defer t))

(defun spacemacs/init-ruby-mode ()
  (use-package ruby-mode
    :mode (("\\(rake\\|thor\\|guard\\|gem\\|cap\\|vagrant\\)file\\'" . ruby-mode)
           ("\\.\\(rb\\|ru\\|builder\\|rake\\|thor\\|gemspec\\)\\'" . ruby-mode))))

(defun spacemacs/init-scss-mode ()
  (use-package scss-mode
    :mode ("\\.scss\\'" . scss-mode)))

(defun spacemacs/init-smartparens ()
  (use-package smartparens-config
    :commands smartparens-mode
    :init
    (add-to-hooks 'smartparens-mode '(erlang-mode-hook
                                      markdown-mode-hook
                                      prog-mode-hook
                                      ))))

(defun spacemacs/init-smeargle ()
  (use-package smeargle
    :defer t
    :init
    (evil-leader/set-key
      "gcC" 'smeargle-clear
      "gcc" 'smeargle-commits
      "gct" 'smeargle)))

(defun spacemacs/init-string-edit ()
  (use-package string-edit
    :defer t
    :init
    (evil-leader/set-key "eds" 'string-edit-at-point)))

(defun spacemacs/init-subword ()
  (use-package subword
    :defer t
    :init
    (add-hook 'prog-mode-hook 'subword-mode)))

(defun spacemacs/init-tagedit ()
  (use-package tagedit
    :defer t
    :config
    (progn
      (tagedit-add-experimental-features)
      (add-hook 'html-mode-hook (lambda () (tagedit-mode 1))))))

(defun spacemacs/init-visual-regexp-steroids ()
  (use-package visual-regexp-steroids
    :defer t
    :init
    (evil-leader/set-key
      "rR" 'vr/query-replace
      "rr" 'vr/replace)))

(defun spacemacs/init-volatile-highlights ()
  (use-package volatile-highlights
    :init (volatile-highlights-mode t)))

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

(defun spacemacs/init-weather ()
  (use-package weather
    :commands weather-report
    :config
    (progn 
      ;; pinit files come from my dropbox folder
      (require 'pinit-weather nil 'noerror)
      (setq weather-distance-unit "km"))))

(defun spacemacs/init-web-mode ()
  (use-package web-mode
    :mode (("\\.phtml\\'"     . web-mode)
           ("\\.tpl\\.php\\'" . web-mode)
           ("\\.[gj]sp\\'  "  . web-mode)
           ("\\.as[cp]x\\'"   . web-mode)
           ("\\.erb\\'"       . web-mode)
           ("\\.mustache\\'"  . web-mode)
           ("\\.djhtml\\'"    . web-mode))))

(defun spacemacs/init-zenburn-theme ()
  (use-package zenburn-theme
    :defer t))
