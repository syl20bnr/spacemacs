;;; packages.el --- Spacemacs Core Layer packages File
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
        bind-key
        (bind-map :step pre)
        bookmark
        diminish
        (electric-indent-mode :location built-in)
        ediff
        eldoc
        evil
        evil-ediff
        evil-escape
        ;; evil-leader
        evil-surround
        evil-visualstar
        (evil-evilified-state :location local :step pre :protected t)
        exec-path-from-shell
        fill-column-indicator
        help-fns+
        hl-todo
        (hs-minor-mode :location built-in)
        (holy-mode :location local :step pre)
        (hybrid-mode :location local :step pre)
        hydra
        (ido :location built-in)
        ido-vertical-mode
        (package-menu :location built-in)
        page-break-lines
        popup
        popwin
        (process-menu :location built-in)
        projectile
        quelpa
        recentf
        ;; request is not a built-in package
        ;; this is a hack to be able to configure request cache directory.
        (request :location built-in)
        restart-emacs
        savehist
        saveplace
        spacemacs-theme
        subword
        undo-tree
        (uniquify :location built-in)
        (url :location built-in)
        use-package
        which-key
        whitespace
        winner
        ws-butler))

;; Initialization of packages

(defun spacemacs-base/init-bind-key ())

(defun spacemacs-base/init-bind-map ()
  (use-package bind-map
    :init
    (bind-map spacemacs-default-map
      :prefix-cmd spacemacs-cmds
      :keys (dotspacemacs-emacs-leader-key)
      :evil-keys (dotspacemacs-leader-key)
      :override-minor-modes t
      :override-mode-name spacemacs-leader-override-mode)))

(defun spacemacs-base/init-bookmark ()
  (use-package bookmark
    :defer t
    :init
    (setq bookmark-default-file (concat spacemacs-cache-directory "bookmarks")
          ;; autosave each change
          bookmark-save-flag 1)))

(defun spacemacs-base/init-diminish ()
  (use-package diminish
    :init
    (progn
      ;; Minor modes abbrev --------------------------------------------------------
      (when (display-graphic-p)
        (with-eval-after-load 'eproject
          (diminish 'eproject-mode " eⓅ"))
        (with-eval-after-load 'flymake
          (diminish 'flymake-mode " Ⓕ2")))
      ;; Minor Mode (hidden) ------------------------------------------------------
      (with-eval-after-load 'elisp-slime-nav
        (diminish 'elisp-slime-nav-mode))
      (with-eval-after-load 'hi-lock
        (diminish 'hi-lock-mode))
      (with-eval-after-load 'abbrev
        (diminish 'abbrev-mode))
      (with-eval-after-load 'subword
        (when (eval-when-compile (version< "24.3.1" emacs-version))
          (diminish 'subword-mode))))))

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

(defun spacemacs-base/init-electric-indent-mode ()
  (electric-indent-mode))

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

(defun spacemacs-base/init-evil-ediff ()
  (use-package evil-ediff
    :after (ediff)
    :if (memq dotspacemacs-editing-style '(hybrid vim))))

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

(defun spacemacs-base/init-evil ()
  (use-package evil
    :init
    (progn
      (defvar spacemacs-evil-cursors '(("normal" "DarkGoldenrod2" box)
                                       ("insert" "chartreuse3" (bar . 2))
                                       ("emacs" "SkyBlue2" box)
                                       ("hybrid" "SkyBlue2" (bar . 2))
                                       ("replace" "chocolate" (hbar . 2))
                                       ("evilified" "LightGoldenrod3" box)
                                       ("visual" "gray" (hbar . 2))
                                       ("motion" "plum3" box)
                                       ("lisp" "HotPink1" box)
                                       ("iedit" "firebrick1" box)
                                       ("iedit-insert" "firebrick1" (bar . 2)))
        "Colors assigned to evil states with cursor definitions.")

      (loop for (state color cursor) in spacemacs-evil-cursors
            do
            (eval `(defface ,(intern (format "spacemacs-%s-face" state))
                     `((t (:background ,color
                                       :foreground ,(face-background 'mode-line)
                                       :box ,(face-attribute 'mode-line :box)
                                       :inherit 'mode-line)))
                     (format "%s state face." state)
                     :group 'spacemacs))
            (eval `(setq ,(intern (format "evil-%s-state-cursor" state))
                         (list (when dotspacemacs-colorize-cursor-according-to-state color)
                               cursor))))

      ;; put back refresh of the cursor on post-command-hook see status of:
      ;; https://bitbucket.org/lyro/evil/issue/502/cursor-is-not-refreshed-in-some-cases
      ;; (add-hook 'post-command-hook 'evil-refresh-cursor)

      (defun spacemacs/state-color-face (state)
        "Return the symbol of the face for the given STATE."
        (intern (format "spacemacs-%s-face" (symbol-name state))))

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

      (defun evil-insert-state-cursor-hide ()
        (setq evil-insert-state-cursor '((hbar . 0))))

      (unless (eq dotspacemacs-editing-style 'emacs)
        (evil-mode 1)))
    :config
    (progn
      ;; bind function keys

      ;; bind evil-jump-forward for GUI only.
      (define-key evil-motion-state-map [C-i] 'evil-jump-forward)

      ;; Make the current definition and/or comment visible.
      (define-key evil-normal-state-map "zf" 'reposition-window)
      ;; toggle maximize buffer
      (define-key evil-window-map (kbd "o") 'spacemacs/toggle-maximize-buffer)
      (define-key evil-window-map (kbd "C-o") 'spacemacs/toggle-maximize-buffer)
      ;; make cursor keys work
      (define-key evil-window-map (kbd "<left>") 'evil-window-left)
      (define-key evil-window-map (kbd "<right>") 'evil-window-right)
      (define-key evil-window-map (kbd "<up>") 'evil-window-up)
      (define-key evil-window-map (kbd "<down>") 'evil-window-down)
      (spacemacs/set-leader-keys "re" 'evil-show-registers)
      (define-key evil-visual-state-map (kbd "<escape>") 'keyboard-quit)
      ;; motions keys for help buffers
      (evil-define-key 'motion help-mode-map (kbd "ESC") 'quit-window)
      (evil-define-key 'motion help-mode-map (kbd "<tab>") 'forward-button)
      (evil-define-key 'motion help-mode-map (kbd "S-<tab>") 'backward-button)
      (evil-define-key 'motion help-mode-map (kbd "]") 'help-go-forward)
      (evil-define-key 'motion help-mode-map (kbd "gf") 'help-go-forward)
      (evil-define-key 'motion help-mode-map (kbd "[") 'help-go-back)
      (evil-define-key 'motion help-mode-map (kbd "gb") 'help-go-back)
      (evil-define-key 'motion help-mode-map (kbd "gh") 'help-follow-symbol)

      ;; replace `dired-goto-file' with `helm-find-files', since `helm-find-files'
      ;; can do the same thing and with fuzzy matching and other features.
      (with-eval-after-load 'dired
        (evil-define-key 'normal dired-mode-map "J" 'spacemacs/helm-find-files)
        (define-key dired-mode-map "j" 'spacemacs/helm-find-files)
        (evil-define-key 'normal dired-mode-map (kbd dotspacemacs-leader-key)
          spacemacs-default-map))

      ;; It's better that the default value is too small than too big
      (setq-default evil-shift-width 2)
      ;; After major mode has changed, reset evil-shift-width
      (add-hook 'after-change-major-mode-hook 'spacemacs//set-evil-shift-width 'append)

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

      ;; scrolling transient state
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
      (spacemacs|define-transient-state scroll
        :title "Scrolling Transient State"
        :bindings
        ("," evil-scroll-page-up "page up")
        ("." evil-scroll-page-down "page down")
        ;; half page
        ("<" spacemacs/scroll-half-page-up "half page up")
        (">" spacemacs/scroll-half-page-down "half page down"))
      (spacemacs/set-leader-keys
        "n," 'spacemacs/scroll-transient-state/evil-scroll-page-up
        "n." 'spacemacs/scroll-transient-state/evil-scroll-page-down
        "n<" 'spacemacs/scroll-transient-state/spacemacs/scroll-half-page-up
        "n>" 'spacemacs/scroll-transient-state/spacemacs/scroll-half-page-down)

      ;; pasting transient-state
      (spacemacs|define-transient-state paste
        :title "Pasting Transient State"
        :doc "\n[%s(length kill-ring-yank-pointer)/%s(length kill-ring)] \
[_C-j_/_C-k_] cycles through yanked text, [_p_/_P_] pastes the same text above or \
below. Anything else exits."
        :bindings
        ("C-j" evil-paste-pop)
        ("C-k" evil-paste-pop-next)
        ("p" evil-paste-after)
        ("P" evil-paste-before))
      (when dotspacemacs-enable-paste-transient-state
        (define-key evil-normal-state-map "p" 'spacemacs/paste-transient-state/evil-paste-after)
        (define-key evil-normal-state-map "P" 'spacemacs/paste-transient-state/evil-paste-before))

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
             (with-eval-after-load 'evil-surround
               (push (cons (string-to-char ,key)
                           (if ,end
                               (cons ,start ,end)
                             ,start))
                     evil-surround-pairs-alist)))))

      (spacemacs|define-text-object "$" "dollar" "$" "$")
      (spacemacs|define-text-object "*" "star" "*" "*")
      (spacemacs|define-text-object "8" "block-star" "/*" "*/")
      (spacemacs|define-text-object "|" "bar" "|" "|")
      (spacemacs|define-text-object "%" "percent" "%" "%")
      (spacemacs|define-text-object "/" "slash" "/" "/")
      (spacemacs|define-text-object "_" "underscore" "_" "_")
      (spacemacs|define-text-object "-" "hyphen" "-" "-")
      (spacemacs|define-text-object "~" "tilde" "~" "~")
      (spacemacs|define-text-object "=" "equal" "=" "=")

      (evil-define-text-object evil-pasted (count &rest args)
        (list (save-excursion (evil-goto-mark ?\[) (point))
              (save-excursion (evil-goto-mark ?\]) (point))))
      (define-key evil-inner-text-objects-map "P" 'evil-pasted)

      ;; define text-object for entire buffer
      (evil-define-text-object evil-inner-buffer (count &optional beg end type)
        (evil-select-paren "\\`" "\\'" beg end type count nil))
      (define-key evil-inner-text-objects-map "g" 'evil-inner-buffer)

      ;; support smart 1parens-strict-mode
      (when (configuration-layer/package-usedp 'smartparens)
        (defadvice evil-delete-backward-char-and-join
            (around spacemacs/evil-delete-backward-char-and-join activate)
          (if (bound-and-true-p smartparens-strict-mode)
              (call-interactively 'sp-backward-delete-char)
            ad-do-it)))

      ;; Define history commands for comint
      (evil-define-key 'insert comint-mode-map
        (kbd "C-k") 'comint-next-input
        (kbd "C-j") 'comint-previous-input)
      (evil-define-key 'normal comint-mode-map
        (kbd "C-k") 'comint-next-input
        (kbd "C-j") 'comint-previous-input))))

(defun spacemacs-base/init-evil-escape ()
  (use-package evil-escape
    :init
    (unless (eq dotspacemacs-editing-style 'emacs)
      (evil-escape-mode))
    :config
    (spacemacs|hide-lighter evil-escape-mode)))

(defun spacemacs-base/init-evil-surround ()
  (use-package evil-surround
    :init
    (progn
      (global-evil-surround-mode 1)
      ;; `s' for surround instead of `substitute'
      ;; see motivation for this change in the documentation
      (evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
      (evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute))))

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

(defun spacemacs-base/init-evil-evilified-state ()
  (use-package evil-evilified-state)
  (define-key evil-evilified-state-map (kbd dotspacemacs-leader-key)
    spacemacs-default-map))

(defun spacemacs-base/init-exec-path-from-shell ()
  (use-package exec-path-from-shell
    :init (when (memq window-system '(mac ns x))
            (exec-path-from-shell-initialize))))

(defun spacemacs-base/init-fill-column-indicator ()
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

(defun spacemacs-base/init-help-fns+ ()
  (use-package help-fns+
    :commands (describe-keymap)
    :init (spacemacs/set-leader-keys "hdK" 'describe-keymap)))

(defun spacemacs-base/init-hl-todo ()
  (use-package hl-todo
    :defer t
    :init (spacemacs/add-to-hooks 'hl-todo-mode '(text-mode-hook
                                                  prog-mode-hook))))

(defun spacemacs-base/init-hs-minor-mode ()
  ;; required for evil folding
  (defun spacemacs//enable-hs-minor-mode ()
    "Enable hs-minor-mode for code folding."
    (ignore-errors
      (hs-minor-mode)
      (spacemacs|hide-lighter hs-minor-mode)))
  (add-hook 'prog-mode-hook 'spacemacs//enable-hs-minor-mode))

(defun spacemacs-base/init-holy-mode ()
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
        :documentation "Globally toggle holy mode."
        :evil-leader "tEe")
      (spacemacs|diminish holy-mode " Ⓔe" " Ee"))))

(defun spacemacs-base/init-hybrid-mode ())
(defun spacemacs-base/post-init-evil ()
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

(defun spacemacs-base/init-hydra ()
  (use-package hydra
    :init
    ;; turn off evil in corelv buffers
    (push '("\\*LV\\*") evil-buffer-regexps)

    (defun spacemacs//hydra-key-doc-function (key key-width doc doc-width)
      (format (format "[%%%ds] %%%ds" key-width (- -1 doc-width))
              key doc))
    (setq hydra-key-doc-function 'spacemacs//hydra-key-doc-function)
    (setq hydra-head-format "[%s] ")))

(defun spacemacs-base/init-ido ()
  (ido-mode t)
  (setq ido-save-directory-list-file (concat spacemacs-cache-directory
                                             "ido.last")
        ;; enable fuzzy matching
        ido-enable-flex-matching t))

(defun spacemacs-base/init-package-menu ()
  (evilified-state-evilify-map package-menu-mode-map
    :mode package-menu-mode))

(defun spacemacs-base/init-ido-vertical-mode ()
  (use-package ido-vertical-mode
    :init
    (progn
      (ido-vertical-mode t)
      (when dotspacemacs-use-ido
        (spacemacs/set-leader-keys "ff" 'ido-find-file))
      (defun spacemacs//ido-minibuffer-setup ()
        "Setup the minibuffer."
        ;; Since ido is implemented in a while loop where each
        ;; iteration setup a whole new minibuffer, we have to keep
        ;; track of any activated ido navigation transient-state and force
        ;; the reactivation at each iteration.
        (when spacemacs--ido-navigation-ms-enabled
          (spacemacs/ido-navigation-micro-state)))
      (add-hook 'ido-minibuffer-setup-hook 'spacemacs//ido-minibuffer-setup)

      (defun spacemacs//ido-setup ()
        (when spacemacs--ido-navigation-ms-face-cookie-minibuffer
          (face-remap-remove-relative
           spacemacs--ido-navigation-ms-face-cookie-minibuffer))
        ;; be sure to wipe any previous transient-state flag
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
        ;; initiate transient-state
        (define-key ido-completion-map (kbd "M-SPC") 'spacemacs/ido-navigation-micro-state)
        (define-key ido-completion-map (kbd "s-M-SPC") 'spacemacs/ido-navigation-micro-state)
        )
      (add-hook 'ido-setup-hook 'spacemacs//ido-setup)

      (defun spacemacs/ido-invoke-in-other-window ()
        "signals ido mode to switch to (or create) another window after exiting"
        (interactive)
        (setq ido-exit-minibuffer-target-window 'other)
        (ido-exit-minibuffer))

      (defun spacemacs/ido-invoke-in-horizontal-split ()
        "signals ido mode to split horizontally and switch after exiting"
        (interactive)
        (setq ido-exit-minibuffer-target-window 'horizontal)
        (ido-exit-minibuffer))

      (defun spacemacs/ido-invoke-in-vertical-split ()
        "signals ido mode to split vertically and switch after exiting"
        (interactive)
        (setq ido-exit-minibuffer-target-window 'vertical)
        (ido-exit-minibuffer))

      (defun spacemacs/ido-invoke-in-new-frame ()
        "signals ido mode to create a new frame after exiting"
        (interactive)
        (setq ido-exit-minibuffer-target-window 'frame)
        (ido-exit-minibuffer))

      (defadvice ido-read-internal
          (around ido-read-internal-with-minibuffer-other-window activate)
        (let* (ido-exit-minibuffer-target-window
               (this-buffer (current-buffer))
               (result ad-do-it))
          (cond
           ((equal ido-exit-minibuffer-target-window 'other)
            (if (= 1 (count-windows))
                (spacemacs/split-window-horizontally-and-switch)
              (other-window 1)))
           ((equal ido-exit-minibuffer-target-window 'horizontal)
            (spacemacs/split-window-horizontally-and-switch))

           ((equal ido-exit-minibuffer-target-window 'vertical)
            (spacemacs/split-window-vertically-and-switch))
           ((equal ido-exit-minibuffer-target-window 'frame)
            (make-frame)))
          ;; why? Some ido commands, such as textmate.el's
          ;; textmate-goto-symbol don't switch the current buffer
          (switch-to-buffer this-buffer)
          result))

      (defvar spacemacs--ido-navigation-ms-enabled nil
        "Flag which is non nil when ido navigation transient-state is enabled.")

      (defvar spacemacs--ido-navigation-ms-face-cookie-minibuffer nil
        "Cookie pointing to the local face remapping.")

      (defface spacemacs-ido-navigation-ms-face
        `((t :background ,(face-attribute 'error :foreground)
             :foreground "black"
             :weight bold))
        "Face for ido minibuffer prompt when ido transient-state is activated."
        :group 'spacemacs)

      (defun spacemacs//ido-navigation-ms-set-face ()
        "Set faces for ido navigation transient-state."
        (setq spacemacs--ido-navigation-ms-face-cookie-minibuffer
              (face-remap-add-relative
               'minibuffer-prompt
               'spacemacs-ido-navigation-ms-face)))

      (defun spacemacs//ido-navigation-ms-on-enter ()
        "Initialization of ido transient-state."
        (setq spacemacs--ido-navigation-ms-enabled t)
        (spacemacs//ido-navigation-ms-set-face))

      (defun spacemacs//ido-navigation-ms-on-exit ()
        "Action to perform when exiting ido transient-state."
        (face-remap-remove-relative
         spacemacs--ido-navigation-ms-face-cookie-minibuffer))

      (defun spacemacs//ido-navigation-ms-full-doc ()
        "Full documentation for ido navigation transient-state."
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

      (spacemacs|define-transient-state ido-navigation
        :title "ido Transient State"
        :foreign-keys run
        :on-enter (spacemacs//ido-navigation-ms-on-enter)
        :on-exit  (spacemacs//ido-navigation-ms-on-exit)
        :bindings
        ;;("?" nil (spacemacs//ido-navigation-ms-full-doc))
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

(defun spacemacs-base/init-page-break-lines ()
  (use-package page-break-lines
    :init
    (global-page-break-lines-mode t)
    (spacemacs|hide-lighter page-break-lines-mode)))

(defun spacemacs-base/init-popup ()
  (use-package popup
    :defer t))

(defun spacemacs-base/init-popwin ()
  (use-package popwin
    :config
    (progn
      (popwin-mode 1)
      (spacemacs/set-leader-keys "wpm" 'popwin:messages)
      (spacemacs/set-leader-keys "wpp" 'popwin:close-popup-window)

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

(defun spacemacs-base/init-process-menu ()
  (evilified-state-evilify process-menu-mode process-menu-mode-map))

(defun spacemacs-base/init-projectile ()
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
      ;; note for Windows: GNU find or Cygwin find must be in path to enable
      ;; fast indexing
      (when (and (spacemacs/system-is-mswindows) (executable-find "find"))
        (setq  projectile-indexing-method 'alien
               projectile-generic-command "find . -type f"))
      (setq projectile-sort-order 'recentf
            projectile-cache-file (concat spacemacs-cache-directory
                                          "projectile.cache")
            projectile-known-projects-file (concat spacemacs-cache-directory
                                                   "projectile-bookmarks.eld"))
      (unless (configuration-layer/package-usedp 'helm-projectile)
        (spacemacs/set-leader-keys
          "pb" 'projectile-switch-to-buffer
          "pd" 'projectile-find-dir
          "pf" 'projectile-find-file
          "pF" 'projectile-find-file-dwim
          "ph" 'helm-projectile
          "pr" 'projectile-recentf
          "ps" 'projectile-switch-project))
      (spacemacs/set-leader-keys
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

(defun spacemacs-base/init-quelpa ())

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

(defun spacemacs-base/init-request ()
  (setq request-storage-directory (concat spacemacs-cache-directory
                                          "request/")))

(defun spacemacs-base/init-restart-emacs()
  (use-package restart-emacs
    :defer t
    :init
    (defun spacemacs/restart-emacs (&optional args)
      "Restart emacs."
      (interactive)
      (setq spacemacs-really-kill-emacs t)
      (restart-emacs args))
    (defun spacemacs/restart-emacs-resume-layouts (&optional args)
      "Restart emacs and resume layouts."
      (interactive)
      (spacemacs/restart-emacs (cons "--resume-layouts" args)))
    (defun spacemacs/restart-emacs-debug-init (&optional args)
      "Restart emacs and enable debug-init."
      (interactive)
      (spacemacs/restart-emacs (cons "--debug-init" args)))
    (defun spacemacs/restart-stock-emacs-with-packages (packages &optional args)
      "Restart emacs without the spacemacs configuration, enable
debug-init and load the given list of packages."
      (interactive
       (let* ((guess (function-called-at-point)))
         (require 'finder-inf nil t)
         ;; Load the package list if necessary (but don't activate them).
         (unless package--initialized
           (package-initialize t))
         (let ((packages (append (mapcar 'car package-alist)
                                 (mapcar 'car package-archive-contents)
                                 (mapcar 'car package--builtins))))
           (unless (memq guess packages)
             (setq guess nil))
           (setq packages (mapcar 'symbol-name packages))
           (let ((val
                  (completing-read-multiple
                   (if guess
                       (format "Describe package (default %s): "
                               guess)
                     "Describe package: ")
                   packages nil t nil nil guess)))
             `(,val)))))
      (let ((load-packages-string (mapconcat (lambda (pkg) (format "(use-package %s)" pkg))
                                             packages " ")))
        (spacemacs/restart-emacs-debug-init
         (append (list "-q" "--execute"
                       (concat "(progn (package-initialize) "
                               "(require 'use-package)"
                               load-packages-string ")"))
                 args))))
    (spacemacs/set-leader-keys
      "qd" 'spacemacs/restart-emacs-debug-init
      "qD" 'spacemacs/restart-stock-emacs-with-packages
      "qr" 'spacemacs/restart-emacs-resume-layouts
      "qR" 'spacemacs/restart-emacs)))

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

(defun spacemacs-base/init-undo-tree ()
  (use-package undo-tree
    :init
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)
    :config
    (spacemacs|hide-lighter undo-tree-mode)))

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

(defun spacemacs-base/init-use-package ())

(defun spacemacs-base/init-which-key ()
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

      (spacemacs/set-leader-keys "hk" 'which-key-show-top-level)

      (let ((new-descriptions
             ;; being higher in this list means the replacement is applied later
             '(
               ("spacemacs/\\(.+\\)" . "\\1")
               ("spacemacs/toggle-\\(.+\\)" . "\\1")
               ("select-window-\\([0-9]\\)" . "window \\1")
               ("spacemacs/alternate-buffer" . "last buffer")
               ("spacemacs/toggle-mode-line-\\(.+\\)" . "\\1")
               ("avy-goto-word-or-subword-1" . "avy word")
               ("shell-command" . "shell cmd")
               ("spacemacs/default-pop-shell" . "open shell")
               ("spacemacs/helm-project-smart-do-search-region-or-symbol" . "smart search w/input")
               ("spacemacs/helm-project-smart-do-search" . "smart search")
               ("spacemacs/search-project-auto-region-or-symbol" . "search project w/input")
               ("spacemacs/search-project-auto" . "search project")
               ("helm-descbinds" . "show keybindings")
               ("sp-split-sexp" . "split sexp")
               ("avy-goto-line" . "avy line")
               ("universal-argument" . "universal arg")
               ("er/expand-region" . "expand region")
               ("helm-apropos" . "apropos")
               ("spacemacs/toggle-hybrid-mode" . "hybrid (hybrid-mode)")
               ("spacemacs/toggle-holy-mode" . "emacs (holy-mode)")
               ("evil-lisp-state-\\(.+\\)" . "\\1")
               ("\\(.+\\)-transient-state/\\(.+\\)" . "\\2")
               ("\\(.+\\)-transient-state/body" . "\\1-transient-state"))))
        (dolist (nd new-descriptions)
          ;; ensure the target matches the whole string
          (push (cons (concat "\\`" (car nd) "\\'") (cdr nd))
                which-key-description-replacement-alist)))
      (dolist (leader-key `(,dotspacemacs-leader-key ,dotspacemacs-emacs-leader-key))
        (which-key-add-key-based-replacements
          (concat leader-key " m")    "major mode commands"
          (concat leader-key " " dotspacemacs-emacs-command-key) "M-x"))
      (which-key-declare-prefixes
        dotspacemacs-leader-key '("root" . "Spacemacs root")
        dotspacemacs-emacs-leader-key '("root" . "Spacemacs root")
        (concat dotspacemacs-leader-key " m")
        '("major-mode-cmd" . "Major mode commands")
        (concat dotspacemacs-emacs-leader-key " m")
        '("major-mode-cmd" . "Major mode commands"))
      ;; disable special key handling for spacemacs, since it can be
      ;; disorienting if you don't understand it
      (pcase dotspacemacs-which-key-position
        (`right (which-key-setup-side-window-right))
        (`bottom (which-key-setup-side-window-bottom))
        (`right-then-bottom (which-key-setup-side-window-right-bottom)))
      (setq which-key-special-keys nil
            which-key-use-C-h-for-paging t
            which-key-prevent-C-h-from-cycling t
            which-key-echo-keystrokes 0.02
            which-key-max-description-length 32
            which-key-sort-order 'which-key-key-order-alpha
            which-key-idle-delay dotspacemacs-which-key-delay
            which-key-allow-evil-operators t)
      (which-key-mode)
      (spacemacs|diminish which-key-mode " Ⓚ" " K"))))

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

(defun spacemacs-base/init-ws-butler ()
  (use-package ws-butler
    :if (eq 'changed dotspacemacs-whitespace-cleanup)
    :config
    (progn
      (ws-butler-global-mode 1)
      (spacemacs|hide-lighter ws-butler-mode))))

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
