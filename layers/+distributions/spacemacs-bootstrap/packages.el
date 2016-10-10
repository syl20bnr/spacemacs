;;; packages.el --- Mandatory Bootstrap Layer packages File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq spacemacs-bootstrap-packages
      '(
        (async :step bootstrap)
        (bind-map :step bootstrap)
        (bind-key :step bootstrap)
        (diminish :step bootstrap)
        (evil :step bootstrap)
        (hydra :step bootstrap)
        (use-package :step bootstrap)
        (which-key :step bootstrap)
        ))

;; Note: `use-package' cannot be used for bootstrap packages configuration

(defun spacemacs-bootstrap/init-async ())

(defun spacemacs-bootstrap/init-bind-key ())

(defun spacemacs-bootstrap/init-diminish ()
  (when (not (configuration-layer/package-usedp 'spaceline))
    (add-hook 'after-load-functions 'spacemacs/diminish-hook)))

(defun spacemacs-bootstrap/init-bind-map ()
  (require 'bind-map)
  (bind-map spacemacs-default-map
    :prefix-cmd spacemacs-cmds
    :keys (dotspacemacs-emacs-leader-key)
    :evil-keys (dotspacemacs-leader-key)
    :override-minor-modes t
    :override-mode-name spacemacs-leader-override-mode))

(defun spacemacs-bootstrap/init-evil ()
  ;; evil-mode is mandatory for Spacemacs to work properly
  ;; evil must be require explicitly, the autoload seems to not
  ;; work properly sometimes.
  (require 'evil)
  (evil-mode 1)

  ;; Use evil as a default jump handler
  (push 'evil-goto-definition spacemacs-default-jump-handlers)

  (require 'cl)
  ;; State cursors
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

  (cl-loop for (state color cursor) in spacemacs-evil-cursors
           do
           (eval `(defface ,(intern (format "spacemacs-%s-face" state))
                    `((t (:background ,color
                                      :foreground ,(face-background 'mode-line)
                                      :inherit 'mode-line)))
                    (format "%s state face." state)
                    :group 'spacemacs))
           (set (intern (format "evil-%s-state-cursor" state))
                (list (when dotspacemacs-colorize-cursor-according-to-state color)
                      cursor)))

  (add-hook 'spacemacs-post-theme-change-hook 'spacemacs/set-state-faces)

  ;; put back refresh of the cursor on post-command-hook see status of:
  ;; https://bitbucket.org/lyro/evil/issue/502/cursor-is-not-refreshed-in-some-cases
  ;; (add-hook 'post-command-hook 'evil-refresh-cursor)

  ;; evil ex-command
  (define-key evil-normal-state-map (kbd dotspacemacs-ex-command-key) 'evil-ex)
  (define-key evil-visual-state-map (kbd dotspacemacs-ex-command-key) 'evil-ex)
  (define-key evil-motion-state-map (kbd dotspacemacs-ex-command-key) 'evil-ex)
  (setq evil-ex-substitute-global dotspacemacs-ex-substitute-global)

  ;; evil-want-Y-yank-to-eol must be set via customize to have an effect
  (customize-set-variable 'evil-want-Y-yank-to-eol dotspacemacs-remap-Y-to-y$)

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
  ;; motions keys for help buffers
  (evil-define-key 'motion help-mode-map (kbd "<escape>") 'quit-window)
  (evil-define-key 'motion help-mode-map (kbd "<tab>") 'forward-button)
  (evil-define-key 'motion help-mode-map (kbd "S-<tab>") 'backward-button)
  (evil-define-key 'motion help-mode-map (kbd "]") 'help-go-forward)
  (evil-define-key 'motion help-mode-map (kbd "gf") 'help-go-forward)
  (evil-define-key 'motion help-mode-map (kbd "[") 'help-go-back)
  (evil-define-key 'motion help-mode-map (kbd "gb") 'help-go-back)
  (evil-define-key 'motion help-mode-map (kbd "gh") 'help-follow-symbol)

  ;; It's better that the default value is too small than too big
  (setq-default evil-shift-width 2)
  ;; After major mode has changed, reset evil-shift-width
  (add-hook 'after-change-major-mode-hook 'spacemacs//set-evil-shift-width 'append)

  ;; Keep the region active when shifting
  (when dotspacemacs-retain-visual-state-on-shift
    (evil-map visual "<" "<gv")
    (evil-map visual ">" ">gv"))

  ;; move selection up and down
  (when dotspacemacs-visual-line-move-text
    (define-key evil-visual-state-map "J" (concat ":m '>+1" (kbd "RET") "gv=gv"))
    (define-key evil-visual-state-map "K" (concat ":m '<-2" (kbd "RET") "gv=gv")))

  (evil-ex-define-cmd "enew" 'spacemacs/new-empty-buffer)

  (define-key evil-normal-state-map (kbd "K") 'spacemacs/evil-smart-doc-lookup)
  (define-key evil-normal-state-map (kbd "gd") 'spacemacs/jump-to-definition)

  ;; scrolling transient state
  (spacemacs|define-transient-state scroll
    :title "Scrolling Transient State"
    :bindings
    ("," evil-scroll-page-up "page up")
    ("." evil-scroll-page-down "page down")
    ;; half page
    ("<" evil-scroll-up "half page up")
    (">" evil-scroll-down "half page down"))
  (spacemacs/set-leader-keys
    "n," 'spacemacs/scroll-transient-state/evil-scroll-page-up
    "n." 'spacemacs/scroll-transient-state/evil-scroll-page-down
    "n<" 'spacemacs/scroll-transient-state/evil-scroll-up
    "n>" 'spacemacs/scroll-transient-state/evil-scroll-down)

  ;; pasting transient-state
  (evil-define-command spacemacs//transient-state-0 ()
    :keep-visual t
    :repeat nil
    (interactive)
    (if current-prefix-arg
        (progn
          (setq this-command #'digit-argument)
          (call-interactively #'digit-argument))
      (setq this-command #'evil-beginning-of-line
            hydra-deactivate t)
      (call-interactively #'evil-beginning-of-line)))

  (spacemacs|define-transient-state paste
    :title "Pasting Transient State"
    :doc "\n[%s(length kill-ring-yank-pointer)/%s(length kill-ring)] \
 [_C-j_/_C-k_] cycles through yanked text, [_p_/_P_] pastes the same text \
 above or below. Anything else exits."
    :bindings
    ("C-j" evil-paste-pop)
    ("C-k" evil-paste-pop-next)
    ("p" evil-paste-after)
    ("P" evil-paste-before)
    ("0" spacemacs//transient-state-0))

  (when dotspacemacs-enable-paste-transient-state
    (define-key evil-normal-state-map
      "p" 'spacemacs/paste-transient-state/evil-paste-after)
    (define-key evil-normal-state-map
      "P" 'spacemacs/paste-transient-state/evil-paste-before))
  ;; fold transient state
  (when (eq 'evil dotspacemacs-folding-method)
    (spacemacs|define-transient-state fold
      :title "Code Fold Transient State"
      :doc "
 Close^^          Open^^              Toggle^^             Other^^
 ───────^^──────  ─────^^───────────  ─────^^────────────  ─────^^───
 [_c_] at point   [_o_] at point      [_a_] around point   [_q_] quit
 ^^               [_O_] recursively   ^^
 [_m_] all        [_r_] all"
      :foreign-keys run
      :bindings
      ("a" evil-toggle-fold)
      ("c" evil-close-fold)
      ("o" evil-open-fold)
      ("O" evil-open-fold-rec)
      ("r" evil-open-folds)
      ("m" evil-close-folds)
      ("q" nil :exit t)
      ("C-g" nil :exit t)
      ("<SPC>" nil :exit t)))
  (spacemacs/set-leader-keys "z." 'spacemacs/fold-transient-state/body)

  ;; define text objects
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
    (list (point-min) (point-max)))
  (define-key evil-inner-text-objects-map "g" 'evil-inner-buffer)

  ;; turn off evil in corelv buffers
  (push '("\\*LV\\*") evil-buffer-regexps)

  ;; replace `dired-goto-file' with `helm-find-files', since `helm-find-files'
  ;; can do the same thing and with fuzzy matching and other features.
  (with-eval-after-load 'dired
    (evil-define-key 'normal dired-mode-map "J" 'spacemacs/helm-find-files)
    (define-key dired-mode-map "j" 'spacemacs/helm-find-files)
    (evil-define-key 'normal dired-mode-map (kbd dotspacemacs-leader-key)
      spacemacs-default-map))

  ;; support smart 1parens-strict-mode
  (when (configuration-layer/package-usedp 'smartparens)
    (defadvice evil-delete-backward-char-and-join
        (around spacemacs/evil-delete-backward-char-and-join activate)
      (if (bound-and-true-p smartparens-strict-mode)
          (call-interactively 'sp-backward-delete-char)
        ad-do-it)))

  ;; Define history commands for comint
  (when (eq dotspacemacs-editing-style 'vim)
    (evil-define-key 'insert comint-mode-map
      (kbd "C-k") 'comint-previous-input
      (kbd "C-j") 'comint-next-input))
  (evil-define-key 'normal comint-mode-map
    (kbd "C-k") 'comint-previous-input
    (kbd "C-j") 'comint-next-input))

(defun spacemacs-bootstrap/init-hydra ()
  (require 'hydra)
  (setq hydra-key-doc-function 'spacemacs//hydra-key-doc-function
        hydra-head-format "[%s] "))

(defun spacemacs-bootstrap/init-use-package ()
  (require 'use-package)
  (setq use-package-verbose init-file-debug
        ;; inject use-package hooks for easy customization of stock package
        ;; configuration
        use-package-inject-hooks t))

(defun spacemacs-bootstrap/init-which-key ()
  (require 'which-key)

  (spacemacs|add-toggle which-key
    :mode which-key-mode
    :documentation
    "Display a buffer with available key bindings."
    :evil-leader "tK")

  (spacemacs/set-leader-keys "hk" 'which-key-show-top-level)

  ;; Replace rules for better naming of functions
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
  (spacemacs|diminish which-key-mode " Ⓚ" " K"))
