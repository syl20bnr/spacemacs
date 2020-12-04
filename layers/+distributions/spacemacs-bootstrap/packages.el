;;; packages.el --- Mandatory Bootstrap Layer packages File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq space-macs-bootstrap-packages
      '(
        ;; bootstrap packages,
        ;; `use-package' cannot be used for bootstrap packages configuration
        (async :step bootstrap)
        (bind-map :step bootstrap)
        (bind-key :step bootstrap)
        (diminish :step bootstrap)
        (evil :step bootstrap)
        (hydra :step bootstrap)
        (use-package :step bootstrap)
        (which-key :step bootstrap)
        ;; pre packages, initialized aftert the bootstrap packages
        ;; these packages can use use-package
        (dotenv-mode :step pre)
        (evil-evilified-state :location local :step pre :protected t)
        (pcre2el :step pre)
        (holy-mode :location local :step pre)
        (hybrid-mode :location (recipe :fetcher local) :step pre)
        (space-macs-theme :location built-in)
        ))


;; bootstrap packages

(defun space-macs-bootstrap/init-async ())

(defun space-macs-bootstrap/init-bind-key ())

(defun space-macs-bootstrap/init-diminish ()
  (when (not (configuration-layer/package-used-p 'spaceline))
    (add-hook 'after-load-functions 'space-macs/diminish-hook)))

(defun space-macs-bootstrap/init-dotenv-mode ()
  (use-package dotenv-mode
    :defer t))

(defun space-macs-bootstrap/init-bind-map ()
  (require 'bind-map)
  (bind-map space-macs-default-map
    :prefix-cmd space-macs-cmds
    :keys (dotspace-macs-e-macs-leader-key)
    :evil-keys (dotspace-macs-leader-key)
    :override-minor-modes t
    :override-mode-name space-macs-leader-override-mode))

(defun space-macs-bootstrap/init-evil ()
  ;; ensure that the search module is set at startup
  ;; must be called before evil is required to really take effect.
  (space-macs/set-evil-search-module dotspace-macs-editing-style)
  (add-hook 'space-macs-editing-style-hook 'space-macs/set-evil-search-module)

  ;; evil-mode is mandatory for Space-macs to work properly
  ;; evil must be require explicitly, the autoload seems to not
  ;; work properly sometimes.
  (require 'evil)
  (evil-mode 1)

  (when (fboundp 'evil-set-undo-system)
    (evil-set-undo-system 'undo-tree))

  ;; Use evil as a default jump handler
  (add-to-list 'space-macs-default-jump-handlers 'evil-goto-definition)

  (require 'cl-lib)
  ;; State cursors
  (cl-loop for (state color shape) in space-macs-evil-cursors
           do (space-macs/add-evil-cursor state color shape))
  (add-hook 'space-macs-post-theme-change-hook 'space-macs/set-state-faces)

  ;; evil ex-command
  (define-key evil-normal-state-map (kbd dotspace-macs-ex-command-key) 'evil-ex)
  (define-key evil-visual-state-map (kbd dotspace-macs-ex-command-key) 'evil-ex)
  (define-key evil-motion-state-map (kbd dotspace-macs-ex-command-key) 'evil-ex)
  (setq evil-ex-substitute-global vim-style-ex-substitute-global)

  ;; evil-want-Y-yank-to-eol must be set via customize to have an effect
  (customize-set-variable 'evil-want-Y-yank-to-eol vim-style-remap-Y-to-y$)

  ;; bind evil-jump-forward for GUI only.
  (define-key evil-motion-state-map [C-i] 'evil-jump-forward)

  ;; Make the current definition and/or comment visible.
  (define-key evil-normal-state-map "zf" 'reposition-window)
  ;; Make set-selective-display more discoverable to Evil folks
  (define-key evil-normal-state-map "z$" 'space-macs/toggle-selective-display)
  ;; toggle maximize buffer
  (define-key evil-window-map (kbd "o") 'space-macs/toggle-maximize-buffer)
  (define-key evil-window-map (kbd "C-o") 'space-macs/toggle-maximize-buffer)
  ;; make cursor keys work
  (define-key evil-window-map (kbd "<left>") 'evil-window-left)
  (define-key evil-window-map (kbd "<right>") 'evil-window-right)
  (define-key evil-window-map (kbd "<up>") 'evil-window-up)
  (define-key evil-window-map (kbd "<down>") 'evil-window-down)
  (space-macs/set-leader-keys
    "re" 'evil-show-registers
    "sc" 'space-macs/evil-search-clear-highlight)
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
  (add-hook 'after-change-major-mode-hook 'space-macs//set-evil-shift-width 'append)

  ;; Keep the region active when shifting
  (when vim-style-retain-visual-state-on-shift
    (evil-map visual "<" "<gv")
    (evil-map visual ">" ">gv"))

  ;; move selection up and down
  (when vim-style-visual-line-move-text
    (define-key evil-visual-state-map "J" (concat ":m '>+1" (kbd "RET") "gv=gv"))
    (define-key evil-visual-state-map "K" (concat ":m '<-2" (kbd "RET") "gv=gv")))

  (evil-ex-define-cmd "enew" 'space-macs/new-empty-buffer)

  (define-key evil-normal-state-map (kbd "K") 'space-macs/evil-smart-doc-lookup)
  (define-key evil-normal-state-map (kbd "gd") 'space-macs/jump-to-definition)
  (define-key evil-normal-state-map (kbd "gD") 'space-macs/jump-to-definition-other-window)

  ;; scrolling transient state
  (space-macs|transient-state-format-hint scroll
    space-macs--scroll-ts-full-hint
    (format "\n[_?_] toggle help
 Line/Column^^^^      Half Page^^^^        Full Page^^ Buffer^^^^    Other
 â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€^^^^â”€â”€â”€â”€â”€ â”€â”€â”€â”€â”€â”€â”€â”€â”€^^^^â”€â”€â”€â”€â”€â”€â”€ â”€â”€â”€â”€â”€â”€â”€â”€â”€^^ â”€â”€â”€â”€â”€â”€^^^^â”€â”€â”€ â”€â”€â”€â”€â”€^^â”€â”€â”€
 [_k_]^^   up         [_u_/_K_] up         [_b_] up    [_<_/_g_] beg [_q_] quit
 [_j_]^^   down       [_d_/_J_] down       [_f_] down  [_>_/_G_] end
 [_h_/_l_] left/right [_H_/_L_] left/right"))
  (space-macs|define-transient-state scroll
    :title "Scrolling Transient State"
    :hint-is-doc t
    :dynamic-hint (space-macs//scroll-ts-hint)
    :bindings
    ("?" space-macs//scroll-ts-toggle-hint)
    ;; lines and columns
    ("j" evil-scroll-line-down)
    ("k" evil-scroll-line-up)
    ("h" evil-scroll-column-left)
    ("l" evil-scroll-column-right)
    ;; half page
    ("d" evil-scroll-down)
    ("u" evil-scroll-up)
    ("J" evil-scroll-down)
    ("K" evil-scroll-up)
    ("H" evil-scroll-left)
    ("L" evil-scroll-right)
    ;; full page
    ("f" evil-scroll-page-down)
    ("b" evil-scroll-page-up)
    ;; buffer
    ("<" evil-goto-first-line)
    (">" evil-goto-line)
    ("g" evil-goto-first-line)
    ("G" evil-goto-line)
    ;; other
    ("q" nil :exit t))
  (space-macs/set-leader-keys
    ;; lines and columns
    "Nj" 'space-macs/scroll-transient-state/evil-scroll-line-down
    "Nk" 'space-macs/scroll-transient-state/evil-scroll-line-up
    "Nh" 'space-macs/scroll-transient-state/evil-scroll-column-left
    "Nl" 'space-macs/scroll-transient-state/evil-scroll-column-right
    ;; half page
    "Nd" 'space-macs/scroll-transient-state/evil-scroll-down
    "Nu" 'space-macs/scroll-transient-state/evil-scroll-up
    "NJ" 'space-macs/scroll-transient-state/evil-scroll-down
    "NK" 'space-macs/scroll-transient-state/evil-scroll-up
    "NH" 'space-macs/scroll-transient-state/evil-scroll-left
    "NL" 'space-macs/scroll-transient-state/evil-scroll-right
    ;; full page
    "Nf" 'space-macs/scroll-transient-state/evil-scroll-page-down
    "Nb" 'space-macs/scroll-transient-state/evil-scroll-page-up
    ;; buffer
    "N<" 'space-macs/scroll-transient-state/evil-goto-first-line
    "N>" 'space-macs/scroll-transient-state/evil-goto-line
    "Ng" 'space-macs/scroll-transient-state/evil-goto-first-line
    "NG" 'space-macs/scroll-transient-state/evil-goto-line)

  ;; pasting transient-state
  (evil-define-command space-macs//transient-state-0 ()
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

  (space-macs|define-transient-state paste
    :title "Pasting Transient State"
    :doc "\n[%s(length kill-ring-yank-pointer)/%s(length kill-ring)] \
 [_C-j_/_C-k_] cycles through yanked text, [_p_/_P_] pastes the same text \
 above or below. Anything else exits."
    :bindings
    ("C-j" evil-paste-pop)
    ("C-k" evil-paste-pop-next)
    ("p" evil-paste-after)
    ("P" evil-paste-before)
    ("0" space-macs//transient-state-0))

  (when dotspace-macs-enable-paste-transient-state
    (define-key evil-normal-state-map
      "p" 'space-macs/paste-transient-state/evil-paste-after)
    (define-key evil-normal-state-map
      "P" 'space-macs/paste-transient-state/evil-paste-before))
  ;; fold transient state
  (when (eq 'evil dotspace-macs-folding-method)
    (space-macs|define-transient-state fold
      :title "Code Fold Transient State"
      :doc "
 Close^^          Open^^              Toggle^^             Other^^
 â”€â”€â”€â”€â”€â”€â”€^^â”€â”€â”€â”€â”€â”€  â”€â”€â”€â”€â”€^^â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€  â”€â”€â”€â”€â”€^^â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€  â”€â”€â”€â”€â”€^^â”€â”€â”€
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
  (space-macs/set-leader-keys "z." 'space-macs/fold-transient-state/body)

  ;; define text objects
  (space-macs|define-text-object "$" "dollar" "$" "$")
  (space-macs|define-text-object "*" "star" "*" "*")
  (space-macs|define-text-object "8" "block-star" "/*" "*/")
  (space-macs|define-text-object "|" "bar" "|" "|")
  (space-macs|define-text-object "%" "percent" "%" "%")
  (space-macs|define-text-object "/" "slash" "/" "/")
  (space-macs|define-text-object "_" "underscore" "_" "_")
  (space-macs|define-text-object "-" "hyphen" "-" "-")
  (space-macs|define-text-object "~" "tilde" "~" "~")
  (space-macs|define-text-object "=" "equal" "=" "=")
  (space-macs|define-text-object "Â«" "double-angle-bracket" "Â«" "Â»")
  (space-macs|define-text-object "ï½¢" "corner-bracket" "ï½¢" "ï½£")
  (space-macs|define-text-object "â€˜" "single-quotation-mark" "â€˜" "â€™")
  (space-macs|define-text-object "â€œ" "double-quotation-mark" "â€œ" "â€")
  (evil-define-text-object evil-pasted (count &rest args)
    (list (save-excursion (evil-goto-mark ?\[) (point))
          (save-excursion (evil-goto-mark ?\]) (1+ (point)))))
  (define-key evil-inner-text-objects-map "P" 'evil-pasted)
  ;; define text-object for entire buffer
  (evil-define-text-object evil-inner-buffer (count &optional beg end type)
    (list (point-min) (point-max)))
  (define-key evil-inner-text-objects-map "g" 'evil-inner-buffer)

  ;; turn off evil in corelv buffers
  (add-to-list 'evil-buffer-regexps '("\\*LV\\*"))

  ;; replace `dired-goto-file' with `helm-find-files', since `helm-find-files'
  ;; can do the same thing and with fuzzy matching and other features.
  (with-eval-after-load 'dired
    (evil-define-key 'normal dired-mode-map "J" 'space-macs/helm-find-files)
    (define-key dired-mode-map "j" 'space-macs/helm-find-files)
    (evil-define-key 'normal dired-mode-map (kbd dotspace-macs-leader-key)
      space-macs-default-map))

  ;; support smart 1parens-strict-mode
  (when (configuration-layer/package-used-p 'smartparens)
    (defadvice evil-delete-backward-char-and-join
        (around space-macs/evil-delete-backward-char-and-join activate)
      (if (bound-and-true-p smartparens-strict-mode)
          (call-interactively 'sp-backward-delete-char)
        ad-do-it)))

  ;; Define history commands for comint
  (when (eq dotspace-macs-editing-style 'vim)
    (evil-define-key 'insert comint-mode-map
      (kbd "C-k") 'comint-previous-input
      (kbd "C-j") 'comint-next-input))
  (evil-define-key 'normal comint-mode-map
    (kbd "C-k") 'comint-previous-input
    (kbd "C-j") 'comint-next-input)

  ;; ignore repeat
  (evil-declare-ignore-repeat 'space-macs/next-error)
  (evil-declare-ignore-repeat 'space-macs/previous-error))

(defun space-macs-bootstrap/init-hydra ()
  (require 'hydra)
  (setq hydra-key-doc-function 'space-macs//hydra-key-doc-function
        hydra-head-format "[%s] "))

(defun space-macs-bootstrap/init-use-package ()
  (require 'use-package)
  (setq use-package-verbose init-file-debug
        ;; inject use-package hooks for easy customization of stock package
        ;; configuration
        use-package-inject-hooks t)
  (add-to-list 'use-package-keywords :spacebind t))

(defun space-macs-bootstrap/init-which-key ()
  (require 'which-key)

  (setq which-key-add-column-padding 1
        which-key-allow-multiple-replacements t
        which-key-echo-keystrokes 0.02
        which-key-idle-delay dotspace-macs-which-key-delay
        which-key-idle-secondary-delay 0.01
        which-key-max-description-length 32
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-prevent-C-h-from-cycling t
        which-key-sort-order 'which-key-prefix-then-key-order
        which-key-sort-uppercase-first nil
        which-key-special-keys nil
        which-key-use-C-h-for-paging t
        which-key-allow-evil-operators t)

  (space-macs|add-toggle which-key
    :mode which-key-mode
    :documentation
    "Display a buffer with available key bindings."
    :evil-leader "tK")

  (space-macs/set-leader-keys "hk" 'which-key-show-top-level)

  ;; Needed to avoid nil variable error before update to recent which-key
  (defvar which-key-replacement-alist nil)
  ;; Reset to the default or customized value before adding our values in order
  ;; to make this initialization code idempotent.
  (custom-reevaluate-setting 'which-key-replacement-alist)
  ;; Replace rules for better naming of functions
  (let ((new-descriptions
         ;; being higher in this list means the replacement is applied later
         '(
           ("space-macs/\\(.+\\)" . "\\1")
           ("space-macs//\\(.+\\)" . "\\1")
           ("space-macs-\\(.+\\)" . "\\1")
           ("space-macs/toggle-\\(.+\\)" . "\\1")
           ("\\(.+\\)-transient-state/\\(.+\\)" . "\\2")
           ("\\(.+\\)-transient-state/body" . "\\1-transient-state")
           ("space-macs-layouts/non-restricted-buffer-list-\\(helm\\|ivy\\)" . "global-list-buffers")
           ("space-macs/toggle-mode-line-\\(.+\\)" . "\\1")
           ("evil-lisp-state-\\(.+\\)" . "\\1")
           ("helm-mini\\|ivy-switch-buffer" . "list-buffers")
           ("lazy-helm/\\(.+\\)" . "\\1")
           ("lazy-helm/space-macs/\\(.+\\)" . "\\1")
           )))
    (dolist (nd new-descriptions)
      ;; ensure the target matches the whole string
      (push (cons (cons nil (concat "\\`" (car nd) "\\'")) (cons nil (cdr nd)))
            which-key-replacement-alist)))

  ;; Group together sequence and identical key entries in the which-key popup
  ;; SPC h k- Top-level bindings
  ;; Remove spaces around the two dots ".."
  (push '(("\\(.*\\)1 .. 9" . "digit-argument") .
          ("\\11..9" . "digit-argument"))
        which-key-replacement-alist)

  ;; And remove the modifier key(s) before the last nr in the sequence
  (push '(("\\(.*\\)C-0 .. C-5" . "digit-argument") .
          ("\\1C-0..5" . "digit-argument"))
        which-key-replacement-alist)

  (push '(("\\(.*\\)C-7 .. C-9" . "digit-argument") .
          ("\\1C-7..9" . "digit-argument"))
        which-key-replacement-alist)

  (push '(("\\(.*\\)C-M-0 .. C-M-9" . "digit-argument") .
          ("\\1C-M-0..9" . "digit-argument"))
        which-key-replacement-alist)

  ;; Rename the entry for M-0 in the SPC h k Top-level bindings,
  ;; and for 0 in the SPC- Space-macs root
  (push '(("\\(.*\\)0" . "winum-select-window-0-or-10") .
          ("\\10" . "select window 0 or 10"))
        which-key-replacement-alist)

  ;; Rename the entry for M-1 in the SPC h k Top-level bindings,
  ;; and for 1 in the SPC- Space-macs root, to 1..9
  (push '(("\\(.*\\)1" . "winum-select-window-1") .
          ("\\11..9" . "select window 1..9"))
        which-key-replacement-alist)

  ;; Hide the entries for M-[2-9] in the SPC h k Top-level bindings,
  ;; and for [2-9] in the SPC- Space-macs root
  (push '((nil . "winum-select-window-[2-9]") . t)
        which-key-replacement-alist)

  ;; SPC- Space-macs root
  ;; Combine the ` (backtick) and Â² (superscript 2) key entries
  (push '(("\\(.*\\)`" . "winum-select-window-by-number") .
          ("\\1`,Â²" . "select window by number"))
        which-key-replacement-alist)

  ;; hide the "Â² -> winum-select-window-by-number" entry
  (push '(("\\(.*\\)Â²" . nil) . t)
        which-key-replacement-alist)

  ;; SPC b- buffers
  ;; rename the buffer-to-window-1 entry, to 1..9
  (push '(("\\(.*\\)1" . "Move buffer to window 1") .
          ("\\11..9" . "Move buffer to window 1..9"))
        which-key-replacement-alist)

  ;; hide the "[2-9] -> buffer-to-window-[2-9]" entries
  (push '((nil . "Move buffer to window [2-9]") . t)
        which-key-replacement-alist)

  ;; SPC k- lisp
  ;; rename "1 .. 9 -> digit-argument" to "1..9 -> digit-argument"
  (push '(("\\(.*\\)1 .. 9" . "evil-lisp-state-digit-argument") .
          ("\\11..9" . "digit-argument"))
        which-key-replacement-alist)

  ;; SPC n- narrow/numbers
  ;; Combine + and =
  (push '(("\\(.*\\)+" . "evil-numbers/inc-at-pt") .
          ("\\1+,=" . "evil-numbers/inc-at-pt"))
        which-key-replacement-alist)

  ;; hide "= -> evil-numbers/inc-at-pt" entry
  (push '(("\\(.*\\)=" . "evil-numbers/inc-at-pt") . t)
        which-key-replacement-alist)

  ;; Combine - and _
  (push '(("\\(.*\\)-" . "evil-numbers/dec-at-pt") .
          ("\\1-,_" . "evil-numbers/dec-at-pt"))
        which-key-replacement-alist)

  ;; hide "_ -> evil-numbers/dec-at-pt" entry
  (push '(("\\(.*\\)_" . "evil-numbers/dec-at-pt") . t)
        which-key-replacement-alist)

  ;; SPC x i- inflection
  ;; rename "k -> string-inflection-kebab-case"
  ;; to "k,- -> string-inflection-kebab-case"
  (push '(("\\(.*\\)k" . "string-inflection-kebab-case") .
          ("\\1k,-" . "string-inflection-kebab-case"))
        which-key-replacement-alist)

  ;; hide the "- -> string-inflection-kebab-case" entry
  (push '(("\\(.*\\)-" . "string-inflection-kebab-case") . t)
        which-key-replacement-alist)

  ;; rename "u -> string-inflection-underscore"
  ;; to "u,_ -> string-inflection-underscore"
  (push '(("\\(.*\\)u" . "string-inflection-underscore") .
          ("\\1u,_" . "string-inflection-underscore"))
        which-key-replacement-alist)

  ;; hide the "_ -> string-inflection-underscore" entry
  (push '(("\\(.*\\)_" . "string-inflection-underscore") . t)
        which-key-replacement-alist)

  ;; C-c C-w-
  ;; rename the eyebrowse-switch-to-window-config-0 entry, to 0..9
  (push '(("\\(.*\\)0" . "eyebrowse-switch-to-window-config-0") .
          ("\\10..9" . "eyebrowse-switch-to-window-config-0..9"))
        which-key-replacement-alist)

  ;; hide the "[1-9] -> eyebrowse-switch-to-window-config-[1-9]" entries
  (push '((nil . "eyebrowse-switch-to-window-config-[1-9]") . t)
        which-key-replacement-alist)

  ;; Combine the c and C-c key entries
  (push '(("\\(.*\\)C-c C-w c" . "eyebrowse-create-window-config") .
          ("\\1c,C-c" . "eyebrowse-create-window-config"))
        which-key-replacement-alist)

  ;; hide the "C-c -> eyebrowse-create-window-config" entry
  (push '(("\\(.*\\)C-c C-w C-c" . "eyebrowse-create-window-config") . t)
          which-key-replacement-alist)

  ;; C-c C-d-
  ;; Combine the d and C-d key entries
  (push '(("\\(.*\\)C-c C-d d" . "elisp-slime-nav-describe-elisp-thing-at-point") .
          ("\\1d,C-d" . "elisp-slime-nav-describe-elisp-thing-at-point"))
        which-key-replacement-alist)

  ;; hide the "C-d -> elisp-slime-nav-describe-elisp-thing-at-point" entry
  (push '(("\\(.*\\)C-c C-d C-d" . "elisp-slime-nav-describe-elisp-thing-at-point") . t)
          which-key-replacement-alist)

  (which-key-add-key-based-replacements
    dotspace-macs-leader-key '("root" . "Space-macs root")
    dotspace-macs-e-macs-leader-key '("root" . "Space-macs root"))

  ;; disable special key handling for space-macs, since it can be
  ;; disorienting if you don't understand it
  (pcase dotspace-macs-which-key-position
    (`right (which-key-setup-side-window-right))
    (`bottom (which-key-setup-side-window-bottom))
    (`right-then-bottom (which-key-setup-side-window-right-bottom)))

  (which-key-mode)
  (space-macs|diminish which-key-mode " â“€" " K"))

;; pre packages

(defun space-macs-bootstrap/init-evil-evilified-state ()
  (use-package evil-evilified-state)
  (define-key evil-evilified-state-map (kbd dotspace-macs-leader-key)
    space-macs-default-map))

;; we own pcre2el here, so that it's always available to ivy and helm
;; (necessary when using space-macs-base distribution)
(defun space-macs-bootstrap/init-pcre2el ()
  (use-package pcre2el :defer t))

(defun space-macs-bootstrap/init-holy-mode ()
  (space-macs|unless-dumping-and-eval-after-loaded-dump holy-mode
    (use-package holy-mode
      :commands holy-mode
      :init
      (progn
        (when (eq 'e-macs dotspace-macs-editing-style)
          (holy-mode))
        (space-macs|add-toggle holy-mode
          :status holy-mode
          :on (progn (when (bound-and-true-p hybrid-mode)
                       (hybrid-mode -1))
                     (holy-mode))
          :off (holy-mode -1)
          :documentation "Globally toggle holy mode."
          :evil-leader "tEe")
        (space-macs|diminish holy-mode " â’ºe" " Ee")))))

(defun space-macs-bootstrap/init-hybrid-mode ()
  (space-macs|unless-dumping-and-eval-after-loaded-dump hybrid-mode
    (use-package hybrid-mode
      :config
      (progn
        (when (eq 'hybrid dotspace-macs-editing-style) (hybrid-mode))
        (space-macs|add-toggle hybrid-mode
          :status hybrid-mode
          :on (progn (when (bound-and-true-p holy-mode)
                       (holy-mode -1))
                     (hybrid-mode))
          :off (hybrid-mode -1)
          :documentation "Globally toggle hybrid mode."
          :evil-leader "tEh")
        (space-macs|diminish hybrid-mode " â’ºh" " Eh")))))

(defun space-macs-bootstrap/init-space-macs-theme ()
  (use-package space-macs-theme
    :defer t))


