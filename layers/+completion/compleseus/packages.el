;;; packages.el --- compleseus layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defconst compleseus-packages
  '(auto-highlight-symbol
    imenu
    marginalia
    ;; (compleseus-spacemacs-help :location local)
    (compleseus-spacemacs-help
     :location (recipe :fetcher local))
    consult
    consult-yasnippet
    embark
    embark-consult
    orderless
    persp-mode
    (selectrum :toggle (eq compleseus-engine 'selectrum))
    (vertico
     :toggle (eq compleseus-engine 'vertico)
     :location elpa)
    (grep :location built-in)
    wgrep))

(defun compleseus/pre-init-auto-highlight-symbol ()
  (spacemacs|use-package-add-hook auto-highlight-symbol
    :post-init
    ;; add some functions to ahs transient states
    (setq spacemacs--symbol-highlight-transient-state-doc
          (concat
           spacemacs--symbol-highlight-transient-state-doc
           "  Search: [_s_] consult-line  [_f_] files  [_/_] project"))
    (spacemacs/transient-state-register-add-bindings 'symbol-highlight
      '(("s" spacemacs/consult-line :exit t)
        ("f" spacemacs/compleseus-search-auto :exit t)
        ("/" spacemacs/compleseus-search-projectile-auto :exit t)))))

(defun compleseus/post-init-imenu ()
  (spacemacs/set-leader-keys "ji" 'spacemacs/consult-jump-in-buffer)
  (spacemacs/set-leader-keys "sj" 'spacemacs/consult-jump-in-buffer))

(defun compleseus/init-marginalia ()
  (use-package marginalia
    ;; Either bind `marginalia-cycle` globally or only in the minibuffer
    :bind (("M-A" . marginalia-cycle)
           :map minibuffer-local-map
           ("M-A" . marginalia-cycle))

    :config
    (dolist (it
             '((spacemacs/compleseus-pers-switch-project . project-file)
               ;; https://github.com/bbatsov/projectile/issues/1664
               ;; https://github.com/minad/marginalia/issues/110
               (persp-switch-to-buffer . buffer)
               (projectile-find-file . project-file)
               (projectile-find-dir . project-file)
               (projectile-recentf . project-file)
               (projectile-switch-to-buffer . buffer)
               (projectile-switch-project . project-file)))
      (push it marginalia-command-categories))
    (setq marginalia-align 'right)
    ;; The :init configuration is always executed (Not lazy!)
    :init
    ;; Must be in the :init section of use-package such that the mode gets
    ;; enabled right away. Note that this forces loading the package.
    (marginalia-mode)))

(defun compleseus/init-consult ()
  (use-package consult
    ;; Replace bindings. Lazily loaded due by `use-package'.
    :bind (;; C-c bindings (mode-specific-map)
           ("C-c h" . consult-history)
           ("C-c m" . consult-mode-command)
           ("C-c b" . consult-bookmark)
           ("C-c k" . consult-kmacro)
           ;; C-x bindings (ctl-x-map)
           ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
           ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
           ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
           ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
           ;; Custom M-# bindings for fast register access
           ("M-#" . consult-register-load)
           ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
           ("C-M-#" . consult-register)
           ;; Other custom bindings
           ("M-y" . consult-yank-pop)                ;; orig. yank-pop
           ;; M-g bindings (goto-map)
           ("M-g e" . consult-compile-error)
           ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
           ("M-g g" . consult-goto-line)             ;; orig. goto-line
           ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
           ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
           ("M-g m" . consult-mark)
           ("M-g k" . consult-global-mark)
           ("M-g i" . consult-imenu)
           ("M-g I" . consult-imenu-multi)
           ;; M-s bindings (search-map)
           ("M-s f" . consult-find)
           ("M-s L" . consult-locate)
           ("M-s g" . consult-grep)
           ("M-s G" . consult-git-grep)
           ("M-s r" . consult-ripgrep)
           ("M-s l" . consult-line)
           ("M-s m" . consult-line-multi)
           ("M-s k" . consult-keep-lines)
           ("M-s u" . consult-focus-lines)
           ;; Isearch integration
           ("M-s e" . consult-isearch-history)
           :map isearch-mode-map
           ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
           ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
           ("M-s l" . consult-line))                 ;; needed by consult-line to detect isearch

    ;; Enable automatic preview at point in the *Completions* buffer.
    ;; This is relevant when you use the default completion UI,
    ;; and not necessary for Selectrum, Vertico etc.
    :hook (completion-list-mode . consult-preview-at-point-mode)

    ;; The :init configuration is always executed (Not lazy)
    :init
    (define-key read-expression-map (kbd "C-r") #'consult-history)
    (spacemacs/set-leader-keys
      dotspacemacs-emacs-command-key 'execute-extended-command
      "#" #'consult-register
      "*" #'spacemacs/compleseus-search-default
      "/" #'spacemacs/compleseus-search-projectile-auto
      "bb" #'spacemacs/compleseus-switch-to-buffer
      "bB" #'consult-buffer
      "fb" #'consult-bookmark
      "ff" #'spacemacs/compleseus-find-file
      "fL" #'consult-locate
      "fr" #'consult-recent-file
      "hda" #'apropos-command
      "hdm" #'describe-mode
      "jm" #'consult-mark
      "jM" #'consult-global-mark
      "sb" #'consult-line-multi
      "sB" #'spacemacs/consult-line-multi
      "ss" #'consult-line
      "sS" #'spacemacs/consult-line
      "sk" #'consult-keep-lines
      "rc" #'consult-complex-command
      "su" #'consult-focus-lines
      "sf" #'spacemacs/compleseus-search-auto
      "sd" #'spacemacs/compleseus-search-dir
      "sp" #'spacemacs/compleseus-search-projectile
      "ry" #'consult-yank-from-kill-ring
      "Ts" #'consult-theme)

    ;; Optionally configure the register formatting. This improves the register
    ;; preview for `consult-register', `consult-register-load',
    ;; `consult-register-store' and the Emacs built-ins.
    (setq register-preview-delay 0
          register-preview-function #'consult-register-format)

    ;; Optionally tweak the register preview window.
    ;; This adds thin lines, sorting and hides the mode line of the window.
    (advice-add #'register-preview :override #'consult-register-window)

    ;; Use Consult to select xref locations with preview
    (setq xref-prompt-for-identifier '(not xref-find-definitions
                                           xref-find-definitions-other-window
                                           xref-find-definitions-other-frame
                                           xref-find-references
                                           spacemacs/jump-to-definition))
    (setq xref-show-xrefs-function #'consult-xref)

    ;; Configure other variables and modes in the :config section,
    ;; after lazily loading the package.
    :config

    ;; disable automatic preview by default,
    ;; selectively enable it for some prompts below.
    (setq consult-preview-key '("M-." "C-SPC"))

    ;; customize preview activation and delay while selecting candiates
    (consult-customize
     consult-theme
     :preview-key '("M-." "C-SPC"
                    :debounce 0.2 any)

     ;; slightly delayed preview upon candidate selection
     ;; one usually wants quick feedback
     consult-buffer
     consult-ripgrep
     consult-git-grep
     consult-grep
     consult-bookmark
     consult-yank-pop
     :preview-key '("M-." "C-SPC"
                    :debounce 0.3 "<up>" "<down>" "C-n" "C-p"
                    :debounce 0.6 any))

    ;; hide magit buffer
    (add-to-list 'consult-buffer-filter "magit.*:.*")

    (setq consult-line-start-from-top nil)

    ;; Optionally configure the narrowing key.
    ;; Both < and C-+ work reasonably well.
    (setq consult-narrow-key "<") ;; (kbd "C-+")

    ;; Optionally make narrowing help available in the minibuffer.
    ;; You may want to use `embark-prefix-help-command' or which-key instead.
    ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

    ;; Make M-n as smart as ivy and helm equivalents
    (setq minibuffer-default-add-function 'spacemacs/minibuffer-default-add-function)

    ;; Optionally configure a function which returns the project root directory.
    (setq consult-project-root-function
          (lambda ()
            (when-let (project (project-current))
              (car (project-root project))))))

  ;; Configure consult-imenu for java-mode.
  (use-package consult-imenu
    :after consult
    :config
    (add-to-list 'consult-imenu-config '(java-mode :toplevel "Classes" :types
                                                   ((?m "Methods" font-lock-function-name-face)
                                                    (?f "Fields" font-lock-variable-name-face)
                                                    (?c "Classes" font-lock-type-face)
                                                    (?p "Packages" font-lock-constant-face)
                                                    (?C "Constants" font-lock-constant-face)
                                                    (?M "Constructors" font-lock-function-name-face)
                                                    (?e "Enums" font-lock-type-face)
                                                    (?E "Enum Members" font-lock-constant-face)
                                                    (?i "Interfaces" font-lock-type-face))))))

(defun compleseus/init-consult-yasnippet ()
  (use-package consult-yasnippet
    :defer t
    :init
    (spacemacs/set-leader-keys
      "is" 'consult-yasnippet)))

(defun compleseus/init-embark ()
  (use-package embark
    :bind
    (("M-o" . embark-act)         ;; pick some comfortable binding
     ("C-;" . embark-dwim)        ;; good alternative: M-.
     ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
    :init
    (spacemacs/set-leader-keys "?" #'embark-bindings)
    ;; this gets you the available-key preview minibuffer popup
    (setq prefix-help-command #'embark-prefix-help-command
          ;; don't use C-h for paging, instead `describe-prefix-bindings`.
          which-key-use-C-h-commands nil)
    ;; same key binding as ivy-occur
    (define-key minibuffer-local-map (kbd "C-c C-o") #'embark-export)
    (define-key minibuffer-local-map (kbd "C-c C-l") #'embark-collect)
    ;; mimic action key bindings from helm
    (define-key minibuffer-local-map (kbd "C-z") #'spacemacs/embark-action-completing-read)
    (define-key minibuffer-local-map (kbd "C-c C-e") #'spacemacs/consult-edit)
    ;; which keys nice display
    (which-key-add-keymap-based-replacements minibuffer-local-map "C-c C-o" "Embark export")
    (which-key-add-keymap-based-replacements minibuffer-local-map "C-c C-l" "Embark collect")
    (which-key-add-keymap-based-replacements minibuffer-local-map "C-c C-e" "Edit buffer")
    (which-key-add-keymap-based-replacements minibuffer-local-map "C-z" "Embark actions...")
    :config
    (define-key embark-file-map "s" 'spacemacs/compleseus-search-from)
    ;; which key integration setup
    ;; https://github.com/oantolin/embark/wiki/Additional-Configuration#use-which-key-like-a-key-menu-prompt
    (setq embark-indicators
          '(spacemacs/embark-which-key-indicator
            embark-highlight-indicator
            embark-isearch-highlight-indicator))
    (advice-add #'embark-completing-read-prompter
                :around #'spacemacs/embark-hide-which-key-indicator)))

(defun compleseus/init-embark-consult ()
  (use-package embark-consult
    :ensure t
    :after (embark consult)
    :demand t ; only necessary if you have the hook below
    ;; if you want to have consult previews as you move around an
    ;; auto-updating embark collect buffer
    :hook
    (embark-collect-mode . consult-preview-at-point-mode)))

(defun compleseus/init-orderless ()
  (use-package orderless
    :init
    ;; company-capf is messed up with orderless
    ;; https://github.com/oantolin/orderless/issues/48#issuecomment-856750410
    (define-advice company-capf (:around (orig-fun &rest args) set-completion-styles)
      ;; when lsp is on stay away
      (if (bound-and-true-p lsp-completion-mode)
          (apply orig-fun args)
        (let ((completion-styles '(basic partial-completion orderless)))
          (apply orig-fun args))))

    (setq orderless-component-separator "[ &]")

    ;; should be all in with orderless otherwise the results are inconsistent.
    ;; the available styles are registered in `completion-styles-alist`.
    (setq completion-styles '(orderless basic)
          completion-category-defaults nil
          ;; we need to have 'basic here first in order to support tramp connections...
          ;; see `completion-styles`.
          completion-category-overrides '((file (styles basic partial-completion))))))

(defun compleseus/init-selectrum ()
  (use-package selectrum
    :init
    ;; Disable ido. We want to use the regular find-file etc.; enhanced by selectrum
    (setq ido-mode nil)

    (selectrum-mode)
    (spacemacs/set-leader-keys
      "rl" 'selectrum-repeat
      "sl" 'selectrum-repeat)

    :config
    (when (spacemacs//support-hjkl-navigation-p)
      (define-key selectrum-minibuffer-map (kbd "C-j") 'selectrum-next-candidate)
      (define-key selectrum-minibuffer-map (kbd "C-r") 'consult-history)
      (define-key selectrum-minibuffer-map (kbd "C-k") 'selectrum-previous-candidate)
      (define-key selectrum-minibuffer-map (kbd "C-M-k") #'spacemacs/selectrum-previous-candidate-preview)
      (define-key selectrum-minibuffer-map (kbd "C-M-j") #'spacemacs/selectrum-next-candidate-preview)
      (define-key selectrum-minibuffer-map (kbd "C-SPC") #'spacemacs/embark-preview))))

(defun compleseus/init-vertico ()
  (use-package vertico
    :init
    ;; Add prompt indicator to `completing-read-multiple'.
    (defun crm-indicator (args)
      (cons (concat "[CRM] " (car args)) (cdr args)))
    (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
    ;; Grow and shrink minibuffer
    ;;(setq resize-mini-windows t)
    ;; Do not allow the cursor in the minibuffer prompt
    (setq minibuffer-prompt-properties
          '(read-only t cursor-intangible t face minibuffer-prompt))
    (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
    (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

    ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
    ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

    ;; Enable recursive minibuffers
    (setq enable-recursive-minibuffers t)

    ;; when vertico is used set this so tab when doing M-: will show suggestions
    ;; https://github.com/minad/vertico/issues/24
    (setq-default completion-in-region-function
                  (lambda (&rest args)
                    (apply (if vertico-mode
                               #'consult-completion-in-region
                             #'completion--in-region)
                           args)))

    (setq vertico-resize nil
          vertico-count 20
          vertico-cycle nil

          ;; ignore case for all basic completions
          ;; if we have orderless completion, we use it's `orderless-smart-case` feature anyway.
          ;; this is the setting when we chose "basic" from `completion-styles`,
          ;; which we do for filename completion.
          read-file-name-completion-ignore-case t
          read-buffer-completion-ignore-case t
          completion-ignore-case t)

    ;; Disable ido. We want to use the regular find-file etc.; enhanced by vertico
    (setq ido-mode nil)

    (vertico-mode)

    :config
    (when (spacemacs//support-hjkl-navigation-p)
      (define-key vertico-map (kbd "C-j") #'vertico-next)
      (define-key vertico-map (kbd "C-k") #'vertico-previous)
      (define-key vertico-map (kbd "C-l") #'vertico-insert)
      (define-key vertico-map (kbd "C-S-j") #'vertico-next-group)
      (define-key vertico-map (kbd "C-S-k") #'vertico-previous-group)
      (define-key vertico-map (kbd "C-M-j") #'spacemacs/next-candidate-preview)
      (define-key vertico-map (kbd "C-M-k") #'spacemacs/previous-candidate-preview)
      (define-key vertico-map (kbd "M-RET") #'vertico-exit-input)
      (define-key vertico-map (kbd "C-SPC") #'spacemacs/embark-preview)
      (define-key vertico-map (kbd "C-r") #'consult-history)))

  (use-package vertico-directory
      :after vertico
      :ensure nil
      ;; More convenient directory navigation commands
      :init (bind-key "C-h" 'vertico-directory-up vertico-map
                      (spacemacs//support-hjkl-navigation-p))
      ;; tidy shadowed file names
      :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

  (use-package vertico-quick
      :after vertico
      :ensure nil
      :init
      (define-key vertico-map "\M-q" #'vertico-quick-insert)
      (define-key vertico-map "\C-q" #'vertico-quick-exit))

  (use-package vertico-repeat
      :after vertico
      :ensure nil
      :init
      (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
      (spacemacs/set-leader-keys
        "rl" 'vertico-repeat-last
        "rL" 'vertico-repeat-select
        "sl" 'vertico-repeat-last
        "sL" 'vertico-repeat-select)))

(defun compleseus/post-init-grep ()
  (spacemacs/set-leader-keys-for-major-mode 'grep-mode
    "w" 'spacemacs/compleseus-grep-change-to-wgrep-mode))

(defun compleseus/init-wgrep ()
  (add-hook 'spacemacs-editing-style-hook #'spacemacs//set-initial-grep-state)
  (evil-define-key 'normal wgrep-mode-map ",," #'spacemacs/wgrep-finish-edit)
  (evil-define-key 'normal wgrep-mode-map ",c" #'spacemacs/wgrep-finish-edit)
  (evil-define-key 'normal wgrep-mode-map ",a" #'spacemacs/wgrep-abort-changes)
  (evil-define-key 'normal wgrep-mode-map ",k" #'spacemacs/wgrep-abort-changes)
  (evil-define-key 'normal wgrep-mode-map ",q" #'spacemacs/wgrep-abort-changes-and-quit)
  (evil-define-key 'normal wgrep-mode-map ",s" #'spacemacs/wgrep-save-changes-and-quit))

(defun compleseus/init-compleseus-spacemacs-help ()
  (use-package compleseus-spacemacs-help
    :defer t
    :init
    (spacemacs/set-leader-keys
      "h ."   'compleseus-spacemacs-help-dotspacemacs
      "h SPC" 'compleseus-spacemacs-help
      "h f"   'compleseus-spacemacs-help-faq
      "h l"   'compleseus-spacemacs-help-layers
      "h p"   'compleseus-spacemacs-help-packages
      "h r"   'compleseus-spacemacs-help-docs
      "h t"   'compleseus-spacemacs-help-toggles)))

(defun compleseus/pre-init-persp-mode ()
  (spacemacs|use-package-add-hook persp-mode
    :post-config
    (setq
     spacemacs--persp-display-buffers-func 'spacemacs/compleseus-switch-to-buffer
     spacemacs--persp-display-perspectives-func 'spacemacs/compleseus-spacemacs-layout-layouts)))
