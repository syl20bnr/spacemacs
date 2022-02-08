;;; packages.el --- compleseus layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
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
    (compleseus-spacemacs-help :location local)
    consult
    consult-yasnippet
    embark
    embark-consult
    orderless
    persp-mode
    (selectrum :toggle (eq compleseus-engine 'selectrum))
    (vertico
     :toggle (eq compleseus-engine 'vertico)
     ;; TODO remove when `vertico-repeat' on ELPA
     :location (recipe :fetcher github
                       :repo "minad/vertico"))
    (vertico-directory
     :toggle (eq compleseus-engine 'vertico)
     ;; TODO remove when it's on ELPA
     :location (recipe :fetcher url
                       :url "https://raw.githubusercontent.com/minad/vertico/main/extensions/vertico-directory.el"))
    (vertico-quick
     :toggle (eq compleseus-engine 'vertico)
     ;; TODO remove when it's on ELPA
     :location (recipe :fetcher url
                       :url "https://raw.githubusercontent.com/minad/vertico/main/extensions/vertico-quick.el"))
    (vertico-repeat
     :toggle (eq compleseus-engine 'vertico)
     ;; TODO: Remove when https://github.com/minad/vertico/issues/83 solved.
     :location (recipe :fetcher url
                       :url "https://raw.githubusercontent.com/minad/vertico/main/extensions/vertico-repeat.el"))
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
  (spacemacs/set-leader-keys "ji" 'consult-imenu))

(defun compleseus/init-marginalia ()
  (use-package marginalia
    ;; Either bind `marginalia-cycle` globally or only in the minibuffer
    :bind (("M-A" . marginalia-cycle)
           :map minibuffer-local-map
           ("M-A" . marginalia-cycle))

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
           ("<help> a" . consult-apropos)            ;; orig. apropos-command
           ;; M-g bindings (goto-map)
           ("M-g e" . consult-compile-error)
           ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
           ("M-g g" . consult-goto-line)             ;; orig. goto-line
           ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
           ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
           ("M-g m" . consult-mark)
           ("M-g k" . consult-global-mark)
           ("M-g i" . consult-imenu)
           ("M-g I" . consult-project-imenu)
           ;; M-s bindings (search-map)
           ("M-s f" . consult-find)
           ("M-s L" . consult-locate)
           ("M-s g" . consult-grep)
           ("M-s G" . consult-git-grep)
           ("M-s r" . consult-ripgrep)
           ("M-s l" . consult-line)
           ("M-s m" . consult-multi-occur)
           ("M-s k" . consult-keep-lines)
           ("M-s u" . consult-focus-lines)
           ;; Isearch integration
           ("M-s e" . consult-isearch)
           :map isearch-mode-map
           ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
           ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
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
      "hda" #'consult-apropos
      "jm" #'consult-mark
      "jM" #'consult-global-mark
      "sb" #'consult-line-multi
      "sB" #'spacemacs/consult-line-multi
      "ss" #'consult-line
      "sS" #'spacemacs/consult-line
      "sk" #'consult-keep-lines
      "rL" #'consult-complex-command
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

    ;; Replace `completing-read-multiple' with an enhanced version.
    (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

    ;; Use Consult to select xref locations with preview
    (setq xref-show-xrefs-function #'consult-xref)

    ;; Configure other variables and modes in the :config section,
    ;; after lazily loading the package.
    :config

    ;; Optionally configure preview. The default value
    ;; is 'any, such that any key triggers the preview.
    ;; (setq consult-preview-key 'any)
    ;; (setq consult-preview-key (kbd "M-."))
    ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
    ;; For some commands and buffer sources it is useful to configure the
    ;; :preview-key on a per-command basis using the `consult-customize' macro.
    (consult-customize
     consult-theme
     :preview-key '(:debounce 0.2 any)
     consult-ripgrep consult-git-grep consult-grep
     consult-bookmark consult-recent-file consult-xref
     consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
     spacemacs/compleseus-search-auto
     spacemacs/compleseus-search-dir
     spacemacs/compleseus-search-projectile
     spacemacs/compleseus-search-default
     spacemacs/compleseus-search-projectile-auto
     :preview-key (list (kbd "C-SPC") (kbd "C-M-j") (kbd "C-M-k")))
    ;; :preview-key (kbd "C-SPC"))

    ;; Optionally configure the narrowing key.
    ;; Both < and C-+ work reasonably well.
    (setq consult-narrow-key "<") ;; (kbd "C-+")

    ;; Optionally make narrowing help available in the minibuffer.
    ;; You may want to use `embark-prefix-help-command' or which-key instead.
    ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

    ;; Optionally configure a function which returns the project root directory.
    (setq consult-project-root-function
          (lambda ()
            (when-let (project (project-current))
              (car (project-roots project)))))))


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
    ;; Optionally replace the key help with a completing-read interface
    (setq prefix-help-command #'embark-prefix-help-command)
    ;; same key binding as ivy-occur
    (define-key minibuffer-local-map (kbd "C-c C-o") #'embark-export)
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
  ;; https://github.com/oantolin/orderless/issues/48#issuecomment-856750410
  (define-advice company-capf
      (:around (orig-fun &rest args) set-completion-styles)
    (let ((completion-styles '(basic partial-completion orderless)))
      (apply orig-fun args)))

  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion)))))))

(defun compleseus/init-selectrum ()
  (use-package selectrum
    :init
    (selectrum-mode)
    (spacemacs/set-leader-keys
      "rl" 'selectrum-repeat
      "sl" 'selectrum-repeat)
    :config
    ;; TODO can we just use `minibuffer-mode-map'?
    (define-key selectrum-minibuffer-map (kbd "C-j") 'selectrum-next-candidate)
    (define-key selectrum-minibuffer-map (kbd "C-r") 'consult-history)
    (define-key selectrum-minibuffer-map (kbd "C-k") 'selectrum-previous-candidate)
    (define-key selectrum-minibuffer-map (kbd "C-M-k") #'spacemacs/selectrum-previous-candidate-preview)
    (define-key selectrum-minibuffer-map (kbd "C-M-j") #'spacemacs/selectrum-next-candidate-preview)
    (define-key selectrum-minibuffer-map (kbd "C-SPC") #'spacemacs/embark-preview)))

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

    ;; Cleans up path when moving directories with shadowed paths syntax, e.g.
    ;; cleans ~/foo/bar/// to /, and ~/foo/bar/~/ to ~/.
    (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

    ;; Enable recursive minibuffers
    (setq enable-recursive-minibuffers t)

    ;; when vertico is used set this so tab when doing M-: will show suggestions
    ;; https://github.com/minad/vertico/issues/24
    (setq completion-in-region-function #'consult-completion-in-region)

    ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
    ;; (setq vertico-cycle t)
    (vertico-mode)
    :config
    (define-key vertico-map (kbd "M-RET") #'vertico-exit-input)
    (define-key vertico-map (kbd "C-SPC") #'spacemacs/embark-preview)
    (define-key vertico-map (kbd "C-j") #'vertico-next)
    (define-key vertico-map (kbd "C-M-j") #'spacemacs/next-candidate-preview)
    (define-key vertico-map (kbd "C-S-j") #'vertico-next-group)
    (define-key vertico-map (kbd "C-k") #'vertico-previous)
    (define-key vertico-map (kbd "C-M-k") #'spacemacs/previous-candidate-preview)
    (define-key vertico-map (kbd "C-S-k") #'vertico-previous-group)
    (define-key vertico-map (kbd "C-r") #'consult-history)))

(defun compleseus/init-vertico-quick ()
  (use-package vertico-quick
    :after vertico
    :init
    (define-key vertico-map "\M-q" #'vertico-quick-insert)
    (define-key vertico-map "\C-q" #'vertico-quick-exit)))

(defun compleseus/init-vertico-repeat ()
  (use-package vertico-repeat
    :after vertico
    :init
    (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
    (spacemacs/set-leader-keys
      "rl" 'vertico-repeat
      "sl" 'vertico-repeat)))

(defun compleseus/init-vertico-directory ()
  (use-package vertico-directory
    ;; More convenient directory navigation commands
    :bind (:map vertico-map
                ("C-h" . vertico-directory-delete-char))
    ;; Tidy shadowed file names
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)))

(defun spacemacs/compleseus-wgrep-change-to-wgrep-mode ()
  (interactive)
  (wgrep-change-to-wgrep-mode)
  (evil-normal-state))

(defun compleseus/post-init-grep ()
  (spacemacs/set-leader-keys-for-major-mode 'grep-mode
    "w" 'spacemacs/compleseus-wgrep-change-to-wgrep-mode
    "s" 'wgrep-save-all-buffers))

(defun compleseus/init-wgrep ()
  (evil-define-key 'normal wgrep-mode-map ",," 'wgrep-finish-edit)
  (evil-define-key 'normal wgrep-mode-map ",c" 'wgrep-finish-edit)
  (evil-define-key 'normal wgrep-mode-map ",a" 'wgrep-abort-changes)
  (evil-define-key 'normal wgrep-mode-map ",k" 'wgrep-abort-changes))

(defun compleseus/init-compleseus-spacemacs-help ()
  (use-package compleseus-spacemacs-help
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
