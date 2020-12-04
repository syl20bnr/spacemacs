;;; packages.el --- Ivy Layer packages File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq ivy-packages
      '(
        auto-highlight-symbol
        bookmark
        counsel
        counsel-projectile
        evil
        flx
        helm-make
        imenu
        ivy
        ivy-avy
        ivy-hydra
        (ivy-rich :toggle ivy-enable-advanced-buffer-information)
        (ivy-space-macs-help :location local)
        ivy-xref
        org
        persp-mode
        projectile
        recentf
        smex
        swiper
        wgrep
        ))

(defun ivy/pre-init-auto-highlight-symbol ()
  (space-macs|use-package-add-hook auto-highlight-symbol
    :post-init
    ;; add some functions to ahs transient states
    (setq space-macs--symbol-highlight-transient-state-doc
          (concat
           space-macs--symbol-highlight-transient-state-doc
           "  Search: [_s_] swiper  [_b_] buffers  [_f_] files  [_/_] project"))
    (space-macs/transient-state-register-add-bindings 'symbol-highlight
      '(("s" swiper-thing-at-point :exit t)
        ("b" swiper-all-thing-at-point :exit t)
        ("f" space-macs/search-auto-region-or-symbol :exit t)
        ("/" space-macs/search-project-auto-region-or-symbol :exit t)))))

(defun ivy/post-init-bookmark ()
  (space-macs/set-leader-keys "fb" 'counsel-bookmark))

(defun ivy/init-counsel ()
  (use-package counsel
    :init
    (progn
      (space-macs/set-leader-keys
        dotspace-macs-e-macs-command-key 'counsel-M-x
        ;; files
        "ff"  'space-macs/counsel-find-file
        "fel" 'counsel-find-library
        "fL"  'counsel-locate
        ;; help
        "?"   'counsel-descbinds
        "gff" 'counsel-git
        "hda" 'counsel-apropos
        "hdf" 'counsel-describe-function
        "hdF" 'counsel-describe-face
        "hdm" 'space-macs/describe-mode
        "hdv" 'counsel-describe-variable
        "hi"  'counsel-info-lookup-symbol
        "hm"  (if (space-macs/system-is-mswindows) 'woman 'man)
        "hR"  'space-macs/counsel-search-docs
        ;; insert
        "iu"  'counsel-unicode-char
        ;; jump
        ;; register/ring
        "ry"  'counsel-yank-pop
        "rm"  'counsel-mark-ring
        ;; jumping
        "sj"  'space-macs/counsel-jump-in-buffer
        ;; themes
        "Ts"  'counsel-load-theme
        ;; search
        "/"   'space-macs/search-project-auto
        "*"   'space-macs/search-project-auto-region-or-symbol
        "sd"  'space-macs/search-dir-auto
        "sD"  'space-macs/search-dir-auto-region-or-symbol
        "sf"  'space-macs/search-auto
        "sF"  'space-macs/search-auto-region-or-symbol
        "sp"  'space-macs/search-project-auto
        "sP"  'space-macs/search-project-auto-region-or-symbol
        "sad" 'space-macs/search-dir-ag
        "saD" 'space-macs/search-dir-ag-region-or-symbol
        "saf" 'space-macs/search-ag
        "saF" 'space-macs/search-ag-region-or-symbol
        "sap" 'space-macs/search-project-ag
        "saP" 'space-macs/search-project-ag-region-or-symbol
        "sgd" 'space-macs/search-dir-grep
        "sgD" 'space-macs/search-dir-grep-region-or-symbol
        "sgf" 'space-macs/search-grep
        "sgF" 'space-macs/search-grep-region-or-symbol
        "sgp" 'counsel-git-grep
        "sgP" 'space-macs/counsel-git-grep-region-or-symbol
        "skd" 'space-macs/search-dir-ack
        "skD" 'space-macs/search-dir-ack-region-or-symbol
        "skf" 'space-macs/search-ack
        "skF" 'space-macs/search-ack-region-or-symbol
        "skp" 'space-macs/search-project-ack
        "skP" 'space-macs/search-project-ack-region-or-symbol
        "srd" 'space-macs/search-dir-rg
        "srD" 'space-macs/search-dir-rg-region-or-symbol
        "srf" 'space-macs/search-rg
        "srF" 'space-macs/search-rg-region-or-symbol
        "srp" 'space-macs/search-project-rg
        "srP" 'space-macs/search-project-rg-region-or-symbol
        "std" 'space-macs/search-dir-pt
        "stD" 'space-macs/search-dir-pt-region-or-symbol
        "stf" 'space-macs/search-pt
        "stF" 'space-macs/search-pt-region-or-symbol
        "stp" 'space-macs/search-project-pt
        "stP" 'space-macs/search-project-pt-region-or-symbol))
    :config
    (progn
      ;; Temporarily handle older versions of ivy
      ;; https://github.com/abo-abo/swiper/pull/1863/files
      (unless (fboundp 'counsel--elisp-to-pcre)
        (defalias 'counsel--elisp-to-pcre 'counsel-unquote-regex-parens))

      ;; set additional ivy actions
      (ivy-set-actions
       'counsel-find-file
       space-macs--ivy-file-actions)

      (when (or (eq 'vim dotspace-macs-editing-style)
                (and (eq 'hybrid dotspace-macs-editing-style)
                     hybrid-style-enable-hjkl-bindings))
        (define-key counsel-find-file-map (kbd "C-h") 'counsel-up-directory))

      (define-key read-expression-map (kbd "C-r") 'counsel-minibuffer-history)
      (space-macs//counsel-search-add-extra-bindings counsel-ag-map)
      ;; remaps built-in commands that have a counsel replacement
      (counsel-mode 1)
      (space-macs|hide-lighter counsel-mode)
      ;; TODO Commands to port
      (space-macs//ivy-command-not-implemented-yet "jI")
      ;; Set syntax highlighting for counsel search results
      (ivy-set-display-transformer 'space-macs/counsel-search
                                   'counsel-git-grep-transformer)
      ;; Enable better auto completion of counsel-find-file
      ;; by recognizing file at point.
      (setq counsel-find-file-at-point t))))

(defun ivy/pre-init-counsel-projectile ()
  ;; overwrite projectile settings
  (space-macs|use-package-add-hook projectile
    :post-init
    (progn
      (setq projectile-switch-project-action 'counsel-projectile-find-file)

      (ivy-set-actions
       'counsel-projectile-find-file
       (append space-macs--ivy-file-actions
               '(("R" (lambda (arg)
                        (interactive)
                        (call-interactively
                         #'projectile-invalidate-cache)
                        (ivy-resume)) "refresh list")
                 )))

      (space-macs/set-leader-keys
        "p SPC" 'counsel-projectile
        "pb"    'counsel-projectile-switch-to-buffer
        "pd"    'counsel-projectile-find-dir
        "pp"    'counsel-projectile-switch-project
        "pf"    'counsel-projectile-find-file))))

(defun ivy/post-init-evil ()
  (space-macs/set-leader-keys
    "re" 'space-macs/ivy-evil-registers))

(defun ivy/init-flx ()
  (use-package flx))

(defun ivy/init-helm-make ()
  (use-package helm-make
    :defer t
    :init
    (progn
      (setq helm-make-completion-method 'ivy)
      (space-macs/set-leader-keys
        "cc" 'helm-make-projectile
        "cm" 'helm-make))))

(defun ivy/post-init-imenu ()
  (space-macs/set-leader-keys "ji" 'space-macs/counsel-jump-in-buffer))

(defun ivy/init-ivy ()
  (use-package ivy
    :init
    (progn
      ;; Key bindings
      (space-macs/set-leader-keys
        "a'" 'space-macs/ivy-available-repls
        "Ce" 'counsel-colors-e-macs
        "Cf" 'counsel-faces
        "Cw" 'counsel-colors-web
        "fr" 'space-macs/counsel-recentf
        "rl" 'ivy-resume
        "sl" 'ivy-resume
        "bb" 'ivy-switch-buffer)
      ;; Moved C-k to C-M-k
      (define-key ivy-switch-buffer-map (kbd "C-M-k") 'ivy-switch-buffer-kill)
      (define-key ivy-reverse-i-search-map
        (kbd "C-M-k") 'ivy-reverse-i-search-kill))
    :config
    (progn
      ;; custom actions for recentf
      (ivy-set-actions
       'counsel-recentf
       space-macs--ivy-file-actions)

      ;; add space-macs/counsel-search command to ivy-highlight-grep-commands
      (add-to-list 'ivy-highlight-grep-commands 'space-macs/counsel-search)

      ;; mappings to quit minibuffer or enter transient state
      (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
      (define-key ivy-minibuffer-map (kbd "M-SPC") 'hydra-ivy/body)
      (define-key ivy-minibuffer-map (kbd "C-<return>") #'ivy-alt-done)

      (when ivy-ret-visits-directory
        (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
        (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-done))

      (ivy-mode 1)
      (global-set-key (kbd "C-c C-r") 'ivy-resume)
      (global-set-key (kbd "<f6>") 'ivy-resume)
      ;; Occur
      (evil-set-initial-state 'ivy-occur-grep-mode 'normal)
      (evil-make-overriding-map ivy-occur-mode-map 'normal)
      (ivy-set-occur 'space-macs/counsel-search
                     'space-macs//counsel-occur)
      (space-macs/set-leader-keys-for-major-mode 'ivy-occur-grep-mode
        "w" 'space-macs/ivy-wgrep-change-to-wgrep-mode
        "s" 'wgrep-save-all-buffers)
      ;; Why do we do this ?
      (ido-mode -1)

      ;; allow to select prompt in some ivy functions
      (setq ivy-use-selectable-prompt t))))

(defun ivy/init-ivy-avy ()
  (use-package ivy-avy
    :after ivy))

(defun ivy/init-ivy-hydra ()
  (use-package ivy-hydra
    :after ivy
    :config
    (define-key hydra-ivy/keymap [escape] 'hydra-ivy/keyboard-escape-quit-and-exit)))

(defun ivy/init-ivy-rich ()
  (use-package ivy-rich
    ;; if `counsel' loads after `ivy-rich', it overrides some of `ivy-rich''s
    ;; transformers
    :after counsel
    :init
    (progn
      (setq ivy-rich-path-style 'abbrev
            ivy-virtual-abbreviate 'full))
    :config
    (progn
      (ivy-rich-mode))))

(defun ivy/init-ivy-space-macs-help ()
  (use-package ivy-space-macs-help
    :commands (ivy-space-macs-help-dotspace-macs
               ivy-space-macs-help
               ivy-space-macs-help-faq
               ivy-space-macs-help-layers
               ivy-space-macs-help-packages
               ivy-space-macs-help-docs
               ivy-space-macs-help-toggles)
    :init (space-macs/set-leader-keys
            "h ."   'ivy-space-macs-help-dotspace-macs
            "h SPC" 'ivy-space-macs-help
            "h f"   'ivy-space-macs-help-faq
            "h l"   'ivy-space-macs-help-layers
            "h p"   'ivy-space-macs-help-packages
            "h r"   'ivy-space-macs-help-docs
            "h t"   'ivy-space-macs-help-toggles)))

(defun ivy/init-ivy-xref ()
  (use-package ivy-xref
    :defer t
    :init
    (progn
      (setq xref-prompt-for-identifier '(not xref-find-definitions
                                             xref-find-definitions-other-window
                                             xref-find-definitions-other-frame
                                             xref-find-references
                                             space-macs/jump-to-definition))

      ;; Use ivy-xref to display `xref.el' results.
      (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))))

(defun ivy/post-init-org ()
  (add-hook 'org-ctrl-c-ctrl-c-hook 'space-macs//counsel-org-ctrl-c-ctrl-c-org-tag))

(defun ivy/pre-init-persp-mode ()
  (space-macs|use-package-add-hook persp-mode
    :post-config
    (setq
     space-macs--persp-display-buffers-func 'space-macs/ivy-space-macs-layout-buffer
     space-macs--persp-display-perspectives-func 'space-macs/ivy-space-macs-layouts)))

(defun ivy/post-init-persp-mode ()
  ;; based on https://gist.github.com/Bad-ptr/1aca1ec54c3bdb2ee80996eb2b68ad2d#file-persp-ivy-el
  (add-hook 'ivy-ignore-buffers #'space-macs//layout-not-contains-buffer-p)
  (setq ivy-sort-functions-alist
        (append ivy-sort-functions-alist
                '((persp-kill-buffer . nil)
                  (persp-remove-buffer . nil)
                  (persp-add-buffer . nil)
                  (persp-switch . nil)
                  (persp-window-switch . nil)
                  (persp-frame-switch . nil))))

  (ivy-set-actions
   'space-macs/ivy-space-macs-layouts
   '(("c" persp-kill-without-buffers "Close layout(s)")
     ("k" persp-kill  "Kill layout(s)")
     ("n" persp-copy "Copy Current Layout")
     ("p" space-macs//create-persp-with-current-project-buffers
      "Create Project Layout")))
  ;; TODO: better handling of C and X bindings for ivy
  ;;       check ivy/pre-init-persp-mode
  (space-macs/transient-state-register-remove-bindings 'layouts
    '("C" "X"))
  (space-macs/transient-state-register-add-bindings 'layouts
    '(("C" space-macs/ivy-space-macs-layout-close-other :exit t)
      ("X" space-macs/ivy-space-macs-layout-kill-other :exit t))))

(defun ivy/post-init-projectile ()
  (setq projectile-completion-system 'ivy)
  (space-macs/set-leader-keys
    "pv"  'projectile-vc))

(defun ivy/post-init-recentf ()
  ;; custom actions for recentf
  (ivy-set-actions
   'counsel-recentf
   (append space-macs--ivy-file-actions
           '(("R" (lambda (arg)
                    (interactive)
                    (recentf-cleanup)
                    (counsel-recentf)) "refresh list")
             ("D" (lambda (arg)
                    (interactive)
                    (setq recentf-list (delete arg recentf-list))
                    (counsel-recentf)) "delete from list"))))
  ;; merge recentf and bookmarks into buffer switching. If we set this
  (setq ivy-use-virtual-buffers t))

(defun ivy/init-smex ()
  (use-package smex
    :defer t
    :init (setq-default smex-history-length 32
                        smex-save-file (concat space-macs-cache-directory
                                               ".smex-items"))))

(defun ivy/init-swiper ()
  (use-package swiper
    :config
    (progn
      (space-macs/set-leader-keys
        "ss" 'swiper
        "sS" 'swiper-thing-at-point
        "sb" 'swiper-all
        "sB" 'swiper-all-thing-at-point)
      (global-set-key "\C-s" 'swiper))))

(defun ivy/init-wgrep ()
  (evil-define-key 'normal wgrep-mode-map ",," 'wgrep-finish-edit)
  (evil-define-key 'normal wgrep-mode-map ",c" 'wgrep-finish-edit)
  (evil-define-key 'normal wgrep-mode-map ",a" 'wgrep-abort-changes)
  (evil-define-key 'normal wgrep-mode-map ",k" 'wgrep-abort-changes))


