;;; packages.el --- Ivy Layer packages File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq ivy-packages
      '(
        auto-highlight-symbol
        counsel
        (counsel-projectile :toggle (configuration-layer/package-usedp 'projectile))
        evil
        flx
        helm-make
        imenu
        ivy
        ivy-hydra
        (ivy-spacemacs-help :location local)
        persp-mode
        projectile
        smex
        swiper
        wgrep
        ))

(defun ivy/pre-init-auto-highlight-symbol ()
  (spacemacs|use-package-add-hook auto-highlight-symbol
    :post-init
    ;; add some functions to ahs transient states
    (setq spacemacs--symbol-highlight-transient-state-doc
          (concat spacemacs--symbol-highlight-transient-state-doc
                  "  [_b_] search buffers [_/_] search proj [_f_] search files")
          spacemacs-symbol-highlight-transient-state-add-bindings
          '(("/" spacemacs/search-project-auto-region-or-symbol :exit t)
            ("b" spacemacs/swiper-all-region-or-symbol :exit t)
            ("f" spacemacs/search-auto-region-or-symbol :exit t)))))

(defun ivy/init-counsel ()
  (use-package counsel
    :config
    (progn
      (define-key counsel-find-file-map (kbd "C-h") 'counsel-up-directory)
      (spacemacs/set-leader-keys
        dotspacemacs-emacs-command-key 'counsel-M-x
        ;; files
        "ff"  'counsel-find-file
        "fL"  'counsel-locate
        ;; help
        "?"   'counsel-descbinds
        "hdf" 'counsel-describe-function
        "hdm" 'spacemacs/describe-mode
        "hdv" 'counsel-describe-variable
        "hR"  'spacemacs/counsel-search-docs
        ;; insert
        "iu"  'counsel-unicode-char
        ;; jump
        ;; register/ring
        "ry"  'counsel-yank-pop
        ;; jumping
        "sj"  'counsel-imenu
        ;; themes
        "Ts"  'counsel-load-theme
        ;; search
        "/"   'spacemacs/search-project-auto
        "*"   'spacemacs/search-project-auto-region-or-symbol
        "sf"  'spacemacs/search-auto
        "sF"  'spacemacs/search-auto-region-or-symbol
        "sp"  'spacemacs/search-project-auto
        "sP"  'spacemacs/search-project-auto-region-or-symbol
        "saf" 'spacemacs/search-ag
        "saF" 'spacemacs/search-ag-region-or-symbol
        "sap" 'spacemacs/search-project-ag
        "saP" 'spacemacs/search-project-ag-region-or-symbol
        "stf" 'spacemacs/search-pt
        "stF" 'spacemacs/search-pt-region-or-symbol
        "stp" 'spacemacs/search-project-pt
        "stP" 'spacemacs/search-project-pt-region-or-symbol
        "sgf" 'spacemacs/search-grep
        "sgF" 'spacemacs/search-grep-region-or-symbol
        "sgp" 'counsel-git-grep
        "sgP" 'spacemacs/counsel-git-grep-region-or-symbol
        "skf" 'spacemacs/search-ack
        "skF" 'spacemacs/search-ack-region-or-symbol
        "skp" 'spacemacs/search-project-ack
        "skP" 'spacemacs/search-project-ack-region-or-symbol)

      ;; set additional ivy actions
      (ivy-set-actions
       'counsel-find-file
       spacemacs--ivy-file-actions)

      ;; remaps built-in commands that have a counsel replacement
      (counsel-mode 1)
      (spacemacs|hide-lighter counsel-mode)
      ;; TODO Commands to port
      (spacemacs//ivy-command-not-implemented-yet "jI")
      ;; Set syntax highlighting for counsel search results
      (ivy-set-display-transformer 'spacemacs/counsel-search 'counsel-git-grep-transformer))))

(defun ivy/init-counsel-projectile ()
  (use-package counsel-projectile
    :defer t
    :init
    ;; overwrite projectile settings
    (spacemacs|use-package-add-hook projectile
      :post-init
      (progn
        (setq projectile-switch-project-action 'counsel-projectile-find-file)
        (spacemacs/set-leader-keys
          "p SPC" 'counsel-projectile
          "pb"    'counsel-projectile-switch-to-buffer
          "pd"    'counsel-projectile-find-dir
          "pp"    'counsel-projectile-switch-project
          "pf"    'counsel-projectile-find-file
          "pr"    'projectile-recentf)))))

(defun ivy/post-init-evil ()
  (spacemacs/set-leader-keys
    "re" 'spacemacs/ivy-evil-registers))

(defun ivy/init-flx ())

(defun ivy/init-helm-make ()
  (use-package helm-make
    :defer t
    :init
    (progn
      (setq helm-make-completion-method 'ivy)
      (spacemacs/set-leader-keys
        "cc" 'helm-make-projectile
        "cm" 'helm-make))))

(defun ivy/post-init-imenu ()
  (spacemacs/set-leader-keys "ji" 'counsel-imenu))

(defun ivy/init-ivy ()
  (use-package ivy
    :config
    (progn
      (with-eval-after-load 'recentf
        ;; merge recentf and bookmarks into buffer switching. If we set this
        ;; before recentf loads, then ivy-mode loads recentf for us,
        ;; which messes up the spacemacs version of recentf.
        (setq ivy-use-virtual-buffers t))
      ;; Key bindings
      (spacemacs/set-leader-keys
        "a'" 'spacemacs/ivy-available-repls
        "fr" 'counsel-recentf
        "rl" 'ivy-resume
        "bb" 'ivy-switch-buffer)

      ;; custom actions for recentf
      (ivy-set-actions
       'counsel-recentf
       spacemacs--ivy-file-actions)

      (ivy-mode 1)
      (global-set-key (kbd "C-c C-r") 'ivy-resume)
      (global-set-key (kbd "<f6>") 'ivy-resume)
      ;; Occur
      (evil-make-overriding-map ivy-occur-mode-map 'normal)
      (ivy-set-occur 'spacemacs/counsel-search
                     'spacemacs//counsel-occur)
      (spacemacs/set-leader-keys-for-major-mode 'ivy-occur-grep-mode
        "w" 'ivy-wgrep-change-to-wgrep-mode)
      ;; Why do we do this ?
      (ido-mode -1))))

(defun ivy/init-ivy-hydra ()
  (use-package ivy-hydra))

(defun ivy/post-init-persp-mode ()
  ;; based on https://gist.github.com/Bad-ptr/1aca1ec54c3bdb2ee80996eb2b68ad2d#file-persp-ivy-el
  (add-hook 'ivy-ignore-buffers #'spacemacs//layout-not-contains-buffer-p)
  (setq ivy-sort-functions-alist
        (append ivy-sort-functions-alist
                '((persp-kill-buffer . nil)
                  (persp-remove-buffer . nil)
                  (persp-add-buffer . nil)
                  (persp-switch . nil)
                  (persp-window-switch . nil)
                  (persp-frame-switch . nil))))

  (ivy-set-actions
   'spacemacs/ivy-spacemacs-layouts
   '(("c" persp-kill-without-buffers "Close layout(s)")
     ("k" persp-kill  "Kill layout(s)")))
  (setq spacemacs-layouts-transient-state-remove-bindings
        '("b" "l" "C" "X"))
  (setq spacemacs-layouts-transient-state-add-bindings
        '(("b" spacemacs/ivy-spacemacs-layout-buffer)
          ("l" spacemacs/ivy-spacemacs-layouts :exit t)
          ("C" spacemacs/ivy-spacemacs-layout-close-other :exit t)
          ("X" spacemacs/ivy-spacemacs-layout-kill-other :exit t))))

(defun ivy/post-init-projectile ()
  (setq projectile-completion-system 'ivy)
  (spacemacs/set-leader-keys
    "pv"  'projectile-vc))

(defun ivy/init-smex ()
  (use-package smex
    :defer t
    :init (setq-default smex-history-length 32
                        smex-save-file (concat spacemacs-cache-directory
                                               ".smex-items"))))

(defun ivy/init-ivy-spacemacs-help ()
  (use-package ivy-spacemacs-help
    :commands (ivy-spacemacs-help-dotspacemacs
               ivy-spacemacs-help
               ivy-spacemacs-help-faq
               ivy-spacemacs-help-layers
               ivy-spacemacs-help-packages
               ivy-spacemacs-help-docs
               ivy-spacemacs-help-toggles)
    :init (spacemacs/set-leader-keys
            "h ."   'ivy-spacemacs-help-dotspacemacs
            "h SPC" 'ivy-spacemacs-help
            "h f"   'ivy-spacemacs-help-faq
            "h l"   'ivy-spacemacs-help-layers
            "h p"   'ivy-spacemacs-help-packages
            "h r"   'ivy-spacemacs-help-docs
            "h t"   'ivy-spacemacs-help-toggles)))

(defun ivy/init-swiper ()
  (use-package swiper
    :config
    (progn
      (spacemacs/set-leader-keys
        "ss" 'swiper
        "sS" 'spacemacs/swiper-region-or-symbol
        "sb" 'swiper-all
        "sB" 'spacemacs/swiper-all-region-or-symbol)
      (global-set-key "\C-s" 'swiper))))

(defun ivy/init-wgrep ()
  (evil-define-key 'normal wgrep-mode-map ",," 'wgrep-finish-edit)
  (evil-define-key 'normal wgrep-mode-map ",c" 'wgrep-finish-edit)
  (evil-define-key 'normal wgrep-mode-map ",a" 'wgrep-abort-changes)
  (evil-define-key 'normal wgrep-mode-map ",k" 'wgrep-abort-changes))
