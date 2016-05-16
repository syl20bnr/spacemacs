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
      '(auto-highlight-symbol
        counsel
        evil
        flx
        ivy
        (ivy-spacemacs-help :location local)
        ;; Why do we need this ?
        pcre2el
        projectile
        smex
        swiper
        wgrep))

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
      ;; remaps built-in commands that have a counsel replacement
      (counsel-mode 1)
      (spacemacs|hide-lighter counsel-mode)
      ;; TODO Commands to port
      (spacemacs//ivy-command-not-implemented-yet "jI")
      ;; Set syntax highlighting for counsel search results
      (ivy-set-display-transformer 'spacemacs/counsel-search 'counsel-git-grep-transformer))))

(defun ivy/post-init-auto-highlight-symbol ()
  (setq spacemacs-symbol-highlight-transient-state-remove-bindings
        '("/" "b" "f"))
  (setq spacemacs-symbol-highlight-transient-state-add-bindings
        '(("/" spacemacs/search-project-auto-region-or-symbol :exit t)
          ("b" spacemacs/swiper-all-region-or-symbol :exit t)
          ("f" spacemacs/search-auto-region-or-symbol :exit t))))

(defun ivy/init-flx ())

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
        "fr" 'ivy-recentf
        "rl" 'ivy-resume
        "bb" 'ivy-switch-buffer)
      (ivy-mode 1)
      (global-set-key (kbd "C-c C-r") 'ivy-resume)
      (global-set-key (kbd "<f6>") 'ivy-resume)
      ;; Occur
      (evil-make-overriding-map ivy-occur-mode-map 'normal)
      (ivy-set-occur 'spacemacs/counsel-search
                     'spacemacs//counsel-occur)
      (spacemacs/set-leader-keys-for-major-mode 'ivy-occur-grep-mode
        "w" 'ivy-wgrep-change-to-wgrep-mode)
      ;; Perspectives support
      (ivy-set-actions
       'spacemacs/ivy-perspectives
       '(("c" persp-kill-without-buffers "Close perspective(s)")
         ("k" persp-kill  "Kill perspective(s)")))
      (setq spacemacs-layouts-transient-state-remove-bindings
            '("b" "l" "C" "X"))
      (setq spacemacs-layouts-transient-state-add-bindings
            '(("b" spacemacs/ivy-persp-buffer)
              ("l" spacemacs/ivy-perspectives)
              ("C" spacemacs/ivy-persp-close-other :exit t)
              ("X" spacemacs/ivy-persp-kill-other :exit t)))
      ;; Why do we do this ?
      (ido-mode -1))))

;; Why do we need this ?
(defun ivy/init-pcre2el ()
  (use-package pcre2el :defer t))

(defun ivy/post-init-projectile ()
  (setq projectile-completion-system 'ivy)
  (spacemacs/set-leader-keys
    "pp"  'projectile-switch-project
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
