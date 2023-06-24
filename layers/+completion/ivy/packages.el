;;; packages.el --- Ivy Layer packages File
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
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


(setq ivy-packages
      '(
        (all-the-icons-ivy-rich :toggle ivy-enable-icons)
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
        (ivy-rich :toggle (progn
                            (when ivy-enable-icons
                              (setq ivy-enable-advanced-buffer-information t))
                            ivy-enable-advanced-buffer-information))
        (ivy-spacemacs-help :location local)
        ivy-xref
        org
        persp-mode
        projectile
        recentf
        smex
        swiper
        wgrep
        ))

(defun ivy/init-all-the-icons-ivy-rich ()
  (use-package all-the-icons-ivy-rich
    :after ivy-rich
    :config
    (all-the-icons-ivy-rich-mode)))

(defun ivy/pre-init-auto-highlight-symbol ()
  (spacemacs|use-package-add-hook auto-highlight-symbol
    :post-init
    ;; add some functions to ahs transient states
    (setq spacemacs--symbol-highlight-transient-state-doc
          (concat
           spacemacs--symbol-highlight-transient-state-doc
           "  Search: [_s_] swiper  [_b_] buffers  [_f_] files  [_/_] project"))
    (spacemacs/transient-state-register-add-bindings 'symbol-highlight
      '(("s" swiper-thing-at-point :exit t)
        ("b" swiper-all-thing-at-point :exit t)
        ("f" spacemacs/search-auto-region-or-symbol :exit t)
        ("/" spacemacs/search-project-auto-region-or-symbol :exit t)))))

(defun ivy/post-init-bookmark ()
  (spacemacs/set-leader-keys "fb" 'counsel-bookmark))

(defun ivy/init-counsel ()
  (use-package counsel
    :init
    (spacemacs/set-leader-keys
      dotspacemacs-emacs-command-key 'counsel-M-x
      ;; files
      "ff"  'spacemacs/counsel-find-file
      "fel" 'counsel-find-library
      "fL"  'counsel-locate
      ;; help
      "?"   'counsel-descbinds
      "gff" 'counsel-git
      "hda" 'counsel-apropos
      "hdf" 'counsel-describe-function
      "hdF" 'counsel-describe-face
      "hdm" 'spacemacs/describe-mode
      "hdv" 'counsel-describe-variable
      "hdx" 'spacemacs/describe-ex-command
      "hi"  'counsel-info-lookup-symbol
      "hm"  (if (spacemacs/system-is-mswindows) 'woman 'man)
      "hR"  'spacemacs/counsel-search-docs
      ;; insert
      "iu"  'counsel-unicode-char
      ;; jump
      ;; register/ring
      "ry"  'counsel-yank-pop
      "rm"  'counsel-mark-ring
      ;; jumping
      "sj"  'spacemacs/counsel-jump-in-buffer
      ;; themes
      "Ts"  'counsel-load-theme
      ;; search
      "/"   'spacemacs/search-project-auto
      "*"   'spacemacs/search-project-auto-region-or-symbol
      "sd"  'spacemacs/search-dir-auto
      "sD"  'spacemacs/search-dir-auto-region-or-symbol
      "sf"  'spacemacs/search-auto
      "sF"  'spacemacs/search-auto-region-or-symbol
      "sp"  'spacemacs/search-project-auto
      "sP"  'spacemacs/search-project-auto-region-or-symbol
      "sad" 'spacemacs/search-dir-ag
      "saD" 'spacemacs/search-dir-ag-region-or-symbol
      "saf" 'spacemacs/search-ag
      "saF" 'spacemacs/search-ag-region-or-symbol
      "sap" 'spacemacs/search-project-ag
      "saP" 'spacemacs/search-project-ag-region-or-symbol
      "sgd" 'spacemacs/search-dir-grep
      "sgD" 'spacemacs/search-dir-grep-region-or-symbol
      "sgf" 'spacemacs/search-grep
      "sgF" 'spacemacs/search-grep-region-or-symbol
      "sgp" 'counsel-git-grep
      "sgP" 'spacemacs/counsel-git-grep-region-or-symbol
      "skd" 'spacemacs/search-dir-ack
      "skD" 'spacemacs/search-dir-ack-region-or-symbol
      "skf" 'spacemacs/search-ack
      "skF" 'spacemacs/search-ack-region-or-symbol
      "skp" 'spacemacs/search-project-ack
      "skP" 'spacemacs/search-project-ack-region-or-symbol
      "srd" 'spacemacs/search-dir-rg
      "srD" 'spacemacs/search-dir-rg-region-or-symbol
      "srf" 'spacemacs/search-rg
      "srF" 'spacemacs/search-rg-region-or-symbol
      "srp" 'spacemacs/search-project-rg
      "srP" 'spacemacs/search-project-rg-region-or-symbol
      "std" 'spacemacs/search-dir-pt
      "stD" 'spacemacs/search-dir-pt-region-or-symbol
      "stf" 'spacemacs/search-pt
      "stF" 'spacemacs/search-pt-region-or-symbol
      "stp" 'spacemacs/search-project-pt
      "stP" 'spacemacs/search-project-pt-region-or-symbol)
    :config
    ;; Temporarily handle older versions of ivy
    ;; https://github.com/abo-abo/swiper/pull/1863/files
    (unless (fboundp 'counsel--elisp-to-pcre)
      (defalias 'counsel--elisp-to-pcre 'counsel-unquote-regex-parens))

    ;; set additional ivy actions
    (ivy-set-actions
     'counsel-find-file
     spacemacs--ivy-file-actions)

    (dolist (action '(spacemacs/counsel-search counsel-rg counsel-ag))
      (ivy-set-actions
       action
       spacemacs--ivy-grep-actions))

    (when (or (eq 'vim dotspacemacs-editing-style)
              (and (eq 'hybrid dotspacemacs-editing-style)
                   hybrid-style-enable-hjkl-bindings))
      (define-key counsel-find-file-map (kbd "C-h") 'counsel-up-directory))

    (define-key read-expression-map (kbd "C-r") 'counsel-minibuffer-history)
    (spacemacs//counsel-search-add-extra-bindings counsel-ag-map)
    ;; remaps built-in commands that have a counsel replacement
    (counsel-mode 1)
    (spacemacs|hide-lighter counsel-mode)
    ;; TODO Commands to port
    (spacemacs//ivy-command-not-implemented-yet "jI")
    ;; Set syntax highlighting for counsel search results
    (ivy-set-display-transformer 'spacemacs/counsel-search
                                 'counsel-git-grep-transformer)
    ;; Enable better auto completion of counsel-find-file
    ;; by recognizing file at point.
    (setq counsel-find-file-at-point t)))

(defun ivy/pre-init-counsel-projectile ()
  ;; overwrite projectile settings
  (spacemacs|use-package-add-hook projectile
    :post-init
    (progn
      (setq projectile-switch-project-action 'counsel-projectile-find-file)

      (ivy-set-actions
       'counsel-projectile-find-file
       (append spacemacs--ivy-file-actions
               '(("R" (lambda (arg)
                        (interactive)
                        (call-interactively
                         #'projectile-invalidate-cache)
                        (ivy-resume)) "refresh list")
                 )))

      (spacemacs/set-leader-keys
        "p SPC" 'counsel-projectile
        "pb"    'counsel-projectile-switch-to-buffer
        "pd"    'counsel-projectile-find-dir
        "pp"    'counsel-projectile-switch-project
        "pf"    'counsel-projectile-find-file))))

(defun ivy/post-init-evil ()
  (spacemacs/set-leader-keys
    "re" 'spacemacs/ivy-evil-registers))

(defun ivy/init-flx ()
  (use-package flx))

(defun ivy/init-helm-make ()
  (use-package helm-make
    :defer t
    :init
    (setq helm-make-completion-method 'ivy)
    (spacemacs/set-leader-keys
      "cc" 'helm-make-projectile
      "cm" 'helm-make)))

(defun ivy/post-init-imenu ()
  (spacemacs/set-leader-keys "ji" 'spacemacs/counsel-jump-in-buffer))

(defun ivy/init-ivy ()
  (use-package ivy
    :init
    ;; Key bindings
    (spacemacs/set-leader-keys
      "a'" 'spacemacs/ivy-available-repls
      "Ce" 'counsel-colors-emacs
      "Cf" 'counsel-faces
      "Cw" 'counsel-colors-web
      "fr" 'spacemacs/counsel-recentf
      "rl" 'ivy-resume
      "sl" 'ivy-resume
      "bb" 'ivy-switch-buffer)
    ;; Common Ctrl-TAB buffer switch behavior
    (with-eval-after-load 'evil
      (evil-global-set-key 'motion (kbd "<C-tab>") 'ivy-switch-buffer)
      (evil-global-set-key 'motion (kbd "<C-iso-lefttab>") 'ivy-switch-buffer))
    (define-key ivy-mode-map (kbd "<C-tab>") 'ivy-next-line-and-call)
    (define-key ivy-mode-map (kbd "<C-iso-lefttab>") 'ivy-previous-line-and-call)
    ;; Moved C-k to C-M-k
    (define-key ivy-switch-buffer-map (kbd "C-M-k") 'ivy-switch-buffer-kill)
    (define-key ivy-reverse-i-search-map
      (kbd "C-M-k") 'ivy-reverse-i-search-kill)
    :config
    ;; custom actions for recentf
    (ivy-set-actions
     'counsel-recentf
     spacemacs--ivy-file-actions)

    ;; add spacemacs/counsel-search command to ivy-highlight-grep-commands
    (add-to-list 'ivy-highlight-grep-commands 'spacemacs/counsel-search)

    ;; mappings to quit minibuffer or enter transient state
    (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
    (define-key ivy-minibuffer-map (kbd "M-SPC") 'hydra-ivy/body)
    (define-key ivy-minibuffer-map (kbd "C-<return>") #'ivy-alt-done)
    (define-key ivy-minibuffer-map (kbd "C-.") #'ivy-mark)
    (define-key ivy-minibuffer-map (kbd "C-,") #'ivy-unmark)
    (define-key ivy-minibuffer-map (kbd "C-<") #'ivy-unmark-backward)
    (define-key ivy-minibuffer-map (kbd "C->") #'ivy-toggle-marks)
    (define-key ivy-minibuffer-map (kbd "C-SPC") #'ivy-call-and-recenter)

    (when ivy-ret-visits-directory
      (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
      (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-done))

    (ivy-mode 1)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    ;; Occur
    (evil-make-overriding-map ivy-occur-grep-mode-map)
    (evil-make-overriding-map ivy-occur-mode-map 'normal)
    (dolist (mode-map (list ivy-occur-mode-map ivy-occur-grep-mode-map))
      (define-key mode-map "g" nil)
      (define-key mode-map "U" 'ivy-occur-revert-buffer))
    (ivy-set-occur 'spacemacs/counsel-search
                   'spacemacs//counsel-occur)
    (spacemacs/set-leader-keys-for-major-mode 'ivy-occur-grep-mode
      "w" 'spacemacs/ivy-wgrep-change-to-wgrep-mode
      "s" 'wgrep-save-all-buffers)

    ;; emacs 27 extend line for ivy highlight
    (setf (alist-get 't ivy-format-functions-alist)
          #'ivy-format-function-line)

    ;; Why do we do this ?
    (ido-mode -1)

    ;; allow to select prompt in some ivy functions
    (setq ivy-use-selectable-prompt t)))

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
    (setq ivy-rich-path-style 'abbrev
          ivy-virtual-abbreviate 'full)
    :config
    (ivy-rich-mode)
    (ivy-rich-project-root-cache-mode)))

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

(defun ivy/init-ivy-xref ()
  (use-package ivy-xref
    :defer t
    :init
    (setq xref-prompt-for-identifier '(not xref-find-definitions
                                           xref-find-definitions-other-window
                                           xref-find-definitions-other-frame
                                           xref-find-references
                                           spacemacs/jump-to-definition))

    ;; Use ivy-xref to display `xref.el' results.
    (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
    (ivy-set-actions
     'ivy-xref-show-xrefs
     '(("j" spacemacs/ivy-xref-open-in-other-window "other window")))))

(defun ivy/post-init-org ()
  (add-hook 'org-ctrl-c-ctrl-c-hook 'spacemacs//counsel-org-ctrl-c-ctrl-c-org-tag))

(defun ivy/pre-init-persp-mode ()
  (spacemacs|use-package-add-hook persp-mode
    :post-config
    (setq
     spacemacs--persp-display-buffers-func 'spacemacs/ivy-spacemacs-layout-buffer
     spacemacs--persp-display-perspectives-func 'spacemacs/ivy-spacemacs-layouts)))

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
     ("k" persp-kill  "Kill layout(s)")
     ("n" persp-copy "Copy Current Layout")
     ("p" spacemacs//create-persp-with-current-project-buffers
      "Create Project Layout")))
  ;; TODO: better handling of C and X bindings for ivy
  ;;       check ivy/pre-init-persp-mode
  (spacemacs/transient-state-register-remove-bindings 'layouts
    '("C" "X"))
  (spacemacs/transient-state-register-add-bindings 'layouts
    '(("C" spacemacs/ivy-spacemacs-layout-close-other :exit t)
      ("X" spacemacs/ivy-spacemacs-layout-kill-other :exit t))))

(defun ivy/post-init-projectile ()
  (setq projectile-completion-system 'ivy)
  (spacemacs/set-leader-keys
    "pv"  'projectile-vc))

(defun ivy/post-init-recentf ()
  ;; custom actions for recentf
  (ivy-set-actions
   'counsel-recentf
   (append spacemacs--ivy-file-actions
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
                        smex-save-file (concat spacemacs-cache-directory
                                               ".smex-items"))))

(defun ivy/init-swiper ()
  (use-package swiper
    :config
    (spacemacs/set-leader-keys
      "ss" 'swiper
      "sS" 'swiper-thing-at-point
      "sb" 'swiper-all
      "sB" 'swiper-all-thing-at-point)
    (global-set-key "\C-s" 'swiper)))

(defun ivy/init-wgrep ()
  (evil-define-key 'normal wgrep-mode-map ",," 'wgrep-finish-edit)
  (evil-define-key 'normal wgrep-mode-map ",c" 'wgrep-finish-edit)
  (evil-define-key 'normal wgrep-mode-map ",a" 'wgrep-abort-changes)
  (evil-define-key 'normal wgrep-mode-map ",k" 'wgrep-abort-changes))
