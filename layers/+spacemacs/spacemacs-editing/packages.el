;;; packages.el --- Spacemacs Editing Layer packages File
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


(defconst spacemacs-editing-packages
  '(aggressive-indent
    avy
    (clean-aindent-mode :toggle dotspacemacs-use-clean-aindent-mode)
    dired-quick-sort
    drag-stuff
    editorconfig
    eval-sexp-fu
    expand-region
    (hexl :location built-in)
    hungry-delete
    link-hint
    lorem-ipsum
    (origami :toggle (eq 'origami dotspacemacs-folding-method))
    password-generator
    (persistent-scratch :toggle dotspacemacs-scratch-buffer-persistent)
    pcre2el
    (smartparens :toggle dotspacemacs-activate-smartparens-mode)
    (evil-swap-keys :toggle dotspacemacs-swap-number-row)
    (spacemacs-whitespace-cleanup :location (recipe :fetcher local))
    string-edit-at-point
    string-inflection
    multi-line
    undo-tree
    (unkillable-scratch :toggle dotspacemacs-scratch-buffer-unkillable)
    uuidgen
    (vimish-fold :toggle (eq 'vimish dotspacemacs-folding-method))
    (evil-vimish-fold :toggle (eq 'vimish dotspacemacs-folding-method))
    (evil-easymotion :toggle (memq dotspacemacs-editing-style '(vim hybrid)))
    ws-butler))

;; Initialization of packages
(defun spacemacs-editing/init-aggressive-indent ()
  (use-package aggressive-indent
    :defer t
    :init
    (spacemacs|add-toggle aggressive-indent
      :mode aggressive-indent-mode
      :documentation "Always keep code indented."
      :evil-leader "tI")
    (spacemacs|add-toggle aggressive-indent-globally
      :mode global-aggressive-indent-mode
      :documentation "Always keep code indented globally."
      :evil-leader "t C-I")
    :config
    (add-hook 'diff-auto-refine-mode-hook 'spacemacs/toggle-aggressive-indent-off)
    (spacemacs|diminish aggressive-indent-mode " Ⓘ" " I")))

(defun spacemacs-editing/init-avy ()
  (use-package avy
    :defer t
    :commands (spacemacs/avy-open-url spacemacs/avy-goto-url avy-pop-mark avy-with)
    :init
    (setq avy-all-windows 'all-frames)
    (setq avy-background t)
    (spacemacs/set-leader-keys
      "jb" 'avy-pop-mark
      "jj" 'evil-avy-goto-char-timer
      "jl" 'evil-avy-goto-line
      "ju" 'spacemacs/avy-goto-url
      "jU" 'spacemacs/avy-open-url
      "jw" 'evil-avy-goto-word-or-subword-1
      "xo" 'spacemacs/avy-open-url)
    :config
    (defun spacemacs/avy-goto-url ()
      "Use avy to go to an URL in the buffer."
      (interactive)
      (avy-jump "https?://"))
    (defun spacemacs/avy-open-url ()
      "Use avy to select an URL in the buffer and open it."
      (interactive)
      (save-excursion
        (spacemacs/avy-goto-url)
        (browse-url-at-point)))))

(defun spacemacs-editing/init-clean-aindent-mode ()
  (use-package clean-aindent-mode
    :config
    (clean-aindent-mode)
    (add-hook 'prog-mode-hook 'spacemacs//put-clean-aindent-last t)))

(defun spacemacs-editing/init-dired-quick-sort ()
  (use-package dired-quick-sort
    :defer t
    :init
    (spacemacs|add-transient-hook dired-mode-hook
      (lambda ()
        (let ((dired-quick-sort-suppress-setup-warning 'message))
          (dired-quick-sort-setup))))
    :config
    (evil-define-key 'normal dired-mode-map "s" 'hydra-dired-quick-sort/body)
    ;; workaround for https://gitlab.com/xuhdev/dired-quick-sort/-/issues/14
    (define-advice dired-sort-toggle (:before ())
      "Recover `dired-actual-switches' with `dired-listing-switches' when long
      option \"--sort=...\" exists, and convert \"--sort=time\" to \"-t\"."
      (when (string-match-p "--sort=" dired-actual-switches)
        (setq dired-actual-switches
              (concat dired-listing-switches
                      (when (string-match-p "--sort=time" dired-actual-switches)
                        " -t")))))))

(defun spacemacs-editing/init-drag-stuff ()
  (use-package drag-stuff
    :defer t
    :init
    (spacemacs|diminish drag-stuff-mode)
    (drag-stuff-mode t)
    (spacemacs|define-transient-state drag-stuff
      :title "Drag Stuff Transient State"
      :doc "
[_k_/_K_] up    [_h_/_H_] left   [_q_] quit
[_j_/_J_] down  [_l_/_L_] right"
      :bindings
      ("j" drag-stuff-down)
      ("J" drag-stuff-down)
      ("<down>" drag-stuff-down)
      ("k" drag-stuff-up)
      ("K" drag-stuff-up)
      ("<up>" drag-stuff-up)
      ("h" drag-stuff-left)
      ("H" drag-stuff-left)
      ("<left>" drag-stuff-left)
      ("l" drag-stuff-right)
      ("L" drag-stuff-right)
      ("<right>" drag-stuff-right)
      ("q" nil :exit t))
    (spacemacs/set-leader-keys
      "x." 'spacemacs/drag-stuff-transient-state/body
      "xK" 'spacemacs/drag-stuff-transient-state/drag-stuff-up
      "xJ" 'spacemacs/drag-stuff-transient-state/drag-stuff-down)))

(defun spacemacs-editing/init-editorconfig ()
  (use-package editorconfig
    :init
    (spacemacs|diminish editorconfig-mode)
    :config
    (editorconfig-mode t)))

(defun spacemacs-editing/init-eval-sexp-fu ()
  (use-package eval-sexp-fu
    :commands eval-sexp-fu-flash-mode))

;; ;; ignore obsolete function warning generated on startup
;; (let ((byte-compile-not-obsolete-funcs (append byte-compile-not-obsolete-funcs '(preceding-sexp))))
;;   (require 'eval-sexp-fu)))

(defun spacemacs-editing/init-expand-region ()
  (use-package expand-region
    :defer t
    :init (spacemacs/set-leader-keys "v" 'er/expand-region)
    :config
    ;; add search capability to expand-region
    (when (configuration-layer/package-used-p 'helm-ag)
      (defadvice er/prepare-for-more-expansions-internal
          (around helm-ag/prepare-for-more-expansions-internal activate)
        ad-do-it
        (let ((new-msg (concat (car ad-return-value)
                                ", / to search in project, "
                                "f to search in files, "
                                "b to search in opened buffers"))
              (new-bindings (cdr ad-return-value)))
          (cl-pushnew
            '("/" (lambda ()
                    (call-interactively
                    'spacemacs/helm-project-smart-do-search-region-or-symbol)))
            new-bindings)
          (cl-pushnew
            '("f" (lambda ()
                    (call-interactively
                    'spacemacs/helm-files-smart-do-search-region-or-symbol)))
            new-bindings)
          (cl-pushnew
            '("b" (lambda ()
                    (call-interactively
                    'spacemacs/helm-buffers-smart-do-search-region-or-symbol)))
            new-bindings)
          (setq ad-return-value (cons new-msg new-bindings))))
      (setq expand-region-contract-fast-key "V"
            expand-region-reset-fast-key "r"))))

(defun spacemacs-editing/init-hexl ()
  (use-package hexl
    :defer t
    :init
    (spacemacs/set-leader-keys "fh" 'hexl-find-file)
    (spacemacs/set-leader-keys-for-major-mode 'hexl-mode
      "d" 'hexl-insert-decimal-char
      "c" 'hexl-insert-octal-char
      "x" 'hexl-insert-hex-char
      "X" 'hexl-insert-hex-string
      "g" 'hexl-goto-address)
    (evil-define-key 'motion hexl-mode-map
      "]]" 'hexl-end-of-1k-page
      "[[" 'hexl-beginning-of-1k-page
      "h" 'hexl-backward-char
      "l" 'hexl-forward-char
      "j" 'hexl-next-line
      "k" 'hexl-previous-line
      "$" 'hexl-end-of-line
      "^" 'hexl-beginning-of-line
      "0" 'hexl-beginning-of-line)))

(defun spacemacs-editing/init-hungry-delete ()
  (use-package hungry-delete
    :defer t
    :init
    (spacemacs|add-toggle hungry-delete
      :mode hungry-delete-mode
      :documentation "Delete consecutive horizontal whitespace with a single key."
      :evil-leader "td")
    :config
    (nconc hungry-delete-except-modes '(term-mode vterm-mode))
    (setq-default hungry-delete-chars-to-skip " \t\f\v") ; only horizontal whitespace
    (define-key hungry-delete-mode-map (kbd "DEL") 'hungry-delete-backward)
    (define-key hungry-delete-mode-map (kbd "S-DEL") 'delete-backward-char)))

(defun spacemacs-editing/init-link-hint ()
  (use-package link-hint
    :defer t
    :init
    (spacemacs/set-leader-keys
      "xA" 'link-hint-open-all-links
      "xm" 'link-hint-open-multiple-links
      "xo" 'link-hint-open-link-at-point
      "xO" 'link-hint-open-link
      "xy" 'link-hint-copy-link-at-point
      "xY" 'link-hint-copy-link)))

(defun spacemacs-editing/init-lorem-ipsum ()
  (use-package lorem-ipsum
    :commands (lorem-ipsum-insert-list
               lorem-ipsum-insert-paragraphs
               lorem-ipsum-insert-sentences)
    :init
    (spacemacs/declare-prefix "il" "lorem ipsum")
    (spacemacs/set-leader-keys
      "ill" 'lorem-ipsum-insert-list
      "ilp" 'lorem-ipsum-insert-paragraphs
      "ils" 'lorem-ipsum-insert-sentences)))

(defun spacemacs-editing/init-origami ()
  (use-package origami
    :defer t
    :init
    (let
        ((rebind-normal-to-motion-state-map
          (lambda (key def)
            (define-key evil-normal-state-map key nil)
            (define-key evil-motion-state-map key def))))
      (global-origami-mode)
      (funcall rebind-normal-to-motion-state-map "za" 'origami-forward-toggle-node)
      (funcall rebind-normal-to-motion-state-map "zc" 'origami-close-node)
      (funcall rebind-normal-to-motion-state-map "zC" 'origami-close-node-recursively)
      (funcall rebind-normal-to-motion-state-map "zO" 'origami-open-node-recursively)
      (funcall rebind-normal-to-motion-state-map "zo" 'origami-open-node)
      (funcall rebind-normal-to-motion-state-map "zr" 'origami-open-all-nodes)
      (funcall rebind-normal-to-motion-state-map "zm" 'origami-close-all-nodes)
      (funcall rebind-normal-to-motion-state-map "zs" 'origami-show-only-node)
      (funcall rebind-normal-to-motion-state-map "zn" 'origami-next-fold)
      (funcall rebind-normal-to-motion-state-map "zp" 'origami-previous-fold)
      (funcall rebind-normal-to-motion-state-map "zR" 'origami-reset)
      (funcall rebind-normal-to-motion-state-map (kbd "z <tab>") 'origami-recursively-toggle-node)
      (funcall rebind-normal-to-motion-state-map (kbd "z TAB") 'origami-recursively-toggle-node)

      (spacemacs|define-transient-state fold
        :title "Code Fold Transient State"
        :doc "
 Close^^            Open^^             Toggle^^         Goto^^         Other^^
 ───────^^───────── ─────^^─────────── ─────^^───────── ──────^^────── ─────^^─────────
 [_c_] at point     [_o_] at point     [_a_] at point   [_n_] next     [_s_] single out
 [_C_] recursively  [_O_] recursively  [_A_] all        [_p_] previous [_R_] reset
 [_m_] all          [_r_] all          [_TAB_] like org ^^             [_q_] quit"
        :foreign-keys run
        :on-enter (unless (bound-and-true-p origami-mode) (origami-mode 1))
        :bindings
        ("a" origami-forward-toggle-node)
        ("A" origami-toggle-all-nodes)
        ("c" origami-close-node)
        ("C" origami-close-node-recursively)
        ("o" origami-open-node)
        ("O" origami-open-node-recursively)
        ("r" origami-open-all-nodes)
        ("m" origami-close-all-nodes)
        ("n" origami-next-fold)
        ("p" origami-previous-fold)
        ("s" origami-show-only-node)
        ("R" origami-reset)
        ("TAB" origami-recursively-toggle-node)
        ("<tab>" origami-recursively-toggle-node)
        ("q" nil :exit t)
        ("C-g" nil :exit t)
        ("<SPC>" nil :exit t)))))
;; Note: The key binding for the fold transient state is defined in
;; evil config

(defun spacemacs-editing/init-vimish-fold ()
  (use-package vimish-fold
    :ensure
    :after evil))

(defun spacemacs-editing/init-evil-vimish-fold ()
  (use-package evil-vimish-fold
    :ensure
    :after vimish-fold
    :init
    (setq evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode))
    :config (global-evil-vimish-fold-mode)))

(defun spacemacs-editing/init-evil-easymotion ()
  (use-package evil-easymotion
    :defer t
    :init
    (defun buffer-evil-avy-goto-char-timer ()
      "Call jump to the given chars use avy"
      (interactive)
      (let ((current-prefix-arg t))
        (evil-avy-goto-char-timer)))

    (evilem-default-keybindings "gs")
    (define-key evilem-map "a" (evilem-create #'evil-forward-arg))
    (define-key evilem-map "A" (evilem-create #'evil-backward-arg))
    (define-key evilem-map "o" (evilem-create #'evil-jump-out-args))
    (define-key evilem-map "s" #'evil-avy-goto-char-2)
    (define-key evilem-map "/" #'evil-avy-goto-char-timer)
    (define-key evilem-map (kbd "SPC") #'buffer-evil-avy-goto-char-timer)

    ;; Provide proper prefixes for which key
    (which-key-add-keymap-based-replacements evil-motion-state-map
      "gs"  "evil-easymotion")
    (which-key-add-keymap-based-replacements evilem-map
      "g" "misc"
      "[" "section backward"
      "]" "section forward")

    ;; Use evil-search backend, instead of isearch
    (evilem-make-motion evilem-motion-search-next #'evil-ex-search-next
                        :bind ((evil-ex-search-highlight-all nil)))
    (evilem-make-motion evilem-motion-search-previous #'evil-ex-search-previous
                        :bind ((evil-ex-search-highlight-all nil)))
    (evilem-make-motion evilem-motion-search-word-forward #'evil-ex-search-word-forward
                        :bind ((evil-ex-search-highlight-all nil)))
    (evilem-make-motion evilem-motion-search-word-backward #'evil-ex-search-word-backward
                        :bind ((evil-ex-search-highlight-all nil)))))

(defun spacemacs-editing/init-password-generator ()
  (use-package password-generator
    :defer t
    :init
    (spacemacs/declare-prefix "ip" "passwords")
    (evil-leader/set-key
      "ip1" 'password-generator-simple
      "ip2" 'password-generator-strong
      "ip3" 'password-generator-paranoid
      "ipp" 'password-generator-phonetic
      "ipn" 'password-generator-numeric)))

(defun spacemacs-editing/post-init-pcre2el ()
  (spacemacs/declare-prefix
    "xr"  "regular expressions"
    "xre" "elisp"
    "xrp" "pcre")
  (spacemacs/set-leader-keys
    "xr/"  'rxt-explain
    "xr'"  'rxt-convert-to-strings
    "xrt"  'rxt-toggle-elisp-rx
    "xrx"  'rxt-convert-to-rx
    "xrc"  'rxt-convert-syntax
    "xre/" 'rxt-explain-elisp
    "xre'" 'rxt-elisp-to-strings
    "xrep" 'rxt-elisp-to-pcre
    "xret" 'rxt-toggle-elisp-rx
    "xrex" 'rxt-elisp-to-rx
    "xrp/" 'rxt-explain-pcre
    "xrp'" 'rxt-pcre-to-strings
    "xrpe" 'rxt-pcre-to-elisp
    "xrpx" 'rxt-pcre-to-rx))

(defun spacemacs-editing/init-smartparens ()
  (use-package smartparens
    :defer t
    :commands (sp-point-in-string-or-comment sp-forward-symbol sp-split-sexp sp-newline sp-up-sexp)
    :init
    ;; settings
    (setq sp-show-pair-delay
          ;; Use this form to allow users to override this setting from
          ;; dotspacemacs/user-init
          (or (bound-and-true-p sp-show-pair-delay) 0.2)
          ;; fix paren highlighting in normal mode
          sp-show-pair-from-inside t
          sp-cancel-autoskip-on-backward-movement nil
          sp-highlight-pair-overlay nil
          sp-highlight-wrap-overlay nil
          sp-highlight-wrap-tag-overlay nil)
    (spacemacs/add-to-hooks #'spacemacs//activate-smartparens
                            '(prog-mode-hook comint-mode-hook))
    ;; enable smartparens-mode in `eval-expression'
    (add-hook 'minibuffer-setup-hook 'spacemacs//conditionally-enable-smartparens-mode)
    ;; toggles
    (spacemacs|add-toggle smartparens
      :status (or (bound-and-true-p smartparens-mode)
                  (bound-and-true-p smartparens-strict-mode))
      :on (spacemacs//activate-smartparens)
      :off (spacemacs//deactivate-smartparens)
      :documentation "Enable smartparens."
      :evil-leader "tp")
    (spacemacs|add-toggle smartparens-globally
      :status (or smartparens-global-mode smartparens-global-strict-mode)
      :on (spacemacs//activate-smartparens t)
      :off (spacemacs//deactivate-smartparens t)
      :documentation "Enable smartparens globally."
      :evil-leader "t C-p")
    ;; key bindings
    (spacemacs/set-leader-keys
      "js" 'sp-split-sexp
      "jn" 'sp-newline)
    :config
    (require 'smartparens-config)
    (spacemacs|diminish smartparens-mode " ⓟ" " p")
    (spacemacs//adaptive-smartparent-pair-overlay-face)
    (add-hook 'spacemacs-post-theme-change-hook
              'spacemacs//adaptive-smartparent-pair-overlay-face)
    (show-smartparens-global-mode +1)
    ;; don't create a pair with single quote in minibuffer
    (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
    (sp-pair "{" nil :post-handlers
              '(:add (spacemacs/smartparens-pair-newline-and-indent "RET")))
    (sp-pair "[" nil :post-handlers
              '(:add (spacemacs/smartparens-pair-newline-and-indent "RET")))
    (when dotspacemacs-smart-closing-parenthesis
      (define-key evil-insert-state-map ")"
        'spacemacs/smart-closing-parenthesis))))

(defun spacemacs-editing/init-spacemacs-whitespace-cleanup ()
  (use-package spacemacs-whitespace-cleanup
    :commands (spacemacs-whitespace-cleanup-mode
               global-spacemacs-whitespace-cleanup-mode)
    :init
    (spacemacs|add-toggle whitespace-cleanup
      :mode spacemacs-whitespace-cleanup-mode
      :documentation "Automatic whitespace clean up."
      :on-message (spacemacs-whitespace-cleanup/on-message)
      :evil-leader "tW")
    (spacemacs|add-toggle global-whitespace-cleanup
      :mode global-spacemacs-whitespace-cleanup-mode
      :status spacemacs-whitespace-cleanup-mode
      :on (let ((spacemacs-whitespace-cleanup-globally t))
            (spacemacs-whitespace-cleanup-mode))
      :off (let ((spacemacs-whitespace-cleanup-globally t))
              (spacemacs-whitespace-cleanup-mode -1))
      :on-message (spacemacs-whitespace-cleanup/on-message t)
      :documentation "Global automatic whitespace clean up."
      :evil-leader "t C-S-w")
    (with-eval-after-load 'ws-butler
      (when dotspacemacs-whitespace-cleanup
        (spacemacs/toggle-global-whitespace-cleanup-on)))
    :config
    (spacemacs|diminish spacemacs-whitespace-cleanup-mode " Ⓦ" " W")
    (spacemacs|diminish global-spacemacs-whitespace-cleanup-mode
                        " Ⓦ" " W")))

(defun spacemacs-editing/init-string-inflection ()
  (use-package string-inflection
    :init
    (spacemacs|define-transient-state string-inflection
      :title "String Inflection Transient State"
      :doc "\n [_i_] cycle"
      :bindings
      ("i" string-inflection-all-cycle))
    (spacemacs/declare-prefix "xi" "inflection")
    (spacemacs/set-leader-keys
      "xic" 'string-inflection-lower-camelcase
      "xiC" 'string-inflection-camelcase
      "xii" 'spacemacs/string-inflection-transient-state/body
      "xi-" 'string-inflection-kebab-case
      "xik" 'string-inflection-kebab-case
      "xi_" 'string-inflection-underscore
      "xiu" 'string-inflection-underscore
      "xiU" 'string-inflection-upcase)))

(defun spacemacs-editing/init-string-edit-at-point ()
  (use-package string-edit-at-point
    :defer t
    :init
    (spacemacs/set-leader-keys "xe" 'string-edit-at-point)
    :config
    (spacemacs/set-leader-keys-for-minor-mode 'string-edit-at-point-mode
      "," 'string-edit-conclude
      "c" 'string-edit-conclude
      "a" 'string-edit-abort
      "k" 'string-edit-abort)))

(defun spacemacs-editing/init-multi-line ()
  (use-package multi-line
    :defer t
    :init
    (spacemacs|define-transient-state multi-line
      :title "Multi-line Transient State"
      :doc "\n [_n_] cycle"
      :bindings
      ("n" multi-line))
    (spacemacs/set-leader-keys
      "xn" 'spacemacs/multi-line-transient-state/body)))

(defun spacemacs-editing/init-undo-tree ()
  (use-package undo-tree
    :defer t
    :init
    (setq undo-tree-visualizer-timestamps t
          undo-tree-visualizer-diff t
          ;; See `vim-style-enable-undo-region'.
          undo-tree-enable-undo-in-region t
          ;; 10X bump of the undo limits to avoid issues with premature
          ;; Emacs GC which truncages the undo history very aggresively
          undo-limit 800000
          undo-strong-limit 12000000
          undo-outer-limit 120000000
          undo-tree-history-directory-alist
          `(("." . ,(let ((dir (expand-file-name "undo-tree-history" spacemacs-cache-directory)))
                      (if (file-exists-p dir)
                          (unless (file-accessible-directory-p dir)
                            (warn "Cannot access directory `%s'.
Perhaps you don't have required permissions, or it's not a directory.
See variable `undo-tree-history-directory-alist'." dir))
                        (make-directory dir))
                      dir))))
    (global-undo-tree-mode)
    :config
    ;; restore diff window after quit.  TODO fix upstream
    (defun spacemacs/undo-tree-restore-default ()
      (setq undo-tree-visualizer-diff t))
    (advice-add 'undo-tree-visualizer-quit :after #'spacemacs/undo-tree-restore-default)
    (spacemacs|hide-lighter undo-tree-mode)
    (evilified-state-evilify-map undo-tree-visualizer-mode-map
      :mode undo-tree-visualizer-mode
      :bindings
      (kbd "j") 'undo-tree-visualize-redo
      (kbd "k") 'undo-tree-visualize-undo
      (kbd "h") 'undo-tree-visualize-switch-branch-left
      (kbd "l") 'undo-tree-visualize-switch-branch-right)))

(defun spacemacs-editing/init-uuidgen ()
  (use-package uuidgen
    :commands (uuidgen-1 uuidgen-4)
    :init
    (spacemacs/declare-prefix "iU" "uuid")
    (spacemacs/set-leader-keys
      "iU1" 'spacemacs/uuidgen-1
      "iU4" 'spacemacs/uuidgen-4
      "iUU" 'spacemacs/uuidgen-4)))

(defun spacemacs-editing/init-ws-butler ()
  ;; not deferred on purpose, init-spacemacs-whitespace-cleanup need
  ;; it to be loaded.
  (use-package ws-butler
    :config (spacemacs|hide-lighter ws-butler-mode)))

(defun spacemacs-editing/init-evil-swap-keys ()
  (use-package evil-swap-keys
    :defer t
    :init
    (setq evil-swap-keys-number-row-keys
          (pcase dotspacemacs-swap-number-row
            ('qwerty-us '(("1" . "!")
                          ("2" . "@")
                          ("3" . "#")
                          ("4" . "$")
                          ("5" . "%")
                          ("6" . "^")
                          ("7" . "&")
                          ("8" . "*")
                          ("9" . "(")
                          ("0" . ")")))
            ('qwertz-de '(("1" . "!")
                          ("2" . "\"")
                          ("3" . "§")
                          ("4" . "$")
                          ("5" . "%")
                          ("6" . "&")
                          ("7" . "/")
                          ("8" . "(")
                          ("9" . ")")
                          ("0" . "=")))
            ('qwerty-ca-fr '(("1" . "!")
                             ("2" . "@")
                             ("3" . "#")
                             ("4" . "$")
                             ("5" . "%")
                             ("6" . "?")
                             ("7" . "&")
                             ("8" . "*")
                             ("9" . "(")
                             ("0" . ")")))
            (x (message "dotspacemacs-swap-number-row %s is not supported." x))))
    (add-hook 'prog-mode-hook #'evil-swap-keys-swap-number-row)))

(defun spacemacs-editing/init-persistent-scratch ()
  (use-package persistent-scratch
    :defer t
    :init
    (setq persistent-scratch-save-file (concat spacemacs-cache-directory ".persistent-scratch")
          persistent-scratch-autosave-interval 60
          persistent-scratch-what-to-save '(point narrowing))
    (add-hook 'spacemacs-scratch-mode-hook 'persistent-scratch-mode)
    (persistent-scratch-autosave-mode t)))

(defun spacemacs-editing/init-unkillable-scratch ()
  (use-package unkillable-scratch
    :defer t
    :init
    (setq unkillable-scratch-do-not-reset-scratch-buffer t)
    (unkillable-scratch dotspacemacs-scratch-buffer-unkillable)))
