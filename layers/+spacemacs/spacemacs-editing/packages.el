;;; packages.el --- Space-macs Editing Layer packages File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(setq space-macs-editing-packages
      '(aggressive-indent
        avy
        (bracketed-paste :toggle (version<= e-macs-version "25.0.92"))
        (clean-aindent-mode :toggle dotspace-macs-use-clean-aindent-mode)
        dired-quick-sort
        editorconfig
        eval-sexp-fu
        expand-region
        (hexl :location built-in)
        hungry-delete
        link-hint
        lorem-ipsum
        move-text
        (origami :toggle (eq 'origami dotspace-macs-folding-method))
        password-generator
        (persistent-scratch :toggle dotspace-macs-scratch-buffer-persistent)
        pcre2el
        smartparens
        (evil-swap-keys :toggle dotspace-macs-swap-number-row)
        (space-macs-whitespace-cleanup :location local)
        string-inflection
        undo-tree
        (unkillable-scratch :toggle dotspace-macs-scratch-buffer-unkillable)
        uuidgen
        (vimish-fold :toggle (eq 'vimish dotspace-macs-folding-method))
        (evil-vimish-fold :toggle (eq 'vimish dotspace-macs-folding-method))
        (evil-easymotion :toggle (memq dotspace-macs-editing-style '(vim hybrid)))
        ws-butler))

;; Initialization of packages
(defun space-macs-editing/init-aggressive-indent ()
  (use-package aggressive-indent
    :defer t
    :init
    (progn
      (space-macs|add-toggle aggressive-indent
        :mode aggressive-indent-mode
        :documentation "Always keep code indented."
        :evil-leader "tI")
      (space-macs|add-toggle aggressive-indent-globally
        :mode global-aggressive-indent-mode
        :documentation "Always keep code indented globally."
        :evil-leader "t C-I"))
    :config
    (progn
      (add-hook 'diff-auto-refine-mode-hook 'space-macs/toggle-aggressive-indent-off)
      (space-macs|diminish aggressive-indent-mode " â’¾" " I"))))

(defun space-macs-editing/init-avy ()
  (use-package avy
    :defer t
    :commands (space-macs/avy-open-url space-macs/avy-goto-url avy-pop-mark avy-with)
    :init
    (progn
      (setq avy-all-windows 'all-frames)
      (setq avy-background t)
      (space-macs/set-leader-keys
        "jb" 'avy-pop-mark
        "jj" 'evil-avy-goto-char-timer
        "jl" 'evil-avy-goto-line
        "ju" 'space-macs/avy-goto-url
        "jU" 'space-macs/avy-open-url
        "jw" 'evil-avy-goto-word-or-subword-1
        "xo" 'space-macs/avy-open-url))
    :config
    (progn
      (defun space-macs/avy-goto-url()
        "Use avy to go to an URL in the buffer."
        (interactive)
        (avy-jump "https?://"))
      (defun space-macs/avy-open-url ()
        "Use avy to select an URL in the buffer and open it."
        (interactive)
        (save-excursion
          (space-macs/avy-goto-url)
          (browse-url-at-point))))))

(defun space-macs-editing/init-bracketed-paste ()
  (use-package bracketed-paste
    :defer t
    :init
    ;; Enable bracketed-paste for tty
    (add-hook 'tty-setup-hook 'bracketed-paste-enable)))

(defun space-macs-editing/init-clean-aindent-mode ()
  (use-package clean-aindent-mode
    :config
    (progn
      (clean-aindent-mode)
      (add-hook 'prog-mode-hook 'space-macs//put-clean-aindent-last t))))

(defun space-macs-editing/init-dired-quick-sort ()
  (use-package dired-quick-sort
    :defer t
    :init
    (dired-quick-sort-setup)))

(defun space-macs-editing/init-editorconfig ()
  (use-package editorconfig
    :init
    (space-macs|diminish editorconfig-mode)
    :config
    (editorconfig-mode t)))

(defun space-macs-editing/init-eval-sexp-fu ()
  (use-package eval-sexp-fu
    :commands eval-sexp-fu-flash-mode))

;; ;; ignore obsolete function warning generated on startup
;; (let ((byte-compile-not-obsolete-funcs (append byte-compile-not-obsolete-funcs '(preceding-sexp))))
;;   (require 'eval-sexp-fu)))

(defun space-macs-editing/init-expand-region ()
  (use-package expand-region
    :defer t
    :init (space-macs/set-leader-keys "v" 'er/expand-region)
    :config
    (progn
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
                      'space-macs/helm-project-smart-do-search-region-or-symbol)))
             new-bindings)
            (cl-pushnew
             '("f" (lambda ()
                     (call-interactively
                      'space-macs/helm-files-smart-do-search-region-or-symbol)))
             new-bindings)
            (cl-pushnew
             '("b" (lambda ()
                     (call-interactively
                      'space-macs/helm-buffers-smart-do-search-region-or-symbol)))
             new-bindings)
            (setq ad-return-value (cons new-msg new-bindings)))))
      (setq expand-region-contract-fast-key "V"
            expand-region-reset-fast-key "r"))))

(defun space-macs-editing/init-hexl ()
  (use-package hexl
    :defer t
    :init
    (progn
      (space-macs/set-leader-keys "fh" 'hexl-find-file)
      (space-macs/set-leader-keys-for-major-mode 'hexl-mode
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
        "0" 'hexl-beginning-of-line))))

(defun space-macs-editing/init-hungry-delete ()
  (use-package hungry-delete
    :defer t
    :init
    (space-macs|add-toggle hungry-delete
      :mode hungry-delete-mode
      :documentation "Delete consecutive horizontal whitespace with a single key."
      :evil-leader "td")
    :config
    (progn
      (setq-default hungry-delete-chars-to-skip " \t\f\v") ; only horizontal whitespace
      (define-key hungry-delete-mode-map (kbd "DEL") 'hungry-delete-backward)
      (define-key hungry-delete-mode-map (kbd "S-DEL") 'delete-backward-char))))

(defun space-macs-editing/init-link-hint ()
  (use-package link-hint
    :defer t
    :init
    (space-macs/set-leader-keys
      "xA" 'link-hint-open-all-links
      "xm" 'link-hint-open-multiple-links
      "xo" 'link-hint-open-link-at-point
      "xO" 'link-hint-open-link
      "xy" 'link-hint-copy-link-at-point
      "xY" 'link-hint-copy-link)))

(defun space-macs-editing/init-lorem-ipsum ()
  (use-package lorem-ipsum
    :commands (lorem-ipsum-insert-list
               lorem-ipsum-insert-paragraphs
               lorem-ipsum-insert-sentences)
    :init
    (progn
      (space-macs/declare-prefix "il" "lorem ipsum")
      (space-macs/set-leader-keys
        "ill" 'lorem-ipsum-insert-list
        "ilp" 'lorem-ipsum-insert-paragraphs
        "ils" 'lorem-ipsum-insert-sentences))))

(defun space-macs-editing/init-move-text ()
  (use-package move-text
    :defer t
    :init
    (space-macs|define-transient-state move-text
      :title "Move Text Transient State"
      :bindings
      ("J" move-text-down "move down")
      ("K" move-text-up "move up"))
    (space-macs/set-leader-keys
      "xJ" 'space-macs/move-text-transient-state/move-text-down
      "xK" 'space-macs/move-text-transient-state/move-text-up)))

(defun space-macs-editing/init-origami ()
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

      (space-macs|define-transient-state fold
        :title "Code Fold Transient State"
        :doc "
 Close^^            Open^^             Toggle^^         Goto^^         Other^^
 â”€â”€â”€â”€â”€â”€â”€^^â”€â”€â”€â”€â”€â”€â”€â”€â”€ â”€â”€â”€â”€â”€^^â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€â”€ â”€â”€â”€â”€â”€^^â”€â”€â”€â”€â”€â”€â”€â”€â”€ â”€â”€â”€â”€â”€â”€^^â”€â”€â”€â”€â”€â”€ â”€â”€â”€â”€â”€^^â”€â”€â”€â”€â”€â”€â”€â”€â”€
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

(defun space-macs-editing/init-vimish-fold ()
  (use-package vimish-fold
    :ensure
    :after evil))

(defun space-macs-editing/init-evil-vimish-fold ()
  (use-package evil-vimish-fold
    :ensure
    :after vimish-fold
    :init
    (setq evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode))
    :config (global-evil-vimish-fold-mode)))

(defun space-macs-editing/init-evil-easymotion ()
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

(defun space-macs-editing/init-password-generator ()
  (use-package password-generator
    :defer t
    :init
    (progn
      (space-macs/declare-prefix "ip" "passwords")
      (evil-leader/set-key
        "ip1" 'password-generator-simple
        "ip2" 'password-generator-strong
        "ip3" 'password-generator-paranoid
        "ipp" 'password-generator-phonetic
        "ipn" 'password-generator-numeric))))

(defun space-macs-editing/post-init-pcre2el ()
  (space-macs/declare-prefix "xr" "regular expressions")
  (space-macs/declare-prefix "xre" "elisp")
  (space-macs/declare-prefix "xrp" "pcre")
  (space-macs/set-leader-keys
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

(defun space-macs-editing/init-smartparens ()
  (use-package smartparens
    :defer t
    :commands (sp-split-sexp sp-newline sp-up-sexp)
    :init
    (progn
      ;; settings
      (setq sp-show-pair-delay
            ;; Use this form to allow users to override this setting from
            ;; dotspace-macs/user-init
            (or (bound-and-true-p sp-show-pair-delay) 0.2)
            ;; fix paren highlighting in normal mode
            sp-show-pair-from-inside t
            sp-cancel-autoskip-on-backward-movement nil
            sp-highlight-pair-overlay nil
            sp-highlight-wrap-overlay nil
            sp-highlight-wrap-tag-overlay nil)
      (space-macs/add-to-hooks (if dotspace-macs-smartparens-strict-mode
                                  'smartparens-strict-mode
                                'smartparens-mode)
                              '(prog-mode-hook comint-mode-hook))
      ;; enable smartparens-mode in `eval-expression'
      (add-hook 'minibuffer-setup-hook 'space-macs//conditionally-enable-smartparens-mode)
      ;; toggles
      (space-macs|add-toggle smartparens
        :mode smartparens-mode
        :documentation "Enable smartparens."
        :evil-leader "tp")
      (space-macs|add-toggle smartparens-globally
        :mode smartparens-global-mode
        :documentation "Enable smartparens globally."
        :evil-leader "t C-p")
      ;; key bindings
      (space-macs/set-leader-keys
        "js" 'sp-split-sexp
        "jn" 'sp-newline))
    :config
    (progn
      (require 'smartparens-config)
      (space-macs|diminish smartparens-mode " â“Ÿ" " p")
      (space-macs//adaptive-smartparent-pair-overlay-face)
      (add-hook 'space-macs-post-theme-change-hook
                'space-macs//adaptive-smartparent-pair-overlay-face)
      (show-smartparens-global-mode +1)
      ;; don't create a pair with single quote in minibuffer
      (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
      (sp-pair "{" nil :post-handlers
               '(:add (space-macs/smartparens-pair-newline-and-indent "RET")))
      (sp-pair "[" nil :post-handlers
               '(:add (space-macs/smartparens-pair-newline-and-indent "RET")))
      (when dotspace-macs-smart-closing-parenthesis
        (define-key evil-insert-state-map ")"
          'space-macs/smart-closing-parenthesis)))))

(defun space-macs-editing/init-space-macs-whitespace-cleanup ()
  (use-package space-macs-whitespace-cleanup
    :commands (space-macs-whitespace-cleanup-mode
               global-space-macs-whitespace-cleanup-mode)
    :init
    (progn
      (space-macs|add-toggle whitespace-cleanup
        :mode space-macs-whitespace-cleanup-mode
        :documentation "Automatic whitespace clean up."
        :on-message (space-macs-whitespace-cleanup/on-message)
        :evil-leader "tW")
      (space-macs|add-toggle global-whitespace-cleanup
        :mode global-space-macs-whitespace-cleanup-mode
        :status space-macs-whitespace-cleanup-mode
        :on (let ((space-macs-whitespace-cleanup-globally t))
              (space-macs-whitespace-cleanup-mode))
        :off (let ((space-macs-whitespace-cleanup-globally t))
               (space-macs-whitespace-cleanup-mode -1))
        :on-message (space-macs-whitespace-cleanup/on-message t)
        :documentation "Global automatic whitespace clean up."
        :evil-leader "t C-S-w")
      (with-eval-after-load 'ws-butler
        (when dotspace-macs-whitespace-cleanup
          (space-macs/toggle-global-whitespace-cleanup-on))))
    :config
    (progn
      (space-macs|diminish space-macs-whitespace-cleanup-mode " â“Œ" " W")
      (space-macs|diminish global-space-macs-whitespace-cleanup-mode
                          " â“Œ" " W"))))

(defun space-macs-editing/init-string-inflection ()
  (use-package string-inflection
    :init
    (progn
      (space-macs|define-transient-state string-inflection
        :title "String Inflection Transient State"
        :doc "\n [_i_] cycle"
        :bindings
        ("i" string-inflection-all-cycle))
      (space-macs/declare-prefix "xi" "inflection")
      (space-macs/set-leader-keys
        "xic" 'string-inflection-lower-camelcase
        "xiC" 'string-inflection-camelcase
        "xii" 'space-macs/string-inflection-transient-state/body
        "xi-" 'string-inflection-kebab-case
        "xik" 'string-inflection-kebab-case
        "xi_" 'string-inflection-underscore
        "xiu" 'string-inflection-underscore
        "xiU" 'string-inflection-upcase))))

(defun space-macs-editing/init-undo-tree ()
  (use-package undo-tree
    :defer t
    :init
    (progn
      (setq undo-tree-visualizer-timestamps t
            undo-tree-visualizer-diff t
            ;; 10X bump of the undo limits to avoid issues with premature
            ;; e-macs GC which truncages the undo history very aggresively
            undo-limit 800000
            undo-strong-limit 12000000
            undo-outer-limit 120000000)
      (global-undo-tree-mode))
    :config
    (progn
      ;; restore diff window after quit.  TODO fix upstream
      (defun space-macs/undo-tree-restore-default ()
        (setq undo-tree-visualizer-diff t))
      (advice-add 'undo-tree-visualizer-quit :after #'space-macs/undo-tree-restore-default)
      (space-macs|hide-lighter undo-tree-mode)
      (evilified-state-evilify-map undo-tree-visualizer-mode-map
        :mode undo-tree-visualizer-mode
        :bindings
        (kbd "j") 'undo-tree-visualize-redo
        (kbd "k") 'undo-tree-visualize-undo
        (kbd "h") 'undo-tree-visualize-switch-branch-left
        (kbd "l") 'undo-tree-visualize-switch-branch-right))))

(defun space-macs-editing/init-uuidgen ()
  (use-package uuidgen
    :commands (uuidgen-1 uuidgen-4)
    :init
    (progn
      (space-macs/declare-prefix "iU" "uuid")
      (space-macs/set-leader-keys
        "iU1" 'space-macs/uuidgen-1
        "iU4" 'space-macs/uuidgen-4
        "iUU" 'space-macs/uuidgen-4))))

(defun space-macs-editing/init-ws-butler ()
  ;; not deferred on purpose, init-space-macs-whitespace-cleanup need
  ;; it to be loaded.
  (use-package ws-butler
    :config (space-macs|hide-lighter ws-butler-mode)))

(defun space-macs-editing/init-evil-swap-keys ()
  (use-package evil-swap-keys
    :defer t
    :init
    (progn
      (pcase dotspace-macs-swap-number-row
        (`qwerty-us (setq evil-swap-keys-number-row-keys  '(("1" . "!")
                                                            ("2" . "@")
                                                            ("3" . "#")
                                                            ("4" . "$")
                                                            ("5" . "%")
                                                            ("6" . "^")
                                                            ("7" . "&")
                                                            ("8" . "*")
                                                            ("9" . "(")
                                                            ("0" . ")"))))
        (`qwertz-de (setq evil-swap-keys-number-row-keys  '(("1" . "!")
                                                            ("2" . "\"")
                                                            ("3" . "Â§")
                                                            ("4" . "$")
                                                            ("5" . "%")
                                                            ("6" . "&")
                                                            ("7" . "/")
                                                            ("8" . "(")
                                                            ("9" . ")")
                                                            ("0" . "="))))
        (`qwerty-ca-fr (setq evil-swap-keys-number-row-keys  '(("1" . "!")
                                                               ("2" . "@")
                                                               ("3" . "#")
                                                               ("4" . "$")
                                                               ("5" . "%")
                                                               ("6" . "?")
                                                               ("7" . "&")
                                                               ("8" . "*")
                                                               ("9" . "(")
                                                               ("0" . ")"))))
        (_ (message "dotspace-macs-swap-number-row %s is not supported." dotspace-macs-swap-number-row)))
      (add-hook 'prog-mode-hook #'evil-swap-keys-swap-number-row))))

(defun space-macs-editing/init-persistent-scratch ()
  (use-package persistent-scratch
    :defer t
    :init
    (progn
      (setq persistent-scratch-save-file (concat space-macs-cache-directory ".persistent-scratch")
            persistent-scratch-autosave-interval 60
            persistent-scratch-what-to-save '(point narrowing))
      (add-hook 'space-macs-scratch-mode-hook 'persistent-scratch-mode)
      (persistent-scratch-autosave-mode t))))

(defun space-macs-editing/init-unkillable-scratch ()
  (use-package unkillable-scratch
    :defer t
    :init
    (progn
      (setq unkillable-scratch-do-not-reset-scratch-buffer t)
      (unkillable-scratch dotspace-macs-scratch-buffer-unkillable))))


