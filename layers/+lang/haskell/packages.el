;;; packages.el --- Haskell Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq haskell-packages
  '(
    cmm-mode
    company
    company-ghc
    company-cabal
    flycheck
    flycheck-haskell
    ghc
    haskell-mode
    haskell-snippets
    hindent
    shm
    ))

(defun haskell/init-cmm-mode ()
  (use-package cmm-mode
    :defer t))

(defun haskell/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'haskell-mode-hook))

(when (configuration-layer/layer-usedp 'syntax-checking)
  (defun haskell/init-flycheck-haskell ()
    (use-package flycheck-haskell
      :if (configuration-layer/package-usedp 'flycheck)
      :commands flycheck-haskell-configure
      :init (add-hook 'flycheck-mode-hook 'flycheck-haskell-configure))))

(defun haskell/init-ghc ()
  (use-package ghc
    :defer t
    :if haskell-enable-ghc-mod-support
    :init (add-hook 'haskell-mode-hook 'ghc-init)
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'haskell-mode "mm" "haskell/ghc-mod")
      (spacemacs/set-leader-keys-for-major-mode 'haskell-mode
          "mt" 'ghc-insert-template-or-signature
          "mu" 'ghc-initial-code-from-signature
          "ma" 'ghc-auto
          "mf" 'ghc-refine
          "me" 'ghc-expand-th
          "mn" 'ghc-goto-next-hole
          "mp" 'ghc-goto-prev-hole
          "m>"  'ghc-make-indent-deeper
          "m<"  'ghc-make-indent-shallower)
      (when (configuration-layer/package-usedp 'flycheck)
        ;; remove overlays from ghc-check.el if flycheck is enabled
        (set-face-attribute 'ghc-face-error nil :underline nil)
        (set-face-attribute 'ghc-face-warn nil :underline nil)))))

(defun haskell/init-haskell-mode ()
  (use-package haskell-mode
    :defer t
    :init
    (progn
      (defun spacemacs//force-haskell-mode-loading ()
        "Force `haskell-mode' loading when visiting cabal file."
        (require 'haskell-mode))
      (add-hook 'haskell-cabal-mode-hook
                'spacemacs//force-haskell-mode-loading))

    (setq
     ;; Use notify.el (if you have it installed) at the end of running
     ;; Cabal commands or generally things worth notifying.
     haskell-notify-p t
     ;; To enable tags generation on save.
     haskell-tags-on-save t
     ;; Remove annoying error popups
     haskell-interactive-popup-errors nil
     ;; Better import handling
     haskell-process-suggest-remove-import-lines t
     haskell-process-auto-import-loaded-modules t
     ;; Disable haskell-stylish-on-save, as it breaks flycheck highlighting.
     ;; NOTE: May not be true anymore - taksuyu 2015-10-06
     haskell-stylish-on-save nil)
    :config
    (progn
      ;; Haskell main editing mode key bindings.
      (defun spacemacs/init-haskell-mode ()
        ;; use only internal indentation system from haskell
        (if (fboundp 'electric-indent-local-mode)
            (electric-indent-local-mode -1))
        (when haskell-enable-shm-support
          ;; in structured-haskell-mode line highlighting creates noise
          (setq-local global-hl-line-mode nil)))

      (defun spacemacs/haskell-interactive-bring ()
        "Bring up the interactive mode for this session without
         switching to it."
        (interactive)
        (let* ((session (haskell-session))
               (buffer (haskell-session-interactive-buffer session)))
          (display-buffer buffer)))

      ;; hooks
      (add-hook 'haskell-mode-hook 'spacemacs/init-haskell-mode)
      (unless haskell-enable-ghc-mod-support
        (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

      ;; prefixes
      (spacemacs/declare-prefix-for-mode 'haskell-mode "mg" "haskell/navigation")
      (spacemacs/declare-prefix-for-mode 'haskell-mode "ms" "haskell/repl")
      (spacemacs/declare-prefix-for-mode 'haskell-mode "mc" "haskell/cabal")
      (spacemacs/declare-prefix-for-mode 'haskell-mode "mh" "haskell/documentation")
      (spacemacs/declare-prefix-for-mode 'haskell-mode "md" "haskell/debug")
      (spacemacs/declare-prefix-for-mode 'haskell-interactive-mode "ms" "haskell/repl")
      (spacemacs/declare-prefix-for-mode 'haskell-cabal-mode "ms" "haskell/repl")

      ;; key bindings
      (defun spacemacs/haskell-process-do-type-on-prev-line ()
        (interactive)
        (if haskell-enable-ghci-ng-support
            (haskell-mode-show-type-at 1)
          (haskell-process-do-type 1)))

      (spacemacs/set-leader-keys-for-major-mode 'haskell-mode
        "gg"  'haskell-mode-jump-to-def-or-tag
        "gi"  'haskell-navigate-imports
        "f"   'haskell-mode-stylish-buffer

        "sb"  'haskell-process-load-file
        "sc"  'haskell-interactive-mode-clear
        "ss"  'spacemacs/haskell-interactive-bring
        "sS"  'haskell-interactive-switch

        "ca"  'haskell-process-cabal
        "cb"  'haskell-process-cabal-build
        "cc"  'haskell-compile
        "cv"  'haskell-cabal-visit-file

        "hd"  'inferior-haskell-find-haddock
        "hh"  'hoogle
        "hH"  'haskell-hoogle-lookup-from-local
        "hi"  (lookup-key haskell-mode-map (kbd "C-c TAB"))
        "ht"  (lookup-key haskell-mode-map (kbd "C-c C-t"))
        "hT"  'spacemacs/haskell-process-do-type-on-prev-line
        "hy"  'hayoo

        "dd"  'haskell-debug
        "db"  'haskell-debug/break-on-function
        "dn"  'haskell-debug/next
        "dN"  'haskell-debug/previous
        "dB"  'haskell-debug/delete
        "dc"  'haskell-debug/continue
        "da"  'haskell-debug/abandon
        "dr"  'haskell-debug/refresh)

      ;; configure C-c C-l so it doesn't throw any errors
      (bind-key "C-c C-l" 'haskell-process-load-file haskell-mode-map)

      ;; Switch back to editor from REPL
      (spacemacs/set-leader-keys-for-major-mode 'haskell-interactive-mode
        "sS"  'haskell-interactive-switch-back)

      ;; Compile
      (spacemacs/set-leader-keys-for-major-mode 'haskell-cabal
        "C"  'haskell-compile)

      ;; Cabal-file bindings
      (spacemacs/set-leader-keys-for-major-mode 'haskell-cabal-mode
        ;; "="   'haskell-cabal-subsection-arrange-lines ;; Does a bad job, 'gg=G' works better
        "d"   'haskell-cabal-add-dependency
        "b"   'haskell-cabal-goto-benchmark-section
        "e"   'haskell-cabal-goto-executable-section
        "t"   'haskell-cabal-goto-test-suite-section
        "m"   'haskell-cabal-goto-exposed-modules
        "l"   'haskell-cabal-goto-library-section
        "n"   'haskell-cabal-next-subsection
        "p"   'haskell-cabal-previous-subsection
        "sc"  'haskell-interactive-mode-clear
        "ss"  'spacemacs/haskell-interactive-bring
        "sS"  'haskell-interactive-switch
        "N"   'haskell-cabal-next-section
        "P"   'haskell-cabal-previous-section
        "f"   'haskell-cabal-find-or-create-source-file)

      ;; Make "RET" behaviour in REPL saner
      (evil-define-key 'insert haskell-interactive-mode-map
        (kbd "RET") 'haskell-interactive-mode-return)
      (evil-define-key 'normal haskell-interactive-mode-map
        (kbd "RET") 'haskell-interactive-mode-return)

      ;;GHCi-ng
      (when haskell-enable-ghci-ng-support
        ;; haskell-process-type is set to auto, so setup ghci-ng for either case
        ;; if haskell-process-type == cabal-repl
        (setq haskell-process-args-cabal-repl '("--ghc-option=-ferror-spans" "--with-ghc=ghci-ng"))
        ;; if haskell-process-type == GHCi
        (setq haskell-process-path-ghci "ghci-ng")
        ;; fixes ghci-ng for stack projects
        (setq haskell-process-wrapper-function
              (lambda (args)
                (append args (list "--with-ghc" "ghci-ng"))))

        (spacemacs/set-leader-keys-for-major-mode 'haskell-mode
          ;; function suggested in
          ;; https://github.com/chrisdone/ghci-ng#using-with-haskell-mode
          "u"   'haskell-mode-find-uses
          "ht"  'haskell-mode-show-type-at
          "gg"  'haskell-mode-goto-loc))

      ;; Useful to have these keybindings for .cabal files, too.
      (with-eval-after-load 'haskell-cabal-mode-map
        (define-key haskell-cabal-mode-map
          [?\C-c ?\C-z] 'haskell-interactive-switch))))

  ;; align rules for Haskell
  (with-eval-after-load 'align
    (add-to-list 'align-rules-list
                 '(haskell-types
                   (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                   (modes . '(haskell-mode literate-haskell-mode))))
    (add-to-list 'align-rules-list
                 '(haskell-assignment
                   (regexp . "\\(\\s-+\\)=\\s-+")
                   (modes . '(haskell-mode literate-haskell-mode))))
    (add-to-list 'align-rules-list
                 '(haskell-arrows
                   (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                   (modes . '(haskell-mode literate-haskell-mode))))
    (add-to-list 'align-rules-list
                 '(haskell-left-arrows
                   (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                   (modes . '(haskell-mode literate-haskell-mode))))))

(defun haskell/init-haskell-snippets ()
  ;; manually load the package since the current implementation is not lazy
  ;; loading friendly (funny coming from the haskell mode :-))
  (setq haskell-snippets-dir (spacemacs//get-package-directory
                              'haskell-snippets))

  (defun haskell-snippets-initialize ()
    (let ((snip-dir (expand-file-name "snippets" haskell-snippets-dir)))
      (add-to-list 'yas-snippet-dirs snip-dir t)
      (yas-load-directory snip-dir)))

  (with-eval-after-load 'yasnippet (haskell-snippets-initialize)))

(defun haskell/init-hindent ()
  (use-package hindent
    :defer t
    :if (stringp haskell-enable-hindent-style)
    :init
    (add-hook 'haskell-mode-hook #'hindent-mode)
    :config
    (progn
      (setq hindent-style haskell-enable-hindent-style)
      (spacemacs/set-leader-keys-for-major-mode 'haskell-mode
        "F" 'hindent-reformat-decl))))

(defun haskell/init-shm ()
  (use-package shm
    :defer t
    :if haskell-enable-shm-support
    :init
    (add-hook 'haskell-mode-hook 'structured-haskell-mode)
    :config
    (progn
      (when (require 'shm-case-split nil 'noerror)
        ;;TODO: Find some better bindings for case-splits
        (define-key shm-map (kbd "C-c S") 'shm/case-split)
        (define-key shm-map (kbd "C-c C-s") 'shm/do-case-split))

      (evil-define-key 'normal shm-map
        (kbd "RET") nil
        (kbd "C-k") nil
        (kbd "C-j") nil
        (kbd "D") 'shm/kill-line
        (kbd "R") 'shm/raise
        (kbd "P") 'shm/yank
        (kbd "RET") 'shm/newline-indent
        (kbd "RET") 'shm/newline-indent
        (kbd "M-RET") 'evil-ret
        )

      (evil-define-key 'operator shm-map
        (kbd ")") 'shm/forward-node
        (kbd "(") 'shm/backward-node)

      (evil-define-key 'motion shm-map
        (kbd ")") 'shm/forward-node
        (kbd "(") 'shm/backward-node)

      (define-key shm-map (kbd "C-j") nil)
      (define-key shm-map (kbd "C-k") nil))))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun haskell/post-init-company ()
    (spacemacs|add-company-hook haskell-mode)
    (spacemacs|add-company-hook haskell-cabal-mode))

  (defun haskell/init-company-ghc ()
    (use-package company-ghc
      :if (configuration-layer/package-usedp 'company)
      :defer t
      :init
      (push (if haskell-enable-ghc-mod-support
                '(company-ghc company-dabbrev-code company-yasnippet)
              '(company-dabbrev-code company-yasnippet))
              company-backends-haskell-mode)))

  (defun haskell/init-company-cabal ()
    (use-package company-cabal
      :if (configuration-layer/package-usedp 'company)
      :defer t
      :init
      (push '(company-cabal)
            company-backends-haskell-cabal-mode))))
