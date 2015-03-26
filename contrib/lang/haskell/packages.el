;;; packages.el --- Haskell Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar haskell-packages
  '(
    company-ghc
    flycheck-haskell
    ghc
    haskell-mode
    hi2
    hindent
    shm
    ))

(defun haskell/init-flycheck-haskell ()
  (use-package flycheck-haskell
    :defer t
    :init (add-hook 'haskell-mode-hook 'flycheck-haskell-setup)))

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

      (evil-define-key 'operator map
        (kbd ")") 'shm/forward-node
        (kbd "(") 'shm/backward-node
        )

      (evil-define-key 'motion map
        (kbd ")") 'shm/forward-node
        (kbd "(") 'shm/backward-node
        )

      (define-key shm-map (kbd "C-j") nil)
      (define-key shm-map (kbd "C-k") nil)
      )
    )
  )

(defun haskell/init-hindent ()
  (use-package hindent
    :defer t
    :if (stringp haskell-enable-hindent-style)
    :init
    (add-hook 'haskell-mode-hook #'hindent-mode)
    :config
    (progn
      (setq hindent-style haskell-enable-hindent-style)
      (evil-leader/set-key-for-mode 'haskell-mode
        "mF"   'hindent/reformat-decl))))

(defun haskell-process-do-type-on-prev-line ()
  (interactive)
  (haskell-process-do-type 1))

(defun haskell/init-haskell-mode ()
  (require 'haskell-yas)
  (use-package haskell-mode
    :defer t
    :config
    (progn
      ;; Customization
      (custom-set-variables

       '(haskell-process-type 'auto)

       ;; Use notify.el (if you have it installed) at the end of running
       ;; Cabal commands or generally things worth notifying.
       '(haskell-notify-p t)

       ;; To enable tags generation on save.
       '(haskell-tags-on-save t)

       ;; Remove annoying error popups
       '(haskell-interactive-popup-error nil)

       ;; Better import handling
       '(haskell-process-suggest-remove-import-lines t)
       '(haskell-process-auto-import-loaded-modules t)

       ;; Disable haskell-stylish on save, it breaks flycheck highlighting
       '(haskell-stylish-on-save nil))

      ;; Make sure company-ghc is properly initialized
      (autoload 'ghc-init "ghc" nil t)
      (autoload 'ghc-debug "ghc" nil t)

      (add-hook 'haskell-mode-hook 'haskell-hook)
      (add-hook 'haskell-cabal-mode-hook 'haskell-cabal-hook)

      ;; Make "RET" behaviour in REPL saner
      (evil-define-key 'insert haskell-interactive-mode-map (kbd "RET") 'haskell-interactive-mode-return)
      (evil-define-key 'normal haskell-interactive-mode-map (kbd "RET") 'haskell-interactive-mode-return)

      ;;;;;;;;; Keybindings ;;;;;;;;;;

      ;; major mode specfic prefixes not support for now
      ;; (spacemacs/declare-prefix "mc" "cabal")
      ;; (spacemacs/declare-prefix "ms" "repl")
      ;; (spacemacs/declare-prefix "md" "debug")
      ;; (spacemacs/declare-prefix "mh" "documentation")

      (evil-leader/set-key-for-mode 'haskell-mode
        "mgg"  'haskell-mode-jump-to-def-or-tag
        "mf"   'haskell-mode-stylish-buffer

        "msb"  'haskell-process-load-or-reload
        "msc"  'haskell-interactive-mode-clear
        "mss"  'haskell-interactive-bring
        "msS"  'haskell-interactive-switch

        "mca"  'haskell-process-cabal
        "mcb"  'haskell-process-cabal-build
        "mcc"  'haskell-compile
        "mcv"  'haskell-cabal-visit-file

        "mhd"  'inferior-haskell-find-haddock
        "mhh"  'hoogle
        "mhi"  'haskell-process-do-info
        "mht"  'haskell-process-do-type
        "mhT"  'haskell-process-do-type-on-prev-line
        "mhy"  'hayoo

        "mdd"  'haskell-debug
        "mdb"  'haskell-debug/break-on-function
        "mdn"  'haskell-debug/next
        "mdN"  'haskell-debug/previous
        "mdB"  'haskell-debug/delete
        "mdc"  'haskell-debug/continue
        "mda"  'haskell-debug/abandon
        "mdr"  'haskell-debug/refresh
        )


      ;; Switch back to editor from REPL
      (evil-leader/set-key-for-mode 'interactive-haskell-mode
        "msS"  'haskell-interactive-switch
        )

      ;; Compile
      (evil-leader/set-key-for-mode 'haskell-cabal
        "mC"  'haskell-compile
        )

      ;; Cabal-file bindings
      (evil-leader/set-key-for-mode 'haskell-cabal-mode
        "md" 'haskell-cabal-add-dependency
        "mb" 'haskell-cabal-goto-benchmark-section
        "me" 'haskell-cabal-goto-executable-section
        "mt" 'haskell-cabal-goto-test-suite-section
        "mm" 'haskell-cabal-goto-exposed-modules
        "ml" 'haskell-cabal-goto-library-section
        "mn" 'haskell-cabal-next-subsection
        "mp" 'haskell-cabal-previous-subsection
        "mN" 'haskell-cabal-next-section
        "mP" 'haskell-cabal-previous-section
        "mf" 'haskell-cabal-find-or-create-source-file
        ;; "m="  'haskell-cabal-subsection-arrange-lines ;; Does a bad job, 'gg=G' works better
        )

      ;; Haskell main editing mode key bindings.
      (defun haskell-hook ()
        (ghc-init)
        ;; Use advanced indention
        (if (not haskell-enable-shm-support)
            (turn-on-haskell-indentation)
          )
        )

      ;; Useful to have these keybindings for .cabal files, too.
      (defun haskell-cabal-hook ()
        (define-key haskell-cabal-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch))

      ;;GHCi-ng
      (defun ghci-ng-setup()
        (progn
          ;; haskell-process-type is set to auto, so setup ghci-ng for either case
          ;; if haskell-process-type == cabal-repl
          (setq haskell-process-args-cabal-repl '("--ghc-option=-ferror-spans" "--with-ghc=ghci-ng"))
          ;; if haskell-process-type == GHCi
          (setq haskell-process-path-ghci "ghci-ng")

          (evil-leader/set-key-for-mode 'haskell-mode
            "mu"   'haskell-mode-find-uses
            "mt"   'haskell-mode-show-type-at
            "mgg"  'haskell-mode-goto-loc
            ))
        )

      (if haskell-enable-ghci-ng-support
          (ghci-ng-setup)))))

(defun haskell/init-company-ghc ()
  (use-package company-ghc
    :if (configuration-layer/package-declaredp 'company)
    :defer t
    :init
    (progn
      (spacemacs|add-mode-company-backend haskell-mode company-ghc))))

(defun haskell/init-hi2 ()
  (use-package hi2
    :diminish hi2-mode
    :commands turn-on-hi2
    :if (not haskell-enable-shm-support)
    :init
    (add-hook 'haskell-mode-hook 'turn-on-hi2)
    :config
    (progn

      (defun spacemacs/haskell-show-hi2-guides ()
        (when (and (boundp 'hi2-mode) hi2-mode)
          (hi2-enable-show-indentations)))

      (defun spacemacs/haskell-hide-hi2-guides ()
        (when (and (boundp 'hi2-mode) hi2-mode)
          (hi2-disable-show-indentations)))

      ;; Show indentation guides for hi2 only in insert state.
      (add-hook 'evil-normal-state-entry-hook 'spacemacs/haskell-hide-hi2-guides)
      (add-hook 'evil-insert-state-entry-hook 'spacemacs/haskell-show-hi2-guides)
      (add-hook 'evil-insert-state-exit-hook  'spacemacs/haskell-hide-hi2-guides))))
