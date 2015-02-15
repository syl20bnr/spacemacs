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
    flycheck
    flycheck-haskell
    ghc
    haskell-mode
    hi2
    ))

(defun haskell/init-flycheck ()
  ;;(add-hook 'haskell-mode-hook 'flycheck-mode))
  (add-hook 'flycheck-mode-hook 'flycheck-haskell-setup))

(defun haskell/init-haskell-mode ()
  (require 'haskell-yas)
  (use-package haskell-mode
    :defer t
    :config
    (progn
      ;; Customization
      (custom-set-variables
       ;; Use cabal-repl for the GHCi session. Ensures our dependencies are in scope.
       ;; cabal-dev is deprecated
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

      ;; ;; use "mc" as prefix for cabal commands
      ;; (setq spacemacs/key-binding-prefixes '(("mc" . "cabal")))

      ;; ;; use "ms" as prefix for REPL commands
      ;; (setq spacemacs/key-binding-prefixes '(("ms" . "Haskell REPL")))

      ;; ;; use "md" as prefix for debug commands
      ;; (setq spacemacs/key-binding-prefixes '(("md" . "Haskell Debug")))

      ;; ;; use "mh" as prefix for documentation commands
      ;; (setq spacemacs/key-binding-prefixes '(("mh" . "Haskell Documentation")))

      
      (evil-leader/set-key-for-mode 'haskell-mode
        "mt"   'haskell-process-do-type
        "mi"   'haskell-process-do-info
        "mu"   'haskell-mode-find-uses
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

        "mhh"  'hoogle
        "mhy"  'hayoo
        "mhd"  'inferior-haskell-find-haddock

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
        (lambda () (ghc-init))
        ;; Use advanced indention
        (turn-on-haskell-indentation)

        ;; Indent the below lines on columns after the current column.
        ;; Might need better bindings for spacemacs and OS X
        (define-key haskell-mode-map (kbd "C-<right>")
          (lambda ()
            (interactive)
            (haskell-move-nested 1)))
        ;; Same as above but backwards.
        (define-key haskell-mode-map (kbd "C-<left>")
          (lambda ()
            (interactive)
            (haskell-move-nested -1))))

      ;; Useful to have these keybindings for .cabal files, too.
      (defun haskell-cabal-hook ()
        (define-key haskell-cabal-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch)))))

(defun haskell/init-company-ghc ()
  (use-package company-ghc
    :if (configuration-layer/layer-declaredp 'company-mode)
    :init
    (progn
      (add-to-list 'company-backends 'company-ghc)
      (add-hook 'haskell-mode-hook 'ghc-comp-init))))

(defun haskell/init-hi2 ()
  (use-package hi2
    :diminish hi2-mode
    :commands turn-on-hi2
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
