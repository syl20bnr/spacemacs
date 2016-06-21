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
    (company-cabal :toggle (configuration-layer/package-usedp 'company))

    ;; ghci completion backend
    (company-ghci :toggle (and (configuration-layer/package-usedp 'company)
                               (eq haskell-completion-backend 'ghci)))

    ;; ghc-mod completion backend
    (company-ghc :toggle (and (configuration-layer/package-usedp 'company)
                              (eq haskell-completion-backend 'ghc-mod)))
    (ghc :toggle (eq haskell-completion-backend 'ghc-mod))

    ;; intero completion backend
    (intero :toggle (eq haskell-completion-backend 'intero))

    flycheck
    (flycheck-haskell :toggle (configuration-layer/package-usedp 'flycheck))
    haskell-mode
    haskell-snippets
    (helm-hoogle :toggle (configuration-layer/package-usedp 'helm))
    hindent
    hlint-refactor
    ))

(defun haskell/init-cmm-mode ()
  (use-package cmm-mode
    :defer t))

(defun haskell/post-init-company ()
  (spacemacs|add-company-hook haskell-mode)
  (spacemacs|add-company-hook haskell-cabal-mode)
  (when (eq haskell-completion-backend 'intero)
    (spacemacs|add-company-hook intero-repl-mode)))

(defun haskell/init-company-cabal ()
  (use-package company-cabal
    :if (configuration-layer/package-usedp 'company)
    :defer t
    :init
    (push '(company-cabal)
          company-backends-haskell-cabal-mode)))

(defun haskell/init-company-ghci ()
  (use-package company-ghci
    :defer t
    :init (push '(company-ghci company-dabbrev-code company-yasnippet)
                company-backends-haskell-mode)))

(defun haskell/init-company-ghc ()
  (use-package company-ghc
    :defer t
    :init (push '(company-ghc company-dabbrev-code company-yasnippet)
                company-backends-haskell-mode)))

(defun haskell/init-ghc ()
  (use-package ghc
    :after (haskell-mode)
    :init (add-hook 'haskell-mode-hook 'ghc-init)
    :config
    (progn
      (dolist (mode haskell-modes)
        (spacemacs/declare-prefix-for-mode mode "mm" "haskell/ghc-mod")
        (spacemacs/set-leader-keys-for-major-mode mode
          "mt" 'ghc-insert-template-or-signature
          "mu" 'ghc-initial-code-from-signature
          "ma" 'ghc-auto
          "mf" 'ghc-refine
          "me" 'ghc-expand-th
          "mn" 'ghc-goto-next-hole
          "mp" 'ghc-goto-prev-hole
          "m>" 'ghc-make-indent-deeper
          "m<" 'ghc-make-indent-shallower
          "hi" 'ghc-show-info
          "ht" 'ghc-show-type))
      (when (configuration-layer/package-usedp 'flycheck)
        ;; remove overlays from ghc-check.el if flycheck is enabled
        (set-face-attribute 'ghc-face-error nil :underline nil)
        (set-face-attribute 'ghc-face-warn nil :underline nil)))))

(defun haskell/init-intero ()
  (use-package intero
    :after (haskell-mode)
    :init
    (progn
      (push '(company-intero company-dabbrev-code company-yasnippet)
            company-backends-haskell-mode)
      (add-hook 'haskell-mode-hook #'intero-mode))
    :config
    (progn
      (spacemacs|diminish intero-mode " λ" " \\")

      (defun haskell-intero/insert-type ()
        (interactive)
        (intero-type-at :insert))

      (defun haskell-intero/display-repl ()
        (interactive)
        (let ((buffer (intero-repl-buffer)))
          (unless (get-buffer-window buffer 'visible)
            (display-buffer (intero-repl-buffer)))))

      (defun haskell-intero/pop-to-repl ()
        (interactive)
        (pop-to-buffer (intero-repl-buffer)))

      (defun haskell-intero//preserve-focus (f)
        (let ((buffer (current-buffer)))
          (funcall f)
          (pop-to-buffer buffer)))

      (advice-add 'intero-repl-load
                  :around #'haskell-intero//preserve-focus)

      (dolist (mode haskell-modes)
        (spacemacs/set-leader-keys-for-major-mode mode
          "gg" 'intero-goto-definition
          "hi" 'intero-info
          "ht" 'intero-type-at
          "hT" 'haskell-intero/insert-type
          "sb" 'intero-repl-load))

      (dolist (mode (cons 'haskell-cabal-mode haskell-modes))
        (spacemacs/set-leader-keys-for-major-mode mode
          "sc"  nil
          "ss"  'haskell-intero/display-repl
          "sS"  'haskell-intero/pop-to-repl))

      (dolist (mode (append haskell-modes '(haskell-cabal-mode intero-repl-mode)))
        (spacemacs/declare-prefix-for-mode mode "mi" "haskell/intero")
        (spacemacs/set-leader-keys-for-major-mode mode
          "ic"  'intero-cd
          "id"  'intero-devel-reload
          "ik"  'intero-destroy
          "il"  'intero-list-buffers
          "ir"  'intero-restart
          "it"  'intero-targets))

      (evil-define-key '(insert normal) intero-mode-map
        (kbd "M-.") 'intero-goto-definition))))

(defun haskell/init-helm-hoogle ()
  (use-package helm-hoogle
    :defer t
    :init
    (dolist (mode haskell-modes)
      (spacemacs/set-leader-keys-for-major-mode mode "hf" 'helm-hoogle))))

(defun haskell/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'haskell-mode))

(defun haskell/init-flycheck-haskell ()
  (use-package flycheck-haskell
    :commands flycheck-haskell-configure
    :init (add-hook 'flycheck-mode-hook 'flycheck-haskell-configure)))

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
            (electric-indent-local-mode -1)))

      (defun spacemacs/haskell-interactive-bring ()
        "Bring up the interactive mode for this session without
         switching to it."
        (interactive)
        (let* ((session (haskell-session))
               (buffer (haskell-session-interactive-buffer session)))
          (display-buffer buffer)))

      ;; hooks
      (add-hook 'haskell-mode-hook 'spacemacs/init-haskell-mode)
      (unless (eq haskell-completion-backend 'ghc-mod)
        (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

      ;; prefixes
      (dolist (mode haskell-modes)
        (spacemacs/declare-prefix-for-mode mode "mg" "haskell/navigation")
        (spacemacs/declare-prefix-for-mode mode "ms" "haskell/repl")
        (spacemacs/declare-prefix-for-mode mode "mc" "haskell/cabal")
        (spacemacs/declare-prefix-for-mode mode "mh" "haskell/documentation")
        (spacemacs/declare-prefix-for-mode mode "md" "haskell/debug"))
      (spacemacs/declare-prefix-for-mode 'haskell-interactive-mode "ms" "haskell/repl")
      (spacemacs/declare-prefix-for-mode 'haskell-cabal-mode "ms" "haskell/repl")

      ;; key bindings
      (defun spacemacs/haskell-process-do-type-on-prev-line ()
        (interactive)
        (haskell-process-do-type 1))

      (dolist (mode haskell-modes)
        (spacemacs/set-leader-keys-for-major-mode mode
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
          "hi"  'haskell-process-do-info
          "ht"  'haskell-process-do-type
          "hT"  'spacemacs/haskell-process-do-type-on-prev-line
          "hy"  'hayoo

          "da"  'haskell-debug/abandon
          "db"  'haskell-debug/break-on-function
          "dB"  'haskell-debug/delete
          "dc"  'haskell-debug/continue
          "dd"  'haskell-debug
          "dn"  'haskell-debug/next
          "dN"  'haskell-debug/previous
          "dp"  'haskell-debug/previous
          "dr"  'haskell-debug/refresh
          "ds"  'haskell-debug/step
          "dt"  'haskell-debug/trace))

      (evilified-state-evilify haskell-debug-mode haskell-debug-mode-map
        "RET" 'haskell-debug/select
        "a" 'haskell-debug/abandon
        "b" 'haskell-debug/break-on-function
        "c" 'haskell-debug/continue
        "d" 'haskell-debug/delete
        "n" 'haskell-debug/next
        "N" 'haskell-debug/previous
        "p" 'haskell-debug/previous
        "r" 'haskell-debug/refresh
        "s" 'haskell-debug/step
        "t" 'haskell-debug/trace)

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
        (kbd "RET") 'haskell-interactive-mode-return))

  ;; align rules for Haskell
  (with-eval-after-load 'align
    (add-to-list 'align-rules-list
                 '(haskell-types
                   (regexp . "\\(\\s-+\\)\\(::\\|∷\\)\\s-+")
                   (modes . haskell-modes)))
    (add-to-list 'align-rules-list
                 '(haskell-assignment
                   (regexp . "\\(\\s-+\\)=\\s-+")
                   (modes . haskell-modes)))
    (add-to-list 'align-rules-list
                 '(haskell-arrows
                   (regexp . "\\(\\s-+\\)\\(->\\|→\\)\\s-+")
                   (modes . haskell-modes)))
    (add-to-list 'align-rules-list
                 '(haskell-left-arrows
                   (regexp . "\\(\\s-+\\)\\(<-\\|←\\)\\s-+")
                   (modes . haskell-modes))))))

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

;; doesn't support literate-haskell-mode :(
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

(defun haskell/init-hlint-refactor ()
  (use-package hlint-refactor
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'haskell-mode "mr" "haskell/refactor")
      (spacemacs/set-leader-keys-for-major-mode 'haskell-mode
        "rb" 'hlint-refactor-refactor-buffer
        "rr" 'hlint-refactor-refactor-at-point))))
