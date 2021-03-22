;;; packages.el --- Haskell Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
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
        (company-cabal :requires company)

        ;; dante completion backend
        (dante :requires company)
        ;; dante auto refactor companion
        (attrap :requires dante)

        lsp-haskell

        flycheck
        (flycheck-haskell :requires flycheck)
        ggtags
        haskell-mode
        haskell-snippets
        counsel-gtags
        helm-gtags
        (helm-hoogle :requires helm)
        hindent
        hlint-refactor
        ))

(defun haskell/init-lsp-haskell ()
  (use-package lsp-haskell
    :defer t))

(defun haskell/init-cmm-mode ()
  (use-package cmm-mode
    :defer t))

(defun haskell/post-init-company ()
  (add-hook 'haskell-mode-local-vars-hook #'spacemacs-haskell//setup-company)
  (add-hook 'haskell-literate-mode-local-vars-hook #'spacemacs-haskell//setup-company)
  ;; renamed as of 04/2020, delete in due course
  (add-hook 'literate-haskell-mode-local-vars-hook #'spacemacs-haskell//setup-company))

(defun haskell/init-company-cabal ()
  (use-package company-cabal
    :defer t
    :init
    (spacemacs|add-company-backends
      :backends company-cabal
      :modes haskell-cabal-mode)))

(defun haskell/init-dante ()
  (use-package dante
    :defer t
    :config
    (progn
      (dolist (mode haskell-modes)
        (spacemacs/set-leader-keys-for-major-mode mode
          "gb" 'xref-pop-marker-stack
          "ht" 'dante-type-at
          "hT" 'spacemacs-haskell//dante-insert-type
          "hi" 'dante-info
          "rs" 'dante-auto-fix
          "se" 'dante-eval-block
          "sr" 'dante-restart)))))

(defun haskell/init-attrap ()
  (use-package attrap
    :defer t))

(defun haskell/init-helm-hoogle ()
  (use-package helm-hoogle
    :defer t
    :init
    (dolist (mode haskell-modes)
      (spacemacs/set-leader-keys-for-major-mode mode "hf" 'helm-hoogle))))

(defun haskell/post-init-flycheck ()
  (progn
    (add-hook 'dante-mode-hook
              (lambda () (flycheck-add-next-checker 'haskell-dante '(warning . haskell-hlint))))
    (spacemacs/enable-flycheck 'haskell-mode))
  )

(defun haskell/init-flycheck-haskell ()
  (use-package flycheck-haskell
    :commands flycheck-haskell-configure
    :init (add-hook 'flycheck-mode-hook 'flycheck-haskell-configure)))

(defun haskell/post-init-ggtags ()
  (add-hook 'haskell-mode-local-vars-hook #'spacemacs/ggtags-mode-enable)
  (add-hook 'haskell-literate-mode-local-vars-hook #'spacemacs/ggtags-mode-enable)
  ;; renamed as of 04/2020, delete in due course
  (add-hook 'literate-haskell-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun haskell/init-haskell-mode ()
  (use-package haskell-mode
    :defer t
    :init
    (progn
      (add-hook 'haskell-mode-local-vars-hook #'spacemacs-haskell//setup-backend)
      (add-hook 'haskell-literate-mode-local-vars-hook #'spacemacs-haskell//setup-backend)
      ;; renamed as of 04/2020, delete in due course
      (add-hook 'literate-haskell-mode-local-vars-hook #'spacemacs-haskell//setup-backend)

      (defun spacemacs//force-haskell-mode-loading ()
        "Force `haskell-mode' loading when visiting cabal file."
        (require 'haskell-mode))
      (add-hook 'haskell-cabal-mode-hook
                'spacemacs//force-haskell-mode-loading)

      ;; Haskell cabal files interact badly with electric-indent-mode
      ;; note: we cannot add this hook in :config, since haskell-mode might
      ;; only be loaded after cabal-mode hooks are already run (see add-hook above)
      (add-hook 'haskell-cabal-mode-hook #'spacemacs-haskell//disable-electric-indent)

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
       haskell-stylish-on-save nil))
    :config
    (progn
      (defun spacemacs/haskell-interactive-bring ()
        "Bring up the interactive mode for this session without
         switching to it."
        (interactive)
        (let* ((session (haskell-session))
               (buffer (haskell-session-interactive-buffer session)))
          (display-buffer buffer)))

      ;; hooks
      (add-hook 'haskell-mode-hook #'spacemacs-haskell//disable-electric-indent)

      ;; prefixes
      (dolist (mode haskell-modes)
        (spacemacs/declare-prefix-for-mode mode "mg" "haskell/navigation")
        (spacemacs/declare-prefix-for-mode mode "ms" "haskell/repl")
        (spacemacs/declare-prefix-for-mode mode "mc" "haskell/cabal")
        (spacemacs/declare-prefix-for-mode mode "mh" "haskell/documentation")
        (spacemacs/declare-prefix-for-mode mode "md" "haskell/debug")
        (spacemacs/declare-prefix-for-mode mode "mr" "haskell/refactor"))
      (spacemacs/declare-prefix-for-mode 'haskell-interactive-mode "ms" "haskell/repl")
      (spacemacs/declare-prefix-for-mode 'haskell-cabal-mode "ms" "haskell/repl")

      ;; key bindings
      (defun spacemacs/haskell-process-do-type-on-prev-line ()
        (interactive)
        (haskell-process-do-type 1))

      ;; repl key bindings
      (evil-define-key 'insert haskell-interactive-mode-map
        (kbd "C-j") 'haskell-interactive-mode-history-next
        (kbd "C-k") 'haskell-interactive-mode-history-previous
        (kbd "C-l") 'haskell-interactive-mode-clear)

      ;; Bind repl
      (spacemacs/register-repl 'haskell
                               'haskell-interactive-switch "haskell")

      (dolist (mode haskell-modes)
        (spacemacs/set-leader-keys-for-major-mode mode
          "sb"  'haskell-process-load-file
          "sc"  'haskell-interactive-mode-clear
          "sS"  'spacemacs/haskell-interactive-bring
          "ss"  'haskell-interactive-switch
          "st"  'haskell-session-change-target
          "'"   'haskell-interactive-switch

          "ca"  'haskell-process-cabal
          "cb"  'haskell-process-cabal-build
          "cc"  'haskell-compile
          "cv"  'haskell-cabal-visit-file

          "hd"  'inferior-haskell-find-haddock
          "hi"  'haskell-process-do-info
          "ht"  'haskell-process-do-type
          "hT"  'spacemacs/haskell-process-do-type-on-prev-line

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
          "dt"  'haskell-debug/trace

          "ri"  'spacemacs/haskell-format-imports)
        (if (eq (spacemacs//haskell-backend) 'lsp)
            (spacemacs/set-leader-keys-for-major-mode mode
              "gl"  'haskell-navigate-imports
              "S"   'haskell-mode-stylish-buffer

              "hg"  'hoogle
              "hG"  'haskell-hoogle-lookup-from-local)
          (spacemacs/set-leader-keys-for-major-mode mode
            "gi"  'haskell-navigate-imports
            "F"   'haskell-mode-stylish-buffer

            "hh"  'hoogle
            "hG"  'haskell-hoogle-lookup-from-local)))

      (evilified-state-evilify haskell-debug-mode haskell-debug-mode-map
        "RET" 'haskell-debug/select
        "a" 'haskell-debug/abandon
        "b" 'haskell-debug/break-on-function
        "c" 'haskell-debug/continue
        "d" 'haskell-debug/delete
        "i" 'haskell-debug/step
        "s" 'haskell-debug/next
        "S" 'haskell-debug/previous
        "r" 'haskell-debug/refresh
        "t" 'haskell-debug/trace)

      ;; configure C-c C-l so it doesn't throw any errors
      (bind-key "C-c C-l" 'haskell-process-load-file haskell-mode-map)
      (bind-key "C-c C-z" 'haskell-interactive-switch haskell-mode-map)

      ;; Switch back to editor from REPL
      (spacemacs/set-leader-keys-for-major-mode 'haskell-interactive-mode
        "ss"  'haskell-interactive-switch-back)

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
        "sS"  'spacemacs/haskell-interactive-bring
        "ss"  'haskell-interactive-switch
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
  (setq haskell-snippets-dir
        (configuration-layer/get-elpa-package-install-directory
         'haskell-snippets))

  (defun haskell-snippets-initialize ()
    (let ((snip-dir (expand-file-name "snippets" haskell-snippets-dir)))
      (add-to-list 'yas-snippet-dirs snip-dir t)
      (yas-load-directory snip-dir)))

  (with-eval-after-load 'yasnippet (haskell-snippets-initialize)))

(defun haskell/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'haskell-mode))

(defun haskell/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'haskell-mode))


;; doesn't support haskell-literate-mode :(
(defun haskell/init-hindent ()
  (use-package hindent
    :defer t
    :if haskell-enable-hindent
    :init
    (add-hook 'haskell-mode-hook #'hindent-mode)
    :config
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'haskell-mode
        "f" 'hindent-reformat-decl-or-fill))))

(defun haskell/init-hlint-refactor ()
  (use-package hlint-refactor
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'haskell-mode
        "rb" 'hlint-refactor-refactor-buffer
        "rr" 'hlint-refactor-refactor-at-point))))
