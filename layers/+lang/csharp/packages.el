;;; packages.el --- csharp Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq csharp-packages
      '(
        company
        csharp-mode
        evil-matchit
        ggtags
        counsel-gtags
        helm-gtags
        omnisharp
        flycheck
        ))

(defun csharp/init-omnisharp ()
  ;; Load omnisharp-mode with csharp-mode,
  ;; this should start the omnisharp server automatically
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (use-package omnisharp
    :defer t
    :init
    (add-to-list 'spacemacs-jump-handlers-csharp-mode
                 '(omnisharp-go-to-definition :async t))
    :config
    (progn
      ;; [missing in roslyn] (spacemacs/declare-prefix-for-mode 'csharp-mode "mc" "csharp/compile")
      ;; [missing in roslyn] (spacemacs/declare-prefix-for-mode 'csharp-mode "mf" "csharp/file")
      (spacemacs/declare-prefix-for-mode 'csharp-mode "mg" "csharp/navigation")
      (spacemacs/declare-prefix-for-mode 'csharp-mode "mh" "csharp/documentation")
      (spacemacs/declare-prefix-for-mode 'csharp-mode "mr" "csharp/refactoring")
      (spacemacs/declare-prefix-for-mode 'csharp-mode "ms" "csharp/server")
      (spacemacs/declare-prefix-for-mode 'csharp-mode "mt" "csharp/tests")

      (spacemacs/set-leader-keys-for-major-mode 'csharp-mode
        ;; Compile
        ;; Only one compile command so use top-level
        ;; [missing in roslyn] "cc" 'omnisharp-build-in-emacs

        ;; Solution/project manipulation
        ;; [missing in roslyn] "fa" 'omnisharp-add-to-solution-current-file
        ;; [missing in roslyn] "fA" 'omnisharp-add-to-solution-dired-selected-files
        ;; [missing in roslyn] "fr" 'omnisharp-remove-from-project-current-file
        ;; [missing in roslyn] "fR" 'omnisharp-remove-from-project-dired-selected-files

        ;; [missing in roslyn] "pl" 'omnisharp-add-reference

        ;; Navigation
        "ge"   'omnisharp-solution-errors
        "gG"   'omnisharp-go-to-definition-other-window
        "gu"   'omnisharp-helm-find-usages
        "gU"   'omnisharp-find-usages-with-ido
        "gs"   'omnisharp-helm-find-symbols
        "gi"   'omnisharp-find-implementations
        "gI"   'omnisharp-find-implementations-with-ido
        "gr"   'omnisharp-navigate-to-region
        "gm"   'omnisharp-navigate-to-solution-member
        "gM"   'omnisharp-navigate-to-solution-member-other-window
        "gf"   'omnisharp-navigate-to-solution-file
        "gF"   'omnisharp-navigate-to-solution-file-then-file-member
        "gc"   'omnisharp-navigate-to-current-file-member

        ;; Help, documentation, info
        "ht" 'omnisharp-current-type-information
        "hT" 'omnisharp-current-type-information-to-kill-ring

        ;; Refactoring
        "rm" 'omnisharp-rename
        ;; [Broken in roslyn] "rM" 'omnisharp-rename-interactively
        "rr" 'omnisharp-run-code-action-refactoring

        ;; Server manipulation, inspired spacemacs REPL bindings since C# does
        ;; not provice a REPL
        "ss" 'omnisharp-start-omnisharp-server
        "sS" 'omnisharp-stop-server
        "sr" 'omnisharp-reload-solution
        "si" 'omnisharp-install-server

        ;; Tests
        ;; [missing in roslyn] "ta" 'omnisharp-unit-test-all
        "tb" 'omnisharp-unit-test-buffer
        "tp" 'omnisharp-unit-test-at-point
        "tt" 'omnisharp-unit-test-last

        ;; Code manipulation
        "u" 'omnisharp-auto-complete-overrides
        "i" 'omnisharp-fix-usings
        ;; [missing in roslyn] "=" 'omnisharp-code-format
        )
      (spacemacs|hide-lighter omnisharp-mode))))

(defun csharp/post-init-company ()
  (when (configuration-layer/package-used-p 'omnisharp)
    (spacemacs|add-company-backends
      :backends company-omnisharp
      :modes csharp-mode)))

(defun csharp/init-csharp-mode ()
  (use-package csharp-mode
    :defer t))

(defun csharp/post-init-evil-matchit ()
  (with-eval-after-load 'evil-matchit
    (plist-put evilmi-plugins 'csharp-mode
               '((evilmi-simple-get-tag evilmi-simple-jump)
                 (evilmi-c-get-tag evilmi-c-jump))))
  (add-hook 'csharp-mode-hook 'turn-on-evil-matchit-mode))

(defun csharp/post-init-flycheck ()
  (spacemacs/enable-flycheck 'csharp-mode))

(defun csharp/post-init-ggtags ()
  (add-hook 'csharp-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun csharp/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'csharp-mode))

(defun csharp/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'csharp-mode))
