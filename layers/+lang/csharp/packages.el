;;; packages.el --- csharp Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
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
    omnisharp
    ))

(defun csharp/init-omnisharp ()
  ;; Load omnisharp-mode with csharp-mode, this should start the omnisharp server automatically
  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (use-package omnisharp
    :defer t
    :init
    (progn
      (when (configuration-layer/package-usedp 'company)
        ;; needed to avoid an error when fetching doc using company
        ;; Note: if you are using a roslyn based omnisharp server you can
        ;; set back this variable to t.
        (setq omnisharp-auto-complete-want-documentation nil))
      (push 'company-omnisharp company-backends-csharp-mode))
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'csharp-mode "mc" "csharp/compile")
      (spacemacs/declare-prefix-for-mode 'csharp-mode "mf" "csharp/file")
      (spacemacs/declare-prefix-for-mode 'csharp-mode "mg" "csharp/navigation")
      (spacemacs/declare-prefix-for-mode 'csharp-mode "mh" "csharp/documentation")
      (spacemacs/declare-prefix-for-mode 'csharp-mode "mr" "csharp/refactoring")
      (spacemacs/declare-prefix-for-mode 'csharp-mode "ms" "csharp/server")
      (spacemacs/declare-prefix-for-mode 'csharp-mode "mt" "csharp/tests")
      (spacemacs/set-leader-keys-for-major-mode 'csharp-mode
        ;; Compile
        "cc" 'omnisharp-build-in-emacs ;; Only one compile command so use top-level
        ;; Solution/project manipulation
        "fa" 'omnisharp-add-to-solution-current-file
        "fA" 'omnisharp-add-to-solution-dired-selected-files
        "fr" 'omnisharp-remove-from-project-current-file
        "fR" 'omnisharp-remove-from-project-dired-selected-files
        "pl" 'omnisharp-add-reference
        ;; Navigation
        "gg"   'omnisharp-go-to-definition
        "gG"   'omnisharp-go-to-definition-other-window
        "gu"   'omnisharp-helm-find-usages
        "gs"   'omnisharp-helm-find-symbols
        "gi"   'omnisharp-find-implementations
        "gr"   'omnisharp-navigate-to-region
        "gm"   'omnisharp-navigate-to-solution-member
        "gM"   'omnisharp-navigate-to-solution-member-other-window
        "gf"   'omnisharp-navigate-to-solution-file
        "gF"   'omnisharp-navigate-to-solution-file-then-file-member
        ;; Help, documentation, info
        "ht" 'omnisharp-current-type-information
        "hT" 'omnisharp-current-type-information-to-kill-ring
        ;; Refactoring
        "rm" 'omnisharp-rename
        "rr" 'omnisharp-run-code-action-refactoring
        ;; Server manipulation, inspired spacemacs REPL bindings since C# does not provice a REPL
        "ss" 'omnisharp-start-omnisharp-server
        "sS" 'omnisharp-stop-server
        "sr" 'omnisharp-reload-solution
        ;; Tests
        "ta" 'omnisharp-unit-test-all
        "tb" 'omnisharp-unit-test-fixture
        "tt" 'omnisharp-unit-test-single
        ;; Code manipulation
        "u" 'omnisharp-auto-complete-overrides
        "i" 'omnisharp-fix-usings
        "=" 'omnisharp-code-format))))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun csharp/post-init-company ()
    (spacemacs|add-company-hook csharp-mode)))
