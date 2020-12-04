;;; funcs.el --- C# Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Muneeb Shaikh <muneeb@reversehack.in>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3


;; backend

(defun space-macs//csharp-setup-backend ()
  "Conditionally setup layer csharp based on backend."
  (pcase csharp-backend
    (`omnisharp (space-macs//csharp-setup-omnisharp))
    (`lsp (space-macs//csharp-setup-lsp))))

(defun space-macs//csharp-setup-company ()
  "Conditionally setup company based on backend."
  (pcase csharp-backend
    (`omnisharp (space-macs//csharp-setup-omnisharp-company))))

(defun space-macs//csharp-configure ()
  "Conditionally configure csharp layer based on backend."
  (pcase csharp-backend
    (`omnisharp (space-macs//csharp-configure-omnisharp))))


;; omnisharp

(defun space-macs//csharp-setup-omnisharp ()
  (when (configuration-layer/package-used-p 'omnisharp)
    ;; Load omnisharp-mode with csharp-mode,
    ;; this should start the omnisharp server automatically
    (add-hook 'csharp-mode-hook 'omnisharp-mode)

    (add-to-list 'space-macs-jump-handlers-csharp-mode
                 '(omnisharp-go-to-definition :async t))))

(defun space-macs//csharp-setup-omnisharp-company ()
  "Setup omnisharp auto-completion."
  (when (configuration-layer/package-used-p 'omnisharp)
    (space-macs|add-company-backends
      :backends company-omnisharp
      :modes csharp-mode)))

(defun space-macs//csharp-configure-omnisharp ()
  (progn
    ;; [missing in roslyn] (space-macs/declare-prefix-for-mode 'csharp-mode "mc" "csharp/compile")
    ;; [missing in roslyn] (space-macs/declare-prefix-for-mode 'csharp-mode "mf" "csharp/file")
    (space-macs/declare-prefix-for-mode 'csharp-mode "mg" "csharp/navigation")
    (space-macs/declare-prefix-for-mode 'csharp-mode "mh" "csharp/documentation")
    (space-macs/declare-prefix-for-mode 'csharp-mode "mr" "csharp/refactoring")
    (space-macs/declare-prefix-for-mode 'csharp-mode "ms" "csharp/server")
    (space-macs/declare-prefix-for-mode 'csharp-mode "mt" "csharp/tests")

    (space-macs/set-leader-keys-for-major-mode 'csharp-mode
      ;; Compile
      ;; Only one compile command so use top-level
      ;; [missing in roslyn] "cc" 'omnisharp-build-in-e-macs

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

      ;; Server manipulation, inspired space-macs REPL bindings since C# does
      ;; not provice a REPL
      "ss" 'omnisharp-start-omnisharp-server
      "sS" 'omnisharp-stop-server
      "sr" 'omnisharp-reload-solution
      "si" 'omnisharp-install-server

      ;; Tests
      ;; [missing in roslyn] "ta" 'omnisharp-unit-test-all
      "tb" 'omnisharp-unit-test-buffer
      "tl" 'omnisharp-unit-test-last
      "tt" 'omnisharp-unit-test-at-point

      ;; Code manipulation
      "u" 'omnisharp-auto-complete-overrides
      "i" 'omnisharp-fix-usings
      ;; [missing in roslyn] "=" 'omnisharp-code-format
      )
    (space-macs|hide-lighter omnisharp-mode)))


;; lsp

(defun space-macs//csharp-setup-lsp ()
  "Setup lsp backend."
  (add-hook 'csharp-mode-hook #'lsp))


