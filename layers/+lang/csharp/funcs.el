;;; funcs.el --- C# Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Muneeb Shaikh <muneeb@reversehack.in>
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



;; backend

(defun spacemacs//csharp-setup-backend ()
  "Conditionally setup layer csharp based on backend."
  (pcase csharp-backend
    ('omnisharp (spacemacs//csharp-setup-omnisharp))
    ('lsp (spacemacs//csharp-setup-lsp))))

(defun spacemacs//csharp-setup-company ()
  "Conditionally setup company based on backend."
  (when (eq csharp-backend 'omnisharp)
    (spacemacs//csharp-setup-omnisharp-company)))

(defun spacemacs//csharp-configure ()
  "Conditionally configure csharp layer based on backend."
  (when (eq csharp-backend 'omnisharp)
    (spacemacs//csharp-configure-omnisharp)))


;; omnisharp

(defun spacemacs//csharp-setup-omnisharp ()
  (when (configuration-layer/package-used-p 'omnisharp)
    ;; Load omnisharp-mode with csharp-mode,
    ;; this should start the omnisharp server automatically
    (add-hook 'csharp-mode-hook 'omnisharp-mode)

    (add-to-list 'spacemacs-jump-handlers-csharp-mode
                 '(omnisharp-go-to-definition :async t))))

(defun spacemacs//csharp-setup-omnisharp-company ()
  "Setup omnisharp auto-completion."
  (when (configuration-layer/package-used-p 'omnisharp)
    (spacemacs|add-company-backends
      :backends company-omnisharp
      :modes csharp-mode)))

(defun spacemacs//csharp-configure-omnisharp ()
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
    "tl" 'omnisharp-unit-test-last
    "tt" 'omnisharp-unit-test-at-point

    ;; Code manipulation
    "u" 'omnisharp-auto-complete-overrides
    "i" 'omnisharp-fix-usings)
  ;; [missing in roslyn] "=" 'omnisharp-code-format

  (spacemacs|hide-lighter omnisharp-mode))


;; lsp

(defun spacemacs//csharp-setup-lsp ()
  "Setup lsp backend."
  (add-hook 'csharp-mode-hook #'lsp))
