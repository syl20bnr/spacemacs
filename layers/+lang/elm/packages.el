;;; packages.el --- elm Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
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


(defconst elm-packages
  '(
    company
    elm-mode
    elm-test-runner
    flycheck
    (flycheck-elm :requires flycheck)
    popwin
    smartparens))

(defun elm/post-init-company ()
  (spacemacs//elm-setup-company))

(defun elm/post-init-flycheck ()
  (spacemacs/enable-flycheck 'elm-mode))

(defun elm/init-flycheck-elm ()
  "Initialize flycheck-elm"
  (use-package flycheck-elm
    :defer t
    :init (add-hook 'flycheck-mode-hook 'flycheck-elm-setup t)))

(defun elm/init-elm-mode ()
  "Initialize elm-mode"
  (use-package elm-mode
    :mode ("\\.elm\\'" . elm-mode)
    :defer t
    :init
    (spacemacs/register-repl 'elm-mode 'elm-repl-load "elm")
    (add-hook 'elm-mode-hook 'spacemacs//elm-setup-backend)
    :config
    ;; Bind non-lsp keys
    (when (eq elm-backend 'company-elm)
      (spacemacs/set-leader-keys-for-major-mode 'elm-mode
        ;; format
        "=b" 'elm-format-buffer
        ;; oracle
        "hh" 'elm-oracle-doc-at-point
        "ht" 'elm-oracle-type-at-point)

      ;; Bind prefixes
      (dolist (x '(("m=" . "format")
                   ("mh" . "help")
                   ("mg" . "goto")
                   ("mr" . "refactor")))
        (spacemacs/declare-prefix-for-mode 'elm-mode (car x) (cdr x))))

    ;; Bind general keys
    (spacemacs/set-leader-keys-for-major-mode 'elm-mode
      ;; refactoring
      "ri" 'elm-sort-imports
      ;; repl
      "'"  'elm-repl-load
      "si" 'elm-repl-load
      "sf" 'elm-repl-push-decl
      "sF" 'spacemacs/elm-repl-push-decl-focus
      "sr" 'elm-repl-push
      "sR" 'spacemacs/elm-repl-push-focus
      ;; make
      "cb" 'elm-compile-buffer
      "cB" 'spacemacs/elm-compile-buffer-output
      "cm" 'elm-compile-main
      ;; reactor
      "Rn" 'elm-preview-buffer
      "Rm" 'elm-preview-main
      ;; package
      "pi" 'elm-import
      "pc" 'elm-package-catalog
      "pd" 'elm-documentation-lookup)

    ;; Bind prefixes
    (dolist (x '(("mp" . "package")
                 ("mc" . "compile")
                 ("mR" . "reactor")
                 ("ms" . "repl")))
      (spacemacs/declare-prefix-for-mode 'elm-mode (car x) (cdr x)))

    (evilified-state-evilify-map elm-package-mode-map
      :mode elm-package-mode
      :bindings
      "g" 'elm-package-refresh
      "v" 'elm-package-view
      "m" 'elm-package-mark
      "u" 'elm-package-unmark
      "x" 'elm-package-install
      "q" 'quit-window)))

(defun elm/init-elm-test-runner ()
  (use-package elm-test-runner
    :after elm-mode
    :init
    (spacemacs/declare-prefix-for-mode 'elm-mode "mt" "test")
    (spacemacs/set-leader-keys-for-major-mode 'elm-mode
      "tb" 'elm-test-runner-run
      "td" 'elm-test-runner-run-directory
      "tp" 'elm-test-runner-run-project
      "tr" 'elm-test-runner-rerun
      "tw" 'elm-test-runner-watch
      "t TAB" 'elm-test-runner-toggle-test-and-target)))

(defun elm/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :post-config
    (push '("*elm*" :tail t :noselect t) popwin:special-display-config)
    (push '("*elm-make*" :tail t :noselect t) popwin:special-display-config)))

(defun elm/post-init-smartparens ()
  (add-hook 'elm-mode-hook #'spacemacs//activate-smartparens))
