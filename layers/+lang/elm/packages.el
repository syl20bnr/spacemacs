;;; packages.el --- elm Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
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
  (add-hook 'elm-mode-local-vars-hook #'spacemacs//elm-setup-company))

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
    :hook (elm-mode-local-vars . spacemacs//elm-setup-backend)
    :init (spacemacs/register-repl 'elm-mode 'elm-repl-load "elm")
    :config
    (progn
      (spacemacs//elm-setup-binding
       ;; Common prefix
       '("mp" "package"
         "mc" "compile"
         "mR" "reactor"
         "ms" "repl")
       ;; Non-lsp prefix
       '("m=" "format"
         "mh" "help"
         "mg" "goto"
         "mr" "refactor")
       ;; Bind general keys
       '(;; refactoring
         "ri" elm-sort-imports
         ;; repl
         "'"  elm-repl-load
         "si" elm-repl-load
         "sf" elm-repl-push-decl
         "sF" spacemacs/elm-repl-push-decl-focus
         "sr" elm-repl-push
         "sR" spacemacs/elm-repl-push-focus
         ;; make
         "cb" elm-compile-buffer
         "cB" spacemacs/elm-compile-buffer-output
         "cm" elm-compile-main
         ;; reactor
         "Rn" elm-preview-buffer
         "Rm" elm-preview-main
         ;; package
         "pi" elm-import
         "pc" elm-package-catalog
         "pd" elm-documentation-lookup)
       ;; Bind non-lsp keys
       '(;; format
          "=b" elm-format-buffer
          ;; oracle
          "hh" elm-oracle-doc-at-point
          "ht" elm-oracle-type-at-point))

      (evilified-state-evilify elm-package-mode elm-package-mode-map
        "g" 'elm-package-refresh
        "v" 'elm-package-view
        "m" 'elm-package-mark
        "u" 'elm-package-unmark
        "x" 'elm-package-install
        "q" 'quit-window))))

(defun elm/init-elm-test-runner ()
  (use-package elm-test-runner
    :after elm-mode
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'elm-mode "mt" "test")
      (spacemacs/set-leader-keys-for-major-mode 'elm-mode
        "tb" 'elm-test-runner-run
        "td" 'elm-test-runner-run-directory
        "tp" 'elm-test-runner-run-project
        "tr" 'elm-test-runner-rerun
        "tw" 'elm-test-runner-watch
        "t TAB" 'elm-test-runner-toggle-test-and-target))))

(defun elm/pre-init-popwin ()
  (spacemacs|use-package-add-hook popwin
    :post-config
    (push '("*elm*" :tail t :noselect t) popwin:special-display-config)
    (push '("*elm-make*" :tail t :noselect t) popwin:special-display-config)))

(defun elm/post-init-smartparens ()
  (add-hook 'elm-mode-hook #'spacemacs//activate-smartparens))
