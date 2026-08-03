;;; packages.el --- Eglot Layer packages file for Spacemacs  -*- lexical-binding: nil; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Codruț Constantin Gușoi <mail+spacemacs@codrut.pro>
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


(defconst eglot-packages
  '(eglot))

(defun eglot/init-eglot ()
  (use-package eglot
    :defer t
    :config
    (add-hook 'eglot-managed-mode-hook #'spacemacs//setup-eglot-jump-handler)
    ;; These key bindings will be matched with the +tools/lsp layer on similar
    ;; functions.
    (spacemacs/set-leader-keys-for-minor-mode 'eglot--managed-mode
      ;; format
      "=" "format"
      "=b" #'eglot-format-buffer
      "=r" #'eglot-format
      "=o" #'eglot-code-action-organize-imports

      ;; code actions
      "a" "code actions"
      "aa" #'eglot-code-actions
      "af" #'eglot-code-action-quickfix
      "ai" #'eglot-code-action-inline
      "ae" #'eglot-code-action-extract
      "ao" #'eglot-code-action-organize-imports

      ;; goto
      "g" "goto"
      "gd" #'xref-find-definitions
      "gD" #'xref-find-definitions-other-window
      "gr" #'xref-find-references
      "gi" #'eglot-find-implementation
      "gt" #'eglot-find-typeDefinition
      "gC" #'eglot-find-declaration
      "gb" #'xref-go-back

      ;; backend
      "b" "backend"
      "br" #'eglot-reconnect
      "bs" #'eglot-shutdown
      "be" #'eglot-events-buffer
      "bS" #'eglot-shutdown-all

      ;; refactor
      "r" "refactor"
      "rr" #'eglot-rename
      "ri" #'eglot-code-action-inline
      "re" #'eglot-code-action-extract
      "ro" #'eglot-code-action-organize-imports
      "rf" #'eglot-format

      ;; toggles
      "T" "toggle"
      "Th" #'eglot-inlay-hints-mode)
    ))

(defun eglot/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (global-flycheck-eglot-mode)))
