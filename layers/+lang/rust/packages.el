;;; packages.el --- Rust Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Chris Hoeppner <me@mkaito.com>
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


(defconst rust-packages
  '(
    counsel-gtags
    dap-mode
    ggtags
    ron-mode
    rustic
    smartparens))


(defun rust/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'rustic-mode))

(defun rust/pre-init-dap-mode ()
  (when (eq rust-backend 'lsp)
    (add-to-list 'spacemacs--dap-supported-modes 'rustic-mode)
    (add-hook 'rustic-mode-local-vars-hook #'spacemacs//rust-setup-dap)))

(defun rust/post-init-ggtags ()
  (add-hook 'rustic-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun rust/init-rustic ()
  (use-package rustic
    :defer t
    :mode ("\\.rs\\'" . rustic-mode)
    :init
    (add-hook 'rustic-mode-hook #'spacemacs//rust-setup-backend)

    (spacemacs/declare-prefix-for-mode 'rustic-mode "mc" "cargo")
    (spacemacs/declare-prefix-for-mode 'rustic-mode "mt" "tests")
    (spacemacs/declare-prefix-for-mode 'rustic-mode "mg" "goto")
    (spacemacs/declare-prefix-for-mode 'rustic-mode "mh" "help")
    (spacemacs/declare-prefix-for-mode 'rustic-mode "m=" "format")
    (spacemacs/set-leader-keys-for-major-mode 'rustic-mode
      "c." 'rustic-cargo-run-rerun
      "c=" 'rustic-cargo-fmt
      "ca" 'rustic-cargo-add
      "cc" 'rustic-cargo-build
      "cC" 'rustic-cargo-clean
      "cd" 'rustic-cargo-doc
      "cs" 'rustic-doc-search
      "ce" 'rustic-cargo-bench
      "ci" 'rustic-cargo-init
      "cl" 'rustic-cargo-clippy
      "cf" 'rustic-cargo-clippy-fix
      "cn" 'rustic-cargo-new
      "co" 'rustic-cargo-outdated
      "cr" 'rustic-cargo-rm
      "cu" 'rustic-cargo-update
      "cU" 'rustic-cargo-upgrade
      "cv" 'rustic-cargo-check
      "cx" 'rustic-cargo-run
      "ta" 'rustic-cargo-test
      "tt" 'rustic-cargo-current-test)

    (with-eval-after-load 'flycheck
      (spacemacs/enable-flycheck 'rustic-mode)
      (push 'rustic-clippy flycheck-checkers))

    (with-eval-after-load 'lsp-mode
      (spacemacs/set-leader-keys-for-major-mode 'rustic-mode
        "=j" 'lsp-rust-analyzer-join-lines
        "==" 'lsp-format-buffer
        "Ti" 'lsp-inlay-hints-mode
        "bD" 'lsp-rust-analyzer-status
        "bS" 'lsp-rust-switch-server
        "gp" 'lsp-rust-find-parent-module
        "gg" 'lsp-find-definition
        "hm" 'lsp-rust-analyzer-expand-macro
        "hs" 'lsp-rust-analyzer-syntax-tree
        "v" 'lsp-extend-selection

        "," 'lsp-rust-analyzer-rerun
        "."  'lsp-rust-analyzer-run))))

(defun rust/post-init-smartparens ()
  (with-eval-after-load 'smartparens
    ;; Don't pair lifetime specifiers
    (sp-local-pair 'rustic-mode "'" nil :actions nil)))

(defun rust/init-ron-mode ()
  (use-package ron-mode
    :mode ("\\.ron\\'" . ron-mode)
    :defer t))
