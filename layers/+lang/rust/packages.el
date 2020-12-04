;;; packages.el --- Rust Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Chris Hoeppner <me@mkaito.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defconst rust-packages
  '(
    cargo
    company
    counsel-gtags
    dap-mode
    flycheck
    (flycheck-rust :requires flycheck)
    ggtags
    helm-gtags
    ron-mode
    racer
    rust-mode
    smartparens
    toml-mode))

(defun rust/init-cargo ()
  (use-package cargo
    :defer t
    :init
    (progn
      (space-macs/declare-prefix-for-mode 'rust-mode "mc" "cargo")
      (space-macs/set-leader-keys-for-major-mode 'rust-mode
        "c." 'cargo-process-repeat
        "ca" 'cargo-process-add
        "cA" 'cargo-process-audit
        "cc" 'cargo-process-build
        "cC" 'cargo-process-clean
        "cd" 'cargo-process-doc
        "cD" 'cargo-process-doc-open
        "ce" 'cargo-process-bench
        "cE" 'cargo-process-run-example
        "cf" 'cargo-process-fmt
        "ci" 'cargo-process-init
        "cl" 'cargo-process-clippy
        "cn" 'cargo-process-new
        "co" 'cargo-process-current-file-tests
        "cr" 'cargo-process-rm
        "cs" 'cargo-process-search
        "ct" 'cargo-process-current-test
        "cu" 'cargo-process-update
        "cU" 'cargo-process-upgrade
        "cx" 'cargo-process-run
        "cX" 'cargo-process-run-bin
        "cv" 'cargo-process-check
        "t" 'cargo-process-test))))

(defun rust/post-init-company ()
  ;; backend specific
  (space-macs//rust-setup-company))

(defun rust/post-init-counsel-gtags ()
  (space-macs/counsel-gtags-define-keys-for-mode 'rust-mode))

(defun rust/pre-init-dap-mode ()
  (pcase (space-macs//rust-backend)
    (`lsp (add-to-list 'space-macs--dap-supported-modes 'rust-mode)))
  (add-hook 'rust-mode-local-vars-hook #'space-macs//rust-setup-dap))

(defun rust/post-init-flycheck ()
  (space-macs/enable-flycheck 'rust-mode))

(defun rust/init-flycheck-rust ()
  (use-package flycheck-rust
    :defer t
    :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(defun rust/post-init-ggtags ()
  (add-hook 'rust-mode-local-vars-hook #'space-macs/ggtags-mode-enable))

(defun rust/post-init-helm-gtags ()
  (space-macs/helm-gtags-define-keys-for-mode 'rust-mode))

(defun rust/init-racer ()
  (use-package racer
    :defer t
    :commands racer-mode
    :config
    (progn
      (space-macs/add-to-hook 'rust-mode-hook '(racer-mode))
      (space-macs/add-to-hook 'racer-mode-hook '(eldoc-mode))
      (add-to-list 'space-macs-jump-handlers-rust-mode 'racer-find-definition)
      (space-macs/set-leader-keys-for-major-mode 'rust-mode
        "hh" 'space-macs/racer-describe)
      (space-macs|hide-lighter racer-mode)
      (evilified-state-evilify-map racer-help-mode-map
        :mode racer-help-mode))))

(defun rust/init-rust-mode ()
  (use-package rust-mode
    :defer t
    :init
    (progn
      (space-macs/add-to-hook 'rust-mode-hook '(space-macs//rust-setup-backend))
      (space-macs/declare-prefix-for-mode 'rust-mode "mg" "goto")
      (space-macs/declare-prefix-for-mode 'rust-mode "mh" "help")
      (space-macs/declare-prefix-for-mode 'rust-mode "m=" "format")
      (space-macs/set-leader-keys-for-major-mode 'rust-mode
        "==" 'rust-format-buffer
        "q" 'space-macs/rust-quick-run))))

(defun rust/post-init-smartparens ()
  (with-eval-after-load 'smartparens
    ;; Don't pair lifetime specifiers
    (sp-local-pair 'rust-mode "'" nil :actions nil)))

(defun rust/init-toml-mode ()
  (use-package toml-mode
    :mode "/\\(Cargo.lock\\|\\.cargo/config\\)\\'"))

(defun rust/init-ron-mode ()
  (use-package ron-mode
    :mode ("\\.ron\\'" . ron-mode)
    :defer t))


