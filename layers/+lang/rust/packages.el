;;; packages.el --- Rust Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Chris Hoeppner <me@mkaito.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq rust-packages
  '(
    company
    racer
    flycheck
    flycheck-rust
    rust-mode
    toml-mode
    ))

(when (configuration-layer/layer-usedp 'syntax-checking)
  (defun rust/post-init-flycheck ()
    (spacemacs/add-flycheck-hook 'rust-mode))

  (defun rust/init-flycheck-rust ()
    (use-package flycheck-rust
      :if (configuration-layer/package-usedp 'flycheck)
      :defer t
      :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))))

(defun rust/init-rust-mode ()
  (use-package rust-mode
    :defer t
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'rust-mode "mc" "cargo")
      (spacemacs/set-leader-keys-for-major-mode 'rust-mode
        "="  'rust-format-buffer
        "cC" 'spacemacs/rust-cargo-clean
        "cc" 'spacemacs/rust-cargo-build
        "cd" 'spacemacs/rust-cargo-doc
        "cf" 'spacemacs/rust-cargo-fmt))))
        "ct" 'spacemacs/rust-cargo-test
        "cx" 'spacemacs/rust-cargo-run

(defun rust/init-toml-mode ()
  (use-package toml-mode
    :mode "/\\(Cargo.lock\\|\\.cargo/config\\)\\'"))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun rust/post-init-company ()
    (push 'company-capf company-backends-rust-mode)
    (spacemacs|add-company-hook rust-mode)
    (add-hook 'rust-mode-hook
              (lambda ()
                (setq-local company-tooltip-align-annotations t)))))

(defun rust/post-init-smartparens ()
  (with-eval-after-load 'smartparens
    ;; Don't pair lifetime specifiers
    (sp-local-pair 'rust-mode "'" nil :actions nil)))

(defun rust/init-racer ()
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-copy-env "RUST_SRC_PATH"))

  (use-package racer
    :diminish racer-mode
    :defer t
    :init
    (progn
      (spacemacs/add-to-hook 'rust-mode-hook '(racer-mode eldoc-mode))
      (spacemacs/declare-prefix-for-mode 'rust-mode "mg" "goto")
      (spacemacs/set-leader-keys-for-major-mode 'rust-mode
        "gg" 'racer-find-definition))))
