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
    company-racer
    racer
    flycheck
    flycheck-rust
    rust-mode
    toml-mode
    ))

(defun rust/post-init-flycheck ()
  (spacemacs/add-flycheck-hook 'rust-mode-hook))

(when (configuration-layer/layer-usedp 'syntax-checking)
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
      (when (fboundp 'sp-local-pair)
        ;; Don't pair lifetime specifiers
        (sp-local-pair 'rust-mode "'" nil :actions nil))

      (spacemacs/declare-prefix-for-mode 'rust-mode "mc" "cargo")
      (spacemacs/declare-prefix-for-mode 'rust-mode "mg" "goto")
      (spacemacs/set-leader-keys-for-major-mode 'rust-mode
        "cc" 'spacemacs/rust-cargo-build
        "ct" 'spacemacs/rust-cargo-test
        "cd" 'spacemacs/rust-cargo-doc
        "cx" 'spacemacs/rust-cargo-run
        "cC" 'spacemacs/rust-cargo-clean
        "gg" 'racer-find-definition))))

(defun rust/init-toml-mode ()
  (use-package toml-mode
    :defer t))

(when (configuration-layer/layer-usedp 'auto-completion)
  (defun rust/post-init-company ()
    (spacemacs|add-company-hook rust-mode))

  (defun rust/init-company-racer ()
    (use-package company-racer
      :if (configuration-layer/package-usedp 'company)
      :defer t
      :init (push 'company-racer company-backends-rust-mode))))

(defun rust/init-racer ()
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-copy-env "RUST_SRC_PATH"))

  (use-package racer
    :if rust-enable-racer
    :defer t
    :init (spacemacs/add-to-hook 'rust-mode-hook '(racer-mode eldoc-mode))))
