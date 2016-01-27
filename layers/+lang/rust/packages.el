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
    rustfmt
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
        "cc" 'spacemacs/rust-cargo-build
        "ct" 'spacemacs/rust-cargo-test
        "cd" 'spacemacs/rust-cargo-doc
        "cx" 'spacemacs/rust-cargo-run
        "cC" 'spacemacs/rust-cargo-clean))))

(defun rust/init-toml-mode ()
  (use-package toml-mode
    :defer t))

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
    :defer t
    :init
    (progn
      (spacemacs/add-to-hook 'rust-mode-hook '(racer-mode eldoc-mode))
      (spacemacs/declare-prefix-for-mode 'rust-mode "mg" "goto")
      (spacemacs/set-leader-keys-for-major-mode 'rust-mode
        "gg" 'racer-find-definition))))

(defun rust/init-rustfmt ()
  (use-package rustfmt
    :defer t
    :init
    (progn
      (when rust-enable-rustfmt-on-save
          (spacemacs/add-to-hook 'rust-mode-hook
                                 '(rustfmt-enable-on-save)))

      (spacemacs/set-leader-keys-for-major-mode 'rust-mode
        "=" 'rustfmt-format-buffer))))
