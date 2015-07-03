;;; packages.el --- Rust Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2015 Chris Hoeppner & Contributors
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
    flycheck
    flycheck-rust
    rust-mode
    toml-mode
    ))

(defun rust/post-init-flycheck ()
  (add-hook 'rust-mode-hook 'flycheck-mode))

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

      (evil-leader/set-key-for-mode 'rust-mode
        "mcc" 'spacemacs/rust-cargo-build
        "mct" 'spacemacs/rust-cargo-test
        "mcd" 'spacemacs/rust-cargo-doc
        "mcx" 'spacemacs/rust-cargo-run))))

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
