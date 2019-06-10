;;; packages.el --- Rust Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Chris Hoeppner <me@mkaito.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq rust-packages
      '(
        cargo
        company
        counsel-gtags
        racer
        flycheck
        (flycheck-rust :requires flycheck)
        ggtags
        helm-gtags
        rust-mode
        smartparens
        toml-mode
        ))

(defun rust/init-cargo ()
  (use-package cargo
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'rust-mode "mc" "cargo")
      (spacemacs/set-leader-keys-for-major-mode 'rust-mode
        "c." 'cargo-process-repeat
        "cC" 'cargo-process-clean
        "cX" 'cargo-process-run-example
        "cc" 'cargo-process-build
        "cd" 'cargo-process-doc
        "cD" 'cargo-process-doc-open
        "ce" 'cargo-process-bench
        "cf" 'cargo-process-fmt
        "ci" 'cargo-process-init
        "cl" 'cargo-process-clippy
        "cn" 'cargo-process-new
        "co" 'cargo-process-current-file-tests
        "cs" 'cargo-process-search
        "ct" 'cargo-process-current-test
        "cu" 'cargo-process-update
        "cx" 'cargo-process-run
        "cv" 'cargo-process-check
        "t" 'cargo-process-test))))

(defun rust/init-rust-mode ()
  (use-package rust-mode
    :defer t
    :init
    (progn
      (spacemacs/add-to-hook 'rust-mode-hook '(spacemacs//rust-setup-backend))
      (spacemacs/declare-prefix-for-mode 'rust-mode "mg" "goto")
      (spacemacs/declare-prefix-for-mode 'rust-mode "mh" "help")
      (spacemacs/declare-prefix-for-mode 'rust-mode "m=" "format")
      (spacemacs/set-leader-keys-for-major-mode 'rust-mode
        "==" 'rust-format-buffer
        "q" 'spacemacs/rust-quick-run))))

(defun rust/init-toml-mode ()
  (use-package toml-mode
    :mode "/\\(Cargo.lock\\|\\.cargo/config\\)\\'"))

(defun rust/init-racer ()
  (use-package racer
    :defer t
    :commands racer-mode
    :config
    (progn
      (spacemacs/add-to-hook 'rust-mode-hook '(racer-mode))
      (spacemacs/add-to-hook 'racer-mode-hook '(eldoc-mode))
      (add-to-list 'spacemacs-jump-handlers-rust-mode 'racer-find-definition)
      (spacemacs/set-leader-keys-for-major-mode 'rust-mode
        "hh" 'spacemacs/racer-describe)
      (spacemacs|hide-lighter racer-mode)
      (evilified-state-evilify-map racer-help-mode-map
        :mode racer-help-mode))))

(defun rust/init-flycheck-rust ()
  (use-package flycheck-rust
    :defer t
    :init (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)))

(defun rust/post-init-company ()
  ;; backend specific
  (spacemacs//rust-setup-company))

(defun rust/post-init-smartparens ()
  (with-eval-after-load 'smartparens
    ;; Don't pair lifetime specifiers
    (sp-local-pair 'rust-mode "'" nil :actions nil)))

(defun rust/post-init-flycheck ()
  (spacemacs/enable-flycheck 'rust-mode))

(defun rust/post-init-ggtags ()
  (add-hook 'rust-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun rust/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'rust-mode))

(defun rust/post-init-helm-gtags ()
  (spacemacs/helm-gtags-define-keys-for-mode 'rust-mode))
