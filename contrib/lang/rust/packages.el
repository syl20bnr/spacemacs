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

(defvar rust-packages
  '(
    rust-mode
    toml-mode
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun rust/init-rust-mode ()
  (use-package rust-mode
    :defer t
    :config
    (when (fboundp 'sp-local-pair) ;Don't pair lifetime specifiers
      (sp-local-pair 'rust-mode "'" nil :actions nil)))
  "Initialize rust-mode"
  )

(defun rust/init-toml-mode ()
  (use-package toml-mode
    :defer t)
  "Initialize toml-mode")
