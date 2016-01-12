;;; funcs.el --- Rust Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Chris Hoeppner <me@mkaito.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; http://doc.crates.io/guide.html
(defun spacemacs/rust-cargo-build ()
  (interactive)
  (compile "cargo build"))

(defun spacemacs/rust-cargo-run ()
  (interactive)
  (compile "cargo run"))

(defun spacemacs/rust-cargo-test ()
  (interactive)
  (compile "cargo test"))

(defun spacemacs/rust-cargo-doc ()
  (interactive)
  (compile "cargo doc"))

(defun spacemacs/rust-cargo-clean ()
  (interactive)
  (compile "cargo clean"))
