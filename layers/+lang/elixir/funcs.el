;;; funcs.el --- Elixir Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//elixir-do-end-close-action (id action context)
  (when (eq action 'insert)
    (newline-and-indent)
    (forward-line -1)
    (indent-according-to-mode)))

(defun spacemacs//elixir-flycheck-check-on-save-only ()
  "Configure flycheck to check on save only since mix is slow."
  (setq-local flycheck-check-syntax-automatically '(mode-enabled save)))

(defun spacemacs//elixir-enable-compilation-checking ()
  "Enable compile checking if `elixir-enable-compilation-checking' is non nil."
  (when (or elixir-enable-compilation-checking)
    (flycheck-mix-setup)))
