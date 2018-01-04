;;; funcs.el --- Elixir Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//elixir-looking-back-special-p (expr)
  (save-excursion
    (when (or (looking-back " ")
              (looking-back "-")) (backward-char))
    (looking-back expr)))

(defun spacemacs//elixir-point-after-fn-p (id action context)
  (save-excursion
    (when (looking-back id) (backward-char))
    (looking-back "fn")))

(defun spacemacs//elixir-do-end-close-action (id action context)
  (when (eq action 'insert)
    (cond ((spacemacs//elixir-looking-back-special-p id)
           (insert " ") (backward-char))
          ((looking-back "(")
           (insert ") ") (backward-char) (backward-char))
          (t
           (newline-and-indent)
           (forward-line -1)
           (indent-according-to-mode)))))

(defun spacemacs//elixir-enable-compilation-checking ()
  "Enable compile checking if `elixir-enable-compilation-checking' is non nil."
  (when (or elixir-enable-compilation-checking)
    (flycheck-mix-setup)
    ;; enable credo only if there are no compilation errors
    (flycheck-add-next-checker 'elixir-mix '(warning . elixir-credo))))
