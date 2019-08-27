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

(defun spacemacs//elixir-setup-backend ()
  "Conditionally setup elixir backend."
  (pcase elixir-backend
    (`alchemist (spacemacs//elixir-setup-alchemist))
    (`lsp (spacemacs//elixir-setup-lsp))))

(defun spacemacs//elixir-setup-company ()
  "Conditionally setup company based on backend."
  (if (eq elixir-backend `alchemist)
      (spacemacs//elixir-setup-alchemist-company)
    (spacemacs//elixir-setup-lsp-company)))


;;alchemist

(defun spacemacs//elixir-setup-alchemist ()
  (alchemist-mode))

(defun spacemacs//elixir-setup-alchemist-company ()
  (when (configuration-layer/package-used-p 'alchemist)
    (progn
      (spacemacs|add-company-backends
        :backends alchemist-company
        :modes elixir-mode alchemist-iex-mode)
      (company-mode))))


;;lsp

(defun spacemacs//elixir-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn (add-to-list 'exec-path elixir-ls-path) (lsp))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile."))
  (if (configuration-layer/layer-used-p 'dap)
      (progn
        (require 'dap-elixir)
        (spacemacs/set-leader-keys-for-major-mode 'elixir-mode "db" nil)
        (spacemacs/dap-bind-keys-for-mode 'elixir-mode))
    (message "`dap' layer is not installed, please add `dap' layer to your dotfile.")))

(defun spacemacs//elixir-setup-lsp-company ()
  "Setup lsp auto-completion."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (spacemacs|add-company-backends
          :backends company-lsp
          :modes elixir-mode
          :append-hooks nil
          :call-hooks t)
        (company-mode))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))


;; others

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
