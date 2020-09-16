;;; funcs.el --- Elixir Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//elixir-backend ()
  "Returns selected backend."
  (if elixir-backend
      elixir-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'alchemist))))

(defun spacemacs//elixir-setup-backend ()
  "Conditionally setup elixir backend."
  (pcase (spacemacs//elixir-backend)
    (`alchemist (spacemacs//elixir-setup-alchemist))
    (`lsp (spacemacs//elixir-setup-lsp))))

(defun spacemacs//elixir-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (spacemacs//elixir-backend)
    (`alchemist (spacemacs//elixir-setup-alchemist-company))))

(defun spacemacs//elixir-setup-dap ()
  "Conditionally setup elixir DAP integration."
  ;; currently DAP is only available using LSP
  (pcase (spacemacs//elixir-backend)
    (`lsp (spacemacs//elixir-setup-lsp-dap))))


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
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun spacemacs//elixir-setup-lsp-dap ()
  "Setup DAP integration."
  (require 'dap-elixir))


;; others

(defun spacemacs//elixir-default ()
  "Default settings for elixir buffers"

  ;; highlight all breakpoints
  (spacemacs/elixir-annotate-pry)
  ;; make C-j work the same way as RET
  (local-set-key (kbd "C-j") 'newline-and-indent))

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

(defun spacemacs/elixir-annotate-pry ()
  "Highlight breakpoint lines."
  (interactive)
  (highlight-lines-matching-regexp "require IEx; IEx.pry"))

(defun spacemacs/elixir-toggle-breakpoint ()
  "Add a breakpoint line or clear it if line is already a breakpoint."
  (interactive)
  (let ((trace "require IEx; IEx.pry")
        (line (thing-at-point 'line)))
    (if (and line (string-match trace line))
        (kill-whole-line)
      (progn
        (back-to-indentation)
        (insert trace)
        (newline-and-indent)))))
