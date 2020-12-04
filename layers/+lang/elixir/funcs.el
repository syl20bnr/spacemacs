;;; funcs.el --- Elixir Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//elixir-backend ()
  "Returns selected backend."
  (if elixir-backend
      elixir-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'alchemist))))

(defun space-macs//elixir-setup-backend ()
  "Conditionally setup elixir backend."
  (pcase (space-macs//elixir-backend)
    (`alchemist (space-macs//elixir-setup-alchemist))
    (`lsp (space-macs//elixir-setup-lsp))))

(defun space-macs//elixir-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (space-macs//elixir-backend)
    (`alchemist (space-macs//elixir-setup-alchemist-company))))

(defun space-macs//elixir-setup-dap ()
  "Conditionally setup elixir DAP integration."
  ;; currently DAP is only available using LSP
  (pcase (space-macs//elixir-backend)
    (`lsp (space-macs//elixir-setup-lsp-dap))))


;;alchemist

(defun space-macs//elixir-setup-alchemist ()
  (alchemist-mode))

(defun space-macs//elixir-setup-alchemist-company ()
  (when (configuration-layer/package-used-p 'alchemist)
    (progn
      (space-macs|add-company-backends
        :backends alchemist-company
        :modes elixir-mode alchemist-iex-mode)
      (company-mode))))


;;lsp

(defun space-macs//elixir-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn (add-to-list 'exec-path elixir-ls-path) (lsp))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun space-macs//elixir-setup-lsp-dap ()
  "Setup DAP integration."
  (require 'dap-elixir))


;; others

(defun space-macs//elixir-default ()
  "Default settings for elixir buffers"

  ;; highlight all breakpoints
  (space-macs/elixir-annotate-pry)
  ;; make C-j work the same way as RET
  (local-set-key (kbd "C-j") 'newline-and-indent))

(defun space-macs//elixir-looking-back-special-p (expr)
  (save-excursion
    (when (or (looking-back " ")
              (looking-back "-")) (backward-char))
    (looking-back expr)))

(defun space-macs//elixir-point-after-fn-p (id action context)
  (save-excursion
    (when (looking-back id) (backward-char))
    (looking-back "fn")))

(defun space-macs//elixir-do-end-close-action (id action context)
  (when (eq action 'insert)
    (cond ((space-macs//elixir-looking-back-special-p id)
           (insert " ") (backward-char))
          ((looking-back "(")
           (insert ") ") (backward-char) (backward-char))
          (t
           (newline-and-indent)
           (forward-line -1)
           (indent-according-to-mode)))))

(defun space-macs/elixir-annotate-pry ()
  "Highlight breakpoint lines."
  (interactive)
  (highlight-lines-matching-regexp "require IEx; IEx.pry"))

(defun space-macs/elixir-toggle-breakpoint ()
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


