;;; funcs.el --- Ruby Layer functions File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3


;; backend

(defun space-macs//ruby-backend ()
  "Returns selected backend."
  (if ruby-backend
      ruby-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'robe))))

(defun space-macs//ruby-setup-backend ()
  "Conditionally configure Ruby backend"
  (space-macs//ruby-setup-version-manager)
  (pcase (space-macs//ruby-backend)
    (`lsp (space-macs//ruby-setup-lsp))
    (`robe (space-macs//ruby-setup-robe))))

(defun space-macs//ruby-setup-company ()
  "Configure backend company"
  (pcase (space-macs//ruby-backend)
    (`robe (space-macs//ruby-setup-robe-company))
    (`lsp nil))) ;; Company is automatically set up by lsp

(defun space-macs//ruby-setup-dap ()
  "Conditionally setup elixir DAP integration."
  ;; currently DAP is only available using LSP
  (pcase (space-macs//ruby-backend)
    (`lsp (space-macs//ruby-setup-lsp-dap))))


;; lsp

(defun space-macs//ruby-setup-lsp ()
  "Setup Ruby lsp."
  (if (configuration-layer/layer-used-p 'lsp)
      (lsp)
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun space-macs//ruby-setup-lsp-dap ()
  "Setup DAP integration."
  (require 'dap-ruby))


;; robe

(defun space-macs//ruby-setup-robe ()
  (robe-mode))

(defun space-macs//ruby-setup-robe-company ()
  "Setup robe auto-completion."
  (when (configuration-layer/package-used-p 'robe)
    (space-macs|add-company-backends
      :backends company-robe
      :modes ruby-mode enh-ruby-mode))
  (with-eval-after-load 'company-dabbrev-code
    (dolist (mode '(ruby-mode enh-ruby-mode))
      (add-to-list 'company-dabbrev-code-modes mode))))


;; version manager

(defun space-macs//ruby-setup-version-manager ()
  "Setup ruby version manager."
  (pcase ruby-version-manager
    (`rbenv (space-macs//enable-rbenv))))


;; rbenv

(defun space-macs//enable-rbenv ()
  "Enable rbenv, use .ruby-version if exists."
  (require 'rbenv)
  (let ((version-file-path (rbenv--locate-file ".ruby-version")))
    (global-rbenv-mode)
    ;; try to use the ruby defined in .ruby-version
    (if version-file-path
        (progn
          (rbenv-use (rbenv--read-version-from-file
                      version-file-path))
          (message (concat "[rbenv] Using ruby version "
                           "from .ruby-version file.")))
      (message "[rbenv] Using the currently activated ruby."))))


;; rspec

(defun space-macs//ruby-enable-rspec-mode ()
  "Conditionally enable `rspec-mode'"
  (when (eq 'rspec ruby-test-runner)
    (rspec-enable-appropriate-mode)))

(defun ruby/rspec-verify-directory (dir)
  "Launch tests in DIR directory.
Called interactively it prompts for a directory."
  (interactive "Drspec directory: ")
  (rspec-run-single-file dir (rspec-core-options)))

(defun space-macs//inf-ruby-auto-enter ()
  "Automatically enters inf-ruby-mode in ruby modes' debugger breakpoints."
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter nil t))


;; ruby-test

(defun space-macs//ruby-enable-ruby-test-mode ()
  "Conditionally enable `ruby-test-mode'"
  (when (eq 'ruby-test ruby-test-runner)
    (ruby-test-mode)))


;; minitest

(defun space-macs//ruby-enable-minitest-mode ()
  "Conditionally enable `minitest-mode'"
  (when (eq 'minitest ruby-test-runner)
    (minitest-enable-appropriate-mode)))


;; highlight debugger keywords

(defun space-macs/ruby-maybe-highlight-debugger-keywords ()
  "Highlight break point lines."
  (interactive)
  (when ruby-highlight-debugger-keywords
    (highlight-lines-matching-regexp "byebug")
    (highlight-lines-matching-regexp "binding.irb")
    (highlight-lines-matching-regexp "binding.pry")))


;; Insert text

(defun space-macs/ruby-insert-frozen-string-literal-comment ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert "# frozen_string_literal: true\n")))

(defun space-macs/ruby-insert-shebang ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert "#!/usr/bin/env ruby\n")))


