;;; funcs.el --- Ruby Layer functions File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; backend

(defun spacemacs//ruby-backend ()
  "Returns selected backend."
  (if ruby-backend
      ruby-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'robe))))

(defun spacemacs//ruby-setup-backend ()
  "Conditionally configure Ruby backend"
  (spacemacs//ruby-setup-version-manager)
  (pcase (spacemacs//ruby-backend)
    (`lsp (spacemacs//ruby-setup-lsp))
    (`robe (spacemacs//ruby-setup-robe))))

(defun spacemacs//ruby-setup-company ()
  "Configure backend company"
  (pcase (spacemacs//ruby-backend)
    (`robe (spacemacs//ruby-setup-robe-company))
    (`lsp nil))) ;; Company is automatically set up by lsp

(defun spacemacs//ruby-setup-dap ()
  "Conditionally setup elixir DAP integration."
  ;; currently DAP is only available using LSP
  (pcase (spacemacs//ruby-backend)
    (`lsp (spacemacs//ruby-setup-lsp-dap))))


;; lsp

(defun spacemacs//ruby-setup-lsp ()
  "Setup Ruby lsp."
  (if (configuration-layer/layer-used-p 'lsp)
      (lsp)
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun spacemacs//ruby-setup-lsp-dap ()
  "Setup DAP integration."
  (require 'dap-ruby))


;; robe

(defun spacemacs//ruby-setup-robe ()
  (robe-mode))

(defun spacemacs//ruby-setup-robe-company ()
  "Setup robe auto-completion."
  (when (configuration-layer/package-used-p 'robe)
    (spacemacs|add-company-backends
      :backends company-robe
      :modes ruby-mode enh-ruby-mode))
  (with-eval-after-load 'company-dabbrev-code
    (dolist (mode '(ruby-mode enh-ruby-mode))
      (add-to-list 'company-dabbrev-code-modes mode))))


;; version manager

(defun spacemacs//ruby-setup-version-manager ()
  "Setup ruby version manager."
  (pcase ruby-version-manager
    (`rbenv (spacemacs//enable-rbenv))))


;; rbenv

(defun spacemacs//enable-rbenv ()
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

(defun spacemacs//ruby-enable-rspec-mode ()
  "Conditionally enable `rspec-mode'"
  (when (eq 'rspec ruby-test-runner)
    (rspec-enable-appropriate-mode)))

(defun ruby/rspec-verify-directory (dir)
  "Launch tests in DIR directory.
Called interactively it prompts for a directory."
  (interactive "Drspec directory: ")
  (rspec-run-single-file dir (rspec-core-options)))

(defun spacemacs//inf-ruby-auto-enter ()
  "Automatically enters inf-ruby-mode in ruby modes' debugger breakpoints."
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter nil t))


;; ruby-test

(defun spacemacs//ruby-enable-ruby-test-mode ()
  "Conditionally enable `ruby-test-mode'"
  (when (eq 'ruby-test ruby-test-runner)
    (ruby-test-mode)))


;; minitest

(defun spacemacs//ruby-enable-minitest-mode ()
  "Conditionally enable `minitest-mode'"
  (when (eq 'minitest ruby-test-runner)
    (minitest-enable-appropriate-mode)))


;; highlight debugger keywords

(defun spacemacs/ruby-maybe-highlight-debugger-keywords ()
  "Highlight break point lines."
  (interactive)
  (when ruby-highlight-debugger-keywords
    (highlight-lines-matching-regexp "byebug")
    (highlight-lines-matching-regexp "binding.irb")
    (highlight-lines-matching-regexp "binding.pry")))
