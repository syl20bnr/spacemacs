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


;; LSP

(defun spacemacs//ruby-setup-backend ()
  "Conditionally setup ruby backend."
  (pcase ruby-backend
    ('lsp (spacemacs//ruby-setup-lsp))))

(defun spacemacs//ruby-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
    (progn
      (lsp-ruby-enable))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun spacemacs//ruby-setup-company ()
  "Conditionally setup company based on backend."
  (message "%s settuing up company" ruby-backend)
  (pcase ruby-backend
    ('robe (spacemacs//ruby-setup-robe-company))
    ('lsp (spacemacs//ruby-setup-lsp-company)))
  (spacemacs//ruby-setup-basic-company))

(defun spacemacs//ruby-setup-robe-company ()
  "Setup robe auto-completion."
  (when (configuration-layer/package-used-p 'robe)
    (spacemacs|add-company-backends
      :backends company-robe
      :modes ruby-mode enh-ruby-mode)))

(defun spacemacs//ruby-setup-lsp-company ()
  "Setup lsp auto-completion."
  (if (configuration-layer/layer-used-p 'lsp)
      (spacemacs|add-company-backends
        :backends company-lsp
        :modes enh-ruby-mode ruby-mode)
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun spacemacs//ruby-setup-basic-company ()
  "Setup dabbrev auto-completion."
  (with-eval-after-load 'company-dabbrev-code
    (let ((mode (if ruby-enable-enh-ruby-mode 'enh-ruby-mode 'ruby-mode)))
      (add-to-list 'company-dabbrev-code-modes mode))))
