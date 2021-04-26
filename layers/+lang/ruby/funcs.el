;;; funcs.el --- Ruby Layer functions File
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.



;; macros

(defmacro spacemacs|eval-for-enabled-ruby-mode (body)
  "Evaluate BODY for the currently active ruby mode.

All occurrences of `mode' in BODY are replaced with the actual mode name symbol.
All occurrences of `hook' in BODY are replaced with the actual hook name symbol.
All occurrences of `local-vars-hook' in BODY are replaced with the actual local
vars hook name symbol.
Note that `mode', `hook' and `local-vars-hook' need to be explicitly quoted
except when they are themselves inside a macro."
  (declare (debug (form)) (indent 0))
  (let* ((mode (if ruby-enable-enh-ruby-mode 'enh-ruby-mode 'ruby-mode))
         (pairs (cl-loop for old in '(mode hook local-vars-hook)
                         for new = (if (eq old 'mode)
                                       mode
                                     (intern (format "%S-%S" mode old)))
                         collect (cons new old))))
    (dolist (pair pairs)
      (let ((new (car pair))
            (old (cdr pair)))
        (cl-nsubstitute-if new 'spacemacs//ruby-substitute-predicate body)))
    `,@body))

(defun spacemacs//ruby-substitute-predicate (body)
  "Predicate to replace all `old' occurrences by `new' occurrences in BODY.
 Reccurse if BODY is a list."
  (if (listp body)
      (progn
        (cl-nsubstitute-if new 'spacemacs//ruby-substitute-predicate body)
        nil)
    (eq body old)))


;; Backend

(defun spacemacs//ruby-setup-backend ()
  "Conditionally configure Ruby backend"
  (pcase ruby-backend
    ('lsp (spacemacs//ruby-setup-lsp))
    ('robe (spacemacs//ruby-setup-robe))))

(defun spacemacs//ruby-setup-company ()
  "Configure backend company"
  ;; Company is automatically set up by lsp
  (when (eq ruby-backend 'robe)
    (spacemacs//ruby-setup-robe-company)))

(defun spacemacs//ruby-setup-dap ()
  "Conditionally setup elixir DAP integration."
  ;; currently DAP is only available using LSP
  (when (eq ruby-backend 'lsp)
    (spacemacs//ruby-setup-lsp-dap)))

;; lsp
(defun spacemacs//ruby-setup-lsp ()
  "Setup Ruby lsp."
  (if (configuration-layer/layer-used-p 'lsp)
      (lsp-deferred)
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun spacemacs//ruby-setup-lsp-dap ()
  "Setup DAP integration."
  (if ruby-enable-enh-ruby-mode
      (add-to-list 'spacemacs--dap-supported-modes 'enh-ruby-mode)
    (add-to-list 'spacemacs--dap-supported-modes 'ruby-mode))
  (require 'dap-ruby))

;; robe
(defun spacemacs//ruby-setup-robe ()
  (spacemacs/register-repl 'robe 'robe-start "robe")
  (spacemacs/add-to-hooks 'robe-jump
                          '(spacemacs-jump-handlers-ruby-mode
                            spacemacs-jump-handlers-enh-ruby-mode))
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


;; Version Manager

(defun spacemacs//ruby-setup-version-manager ()
  "Conditionally configure Ruby version manager."
  (pcase ruby-version-manager
    ('chruby (spacemacs//ruby-setup-chruby))
    ('rbenv (spacemacs//ruby-setup-rbenv ))
    ('rvm (spacemacs//ruby-setup-rvm ))))

;; chruby
(defun spacemacs//ruby-setup-chruby ()
  "Setup chruby version manager."
  (require 'chruby)
  (chruby-use-corresponding))

;; rbenv
(defun spacemacs//ruby-setup-rbenv ()
  "Setup rbenv version manager, use .ruby-version if exists."
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

;; rvm
(defun spacemacs//ruby-setup-rvm ()
  "Setup rvm version manager."
  (require 'rvm)
  (rvm-activate-corresponding-ruby))


;; Test runner

(defun spacemacs//ruby-setup-test-runner ()
  "Conditionally configure Ruby test runner."
  (pcase ruby-test-runner
    ('minitest (spacemacs//ruby-setup-minitest))
    ('rspec (spacemacs//ruby-setup-rspec ))
    ('ruby-test (spacemacs//ruby-setup-ruby-test ))))

;; minitest
(defun spacemacs//ruby-setup-minitest ()
  "Setup minitest test runner."
  (minitest-enable-appropriate-mode))

;; rspec
(defun spacemacs//ruby-setup-rspec ()
  "Setup rspec test runner."
  (when (eq ruby-version-manager 'rvm)
    (setq rspec-use-rvm t))
  (add-hook 'rspec-compilation-mode-hook 'spacemacs//inf-ruby-auto-enter)
  (rspec-enable-appropriate-mode))

(defun spacemacs//inf-ruby-auto-enter ()
  "Automatically enters inf-ruby-mode in ruby modes' debugger breakpoints."
  (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter nil t))

(defun spacemacs/rspec-verify-directory (dir)
  "Launch tests in DIR directory.
Called interactively it prompts for a directory."
  (interactive "Drspec directory: ")
  (rspec-run-single-file dir (rspec-core-options)))

;; ruby-test
(defun spacemacs//ruby-setup-ruby-test ()
  "Setup ruby-test test runner."
  (ruby-test-mode))


;; highlight debugger keywords

(defun spacemacs/ruby-maybe-highlight-debugger-keywords ()
  "Highlight break point lines."
  (interactive)
  (when ruby-highlight-debugger-keywords
    (highlight-lines-matching-regexp "byebug")
    (highlight-lines-matching-regexp "binding.irb")
    (highlight-lines-matching-regexp "binding.pry")))


;; Insert text

(defun spacemacs/ruby-insert-frozen-string-literal-comment ()
  "Insert frozen_string_literal comment at start of file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert "# frozen_string_literal: true\n")))

(defun spacemacs/ruby-insert-shebang ()
  "Insert ruby shebang at start of file."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (insert "#!/usr/bin/env ruby\n")))


;; Prettier

(defun spacemacs//ruby-add-prettier-js-before-save-hook ()
  (add-hook 'before-save-hook 'prettier-js t t))
