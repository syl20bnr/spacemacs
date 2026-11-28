;;; funcs.el --- Harper layer funcs file for Spacemacs.  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: sunlin7<at>hotmail.com
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

;;; Commentary:

;; Harper integration is implemented with lsp-mode.  harper-ls is registered as
;; an *add-on* LSP client (`:add-on? t') so that lsp-mode starts it in the same
;; buffer as the primary language server (e.g. clangd for C/C++).  lsp-mode
;; aggregates diagnostics coming from all the workspaces attached to a buffer,
;; so grammar/spelling hints from harper-ls and compiler/syntax diagnostics
;; from clangd are shown together.

;;; Code:

(defun harper//harper-enabled-for-mode-p (mode)
  "Return non-nil when harper-ls should run in buffers of major mode MODE."
  (memq mode harper-ls-enabled-modes))

(defun harper//setup-lsp-client ()
  "Register harper-ls as an add-on LSP client for the enabled major modes.

The activation function only returns non-nil for the modes listed in
`harper-ls-enabled-modes'.  Because the client is marked with `:add-on?'
lsp-mode will start it alongside the primary client (its highest priority
non-add-on server, e.g. clangd) whenever one is selected for the buffer."
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection (cons harper-ls-bin harper-ls-args))
    :activation-fn (lambda (_file-name mode)
                     (harper//harper-enabled-for-mode-p mode))
    :add-on? t
    :server-id 'harper-ls)))

(defun harper//next-error-cmd ()
  "Return the flycheck or flymake command to jump to the next diagnostic.

lsp-mode surfaces diagnostics through either flycheck or flymake depending on
`lsp-diagnostics-provider' (its `:auto' setting picks flycheck when it is
loaded, otherwise flymake)."
  (cond ((bound-and-true-p flycheck-mode) 'flycheck-next-error)
        ((bound-and-true-p flymake-mode) 'flymake-goto-next-error)
        (t 'next-error)))

(defun spacemacs/harper-next-error (n)
  "Jump to the Nth next diagnostic error in the current buffer.
Interactively, N is the prefix argument.  Diagnostics from both harper-ls
(grammar/spelling) and the primary language server (e.g. clangd) are
reported by lsp-mode through flycheck or flymake, and both are navigated."
  (interactive "p")
  (let ((cmd (harper//next-error-cmd)))
    (if (eq cmd 'flycheck-next-error)
        (flycheck-next-error (or n 1))
      (funcall cmd (or n 1)))))

(defun spacemacs/harper-prev-error (n)
  "Jump to the Nth previous diagnostic error in the current buffer.
Interactively, N is the prefix argument."
  (interactive "p")
  (let ((cmd (harper//next-error-cmd)))
    (if (eq cmd 'flycheck-next-error)
        (flycheck-next-error (- (or n 1)))
      (funcall cmd (- (or n 1))))))

(defun spacemacs/harper-toggle ()
  "Toggle grammar and spell checking (harper-ls) for the current buffer.

This adds or removes the buffer's major mode from `harper-ls-enabled-modes'
and restarts the LSP workspaces so the change takes effect immediately."
  (interactive)
  (let ((mode major-mode))
    (if (harper//harper-enabled-for-mode-p mode)
        (progn
          (setq harper-ls-enabled-modes (delq mode harper-ls-enabled-modes))
          (message "Harper disabled for %s" mode))
      (setq harper-ls-enabled-modes (add-to-list 'harper-ls-enabled-modes mode))
      (message "Harper enabled for %s" mode))
    (when (featurep 'lsp-mode)
      (lsp-restart-workspace))))
