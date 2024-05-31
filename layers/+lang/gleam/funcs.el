;;; funcs.el --- Gleam layer functions file for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Qynn Schwaab <qynn@riseup.net>
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


;; lsp

(defun spacemacs//gleam-setup-lsp ()
  "Conditionally setup lsp."
  (when gleam-enable-lsp (lsp-deferred)))


;; formatting

(defun spacemacs//gleam-format ()
"Ran before saving a file when `gleam-format-on-save' is non-nil.
Either `lsp-format-buffer' when `lsp-mode' is active, `gleam-format' otherwise."
  (if lsp-mode
      (lsp-format-buffer)
    (gleam-format)))

(defun spacemacs//gleam-setup-format-on-save ()
  "Conditionally setup format on save."
  (funcall (if gleam-format-on-save 'add-hook 'remove-hook) 'gleam-mode-hook
           (lambda () (add-hook 'before-save-hook 'spacemacs//gleam-format nil t))))

(defun spacemacs//gleam-toggle-format-on-save ()
  "Toggle gleam-format-on-save."
  (interactive)
  (setq gleam-format-on-save (not gleam-format-on-save))
  (message "gleam-format-on-save set to %s" gleam-format-on-save)
  (if gleam-format-on-save
      (add-hook 'before-save-hook 'spacemacs//gleam-format nil t)
    (remove-hook 'before-save-hook 'spacemacs//gleam-format t))
  (spacemacs//gleam-setup-format-on-save))


;; execution

(defun spacemacs//gleam-shell-command (command &optional scope target runtime)
  "Execute gleam shell command.
CMD is a string, the command to execute.
SCOPE is a quoted symbol corresponding to `gleam-run-scope'
TARGET is a quoted symbol corresponding to `gleam-target'
RUNTIME is a quoted symbol corresponding to `gleam-runtime'."
  (setq cmd (concat
             (if (eq scope 'project)
                 (format "cd %s; %s" (projectile-project-root) command)
               command)
             (concat
              (format " --target %s" (symbol-name target))
              (when (and runtime (eq gleam-target 'javascript))
                (format " --runtime %s" (symbol-name runtime)))
              (when (eq scope 'module)
                (format " --module %s" (file-name-base (buffer-file-name)))))))
  (message "Running `%s'" cmd)
  (shell-command cmd))


(defun spacemacs//gleam-build ()
  "Build project. Execute \"gleam run\" inside current directory."
  (interactive)
  (spacemacs//gleam-shell-command gleam-build-command nil gleam-target nil))

(defun spacemacs//gleam-test-project ()
  "Test project. Execute \"gleam test --target <gleam-target>\" inside project root directory."
  (interactive)
  (spacemacs//gleam-shell-command gleam-test-command (intern "project") gleam-target gleam-runtime))

(defun spacemacs//gleam-run-project ()
  "Run project. Execute \"gleam run --target <gleam-target> [--runtime <gleam-runtime>]\" inside project root directory."
  (interactive)
  (spacemacs//gleam-shell-command gleam-run-command (intern "project") gleam-target gleam-runtime))

(defun spacemacs//gleam-run-module ()
  "Run module. Execute \"gleam run --module <module> --target <gleam-target> [--runtime <gleam-runtime>]\"."
  (interactive)
  (spacemacs//gleam-shell-command gleam-run-command (intern "module") gleam-target gleam-runtime))

(defun spacemacs//gleam-run ()
  "Run project or module depending on the value of `gleam-run-scope'."
  (interactive)
  (spacemacs//gleam-shell-command gleam-run-command gleam-run-scope gleam-target gleam-runtime))
