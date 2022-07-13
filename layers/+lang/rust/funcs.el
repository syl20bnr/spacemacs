;;; funcs.el --- rust Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: NJBS <DoNotTrackMeUsingThis@gmail.com>
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


(defun spacemacs//rust-setup-backend ()
  "Conditionally setup rust backend."
  (pcase rust-backend
    ('racer (spacemacs//rust-setup-racer))
    ('lsp (spacemacs//rust-setup-lsp))))

(defun spacemacs//rust-setup-company ()
  "Conditionally setup company based on backend."
  (when (eq rust-backend 'racer)
    (spacemacs//rust-setup-racer-company)))

(defun spacemacs//rust-setup-dap ()
  "Conditionally setup elixir DAP integration."
  ;; currently DAP is only available using LSP
  (when (eq rust-backend 'lsp)
    (spacemacs//rust-setup-lsp-dap)))


;; lsp
(defun spacemacs//lsp-layer-not-installed-message ()
  (message (concat "`lsp' layer is not installed, "
                   "please add `lsp' layer to your dotfile.")))

(defun spacemacs/lsp-rust-switch-server ()
  "Switch between rust-analyzer and rls."
  (interactive)
  (lsp-rust-switch-server)
  (call-interactively 'lsp-workspace-restart))

(defun spacemacs//rust-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (lsp-deferred)
        (spacemacs/declare-prefix-for-mode 'rust-mode "ms" "switch")
        (spacemacs/set-leader-keys-for-major-mode 'rust-mode
          "ss" 'spacemacs/lsp-rust-switch-server
          (if lsp-use-upstream-bindings "wR" "bR") 'spacemacs/lsp-rust-analyzer-reload-workspace))
    (spacemacs//lsp-layer-not-installed-message)))

(defun spacemacs//rust-setup-lsp-dap ()
  "Setup DAP integration."
  (require 'dap-gdb-lldb))

(defun spacemacs/lsp-rust-analyzer-reload-workspace ()
  "Reload workspaces to pick up changes in Cargo.toml.
Only applies to rust-analyzer, since rls automatically picks up changes already."
  (interactive)
  (if (member 'rust-analyzer (spacemacs//lsp-client-server-id))
      (progn
        (lsp-rust-analyzer-reload-workspace)
        (message "Reloaded workspace"))
    (message "RLS reloads automatically, and doesn't require an explicit reload")))

(when (configuration-layer/package-used-p 'cargo)
  (defun spacemacs//cargo-maybe-reload ()
    "Reload the workspace conditionally.
When one of the following is true, it won't reload:
- Backend is not rust-analyzer.
- `cargo-process-reload-on-modify' is nil."
    (when (and cargo-process-reload-on-modify
               (eq rust-backend 'lsp)
               (member 'rust-analyzer (spacemacs//lsp-client-server-id)))
      (lsp-rust-analyzer-reload-workspace)))

  (defun spacemacs/cargo-process-repeat ()
    "Run last cargo process command, and conditionally reload the workspace."
    (interactive)
    (call-interactively 'cargo-process-repeat)
    (when (member (car cargo-process-last-command)
                  '("Add" "Remove" "Upgrade"))
      (spacemacs//cargo-maybe-reload)))

  (defun spacemacs/cargo-process-add ()
    "Run the cargo add command, and conditionally reload the workspace."
    (interactive)
    (call-interactively 'cargo-process-add)
    (spacemacs//cargo-maybe-reload))

  (defun spacemacs/cargo-process-rm()
    "Run the cargo rm command, and conditionally reload the workspace."
    (interactive)
    (call-interactively 'cargo-process-rm)
    (spacemacs//cargo-maybe-reload))

  (defun spacemacs/cargo-process-upgrade()
    "Run the cargo upgrade command, and conditionally reload the workspace."
    (interactive)
    (call-interactively 'cargo-process-upgrade)
    (spacemacs//cargo-maybe-reload)))


;; racer

(defun spacemacs//rust-setup-racer ()
  "Setup racer backend"
  (progn
    (racer-mode)))

(defun spacemacs//rust-setup-racer-company ()
  "Setup racer auto-completion."
  (spacemacs|add-company-backends
    :backends company-capf
    :modes rust-mode
    :variables company-tooltip-align-annotations t))

(defun spacemacs/racer-describe ()
  "Show a *Racer Help* buffer for the function or type at point.
If `help-window-select' is non-nil, also select the help window."
  (interactive)
  (let ((window (racer-describe)))
    (when help-window-select
      (select-window window))))

;; Misc

(defvar spacemacs//rust-quick-run-tmp-file nil
  "Stores filename for the rust-quick-run function")

(defun spacemacs//rust-quick-run-generate-tmp-file-name (input-file-name)
  (concat temporary-file-directory
          (file-name-nondirectory (buffer-file-name))
          "-"
          (md5 (buffer-file-name))))

(defun spacemacs//rust-quick-run-compilation-finish-function (buffer status)
  (setq compilation-finish-functions
        (delete 'spacemacs//rust-quick-run-compilation-finish-function
                compilation-finish-functions))
  (when (string-match "finished" status)
    (newline)
    (shell-command
     (shell-quote-argument spacemacs//rust-quick-run-tmp-file) t)))

(defun spacemacs/rust-quick-run ()
  "Quickly run a Rust file using rustc.
Meant for a quick-prototype flow only - use `spacemacs/open-junk-file' to open a
junk Rust file, type in some code and quickly run it. If you want to use
third-party crates, create a new project using `cargo-process-new' and run using
`cargo-process-run'."
  (interactive)
  (setq spacemacs//rust-quick-run-tmp-file
        (spacemacs//rust-quick-run-generate-tmp-file-name(buffer-file-name)))
  (add-to-list 'compilation-finish-functions
               'spacemacs//rust-quick-run-compilation-finish-function)
  (compile
   (format "rustc -o %s %s"
           (shell-quote-argument spacemacs//rust-quick-run-tmp-file)
           (shell-quote-argument buffer-file-name))))
