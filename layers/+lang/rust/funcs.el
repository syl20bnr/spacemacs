;;; funcs.el --- rust Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
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
  (when (eq rust-backend 'lsp)
    (spacemacs//rust-setup-lsp)))

(defun spacemacs//rust-setup-dap ()
  "Conditionally setup rust DAP integration."
  ;; currently DAP is only available using LSP
  (spacemacs//rust-setup-lsp-dap))


;; lsp
(defun spacemacs//lsp-layer-not-installed-message ()
  (message (concat "`lsp' layer is not installed, "
                   "please add `lsp' layer to your dotfile.")))

(defun spacemacs//rust-setup-lsp ()
  "Setup lsp backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (lsp-deferred)
        (spacemacs/declare-prefix-for-mode 'rustic-mode "ms" "switch")
        (spacemacs/set-leader-keys-for-major-mode 'rustic-mode
          (if lsp-use-upstream-bindings "wR" "bR") 'spacemacs/lsp-rust-analyzer-reload-workspace))
    (spacemacs//lsp-layer-not-installed-message)))

(defun spacemacs//rust-setup-lsp-dap ()
  "Setup DAP integration."
  (require 'dap-cpptools)
  (require 'dap-lldb)
  (require 'dap-gdb-lldb)
  (dap-register-debug-template "Rust::GDB Run Configuration"
                               (list :type "gdb"
                                     :request "launch"
                                     :name "GDB::Run"
                                     :gdbpath "rust-gdb"
                                     :target nil
                                     :dap-compilation "cargo build"
                                     :dap-compilation-dir "${workspaceFolder}"
                                     :cwd "${workspaceFolder}")))

(defun spacemacs/lsp-rust-analyzer-reload-workspace ()
  "Reload workspaces to pick up changes in Cargo.toml."
  (interactive)
  (when (member 'rust-analyzer (spacemacs//lsp-client-server-id))
    (lsp-rust-analyzer-reload-workspace)
    (message "Reloaded workspace")))


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
