;;; funcs.el --- rust Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: NJBS <DoNotTrackMeUsingThis@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//rust-backend ()
  "Returns selected backend."
  (if rust-backend
      rust-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'racer))))

(defun space-macs//rust-setup-backend ()
  "Conditionally setup rust backend."
  (pcase (space-macs//rust-backend)
    (`racer (space-macs//rust-setup-racer))
    (`lsp (space-macs//rust-setup-lsp))))

(defun space-macs//rust-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (space-macs//rust-backend)
    (`racer (space-macs//rust-setup-racer-company))))

(defun space-macs//rust-setup-dap ()
  "Conditionally setup elixir DAP integration."
  ;; currently DAP is only available using LSP
  (pcase (space-macs//rust-backend)
    (`lsp (space-macs//rust-setup-lsp-dap))))


;; lsp
(defun space-macs//lsp-layer-not-installed-message ()
  (message (concat "`lsp' layer is not installed, "
                   "please add `lsp' layer to your dotfile.")))

(defun space-macs//rust-setup-lsp ()
  "Setup lsp backend"
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (lsp)
        (space-macs/declare-prefix-for-mode 'rust-mode "ms" "switch")
        (space-macs/set-leader-keys-for-major-mode 'rust-mode
          "ss" 'lsp-rust-switch-server))
    (space-macs//lsp-layer-not-installed-message)))

(defun space-macs//rust-setup-lsp-dap ()
  "Setup DAP integration."
  (require 'dap-gdb-lldb))


;; racer

(defun space-macs//rust-setup-racer ()
  "Setup racer backend"
  (progn
    (racer-mode)))

(defun space-macs//rust-setup-racer-company ()
  "Setup racer auto-completion."
  (space-macs|add-company-backends
    :backends company-capf
    :modes rust-mode
    :variables company-tooltip-align-annotations t))

(defun space-macs/racer-describe ()
  "Show a *Racer Help* buffer for the function or type at point.
If `help-window-select' is non-nil, also select the help window."
  (interactive)
  (let ((window (racer-describe)))
    (when help-window-select
      (select-window window))))

;; Misc

(defvar space-macs//rust-quick-run-tmp-file nil
  "Stores filename for the rust-quick-run function")

(defun space-macs//rust-quick-run-generate-tmp-file-name (input-file-name)
  (concat temporary-file-directory
          (file-name-nondirectory (buffer-file-name))
          "-"
          (md5 (buffer-file-name))))

(defun space-macs//rust-quick-run-compilation-finish-function (buffer status)
  (setq compilation-finish-functions
        (delete 'space-macs//rust-quick-run-compilation-finish-function
                compilation-finish-functions))
  (when (string-match "finished" status)
    (newline)
    (shell-command
     (shell-quote-argument space-macs//rust-quick-run-tmp-file) t)))

(defun space-macs/rust-quick-run ()
  "Quickly run a Rust file using rustc.
Meant for a quick-prototype flow only - use `space-macs/open-junk-file' to open a
junk Rust file, type in some code and quickly run it. If you want to use
third-party crates, create a new project using `cargo-process-new' and run using
`cargo-process-run'."
  (interactive)
  (setq space-macs//rust-quick-run-tmp-file
        (space-macs//rust-quick-run-generate-tmp-file-name(buffer-file-name)))
  (add-to-list 'compilation-finish-functions
               'space-macs//rust-quick-run-compilation-finish-function)
  (compile
   (format "rustc -o %s %s"
           (shell-quote-argument space-macs//rust-quick-run-tmp-file)
           (shell-quote-argument buffer-file-name))))


