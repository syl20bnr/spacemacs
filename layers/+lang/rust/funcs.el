;;; funcs.el --- rust Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: NJBS <DoNotTrackMeUsingThis@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/racer-describe ()
  "Show a *Racer Help* buffer for the function or type at point.
If `help-window-select' is non-nil, also select the help window."
  (interactive)
  (let ((window (racer-describe)))

    (when help-window-select
      (select-window window)))
  )

(defun spacemacs/racer-describe ()
  "Show a *Racer Help* buffer for the function or type at point.
If `help-window-select' is non-nil, also select the help window."
  (interactive)
  (let ((window (racer-describe)))
    (when help-window-select
      (select-window window))))

(defun spacemacs/rust-quick-run ()
  "Quickly run a Rust file using rustc.
Meant for a quick-prototype flow only - use `spacemacs/open-junk-file' to
open a junk Rust file, type in some code and quickly run it.
If you want to use third-party crates, create a a new project using `cargo-process-new' and run
using `cargo-process-run'."
  (interactive)
  (lexical-let ((input-file-name
                  (buffer-file-name))
                (output-file-name
                  (concat
                    temporary-file-directory
                    (file-name-nondirectory (buffer-file-name))
                    "-"
                    (md5 (buffer-file-name)))))
    (add-to-list 'compilation-finish-functions
                 (lambda (buffer status)
                   (if (string-match "finished" status)
                     (shell-command
                       (shell-quote-argument output-file-name)
                       buffer)
                     (message "Compilation failed with: %s" status))))
    (compile
      (format "rustc -o %s %s"
              (shell-quote-argument output-file-name)
              (shell-quote-argument input-file-name)))
    )
  )

(defun spacemacs//rust-setup-lsp ()
  "Setup lsp backend"
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (lsp-rust-enable)
        )
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile."))
  )

(defun spacemacs//rust-setup-racer ()
  "Setup racer backend"
  (progn
    (racer-mode)
    )
  )

(defun spacemacs//rust-setup-backend ()
  "Conditionally setup rust backend."
  (pcase rust-backend
    (`racer (spacemacs//rust-setup-racer))
    (`lsp (spacemacs//rust-setup-lsp)))
  )

(defun spacemacs//rust-setup-lsp-company ()
  "Setup lsp auto-completion."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (spacemacs|add-company-backends
          :backends company-lsp
          :modes rust-mode)
        (company-mode))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile."))
  )

(defun spacemacs//rust-setup-racer-company ()
  "Setup racer auto-completion."
        (spacemacs|add-company-backends
          :backends company-capf
          :modes rust-mode
          :variables company-tooltip-align-annotations t)
        (company-mode)
  )

(defun spacemacs//rust-setup-company ()
  "Conditionally setup company based on backend."
  (pcase rust-backend
    (`racer (spacemacs//rust-setup-racer-company))
    (`lsp (spacemacs//rust-setup-lsp-company)))
  )

(defun spacemacs//rust-setup-rust-flycheck ()
  "Setup racer rust flycheck"
  (spacemacs/enable-flycheck 'rust-mode)
  )

(defun spacemacs//rust-setup-lsp-flycheck ()
  "Setup lsp flycheck"
  (spacemacs/enable-flycheck 'lsp-mode)
  (add-hook 'lsp-mode-hook 'flycheck-mode)
  )


(defun spacemacs//setup-flycheck ()
  "Conditionally setup company based on backend."
  (pcase rust-backend
    (`racer (spacemacs//rust-setup-rust-flycheck))
    (`lsp (spacemacs//rust-setup-lsp-flycheck)))
  )
