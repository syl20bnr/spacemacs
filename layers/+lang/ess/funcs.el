;;; funcs.el --- ESS Layer functions File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3


;; R

(defun space-macs//ess-r-backend ()
  "Returns selected backend."
  (if ess-r-backend
      ess-r-backend
    (cond ((configuration-layer/layer-used-p 'lsp) 'lsp)
          (t 'ess))))

(defun space-macs//ess-may-setup-r-lsp ()
  "Conditionally setup LSP based on backend."
  (when (eq (space-macs//ess-r-backend) 'lsp)
    (space-macs//ess-setup-r-lsp)))

(defun space-macs//ess-setup-r-lsp ()
  "Setup LSP backend."
  (if (configuration-layer/layer-used-p 'lsp)
      (lsp)
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))


;; Key Bindings

(defun space-macs//ess-bind-keys-for-mode (mode)
  "Bind the keys in MODE."
  (space-macs/declare-prefix-for-mode mode "md" "debug")
  (space-macs/declare-prefix-for-mode mode "mD" "devtools")
  (space-macs/declare-prefix-for-mode mode "mDc" "check")
  (space-macs/declare-prefix-for-mode mode "mE" "extra")
  (space-macs/declare-prefix-for-mode mode "mh" "help")
  (space-macs/set-leader-keys-for-major-mode mode
    "h" 'ess-doc-map              ;; help
    "d" 'ess-dev-map              ;; debug
    "D" 'ess-r-package-dev-map    ;; devtools
    "E" 'ess-extra-map            ;; extra
    ))

(defun space-macs//ess-bind-repl-keys-for-mode (mode)
  "Set the REPL keys in MODE."
  (space-macs/declare-prefix-for-mode mode "ms" "repl")
  (space-macs/set-leader-keys-for-major-mode mode
    ","  #'ess-eval-region-or-function-or-paragraph-and-step
    "'"  #'space-macs/ess-start-repl
    "si" #'space-macs/ess-start-repl
    "ss" #'ess-switch-to-inferior-or-script-buffer
    "sS" #'ess-switch-process
    "sB" #'ess-eval-buffer-and-go
    "sb" #'ess-eval-buffer
    "sd" #'ess-eval-region-or-line-and-step
    "sD" #'ess-eval-function-or-paragraph-and-step
    "sL" #'ess-eval-line-and-go
    "sl" #'ess-eval-line
    "sR" #'ess-eval-region-and-go
    "sr" #'ess-eval-region
    "sF" #'ess-eval-function-and-go
    "sf" #'ess-eval-function))

(defun space-macs/ess-bind-keys-for-julia ()
  (space-macs//ess-bind-keys-for-mode 'ess-julia-mode)
  (space-macs//ess-bind-repl-keys-for-mode 'ess-julia-mode))

(defun space-macs/ess-bind-keys-for-r ()
  (when ess-assign-key
    (define-key ess-r-mode-map ess-assign-key #'ess-insert-assign))

  (space-macs//ess-bind-keys-for-mode 'ess-r-mode)
  (space-macs//ess-bind-repl-keys-for-mode 'ess-r-mode))

(defun space-macs/ess-bind-keys-for-inferior ()
  (define-key inferior-ess-mode-map (kbd "C-j") #'comint-next-input)
  (define-key inferior-ess-mode-map (kbd "C-k") #'comint-previous-input)
  (when ess-assign-key
    (define-key inferior-ess-r-mode-map ess-assign-key #'ess-insert-assign))

  (space-macs/declare-prefix-for-mode 'inferior-ess-mode "ms" "repl")
  (space-macs/declare-prefix-for-mode 'inferior-ess-mode "me" "eval")
  (space-macs/declare-prefix-for-mode 'inferior-ess-mode "mg" "xref")
  (space-macs/set-leader-keys-for-major-mode 'inferior-ess-mode
    ","  #'ess-smart-comma
    "ss" #'ess-switch-to-inferior-or-script-buffer))


;; REPL

(defun space-macs/ess-start-repl ()
  "Start a REPL corresponding to the ess-language of the current buffer."
  (interactive)
  (cond
   ((string= "S" ess-language) (call-interactively 'R))
   ((string= "STA" ess-language) (call-interactively 'stata))
   ((string= "SAS" ess-language) (call-interactively 'SAS))
   ((string= "julia" ess-language) (call-interactively 'julia))))

(defun space-macs//ess-fix-read-only-inferior-ess-mode ()
  "Fixes a bug when `comint-prompt-read-only' in non-nil.
See https://github.com/e-macs-ess/ESS/issues/300"
  (setq-local comint-use-prompt-regexp nil)
  (setq-local inhibit-field-text-motion nil))


