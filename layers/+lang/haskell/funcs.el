;;; funcs.el --- Haskell Layer funcs File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3


;; Completion setup functions

(defun space-macs//haskell-backend ()
  "Returns selected backend."
  (if haskell-completion-backend
      haskell-completion-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'dante))))

(defun space-macs-haskell//setup-backend ()
  "Conditionally setup haskell backend."
  (pcase (space-macs//haskell-backend)
    (`lsp (space-macs-haskell//setup-lsp))
    (`dante (space-macs-haskell//setup-dante))))

(defun space-macs-haskell//setup-company ()
  "Conditionally setup haskell completion backend."
  (pcase (space-macs//haskell-backend)
    (`lsp nil) ;; nothing to do, auto-configured by lsp-mode
    (`dante (space-macs-haskell//setup-dante-company))))


;; LSP functions

(defun space-macs-haskell//setup-lsp ()
  "Setup lsp backend"
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        ;; The functionality we require from this is not an autoload, but rather some
        ;; top-level code that registers a LSP server type. So we need to load it
        ;; directly and can't rely on it being autoloaded.
        (require 'lsp-haskell)
        (lsp))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))


;; Dante functions

(defun space-macs-haskell//setup-dante ()
  (dante-mode)
  (add-to-list 'space-macs-jump-handlers 'xref-find-definitions))

(defun space-macs-haskell//setup-dante-company ()
  (space-macs|add-company-backends
    :backends (dante-company company-dabbrev-code company-yasnippet)
    :modes haskell-mode))

(defun space-macs-haskell//dante-insert-type ()
  (interactive)
  (dante-type-at :insert))


;; misc

(defun space-macs-haskell//disable-electric-indent ()
  "Disable electric indent mode if available"
  ;; use only internal indentation system from haskell
  (if (fboundp 'electric-indent-local-mode)
      (electric-indent-local-mode -1)))

(defun space-macs/haskell-format-imports ()
  "Sort and align import statements from anywhere in the source file."
  (interactive)
  (save-excursion
    (haskell-navigate-imports)
    (haskell-mode-format-imports)))


