;;; funcs.el --- Haskell Layer funcs File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs-haskell//disable-electric-indent ()
  "Disable electric indent mode if available"
  ;; use only internal indentation system from haskell
  (if (fboundp 'electric-indent-local-mode)
      (electric-indent-local-mode -1)))

(defun spacemacs/haskell-format-imports ()
  "Sort and align import statements from anywhere in the source file."
  (interactive)
  (save-excursion
    (haskell-navigate-imports)
    (haskell-mode-format-imports)))


;; Completion setup functions

(defun spacemacs-haskell//setup-backend ()
  "Conditionally setup haskell backend."
  (pcase haskell-completion-backend
    (`ghci (spacemacs-haskell//setup-ghci))
    (`lsp (spacemacs-haskell//setup-lsp))
    (`intero (spacemacs-haskell//setup-intero))
    (`dante (spacemacs-haskell//setup-dante))
    (`ghc-mod (spacemacs-haskell//setup-ghc-mod))))

(defun spacemacs-haskell//setup-company ()
  "Conditionally setup haskell completion backend."
  (pcase haskell-completion-backend
    (`ghci (spacemacs-haskell//setup-ghci-company))
    (`lsp nil) ;; nothing to do, auto-configured by lsp-mode
    (`intero (spacemacs-haskell//setup-intero-company))
    (`dante (spacemacs-haskell//setup-dante-company))
    (`ghc-mod (spacemacs-haskell//setup-ghc-mod-company))))


;; ghci functions

(defun spacemacs-haskell//setup-ghci ()
  (interactive-haskell-mode))

(defun spacemacs-haskell//setup-ghci-company ()
  (spacemacs|add-company-backends
    :backends (company-ghci company-dabbrev-code company-yasnippet)
    :modes haskell-mode))

;; LSP functions

(defun spacemacs-haskell//setup-lsp ()
  "Setup lsp backend"
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        ;; The functionality we require from this is not an autoload, but rather some
        ;; top-level code that registers a LSP server type. So we need to load it
        ;; directly and can't rely on it being autoloaded.
        (require 'lsp-haskell)
        (lsp))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))


;; ghc-mod functions

(defun spacemacs-haskell//setup-ghc-mod ()
  (ghc-init))

(defun spacemacs-haskell//setup-ghc-mod-company ()
  (spacemacs|add-company-backends
    :backends (company-ghc company-dabbrev-code company-yasnippet)
    :modes haskell-mode))


;; Dante functions

(defun spacemacs-haskell//setup-dante ()
  (dante-mode)
  (add-to-list 'spacemacs-jump-handlers-haskell-mode 'xref-find-definitions))

(defun spacemacs-haskell//setup-dante-company ()
  (spacemacs|add-company-backends
    :backends (dante-company company-dabbrev-code company-yasnippet)
    :modes haskell-mode))

(defun spacemacs-haskell//dante-insert-type ()
  (interactive)
  (dante-type-at :insert))


;; Intero functions

(defun spacemacs-haskell//setup-intero ()
  (interactive-haskell-mode)
  (intero-mode)
  (add-to-list 'spacemacs-jump-handlers-haskell-mode 'intero-goto-definition))

(defun spacemacs-haskell//setup-intero-company ()
  (spacemacs|add-company-backends
    :backends (company-intero company-dabbrev-code company-yasnippet)
    :modes haskell-mode))

(defun haskell-intero/insert-type ()
  (interactive)
  (intero-type-at :insert))

(defun haskell-intero/display-repl (&optional prompt-options)
  (interactive "P")
  (let ((buffer (intero-repl-buffer prompt-options t)))
    (unless (get-buffer-window buffer 'visible)
      (display-buffer buffer))))

(defun haskell-intero/pop-to-repl (&optional prompt-options)
  (interactive "P")
  (pop-to-buffer (intero-repl-buffer prompt-options t)))

(defun haskell-intero//preserve-focus (f &rest args)
  (let ((buffer (current-buffer)))
    (apply f args)
    (pop-to-buffer buffer)))
