;;; config.el --- Haskell Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Bjarke Vad Andersen <bjarke.vad90@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(setq haskell-modes '(haskell-mode literate-haskell-mode))

(spacemacs|define-jump-handlers haskell-mode haskell-mode-jump-to-def-or-tag)

(defvar haskell-completion-backend 'ghci
  "Completion backend used by company.
Available options are `ghci', `intero', `dante', `lsp' and `ghc-mod'. Default is
`ghci'.")

(defvar haskell-enable-hindent nil
  "Formatting with hindent; If t hindent is enabled.")
