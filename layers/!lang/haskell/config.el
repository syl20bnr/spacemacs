;;; config.el --- Haskell Layer configuration File for Spacemacs
;;
;; Copyright (c) 2015 Bjarke Vad Andersen
;;
;; Author: Bjarke Vad Andersen <bjarke.vad90@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(spacemacs|defvar-company-backends haskell-mode)
(spacemacs|defvar-company-backends haskell-cabal-mode)

(defvar haskell-enable-ghci-ng-support nil
  "If non-nil ghci-ng support is enabled")

(defvar haskell-enable-shm-support nil
  "If non-nil structured-haskell-mode support is enabled")

(defvar haskell-enable-hindent-style nil
  "Style to use for formatting with hindent; available are: fundamental johan-tibell chris-done gibiansky. If nil hindent is disabled.")

(defvar haskell-enable-ghc-mod-support t
  "If non-nil ghc-mod support is enabled")
