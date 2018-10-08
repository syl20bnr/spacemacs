;;; config.el --- Python Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Adam Beckmeyer <adam_git@thebeckmeyers.xyz>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; variables

(spacemacs|define-jump-handlers julia-mode)

;; ess-mode is what the majority of people developing julia in emacs currently use
(defvar julia-mode-enable-ess nil
  "If non-nil, enable ESS in julia-mode buffers and disable julia-repl.")

;; disabled by default since most won't have lsp-mode working
(defvar julia-mode-enable-lsp nil
  "If non-nil, enable lsp-mode in julia-mode buffers.")
