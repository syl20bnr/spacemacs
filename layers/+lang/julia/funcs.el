;;; funcs.el --- Julia Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Adam Beckmeyer <adam_git@thebeckmeyers.xyz>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//julia-setup-buffer ()
  "Setup ESS and/or lsp for buffer depending on config."
  (when (not julia-mode-enable-ess)
    (spacemacs//julia-setup-repl))
  (when julia-mode-enable-lsp
    (spacemacs//julia-setup-lsp)))

(defun spacemacs//julia-setup-repl ()
  "Start julia-repl minor mode and configure for buffer."
  (julia-repl-mode))

(defun spacemacs//julia-setup-lsp ()
  "Start lsp-mode and configure for buffer."
  (if (configuration-layer/layer-used-p 'lsp)
      (lsp)
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))
