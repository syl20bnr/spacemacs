;;; packages.el --- F# Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Maximilian Wolff <smile13241324@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//fsharp-backend ()
  "Returns selected backend."
  (if fsharp-backend
      fsharp-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'eglot))))

(defun spacemacs//fsharp-setup-company ()
  "Conditionally setup company based on backend."
  ;; Activate lsp company explicitly to activate
  ;; standard backends as well
  ;; Eglot and LSP-mode use the same company-backend.
    (spacemacs|add-company-backends
      :backends company-capf
      :modes fsharp-mode
      :variables company-tooltip-align-annotations t))

(defun spacemacs//fsharp-setup-backend ()
  "Conditionally setup fsharp backend."
  (pcase (spacemacs//fsharp-backend)
    (`lsp (lsp))
    (_ (eglot-ensure))))
