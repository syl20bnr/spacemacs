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
  (pcase (spacemacs//fsharp-backend)
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    (`lsp (spacemacs|add-company-backends
            :backends company-capf
            :modes fsharp-mode))))

(defun spacemacs//fsharp-setup-backend ()
  "Conditionally setup fsharp backend."
  (pcase (spacemacs//fsharp-backend)
    (`lsp (lsp))
    (_ (spacemacs/fsharp-eglot-jack-in))))

(defun spacemacs/fsharp-eglot-jack-in ()
  "Start a new Eglot server instance or reconnect."
  (interactive)
  (call-interactively 'eglot))
