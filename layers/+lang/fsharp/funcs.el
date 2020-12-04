;;; packages.el --- F# Layer packages File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Maximilian Wolff <smile13241324@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//fsharp-backend ()
  "Returns selected backend."
  (if fsharp-backend
      fsharp-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'eglot))))

(defun space-macs//fsharp-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (space-macs//fsharp-backend)
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    (`lsp (space-macs|add-company-backends
            :backends company-capf
            :modes fsharp-mode))))

(defun space-macs//fsharp-setup-backend ()
  "Conditionally setup fsharp backend."
  (pcase (space-macs//fsharp-backend)
    (`lsp (lsp))
    (_ (space-macs/fsharp-eglot-jack-in))))

(defun space-macs/fsharp-eglot-jack-in ()
  "Start a new Eglot server instance or reconnect."
  (interactive)
  (call-interactively 'eglot))


