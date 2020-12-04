;;; funcs.el --- Elm Layer functions File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defun space-macs//elm-backend ()
  "Returns selected backend."
  (if elm-backend
      elm-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'company-elm))))

(defun space-macs//elm-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (space-macs//elm-backend)
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    (`lsp (space-macs|add-company-backends
            :backends company-capf
            :modes elm-mode))
    (`company-elm (space-macs|add-company-backends
                    :backends elm-company
                    :modes elm-mode))))

(defun space-macs//elm-setup-backend ()
  "Conditionally setup elm backend."
  (space-macs/init-elm-mode)
  (pcase (space-macs//elm-backend)
    (`lsp (lsp))
    (`company-elm (elm-oracle-setup-completion))))


;; elm-mode

(defun space-macs/init-elm-mode ()
  "Disable electric-indent-mode and let indentation cycling feature work"
  (if (fboundp 'electric-indent-local-mode)
      (electric-indent-local-mode -1)))

(defun space-macs/elm-compile-buffer-output ()
  (interactive)
  (let* ((fname (format "%s.js" (downcase (file-name-base (buffer-file-name))))))
    (elm-compile--file (elm--buffer-local-file-name) fname)))

(defun space-macs/elm-repl-push-decl-focus ()
  "Send current function to the REPL and focus it in insert state."
  (interactive)
  (elm-repl-push-decl)
  (run-elm-interactive)
  (evil-insert-state))

(defun space-macs/elm-repl-push-focus ()
  "Send current region to the REPL and focus it in insert state."
  (elm-repl-push)
  (run-elm-interactive)
  (evil-insert-state))


