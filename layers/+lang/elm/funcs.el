;;; funcs.el --- Elm Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//elm-backend ()
  "Returns selected backend."
  (if elm-backend
      elm-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp)
     (t 'company-elm))))

(defun spacemacs//elm-setup-company ()
  "Conditionally setup company based on backend."
  (pcase (spacemacs//elm-backend)
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    (`lsp (spacemacs|add-company-backends
            :backends company-capf
            :modes elm-mode))
    (`company-elm (spacemacs|add-company-backends
                    :backends elm-company
                    :modes elm-mode))))

(defun spacemacs//elm-setup-backend ()
  "Conditionally setup elm backend."
  (spacemacs/init-elm-mode)
  (pcase (spacemacs//elm-backend)
    (`lsp (lsp))
    (`company-elm (elm-oracle-setup-completion))))


;; elm-mode

(defun spacemacs/init-elm-mode ()
  "Disable electric-indent-mode and let indentation cycling feature work"
  (if (fboundp 'electric-indent-local-mode)
      (electric-indent-local-mode -1)))

(defun spacemacs/elm-compile-buffer-output ()
  (interactive)
  (let* ((fname (format "%s.js" (downcase (file-name-base (buffer-file-name))))))
    (elm-compile--file (elm--buffer-local-file-name) fname)))

(defun spacemacs/elm-repl-push-decl-focus ()
  "Send current function to the REPL and focus it in insert state."
  (interactive)
  (elm-repl-push-decl)
  (run-elm-interactive)
  (evil-insert-state))

(defun spacemacs/elm-repl-push-focus ()
  "Send current region to the REPL and focus it in insert state."
  (elm-repl-push)
  (run-elm-interactive)
  (evil-insert-state))
