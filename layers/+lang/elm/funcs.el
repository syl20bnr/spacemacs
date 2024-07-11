;;; funcs.el --- Elm Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defun spacemacs//elm-setup-company ()
  "Conditionally setup company based on backend."
  (pcase elm-backend
    ('lsp
     (spacemacs|add-company-backends ;; Activate lsp company explicitly to activate
       :backends company-capf        ;; standard backends as well
       :modes elm-mode))
    ('company-elm
      (message "Warning: `company-elm' backend is no longer supported for `elm' layer, use `lsp' instead."))))

(defun spacemacs//elm-setup-backend ()
  "Conditionally setup elm backend."
  (spacemacs/init-elm-mode)
  (when (eq elm-backend 'lsp) (lsp-deferred)))


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
