;;; packages.el --- F# Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Maximilian Wolff <smile13241324@gmail.com>
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
  (pcase fsharp-backend
    ('lsp (lsp))
    ('eglot (eglot-ensure))))

(defun spacemacs/fsharp-load-buffer-file-focus ()
  "Send the current buffer to REPL and switch to the REPL in `insert state'."
  (interactive)
  (fsharp-load-buffer-file)
  (switch-to-buffer-other-window inferior-fsharp-buffer-name)
  (evil-insert-state))

(defun spacemacs/fsharp-eval-phrase-focus ()
  "Send the current phrase to REPL and switch to the REPL in `insert state'."
  (interactive)
  (fsharp-eval-phrase)
  (switch-to-buffer-other-window inferior-fsharp-buffer-name)
  (evil-insert-state))

(defun spacemacs/fsharp-eval-region-focus (start end)
  "Send the current phrase to REPL and switch to the REPL in `insert state'."
  (interactive "r")
  (fsharp-eval-region start end)
  (switch-to-buffer-other-window inferior-fsharp-buffer-name)
  (evil-insert-state))
