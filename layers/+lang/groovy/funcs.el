;;; funcs.el --- Groovy functions File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
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


(defun spacemacs//groovy-setup-company ()
  "Conditionally setup company based on backend."
  (pcase groovy-backend
    ;; Activate lsp company explicitly to activate
    ;; standard backends as well
    ('lsp (spacemacs|add-company-backends
            :backends company-capf
            :modes groovy-mode))
    ('company-groovy (spacemacs|add-company-backends
                       :modes groovy-mode))))

(defun spacemacs//groovy-setup-backend ()
  "Conditionally setup groovy backend."
  (when (eq groovy-backend 'lsp)
    (lsp-deferred)))


;; REPL

(defun spacemacs/groovy-send-region-switch (start end)
  "Send region content to REPL and switch to it in insert mode."
  (interactive "r")
  (groovy-send-region-and-go start end)
  (evil-insert-state))

(defun spacemacs/groovy-send-definition-switch ()
  "Send function content to REPL and switch to it in insert mode."
  (interactive)
  (groovy-send-definition-and-go)
  (evil-insert-state))

(defun spacemacs/groovy-load-file ()
  "Save buffer, load it to REPL."
  (interactive)
  (save-buffer)
  (groovy-load-file (buffer-file-name)))

(defun spacemacs/groovy-load-file-switch ()
  "Save buffer, load buffer to REPL and switch to it in insert mode."
  (interactive)
  (spacemacs/groovy-load-file)
  (switch-to-groovy nil)
  (evil-insert-state))
