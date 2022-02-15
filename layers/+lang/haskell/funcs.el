;;; funcs.el --- Haskell Layer funcs File for Spacemacs
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



;; Completion setup functions

(defun spacemacs-haskell//setup-backend ()
  "Conditionally setup haskell backend."
  (pcase haskell-completion-backend
    ('lsp (spacemacs-haskell//setup-lsp))
    ('dante (spacemacs-haskell//setup-dante))))

(defun spacemacs-haskell//setup-company ()
  "Conditionally setup haskell completion backend."
  (when (eq haskell-completion-backend 'dante)
    (spacemacs-haskell//setup-dante-company)))


;; LSP functions

(defun spacemacs-haskell//setup-lsp ()
  "Setup lsp backend"
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        ;; The functionality we require from this is not an autoload, but rather some
        ;; top-level code that registers a LSP server type. So we need to load it
        ;; directly and can't rely on it being autoloaded.
        (require 'lsp-haskell)
        (lsp-deferred))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))


;; Dante functions

(defun spacemacs-haskell//setup-dante ()
  (dante-mode)
  (add-to-list 'spacemacs-jump-handlers 'xref-find-definitions))

(defun spacemacs-haskell//setup-dante-company ()
  (spacemacs|add-company-backends
    :backends (dante-company company-dabbrev-code company-yasnippet)
    :modes haskell-mode))

(defun spacemacs-haskell//dante-insert-type ()
  (interactive)
  (dante-type-at :insert))


;; misc

(defun spacemacs-haskell//disable-electric-indent ()
  "Disable electric indent mode if available"
  ;; use only internal indentation system from haskell
  (if (fboundp 'electric-indent-local-mode)
      (electric-indent-local-mode -1)))

(defun spacemacs/haskell-format-imports ()
  "Sort and align import statements from anywhere in the source file."
  (interactive)
  (save-excursion
    (haskell-navigate-imports)
    (haskell-mode-format-imports)))
