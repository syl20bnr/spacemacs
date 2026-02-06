;;; funcs.el --- C# Layer functions File for Spacemacs  -*- lexical-binding: nil; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Muneeb Shaikh <muneeb@reversehack.in>
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



;; backend

(defun spacemacs//csharp-setup-backend ()
  "Conditionally setup layer csharp based on backend."
  (pcase csharp-backend
    ('lsp (spacemacs//csharp-setup-lsp))))


;; lsp

(defun spacemacs//csharp-setup-lsp ()
  "Setup lsp backend."
  (add-hook 'csharp-mode-hook #'lsp))
