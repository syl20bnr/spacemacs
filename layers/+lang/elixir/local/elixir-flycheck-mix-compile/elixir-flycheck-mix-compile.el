;;; elixir-flycheck-mix-compile.el --- Elixir flycheck integration -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Tomasz Kowal

;; Author: Tomasz Kowal <tomekowal@gmail.com>
;; Keywords: Elixir flycheck mix
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package should be distributed with =mix_compile_helper=.
;; Make sure it is in your PATH

;;; Code:
(require 'flycheck)

;; :command uses source-original, source-inplace copies the file
;; which makes mix throw errors
(flycheck-define-checker
 elixir-mix-compile
 "Defines a checker for elixir with mix compile"
 :command ("mix_compile_helper" source-original)
 :error-patterns
 ((warning line-start
            (file-name)
            ":"
            line
            ": warning: "
            (message)
            line-end)
   (error line-start
          "** ("
          (one-or-more word)
          "Error) "
          (file-name)
          ":"
          line
          ": "
          (message)
          line-end))
 :modes (elixir-mode))

;;;###autoload
(defun elixir-flycheck-mix-compile-setup ()
  "Setup Flycheck for Elixir."
  (add-to-list 'flycheck-checkers 'elixir-mix-compile))

(provide 'elixir-flycheck-mix-compile)
;;; elixir-flycheck-mix-compile.el ends here
