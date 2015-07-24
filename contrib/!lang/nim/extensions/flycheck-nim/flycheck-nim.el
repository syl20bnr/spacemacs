;;; flycheck-nim.el --- Defines a flycheck syntax checker for nim

;; Copyright (c) 2015 Adam Schwalm
;; Author: Adam Schwalm <adamschwalm@gmail.com>
;; Version: 0.1

;; This program is free software: you can redistribute it and/or modify
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

;;; Code:

(require 'flycheck)
(require 'dash)

(flycheck-def-args-var flycheck-nim-args nim)

(flycheck-def-option-var flycheck-nim-experimental nil nim
  "Whether to enable experimental features in the nim compiler.
When non-nil, enables experimental features, via `--experimental'."
  :type 'boolean
  :safe #'booleanp)

(flycheck-def-option-var flycheck-nim-threads t nim
  "Whether to enable threads.
When non-nil, enables threads, via `--threads:on'."
  :type 'boolean
  :safe #'booleanp)

(flycheck-def-option-var flycheck-nim-hints "on" nim
  "Whether to enable compiler hints.
Enables or disables all hints via `--hints'."
  :type '(choice (const :tag "Off" "off")
                 (const :tag "On" "on"))
  :safe #'stringp)

(flycheck-def-option-var flycheck-nim-warnings "on" nim
  "Whether to enable compiler warnings.
Enables or disables all warnings via `--warnings'."
  :type '(choice (const :tag "Off" "off")
                 (const :tag "On" "on"))
  :safe #'stringp)

(flycheck-def-option-var flycheck-nim-specific-hints nil nim
  "Settings for specific hints from the compiler.
Enables or disables specific hints via `--hint[x]'"
  :type '(choice (const :tag "No specific hint settings" nil)
                 (repeat :tag "Specific hint settings"
                         (group (string :tag "Hint name")
                                (choice (const :tag "Off" "off")
                                        (const :tag "On" "on")))))
  :safe (lambda (l) (-all? #'flycheck-string-list-p l)))

(flycheck-def-option-var flycheck-nim-specific-warnings nil nim
  "Settings for specific warnings from the compiler.
Enables or disables specific warnings via `--warning[x]'"
  :type '(choice (const :tag "No specific warning settings" nil)
                 (repeat :tag "Specific warning settings"
                         (group (string :tag "Warning name")
                                (choice (const :tag "Off" "off")
                                        (const :tag "On" "on")))))
  :safe (lambda (l) (-all? #'flycheck-string-list-p l)))

(flycheck-define-checker nim
  "A syntax checker for the nim programming language.

See http://nim-lang.org"
  :command ("nim" "check"
            (option-flag "--experimental" flycheck-nim-experimental)
            (option-flag "--threads:on" flycheck-nim-threads)
            (option "--hints:" flycheck-nim-hints concat)
            (option "--warnings:" flycheck-nim-warnings concat)
            (eval (--map (format "--hint[%s]:%s" (car it) (cadr it))
                         flycheck-nim-specific-hints))
            (eval (--map (format "--warning[%s]:%s" (car it) (cadr it))
                         flycheck-nim-specific-warnings))
            (eval flycheck-nim-args)
            ;; Must use source-original so relative imports and
            ;; qualified references to local variables resolve correctly
            source-original)
  :error-patterns
  ((error line-start (file-name) "(" line ", "
          column ") Error:"
          (message (one-or-more not-newline)
                   (optional
                    (and "\nbut expected one of:"
                         (minimal-match (one-or-more anything)) "\n\n"))))
   (warning line-start (file-name) "(" line ", "
            column ") " (or "Hint:" "Warning:") (message) line-end))
  :error-filter
  (lambda (errors)
    (flycheck-sanitize-errors (flycheck-increment-error-columns errors)))
  :modes (nim-mode nimrod-mode))

(add-to-list 'flycheck-checkers 'nim)

(provide 'flycheck-nim)
;;; flycheck-nim.el ends here
