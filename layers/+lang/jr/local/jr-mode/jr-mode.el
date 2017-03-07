
;; Template taken from http://cc-mode.sourceforge.net/derived-mode-ex.el
;; Author:     Bryan Bell
;; Maintainer: Bryan Bell
;; Created:    February 2007
;; Version:    0.12
;; Keywords:    JR programming language, cc-mode, emacs


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;Code

(require 'cc-mode)

;; These are only required at compile time to get the sources for the
;; language constants.  (The cc-fonts require and the font-lock
;; related constants could additionally be put inside an
;; (eval-after-load "font-lock" ...) but then some trickery is
;; necessary to get them compiled.)
(eval-when-compile
  (require 'cc-langs)
  (require 'cc-fonts))

(eval-and-compile
  ;; Make our mode known to the language constant system.  Use Java
  ;; mode as the fallback for the constants we don't change here.
  ;; This needs to be done also at compile time since the language
  ;; constants are evaluated then.
  (c-add-language 'jr-mode 'java-mode))

(c-lang-defconst c-primative-type-kwds
  "Primitive type keywords.  As opposed to the other keyword lists, the
keywords listed here are fontified with the type face instead of the
keyword face."
  jr (append (list "process" "sem") (c-lang-const c-primative-type-kwds)))

(c-lang-defconst c-block-stmt-1-kwds
  "Statement keywords followed directly by a substatement."
  jr (append (list "st" "by" "elseafter") (c-lang-const c-block-stmt-1-kwds)))

;; List of keywords for JR. It's a hack to list all the keywords as
;; c-modifiers-kwds, since many of them are not modifiers, but it works okay.
(c-lang-defconst c-modifier-kwds
  jr (append (list "inni" "remote" "as" "call" "cap" "forward" "handler" "noop"
                   "on" "op" "over" "P" "receive" "remote" "reply" "send" "V"
                   "view" "vm" "with") (c-lang-const c-modifier-kwds)))


(defcustom jr-font-lock-extra-types nil
  "*List of extra types (aside from the type keywords) to recognize in jr mode.
Each list item should be a regexp matching a single identifier.")

(defconst jr-font-lock-keywords-1 (c-lang-const c-matchers-1 jr)
  "Minimal highlighting for jr mode.")

(defconst jr-font-lock-keywords-2 (c-lang-const c-matchers-2 jr)
  "Fast normal highlighting for jr mode.")

(defconst jr-font-lock-keywords-3 (c-lang-const c-matchers-3 jr)
  "Accurate normal highlighting for jr mode.")

(defvar jr-font-lock-keywords jr-font-lock-keywords-3
  "Default expressions to highlight in jr mode.")

(defvar jr-mode-syntax-table nil
  "Syntax table used in jr-mode buffers.")
(or jr-mode-syntax-table
    (setq jr-mode-syntax-table
          (funcall (c-lang-const c-make-mode-syntax-table jr))))

(defvar jr-mode-abbrev-table nil
  "Abbreviation table used in jr-mode buffers.")
(c-define-abbrev-table 'jr-mode-abbrev-table
  ;; Keywords that if they occur first on a line might alter the
  ;; syntactic context, and which therefore should trig reindentation
  ;; when they are completed.
  '(("else" "else" c-electric-continued-statement 0)
    ("while" "while" c-electric-continued-statement 0)
    ("catch" "catch" c-electric-continued-statement 0)
    ("finally" "finally" c-electric-continued-statement 0)))

(defvar jr-mode-map (let ((map (c-make-inherited-keymap)))
                      ;; Add bindings which are only useful for JR
                      map)
  "Keymap used in jr-mode buffers.")

(easy-menu-define jr-menu jr-mode-map "JR Mode Commands"
  ;; Can use `jr' as the language for `c-mode-menu'
  ;; since its definition covers any language.  In
  ;; this case the language is used to adapt to the
  ;; nonexistence of a cpp pass and thus removing some
  ;; irrelevant menu alternatives.
  (cons "jr" (c-lang-const c-mode-menu jr)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.jr\\'" . jr-mode))

;;;###autoload
(defun jr-mode ()
  "Major mode for editing JR code.

The hook `c-mode-common-hook' is run with no args at mode
initialization, then `jr-mode-hook'.

Key bindings:
\\{jr-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (c-initialize-cc-mode t)
  (set-syntax-table jr-mode-syntax-table)
  (setq major-mode 'jr-mode
        mode-name "jr"
        local-abbrev-table jr-mode-abbrev-table
        abbrev-mode t)
  (use-local-map c-mode-map)
  ;; `c-init-language-vars' is a macro that is expanded at compile
  ;; time to a large `setq' with all the language variables and their
  ;; customized values for our language.
  (c-init-language-vars jr-mode)
  ;; `c-common-init' initializes most of the components of a CC Mode
  ;; buffer, including setup of the mode menu, font-lock, etc.
  ;; There's also a lower level routine `c-basic-common-init' that
  ;; only makes the necessary initialization to get the syntactic
  ;; analysis and similar things working.
  (c-common-init 'jr-mode)
  (easy-menu-add jr-menu)
  (run-hooks 'c-mode-common-hook)
  (run-hooks 'jr-mode-hook)
  (c-update-modeline))

(provide 'jr-mode)
