;;; evil-lisp-state.el --- An evil state to navigate Lisp code and modify it with smartparens

;; Copyright (C) 2014 syl20bnr

;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: convenience editing evil smartparens lisp mnemonic
;; Created: 9 Oct 2014
;; Version: 1.0
;; Package-Requires: ((evil "1.0.9") (smartparens "1.6.1"))
;; URL: https://github.com/syl20bnr/evil-lisp-state

;; This file is not part of GNU Emacs.

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

;; Adds a new Evil state called --LISP-- (<L>) with mnemonics key bindings
;; to navigate Lisp code and edit the sexp tree.

;; Example of a configuration overriding the `L` key bindings of
;; evil `motion state`:

;; (require 'evil-lisp-state)
;; (define-key evil-normal-state-map "L" 'evil-lisp-state)

;; More information in the readme of the repository:
;; https://github.com/syl20bnr/evil-lisp-state

;;; Code:

(require 'evil)
(require 'smartparens)

(evil-define-state lisp
  "Lisp state.
 Used to navigate lisp code and manipulate the sexp tree."
  :tag " <L> "
  :cursor (bar . 2)
  ;; force smartparens mode
  (if (evil-lisp-state-p) (smartparens-mode)))

;; key bindings
(define-key evil-lisp-state-map "1"   'digit-argument)
(define-key evil-lisp-state-map "2"   'digit-argument)
(define-key evil-lisp-state-map "3"   'digit-argument)
(define-key evil-lisp-state-map "4"   'digit-argument)
(define-key evil-lisp-state-map "5"   'digit-argument)
(define-key evil-lisp-state-map "6"   'digit-argument)
(define-key evil-lisp-state-map "7"   'digit-argument)
(define-key evil-lisp-state-map "8"   'digit-argument)
(define-key evil-lisp-state-map "9"   'digit-argument)
(define-key evil-lisp-state-map "$"   'sp-end-of-sexp)
(define-key evil-lisp-state-map "0"   'sp-beginning-of-sexp)
(define-key evil-lisp-state-map "a"   'sp-absorb-sexp)
(define-key evil-lisp-state-map "bh"  'sp-backward-barf-sexp)
(define-key evil-lisp-state-map "bl"  'sp-forward-barf-sexp)
(define-key evil-lisp-state-map "c"   'sp-convolute-sexp)
(define-key evil-lisp-state-map "C"   'sp-comment)
(define-key evil-lisp-state-map "dd"  'sp-kill-hybrid-sexp)
(define-key evil-lisp-state-map "ds"  'sp-kill-symbol)
(define-key evil-lisp-state-map "dw"  'sp-kill-word)
(define-key evil-lisp-state-map "dx"  'sp-kill-sexp)
(define-key evil-lisp-state-map "e$"  'evil-lisp-state-eval-sexp-end-of-line)
(define-key evil-lisp-state-map "ef"  'eval-defun)
(define-key evil-lisp-state-map "el"  'eval-last-sexp)
(define-key evil-lisp-state-map "es"  'eval-sexp)
(define-key evil-lisp-state-map "h"   'sp-backward-sexp)
(define-key evil-lisp-state-map "H"   'evil-backward-char)
(define-key evil-lisp-state-map "i"   'evil-insert-state)
(define-key evil-lisp-state-map "j"   'sp-down-sexp)
(define-key evil-lisp-state-map "J"   'sp-backward-down-sexp)
(define-key evil-lisp-state-map "k"   'sp-up-sexp)
(define-key evil-lisp-state-map "K"   'sp-backward-up-sexp)
(define-key evil-lisp-state-map "l"   'sp-forward-sexp)
(define-key evil-lisp-state-map "L"   'evil-forward-char)
(define-key evil-lisp-state-map "m"   'sp-join-sexp)
(define-key evil-lisp-state-map "p"   'evil-paste-after)
(define-key evil-lisp-state-map "P"   'evil-paste-before)
(define-key evil-lisp-state-map "r"   'sp-raise-sexp)
(define-key evil-lisp-state-map "R"   'sp-rewrap-sexp)
(define-key evil-lisp-state-map (kbd "C-r") 'undo-tree-redo)
(define-key evil-lisp-state-map "ska" 'sp-splice-sexp-killing-around)
(define-key evil-lisp-state-map "skh" 'sp-splice-sexp-killing-backward)
(define-key evil-lisp-state-map "skl" 'sp-splice-sexp-killing-forward)
(define-key evil-lisp-state-map "sh"  'sp-backward-slurp-sexp)
(define-key evil-lisp-state-map "sl"  'sp-forward-slurp-sexp)
(define-key evil-lisp-state-map "sx"  'sp-splice-sexp)
(define-key evil-lisp-state-map "t"   'sp-transpose-sexp)
(define-key evil-lisp-state-map "T"   'sp-transpose-hybrid-sexp)
(define-key evil-lisp-state-map "u"   'undo-tree-undo)
(define-key evil-lisp-state-map "U"   'sp-unwrap-sexp)
(define-key evil-lisp-state-map "y"   'sp-copy-sexp)

(define-key evil-lisp-state-map (kbd "RET") 'sp-newline)
(define-key evil-lisp-state-map [escape]    'evil-normal-state)

(defun evil-lisp-state-eval-sexp-end-of-line ()
  "Evaluate the last sexp at the end of the current line."
  (interactive)
  (save-excursion
    (evil-end-of-line)
    (eval-last-sexp nil)))

(provide 'evil-lisp-state)
