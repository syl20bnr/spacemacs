;;; evil-collection-calc.el --- Evil bindings for calc -*- lexical-binding: t -*-

;; Copyright (C) 2018 Pierre Neidhardt

;; Author: Pierre Neidhardt <mail@ambrevar.xyz>
;; Maintainer: James Nguyen <james@jojojames.com>, Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, calc, tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Evil bindings for calc.

;;; Code:
(require 'evil-collection)
(require 'calc)

(defconst evil-collection-calc-maps '(calc-mode-map))

;;;###autoload
(defun evil-collection-calc-setup ()
  "Set up `evil' bindings for `calc'."
  (evil-collection-set-readonly-bindings 'calc-mode-map)
  (evil-set-initial-state 'calc-mode 'normal)

  ;; Calc sets up its bindings just-in-time for its "extensions".  I don't think
  ;; it's worth copying this clumsy design (for what performance benefit?),
  ;; while making the bindings much harder to maintain.
  (require 'calc-ext)

  (evil-collection-define-key 'normal 'calc-mode-map
    "0" 'calcDigit-start
    "1" 'calcDigit-start
    "2" 'calcDigit-start
    "3" 'calcDigit-start
    "4" 'calcDigit-start
    "5" 'calcDigit-start
    "6" 'calcDigit-start
    "7" 'calcDigit-start
    "8" 'calcDigit-start
    "9" 'calcDigit-start

    (kbd "<tab>") 'calc-roll-down
    (kbd "S-<return>") 'calc-over
    (kbd "RET") 'calc-enter
    (kbd "SPC") 'calc-enter

    (kbd "C-x C-t") 'calc-transpose-lines
    (kbd "C-M-d") 'calc-pop-above
    (kbd "C-M-i") 'calc-roll-up
    (kbd "M-RET") 'calc-last-args
    (kbd "C-M-w") 'kill-ring-save
    (kbd "M-%") 'calc-percent
    (kbd "M-k") 'calc-copy-as-kill
    (kbd "M-w") 'calc-copy-region-as-kill
    (kbd "M-DEL") 'calc-pop-above
    (kbd "M-m t") 'calc-total-algebraic-mode
    (kbd "<delete>") 'calc-pop
    (kbd "<mouse-2>") 'calc-yank
    (kbd "DEL") 'calc-pop ; was "C-d"
    "d" 'calc-kill                      ; was "C-k"
    "u" 'calc-undo                      ; was "U"
    "X" 'calc-call-last-kbd-macro       ; "@" is already used.
    "pp" 'calc-yank                     ; was "C-y"
    "pP" 'calc-copy-to-buffer           ; was "y"

    (kbd "C-p") 'calc-precision         ; was "p"

    "?" 'calc-help
    ;; "h" 'calc-help-prefix ; TODO: Rebind?
    "i" 'calc-info

    "\"" 'calc-auto-algebraic-entry
    "$" 'calc-auto-algebraic-entry      ; TODO: No need for this one?
    "'" 'calc-algebraic-entry

    "!" 'calc-factorial
    "#" 'calcDigit-start
    "%" 'calc-mod
    "&" 'calc-inv
    "(" 'calc-begin-complex
    ")" 'calc-end-complex
    "*" 'calc-times
    "+" 'calc-plus
    "," 'calc-comma
    "-" 'calc-minus
    "." 'calcDigit-start
    "/" 'calc-divide
    ":" 'calc-fdiv
    ";" 'calc-semi         ; TODO: Shall we really override `evil-ex'?
    "<" 'calc-scroll-left
    "=" 'calc-evaluate
    ">" 'calc-scroll-right
    "@" 'calcDigit-start
    "A" 'calc-abs
    "B" 'calc-log
    "C" 'calc-cos
    ;; "D" 'calc-redo             ; TODO: What's the purpose of this?  Bind to C-r?
    "E" 'calc-exp
    "F" 'calc-floor
    "G" 'calc-argument
    "H" 'calc-hyperbolic
    "I" 'calc-inverse
    "J" 'calc-conj
    "K" 'calc-keep-args
    "L" 'calc-ln
    "M" 'calc-more-recursion-depth
    "N" 'calc-eval-num
    "O" 'calc-option
    "P" 'calc-pi
    "Q" 'calc-sqrt
    "R" 'calc-round
    "S" 'calc-sin
    "T" 'calc-tan
    "[[" 'calc-begin-vector
    "]]" 'calc-end-vector
    "\\" 'calc-idiv
    "^" 'calc-power
    "_" 'calcDigit-start
    "`" 'calc-edit
    "e" 'calcDigit-start
    "n" 'calc-change-sign
    "o" 'calc-realign
    "w" 'calc-why
    "x" 'calc-execute-extended-command
    "|" 'calc-concat
    "{" 'calc-scroll-down               ; TODO: Not necessary?
    "}" 'calc-scroll-up                 ; TODO: Not necessary?
    "~" 'calc-num-prefix

    "V" (lookup-key calc-mode-map (kbd "V"))
    "Y" (lookup-key calc-mode-map (kbd "Y"))
    "Z" (lookup-key calc-mode-map (kbd "Z"))
    "a" (lookup-key calc-mode-map (kbd "a"))
    "b" (lookup-key calc-mode-map (kbd "b"))
    "c" (lookup-key calc-mode-map (kbd "c"))
    "D" (lookup-key calc-mode-map (kbd "d"))
    "f" (lookup-key calc-mode-map (kbd "f"))
    "g" (lookup-key calc-mode-map (kbd "g"))
    "zj" (lookup-key calc-mode-map (kbd "j"))
    "zk" (lookup-key calc-mode-map (kbd "k"))
    "zl" (lookup-key calc-mode-map (kbd "l"))
    "m" (lookup-key calc-mode-map (kbd "m"))
    "r" (lookup-key calc-mode-map (kbd "r"))
    "s" (lookup-key calc-mode-map (kbd "s"))
    "t" (lookup-key calc-mode-map (kbd "t"))
    "U" (lookup-key calc-mode-map (kbd "u"))
    "v" (lookup-key calc-mode-map (kbd "v"))
    "zz" (lookup-key calc-mode-map (kbd "z"))

    ;; quit
    "q" 'calc-quit)

  (evil-collection-define-key 'visual 'calc-mode-map
    "d" 'calc-kill-region))

(provide 'evil-collection-calc)
;;; evil-collection-calc.el ends here
