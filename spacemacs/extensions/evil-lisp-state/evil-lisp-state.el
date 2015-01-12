;;; evil-lisp-state.el --- An evil state to edit Lisp code

;; Copyright (C) 2014, 2015 syl20bnr
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: convenience editing evil smartparens lisp mnemonic
;; Created: 9 Oct 2014
;; Version: 5.1.0
;; Package-Requires: ((evil "1.0.9") (evil-leader "0.4.3") (smartparens "1.6.1"))
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

;; Principle:
;; ----------

;; To execute a command while in normal state, the evil-leader is used.
;; By default, the prefix for each command is `<leader> m`.
;; Each command when executed set the current state to `lisp state`.
;; By example, to slurp three times while in normal state:
;;     <leader> m s s s
;; Or to wrap a symbol in parenthesis then slurping two times:
;;     <leader> m w s s

;; Commands and key bindings:
;; --------------------------

;; Evil Lisp state binds the most common commands on hjkl:
;;
;; Key Binding     | Function
;; ----------------|------------------------------------------------------------
;; `<leader> m h`  | previous symbol
;; `<leader> m H`  | forward barf sexp (move the current symbol or sexp outside)
;; `<leader> m j`  | next closing parenthesis
;; `<leader> m J`  | wrap symbol with parenthesis (down one level)
;; `<leader> m k`  | previous opening parenthesis
;; `<leader> m K`  | unwrap current sexp (up one level)
;; `<leader> m l`  | next symbol
;; `<leader> m L`  | forward slurp sexp (move next outside sexp into current one)
;;
;; So with just hjkl keys you can:
;; - navigate between symbols and sexps
;; - slurp and barf symbols and sexps
;; - wrap and unwrap symbols and sexps

;; Slurping, barfing and wrapping are also bound on other keys.
;; All the other commands are:

;; Key Binding     | Function
;; ----------------|------------------------------------------------------------
;; `<leader> m (`  | insert expression before (same level as current one)
;; `<leader> m )`  | insert expression after (same level as current one)
;; `<leader> m a`  | absorb expression
;; `<leader> m b`  | forward barf expression
;; `<leader> m B`  | backward barf expression
;; `<leader> m c`  | convolute expression
;; `<leader> m i`  | switch to `insert state`
;; `<leader> m I`  | go to beginning of current expression and switch to `insert state`
;; `<leader> m m`  | merge (join) expression
;; `<leader> m n`  | forwared slurp expression
;; `<leader> m N`  | backward slurp expression
;; `<leader> m p`  | paste after
;; `<leader> m P`  | paste before
;; `<leader> m q`  | unwrap current expression and kill all symbols after point
;; `<leader> m Q`  | unwrap current expression and kill all symbols before point
;; `<leader> m r`  | raise expression (replace parent expression by current one)
;; `<leader> m T`  | transpose expression
;; `<leader> m u`  | undo
;; `<leader> m C-r`| redo
;; `<leader> m v`  | switch to `visual state`
;; `<leader> m V`  | switch to `visual line state`
;; `<leader> m C-v`| switch to `visual block state`
;; `<leader> m w`  | wrap expression with parenthesis
;; `<leader> m W`  | unwrap expression
;; `<leader> m xs` | delete symbol
;; `<leader> m xw` | delete word
;; `<leader> m xx` | delete expression
;; `<leader> m y`  | copy expression

;; Configuration:
;; --------------

;; Key bindings are set only for `emacs-lisp-mode' by default.
;; It is possible to add major modes with the variable
;; `evil-lisp-state-major-modes'.

;; The prefix key is `<leader> m' by default, it is possible to
;; change the `m' key to anything else with the variable
;; `evil-lisp-state-leader-prefix'. Set it to an empty string
;; if you want all the commands to be directly available
;; under the `<leader>' key.

;;; Code:

(require 'evil)
(require 'evil-leader)
(require 'smartparens)

(evil-define-state lisp
  "Lisp state.
 Used to navigate lisp code and manipulate the sexp tree."
  :tag " <L> "
  :suppress-keymap t
  :cursor (bar . 2)
  ;; force smartparens mode
  (if (evil-lisp-state-p) (smartparens-mode)))

(defgroup evil-lisp-state nil
  "Evil lisp state."
  :group 'emulations
  :prefix 'evil-lisp-state-)

(eval-and-compile
  (defcustom evil-lisp-state-leader-prefix "m"
    "Prefix key added to evil-lader, be default `m'"
    :type 'string
    :group 'evil-lisp-state)

  (defcustom evil-lisp-state-major-modes '(emacs-lisp-mode)
    "Major modes where evil leader key bindings are defined."
    :type 'sexp
    :group 'evil-lisp-state))

(defmacro evil-lisp-state-enter-command (command)
  "Wrap COMMAND to call evil-lisp-state before executing COMMAND."
  (let ((funcname (if (string-match "lisp-state-"
                                    (symbol-name command))
                      (intern (format "evil-%s" command))
                    (intern (format "evil-lisp-state-%s" command)))))
    `(progn
       (defun ,funcname ()
        (interactive)
        (evil-lisp-state)
        (call-interactively ',command))
       ',funcname)))

(defun evil-lisp-state-escape-command (command)
  "Wrap COMMAND to escape to normal state before executing COMMAND."
  `(lambda ()
     (interactive)
     (evil-normal-state)
     (call-interactively ',command)))

(define-key evil-lisp-state-map [escape] 'evil-normal-state)
(defconst evil-lisp-state-commands
  `(("("   . lisp-state-insert-sexp-before)
    (")"   . lisp-state-insert-sexp-after)
    ("1"   . digit-argument)
    ("2"   . digit-argument)
    ("3"   . digit-argument)
    ("4"   . digit-argument)
    ("5"   . digit-argument)
    ("6"   . digit-argument)
    ("7"   . digit-argument)
    ("8"   . digit-argument)
    ("9"   . digit-argument)
    ("a"   . sp-absorb-sexp)
    ("b"   . sp-forward-barf-sexp)
    ("B"   . sp-backward-barf-sexp)
    ("c"   . sp-convolute-sexp)
    ("h"   . sp-backward-symbol)
    ("H"   . sp-forward-barf-sexp)
    ("i"   . evil-insert-state)
    ("I"   . evil-insert-line)
    ("j"   . lisp-state-next-closing-paren)
    ("J"   . lisp-state-wrap)
    ("k"   . lisp-state-prev-opening-paren)
    ("K"   . sp-unwrap-sexp)
    ("l"   . lisp-state-forward-symbol)
    ("L"   . sp-forward-slurp-sexp)
    ("m"   . sp-join-sexp)
    ("n"   . sp-forward-slurp-sexp)
    ("N"   . sp-backward-slurp-sexp)
    ("p"   . evil-past-after)
    ("P"   . evil-past-before)
    ("q"   . sp-splice-sexp-killing-forward)
    ("Q"   . sp-splice-sexp-killing-backward)
    ("r"   . sp-raise-sexp)
    ("T"  . sp-transpose-sexp)
    ("u"   . undo-tree-undo)
    ("C-r" . undo-tree-redo)
    ("v"   . evil-visual-char)
    ("V"   . evil-visual-line)
    ("C-v" . evil-visual-block)
    ("w"   . lisp-state-wrap)
    ("W"   . sp-unwrap-sexp)
    ("xs"  . sp-kill-symbol)
    ("Xs"  . sp-backward-kill-symbol)
    ("xw"  . sp-kill-word)
    ("Xw"  . sp-backward-kill-word)
    ("xx"  . sp-kill-sexp)
    ("Xx"  . sp-backward-kill-sexp)
    ("y"   . sp-copy-sexp))
  "alist of keys and commands in lisp state.")
(dolist (x evil-lisp-state-commands)
  (let ((key (car x))
        (cmd (cdr x)))
    (eval
     `(progn
        (define-key evil-lisp-state-map ,(kbd key) ',cmd)
        (dolist (mm evil-lisp-state-major-modes)
          (evil-leader/set-key-for-mode mm
            ,(kbd (concat evil-lisp-state-leader-prefix key))
            (evil-lisp-state-enter-command ,cmd)))))))

(defun lisp-state-wrap (&optional arg)
  "Wrap a symbol with parenthesis."
  (interactive "P")
  (sp-wrap-with-pair "("))

(defun evil-lisp-state-next-paren (&optional closing)
  "Go to the next/previous closing/opening parenthesis."
  (if closing
      (let ((curr (point)))
        (forward-char)
        (unless (eq curr (search-forward ")"))
          (backward-char)))
    (search-backward "(")))

(defun lisp-state-prev-opening-paren ()
  "Go to the next closing parenthesis."
  (interactive)
  (evil-lisp-state-next-paren))

(defun lisp-state-next-closing-paren ()
  "Go to the next closing parenthesis."
  (interactive)
  (evil-lisp-state-next-paren 'closing))

(defun lisp-state-forward-symbol (&optional arg)
  "Go to the beginning of the next symbol."
  (interactive "P")
  (let ((n (if (char-equal (char-after) ?\() 1 2)))
    (sp-forward-symbol (+ (if arg arg 0) n))
    (sp-backward-symbol)))

(defun lisp-state-insert-sexp-after ()
  "Insert sexp after the current one."
  (interactive)
  (let ((sp-navigate-consider-symbols nil))
    (if (char-equal (char-after) ?\() (forward-char))
    (sp-up-sexp)
    (evil-insert-state)
    (sp-newline)
    (sp-insert-pair "(")))

(defun lisp-state-insert-sexp-before ()
  "Insert sexp before the current one."
  (interactive)
  (let ((sp-navigate-consider-symbols nil))
    (if (char-equal (char-after) ?\() (forward-char))
    (sp-backward-sexp)
    (evil-insert-state)
    (sp-newline)
    (evil-previous-visual-line)
    (evil-end-of-line)
    (insert " ")
    (sp-insert-pair "(")
    (indent-for-tab-command)))

(provide 'evil-lisp-state)

;;; evil-lisp-state.el ends here
