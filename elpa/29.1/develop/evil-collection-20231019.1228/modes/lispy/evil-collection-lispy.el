;;; evil-collection-lispy.el --- Evil Bindings for Lispy -*- lexical-binding: t; -*-

;; Copyright (C) 2019 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, lispy, tools

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
;; Evil bindings for `lispy-mode'.

;;; Code:
(require 'lispy nil t)
(require 'evil-collection)

(defconst evil-collection-lispy-maps '(lispy-mode-map
                                       evil-collection-lispy-mode-map))

(defvar lispy-mode-map-base)
(defvar lispy-mode-map-special)
(defvar lispy-mode-map-lispy)
(defvar lispy-mode-map-paredit)
(defvar lispy-mode-map-parinfer)
(defvar lispy-mode-map-evilcp)
(defvar lispy-mode-map-c-digits)
(defvar lispy-mode-map-oleh)
(defvar lispy-mode-map)
(defvar lispy-outline)
(defvar lispy-outline-header)
(defvar hydra-curr-body-fn)

(declare-function lispy--out-backward "lispy")
(declare-function lispy--out-forward "lispy")
(declare-function hydra-default-pre "lispy")
(declare-function hydra-keyboard-quit "lispy")
(declare-function hydra--call-interactively-remap-maybe "lispy")
(declare-function lispy-knight-down "lispy")
(declare-function lispy-knight-up "lispy")
(declare-function lispy-beginning-of-defun "lispy")
(declare-function lispy-goto "lispy")
(declare-function lispy-goto-local "lispy")
(declare-function hydra-idle-message "lispy")
(declare-function hydra-set-transient-map "lispy")
(declare-function lispy-tab "lispy")
(declare-function lispy-shifttab "lispy")
(declare-function lispy-left-p "lispy")
(declare-function lispy-right-p "lispy")
(declare-function lispy-bolp "lispy")
(declare-function lispy-down "lispy")
(declare-function lispy-new-copy "lispy")
(declare-function lispy-delete "lispy")
(declare-function lispy-delete-backward "lispy")
(declare-function lispy-define-key "lispy")
(declare-function lispy-set-key-theme "lispy")

;; ------------------------------- LISPYVILLE ----------------------------------
;; Copied from `lispyville'.
;; -> `lispyville-preferred-lispy-state'
(defcustom evil-collection-lispy-preferred-lispy-state 'insert
  "The preferred evil state for insertion and using lispy.
This is used by any command that should enter special to determine the correct
state."
  :type '(choice
          (const :tag "Use insert state to get into special." insert)
          (const :tag "Use emacs state to get into special." emacs))
  :group 'evil-collection)

;; -> `lispyville-insert-at-beginning-of-list'
(evil-define-command evil-collection-lispy-insert-at-beginning-of-list (count)
  "Enter `evil-collection-lispy-preferred-lispy-state' at the start of the list.

With COUNT, move backward/out COUNT lists first. This is the
lispyville equivalent of `evil-cp-insert-at-beginning-of-form' except for lists
only."
  (interactive "<c>")
  (when (lispy--out-backward (or count 1))
    (forward-char)
    (evil-change-state evil-collection-lispy-preferred-lispy-state)))

;; -> `lispyville-insert-at-end-of-list'
(evil-define-command evil-collection-lispy-insert-at-end-of-list (count)
  "Enter `lispyville-preferred-state' at the end of the list.
With COUNT, move forward/out COUNT lists first. This is the lispyville
equivalent of `evil-cp-insert-at-end-of-form' except for lists only."
  (interactive "<c>")
  (when (evil-collection-lispy--out-forward (or count 1))
    (backward-char)
    (evil-change-state evil-collection-lispy-preferred-lispy-state)))

;; -> `lispyville--out-forward'
(defun evil-collection-lispy--out-forward (count)
  "Like `lispyville--out-forward' but don't return nil if move at least once.
COUNT is passed to `lispy--out-forward'."
  (let ((orig-pos (point)))
    (lispy--out-forward count)
    (not (= (point) orig-pos))))

;; ------------------------------- LISPYVILLE ----------------------------------

;; ------------------------------- HYDRA ---------------------------------------
(when (featurep 'hydra)
  ;; (defhydra g-knight (:color blue :hint nil :idle .3 :columns 3)
  ;;   "g knight"
  ;;   ("j" lispy-knight-down "Down")
  ;;   ("k" lispy-knight-up "Up")
  ;;   ("g" lispy-beginning-of-defun "Beginning")
  ;;   ("d" lispy-goto "Goto")
  ;;   ("l" lispy-goto-local "Goto Local"))

  ;; Macroexpanded from g-knight hydra.
  (progn
    (set
     (defvar g-knight/params nil "Params of g-knight.")
     '(nil nil :columns 3 :exit t :foreign-keys nil :hint nil :idle 0.3))
    (set
     (defvar g-knight/docstring nil "Docstring of g-knight.")
     "g knight")
    (set
     (defvar g-knight/heads nil "Heads for g-knight.")
     '(("j" lispy-knight-down "Down" :exit t)
       ("k" lispy-knight-up "Up" :exit t)
       ("g" lispy-beginning-of-defun "Beginning" :exit t)
       ("d" lispy-goto "Goto" :exit t)
       ("l" lispy-goto-local "Goto Local" :exit t)))
    (set
     (defvar g-knight/keymap nil "Keymap for g-knight.")
     '(keymap
       (108 . g-knight/lispy-goto-local-and-exit)
       (100 . g-knight/lispy-goto-and-exit)
       (103 . g-knight/lispy-beginning-of-defun-and-exit)
       (107 . g-knight/lispy-knight-up-and-exit)
       (106 . g-knight/lispy-knight-down-and-exit)
       (kp-subtract . hydra--negative-argument)
       (kp-9 . hydra--digit-argument)
       (kp-8 . hydra--digit-argument)
       (kp-7 . hydra--digit-argument)
       (kp-6 . hydra--digit-argument)
       (kp-5 . hydra--digit-argument)
       (kp-4 . hydra--digit-argument)
       (kp-3 . hydra--digit-argument)
       (kp-2 . hydra--digit-argument)
       (kp-1 . hydra--digit-argument)
       (kp-0 . hydra--digit-argument)
       (57 . hydra--digit-argument)
       (56 . hydra--digit-argument)
       (55 . hydra--digit-argument)
       (54 . hydra--digit-argument)
       (53 . hydra--digit-argument)
       (52 . hydra--digit-argument)
       (51 . hydra--digit-argument)
       (50 . hydra--digit-argument)
       (49 . hydra--digit-argument)
       (48 . hydra--digit-argument)
       (45 . hydra--negative-argument)
       (21 . hydra--universal-argument)))
    (set
     (defvar g-knight/hint nil "Dynamic hint for g-knight.")
     '(format
       #("g knight:
j: Down       k: Up         g: Beginning
d: Goto       l: Goto Local" 10 11
(face hydra-face-blue)
24 25
(face hydra-face-blue)
38 39
(face hydra-face-blue)
51 52
(face hydra-face-blue)
65 66
(face hydra-face-blue))))
    (defun g-knight/lispy-knight-down-and-exit nil "Call the head `lispy-knight-down' in the \"g-knight\" hydra.

The heads for the associated hydra are:

\"j\":    `lispy-knight-down',
\"k\":    `lispy-knight-up',
\"g\":    `lispy-beginning-of-defun',
\"d\":    `lispy-goto',
\"l\":    `lispy-goto-local'

The body can be accessed via `g-knight/body'."
           (interactive)
           (require 'hydra)
           (hydra-default-pre)
           (hydra-keyboard-quit)
           (setq hydra-curr-body-fn 'g-knight/body)
           (progn
             (setq this-command 'lispy-knight-down)
             (hydra--call-interactively-remap-maybe
              (function lispy-knight-down))))
    (defun g-knight/lispy-knight-up-and-exit nil "Call the head `lispy-knight-up' in the \"g-knight\" hydra.

The heads for the associated hydra are:

\"j\":    `lispy-knight-down',
\"k\":    `lispy-knight-up',
\"g\":    `lispy-beginning-of-defun',
\"d\":    `lispy-goto',
\"l\":    `lispy-goto-local'

The body can be accessed via `g-knight/body'."
           (interactive)
           (require 'hydra)
           (hydra-default-pre)
           (hydra-keyboard-quit)
           (setq hydra-curr-body-fn 'g-knight/body)
           (progn
             (setq this-command 'lispy-knight-up)
             (hydra--call-interactively-remap-maybe
              (function lispy-knight-up))))
    (defun g-knight/lispy-beginning-of-defun-and-exit nil "Call the head `lispy-beginning-of-defun' in the \"g-knight\" hydra.

The heads for the associated hydra are:

\"j\":    `lispy-knight-down',
\"k\":    `lispy-knight-up',
\"g\":    `lispy-beginning-of-defun',
\"d\":    `lispy-goto',
\"l\":    `lispy-goto-local'

The body can be accessed via `g-knight/body'."
           (interactive)
           (require 'hydra)
           (hydra-default-pre)
           (hydra-keyboard-quit)
           (setq hydra-curr-body-fn 'g-knight/body)
           (progn
             (setq this-command 'lispy-beginning-of-defun)
             (hydra--call-interactively-remap-maybe
              (function lispy-beginning-of-defun))))
    (defun g-knight/lispy-goto-and-exit nil "Call the head `lispy-goto' in the \"g-knight\" hydra.

The heads for the associated hydra are:

\"j\":    `lispy-knight-down',
\"k\":    `lispy-knight-up',
\"g\":    `lispy-beginning-of-defun',
\"d\":    `lispy-goto',
\"l\":    `lispy-goto-local'

The body can be accessed via `g-knight/body'."
           (interactive)
           (require 'hydra)
           (hydra-default-pre)
           (hydra-keyboard-quit)
           (setq hydra-curr-body-fn 'g-knight/body)
           (progn
             (setq this-command 'lispy-goto)
             (hydra--call-interactively-remap-maybe
              (function lispy-goto))))
    (defun g-knight/lispy-goto-local-and-exit nil "Call the head `lispy-goto-local' in the \"g-knight\" hydra.

The heads for the associated hydra are:

\"j\":    `lispy-knight-down',
\"k\":    `lispy-knight-up',
\"g\":    `lispy-beginning-of-defun',
\"d\":    `lispy-goto',
\"l\":    `lispy-goto-local'

The body can be accessed via `g-knight/body'."
           (interactive)
           (require 'hydra)
           (hydra-default-pre)
           (hydra-keyboard-quit)
           (setq hydra-curr-body-fn 'g-knight/body)
           (progn
             (setq this-command 'lispy-goto-local)
             (hydra--call-interactively-remap-maybe
              (function lispy-goto-local))))
    (defun g-knight/body nil "Call the body in the \"g-knight\" hydra.

The heads for the associated hydra are:

\"j\":    `lispy-knight-down',
\"k\":    `lispy-knight-up',
\"g\":    `lispy-beginning-of-defun',
\"d\":    `lispy-goto',
\"l\":    `lispy-goto-local'

The body can be accessed via `g-knight/body'."
           (interactive)
           (require 'hydra)
           (hydra-default-pre)
           (let
               ((hydra--ignore nil))
             ;; Unfortunate, but this ignore has to be manually placed here
             ;; after macroexpanding.
             (ignore hydra--ignore)
             (hydra-keyboard-quit)
             (setq hydra-curr-body-fn 'g-knight/body))
           (hydra-idle-message 0.3 g-knight/hint 'g-knight)
           (hydra-set-transient-map g-knight/keymap
                                    (lambda nil
                                      (hydra-keyboard-quit)
                                      nil)
                                    nil)
           (setq prefix-arg current-prefix-arg)))

  ;; (defhydra lispy-tab-hydra (:color blue :hint nil :idle .3)
  ;;   "Tab"
  ;;   ("i" lispy-tab "Tab")
  ;;   ("s" lispy-shifttab "Shifttab"))

  ;; Macroexpanded from lispy-tab-hydra.
  (progn
    (set
     (defvar lispy-tab-hydra/params nil "Params of lispy-tab-hydra.")
     '(nil nil :exit t :foreign-keys nil :hint nil :idle 0.3))
    (set
     (defvar lispy-tab-hydra/docstring nil "Docstring of lispy-tab-hydra.")
     "Tab")
    (set
     (defvar lispy-tab-hydra/heads nil "Heads for lispy-tab-hydra.")
     '(("i" lispy-tab "Tab" :exit t)
       ("s" lispy-shifttab "Shifttab" :exit t)))
    (set
     (defvar lispy-tab-hydra/keymap nil "Keymap for lispy-tab-hydra.")
     '(keymap
       (115 . lispy-tab-hydra/lispy-shifttab-and-exit)
       (105 . lispy-tab-hydra/lispy-tab-and-exit)
       (kp-subtract . hydra--negative-argument)
       (kp-9 . hydra--digit-argument)
       (kp-8 . hydra--digit-argument)
       (kp-7 . hydra--digit-argument)
       (kp-6 . hydra--digit-argument)
       (kp-5 . hydra--digit-argument)
       (kp-4 . hydra--digit-argument)
       (kp-3 . hydra--digit-argument)
       (kp-2 . hydra--digit-argument)
       (kp-1 . hydra--digit-argument)
       (kp-0 . hydra--digit-argument)
       (57 . hydra--digit-argument)
       (56 . hydra--digit-argument)
       (55 . hydra--digit-argument)
       (54 . hydra--digit-argument)
       (53 . hydra--digit-argument)
       (52 . hydra--digit-argument)
       (51 . hydra--digit-argument)
       (50 . hydra--digit-argument)
       (49 . hydra--digit-argument)
       (48 . hydra--digit-argument)
       (45 . hydra--negative-argument)
       (21 . hydra--universal-argument)))
    (set
     (defvar lispy-tab-hydra/hint nil "Dynamic hint for lispy-tab-hydra.")
     '(format
       #("Tab: [i]: Tab, [s]: Shifttab." 6 7
         (face hydra-face-blue)
         16 17
         (face hydra-face-blue))))
    (defun lispy-tab-hydra/lispy-tab-and-exit nil "Call the head `lispy-tab' in the \"lispy-tab-hydra\" hydra.

The heads for the associated hydra are:

\"i\":    `lispy-tab',
\"s\":    `lispy-shifttab'

The body can be accessed via `lispy-tab-hydra/body'."
           (interactive)
           (require 'hydra)
           (hydra-default-pre)
           (hydra-keyboard-quit)
           (setq hydra-curr-body-fn 'lispy-tab-hydra/body)
           (progn
             (setq this-command 'lispy-tab)
             (hydra--call-interactively-remap-maybe
              (function lispy-tab))))
    (defun lispy-tab-hydra/lispy-shifttab-and-exit nil "Call the head `lispy-shifttab' in the \"lispy-tab-hydra\" hydra.

The heads for the associated hydra are:

\"i\":    `lispy-tab',
\"s\":    `lispy-shifttab'

The body can be accessed via `lispy-tab-hydra/body'."
           (interactive)
           (require 'hydra)
           (hydra-default-pre)
           (hydra-keyboard-quit)
           (setq hydra-curr-body-fn 'lispy-tab-hydra/body)
           (progn
             (setq this-command 'lispy-shifttab)
             (hydra--call-interactively-remap-maybe
              (function lispy-shifttab))))
    (defun lispy-tab-hydra/body nil "Call the body in the \"lispy-tab-hydra\" hydra.

The heads for the associated hydra are:

\"i\":    `lispy-tab',
\"s\":    `lispy-shifttab'

The body can be accessed via `lispy-tab-hydra/body'."
           (interactive)
           (require 'hydra)
           (hydra-default-pre)
           (let
               ((hydra--ignore nil))
             ;; Unfortunate, but this ignore has to be manually placed here
             ;; after macroexpanding.
             (ignore hydra--ignore)
             (hydra-keyboard-quit)
             (setq hydra-curr-body-fn 'lispy-tab-hydra/body))
           (hydra-idle-message 0.3 lispy-tab-hydra/hint 'lispy-tab-hydra)
           (hydra-set-transient-map lispy-tab-hydra/keymap
                                    (lambda nil
                                      (hydra-keyboard-quit)
                                      nil)
                                    nil)
           (setq prefix-arg current-prefix-arg))))

;; ------------------------------- HYDRA ---------------------------------------

(defun evil-collection-lispy-action-then-next-sexp (lispy-action)
  "Return function that triggers LISPY-ACTION and then moves to next sexp."
  (defalias (intern (format "%S-then-next-sexp" lispy-action))
    (lambda ()
      (interactive)
      (call-interactively lispy-action)
      (unless (or (lispy-left-p)
                  (lispy-right-p)
                  (and (lispy-bolp)
                       (or (looking-at lispy-outline-header)
                           (looking-at lispy-outline))))
        (call-interactively #'lispy-down)))))

(defun evil-collection-lispy-delete (arg)
  "Copy and delete current sexp.
Passes ARG to `lispy-delete' or `lispy-delete-back'.

Copy of `noc:lispy-delete'."
  (interactive "p")
  (cond ((or (lispy-left-p)
             (region-active-p))
         (lispy-new-copy)
         (lispy-delete arg))
        ((lispy-right-p)
         (lispy-new-copy)
         (lispy-delete-backward arg))))

(defvar evil-collection-lispy-mode-map-special
  (let ((map (make-sparse-keymap)))
    (lispy-define-key map
        ";" (evil-collection-lispy-action-then-next-sexp
             'lispy-comment))

    ;; navigation
    (lispy-define-key map "l" 'lispy-right)
    (lispy-define-key map "h" 'lispy-left)
    (lispy-define-key map "f" 'lispy-ace-paren
      :override '(cond ((bound-and-true-p view-mode)
                        (View-quit)))) ;; `lispy-flow' -> w
    (lispy-define-key map "j" 'lispy-down)
    (lispy-define-key map "k" 'lispy-up)

    (lispy-define-key map
        "d" (evil-collection-lispy-action-then-next-sexp
             'evil-collection-lispy-delete)) ;; `lispy-different' -> o

    (lispy-define-key map "o" 'lispy-different) ;; `lispy-other-mode' -> Q
    (lispy-define-key map "p" 'lispy-paste) ;; `lispy-eval-other-window' -> P
    (lispy-define-key map "P" 'lispy-eval-other-window) ;; `lispy-paste' -> p
    (lispy-define-key map "y" 'lispy-new-copy)          ;; `lispy-occur' -> /
    (lispy-define-key map "z" 'lispy-view) ;; `lispy-mark-list' -> v

    ;; outline
    (lispy-define-key map "J" 'lispy-join)     ;; `lispy-outline-next'
    (lispy-define-key map "K" 'lispy-describe) ;; `lispy-outline-prev'
    (lispy-define-key map "L" 'lispy-outline-goto-child)

    ;; Paredit transformations
    (lispy-define-key map ">" 'lispy-slurp-or-barf-right) ;; `lispy-slurp'
    (lispy-define-key map "<" 'lispy-slurp-or-barf-left)  ;; `lispy-barf'

    ;; FIXME: This doesn't work for me for some reason...
    (lispy-define-key map "/" 'lispy-occur) ;; `lispy-x' -> q

    (lispy-define-key map "r" 'lispy-raise)
    (lispy-define-key map "R" 'lispy-raise-some)

    (lispy-define-key map "+" 'lispy-join) ;; Hmnn this can be something else.

    ;; more transformations
    (lispy-define-key map "C" 'lispy-convolute)
    (lispy-define-key map "X" 'lispy-convolute-left)
    (lispy-define-key map "w" 'lispy-move-up)
    (lispy-define-key map "s" 'lispy-move-down)
    (lispy-define-key map "O" 'lispy-oneline)
    (lispy-define-key map "M" 'lispy-alt-multiline)
    (lispy-define-key map "S" 'lispy-stringify)

    ;; marking
    (lispy-define-key map "a" 'lispy-ace-symbol
      :override '(cond ((looking-at lispy-outline)
                        (lispy-meta-return))))
    (lispy-define-key map "H" 'lispy-ace-symbol-replace)

    ;; dialect-specific
    (lispy-define-key map "e" 'lispy-eval)
    (lispy-define-key map "E" 'lispy-eval-and-insert)

    ;; Hmnn, not sure why there's no `lispy-end-of-defun'.
    ;; `end-of-defun' doesn't work quite right yet. It exits the list
    ;; which would exit lispy state.
    (lispy-define-key map "G" 'end-of-defun) ;; `lispy-goto-local' -> gl
    (lispy-define-key map "g" 'g-knight/body) ;; `lispy-goto' -> gd

    (lispy-define-key map "A" 'evil-collection-lispy-insert-at-end-of-list) ;; `lispy-beginning-of-defun' -> gg
    (lispy-define-key map "I" 'evil-collection-lispy-insert-at-beginning-of-list) ;; `lispy-shifttab' -> zs

    (lispy-define-key map "F" 'lispy-follow t)
    (lispy-define-key map "D" 'pop-tag-mark)
    (lispy-define-key map "_" 'lispy-underscore)

    ;; miscellanea
    (define-key map (kbd "SPC") 'lispy-space)
    (lispy-define-key map "TAB" 'lispy-tab-hydra/body) ;; `lh-knight/body'  -> g

    (lispy-define-key map "N" 'lispy-narrow)
    (lispy-define-key map "W" 'lispy-widen)
    (lispy-define-key map "c" 'lispy-clone)
    (lispy-define-key map "u" 'lispy-undo)

    (lispy-define-key map "q" 'lispy-x)          ;; `lispy-ace-paren' -> f
    (lispy-define-key map "Q" 'lispy-other-mode) ;; `lispy-ace-char' -> t
    (lispy-define-key map "v" 'lispy-mark-list)  ;; `lispy-view' -> z
    (lispy-define-key map "t" 'lispy-ace-char)   ;; `lispy-teleport' -> T
    (lispy-define-key map "n" 'lispy-flow)       ;; `lispy-new-copy' -> y
    (lispy-define-key map "b" 'lispy-back)

    (lispy-define-key map "B" 'lispy-ediff-regions)
    (lispy-define-key map "x" 'lispy-splice) ;; `lispy-x' -> q

    (lispy-define-key map "Z" 'lispy-edebug-stop)
    (lispy-define-key map "V" 'lispy-visit)
    (lispy-define-key map "-" 'lispy-ace-subword)
    (lispy-define-key map "." 'lispy-repeat)
    (lispy-define-key map "~" 'lispy-tilde)
    ;; digit argument
    (mapc (lambda (x) (lispy-define-key map (format "%d" x) 'digit-argument))
          (number-sequence 0 9))

    ;; additional
    (lispy-define-key map "T" 'lispy-teleport
      :override '(cond ((looking-at lispy-outline)
                        (end-of-line))))
    (lispy-define-key map "]" 'lispy-forward)
    (lispy-define-key map "[" 'lispy-backward)
    (lispy-define-key map "{" 'lispy-brackets)

    ;; Experimental
    (lispy-define-key map "C-J" 'lispy-outline-next)
    (lispy-define-key map "C-K" 'lispy-outline-prev)
    (lispy-define-key map "^" 'lispy-splice-sexp-killing-backward)
    (lispy-define-key map "$" 'lispy-splice-sexp-killing-forward)
    (lispy-define-key map "M-j" 'lispy-move-down) ;; `lispy-split'
    (lispy-define-key map "M-k" 'lispy-move-up) ;; `lispy-kill-sentence'
    map)
  "`evil' flavored `lispy' bindings when in special state.")

(defvar evil-collection-lispy-mode-map (make-sparse-keymap)
  "`evil' flavored `lispy-mode' bindings.")

(defun evil-collection-lispy-set-key-theme (theme)
  "Set `lispy-mode-map' for according to THEME.
THEME is a list of choices: \='special', \='lispy', \='paredit', \='evilcp',
 \='c-digits', \='special-evil', \='evil'.

This is an exact copy of `lispy-set-key-theme' except with the additions of
 \='special-evil' and \='evil' themes."
  (setq lispy-mode-map
        (make-composed-keymap
         (delq nil
               (list
                (when (memq 'special-evil theme) evil-collection-lispy-mode-map-special)
                (when (memq 'evil theme) evil-collection-lispy-mode-map)
                (when (memq 'special theme) lispy-mode-map-special)
                (when (memq 'lispy theme) lispy-mode-map-lispy)
                (when (memq 'paredit theme) lispy-mode-map-paredit)
                (when (memq 'parinfer theme) lispy-mode-map-parinfer)
                (when (memq 'evilcp theme) lispy-mode-map-evilcp)
                (when (memq 'c-digits theme) lispy-mode-map-c-digits)
                (when (memq 'oleh theme) lispy-mode-map-oleh)))))
  (setcdr
   (assq 'lispy-mode minor-mode-map-alist)
   lispy-mode-map))

;;;###autoload
(defun evil-collection-lispy-setup ()
  "Set up `evil' bindings for `lispy'."
  (advice-add 'lispy-set-key-theme :override 'evil-collection-lispy-set-key-theme)
  (lispy-set-key-theme '(special-evil evil))
  (evil-collection-define-key 'normal 'evil-collection-lispy-mode-map
    ;; Instead of inheriting, explicitly define keys from `lispy-mode-map-base'.
    ;; navigation.
    ;; Commented out define-keys are from the original map that we're not using.
    ;; -- Begin Inheritance
    ;; (define-key map (kbd "C-a") 'lispy-move-beginning-of-line)
    ;; (define-key map (kbd "C-e") 'lispy-move-end-of-line)
    (kbd "M-n") 'lispy-left
    ;; killing
    ;; (define-key map (kbd "C-k") 'lispy-kill)
    (kbd "M-d") 'lispy-kill-word
    (kbd "M-DEL") 'lispy-backward-kill-word
    ;; misc
    "(" 'lispy-parens
    ";" 'lispy-comment
    ;; (kbd "M-q") 'lispy-fill
    ;; (define-key map (kbd "C-j") 'lispy-newline-and-indent)
    ;; (define-key map (kbd "RET") 'lispy-newline-and-indent-plain)
    ;; tags
    "gd" 'lispy-goto-symbol
    (kbd "C-t") 'pop-tag-mark
    ;; -- End Inheritance.

    (kbd "M-)") 'lispy-wrap-round
    (kbd "M-s") 'lispy-splice
    (kbd "M-<up>") 'lispy-splice-sexp-killing-backward
    (kbd "M-<down>") 'lispy-splice-sexp-killing-forward
    (kbd "M-r") 'lispy-raise-sexp
    (kbd "M-R") 'lispy-raise-some
    (kbd "M-C") 'lispy-convolute-sexp
    (kbd "M-S") 'lispy-split
    (kbd "M-J") 'lispy-join
    "]" 'lispy-forward
    "[" 'lispy-backward
    (kbd "M-(") 'lispy-wrap-round
    (kbd "M-{") 'lispy-wrap-braces
    (kbd "M-}") 'lispy-wrap-braces
    "<" 'lispy-slurp-or-barf-left
    ">" 'lispy-slurp-or-barf-right
    (kbd "M-y") 'lispy-new-copy
    (kbd "<C-return>") 'lispy-open-line
    (kbd "<M-return>") 'lispy-meta-return
    (kbd "M-k") 'lispy-move-up
    (kbd "M-j") 'lispy-move-down
    (kbd "M-o") 'lispy-string-oneline
    (kbd "M-p") 'lispy-clone
    (kbd "M-d") 'evil-collection-lispy-delete))

(provide 'evil-collection-lispy)
;;; evil-collection-lispy.el ends here
