;;; org-sudoku.el --- Create and solve SUDOKU games in Org tables

;; Copyright (C) 2012-2021 Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Keywords: outlines, hypermedia, calendar, wp, games
;; Homepage: https://git.sr.ht/~bzg/org-contrib
;; Version: 0.01
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This is a quick hack to create and solve SUDOKU games in org tables.
;;
;; Commands:
;;
;; org-sudoku-create         Create a new SUDOKU game
;; org-sudoku-solve-field    Solve the field at point in a SUDOKU game
;;                           (this is for cheeting when you are stuck)
;; org-sudoku-solve          Solve the entire game
;;

;;; Code

(require 'org)
(require 'org-table)

;;; Customization

(defvar org-sudoku-size 9
  "The size of the sudoku game, 9 for a 9x9 game and 4 for a 4x4 game.
Larger games do not seem to work because of limited resources - even though
the algorithm is general.")

(defvar org-sudoku-timeout 2.0
  "Timeout for finding a solution when creating a new game.
After this timeout, the program starts over from scratch to create
a game.")

;;; Interactive commands

(defun org-sudoku-create (nfilled)
  "Create a sudoku game."
  (interactive "nNumber of pre-filled fields: ")
  (let ((sizesq org-sudoku-size)
	game)
    (loop for i from 1 to org-sudoku-size do
	  (loop for j from 1 to org-sudoku-size do
		(push (list (cons i j) 0) game)))
    (setq game (nreverse game))
    (random t)
    (setq game (org-sudoku-build-allowed game))
    (setq game (org-sudoku-set-field game (cons 1 1)
				     (1+ (random org-sudoku-size))))
    (catch 'solved
      (let ((cnt 0))
	(while t
	  (catch 'abort
	    (message "Attempt %d to create a game" (setq cnt (1+ cnt)))
	    (setq game1 (org-sudoku-deep-copy game))
	    (setq game1 (org-sudoku-solve-game
			 game1 'random (+ (float-time) org-sudoku-timeout)))
	    (when game1
	      (setq game game1)
	      (throw 'solved t))))))
    (let ((sqrtsize (floor (sqrt org-sudoku-size))))
      (loop for i from 1 to org-sudoku-size do
	    (insert "| |\n")
	    (if (and (= (mod i sqrtsize) 0) (< i org-sudoku-size))
		(insert "|-\n")))
      (backward-char 5)
      (org-table-align))
    (while (> (length game) nfilled)
      (setq game (delete (nth (1+ (random (length game))) game) game)))
    (mapc (lambda (e)
	    (org-table-put (caar e) (cdar e) (int-to-string (nth 1 e))))
	  game)
    (org-table-align)
    (org-table-goto-line 1)
    (org-table-goto-column 1)
    (message "Enjoy!")))

(defun org-sudoku-solve ()
  "Solve the sudoku game in the table at point."
  (interactive)
  (unless (org-at-table-p)
    (error "not at a table"))
  (let (game)
    (setq game (org-sudoku-get-game))
    (setq game (org-sudoku-build-allowed game))
    (setq game (org-sudoku-solve-game game))
    ;; Insert the values
    (mapc (lambda (e)
	    (org-table-put (caar e) (cdar e) (int-to-string (nth 1 e))))
	  game)
    (org-table-align)))

(defun org-sudoku-solve-field ()
  "Just solve the field at point.
This works by solving the whole game, then inserting only the single field."
  (interactive)
  (unless (org-at-table-p)
    (error "Not at a table"))
  (org-table-check-inside-data-field)
  (let ((i (org-table-current-dline))
	(j (org-table-current-column))
	game)
    (setq game (org-sudoku-get-game))
    (setq game (org-sudoku-build-allowed game))
    (setq game (org-sudoku-solve-game game))
    (if game
	(progn
	  (org-table-put i j (number-to-string
			      (nth 1 (assoc (cons i j) game)))
			 'align)
	  (org-table-goto-line i)
	  (org-table-goto-column j))
      (error "No solution"))))

;;; Internal functions

(defun org-sudoku-get-game ()
  "Interpret table at point as sudoku game and read it.
A game structure is returned."
  (let (b e g i j game)

    (org-table-goto-line 1)
    (org-table-goto-column 1)
    (setq b (point))
    (org-table-goto-line org-sudoku-size)
    (org-table-goto-column org-sudoku-size)
    (setq e (point))
    (setq g (org-table-copy-region b e))
    (setq i 0 j 0)
    (mapc (lambda (c)
	    (setq i (1+ i) j 0)
	    (mapc
	     (lambda (v)
	       (setq j (1+ j))
	       (push (list (cons i j)
			   (string-to-number v))
		     game))
	     c))
	  g)
    (nreverse game)))

(defun org-sudoku-build-allowed (game)
  (let (i j v numbers)
    (loop for i from 1 to org-sudoku-size do
	  (push i numbers))
    (setq numbers (nreverse numbers))
    ;; add the lists of allowed values for each entry
    (setq game (mapcar
		(lambda (e)
		  (list (car e) (nth 1 e)
			(if (= (nth 1 e) 0)
			    (copy-sequence numbers)
			  nil)))
		game))
    ;; remove the known values from the list of allowed values
    (mapc
     (lambda (e)
       (setq i (caar e) j (cdar e) v (cadr e))
       (when (> v 0)
	 ;; We do have a value here
	 (mapc
	  (lambda (f)
	    (setq a (assoc f game))
	    (setf (nth 2 a) (delete v (nth 2 a))))
	  (cons (cons i j) (org-sudoku-rel-fields i j)))))
     game)
    game))

(defun org-sudoku-find-next-constrained-field (game)
  (setq game (mapcar (lambda (e) (if (nth 2 e) e nil)) game))
  (setq game (delq nil game))
  (let (va vb la lb)
    (setq game
	  (sort game (lambda (a b)
		       (setq va (nth 1 a) vb (nth 1 b)
			     la (length (nth 2 a)) lb (length (nth 2 b)))
		       (cond
			((and (= va 0) (> vb 0)) t)
			((and (> va 0) (= vb 0)) nil)
			((not (= (* va vb) 0)) nil)
			(t (< la lb))))))
    (if (or (not game) (> 0 (nth 1 (car game))))
	nil
      (caar game))))

(defun org-sudoku-solve-game (game &optional random stop-at)
  "Solve GAME.
If RANDOM is non-nit, select candidates randomly from a fields option.
If RANDOM is nil, always start with the first allowed value and try
solving from there.
STOP-AT can be a float time, the solver will abort at that time because
it is probably stuck."
  (let (e v v1 allowed next g)
    (when (and stop-at
	       (> (float-time) stop-at))
      (setq game nil)
      (throw 'abort nil))
    (while (setq next (org-sudoku-find-next-constrained-field game))
      (setq e (assoc next game)
	    v (nth 1 e)
	    allowed (nth 2 e))
      (catch 'solved
	(if (= (length allowed) 1)
	    (setq game (org-sudoku-set-field game next (car allowed)))
	  (while allowed
	    (setq g (org-sudoku-deep-copy game))
	    (if (not random)
		(setq v1 (car allowed))
	      (setq v1 (nth (random (length allowed)) allowed)))
	    (setq g (org-sudoku-set-field g next v1))
	    (setq g (org-sudoku-solve-game g random stop-at))
	    (when g
	      (setq game g)
	      (throw 'solved g)))
	  (setq game nil))))
    (if (or (not game)
	    (org-sudoku-unknown-field-p game))
	nil
      game)))

(defun org-sudoku-unknown-field-p (game)
  "Are there still unknown fields in the game?"
  (delq nil (mapcar (lambda (e) (if (> (nth 1 e) 0) nil t)) game)))

(defun org-sudoku-deep-copy (game)
  "Make a copy of the game so that manipulating the copy does not change the parent."
  (mapcar (lambda(e)
	    (list (car e) (nth 1 e) (copy-sequence (nth 2 e))))
	  game))

(defun org-sudoku-set-field (game field value)
  "Put VALUE into FIELD, and tell related fields that they cannot be VALUE."
  (let (i j)
    (setq i (car field) j (cdr field))
    (setq a (assoc field game))
    (setf (nth 1 a) value)
    (setf (nth 2 a) nil)

    ;; Remove value from all related fields
    (mapc
     (lambda (f)
       (setq a (assoc f game))
       (setf (nth 2 a) (delete value (nth 2 a))))
     (org-sudoku-rel-fields i j))
    game))

(defun org-sudoku-rel-fields (i j)
  "Compute the list of related fields for field (i j)."
  (let ((sqrtsize (floor (sqrt org-sudoku-size)))
	ll imin imax jmin jmax f)
    (setq f (cons i j))
    (loop for ii from 1 to org-sudoku-size do
	  (or (= ii i) (push (cons ii j) ll)))
    (loop for jj from 1 to org-sudoku-size do
	  (or (= jj j) (push (cons i jj) ll)))
    (setq imin (1+ (* sqrtsize (/ (1- i) sqrtsize)))
	  imax (+ imin sqrtsize -1))
    (setq jmin (1+ (* sqrtsize (/ (1- j) sqrtsize)))
	  jmax (+ jmin sqrtsize -1))
    (loop for ii from imin to imax do
	  (loop for jj from jmin to jmax do
		(setq ff (cons ii jj))
		(or (equal ff f)
		    (member ff ll)
		    (push ff ll))))
    ll))

;;; org-sudoku ends here
