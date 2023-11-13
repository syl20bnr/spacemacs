;;; evil-nerd-commenter-operator.el --- evil operator for this program  -*- lexical-binding: t -*-

;; Author: Chen Bin <chenbin.sh@gmail.com>

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;; Provides operators for evil-mode.

;;; Code:

(require 'evil)
(require 'evil-nerd-commenter-sdk)

(defvar evilnc-c-style-comment-modes
  '(awk-mode
    c++-mode
    c-mode
    css-mode
    dart-mode
    ess-mode
    go-mode
    java-mode
    javascript-mode
    js-mode
    js2-mode
    perl-mode
    php-mode
    swift-mode
    web-mode)
  "Major modes using C comment syntax.")

(defvar evilnc-current-text-object nil
  "Internal variable to detect current object working on.")

(defvar evilnc-whole-line-text-objects
  '(evil-forward-paragraph
    evil-backward-paragraph)
  "Comment operator operates on wholes when dealing with these text objects.")

(defvar evilnc-temporary-goal-column 0
  "Value of`temporary-goal-column' specifying right edge of rectangle yank.")

(defadvice evil-visual-highlight-block (around evil-visual-highlight-block-hack activate)
  "Show overlay over inner comment text object."
  ad-do-it
  (when (eq this-command 'evilnc-inner-commenter)
    (dolist (overlay evil-visual-block-overlays)
      (let* ((b (overlay-start overlay))
             (e (save-excursion
                  (goto-char (overlay-end overlay))
                  (line-end-position))))
        (move-overlay overlay b e)))))

(defadvice evil-apply-on-block (around evil-apply-on-block-around-hack activate)
  "Yank correct region of inner comment text object."
  (let* ((tmp-command last-command))
    ;; force `evil-apply-on-block' use our temporary-goal-column
    (when (> evilnc-temporary-goal-column 0)
      (setq temporary-goal-column (max temporary-goal-column
                                       evilnc-temporary-goal-column))
      ;; Read `evil-apply-on-block'. Note `temporary-goal-column' is used
      ;; if and only if `last-command' is `next-line' or `previous-line'
      (setq last-command 'next-line))
    ad-do-it
    ;; restore last command
    (setq last-command tmp-command)
    (setq evilnc-temporary-goal-column 0)))

(defun evilnc-expand-to-whole-comment-or-line (start end)
  "Expand the comment region defined by START and END.
Make sure all comment is included.
Or expand the region to contain whole lines."

  (cond
   ((evilnc-pure-comment-p start)
    (save-excursion
      (let* ((newstart start)
             (newend end))

        ;; expand the beginning
        (goto-char newstart)
        (while (and (>= (1- newstart) (line-beginning-position)) (evilnc-pure-comment-p (1- newstart)))
          (setq newstart (1- newstart)))

        ;; expand the end
        (goto-char newend)
        (while (and (<= newend (line-end-position)) (evilnc-pure-comment-p newend))
          (setq newend (1+ newend)))

        (cons newstart newend))))

   ;; try to expand region to contain whole line if,
   ;; - currently more than one line text in the region,
   ;; - a specific text object is touched just before the operator
   ((and (not (evilnc-sdk-inside-one-line-p start end))
         evilnc-current-text-object
         (member (car evilnc-current-text-object) evilnc-whole-line-text-objects)
         ;; 0.5 second
         (< (- (float-time (current-time)) (cdr evilnc-current-text-object)) 0.5))
    (evilnc-sdk-expand-to-contain-whole-lines start end))
   (t
    (cons start end))))

;; {{ know text object type to operate on
(defun evilnc-set-current-text-object (text-object)
  "Set current TEXT-OBJECT."
  (setq evilnc-current-text-object
        (cons text-object (float-time (current-time)))))

(defadvice evil-select-an-object (before evilnc-evil-select-an-object-hack activate)
  "Figure out text object type."
  (let* ((thing (car (ad-get-args 0))))
    ;; record the thing and timestamp `evil-select-an-object' is called
    (evilnc-set-current-text-object thing)))

(defadvice evil-backward-paragraph (before evilnc-evil-backward-paragraph-hack activate)
  "Record current text object."
  (evilnc-set-current-text-object 'evil-backward-paragraph))

(defadvice evil-forward-paragraph (before evilnc-evil-forward-paragraph-hack activate)
  "Record current text object."
  (evilnc-set-current-text-object 'evil-forward-paragraph))
;; }}

(evil-define-operator evilnc-comment-operator (start end type)
  "Comments text from START to END with TYPE."
  (interactive "<R>")
  (cond
   ((eq type 'block)
    (let* ((newpos (evilnc-expand-to-whole-comment-or-line start end) ))
      (evil-apply-on-block #'evilnc-comment-or-uncomment-region
                           (car newpos)
                           (cdr newpos)
                           nil)))

   ((and (eq type 'line)
         (= end (point-max))
         (or (= start end)
             (/= (char-before end) ?\n))
         (/= start (point-min))
         (=  (char-before start) ?\n))
    (evilnc-comment-or-uncomment-region (1- start) end))

   ((eq type 'line)
    ;; comment whole line, for now
    (evilnc-comment-or-uncomment-region start
                                         (save-excursion
                                           (goto-char (1- end))
                                           (line-end-position))))

   (t
    (when (and start end)
      (let* ((newpos (evilnc-expand-to-whole-comment-or-line start end)))
        (evilnc-comment-or-uncomment-region (car newpos) (cdr newpos))))))

  ;; place cursor on beginning of line
  (if (and (called-interactively-p 'any) (eq type 'line))
      (evil-first-non-blank)))

(defun evilnc-comment-or-uncomment-region-then-action (start end commenter &optional action)
  "Comment/uncomment between START and END using COMMENTER, then take ACTION."
  (evil-with-single-undo
    ;; yank original text
    (evil-yank-lines start end nil 'lines)

    (when (evil-visual-state-p)
      ;; `evil-paste-before' does not work in visual state.
      (evil-normal-state))

    (cond
     (evilnc-original-above-comment-when-copy-and-comment
      (let* ((p (point)))
        (funcall commenter start end)
        (goto-char start)
        (when action (funcall action))
        (goto-char p)))

     (t
      (goto-char end)
      (when action (funcall action))
      ;; actual comment operation should happen at last
      ;; or else "(start end)" is screwed up
      (funcall commenter start end)))))

(evil-define-operator evilnc-yank-and-comment-operator (start end)
  "(Un)comment and yank the text from START to END."
  :move-point (not evilnc-original-above-comment-when-copy-and-comment)
  (interactive "<r>")
  (evilnc-comment-or-uncomment-region-then-action start
                                                  end
                                                  evilnc-comment-or-uncomment-region-function))

(evil-define-operator evilnc-copy-and-comment-operator (start end)
  "Inserts a commented copy of the text from START to END."
  :move-point (not evilnc-original-above-comment-when-copy-and-comment)
  (interactive "<r>")
  (evilnc-comment-or-uncomment-region-then-action start
                                                  end
                                                  'comment-region
                                                  (lambda () (evil-paste-before 1))))

(defun evilnc-one-line-comment-p (start end)
  "Test if text between START and END is one line comment."
  (save-excursion
    (goto-char start)
    (and (<= (line-beginning-position) start)
         ;; end is the upper limit great than (line-end-position)
         (<= end (1+ (line-end-position))))))

(defun evilnc-get-comment-bounds ()
  "Return bounds like (cons start end)."
  (let* ((b (point))
         (e (point))
         (col 0)
         rlt)
    ;; decrease start position
    (while (evilnc-comment-p (- b 1))
      (setq b (- b 1)))

    ;; increase end
    (while (evilnc-comment-p (+ e 1))
      (setq e (+ e 1)))

    ;; we could select extra spaces at the end of comment
    ;; so we need go back
    (let* ((str (save-excursion
                  (goto-char e)
                  (evilnc-sdk-cur-line e)))
           (empty-line-p (string-match "^[ \t]*$" str)))
      (when empty-line-p
        ;; empty line plus line feed
        (setq e (- e (length str) 1))))
    (cond
     ((>= b e)
      (setq rlt nil))
     ((evilnc-one-line-comment-p b e)
      ;; contract from start position
      (while (not (evilnc-pure-comment-p b))
        (setq b (+ b 1)))

      ;; contract from end position
      (while (not (evilnc-pure-comment-p e))
        (setq e (- e 1)))

      (if (< b e) (setq rlt (cons b (1+ e)))))
     (t
      ;; multi-line comment
      (setq rlt (cons b (1+ e)))))
    rlt))

(defun evilnc-adjusted-comment-end (b e)
  "Adjust comment end of region between B and E."
  (let* ((next-end-char (evilnc-get-char (- e 2)))
         (end-char (evilnc-get-char (- e 1))))
    ;; avoid selecting CR/LF at the end of comment
    (while (and (< b e)
                (memq (evilnc-get-char (- e 1)) '(10 13)))
      (setq e (- e 1)))

    ;; avoid selecting comment limiter
    (cond
     ((and (memq major-mode evilnc-c-style-comment-modes)
           (= end-char ?/)
           (= next-end-char ?*))
      ;; avoid selecting the ending comment limiter "*/"
      (setq e (- e 2))
      (while (and (> e b)
                  (= (evilnc-get-char (- e 1)) ?*))
        (setq e (- e 1))))
     (t
      ;; other languages we can safely use font face
      (while (and (> e b)
                  (evilnc-comment-delimiter-p (- e 1)))
        (setq e (- e 1)))))
    e))

(defun evilnc-c-style-comment-p (pos)
  "Is C style comment at POS?"
  (and (memq major-mode evilnc-c-style-comment-modes)
       (= (evilnc-get-char pos) ?/)
       (= (memq (evilnc-get-char (1+ pos)) '(?/ ?*)))))

(defun evilnc-comment-column-bounds (start end &optional c-style)
  "From START to END find column bounds of rectangle selection.
Return (cons col-min col-max) or nil.  If C-STYLE is t,
we are processing C like language."
  (let* ((col-min most-positive-fixnum)
         (col-max 0))
    (while (< start end)
      (when (and (not (evilnc-whitespace-p start))
                 (evilnc-pure-comment-p start)
                 (not (or (evilnc-comment-delimiter-p start)
                          (and c-style
                               (memq (evilnc-get-char start) '(?/ ?*))))))
        (let* ((col (evil-column start)))
          (if (< col col-min)
              (setq col-min col))
          (if (> col col-max)
              (setq col-max col))))
      (setq start (1+ start)))
    (if (< col-min col-max)
        (cons col-min col-max))))

(evil-define-text-object evilnc-inner-commenter (&optional count start end type)
  "An inner comment text object."
  (let* ((bounds (evilnc-get-comment-bounds))
         b
         e
         c-style)
    (cond
     (bounds
      (setq b (car bounds))
      (setq e (cdr bounds))
      (cond
       ((setq c-style (evilnc-c-style-comment-p b))
        (while (and (< b e)
                    (or (evilnc-whitespace-p b)
                        (evilnc-line-end-p b)
                        (memq (evilnc-get-char b) '(?/ ?*))))
          (setq b (1+ b)))
        (while (and (< b e)
                    (or (evilnc-whitespace-p e)
                        (evilnc-line-end-p e)
                        (memq (evilnc-get-char e) '(?/ ?*))))
          (setq e (1- e)))
        (setq e (1+ e))
        (setq b (save-excursion
                  (goto-char b)
                  (forward-word 1)
                  (forward-word -1)
                  (point))))
       (t
        (setq b (save-excursion
                  (goto-char (car bounds))
                  (forward-word 1)
                  (forward-word -1)
                  (point)))
        (setq e (save-excursion
                  (goto-char (cdr bounds))
                  (goto-char (evilnc-adjusted-comment-end b (line-end-position)))
                  (point)))))
      (cond
       ((evilnc-one-line-comment-p b e)
        ;; keep move e to the end of comment
        (evil-range b ;; (if c-style (1+ e) e)
                    e))
       (t
        ;; multi-line comment
        (let* ((col-b (evil-column b))
               (col-bounds (evilnc-comment-column-bounds b e c-style)))
          (cond
           (col-bounds
            (if (> col-b (car col-bounds))
                (setq b (- b (- col-b (car col-bounds)))))
            (setq evilnc-temporary-goal-column (cdr col-bounds)))
           (t
            (setq evilnc-temporary-goal-column (evil-column e)))))
        (evil-range b e 'block :expanded t))))
     (t
      (error "Not inside a comment")))))

(evil-define-text-object evilnc-outer-commenter (&optional count start end type)
  "An outer comment text object."
  (let* ((bounds (evilnc-get-comment-bounds)))
    (cond
     (bounds
      (evil-range (car bounds) (cdr bounds) 'exclusive :expanded t))
     (t
      (error "Not inside a comment")))))

(evil-define-text-object evilnc-inner-comment (&optional count start end type)
  "An inner comment text object."
  (evilnc-inner-commenter count start end type))

(evil-define-text-object evilnc-outer-comment (&optional count start end type)
  "An outer comment text object."
  (evilnc-outer-commenter count start end type))

(provide 'evil-nerd-commenter-operator)
;;; evil-nerd-commenter-operator.el ends here

;; Local Variables:
;; no-byte-compile: t
;; End:
