;;; evil-types.el --- Type system -*- lexical-binding: t -*-

;; Author: Vegard Øye <vegard_oye at hotmail.com>
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>

;; Version: 1.15.0

;;
;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is part of Evil.
;;
;; Evil is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Evil is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Evil.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A type defines a transformation on a pair of buffer positions.
;; Types are used by Visual state (character/line/block selection)
;; and Operator-Pending state (character/line/block motions).
;;
;; The basic transformation is "expansion". For example, the `line'
;; type "expands" a pair of positions to whole lines by moving the
;; first position to the beginning of the line and the last position
;; to the end of the line. That expanded selection is what the rest
;; of Emacs sees and acts on.
;;
;; An optional transformation is "contraction", which is the opposite
;; of expansion. If the transformation is one-to-one, expansion
;; followed by contraction always returns the original range.
;; (The `line' type is not one-to-one, as it may expand multiple
;; positions to the same lines.)
;;
;; Another optional transformation is "normalization", which takes
;; two unexpanded positions and adjusts them before expansion.
;; This is useful for cleaning up "invalid" positions.
;;
;; Types are defined at the end of this file using the macro
;; `evil-define-type'.

(require 'evil-common)
(require 'evil-macros)

;;; Code:

;;; Type definitions

(evil-define-type exclusive
  "Return the positions unchanged, with some exceptions.
If the end position is at the beginning of a line, then:

* If the beginning position is at or before the first non-blank
  character on the line, return `line' (expanded).

* Otherwise, move the end position to the end of the previous
  line and return `inclusive' (expanded)."
  :normalize (lambda (beg end)
               (cond
                ((progn
                   (goto-char end)
                   (and (/= beg end) (bolp)))
                 (setq end (max beg (1- end)))
                 (cond
                  ((progn
                     (goto-char beg)
                     (looking-back "^[ \f\t\v]*" (line-beginning-position)))
                   (evil-expand beg end 'line))
                  (t
                   (unless evil-cross-lines
                     (setq end (max beg (1- end))))
                   (evil-expand beg end 'inclusive))))
                (t
                 (evil-range beg end))))
  :string (lambda (beg end)
            (let ((width (- end beg)))
              (format "%s character%s" width
                      (if (= width 1) "" "s")))))

(evil-define-type inclusive
  "Include the character under point.
Handling for `evil-want-visual-char-semi-exclusive' is deprecated,
and will be removed in a future version."
  :expand (lambda (beg end)
            (if (and evil-want-visual-char-semi-exclusive
                     (evil-visual-state-p)
                     (< beg end)
                     (save-excursion
                       (goto-char end)
                       (or (bolp) (eolp))))
                (evil-range beg end 'exclusive)
              (evil-range beg (1+ end))))
  :contract (lambda (beg end)
              (evil-range beg (max beg (1- end))))
  :normalize (lambda (beg end)
               (evil-range beg (if (eq (char-after end) ?\n)
                                   (max beg (1- end)) end)))
  :string (lambda (beg end)
            (let ((width (- end beg)))
              (format "%s character%s" width
                      (if (= width 1) "" "s")))))

(evil-define-type line
  "Include whole lines."
  :one-to-one nil
  :expand (lambda (beg end)
            (evil-range
             (progn
               (goto-char beg)
               ;; move to beginning of line as displayed
               (evil-move-beginning-of-line)
               (point))
             (progn
               (goto-char end)
               ;; move to the end of line as displayed
               (evil-move-end-of-line)
               (line-beginning-position 2))))
  :contract (lambda (beg end)
              (evil-range beg (max beg (1- end))))
  :string (lambda (beg end)
            (let ((height (count-lines beg end)))
              (format "%s line%s" height
                      (if (= height 1) "" "s")))))

(evil-define-type screen-line
  "Include whole lines, being aware of `visual-line-mode'
when `evil-respect-visual-line-mode' is non-nil."
  :one-to-one nil
  :expand (lambda (beg end)
            (if (or (not evil-respect-visual-line-mode)
                    (not visual-line-mode))
                (evil-line-expand beg end)
              (evil-range
               (progn
                 (goto-char beg)
                 (save-excursion
                   (beginning-of-visual-line)))
               (progn
                 (goto-char end)
                 (save-excursion
                   ;; `beginning-of-visual-line' reverts to the beginning of the
                   ;; last visual line if the end of the last line is the end of
                   ;; the buffer. This would prevent selecting the last screen
                   ;; line.
                   (if (= (line-beginning-position 2) (point-max))
                       (point-max)
                     (beginning-of-visual-line 2)))))))
  :contract (lambda (beg end)
              (evil-range beg (max beg (1- end))))
  :string (lambda (beg end)
            (let ((height (count-screen-lines beg end)))
              (format "%s screen line%s" height
                      (if (= height 1) "" "s")))))

(evil-define-type block
  "Like `inclusive', but for rectangles:
the last column is included."
  :expand (lambda (beg end &rest properties)
            (let ((beg-col (evil-column beg))
                  (end-col (evil-column end))
                  (corner (plist-get properties :corner)))
              ;; Since blocks are implemented as a pair of buffer
              ;; positions, expansion is restricted to what the buffer
              ;; allows. In the case of a one-column block, there are
              ;; two ways to expand it (either move the upper corner
              ;; beyond the lower corner, or the lower beyond the
              ;; upper), so try out both possibilities when
              ;; encountering the end of the line.
              (cond
               ((= beg-col end-col)
                (goto-char end)
                (cond
                 ((eolp)
                  (goto-char beg)
                  (if (eolp)
                      (evil-range beg end)
                    (evil-range (1+ beg) end)))
                 ((memq corner '(lower-right upper-right right))
                  (evil-range (1+ beg) end))
                 (t
                  (evil-range beg (1+ end)))))
               ((< beg-col end-col)
                (goto-char end)
                (if (eolp)
                    (evil-range beg end)
                  (evil-range beg (1+ end))))
               (t
                (goto-char beg)
                (if (eolp)
                    (evil-range beg end)
                  (evil-range (1+ beg) end))))))
  :contract (lambda (beg end)
              (let ((beg-col (evil-column beg))
                    (end-col (evil-column end)))
                (if (> beg-col end-col)
                    (evil-range (1- beg) end)
                  (evil-range beg (max beg (1- end))))))
  :string (lambda (beg end)
            (let ((height (count-lines
                           beg
                           (progn
                             (goto-char end)
                             (if (and (bolp) (not (eobp)))
                                 (1+ end)
                               end))))
                  (width (abs (- (evil-column beg)
                                 (evil-column end)))))
              (format "%s row%s and %s column%s"
                      height
                      (if (= height 1) "" "s")
                      width
                      (if (= width 1) "" "s"))))
  :rotate (lambda (beg end &rest properties)
            "Rotate block according to :corner property.
:corner can be one of `upper-left',``upper-right', `lower-left'
and `lower-right'."
            (let ((left  (evil-column beg))
                  (right (evil-column end))
                  (corner (or (plist-get properties :corner)
                              'upper-left)))
              (evil-sort left right)
              (goto-char beg)
              (if (memq corner '(upper-right lower-left))
                  (move-to-column right)
                (move-to-column left))
              (setq beg (point))
              (goto-char end)
              (if (memq corner '(upper-right lower-left))
                  (move-to-column left)
                (move-to-column right))
              (setq end (point))
              (setq properties (plist-put properties
                                          :corner corner))
              (apply #'evil-range beg end properties))))

(evil-define-type rectangle
  "Like `exclusive', but for rectangles:
the last column is excluded."
  :expand (lambda (beg end)
            ;; select at least one column
            (if (= (evil-column beg) (evil-column end))
                (evil-expand beg end 'block)
              (evil-range beg end 'block))))

;;; Standard interactive codes

(evil-define-interactive-code "*"
  "Signal error if the buffer is read-only."
  (when buffer-read-only
    (signal 'buffer-read-only nil)))

(evil-define-interactive-code "b" (prompt)
  "Name of existing buffer."
  (list (read-buffer prompt (current-buffer) t)))

(evil-define-interactive-code "c"
  "Read character."
  (list (read-char)))

(evil-define-interactive-code "p"
  "Prefix argument converted to number."
  (list (prefix-numeric-value current-prefix-arg)))

(evil-define-interactive-code "P"
  "Prefix argument in raw form."
  (list current-prefix-arg))

;;; Custom interactive codes

(evil-define-interactive-code "<c>"
  "Count."
  (list (when current-prefix-arg
          (prefix-numeric-value current-prefix-arg))))

(evil-define-interactive-code "<vc>"
  "Count, but only in visual state.
This should be used by an operator taking a count. In normal
state the count should not be handled by the operator but by the
motion that defines the operator's range. In visual state the
range is specified by the visual region and the count is not used
at all. Thus in the case the operator may use the count
directly."
  (list (when (and (evil-visual-state-p) current-prefix-arg)
          (prefix-numeric-value
           current-prefix-arg))))

(evil-define-interactive-code "<C>"
  "Character read through `evil-read-key'."
  (list
   (if (evil-operator-state-p)
       (evil-without-restriction (evil-read-key))
     (evil-read-key))))

(evil-define-interactive-code "<r>"
  "Untyped motion range (BEG END)."
  (evil-operator-range))

(evil-define-interactive-code "<R>"
  "Typed motion range (BEG END TYPE)."
  (evil-operator-range t))

(evil-define-interactive-code "<v>"
  "Typed motion range of visual range (BEG END TYPE).
If visual state is inactive then those values are nil."
  (if (evil-visual-state-p)
      (let ((range (evil-visual-range)))
        (list (car range)
              (cadr range)
              (evil-type range)))
    (list nil nil nil)))

(evil-define-interactive-code "<x>"
  "Current register."
  (list evil-this-register))

(evil-define-interactive-code "<y>"
  "Current yank-handler."
  (list (evil-yank-handler)))

(evil-define-interactive-code "<a>"
  "Ex argument."
  :ex-arg t
  (list (when evil-called-from-ex-p evil-ex-argument)))

(evil-define-interactive-code "<N>" ()
  "Prefix argument or ex-arg, converted to number"
  (list (cond
         (current-prefix-arg (prefix-numeric-value current-prefix-arg))
         (evil-ex-argument (string-to-number evil-ex-argument))
         (evil-called-from-ex-p nil)
         (t 1))))

(evil-define-interactive-code "<f>"
  "Ex file argument."
  :ex-arg file
  (list (when evil-called-from-ex-p (evil-ex-file-arg))))

(evil-define-interactive-code "<b>"
  "Ex buffer argument."
  :ex-arg buffer
  (list evil-ex-argument))

(evil-define-interactive-code "<sh>"
  "Ex shell command argument."
  :ex-arg shell
  (list evil-ex-argument))

(evil-define-interactive-code "<fsh>"
  "Ex file or shell command argument."
  :ex-arg file-or-shell
  (list evil-ex-argument))

(evil-define-interactive-code "<sym>"
  "Ex symbolic argument."
  :ex-arg sym
  (list (and evil-ex-argument (intern evil-ex-argument))))

(evil-define-interactive-code "<addr>"
  "Ex line number."
  (list
   (when evil-called-from-ex-p
     (let ((expr (evil-ex-parse (or evil-ex-argument ""))))
       (if (eq (car expr) 'evil-goto-line)
           (save-excursion (goto-char evil-ex-point)
                           (eval (cadr expr) t))
         (user-error "Invalid address"))))))

(evil-define-interactive-code "<!>"
  "Ex bang argument."
  :ex-bang t
  (list evil-ex-bang))

(evil-define-interactive-code "</>"
  "Ex delimited argument."
  (when evil-called-from-ex-p
    (evil-delimited-arguments (or evil-ex-argument ""))))

(evil-define-interactive-code "<g/>"
  "Ex global argument."
  (when evil-called-from-ex-p
    (evil-ex-parse-global (or evil-ex-argument ""))))

(evil-define-interactive-code "<s/>"
  "Ex substitution argument."
  :ex-arg substitution
  (when evil-called-from-ex-p
    (evil-ex-get-substitute-info (or evil-ex-argument "") t)))

(evil-define-interactive-code "<xc/>"
  "Ex register and count argument, both optional.
Can be used for commands such as :delete [REGISTER] [COUNT] where the
command can be called with either zero, one or two arguments. With one
argument, if it is numeric, it is treated as a COUNT, otherwise, as a
REGISTER."
  (when evil-called-from-ex-p
    (evil-ex-get-optional-register-and-count evil-ex-argument)))

(defun evil-ex-get-optional-register-and-count (string)
  "Parse STRING as an ex arg with both optional REGISTER and COUNT.
Return a list (REGISTER COUNT)."
  (let* ((split-args (split-string (or string "")))
         (arg-count (length split-args))
         (arg0 (car split-args))
         (arg1 (cadr split-args))
         (number-regex "^-?[1-9][0-9]*$")
         (register nil)
         (count nil))
    (cond
     ;; :command REGISTER or :command COUNT
     ((= arg-count 1)
      (if (string-match-p number-regex arg0)
          (setq count arg0)
        (setq register arg0)))
     ;; :command REGISTER COUNT
     ((eq arg-count 2)
      (setq register arg0
            count arg1))
     ;; more than 2 args aren't allowed
     ((> arg-count 2)
      (user-error "Invalid use")))

    ;; if register is given, check it's valid
    (when register
      (unless (= (length register) 1)
        (user-error "Invalid register"))
      (setq register (string-to-char register)))

    ;; if count is given, check it's valid
    (when count
      (unless (string-match-p number-regex count)
        (user-error "Invalid count"))
      (setq count (string-to-number count))
      (unless (> count 0)
        (user-error "Invalid count")))

    (list register count)))

(provide 'evil-types)

;;; evil-types.el ends here
