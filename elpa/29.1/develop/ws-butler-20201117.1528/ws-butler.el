;;; ws-butler.el --- Unobtrusively remove trailing whitespace.

;; this file is not part of Emacs

;; Copyright (C) 2013 Le Wang
;; Author: Le Wang
;; Maintainer: Le Wang
;; Description: unobtrusively remove trailing whitespace
;; Author: Le Wang
;; Maintainer: Le Wang

;; Created: Sat Jan  5 16:49:23 2013 (+0800)
;; Version: 0.6
;; Last-Updated:
;;           By:
;; URL: https://github.com/lewang/ws-butler
;; Keywords:
;; Compatibility: Emacs 24

;;; Installation:

;;
;; To enable for all ruby-mode buffers, add to .emacs.el:
;;
;;      (require 'ws-butler)
;;      (add-hook 'ruby-mode-hook 'ws-butler-mode)
;;

;;; Commentary:

;;
;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(eval-and-compile
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      ;; Can't use backquote here, it's too early in the bootstrap.
      (list 'set (list 'make-local-variable (list 'quote var)) val))))

(defgroup ws-butler nil
  "Unobtrusively whitespace deletion like a butler."
  :group 'convenience)

(defcustom ws-butler-keep-whitespace-before-point
  t
  "Keep whitespace at current point after save.

That is to say, if whitespace around is trimmed, perform the
cleanup only on disk, don't move the point in the buffer.

i.e. only the \"virtual\" space is preserved in the buffer."
  :type 'boolean
  :group 'ws-butler)

(defcustom ws-butler-convert-leading-tabs-or-spaces
  nil
  "Make leading whitespace be tabs or spaces

If `indent-tabs-mode' is non-nil, call `tabify', else call
`untabify'. Do neither if `smart-tabs-mode' is enabled for this
buffer. This is off by default because it's unwanted if you
occasionally edit files where leading whitespace should not be
changed in this specific way."

  :type 'boolean
  :group 'ws-butler)

(defcustom ws-butler-global-exempt-modes
  '(markdown-mode)
  "Don't enable ws-butler in modes that inherit from these.

This should be a list of trailing whitespace significant major-modes."
  :type '(repeat (symbol :tag "Major mode"))
  :group 'ws-butler)

(defcustom ws-butler-trim-predicate
  (lambda (_beg _end) t)
  "Return true for regions that should be trimmed.

Expects 2 arguments - beginning and end of a region.
Should return a truthy value for regions that should
have their trailing whitespace trimmed.
When not defined all regions are trimmed."
  :type 'function
  :group 'ws-butler)

(defvar ws-butler-saved)

(defmacro ws-butler-with-save (&rest forms)
  "Run FORMS with restriction and excursion saved once."
  (declare (debug (body)))
  `(if (and (boundp 'ws-butler-saved)
            ws-butler-saved)
       (progn
         ,@forms)
     (let ((ws-butler-saved t))
       (save-excursion
         (save-restriction
           ,@forms)))))

(defun ws-butler-trim-eob-lines ()
  "Trim lines at EOB in efficient manner.
Also see `require-final-newline'."
  (ws-butler-with-save
   (widen)
   ;; we need to clean up multiple blank lines at EOF to just one.  Or if
   ;; there is no blank line and there needs one, we add it.
   (goto-char (point-max))
   (skip-chars-backward " \t\n\v")
   (let ((saved-point (point)))
     (ws-butler-clean-region saved-point (point-max))
     (goto-char saved-point)
     ;; we try to make as few buffer modifications as possible
     ;;
     ;; We refuse to remove final-newline regardless of the value of
     ;; `require-final-newline'
     (when (looking-at "\n\\(\n\\|\\'\\)")
       (forward-char 1)))
   (when require-final-newline
     (unless (bolp)
       (insert "\n")))
   (when (looking-at "\n+")
     (replace-match ""))))

(defun ws-butler-maybe-trim-eob-lines (last-modified-pos)
  "Delete extra newlines at end of buffer if LAST-MODIFIED-POS is in the patch of excess newlines."
  (interactive (list nil))
  (unless buffer-read-only
    (unless last-modified-pos
      (ws-butler-map-changes
       (lambda (_prop beg end)
         (setq last-modified-pos end))))
    ;; trim EOF newlines if required
    (when last-modified-pos
      (ws-butler-with-save
       (widen)
       (goto-char (point-max))
       (skip-chars-backward " \t\n\v")
       (let ((printable-point-max (point)))
         (when (and (funcall ws-butler-trim-predicate printable-point-max (point-max))
                  (>= last-modified-pos printable-point-max))
           (ws-butler-trim-eob-lines))))))
  ;; clean return code for hooks
  nil)

(defun ws-butler-clean-region (beg end)
  "Delete trailing blanks in region BEG END.

If `indent-tabs-mode' is nil, then tabs in indentation are
replaced by spaces, and vice versa if t."
  (interactive "*r")
  (ws-butler-with-save
   (narrow-to-region beg end)
   ;;  _much slower would be:       (replace-regexp "[ \t]+$" "")
   (goto-char (point-min))
   (while (not (eobp))
     (when (and ws-butler-convert-leading-tabs-or-spaces
                (not (bound-and-true-p smart-tabs-mode)))
       ;; convert leading tabs to spaces or v.v.
       (let ((eol (point-at-eol)))
         (if indent-tabs-mode
             (progn
               (skip-chars-forward "\t" eol)
               (when (eq (char-after) ?\ )
                 (tabify (point) (progn (skip-chars-forward " \t" eol)
                                        (point)))))
           (skip-chars-forward " " eol)
           (when (eq (char-after) ?\t )
             (untabify (point) (progn (skip-chars-forward " \t" eol)
                                      (point)))))))
     (end-of-line)
     (delete-horizontal-space)
     (forward-line 1)))
  ;; clean return code for hooks
  nil)


(defvar ws-butler-presave-coord nil
  "Saved list of (LINE COLUMN) used to restore point after saving.

This is the key to the virtual spaces preserving indentation mechanism.")
(make-variable-buffer-local 'ws-butler-presave-coord)

(defun ws-butler-map-changes (func &optional start-position end-position)
  "Call FUNC with each changed region (START-POSITION END-POSITION).

This simply uses an end marker since we are modifying the buffer
in place."
  ;; See `hilit-chg-map-changes'.
  (let ((start (or start-position (point-min)))
        (limit (copy-marker (or end-position (point-max))))
        prop end)
    (while (and start (< start limit))
      (setq prop (get-text-property start 'ws-butler-chg))
      (setq end (text-property-not-all start limit 'ws-butler-chg prop))
      (if prop
          (funcall func prop start (or end limit)))
      (setq start end))
    (set-marker limit nil)))

(defun ws-butler-before-save ()
  "Trim white space before save.

Setting `ws-butler-keep-whitespace-before-point' will also
ensure point doesn't jump due to white space trimming."

  ;; save data to restore later
  (when ws-butler-keep-whitespace-before-point
    (ws-butler-with-save
     (widen)
     (setq ws-butler-presave-coord (list
                                    (line-number-at-pos (point))
                                    (current-column)))))
  (let (last-end)
    (ws-butler-map-changes
     (lambda (_prop beg end)
       (save-excursion
         (setq beg (progn (goto-char beg)
                          (point-at-bol))
               ;; Subtract one from end to overcome Emacs bug #17784, since we
               ;; always expand to end of line anyway, this should be OK.
               end (progn (goto-char (1- end))
                          (point-at-eol))))
       (when (funcall ws-butler-trim-predicate beg end)
         (ws-butler-clean-region beg end))
       (setq last-end end)))
    (ws-butler-maybe-trim-eob-lines last-end)))

(defun ws-butler-clear-properties ()
  "Clear all ws-butler text properties in buffer."
  (with-silent-modifications
    (ws-butler-map-changes (lambda (_prop start end)
                             (remove-list-of-text-properties start end '(ws-butler-chg))))))

(defun ws-butler-after-change (beg end length-before)
  (let ((type (if (and (= beg end) (> length-before 0))
                  'delete
                'chg)))
    (if undo-in-progress
        ;; add back deleted text during undo
        (if (and (zerop length-before)
               (> end beg)
               (eq (get-text-property end 'ws-butler-chg) 'delete))
            (remove-list-of-text-properties end (1+ end) '(ws-butler-chg)))
      (with-silent-modifications
        (when (eq type 'delete)
          (setq end (min (+ end 1) (point-max))))
        (put-text-property beg end 'ws-butler-chg type)))))

(defun ws-butler-after-save ()
  "Restore trimmed whitespace before point."

  (ws-butler-clear-properties)
  ;; go to saved line+col
  (when ws-butler-presave-coord
    (let (remaining-lines)
      (ws-butler-with-save
       (widen)
       (goto-char (point-min))
       (setq remaining-lines (forward-line (1- (car ws-butler-presave-coord)))))
      (unless (eq remaining-lines 0)
        (insert (make-string remaining-lines ?\n))))
    (move-to-column (cadr ws-butler-presave-coord) t)
    (set-buffer-modified-p nil)))

(defun ws-butler-before-revert ()
  "Clear `ws-butler-presave-coord'."
  (setq ws-butler-presave-coord nil))

;;;###autoload
(define-minor-mode ws-butler-mode
  "White space cleanup, without obtrusive white space removal.

Whitespaces at EOL and EOF are trimmed upon file save, and only
for lines modified by you."
  :lighter " wb"
  :group 'ws-butler
  (if ws-butler-mode
      (progn
        (add-hook 'after-change-functions 'ws-butler-after-change t t)
        (add-hook 'before-save-hook 'ws-butler-before-save t t)
        (add-hook 'after-save-hook 'ws-butler-after-save t t)
        (add-hook 'before-revert-hook 'ws-butler-before-revert t t)
        (add-hook 'after-revert-hook 'ws-butler-after-save t t)
        (add-hook 'edit-server-done-hook 'ws-butler-before-save t t))
    (remove-hook 'after-change-functions 'ws-butler-after-change t)
    (remove-hook 'before-save-hook 'ws-butler-before-save t)
    (remove-hook 'after-save-hook 'ws-butler-after-save t)
    (remove-hook 'before-revert-hook 'ws-butler-before-revert t)
    (remove-hook 'after-revert-hook 'ws-butler-after-save t)
    (remove-hook 'edit-server-done-hook 'ws-butler-before-save t)))

;;;###autoload
(define-globalized-minor-mode ws-butler-global-mode ws-butler-mode
  (lambda ()
    (unless (apply #'derived-mode-p ws-butler-global-exempt-modes)
      (ws-butler-mode))))

(provide 'ws-butler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ws-butler.el ends here
