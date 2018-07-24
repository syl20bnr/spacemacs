;;; hide-comnt.el --- Hide/show comments in code.
;;
;; Filename: hide-comnt.el
;; Description: Hide/show comments in code.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2011-2017, Drew Adams, all rights reserved.
;; Created: Wed May 11 07:11:30 2011 (-0700)
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Thu Feb 23 07:40:11 2017 (-0800)
;;           By: dradams
;;     Update #: 228
;; URL: https://www.emacswiki.org/emacs/download/hide-comnt.el
;; Doc URL: http://www.emacswiki.org/HideOrIgnoreComments
;; Keywords: comment, hide, show
;; Compatibility: GNU Emacs: 20.x, 21.x, 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   None
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Hide/show comments in code.
;;
;;  Comments are hidden by giving them and `invisible' property of
;;  value `hide-comment'.
;;
;;
;;  Macros defined here:
;;
;;    `with-comments-hidden'.
;;
;;  Commands defined here:
;;
;;    `hide/show-comments', `hide/show-comments-toggle'.
;;
;;  User options defined here:
;;
;;    `hide-whitespace-before-comment-flag', `ignore-comments-flag',
;;    `show-invisible-comments-shows-all'.
;;
;;  Non-interactive functions defined here:
;;
;;    `hide/show-comments-1'.
;;
;;
;;  Put this in your init file (`~/.emacs'):
;;
;;   (require 'hide-comnt)
;;
;;
;;  Note for Emacs 20: The commands and option defined here DO NOTHING
;;  IN EMACS 20.  Nevertheless, the library can be byte-compiled in
;;  Emacs 20 and `hide-comnt.elc' can be loaded in later Emacs
;;  versions and used there.  This is the only real use of this
;;  library for Emacs 20: it provides macro `with-comments-hidden'.
;;
;;  Note for Emacs 21: It lacks the `comment-forward' function, so we
;;  rely on the `comment-end' variable to determine the end of a
;;  comment. This means that only one type of comment terminator is
;;  supported.  For example, `c++-mode' sets `comment-end' to "",
;;  which is the convention for single-line comments ("// COMMENT").
;;  So "/* */" comments are treated as single-line commentsonly the
;;  first line of such comments is hidden.  The "*/" terminator is not
;;  taken into account.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2017/01/16 dadams
;;     hide/show-comments-1: ((add-to|remove-from)-invisibility-spec 'hide-comment).
;;     See https://github.com/syl20bnr/spacemacs/issues/8123.
;; 2016/12/27 dadams
;;     Added: show-invisible-comments-shows-all.
;;     hide/show-comments(-1): Respect show-invisible-comments-shows-all.
;;     NOTE: Default behavior has changed: other invisible text is no longer made visible.
;; 2015/08/01 dadams
;;     Added hide/show-comments-1.  (And removed save-excursion around looking-back etc.)
;;     hide/show-comments:
;;       Use with-silent-modifications if available.  Use hide/show-comments-1.
;; 2015/07/31 dadams
;;     hide/show-comments:
;;       Bind buffer-file-name to nil to inhibit ask-user-about-supersession-threat.
;; 2015/07/29 dadams
;;     hide/show-comments:
;;       No-op if no comment-start.  Pass NOERROR arg to comment-normalize-vars.
;; 2014/11/05 dadams
;;     hide/show-comments:
;;       Use comment-forward even for "", so handle setting CEND correctly, e.g., for C++,
;;       where comment-end is "" but multi-line comments are also OK.
;;       Do not hide newline after single-line comments.
;;       hide-whitespace-before-comment-flag non-nil no longer hides empty lines.
;;       Prevent infloop for comment at bol.
;;       Thx to Hinrik Sigurosson.
;; 2014/02/06 dadams
;;     Added: hide-whitespace-before-comment-flag.
;;     hide/show-comments:
;;       Go to start of comment before calling comment-forward.
;;       Hide whitespace preceding comment, if hide-whitespace-before-comment-flag.
;; 2013/12/26 dadams
;;     hide/show-comments: Update START to comment end or END.
;; 2013/10/09 dadams
;;     hide/show-comments: Use save-excursion.  If empty comment-end go to CBEG.
;;                         Use comment-forward if available.
;; 2012/10/06 dadams
;;     hide/show-comments: Call comment-normalize-vars first.  Thx to Stefan Monnier.
;;     hide/show-comments-toggle: Do nothing if newcomment.el not available.
;; 2012/05/10 dadams
;;     Added: hide/show-comments-toggle.  Thx to Denny Zhang for the suggestion.
;; 2011/11/23 dadams
;;     hide/show-comments: Bug fix - ensure CEND is not past eob.
;; 2011/05/11 dadams
;;     Created: moved code here from thing-cmds.el.
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
;;
;;; Code:


(defvar comment-start)                  ; In `newcomment.el'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defcustom ignore-comments-flag t
  "*Non-nil means macro `with-comments-hidden' hides comments."
  :type 'boolean :group 'matching)

;;;###autoload
(defcustom hide-whitespace-before-comment-flag t
  "*Non-nil means `hide/show-comments' hides whitespace preceding a comment.
It does not hide empty lines (newline chars), however."
  :type 'boolean :group 'matching)

;;;###autoload
(defcustom show-invisible-comments-shows-all nil
  "Non-nil means `(hide/show-comments 'show ...)' shows all invisible text.
The default value, nil, means it shows only text that was made
invisible by `(hide/show-comments 'hide ...)'."
  :type 'boolean :group 'matching)


(defmacro with-comments-hidden (start end &rest body)
  "Evaluate the forms in BODY while comments are hidden from START to END.
But if `ignore-comments-flag' is nil, just evaluate BODY,
without hiding comments.  Show comments again when BODY is finished.

See `hide/show-comments', which is used to hide and show the comments.
Note that prior to Emacs 21, this never hides comments."
  (let ((result  (make-symbol "result"))
        (ostart  (make-symbol "ostart"))
        (oend    (make-symbol "oend")))
    `(let ((,ostart  ,start)
           (,oend    ,end)
           ,result)
      (unwind-protect (setq ,result  (progn (when ignore-comments-flag
                                              (hide/show-comments 'hide ,ostart ,oend))
                                            ,@body))
        (when ignore-comments-flag (hide/show-comments 'show ,ostart ,oend))
        ,result))))

;;;###autoload
(defun hide/show-comments (&optional hide/show start end)
  "Hide or show comments from START to END.
Interactively, hide comments, or show them if you use a prefix arg.
\(This is thus *NOT* a toggle command.)

If option `hide-whitespace-before-comment-flag' is non-nil, then hide
also any whitespace preceding a comment.

Interactively, START and END default to the region limits, if active.
Otherwise, including non-interactively, they default to `point-min'
and `point-max'.

Uses `save-excursion', restoring point.

Option `show-invisible-comments-shows-all':

* If non-nil then using this command to show invisible text shows
  *ALL* such text, regardless of how it was hidden.  IOW, it does not
  just show invisible text that you previously hid using this command.

* If nil (the default value) then using this command to show invisible
  text makes visible only such text that was previously hidden by this
  command.  (More precisely, it makes visible only text whose
  `invisible' property has value `hide-comment'.)

From Lisp, a HIDE/SHOW value of `hide' hides comments.  Other values
show them.

This command does nothing in Emacs versions prior to Emacs 21, because
it needs `comment-search-forward'."

  (interactive
   (cons (if current-prefix-arg 'show 'hide)
         (if (or (not mark-active)  (null (mark))  (= (point) (mark)))
             (list (point-min) (point-max))
           (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point))))))
  (when (and comment-start              ; No-op if no comment syntax defined.
             (require 'newcomment nil t)) ; `comment-search-forward', Emacs 21+.
    (comment-normalize-vars 'NO-ERROR)  ; Must call this first.
    (unless start (setq start  (point-min)))
    (unless end   (setq end    (point-max)))
    (unless (<= start end) (setq start  (prog1 end (setq end  start)))) ; Swap.
    (if (fboundp 'with-silent-modifications)
        (with-silent-modifications      ; Emacs 23+.
            (restore-buffer-modified-p nil) (hide/show-comments-1 hide/show start end))
      (let ((bufmodp           (buffer-modified-p)) ; Emacs < 23.
            (buffer-read-only  nil)
            (buffer-file-name  nil))    ; Inhibit `ask-user-about-supersession-threat'.
        (set-buffer-modified-p nil)
        (unwind-protect (hide/show-comments-1 hide/show start end)
          (set-buffer-modified-p bufmodp))))))

;; Used only so that we can use `hide/show-comments' with older Emacs releases that do not
;; have macro `with-silent-modifications' and built-in `restore-buffer-modified-p', which
;; it uses.
(defun hide/show-comments-1 (hide/show start end)
  "Helper for `hide/show-comments'."
  (let (cbeg cend)
    (if (eq 'hide hide/show)
        (add-to-invisibility-spec 'hide-comment)
      (remove-from-invisibility-spec 'hide-comment))
    (save-excursion
      (goto-char start)
      (while (and (< start end)  (save-excursion
                                   (setq cbeg  (comment-search-forward end 'NOERROR))))
        (goto-char cbeg)
        (save-excursion
          (setq cend  (cond ((fboundp 'comment-forward) ; Emacs 22+
                             (if (comment-forward 1)
                                 (if (= (char-before) ?\n) (1- (point)) (point))
                               end))
                            ((string= "" comment-end) (min (line-end-position) end))
                            (t (search-forward comment-end end 'NOERROR)))))
        (when hide-whitespace-before-comment-flag ; Hide preceding whitespace.
          (if (fboundp 'looking-back)   ; Emacs 22+
              (when (looking-back "\n?\\s-*" nil 'GREEDY)
                (setq cbeg  (match-beginning 0)))
            (while (memq (char-before cbeg) '(?\   ?\t ?\f)) (setq cbeg  (1- cbeg)))
            (when (eq (char-before cbeg) ?\n) (setq cbeg  (1- cbeg))))
          ;; First line: Hide newline after comment.
          (when (and (= cbeg (point-min))  (= (char-after cend) ?\n))
            (setq cend  (min (1+ cend) end))))
        (when (and cbeg  cend)
          (if show-invisible-comments-shows-all
              (put-text-property cbeg cend 'invisible (and (eq 'hide hide/show)
                                                           'hide-comment))
            (while (< cbeg cend)
              ;; Do nothing to text that is already invisible for some other reason.
              (unless (and (get-text-property cbeg 'invisible)
                           (not (eq 'hide-comment (get-text-property cbeg 'invisible))))
                (put-text-property cbeg (1+ cbeg) 'invisible (and (eq 'hide hide/show)
                                                                  'hide-comment)))
              (setq cbeg  (1+ cbeg)))))
        (goto-char (setq start  (or cend  end)))))))

(defun hide/show-comments-toggle (&optional start end)
  "Toggle hiding/showing of comments in the active region or whole buffer.
If the region is active then toggle in the region.  Otherwise, in the
whole buffer.

This command does nothing in Emacs versions prior to Emacs 21, because
it needs `comment-search-forward'.

Interactively, START and END default to the region limits, if active.
Otherwise, including non-interactively, they default to `point-min'
and `point-max'.

See `hide/show-comments' for more information."
  (interactive (if (or (not mark-active)  (null (mark))  (= (point) (mark)))
                   (list (point-min) (point-max))
                 (if (< (point) (mark)) (list (point) (mark)) (list (mark) (point)))))
  (when (require 'newcomment nil t)     ; `comment-search-forward', Emacs 21+.
    (comment-normalize-vars)            ; Must call this first.
    (if (save-excursion
          (goto-char start)
          (and (comment-search-forward end 'NOERROR)
               (if show-invisible-comments-shows-all
                   (get-text-property (point) 'invisible)
                 (eq 'hide-comment (get-text-property (point) 'invisible)))))
        (hide/show-comments 'show start end)
      (hide/show-comments 'hide start end))))

;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'hide-comnt)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; hide-comnt.el ends here
