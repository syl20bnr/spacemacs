;;; diminish.el --- Diminished modes are minor modes with no modeline display

;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: Will Mengarini <seldon@eskimo.com>
;; Maintainer: Martin Yrjölä <martin.yrjola@gmail.com>
;; URL: https://github.com/myrjola/diminish.el
;; Created: Th 19 Feb 98
;; Version: 0.46
;; Package-Requires: ((emacs "24.3"))
;; Keywords: extensions, diminish, minor, codeprose

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Minor modes each put a word on the mode line to signify that they're
;; active.  This can cause other displays, such as % of file that point is
;; at, to run off the right side of the screen.  For some minor modes, such
;; as mouse-avoidance-mode, the display is a waste of space, since users
;; typically set the mode in their .emacs & never change it.  For other
;; modes, such as my jiggle-mode, it's a waste because there's already a
;; visual indication of whether the mode is in effect.

;; A diminished mode is a minor mode that has had its mode line
;; display diminished, usually to nothing, although diminishing to a
;; shorter word or a single letter is also supported.  This package
;; implements diminished modes.

;; You can use this package either interactively or from your .emacs file.
;; In either case, first you'll need to copy this file to a directory that
;; appears in your load-path.  `load-path' is the name of a variable that
;; contains a list of directories Emacs searches for files to load.
;; To prepend another directory to load-path, put a line like
;; (add-to-list 'load-path "c:/My_Directory") in your .emacs file.

;; To create diminished modes interactively, type
;;   M-x load-library
;; to get a prompt like
;;   Load library:
;; and respond `diminish' (unquoted).  Then type
;;   M-x diminish
;; to get a prompt like
;;   Diminish what minor mode:
;; and respond with the name of some minor mode, like mouse-avoidance-mode.
;; You'll then get this prompt:
;;   To what mode-line display:
;; Respond by just hitting <Enter> if you want the name of the mode
;; completely removed from the mode line.  If you prefer, you can abbreviate
;; the name.  If your abbreviation is 2 characters or more, such as "Av",
;; it'll be displayed as a separate word on the mode line, just like minor
;; modes' names.  If it's a single character, such as "V", it'll be scrunched
;; up against the previous word, so for example if the undiminished mode line
;; display had been "Abbrev Fill Avoid", it would become "Abbrev FillV".
;; Multiple single-letter diminished modes will all be scrunched together.
;; The display of undiminished modes will not be affected.

;; To find out what the mode line would look like if all diminished modes
;; were still minor, type M-x diminished-modes.  This displays in the echo
;; area the complete list of minor or diminished modes now active, but
;; displays them all as minor.  They remain diminished on the mode line.

;; To convert a diminished mode back to a minor mode, type M-x diminish-undo
;; to get a prompt like
;;   Restore what diminished mode:
;; Respond with the name of some diminished mode.  To convert all
;; diminished modes back to minor modes, respond to that prompt
;; with `diminished-modes' (unquoted, & note the hyphen).

;; When you're responding to the prompts for mode names, you can use
;; completion to avoid extra typing; for example, m o u SPC SPC SPC
;; is usually enough to specify mouse-avoidance-mode.  Mode names
;; typically end in "-mode", but for historical reasons
;; auto-fill-mode is named by "auto-fill-function".

;; To create diminished modes noninteractively in your .emacs file, put
;; code like
;;   (require 'diminish)
;;   (diminish 'abbrev-mode "Abv")
;;   (diminish 'jiggle-mode)
;;   (diminish 'mouse-avoidance-mode "M")
;; near the end of your .emacs file.  It should be near the end so that any
;; minor modes your .emacs loads will already have been loaded by the time
;; they're to be converted to diminished modes.

;; To diminish a major mode, (setq mode-name "whatever") in the mode hook.

;;; Epigraph:

;;         "The quality of our thoughts is bordered on all sides
;;          by our facility with language."
;;               --J. Michael Straczynski

;;; Code:

(eval-when-compile (require 'cl-lib))

(defvar diminish-must-not-copy-minor-mode-alist nil
  "Non-nil means loading diminish.el won't (copy-alist minor-mode-alist).
Normally `minor-mode-alist' is setq to that copy on loading diminish because
at least one of its cons cells, that for abbrev-mode, is read-only (see
ELisp Info on \"pure storage\").  If you setq this variable to t & then
try to diminish abbrev-mode under GNU Emacs 19.34, you'll get the error
message \"Attempt to modify read-only object\".")

(or diminish-must-not-copy-minor-mode-alist
    (cl-callf copy-alist minor-mode-alist))

(defvar diminished-mode-alist nil
  "The original `minor-mode-alist' value of all (diminish)ed modes.")

(defvar diminish-history-symbols nil
  "Command history for symbols of diminished modes.")

(defvar diminish-history-names nil
  "Command history for names of diminished modes.")

;; When we diminish a mode, we are saying we want it to continue doing its
;; work for us, but we no longer want to be reminded of it.  It becomes a
;; night worker, like a janitor; it becomes an invisible man; it remains a
;; component, perhaps an important one, sometimes an indispensable one, of
;; the mechanism that maintains the day-people's world, but its place in
;; their thoughts is diminished, usually to nothing.  As we grow old we
;; diminish more and more such thoughts, such people, usually to nothing.

;; "The wise man knows that to keep under is to endure."  The diminished
;; often come to value their invisibility.  We speak--speak--of "the strong
;; silent type", but only as a superficiality; a stereotype in a movie,
;; perhaps, but even if an acquaintance, necessarily, by hypothesis, a
;; distant one.  The strong silent type is actually a process.  It begins
;; with introspection, continues with judgment, and is shaped by the
;; discovery that these judgments are impractical to share; there is no
;; appetite for the wisdom of the self-critical among the creatures of
;; material appetite who dominate our world.  Their dominance's Darwinian
;; implications reinforce the self-doubt that is the germ of higher wisdom.
;; The thoughtful contemplate the evolutionary triumph of the predator.
;; Gnostics deny the cosmos could be so evil; this must all be a prank; the
;; thoughtful remain silent, invisible, self-diminished, and discover,
;; perhaps at first in surprise, the freedom they thus gain, and grow strong.

;;;###autoload
(defun diminish (mode &optional to-what)
  "Diminish mode-line display of minor mode MODE to TO-WHAT (default \"\").

Interactively, enter (with completion) the name of any minor mode, followed
on the next line by what you want it diminished to (default empty string).
The response to neither prompt should be quoted.  However, in Lisp code,
both args must be quoted, the first as a symbol, the second as a string,
as in (diminish \\='jiggle-mode \" Jgl\").

The mode-line displays of minor modes usually begin with a space, so
the modes' names appear as separate words on the mode line.  However, if
you're having problems with a cramped mode line, you may choose to use single
letters for some modes, without leading spaces.  Capitalizing them works
best; if you then diminish some mode to \"X\" but have `abbrev-mode' enabled as
well, you'll get a display like \"AbbrevX\".  This function prepends a space
to TO-WHAT if it's > 1 char long & doesn't already begin with a space."
  (interactive (list (read (completing-read
                            "Diminish what minor mode: "
                            (mapcar (lambda (x) (list (symbol-name (car x))))
                                    minor-mode-alist)
                            nil t nil 'diminish-history-symbols))
                     (read-from-minibuffer
                      "To what mode-line display: "
                      nil nil nil 'diminish-history-names)))
  (let ((minor (assq mode minor-mode-alist)))
    (when minor
        (progn (cl-callf or to-what "")
               (when (and (stringp to-what)
                          (> (length to-what) 1))
                 (or (= (string-to-char to-what) ?\ )
                     (cl-callf2 concat " " to-what)))
               (or (assq mode diminished-mode-alist)
                   (push (copy-sequence minor) diminished-mode-alist))
               (setcdr minor (list to-what))))))

;; But an image comes to me, vivid in its unreality, of a loon alone on his
;; forest lake, shrieking his soul out into a canopy of stars.  Alone this
;; afternoon in my warm city apartment, I can feel the bite of his night air,
;; and smell his conifers.  In him there is no acceptance of diminishment.

;; "I have a benevolent habit of pouring out myself to everybody,
;;  and would even pay for a listener, and I am afraid
;;  that the Athenians may think me too talkative."
;;       --Socrates, in the /Euthyphro/

;; I remember a news story about a retired plumber who had somehow managed to
;; steal a military tank.  He rode it down city streets, rode over a parked
;; car--no one was hurt--rode onto a freeway, that concrete symbol of the
;; American spirit, or so we fancy it, shouting "Plumber Bob!  Plumber Bob!".
;; He was shot dead by police.

;;;###autoload
(defun diminish-undo (mode)
  "Restore mode-line display of diminished mode MODE to its minor-mode value.
Do nothing if the arg is a minor mode that hasn't been diminished.

Interactively, enter (with completion) the name of any diminished mode (a
mode that was formerly a minor mode on which you invoked \\[diminish]).
To restore all diminished modes to minor status, answer `diminished-modes'.
The response to the prompt shouldn't be quoted.  However, in Lisp code,
the arg must be quoted as a symbol, as in (diminish-undo \\='diminished-modes)."
  (interactive
   (list (read (completing-read
                "Restore what diminished mode: "
                (cons (list "diminished-modes")
                      (mapcar (lambda (x) (list (symbol-name (car x))))
                              diminished-mode-alist))
                nil t nil 'diminish-history-symbols))))
  (if (eq mode 'diminished-modes)
      (let ((diminished-modes diminished-mode-alist))
        (while diminished-modes
          (diminish-undo (caar diminished-modes))
          (cl-callf cdr diminished-modes)))
    (let ((minor      (assq mode      minor-mode-alist))
          (diminished (assq mode diminished-mode-alist)))
      (or minor
          (error "%S is not currently registered as a minor mode" mode))
      (when diminished
        (setcdr minor (cdr diminished))))))

;; Plumber Bob was not from Seattle, my grey city, for rainy Seattle is a
;; city of interiors, a city of the self-diminished.  When I moved here one
;; sunny June I was delighted to find that ducks and geese were common in
;; the streets.  But I hoped to find a loon or two, and all I found were
;; ducks and geese.  I wondered about this; I wondered why there were no
;; loons in Seattle; but my confusion resulted from my ignorance of the
;; psychology of rain, which is to say my ignorance of diminished modes.
;; What I needed, and lacked, was a way to discover they were there.

;;;###autoload
(defun diminished-modes ()
  "Echo all active diminished or minor modes as if they were minor.
The display goes in the echo area; if it's too long even for that,
you can see the whole thing in the *Messages* buffer.
This doesn't change the status of any modes; it just lets you see
what diminished modes would be on the mode-line if they were still minor."
  (interactive)
  (let ((minor-modes minor-mode-alist)
        message)
    (while minor-modes
      (when (symbol-value (caar minor-modes))
        ;; This minor mode is active in this buffer
        (let* ((mode-pair (car minor-modes))
               (mode (car mode-pair))
               (minor-pair (or (assq mode diminished-mode-alist) mode-pair))
               (minor-name (cadr minor-pair)))
          (when (symbolp minor-name)
            ;; This minor mode uses symbol indirection in the cdr
            (let ((symbols-seen (list minor-name)))
              (while (and (symbolp (cl-callf symbol-value minor-name))
                          (not (memq minor-name symbols-seen)))
                (push minor-name symbols-seen))))
          (push minor-name message)))
      (cl-callf cdr minor-modes))
    (setq message (mapconcat 'identity (nreverse message) ""))
    (when (= (string-to-char message) ?\ )
      (cl-callf substring message 1))
    (message "%s" message)))

;; A human mind is a Black Forest of diminished modes.  Some are dangerous;
;; most of the mind of an intimate is a secret stranger, and these diminished
;; modes are rendered more unpredictable by their long isolation from the
;; corrective influence of interaction with reality.  The student of history
;; learns that this description applies to whole societies as well.  In some
;; ways the self-diminished are better able to discern the night worker.
;; They are rendered safer by their heightened awareness of others'
;; diminished modes, and more congenial by the spare blandness of their own
;; mode lines.  To some people rain is truly depressing, but others it just
;; makes pensive, and, forcing them indoors where they may not have the
;; luxury of solitude, teaches them to self-diminish.  That was what I had
;; not understood when I was searching for loons among the ducks and geese.
;; Loons come to Seattle all the time, but the ones that like it learn to be
;; silent, learn to self-diminish, and take on the colors of ducks and geese.
;; Now, here a dozen years, I can recognize them everywhere, standing quietly
;; in line with the ducks and geese at the espresso counter, gazing placidly
;; out on the world through loon-red eyes, thinking secret thoughts.

(provide 'diminish)

;;; diminish.el ends here
