;;; git-rebase.el --- Edit Git rebase files  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2024 The Magit Project Contributors

;; Author: Phil Jackson <phil@shellarchive.co.uk>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Magit is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package assists the user in editing the list of commits to be
;; rewritten during an interactive rebase.

;; When the user initiates an interactive rebase, e.g., using "r e" in
;; a Magit buffer or on the command line using "git rebase -i REV",
;; Git invokes the `$GIT_SEQUENCE_EDITOR' (or if that is undefined
;; `$GIT_EDITOR' or even `$EDITOR') letting the user rearrange, drop,
;; reword, edit, and squash commits.

;; This package provides the major-mode `git-rebase-mode' which makes
;; doing so much more fun, by making the buffer more colorful and
;; providing the following commands:
;;
;;   C-c C-c  Tell Git to make it happen.
;;   C-c C-k  Tell Git that you changed your mind, i.e., abort.
;;
;;   p        Move point to previous line.
;;   n        Move point to next line.
;;
;;   M-p      Move the commit at point up.
;;   M-n      Move the commit at point down.
;;
;;   k        Drop the commit at point.
;;   c        Don't drop the commit at point.
;;   r        Change the message of the commit at point.
;;   e        Edit the commit at point.
;;   s        Squash the commit at point, into the one above.
;;   f        Like "s" but don't also edit the commit message.
;;   b        Break for editing at this point in the sequence.
;;   x        Add a script to be run with the commit at point
;;            being checked out.
;;   z        Add noop action at point.
;;
;;   SPC      Show the commit at point in another buffer.
;;   RET      Show the commit at point in another buffer and
;;            select its window.
;;   C-/      Undo last change.
;;
;;   Commands for --rebase-merges:
;;   l        Associate label with current HEAD in sequence.
;;   MM       Merge specified revisions into HEAD.
;;   Mt       Toggle whether the merge will invoke an editor
;;            before committing.
;;   t        Reset HEAD to the specified label.

;; You should probably also read the `git-rebase' manpage.

;;; Code:

(require 'magit)

(require 'easymenu)
(require 'server)
(require 'with-editor)

(defvar recentf-exclude)

;;; Options
;;;; Variables

(defgroup git-rebase nil
  "Edit Git rebase sequences."
  :link '(info-link "(magit)Editing Rebase Sequences")
  :group 'tools)

(defcustom git-rebase-auto-advance t
  "Whether to move to next line after changing a line."
  :group 'git-rebase
  :type 'boolean)

(defcustom git-rebase-show-instructions t
  "Whether to show usage instructions inside the rebase buffer."
  :group 'git-rebase
  :type 'boolean)

(defcustom git-rebase-confirm-cancel t
  "Whether confirmation is required to cancel."
  :group 'git-rebase
  :type 'boolean)

;;;; Faces

(defgroup git-rebase-faces nil
  "Faces used by Git-Rebase mode."
  :group 'faces
  :group 'git-rebase)

(defface git-rebase-hash '((t :inherit magit-hash))
  "Face for commit hashes."
  :group 'git-rebase-faces)

(defface git-rebase-label '((t :inherit magit-refname))
  "Face for labels in label, merge, and reset lines."
  :group 'git-rebase-faces)

(defface git-rebase-description '((t nil))
  "Face for commit descriptions."
  :group 'git-rebase-faces)

(defface git-rebase-action
  '((t :inherit font-lock-keyword-face))
  "Face for action keywords."
  :group 'git-rebase-faces)

(defface git-rebase-killed-action
  '((t :inherit font-lock-comment-face :strike-through t))
  "Face for commented commit action lines."
  :group 'git-rebase-faces)

(defface git-rebase-comment-hash
  '((t :inherit git-rebase-hash :weight bold))
  "Face for commit hashes in commit message comments."
  :group 'git-rebase-faces)

(defface git-rebase-comment-heading
  '((t :inherit font-lock-keyword-face))
  "Face for headings in rebase message comments."
  :group 'git-rebase-faces)

;;; Keymaps

(defvar-keymap git-rebase-mode-map
  :doc "Keymap for Git-Rebase mode."
  :parent special-mode-map
  "C-m" #'git-rebase-show-commit
  "p"   #'git-rebase-backward-line
  "n"   #'forward-line
  "M-p" #'git-rebase-move-line-up
  "M-n" #'git-rebase-move-line-down
  "c"   #'git-rebase-pick
  "k"   #'git-rebase-kill-line
  "C-k" #'git-rebase-kill-line
  "b"   #'git-rebase-break
  "e"   #'git-rebase-edit
  "l"   #'git-rebase-label
  "M M" #'git-rebase-merge
  "M t" #'git-rebase-merge-toggle-editmsg
  "m"   #'git-rebase-edit
  "f"   #'git-rebase-fixup
  "q"   #'undefined
  "r"   #'git-rebase-reword
  "w"   #'git-rebase-reword
  "s"   #'git-rebase-squash
  "t"   #'git-rebase-reset
  "u"   #'git-rebase-update-ref
  "x"   #'git-rebase-exec
  "y"   #'git-rebase-insert
  "z"   #'git-rebase-noop
  "SPC" #'git-rebase-show-or-scroll-up
  "DEL" #'git-rebase-show-or-scroll-down
  "C-x C-t"        #'git-rebase-move-line-up
  "M-<up>"         #'git-rebase-move-line-up
  "M-<down>"       #'git-rebase-move-line-down
  "<remap> <undo>" #'git-rebase-undo)
(put 'git-rebase-reword       :advertised-binding (kbd "r"))
(put 'git-rebase-move-line-up :advertised-binding (kbd "M-p"))
(put 'git-rebase-kill-line    :advertised-binding (kbd "k"))

(easy-menu-define git-rebase-mode-menu git-rebase-mode-map
  "Git-Rebase mode menu"
  '("Rebase"
    ["Pick" git-rebase-pick t]
    ["Reword" git-rebase-reword t]
    ["Edit" git-rebase-edit t]
    ["Squash" git-rebase-squash t]
    ["Fixup" git-rebase-fixup t]
    ["Kill" git-rebase-kill-line t]
    ["Noop" git-rebase-noop t]
    ["Execute" git-rebase-exec t]
    ["Move Down" git-rebase-move-line-down t]
    ["Move Up" git-rebase-move-line-up t]
    "---"
    ["Cancel" with-editor-cancel t]
    ["Finish" with-editor-finish t]))

(defvar git-rebase-command-descriptions
  '((with-editor-finish           . "tell Git to make it happen")
    (with-editor-cancel           . "tell Git that you changed your mind, i.e., abort")
    (git-rebase-backward-line     . "move point to previous line")
    (forward-line                 . "move point to next line")
    (git-rebase-move-line-up      . "move the commit at point up")
    (git-rebase-move-line-down    . "move the commit at point down")
    (git-rebase-show-or-scroll-up . "show the commit at point in another buffer")
    (git-rebase-show-commit
     . "show the commit at point in another buffer and select its window")
    (undo                         . "undo last change")
    (git-rebase-kill-line         . "drop the commit at point")
    (git-rebase-insert            . "insert a line for an arbitrary commit")
    (git-rebase-noop              . "add noop action at point")))

;;; Commands

(defun git-rebase-pick ()
  "Use commit on current line.
If the region is active, act on all lines touched by the region."
  (interactive)
  (git-rebase-set-action "pick"))

(defun git-rebase-reword ()
  "Edit message of commit on current line.
If the region is active, act on all lines touched by the region."
  (interactive)
  (git-rebase-set-action "reword"))

(defun git-rebase-edit ()
  "Stop at the commit on the current line.
If the region is active, act on all lines touched by the region."
  (interactive)
  (git-rebase-set-action "edit"))

(defun git-rebase-squash ()
  "Meld commit on current line into previous commit, edit message.
If the region is active, act on all lines touched by the region."
  (interactive)
  (git-rebase-set-action "squash"))

(defun git-rebase-fixup ()
  "Meld commit on current line into previous commit, discard its message.
If the region is active, act on all lines touched by the region."
  (interactive)
  (git-rebase-set-action "fixup"))

(defvar-local git-rebase-comment-re nil)

(defvar git-rebase-short-options
  '((?b . "break")
    (?e . "edit")
    (?f . "fixup")
    (?l . "label")
    (?m . "merge")
    (?p . "pick")
    (?r . "reword")
    (?s . "squash")
    (?t . "reset")
    (?u . "update-ref")
    (?x . "exec"))
  "Alist mapping single key of an action to the full name.")

(defclass git-rebase-action ()
  (;; action-type: commit, exec, bare, label, merge
   (action-type    :initarg :action-type    :initform nil)
   ;; Examples for each action type:
   ;; | action | action options | target  | trailer |
   ;; |--------+----------------+---------+---------|
   ;; | pick   |                | hash    | subject |
   ;; | exec   |                | command |         |
   ;; | noop   |                |         |         |
   ;; | reset  |                | name    | subject |
   ;; | merge  | -C hash        | name    | subject |
   (action         :initarg :action         :initform nil)
   (action-options :initarg :action-options :initform nil)
   (target         :initarg :target         :initform nil)
   (trailer        :initarg :trailer        :initform nil)
   (comment-p      :initarg :comment-p      :initform nil)))

(defvar git-rebase-line-regexps
  `((commit . ,(concat
                (regexp-opt '("e" "edit"
                              "f" "fixup"
                              "p" "pick"
                              "r" "reword"
                              "s" "squash")
                            "\\(?1:")
                " \\(?3:[^ \n]+\\) ?\\(?4:.*\\)"))
    (exec . "\\(?1:x\\|exec\\) \\(?3:.*\\)")
    (bare . ,(concat (regexp-opt '("b" "break" "noop") "\\(?1:")
                     " *$"))
    (label . ,(concat (regexp-opt '("l" "label"
                                    "t" "reset"
                                    "u" "update-ref")
                                  "\\(?1:")
                      " \\(?3:[^ \n]+\\) ?\\(?4:.*\\)"))
    (merge . ,(concat "\\(?1:m\\|merge\\) "
                      "\\(?:\\(?2:-[cC] [^ \n]+\\) \\)?"
                      "\\(?3:[^ \n]+\\)"
                      " ?\\(?4:.*\\)"))))

;;;###autoload
(defun git-rebase-current-line ()
  "Parse current line into a `git-rebase-action' instance.
If the current line isn't recognized as a rebase line, an
instance with all nil values is returned."
  (save-excursion
    (goto-char (line-beginning-position))
    (if-let ((re-start (concat "^\\(?5:" (regexp-quote comment-start)
                               "\\)? *"))
             (type (seq-some (lambda (arg)
                               (let ((case-fold-search nil))
                                 (and (looking-at (concat re-start (cdr arg)))
                                      (car arg))))
                             git-rebase-line-regexps)))
        (git-rebase-action
         :action-type    type
         :action         (and-let* ((action (match-string-no-properties 1)))
                           (or (cdr (assoc action git-rebase-short-options))
                               action))
         :action-options (match-string-no-properties 2)
         :target         (match-string-no-properties 3)
         :trailer        (match-string-no-properties 4)
         :comment-p      (and (match-string 5) t))
      ;; Use default empty class rather than nil to ease handling.
      (git-rebase-action))))

(defun git-rebase-set-action (action)
  "Set action of commit line to ACTION.
If the region is active, operate on all lines that it touches.
Otherwise, operate on the current line.  As a special case, an
ACTION of nil comments the rebase line, regardless of its action
type."
  (pcase (git-rebase-region-bounds t)
    (`(,beg ,end)
     (let ((end-marker (copy-marker end))
           (pt-below-p (and mark-active (< (mark) (point)))))
       (set-marker-insertion-type end-marker t)
       (goto-char beg)
       (while (< (point) end-marker)
         (with-slots (action-type target trailer comment-p)
             (git-rebase-current-line)
           (cond
            ((and action (eq action-type 'commit))
             (let ((inhibit-read-only t))
               (magit-delete-line)
               (insert (concat action " " target " " trailer "\n"))))
            ((and action-type (not (or action comment-p)))
             (let ((inhibit-read-only t))
               (insert comment-start " "))
             (forward-line))
            (t
             ;; In the case of --rebase-merges, commit lines may have
             ;; other lines with other action types, empty lines, and
             ;; "Branch" comments interspersed.  Move along.
             (forward-line)))))
       (goto-char
        (if git-rebase-auto-advance
            end-marker
          (if pt-below-p (1- end-marker) beg)))
       (goto-char (line-beginning-position))))
    (_ (ding))))

(defun git-rebase-line-p (&optional pos)
  (save-excursion
    (when pos (goto-char pos))
    (and (oref (git-rebase-current-line) action-type)
         t)))

(defun git-rebase-region-bounds (&optional fallback)
  "Return region bounds if both ends touch rebase lines.
Each bound is extended to include the entire line touched by the
point or mark.  If the region isn't active and FALLBACK is
non-nil, return the beginning and end of the current rebase line,
if any."
  (cond
   ((use-region-p)
    (let ((beg (save-excursion (goto-char (region-beginning))
                               (line-beginning-position)))
          (end (save-excursion (goto-char (region-end))
                               (line-end-position))))
      (when (and (git-rebase-line-p beg)
                 (git-rebase-line-p end))
        (list beg (1+ end)))))
   ((and fallback (git-rebase-line-p))
    (list (line-beginning-position)
          (1+ (line-end-position))))))

(defun git-rebase-move-line-down (n)
  "Move the current commit (or command) N lines down.
If N is negative, move the commit up instead.  With an active
region, move all the lines that the region touches, not just the
current line."
  (interactive "p")
  (pcase-let* ((`(,beg ,end)
                (or (git-rebase-region-bounds)
                    (list (line-beginning-position)
                          (1+ (line-end-position)))))
               (pt-offset (- (point) beg))
               (mark-offset (and mark-active (- (mark) beg))))
    (save-restriction
      (narrow-to-region
       (point-min)
       (1-
        (if git-rebase-show-instructions
            (save-excursion
              (goto-char (point-min))
              (while (or (git-rebase-line-p)
                         ;; The output for --rebase-merges has empty
                         ;; lines and "Branch" comments interspersed.
                         (looking-at-p "^$")
                         (looking-at-p (concat git-rebase-comment-re
                                               " Branch")))
                (forward-line))
              (line-beginning-position))
          (point-max))))
      (if (or (and (< n 0) (= beg (point-min)))
              (and (> n 0) (= end (point-max)))
              (> end (point-max)))
          (ding)
        (goto-char (if (< n 0) beg end))
        (forward-line n)
        (atomic-change-group
          (let ((inhibit-read-only t))
            (insert (delete-and-extract-region beg end)))
          (let ((new-beg (- (point) (- end beg))))
            (when (use-region-p)
              (setq deactivate-mark nil)
              (set-mark (+ new-beg mark-offset)))
            (goto-char (+ new-beg pt-offset))))))))

(defun git-rebase-move-line-up (n)
  "Move the current commit (or command) N lines up.
If N is negative, move the commit down instead.  With an active
region, move all the lines that the region touches, not just the
current line."
  (interactive "p")
  (git-rebase-move-line-down (- n)))

(defun git-rebase-highlight-region (start end window rol)
  (let ((inhibit-read-only t)
        (deactivate-mark nil)
        (bounds (git-rebase-region-bounds)))
    (mapc #'delete-overlay magit-section-highlight-overlays)
    (when bounds
      (magit-section-make-overlay (car bounds) (cadr bounds)
                                  'magit-section-heading-selection))
    (if (and bounds (not magit-section-keep-region-overlay))
        (funcall (default-value 'redisplay-unhighlight-region-function) rol)
      (funcall (default-value 'redisplay-highlight-region-function)
               start end window rol))))

(defun git-rebase-unhighlight-region (rol)
  (mapc #'delete-overlay magit-section-highlight-overlays)
  (funcall (default-value 'redisplay-unhighlight-region-function) rol))

(defun git-rebase-kill-line ()
  "Kill the current action line.
If the region is active, act on all lines touched by the region."
  (interactive)
  (git-rebase-set-action nil))

(defun git-rebase-insert (rev)
  "Read an arbitrary commit and insert it below current line."
  (interactive (list (magit-read-branch-or-commit "Insert revision")))
  (forward-line)
  (if-let ((info (magit-rev-format "%h %s" rev)))
      (let ((inhibit-read-only t))
        (insert "pick " info ?\n))
    (user-error "Unknown revision")))

(defun git-rebase-set-noncommit-action (action value-fn arg)
  (goto-char (line-beginning-position))
  (pcase-let* ((inhibit-read-only t)
               (`(,initial ,trailer ,comment-p)
                (and (not arg)
                     (with-slots ((ln-action action)
                                  target trailer comment-p)
                         (git-rebase-current-line)
                       (and (equal ln-action action)
                            (list target trailer comment-p)))))
               (value (funcall value-fn initial)))
    (pcase (list value initial comment-p)
      (`("" nil ,_)
       (ding))
      (`(""  ,_ ,_)
       (magit-delete-line))
      (_
       (if initial
           (magit-delete-line)
         (forward-line))
       (insert (concat action " " value
                       (and (equal value initial)
                            trailer
                            (concat " " trailer))
                       "\n"))
       (unless git-rebase-auto-advance
         (forward-line -1))))))

(defun git-rebase-exec (arg)
  "Insert a shell command to be run after the current commit.

If there already is such a command on the current line, then edit
that instead.  With a prefix argument insert a new command even
when there already is one on the current line.  With empty input
remove the command on the current line, if any."
  (interactive "P")
  (git-rebase-set-noncommit-action
   "exec"
   (lambda (initial) (read-shell-command "Execute: " initial))
   arg))

(defun git-rebase-label (arg)
  "Add a label after the current commit.
If there already is a label on the current line, then edit that
instead.  With a prefix argument, insert a new label even when
there is already a label on the current line.  With empty input,
remove the label on the current line, if any."
  (interactive "P")
  (git-rebase-set-noncommit-action
   "label"
   (lambda (initial)
     (read-from-minibuffer
      "Label: " initial magit-minibuffer-local-ns-map))
   arg))

(defun git-rebase-buffer-labels ()
  (let (labels)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\(?:l\\|label\\) \\([^ \n]+\\)" nil t)
        (push (match-string-no-properties 1) labels)))
    (nreverse labels)))

(defun git-rebase-reset (arg)
  "Reset the current HEAD to a label.
If there already is a reset command on the current line, then
edit that instead.  With a prefix argument, insert a new reset
line even when point is already on a reset line.  With empty
input, remove the reset command on the current line, if any."
  (interactive "P")
  (git-rebase-set-noncommit-action
   "reset"
   (lambda (initial)
     (or (magit-completing-read "Label" (git-rebase-buffer-labels)
                                nil t initial)
         ""))
   arg))

(defun git-rebase-update-ref (arg)
  "Insert an update-ref action after the current line.
If there is already an update-ref action on the current line,
then edit that instead.  With a prefix argument, insert a new
action even when there is already one on the current line.  With
empty input, remove the action on the current line, if any."
  (interactive "P")
  (git-rebase-set-noncommit-action
   "update-ref"
   (lambda (initial)
     (or (magit-completing-read "Ref" (magit-list-refs) nil nil initial)
         ""))
   arg))

(defun git-rebase-merge (arg)
  "Add a merge command after the current commit.
If there is already a merge command on the current line, then
replace that command instead.  With a prefix argument, insert a
new merge command even when there is already one on the current
line.  With empty input, remove the merge command on the current
line, if any."
  (interactive "P")
  (git-rebase-set-noncommit-action
   "merge"
   (lambda (_)
     (or (magit-completing-read "Merge" (git-rebase-buffer-labels))
         ""))
   arg))

(defun git-rebase-merge-toggle-editmsg ()
  "Toggle whether an editor is invoked when performing the merge at point.
When a merge command uses a lower-case -c, the message for the
specified commit will be opened in an editor before creating the
commit.  For an upper-case -C, the message will be used as is."
  (interactive)
  (with-slots (action-type target action-options trailer)
      (git-rebase-current-line)
    (if (eq action-type 'merge)
        (let ((inhibit-read-only t))
          (magit-delete-line)
          (insert
           (format "merge %s %s %s\n"
                   (replace-regexp-in-string
                    "-[cC]" (lambda (c)
                              (if (equal c "-c") "-C" "-c"))
                    action-options t t)
                   target
                   trailer)))
      (ding))))

(defun git-rebase-set-bare-action (action arg)
  (goto-char (line-beginning-position))
  (with-slots ((ln-action action) comment-p)
      (git-rebase-current-line)
    (let ((same-action-p (equal action ln-action))
          (inhibit-read-only t))
      (when (or arg
                (not ln-action)
                (not same-action-p)
                (and same-action-p comment-p))
        (unless (or arg (not same-action-p))
          (magit-delete-line))
        (insert action ?\n)
        (unless git-rebase-auto-advance
          (forward-line -1))))))

(defun git-rebase-noop (&optional arg)
  "Add noop action at point.

If the current line already contains a noop action, leave it
unchanged.  If there is a commented noop action present, remove
the comment.  Otherwise add a new noop action.  With a prefix
argument insert a new noop action regardless of what is already
present on the current line.

A noop action can be used to make git perform a rebase even if
no commits are selected.  Without the noop action present, git
would see an empty file and therefore do nothing."
  (interactive "P")
  (git-rebase-set-bare-action "noop" arg))

(defun git-rebase-break (&optional arg)
  "Add break action at point.

If there is a commented break action present, remove the comment.
If the current line already contains a break action, add another
break action only if a prefix argument is given.

A break action can be used to interrupt the rebase at the
specified point.  It is particularly useful for pausing before
the first commit in the sequence.  For other cases, the
equivalent behavior can be achieved with `git-rebase-edit'."
  (interactive "P")
  (git-rebase-set-bare-action "break" arg))

(defun git-rebase-undo (&optional arg)
  "Undo some previous changes.
Like `undo' but works in read-only buffers."
  (interactive "P")
  (let ((inhibit-read-only t))
    (undo arg)))

(defun git-rebase--show-commit (&optional scroll)
  (let ((magit--disable-save-buffers t))
    (save-excursion
      (goto-char (line-beginning-position))
      (if-let ((rev (with-slots (action-type target)
                        (git-rebase-current-line)
                      (and (eq action-type 'commit)
                           target))))
          (pcase scroll
            ('up   (magit-diff-show-or-scroll-up))
            ('down (magit-diff-show-or-scroll-down))
            (_     (apply #'magit-show-commit rev
                          (magit-diff-arguments 'magit-revision-mode))))
        (ding)))))

(defun git-rebase-show-commit ()
  "Show the commit on the current line if any."
  (interactive)
  (git-rebase--show-commit))

(defun git-rebase-show-or-scroll-up ()
  "Update the commit buffer for commit on current line.

Either show the commit at point in the appropriate buffer, or if
that buffer is already being displayed in the current frame and
contains information about that commit, then instead scroll the
buffer up."
  (interactive)
  (git-rebase--show-commit 'up))

(defun git-rebase-show-or-scroll-down ()
  "Update the commit buffer for commit on current line.

Either show the commit at point in the appropriate buffer, or if
that buffer is already being displayed in the current frame and
contains information about that commit, then instead scroll the
buffer down."
  (interactive)
  (git-rebase--show-commit 'down))

(defun git-rebase-backward-line (&optional n)
  "Move N lines backward (forward if N is negative).
Like `forward-line' but go into the opposite direction."
  (interactive "p")
  (forward-line (- (or n 1))))

;;; Mode

;;;###autoload
(define-derived-mode git-rebase-mode special-mode "Git Rebase"
  "Major mode for editing of a Git rebase file.

Rebase files are generated when you run \"git rebase -i\" or run
`magit-interactive-rebase'.  They describe how Git should perform
the rebase.  See the documentation for git-rebase (e.g., by
running \"man git-rebase\" at the command line) for details."
  :group 'git-rebase
  (setq comment-start (or (magit-get "core.commentChar") "#"))
  (setq git-rebase-comment-re (concat "^" (regexp-quote comment-start)))
  (setq font-lock-defaults (list (git-rebase-mode-font-lock-keywords) t t))
  (unless git-rebase-show-instructions
    (let ((inhibit-read-only t))
      (flush-lines git-rebase-comment-re)))
  (unless with-editor-mode
    ;; Maybe already enabled when using `shell-command' or an Emacs shell.
    (with-editor-mode 1))
  (when git-rebase-confirm-cancel
    (add-hook 'with-editor-cancel-query-functions
              #'git-rebase-cancel-confirm nil t))
  (setq-local redisplay-highlight-region-function #'git-rebase-highlight-region)
  (setq-local redisplay-unhighlight-region-function #'git-rebase-unhighlight-region)
  (add-hook 'with-editor-pre-cancel-hook  #'git-rebase-autostash-save  nil t)
  (add-hook 'with-editor-post-cancel-hook #'git-rebase-autostash-apply nil t)
  (setq imenu-prev-index-position-function
        #'magit-imenu--rebase-prev-index-position-function)
  (setq imenu-extract-index-name-function
        #'magit-imenu--rebase-extract-index-name-function)
  (when (boundp 'save-place)
    (setq save-place nil)))

(defun git-rebase-cancel-confirm (force)
  (or (not (buffer-modified-p))
      force
      (magit-confirm 'abort-rebase "Abort this rebase" nil 'noabort)))

(defun git-rebase-autostash-save ()
  (when-let ((rev (magit-file-line
                   (expand-file-name "rebase-merge/autostash" (magit-gitdir)))))
    (push (cons 'stash rev) with-editor-cancel-alist)))

(defun git-rebase-autostash-apply ()
  (when-let ((rev (cdr (assq 'stash with-editor-cancel-alist))))
    (magit-stash-apply rev)))

(defun git-rebase-match-comment-line (limit)
  (re-search-forward (concat git-rebase-comment-re ".*") limit t))

(defun git-rebase-mode-font-lock-keywords ()
  "Font lock keywords for Git-Rebase mode."
  `((,(concat "^" (cdr (assq 'commit git-rebase-line-regexps)))
     (1 'git-rebase-action)
     (3 'git-rebase-hash)
     (4 'git-rebase-description))
    (,(concat "^" (cdr (assq 'exec git-rebase-line-regexps)))
     (1 'git-rebase-action)
     (3 'git-rebase-description))
    (,(concat "^" (cdr (assq 'bare git-rebase-line-regexps)))
     (1 'git-rebase-action))
    (,(concat "^" (cdr (assq 'label git-rebase-line-regexps)))
     (1 'git-rebase-action)
     (3 'git-rebase-label)
     (4 'font-lock-comment-face))
    ("^\\(m\\(?:erge\\)?\\) -[Cc] \\([^ \n]+\\) \\([^ \n]+\\)\\( #.*\\)?"
     (1 'git-rebase-action)
     (2 'git-rebase-hash)
     (3 'git-rebase-label)
     (4 'font-lock-comment-face))
    ("^\\(m\\(?:erge\\)?\\) \\([^ \n]+\\)"
     (1 'git-rebase-action)
     (2 'git-rebase-label))
    (,(concat git-rebase-comment-re " *"
              (cdr (assq 'commit git-rebase-line-regexps)))
     0 'git-rebase-killed-action t)
    (git-rebase-match-comment-line 0 'font-lock-comment-face)
    ("\\[[^[]*\\]"
     0 'magit-keyword t)
    ("\\(?:fixup!\\|squash!\\)"
     0 'magit-keyword-squash t)
    (,(format "^%s Rebase \\([^ ]*\\) onto \\([^ ]*\\)" comment-start)
     (1 'git-rebase-comment-hash t)
     (2 'git-rebase-comment-hash t))
    (,(format "^%s \\(Commands:\\)" comment-start)
     (1 'git-rebase-comment-heading t))
    (,(format "^%s Branch \\(.*\\)" comment-start)
     (1 'git-rebase-label t))))

(defun git-rebase-mode-show-keybindings ()
  "Modify the \"Commands:\" section of the comment Git generates
at the bottom of the file so that in place of the one-letter
abbreviation for the command, it shows the command's keybinding.
By default, this is the same except for the \"pick\" command."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (when (and git-rebase-show-instructions
                 (re-search-forward
                  (concat git-rebase-comment-re "\\s-+p, pick")
                  nil t))
        (goto-char (line-beginning-position))
        (pcase-dolist (`(,cmd . ,desc) git-rebase-command-descriptions)
          (insert (format (propertize "%s %s %s\n"
                                      'font-lock-face 'font-lock-comment-face)
                          comment-start
                          (string-pad
                           (substitute-command-keys (format "\\[%s]" cmd)) 8)
                          desc)))
        (while (re-search-forward
                (concat git-rebase-comment-re "\\(?:"
                        "\\( \\.?     *\\)\\|"
                        "\\( +\\)\\([^\n,],\\) \\([^\n ]+\\) \\)")
                nil t)
          (if (match-string 1)
              (replace-match (make-string 10 ?\s) t t nil 1)
            (let ((cmd (intern (concat "git-rebase-" (match-string 4)))))
              (if (not (fboundp cmd))
                  (delete-region (line-beginning-position)
                                 (1+ (line-end-position)))
                (add-text-properties (line-beginning-position)
                                     (1+ (line-end-position))
                                     '(font-lock-face font-lock-comment-face))
                (replace-match " " t t nil 2)
                (replace-match
                 (string-pad
                  (save-match-data
                    (substitute-command-keys (format "\\[%s]" cmd)))
                  8)
                 t t nil 3)))))))))

(add-hook 'git-rebase-mode-hook #'git-rebase-mode-show-keybindings t)

(defun git-rebase-mode-disable-before-save-hook ()
  (setq-local before-save-hook nil))

(add-hook 'git-rebase-mode-hook #'git-rebase-mode-disable-before-save-hook)

;;;###autoload
(defconst git-rebase-filename-regexp "/git-rebase-todo\\'")
;;;###autoload
(add-to-list 'auto-mode-alist
             (cons git-rebase-filename-regexp #'git-rebase-mode))

(add-to-list 'with-editor-server-window-alist
             (cons git-rebase-filename-regexp #'switch-to-buffer))

(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude git-rebase-filename-regexp))

(add-to-list 'with-editor-file-name-history-exclude git-rebase-filename-regexp)

;;; Imenu Support

(defun magit-imenu--rebase-prev-index-position-function ()
  "Move point to previous commit in git-rebase buffer.
Used as a value for `imenu-prev-index-position-function'."
  (catch 'found
    (while (not (bobp))
      (git-rebase-backward-line)
      (when (git-rebase-line-p)
        (throw 'found t)))))

(defun magit-imenu--rebase-extract-index-name-function ()
  "Return imenu name for line at point.
Point should be at the beginning of the line.  This function
is used as a value for `imenu-extract-index-name-function'."
  (buffer-substring-no-properties (line-beginning-position)
                                  (line-end-position)))

;;; _
(provide 'git-rebase)
;;; git-rebase.el ends here
