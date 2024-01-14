;;; evil-search.el --- Search and substitute -*- lexical-binding: t -*-

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

;;; Code:

(require 'evil-core)
(require 'evil-common)
(require 'evil-ex)

(defun evil-select-search-module (option module)
  "Change the search module according to MODULE.
If MODULE is `isearch', then Emacs' isearch module is used.
If MODULE is `evil-search', then Evil's own interactive
search module is used."
  (let ((search-functions
         '(forward
           backward
           word-forward
           word-backward
           unbounded-word-forward
           unbounded-word-backward
           next
           previous)))
    (dolist (fun search-functions)
      (let ((isearch (intern (format "evil-search-%s" fun)))
            (evil-search (intern (format "evil-ex-search-%s" fun))))
        (if (eq module 'isearch)
            (substitute-key-definition
             evil-search isearch evil-motion-state-map)
          (substitute-key-definition
           isearch evil-search evil-motion-state-map)))))
  (set-default option module))

;; this customization is here because it requires
;; the knowledge of `evil-select-search-mode'
(defcustom evil-search-module 'isearch
  "The search module to be used.  May be either `isearch', for
Emacs' isearch module, or `evil-search', for Evil's own
interactive search module.  N.b. changing this will not affect keybindings.
To swap out relevant keybindings, see `evil-select-search-module' function."
  :type '(radio (const :tag "Emacs built-in isearch." :value isearch)
                (const :tag "Evil interactive search." :value evil-search))
  :group 'evil
  :set #'evil-select-search-module
  :initialize #'evil-custom-initialize-pending-reset)

(defun evil-push-search-history (string forward)
  "Push STRING into the appropriate search history (determined by FORWARD)."
  (let ((var (if forward
                 'evil-search-forward-history
               'evil-search-backward-history)))
    (unless (equal string (car (symbol-value var)))
      (push string (symbol-value var)))))

(defun evil-search-incrementally (forward regexp-p)
  "Search incrementally for user-entered text."
  (let ((evil-search-prompt (evil-search-prompt forward))
        (isearch-search-fun-function 'evil-isearch-function)
        (point (point))
        search-nonincremental-instead)
    (setq isearch-forward forward)
    (evil-save-echo-area
      (evil-without-input-method-hooks
       ;; set the input method locally rather than globally to ensure that
       ;; isearch clears the input method when it's finished
       (setq current-input-method evil-input-method)
       (if forward
           (isearch-forward regexp-p)
         (isearch-backward regexp-p))
       (evil-push-search-history isearch-string forward)
       (setq current-input-method nil))
      (when (/= (point) point)
        ;; position the point at beginning of the match only if the call to
        ;; `isearch' has really moved the point. `isearch' doesn't move the
        ;; point only if "C-g" is hit twice to exit the search, in which case we
        ;; shouldn't move the point either.
        (when (and forward isearch-other-end)
          (goto-char isearch-other-end))
        (when (and (eq point (point))
                   (not (string= isearch-string "")))
          (if forward
              (isearch-repeat-forward)
            (isearch-repeat-backward))
          (isearch-exit)
          (when (and forward isearch-other-end)
            (goto-char isearch-other-end)))
        (evil-flash-search-pattern
         (evil-search-message isearch-string forward))))))

(defun evil-flash-search-pattern (string &optional all)
  "Flash last search matches for duration of `evil-flash-delay'.
If ALL is non-nil, flash all matches. STRING is a message
to display in the echo area."
  (let ((lazy-highlight-initial-delay 0)
        (isearch-search-fun-function 'evil-isearch-function)
        (isearch-case-fold-search case-fold-search)
        (disable #'(lambda (&optional _arg) (evil-flash-hook t))))
    (when evil-flash-timer
      (cancel-timer evil-flash-timer))
    (unless (or (null string)
                (string= string ""))
      (evil-echo-area-save)
      (evil-echo "%s" string)
      (isearch-highlight (match-beginning 0) (match-end 0))
      (when all
        (setq isearch-lazy-highlight-wrapped nil
              isearch-lazy-highlight-start (point)
              isearch-lazy-highlight-end (point))
        (isearch-lazy-highlight-new-loop)
        (unless isearch-lazy-highlight-overlays
          (isearch-lazy-highlight-update)))
      (add-hook 'pre-command-hook #'evil-flash-hook nil t)
      (add-hook 'evil-operator-state-exit-hook #'evil-flash-hook nil t)
      (add-hook 'pre-command-hook #'evil-clean-isearch-overlays nil t)
      (setq evil-flash-timer
            (run-at-time evil-flash-delay nil disable)))))

(defun evil-clean-isearch-overlays ()
  "Clean isearch overlays unless `this-command' is search."
  (remove-hook 'pre-command-hook #'evil-clean-isearch-overlays t)
  (unless (memq this-command
                '(evil-search-backward
                  evil-search-forward
                  evil-search-next
                  evil-search-previous
                  evil-search-word-backward
                  evil-search-word-forward))
    (isearch-clean-overlays)))
(put 'evil-clean-isearch-overlays 'permanent-local-hook t)

(defun evil-flash-hook (&optional force)
  "Disable hightlighting if `this-command' is not search.
Disable anyway if FORCE is t."
  (when (or force
            ;; to avoid flicker, don't disable highlighting
            ;; if the next command is also a search command
            (not (memq this-command
                       '(evil-search-backward
                         evil-search-forward
                         evil-search-next
                         evil-search-previous
                         evil-search-word-backward
                         evil-search-word-forward))))
    (evil-echo-area-restore)
    (isearch-dehighlight)
    (setq isearch-lazy-highlight-last-string nil)
    (lazy-highlight-cleanup t)
    (when evil-flash-timer
      (cancel-timer evil-flash-timer)))
  (remove-hook 'pre-command-hook #'evil-flash-hook t)
  (remove-hook 'evil-operator-state-exit-hook #'evil-flash-hook t))
(put 'evil-flash-hook 'permanent-local-hook t)

(defun evil-search-with-predicate (search-fun pred string bound noerror count)
  "Execute a search with a predicate function.
SEARCH-FUN is a search function (e.g. `re-search-forward') and
PREDICATE is a two-argument function satisfying the interface of
`isearch-filter-predicate', or `nil'.  STRING, BOUND, NOERROR and
COUNT are passed unchanged to SEARCH-FUN.  The first match
satisfying the predicate (or `nil') is returned."
  (catch 'done
    (while t
      (let ((result (funcall search-fun string bound noerror count)))
        (cond
         ((not result) (throw 'done nil))
         ((not pred) (throw 'done result))
         ((funcall pred (match-beginning 0) (match-end 0)) (throw 'done result)))))))

(defun evil-search-function (&optional forward regexp-p wrap predicate)
  "Return a search function.
If FORWARD is nil, search backward, otherwise forward.
If REGEXP-P is non-nil, the input is a regular expression.
If WRAP is non-nil, the search wraps around the top or bottom
of the buffer.
If PREDICATE is non-nil, it must be a function accepting two
arguments: the bounds of a match, returning non-nil if that match is
acceptable."
  `(lambda (string &optional bound noerror count)
     (let ((start (point))
           (search-fun ',(if regexp-p
                             (if forward
                                 're-search-forward
                               're-search-backward)
                           (if forward
                               'search-forward
                             'search-backward)))
           result)
       (setq result (evil-search-with-predicate
                     search-fun ,predicate string
                     bound ,(if wrap t 'noerror) count))
       (when (and ,wrap (null result))
         (goto-char ,(if forward '(point-min) '(point-max)))
         (unwind-protect
             (setq result (evil-search-with-predicate
                           search-fun ,predicate string bound noerror count))
           (unless result
             (goto-char start))))
       result)))

(defun evil-isearch-function ()
  "Return a search function for use with isearch.
Based on `isearch-regexp' and `isearch-forward'."
  (evil-search-function isearch-forward evil-regexp-search evil-search-wrap 'isearch-filter-predicate))

(defun evil-search (string forward &optional regexp-p start)
  "Search for STRING and highlight matches.
If FORWARD is nil, search backward, otherwise forward.
If REGEXP-P is non-nil, STRING is taken to be a regular expression.
START is the position to search from; if unspecified, it is
one more than the current position."
  (when (and (stringp string)
             (not (string= string "")))
    (let* ((orig (point))
           (start (or start
                      (if forward
                          (min (point-max) (1+ orig))
                        orig)))
           (isearch-regexp regexp-p)
           (isearch-forward forward)
           (case-fold-search
            (unless (and search-upper-case
                         (not (isearch-no-upper-case-p string nil)))
              case-fold-search))
           (search-func (evil-search-function
                         forward regexp-p evil-search-wrap 'isearch-filter-predicate)))
      ;; no text properties, thank you very much
      (set-text-properties 0 (length string) nil string)
      ;; position to search from
      (goto-char start)
      (setq isearch-string string)
      (isearch-update-ring string regexp-p)
      (condition-case nil
          (funcall search-func string)
        (search-failed
         (goto-char orig)
         (user-error "\"%s\": %s not found"
                     string (if regexp-p "pattern" "string"))))
      ;; always position point at the beginning of the match
      (goto-char (match-beginning 0))
      ;; determine message for echo area
      (cond
       ((and forward (< (point) start))
        (when evil-search-wrap-ring-bell (ding))
        (setq string "Search wrapped around BOTTOM of buffer"))
       ((and (not forward) (> (point) start))
        (when evil-search-wrap-ring-bell (ding))
        (setq string "Search wrapped around TOP of buffer"))
       (t
        (setq string (evil-search-message string forward))))
      (evil-flash-search-pattern string t))))

(defun evil-search-word (forward unbounded symbol)
  "Search for word near point.
If FORWARD is nil, search backward, otherwise forward. If SYMBOL
is non-nil then the functions searches for the symbol at point,
otherwise for the word at point."
  (let ((string (car regexp-search-ring)))
    (setq isearch-forward forward)
    (cond
     ((and (memq last-command
                 '(evil-search-word-forward
                   evil-search-word-backward))
           (stringp string)
           (not (string= string "")))
      (evil-search string forward t))
     (t
      (setq string (evil-find-thing forward (if symbol 'symbol 'evil-word)))
      (cond
       ((null string)
        (user-error "No word under point"))
       (unbounded
        (setq string (regexp-quote string)))
       (t
        (setq string
              (format (if symbol "\\_<%s\\_>" "\\<%s\\>")
                      (regexp-quote string)))))
      (evil-push-search-history string forward)
      (evil-search string forward t)))))

(defun evil--find-thing (forward thing)
  "Return a cons of THING near point as a string and its position.
THING should be a symbol understood by `thing-at-point',
e.g. `symbol' or `word'.  If FORWARD is nil, search backward,
otherwise forward.  Returns nil if nothing is found."
  (let ((move (if forward #'forward-char #'backward-char))
        (end (if forward #'eobp #'bobp))
        string)
    (save-excursion
      (setq string (thing-at-point thing))
      ;; if there's nothing under point, go forwards
      ;; (or backwards) to find it
      (while (and (null string) (not (funcall end)))
        (funcall move)
        (setq string (thing-at-point thing)))
      (when (stringp string)
        (set-text-properties 0 (length string) nil string))
      (when (> (length string) 0)
        (cons string (point))))))

(defun evil-find-thing (forward thing)
  "Return a THING near point as a string.
THING should be a symbol understood by `thing-at-point',
e.g. `symbol' or `word'.  If FORWARD is nil, search backward,
otherwise forward.  Returns nil if nothing is found."
  (car (evil--find-thing forward thing)))

(defun evil-find-word (forward)
  "Return word near point as a string.
If FORWARD is nil, search backward, otherwise forward.  Returns
nil if nothing is found."
  (evil-find-thing forward 'word))

(defun evil-find-symbol (forward)
  "Return word near point as a string.
If FORWARD is nil, search backward, otherwise forward.  Returns
nil if nothing is found."
  (evil-find-thing forward 'symbol))

(defun evil-search-prompt (forward)
  "Return the search prompt for the given direction."
  (if forward "/" "?"))

(defun evil-search-message (string forward)
  "Prefix STRING with the search prompt."
  (format "%s%s" (evil-search-prompt forward) string))

(defadvice isearch-message-prefix (around evil activate)
  "Use `evil-search-prompt'."
  (if evil-search-prompt
      (setq ad-return-value evil-search-prompt)
    ad-do-it))

(defadvice isearch-delete-char (around evil activate)
  "Exit search if no search string."
  (cond
   ((and evil-search-prompt (string= isearch-string ""))
    (let (search-nonincremental-instead)
      (setq isearch-success nil)
      (isearch-exit)))
   (t
    ad-do-it)))

(defadvice isearch-lazy-highlight-search (around evil activate)
  "Never wrap the search in this context."
  (let (evil-search-wrap)
    ad-do-it))

;;; Ex search

(defun evil-ex-regex-without-case (re)
  "Return the regular expression without all occurrences of \\c and \\C."
  (evil-transform-regexp re '((?c . "") (?C . ""))))

(defun evil-ex-regex-case (re default-case)
  "Return the case as implied by \\c or \\C in regular expression RE.
If \\c appears anywhere in the pattern, the pattern is case
insensitive. If \\C appears, the pattern is case sensitive.
Only the first occurrence of \\c or \\C is used, all others are
ignored. If neither \\c nor \\C appears in the pattern, the case
specified by DEFAULT-CASE is used. DEFAULT-CASE should be either
`sensitive', `insensitive' or `smart'. In the latter case, the pattern
will be case-sensitive if and only if it contains an upper-case
letter, otherwise it will be case-insensitive."
  (cond
   ((string-match "\\(?:^\\|[^\\\\]\\)\\(?:\\\\\\\\\\)*\\\\\\([cC]\\)" re)
    (if (eq (aref (match-string 1 re) 0) ?c) 'insensitive 'sensitive))
   ((eq default-case 'smart)
    (if (isearch-no-upper-case-p re t) 'insensitive 'sensitive))
   (t default-case)))

;; a pattern
(defun evil-ex-make-substitute-pattern (regexp flags)
  "Create a PATTERN for substitution with FLAGS.
This function respects the values of `evil-ex-substitute-case'
and `evil-ex-substitute-global'."
  (evil-ex-make-pattern
   regexp
   (cond ((memq ?i flags) 'insensitive)
         ((memq ?I flags) 'sensitive)
         ((not evil-ex-substitute-case) evil-ex-search-case)
         (t evil-ex-substitute-case))
   (if (memq ?g flags)
       (not evil-ex-substitute-global)
     evil-ex-substitute-global)))

(defun evil-ex-make-search-pattern (regexp)
  "Create a PATTERN for search.
This function respects the values of `evil-ex-search-case'."
  (evil-ex-make-pattern regexp evil-ex-search-case t))

(defun evil-ex-make-pattern (regexp case whole-line)
  "Create a new search pattern.
REGEXP is the regular expression to be searched for. CASE should
be either `sensitive', `insensitive' for case-sensitive and
case-insensitive search respectively, or `smart'.  In the latter case
the pattern is smart-case, i.e. it is automatically sensitive of the
pattern contains an upper case letter, otherwise it is insensitive.
The input REGEXP is considered a Vim-style regular expression if
`evil-ex-search-vim-style-regexp' is non-nil, in which case it is
transformed to an Emacs style regular expression (i.e. certain
backslash-codes are transformed.  Otherwise REGEXP must be an Emacs
style regular expression and is not transformed."
  (let ((re (evil-ex-regex-without-case regexp))
        (ignore-case (eq (evil-ex-regex-case regexp case) 'insensitive)))
    ;; possibly transform regular expression from vim-style to
    ;; Emacs-style.
    (if (and evil-ex-search-vim-style-regexp
             (not (or (string-prefix-p "\\_<" regexp)
                      (string-suffix-p "\\_>" regexp))))
        (setq re (evil-transform-vim-style-regexp re))
      ;; Even for Emacs regular expressions we translate certain
      ;; whitespace sequences
      (setq re (evil-transform-regexp re '((?t . "\t")
                                           (?n . "\n")
                                           (?r . "\r")))))
    (list re ignore-case whole-line)))

(defun evil-ex-pattern-regex (pattern)
  "Return the regular expression of a search PATTERN."
  (nth 0 pattern))

(defun evil-ex-pattern-ignore-case (pattern)
  "Return t if and only if PATTERN should ignore case."
  (nth 1 pattern))

(defun evil-ex-pattern-whole-line (pattern)
  "Return t if and only if PATTERN should match all occurences of a line.
Otherwise PATTERN matches only the first occurence."
  (nth 2 pattern))

;;; Highlight

(cl-defstruct (evil-ex-hl (:type vector) (:constructor nil)
                          (:copier nil) (:predicate nil))
  (name nil :read-only t) pattern
  (face nil :read-only t) (window nil :read-only t)
  (min nil :documentation "The minimal buffer position of the highlight.")
  (max nil :documentation "The maximal buffer position of the highlight.")
  (match-hook nil :read-only t) (update-hook nil :read-only t)
  (overlays nil :documentation "The active overlays of the highlight."))

(cl-defun evil-ex-make-hl
    (name &key (face 'evil-ex-lazy-highlight) (win (selected-window))
          min max match-hook update-hook)
  "Create a new highlight object with name NAME and properties ARGS.
The following properties are supported:
:face The face to be used for the highlighting overlays.
:win The window in which the highlighting should be shown.
     Note that the highlight will be visible in all windows showing
     the corresponding buffer, but only the matches visible in the
     specified window will actually be highlighted. If :win is nil,
     the matches in all windows will be highlighted.
:min The minimal buffer position for highlighted matches.
:max The maximal buffer position for highlighted matches.
:match-hook A hook to be called once for each highlight.
            The hook must take two arguments, the highlight and
            the overlay for that highlight.
:update-hook A hook called once after updating the highlighting
             with two arguments, the highlight and a message string
             describing the current match status."
  (unless (symbolp name)
    (user-error "Expected symbol as name of highlight"))
  (when (assq name evil-ex-active-highlights-alist)
    (evil-ex-delete-hl name))
  (unless evil-ex-active-highlights-alist
    (add-hook 'window-scroll-functions
              #'evil-ex-hl-update-highlights-scroll nil t)
    (add-hook 'window-size-change-functions
              #'evil-ex-hl-update-highlights-resize nil))
  (push (cons name (vector name
                           nil
                           face
                           win
                           min max
                           match-hook update-hook
                           nil))
        evil-ex-active-highlights-alist))

(defun evil-ex-delete-hl (name)
  "Remove the highlighting object with a certain NAME."
  (let ((hl (cdr (assq name evil-ex-active-highlights-alist))))
    (when hl
      (mapc #'delete-overlay (evil-ex-hl-overlays hl))
      (setq evil-ex-active-highlights-alist
            (assq-delete-all name evil-ex-active-highlights-alist))
      (evil-ex-hl-update-highlights))
    (unless evil-ex-active-highlights-alist
      (remove-hook 'window-scroll-functions
                   #'evil-ex-hl-update-highlights-scroll t)
      (remove-hook 'window-size-change-functions
                   #'evil-ex-hl-update-highlights-resize))))

(defun evil-ex-hl-active-p (name)
  "Whether the highlight with a certain NAME is active."
  (and (assq name evil-ex-active-highlights-alist) t))

(defun evil-ex-hl-change (name pattern)
  "Set the regular expression of highlight NAME to PATTERN."
  (let ((hl (cdr (assq name evil-ex-active-highlights-alist))))
    (when hl
      (setf (evil-ex-hl-pattern hl) pattern)
      (evil-ex-hl-idle-update))))

(defun evil-ex-hl-set-region (name beg end &optional _type)
  "Set minimal and maximal position of highlight NAME to BEG and END."
  (let ((hl (cdr (assq name evil-ex-active-highlights-alist))))
    (when hl
      (setf (evil-ex-hl-min hl) beg
            (evil-ex-hl-max hl) end)
      (evil-ex-hl-idle-update))))

(defun evil-ex-hl-get-max (name)
  "Return the maximal position of the highlight with name NAME."
  (let ((hl (cdr (assq name evil-ex-active-highlights-alist))))
    (and hl (evil-ex-hl-max hl))))

(defun evil-ex-hl-update-highlights ()
  "Update the overlays of all active highlights."
  (dolist (hl (mapcar #'cdr evil-ex-active-highlights-alist))
    (let* ((old-ovs (evil-ex-hl-overlays hl))
           new-ovs
           (pattern (evil-ex-hl-pattern hl))
           (case-fold-search (evil-ex-pattern-ignore-case pattern))
           (case-replace case-fold-search)
           (face (evil-ex-hl-face hl))
           (match-hook (evil-ex-hl-match-hook hl))
           result)
      (if pattern
          ;; collect all visible ranges
          (let (ranges sranges)
            (dolist (win (if (eq evil-ex-interactive-search-highlight
                                 'all-windows)
                             (get-buffer-window-list (current-buffer) nil t)
                           (list (evil-ex-hl-window hl))))
              (when (window-live-p win)
                (let ((beg (max (window-start win)
                                (or (evil-ex-hl-min hl) (point-min))))
                      (end (min (window-end win)
                                (or (evil-ex-hl-max hl) (point-max)))))
                  (when (< beg end)
                    (push (cons beg end) ranges)))))
            (setq ranges (sort ranges #'car-less-than-car))
            (while ranges
              (let ((r1 (pop ranges))
                    (r2 (pop ranges)))
                (cond
                 ;; last range
                 ((null r2) (push r1 sranges))
                 ;; ranges overlap, union
                 ((>= (cdr r1) (car r2))
                  (push (cons (car r1)
                              (max (cdr r1) (cdr r2)))
                        ranges))
                 ;; ranges distinct
                 (t (push r1 sranges)
                    (push r2 ranges)))))

            ;; run through all ranges
            (condition-case lossage
                (save-match-data
                  (dolist (r sranges)
                    (let ((beg (car r))
                          (end (cdr r)))
                      (save-excursion
                        (goto-char beg)
                        ;; set the overlays for the current highlight,
                        ;; reusing old overlays (if possible)
                        (while (and (not (eobp))
                                    (evil-ex-search-find-next-pattern pattern)
                                    (<= (match-end 0) end)
                                    (not (and (= (match-beginning 0) end)
                                              (save-excursion
                                                (goto-char (match-beginning 0))
                                                (bolp)))))
                          (let ((ov (or (pop old-ovs) (make-overlay 0 0))))
                            (move-overlay ov (match-beginning 0) (match-end 0))
                            (overlay-put ov 'face face)
                            (overlay-put ov 'evil-ex-hl (evil-ex-hl-name hl))
                            (overlay-put ov 'priority 1000)
                            (push ov new-ovs)
                            (when match-hook (funcall match-hook hl ov)))
                          (cond
                           ((not (or (evil-ex-pattern-whole-line pattern)
                                     (save-excursion
                                       (goto-char (match-beginning 0))
                                       (search-forward "\n" (match-end 0) t))))
                            (forward-line))
                           ((= (match-beginning 0) (match-end 0))
                            (forward-char))
                           (t (goto-char (match-end 0))))))))
                  (mapc #'delete-overlay old-ovs)
                  (setf (evil-ex-hl-overlays hl) new-ovs)
                  (if (or (null pattern) new-ovs)
                      (setq result t)
                    ;; Maybe the match could just not be found somewhere else?
                    (save-excursion
                      (goto-char (or (evil-ex-hl-min hl) (point-min)))
                      (if (and (evil-ex-search-find-next-pattern pattern)
                               (< (match-end 0) (or (evil-ex-hl-max hl)
                                                    (point-max))))
                          (setq result (format "Match in line %d"
                                               (line-number-at-pos
                                                (match-beginning 0))))
                        (setq result "No match")))))

              (invalid-regexp (setq result (cadr lossage)))
              (search-failed (setq result (nth 2 lossage)))
              (error (setq result (format "%s" (cadr lossage))))))
        ;; no pattern, remove all highlights
        (mapc #'delete-overlay old-ovs)
        (setf (evil-ex-hl-overlays hl) new-ovs))
      (when (evil-ex-hl-update-hook hl)
        (funcall (evil-ex-hl-update-hook hl) hl result)))))

(defun evil-ex-search-find-next-pattern (pattern &optional direction)
  "Look for the next occurrence of PATTERN in a certain DIRECTION.
Note that this function ignores the whole-line property of PATTERN."
  (setq direction (or direction 'forward))
  (let ((case-fold-search (evil-ex-pattern-ignore-case pattern)))
    (cond
     ((eq direction 'forward)
      (re-search-forward (evil-ex-pattern-regex pattern) nil t))
     ((eq direction 'backward)
      (let* ((pnt (point))
             (ret (re-search-backward (evil-ex-pattern-regex pattern) nil t))
             (m (and ret (match-data))))
        (if ret
            (forward-char)
          (goto-char (point-min)))
        (let ((fwdret
               (re-search-forward (evil-ex-pattern-regex pattern) nil t)))
          (cond
           ((and fwdret (< (match-beginning 0) pnt))
            (setq ret fwdret)
            (goto-char (match-beginning 0)))
           (ret
            (set-match-data m)
            (goto-char (match-beginning 0)))
           (t
            (goto-char pnt)
            ret)))))
     (t (user-error "Unknown search direction `%s'" direction)))))

(defun evil-ex-hl-idle-update ()
  "Trigger the timer to update the highlights in the current buffer."
  (when (and evil-ex-interactive-search-highlight
             evil-ex-active-highlights-alist)
    (when evil-ex-hl-update-timer
      (cancel-timer evil-ex-hl-update-timer))
    (setq evil-ex-hl-update-timer
          (run-at-time evil-ex-hl-update-delay nil
                       #'evil-ex-hl-do-update-highlight
                       (current-buffer)))))

(defun evil-ex-hl-do-update-highlight (&optional buffer)
  "Timer function for updating the highlights."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (evil-ex-hl-update-highlights)))
  (setq evil-ex-hl-update-timer nil))

(defun evil-ex-hl-update-highlights-scroll (win _beg)
  "Update highlights after scrolling in some window."
  (with-current-buffer (window-buffer win)
    (evil-ex-hl-idle-update)))
(put 'evil-ex-hl-update-highlights-scroll 'permanent-local-hook t)

(defun evil-ex-hl-update-highlights-resize (frame)
  "Update highlights after resizing a window."
  (let ((buffers (delete-dups (mapcar #'window-buffer (window-list frame)))))
    (dolist (buf buffers)
      (with-current-buffer buf
        (evil-ex-hl-idle-update)))))
(put 'evil-ex-hl-update-highlights-resize 'permanent-local-hook t)

;; interactive search
(defun evil-ex-search-activate-highlight (pattern)
  "Activate highlighting of the search pattern set to PATTERN.
This function does nothing if `evil-ex-search-interactive' or
`evil-ex-search-highlight-all' is nil. "
  (when (and evil-ex-search-interactive evil-ex-search-highlight-all)
    (with-current-buffer (or evil-ex-original-buffer (current-buffer))
      (unless (evil-ex-hl-active-p 'evil-ex-search)
        (evil-ex-make-hl 'evil-ex-search
                         :win (or (minibuffer-selected-window) (selected-window))))
      (when pattern
        (evil-ex-hl-change 'evil-ex-search pattern)))))

(defun evil-ex-search (&optional count)
  "Search forward or backward COUNT times for the current Ex search pattern.
The search pattern is determined by `evil-ex-search-pattern', and the
direction by `evil-ex-search-direction'."
  (setq evil-ex-search-start-point (point)
        evil-ex-last-was-search t
        count (or count 1))
  (let ((orig (point))
        wrapped)
    (dotimes (_ (or count 1))
      (when (and (eq evil-ex-search-direction 'forward) (not (eobp)))
        (forward-char)
        ;; maybe skip end-of-line
        (and (not evil-move-beyond-eol) (eolp) (not (eobp))
             (forward-char)))
      (let ((res (evil-ex-find-next nil nil (not evil-search-wrap))))
        (cond
         ((not res)
          (goto-char orig)
          (signal 'search-failed
                  (list (evil-ex-pattern-regex evil-ex-search-pattern))))
         ((eq res 'wrapped) (setq wrapped t)))))
    (if wrapped
        (let (message-log-max)
          (when evil-search-wrap-ring-bell (ding))
          (message "Search wrapped")))
    (goto-char (match-beginning 0))
    (setq evil-ex-search-match-beg (match-beginning 0)
          evil-ex-search-match-end (match-end 0))
    (evil-ex-search-goto-offset evil-ex-search-offset)
    (evil-ex-search-activate-highlight evil-ex-search-pattern)))

(defun evil-ex-find-next (&optional pattern direction nowrap)
  "Search for the next occurrence of the PATTERN in DIRECTION.
PATTERN must be created using `evil-ex-make-pattern', DIRECTION is
either `forward' or `backward'. If NOWRAP is non-nil, the search does
not wrap at buffer boundaries. Furthermore this function only searches
invisible text if `search-invisible' is t. If PATTERN is not specified
the current global pattern `evil-ex-search-pattern' and if DIRECTION
is not specified the current global direction
`evil-ex-search-direction' is used. This function returns t if the
search was successful, nil if it was unsuccessful and `wrapped' if the
search was successful but wrapped around at buffer boundaries."
  (setq pattern (or pattern evil-ex-search-pattern)
        direction (or direction evil-ex-search-direction))
  (unless (and pattern (evil-ex-pattern-regex pattern))
    (signal 'search-failed (list "No search pattern")))
  (catch 'done
    (let (wrapped)
      (while t
        (if (evil-ex-search-find-next-pattern pattern direction)
            (when (or (eq search-invisible t)
                      (not (isearch-range-invisible
                            (match-beginning 0) (match-end 0))))
              ;; Successful search and not invisible
              (throw 'done (if wrapped 'wrapped t)))
          ;; Unsuccessful search
          (if nowrap
              (throw 'done nil)
            (setq nowrap t
                  wrapped t)
            (goto-char (if (eq direction 'forward)
                           (point-min)
                         (point-max)))))))))

(defun evil-ex-search-update (pattern offset beg end)
  "Update the highlighting for the search pattern.
PATTERN is the search pattern and OFFSET the associated offset.
BEG and END specify the current match."
  (cond
   ((and beg end)
    ;; update overlay
    (if evil-ex-search-overlay
        (move-overlay evil-ex-search-overlay beg end)
      (setq evil-ex-search-overlay (make-overlay beg end))
      (overlay-put evil-ex-search-overlay 'priority 1001)
      (overlay-put evil-ex-search-overlay 'face 'evil-ex-search))
    ;; move point
    (goto-char beg)
    (evil-ex-search-goto-offset offset)
    ;; update highlights
    (when evil-ex-search-highlight-all
      (evil-ex-hl-change 'evil-ex-search pattern)))
   (t
    ;; no match
    (when evil-ex-search-overlay
      ;; remove overlay
      (delete-overlay evil-ex-search-overlay)
      (setq evil-ex-search-overlay nil))
    ;; no highlights
    (when evil-ex-search-highlight-all
      (evil-ex-hl-change 'evil-ex-search nil))
    ;; and go to initial position
    (goto-char evil-ex-search-start-point))))

(defvar evil--ex-search-update-timer nil
  "Timer to coalesce `evil-ex-search-update-pattern' calls after pattern changes.")

(defun evil-ex-search-after-change-function (_beg _end _old-len)
  (when evil--ex-search-update-timer
    (cancel-timer evil--ex-search-update-timer))
  (setq evil--ex-search-update-timer
        (run-with-idle-timer 0 nil #'evil-ex-search-update-pattern)))

(defun evil-ex-search-start-session ()
  "Initialize Ex for interactive search."
  (add-hook 'pre-command-hook #'evil-ex-remove-default nil t)
  (and evil-ex-search-interactive evil-ex-search-incremental
       (add-hook 'after-change-functions
                 #'evil-ex-search-after-change-function nil t))
  (add-hook 'minibuffer-exit-hook #'evil-ex-search-stop-session nil t)
  (add-hook 'mouse-leave-buffer-hook #'evil-ex-search-exit)
  (evil-ex-search-activate-highlight nil))

(defvar evil-ex-search-yank-point nil)

(defun evil-ex-search-stop-session ()
  "Stop interactive search."
  (with-current-buffer evil-ex-original-buffer
    ;; TODO: This is a bad fix to remove duplicates. The duplicates
    ;;       exist because `isearch-range-invisible' may add a single
    ;;       overlay multiple times if we are in an unlucky situation
    ;;       of overlapping overlays. This happens in our case because
    ;;       of the overlays that are used for (lazy) highlighting.
    ;;       Perhaps it would be better to disable those overlays
    ;;       temporarily before calling `isearch-range-invisible'.
    (setq isearch-opened-overlays (delete-dups isearch-opened-overlays))
    (isearch-clean-overlays))
  (remove-hook 'mouse-leave-buffer-hook #'evil-ex-search-exit)
  (when evil--ex-search-update-timer
    (cancel-timer evil--ex-search-update-timer)
    (setq evil--ex-search-update-timer nil))
  (when evil-ex-search-overlay
    (delete-overlay evil-ex-search-overlay)
    (setq evil-ex-search-overlay nil))
  (setq evil-ex-search-yank-point nil))

(defun evil-ex-split-search-pattern (pattern direction)
  "Split PATTERN in regexp, offset and next-pattern parts.
Return a triplet (REGEXP OFFSET NEXT-SEARCH)."
  (if (string-match
       (if (eq direction 'forward)
           "\\(?:^\\|[^\\\\]\\)\\(?:\\\\\\\\\\)*\\(/\\([^;]*\\)\\(?:;\\([/?].*\\)?\\)?\\)?$"
         "\\(?:^\\|[^\\\\]\\)\\(?:\\\\\\\\\\)*\\(\\?\\([^;]*\\)\\(?:;\\([/?].*\\)?\\)?\\)?$")
       pattern)
      (list (substring pattern 0 (match-beginning 1))
            (match-string 2 pattern)
            (match-string 3 pattern))
    (list pattern nil nil)))

(defun evil-ex-search-full-pattern (string count direction)
  "Search for a full search pattern STRING in DIRECTION.
This function splits STRING into \"pattern/offset/;next-pattern\"
parts and performs the search in DIRECTION which must be either
`forward' or `backward'. The first search is repeated COUNT times. If
the pattern part of STRING is empty, the last global pattern stored in
`evil-ex-search-pattern' is used instead if in addition the offset
part is nil (i.e. no pattern/offset separator), the last global offset
stored in `evil-ex-search-offset' is used as offset. The current match
data will correspond to the last successful match. This function
returns a triplet (RESULT PATTERN OFFSET) where RESULT is one of

  t               the search was successful without wrapping
  `wrap'          the search was successful with wrapping
  `empty-pattern' the last pattern was empty
  nil             the search was unsuccessful

and PATTERN and OFFSET are the last pattern and offset this
function searched for. Note that this function does not handle
any error conditions."
  (setq count (or count 1))
  (catch 'done
    (while t
      (let* ((res (evil-ex-split-search-pattern string direction))
             (pat (pop res))
             (offset (pop res))
             (next-pat (pop res))
             (orig-pat pat)
             new-dir repeat-last wrapped)
        (if (> (length pat) 0)
            (setq pat (evil-ex-make-search-pattern pat))
          ;; use last pattern if no new pattern has been specified
          (setq pat (or evil-ex-search-pattern
                        (throw 'done (list 'empty-pattern pat offset)))
                offset (or offset evil-ex-search-offset)))
        (while (> count 0)
          (when (eq (or (evil-ex-find-next pat direction
                                           (not evil-search-wrap))
                        (throw 'done (list nil pat offset)))
                    'wrapped)
            (setq wrapped t))
          (setq count (1- count)))
        (cond
         ;; no next pattern, search complete
         ((zerop (length next-pat))
          (evil-ex-search-goto-offset offset)
          (throw 'done (list (if wrapped 'wrap t) pat offset)))
         ;; single \"?\" or \"/\" means repeat last pattern and finish
         ((= 1 (length next-pat))
          (evil-ex-search-goto-offset offset)
          (setq new-dir (if (string= "/" next-pat) 'forward 'backward)
                count (if (eq direction new-dir) 1 2)
                string orig-pat
                direction new-dir))
         ;; next non-empty pattern, next search iteration
         (t (evil-ex-search-goto-offset offset)
            (setq new-dir (if (= (aref next-pat 0) ?/) 'forward 'backward)
                  repeat-last (when (<= 2 (length next-pat))
                                (member (substring next-pat 0 2) '("//" "??")))
                  count (if (or (eq direction new-dir) (not repeat-last)) 1 2)
                  string (if repeat-last
                             (concat orig-pat (substring next-pat 1))
                           (substring next-pat 1))
                  direction new-dir)))))))

(defun evil-ex-search-update-pattern ()
  "Update the current search pattern."
  (let ((pattern-string (minibuffer-contents-no-properties))
        message)
    (with-selected-window (minibuffer-selected-window)
      (goto-char (1+ evil-ex-search-start-point))
      (condition-case err
          (cl-destructuring-bind (success pattern offset)
              (evil-ex-search-full-pattern pattern-string
                                           (or evil-ex-search-count 1)
                                           evil-ex-search-direction)
            (cond
             ((eq success 'wrap)
              (evil-ex-search-update pattern offset
                                     (match-beginning 0) (match-end 0))
              (setq message "Wrapped"))
             ((eq success 'empty-pattern)
              (evil-ex-search-update nil nil nil nil))
             (success
              (evil-ex-search-update pattern offset
                                     (match-beginning 0) (match-end 0)))
             (t (evil-ex-search-update nil nil nil nil)
                (setq message "Search failed"))))
        (invalid-regexp
         (evil-ex-search-update nil nil nil nil)
         (setq message (cadr err)))
        (error
         (evil-ex-search-update nil nil nil nil)
         (setq message (error-message-string err)))))
    (when message (evil-ex-echo "%s" message))))

(defun evil-ex-search-exit ()
  "Exit interactive search, keeping lazy highlighting active."
  (interactive)
  (exit-minibuffer))

(defun evil-ex-search-abort ()
  "Abort interactive search, disabling lazy highlighting."
  (interactive)
  (evil-ex-delete-hl 'evil-ex-search)
  (abort-recursive-edit))

(defun evil-ex-search-goto-offset (offset)
  "Move point according to search OFFSET and set `evil-this-type' accordingly.
This function assumes that the current match data represents the
current search result."
  (unless (zerop (length offset))
    (let ((beg (match-beginning 0))
          (end (match-end 0)))
      (save-match-data
        (unless (string-match
                 "^\\([esb]\\)?\\(\\([-+]\\)?\\([0-9]*\\)\\)$"
                 offset)
          (user-error "Invalid search offset: %s" offset))
        (let ((count (if (= (match-beginning 4) (match-end 4))
                         (cond
                          ((not (match-beginning 3)) 0)
                          ((= (aref offset (match-beginning 3)) ?+) +1)
                          (t -1))
                       (string-to-number (match-string 2 offset)))))
          (cond
           ((not (match-beginning 1))
            (setq evil-this-type 'line)
            (forward-line count))
           ((= (aref offset (match-beginning 1)) ?e)
            (goto-char (+ end count -1))
            (setq evil-this-type 'inclusive))
           ((memq (aref offset (match-beginning 1)) '(?s ?b))
            (goto-char (+ beg count))
            (setq evil-this-type 'inclusive))))))))

(defun evil-ex-start-search (direction count)
  "Start a new search in a certain DIRECTION."
  (setq evil-ex-search-count count
        evil-ex-search-direction direction
        evil-ex-search-start-point (point)
        evil-ex-last-was-search t)
  ;; store buffer and window where the search started
  (let* ((evil-ex-original-buffer (current-buffer))
         ;; read the search string
         (minibuffer-local-map evil-ex-search-keymap)
         (search-string
          (condition-case err
              (minibuffer-with-setup-hook #'evil-ex-search-start-session
                (read-string
                 (if (eq evil-ex-search-direction 'forward) "/" "?")
                 (when evil-ex-search-history
                   (propertize (car evil-ex-search-history) 'face 'shadow))
                 'evil-ex-search-history))
            (quit
             (evil-ex-delete-hl 'evil-ex-search)
             (goto-char evil-ex-search-start-point)
             (signal (car err) (cdr err))))))
    ;; pattern entered successful
    (goto-char (if (eq evil-ex-search-direction 'forward)
                   (1+ evil-ex-search-start-point)
                 (1- evil-ex-search-start-point)))
    (cl-destructuring-bind (success pattern offset)
        (evil-ex-search-full-pattern search-string
                                     evil-ex-search-count
                                     evil-ex-search-direction)
      (setq evil-ex-search-pattern pattern
            evil-ex-search-offset offset)
      (cond
       ((memq success '(t wrap))
        (goto-char (match-beginning 0))
        (setq evil-ex-search-match-beg (match-beginning 0)
              evil-ex-search-match-end (match-end 0))
        (evil-ex-search-goto-offset offset)
        (evil-push-search-history search-string (eq direction 'forward))
        (if evil-ex-search-incremental
            (unless evil-ex-search-persistent-highlight
              (evil-ex-delete-hl 'evil-ex-search))
          (when evil-ex-search-highlight-all
            (evil-ex-search-activate-highlight pattern))))
       (t
        (goto-char evil-ex-search-start-point)
        (evil-ex-delete-hl 'evil-ex-search)
        (signal 'search-failed (list search-string)))))))

(declare-function evil-ex-search-next "evil-commands")
(defun evil-ex-start-word-search (unbounded direction count &optional symbol)
  "Search for the symbol under point.
The search matches the COUNT-th occurrence of the word.  If the
UNBOUNDED argument is nil, the search matches only at symbol
boundaries, otherwise it matches anywhere.  The DIRECTION
argument should be either `forward' or `backward', determining
the search direction. If SYMBOL is non-nil then the functions
searches for the symbol at point, otherwise for the word at
point."
  (let* ((string (or (evil-find-thing (eq direction 'forward)
                                      (if symbol 'symbol 'word))
                     (user-error "No word under point")))
         (regex (if unbounded
                    (regexp-quote string)
                  (format (if symbol "\\_<%s\\_>" "\\<%s\\>")
                          (regexp-quote string)))))
    (setq evil-ex-search-count count
          evil-ex-search-direction direction
          evil-ex-search-pattern
          (let (evil-ex-search-vim-style-regexp)
            (evil-ex-make-search-pattern regex))
          evil-ex-search-offset nil
          evil-ex-last-was-search t)
    ;; update search history unless this pattern equals the
    ;; previous pattern
    (unless (equal regex (car evil-ex-search-history))
      (push regex evil-ex-search-history))
    (evil-push-search-history regex (eq direction 'forward)))
  (evil-ex-delete-hl 'evil-ex-search)
  (evil-ex-search-next count))

;; substitute
(evil-ex-define-argument-type substitution
  "A substitution pattern argument /pattern/replacement/flags.
This handler highlights the pattern of the current substitution."
  :runner
  (lambda (flag &optional arg)
    (with-current-buffer evil-ex-original-buffer
      (cond
       ((eq flag 'start)
        (evil-ex-make-hl
         'evil-ex-substitute
         :face 'evil-ex-substitute-matches
         :win (minibuffer-selected-window)
         :update-hook #'evil-ex-pattern-update-ex-info
         :match-hook (and evil-ex-substitute-interactive-replace
                          #'evil-ex-pattern-update-replacement))
        (setq flag 'update))
       ((eq flag 'stop)
        (evil-ex-delete-hl 'evil-ex-substitute)))

      (when (and (eq flag 'update)
                 evil-ex-substitute-highlight-all
                 (> (length arg) 0))
        (condition-case err
            (cl-destructuring-bind (pattern replacement _flags)
                (evil-ex-get-substitute-info (or arg "") t)
              (evil-ex-hl-change 'evil-ex-substitute pattern)
              (setq evil-ex-substitute-current-replacement replacement)
              (cl-destructuring-bind (beg end &rest)
                  (if evil-ex-range
                      (evil-expand-range evil-ex-range t)
                    (evil-range (line-beginning-position) (line-end-position)
                                'line :expanded t))
                (evil-ex-hl-set-region 'evil-ex-substitute beg end)))
          (end-of-file (evil-ex-echo "Incomplete replacement"))
          (user-error (evil-ex-echo (error-message-string err))))))))

(defun evil-ex-pattern-update-ex-info (_hl result)
  "Update the Ex info string."
  (when (stringp result)
    (evil-ex-echo "%s" result)))

(defun evil-ex-pattern-update-replacement (_hl overlay)
  "Update the replacement display."
  (let ((repl (if evil-ex-substitute-current-replacement
                  (evil-match-substitute-replacement
                   evil-ex-substitute-current-replacement
                   (not case-replace))
                "")))
    (put-text-property 0 (length repl)
                       'face 'evil-ex-substitute-replacement
                       repl)
    (overlay-put overlay 'after-string repl)))

(defun evil-ex-parse-global (string)
  "Parse STRING as a global argument."
  (let* ((pattern (nth 0 (evil-delimited-arguments string 2)))
         (command (and pattern
                       (>= (- (length string) (length pattern)) 2)
                       (substring string (+ (length pattern) 2)))))
    ;; use last pattern if none given
    (when (zerop (length pattern))
      (setq pattern
            (cond
             ((and (eq evil-search-module 'evil-search) evil-ex-search-pattern)
              (evil-ex-pattern-regex evil-ex-search-pattern))
             ((and (eq evil-search-module 'isearch) (not (zerop (length isearch-string))))
              isearch-string)
             (t (user-error "No previous pattern")))))
    (list pattern command)))

(defun evil-ex-get-substitute-info (string &optional implicit-r)
  "Return the substitution info of :substitute argument STRING.
The result is a triplet \(PATTERN REPLACEMENT FLAGS). PATTERN is an
Ex-pattern (see `evil-ex-make-pattern') and REPLACEMENT is a compiled
replacement expression (see `evil-compile-replacement'). The
information returned is the actual substitution information w.r.t. to
special situations like empty patterns or repetition of previous
substitution commands. If IMPLICIT-R is non-nil, then the \"r\" flag
is assumed, i.e. in case of an empty pattern the last search pattern
is used. It is meant for :substitute commands with arguments."
  (let (pattern replacement flags using-prev-pattern)
    (cond
     ((or (string= string "") (string-match-p "\\`[a-zA-Z]" string))
      ;; No pattern, since it starts with a letter which cannot be a
      ;; separator: Repeat last substitute.
      (setq replacement evil-ex-substitute-replacement)
      ;; flags are everything that is not whitespace
      (when (string-match "[^[:space:]]+" string)
        (setq flags (match-string 0 string))))
     (t (let ((args (evil-delimited-arguments string 3)))
          (setq pattern (pop args)
                replacement (pop args)
                flags (pop args))
          ;; if replacment equals "~" use previous replacement
          (setq replacement (if (equal replacement "~")
                                evil-ex-substitute-replacement
                              (evil-compile-replacement replacement)))
          ;; append implicit "r" flag if required
          (when (and implicit-r (not (memq ?r (append flags nil))))
            (setq flags (concat flags "r"))))))
    ;; if flags equals "&" add previous flags
    (setq flags
          (if (and (> (length flags) 0) (= (aref flags 0) ?&))
              (append (substring flags 1) evil-ex-substitute-flags)
            (append flags nil)))
    ;; if no pattern, use previous pattern, either search or
    ;; substitute pattern depending on `evil-ex-last-was-search' and
    ;; the r flag
    (when (zerop (length pattern))
      (setq pattern
            (if (eq evil-search-module 'evil-search)
                (if (and evil-ex-last-was-search (memq ?r flags))
                    (and evil-ex-search-pattern
                         (setq using-prev-pattern t)
                         (evil-ex-pattern-regex evil-ex-search-pattern))
                  (and evil-ex-substitute-pattern
                       (setq using-prev-pattern t)
                       (evil-ex-pattern-regex evil-ex-substitute-pattern)))
              (if (eq case-fold-search t)
                  isearch-string
                (concat isearch-string "\\C")))
            flags (remq ?r flags)))
    ;; generate pattern
    (when pattern
      ;; Disable vim-style regexp conversion if using a previous pattern, because
      ;; this conversion will already have been done before storing it
      (let ((evil-ex-search-vim-style-regexp (and evil-ex-search-vim-style-regexp
                                                  (not using-prev-pattern))))
        (setq pattern (evil-ex-make-substitute-pattern pattern flags))))
    (list pattern replacement flags)))

(defun evil-ex-nohighlight ()
  "Disable the active search highlightings."
  (interactive)
  (evil-ex-delete-hl 'evil-ex-substitute)
  (evil-ex-delete-hl 'evil-ex-search))

(defun evil-search-yank-word ()
  "Pull next word from buffer into search string."
  (interactive)
  (let ((minibuf-content (minibuffer-contents-no-properties))
        (word "") start)
    (with-current-buffer evil-ex-original-buffer
      ;; Start to initial point if C-w have never been hit.
      (unless evil-ex-search-yank-point
        (if (string= "" minibuf-content)
            (setq evil-ex-search-yank-point evil-ex-search-start-point)
          (setq evil-ex-search-yank-point (point))))
      (save-excursion
        (goto-char evil-ex-search-yank-point)
        (setq start (point))
        (when (looking-at-p (regexp-quote minibuf-content))
          (forward-char (length minibuf-content))
          (setq start (point))
          (forward-word))
        (setq word (buffer-substring-no-properties start (point)))))
    (and (eq evil-ex-search-case 'smart)
         (let (case-fold-search)
           (not (string-match-p "[A-Z]" minibuf-content)))
         (setq word (downcase word)))
    (insert word)))

(provide 'evil-search)

;;; evil-search.el ends here
