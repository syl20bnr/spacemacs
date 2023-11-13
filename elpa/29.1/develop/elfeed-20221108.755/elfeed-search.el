;;; elfeed-search.el --- list feed entries -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Code:

(require 'cl-lib)
(require 'browse-url)
(require 'wid-edit) ; widget-inactive face
(require 'bookmark)
(bookmark-maybe-load-default-file)

(require 'elfeed)
(require 'elfeed-db)
(require 'elfeed-lib)

;; Interface to elfeed-show (lazy required)
(declare-function elfeed-show-entry 'elfeed-show (entry))

(defvar elfeed-search-entries ()
  "List of the entries currently on display.")

(defvar elfeed-search-filter-history nil
  "Filter history for `completing-read'.")

(defvar elfeed-search-last-update 0
  "The last time the buffer was redrawn in epoch seconds.")

(defvar elfeed-search-update-hook ()
  "List of functions to run immediately following a search buffer update.")

(defcustom elfeed-search-filter "@6-months-ago +unread"
  "Query string filtering shown entries."
  :group 'elfeed
  :type 'string)

(defcustom elfeed-sort-order 'descending
  "The order in which entries should be displayed.

Changing this from the default will lead to misleading results
during live filter editing, but the results be will correct when
live filter editing is exited. "
  :group 'elfeed
  :type '(choice (const descending) (const ascending)))

(defcustom elfeed-search-sort-function nil
  "Sort predicate applied to the list of entries before display.

This function must take two entries as arguments, an interface
suitable as the predicate for `sort'.

Changing this from the default will lead to misleading results
during live filter editing, but the results be will correct when
live filter editing is exited."
  :group 'elfeed
  :type '(choice function (const nil)))

(defcustom elfeed-search-remain-on-entry nil
  "When non-nil, keep point at entry after performing a command.

When nil, move to next entry."
  :group 'elfeed
  :type 'boolean)

(defcustom elfeed-search-clipboard-type 'PRIMARY
  "Selects the clipboard `elfeed-search-yank' should use.
Choices are the symbols PRIMARY, SECONDARY, or CLIPBOARD."
  :group 'elfeed
  :type '(choice (const PRIMARY) (const SECONDARY) (const CLIPBOARD)))

(defcustom elfeed-search-date-format '("%Y-%m-%d" 10 :left)
  "The `format-time-string' format, target width, and alignment for dates.

This should be (string integer keyword) for (format width alignment).
Possible alignments are :left and :right."
  :group 'elfeed
  :type '(list string integer (choice (const :left) (const :right))))

(defcustom elfeed-search-compile-filter t
  "If non-nil, compile search filters into bytecode on the fly."
  :group 'elfeed
  :type 'boolean)

(defvar elfeed-search-filter-active nil
  "When non-nil, Elfeed is currently reading a filter from the minibuffer.
When live editing the filter, it is bound to :live.")

(defvar elfeed-search-filter-overflowing nil
  "When non-nil, the current live filter overflows the window.")

(defvar elfeed-search--offset 1
  "Offset between line numbers and entry list position.")

(defvar elfeed-search-header-function #'elfeed-search--header
  "Function that returns the string to be used for the Elfeed search header.")

(defvar elfeed-search-print-entry-function #'elfeed-search-print-entry--default
  "Function to print entries into the *elfeed-search* buffer.")

(defalias 'elfeed-search-tag-all-unread
  (elfeed-expose #'elfeed-search-tag-all 'unread)
  "Add the `unread' tag to all selected entries.")

(defalias 'elfeed-search-untag-all-unread
  (elfeed-expose #'elfeed-search-untag-all 'unread)
  "Remove the `unread' tag from all selected entries.")

(defalias 'elfeed-search-update--force
  (elfeed-expose #'elfeed-search-update :force)
  "Force refresh view of the feed listing.")

(defun elfeed-search-quit-window ()
  "Save the database, then `quit-window'."
  (interactive)
  (elfeed-db-save)
  (quit-window))

(defun elfeed-search-last-entry ()
  "Place point on last entry."
  (interactive)
  (goto-char (point-max))
  (forward-line -1))

(defun elfeed-search-first-entry ()
  "Place point on first entry."
  (interactive)
  (goto-char (point-min)))

(defvar elfeed-search-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map "h" #'describe-mode)
      (define-key map "q" #'elfeed-search-quit-window)
      (define-key map "g" #'elfeed-search-update--force)
      (define-key map "G" #'elfeed-search-fetch)
      (define-key map (kbd "RET") #'elfeed-search-show-entry)
      (define-key map "s" #'elfeed-search-live-filter)
      (define-key map "S" #'elfeed-search-set-filter)
      (define-key map "c" #'elfeed-search-clear-filter)
      (define-key map "b" #'elfeed-search-browse-url)
      (define-key map "y" #'elfeed-search-yank)
      (define-key map "u" #'elfeed-search-tag-all-unread)
      (define-key map "r" #'elfeed-search-untag-all-unread)
      (define-key map "n" #'next-line)
      (define-key map "p" #'previous-line)
      (define-key map "+" #'elfeed-search-tag-all)
      (define-key map "-" #'elfeed-search-untag-all)
      (define-key map "<" #'elfeed-search-first-entry)
      (define-key map ">" #'elfeed-search-last-entry)))
  "Keymap for elfeed-search-mode.")

(defun elfeed-search--intro-header ()
  "Return the header shown to new users."
  (with-temp-buffer
    (cl-flet ((button (f)
                (insert-button (symbol-name f)
                               'follow-link t
                               'action (lambda (_) (call-interactively f)))))
      (insert "Database empty. Use ")
      (button 'elfeed-add-feed)
      (insert ", or ")
      (button 'elfeed-load-opml)
      (insert ", or ")
      (button 'elfeed-update)
      (insert ".")
      (buffer-string))))

(defun elfeed-search--count-unread ()
  "Count the number of entries and feeds being currently displayed."
  (if (and elfeed-search-filter-active elfeed-search-filter-overflowing)
      "?/?:?"
    (cl-loop with feeds = (make-hash-table :test 'equal)
             for entry in elfeed-search-entries
             for feed = (elfeed-entry-feed entry)
             for url = (elfeed-feed-url feed)
             count entry into entry-count
             count (elfeed-tagged-p 'unread entry) into unread-count
             do (puthash url t feeds)
             finally
             (cl-return
              (format "%d/%d:%d"
                      unread-count entry-count
                      (hash-table-count feeds))))))

(defun elfeed-search--header ()
  "Computes the string to be used as the Elfeed header."
  (cond
   ((zerop (elfeed-db-last-update))
    (elfeed-search--intro-header))
   ((> (elfeed-queue-count-total) 0)
    (let ((total (elfeed-queue-count-total))
          (in-process (elfeed-queue-count-active)))
      (format "%d jobs pending, %d active..."
              (- total in-process) in-process)))
   ((let* ((db-time (seconds-to-time (elfeed-db-last-update)))
           (update (format-time-string "%Y-%m-%d %H:%M" db-time))
           (unread (elfeed-search--count-unread)))
      (format "Updated %s, %s%s"
              (propertize update 'face 'elfeed-search-last-update-face)
              (propertize unread 'face 'elfeed-search-unread-count-face)
              (cond
               (elfeed-search-filter-active "")
               ((string-match-p "[^ ]" elfeed-search-filter)
                (concat ", " (propertize elfeed-search-filter
                                         'face 'elfeed-search-filter-face)))
               ("")))))))

(defun elfeed-search-mode ()
  "Major mode for listing elfeed feed entries.
\\{elfeed-search-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (use-local-map elfeed-search-mode-map)
  (setq major-mode 'elfeed-search-mode
        mode-name "elfeed-search"
        truncate-lines t
        buffer-read-only t
        desktop-save-buffer #'elfeed-search-desktop-save
        ;; Provide format string via symbol value slot so that it will
        ;; not be %-construct interpolated. The symbol is uninterned
        ;; so that it's not *really* a global variable.
        header-line-format
        (let ((symbol (make-symbol "dummy")))
          (put symbol 'risky-local-variable t)
          `(:eval
            (prog1 ',symbol
              (set ',symbol (funcall elfeed-search-header-function))))))
  (set (make-local-variable 'bookmark-make-record-function)
       #'elfeed-search-bookmark-make-record)
  (buffer-disable-undo)
  (hl-line-mode)
  (make-local-variable 'elfeed-search-entries)
  (make-local-variable 'elfeed-search-filter)
  (add-hook 'elfeed-update-hooks #'elfeed-search-update)
  (add-hook 'elfeed-update-init-hooks #'elfeed-search-update--force)
  (add-hook 'kill-buffer-hook #'elfeed-db-save t t)
  (add-hook 'elfeed-db-unload-hook #'elfeed-search--unload)
  (elfeed-search-update :force)
  (run-mode-hooks 'elfeed-search-mode-hook))

(defun elfeed-search-buffer ()
  (get-buffer-create "*elfeed-search*"))

(defun elfeed-search--unload ()
  "Hook function for `elfeed-db-unload-hook'."
  (with-current-buffer (elfeed-search-buffer)
    ;; don't try to save the database in this case
    (remove-hook 'kill-buffer-hook #'elfeed-db-save t)
    (kill-buffer )))

(defun elfeed-search-format-date (date)
  "Format a date for printing in `elfeed-search-mode'.
The customization `elfeed-search-date-format' sets the formatting."
  (cl-destructuring-bind (format target alignment) elfeed-search-date-format
    (let* ((string (format-time-string format (seconds-to-time date)))
           (width (string-width string)))
      (cond
       ((> width target)
        (if (eq alignment :left)
            (substring string 0 target)
          (substring string (- width target) width)))
       ((< width target)
        (let ((pad (make-string (- target width) ?\s)))
          (if (eq alignment :left)
              (concat string pad)
            (concat pad string))))
       (string)))))

(defface elfeed-search-date-face
  '((((class color) (background light)) (:foreground "#aaa"))
    (((class color) (background dark))  (:foreground "#77a")))
  "Face used in search mode for dates."
  :group 'elfeed)

(defface elfeed-search-title-face
  '((((class color) (background light)) (:foreground "#000"))
    (((class color) (background dark))  (:foreground "#fff")))
  "Face used in search mode for titles."
  :group 'elfeed)

(defface elfeed-search-unread-title-face
  '((t :weight bold))
  "Face used in search mode for unread entry titles."
  :group 'elfeed)

(defface elfeed-search-feed-face
  '((((class color) (background light)) (:foreground "#aa0"))
    (((class color) (background dark))  (:foreground "#ff0")))
  "Face used in search mode for feed titles."
  :group 'elfeed)

(defface elfeed-search-tag-face
  '((((class color) (background light)) (:foreground "#070"))
    (((class color) (background dark))  (:foreground "#0f0")))
  "Face used in search mode for tags."
  :group 'elfeed)

(defface elfeed-search-last-update-face
  '((t))
  "Face for showing the date and time the database was last updated."
  :group 'elfeed)

(defface elfeed-search-unread-count-face
  '((((class color) (background light)) (:foreground "#000"))
    (((class color) (background dark))  (:foreground "#fff")))
  "Face used in search mode for unread entry titles."
  :group 'elfeed)

(defface elfeed-search-filter-face
  '((t :inherit mode-line-buffer-id))
  "Face for showing the current Elfeed search filter."
  :group 'elfeed)

(defcustom elfeed-search-title-max-width 70
  "Maximum column width for titles in the elfeed-search buffer."
  :group 'elfeed
  :type 'integer)

(defcustom elfeed-search-title-min-width 16
  "Minimum column width for titles in the elfeed-search buffer."
  :group 'elfeed
  :type 'integer)

(defcustom elfeed-search-trailing-width 30
  "Space reserved for displaying the feed and tag information."
  :group 'elfeed
  :type 'integer)

(defcustom elfeed-search-face-alist
  '((unread elfeed-search-unread-title-face))
  "Mapping of tags to faces in the Elfeed entry listing."
  :group 'elfeed
  :type '(alist :key-type symbol :value-type (repeat face)))

(defun elfeed-search--faces (tags)
  "Return all the faces that apply to an entry with TAGS."
  (nconc (cl-loop for (tag . faces) in elfeed-search-face-alist
                  when (memq tag tags)
                  append faces)
         (list 'elfeed-search-title-face)))

(defun elfeed-search-print-entry--default (entry)
  "Print ENTRY to the buffer."
  (let* ((date (elfeed-search-format-date (elfeed-entry-date entry)))
         (title (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed (elfeed-entry-feed entry))
         (feed-title
          (when feed
            (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         (tags (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str (mapconcat
                    (lambda (s) (propertize s 'face 'elfeed-search-tag-face))
                    tags ","))
         (title-width (- (window-width) 10 elfeed-search-trailing-width))
         (title-column (elfeed-format-column
                        title (elfeed-clamp
                               elfeed-search-title-min-width
                               title-width
                               elfeed-search-title-max-width)
                        :left)))
    (insert (propertize date 'face 'elfeed-search-date-face) " ")
    (insert (propertize title-column 'face title-faces 'kbd-help title) " ")
    (when feed-title
      (insert (propertize feed-title 'face 'elfeed-search-feed-face) " "))
    (when tags
      (insert "(" tags-str ")"))))

(defun elfeed-search-parse-filter (filter)
  "Parse the elements of a search filter into a plist."
  (let ((must-have ())
        (must-not-have ())
        (before nil)
        (after nil)
        (matches ())
        (not-matches ())
        (limit nil)
        (feeds ())
        (not-feeds ()))
    (cl-loop for element in (split-string filter)
             for type = (aref element 0)
             do (cl-case type
                  (?+
                   (let ((symbol (intern (substring element 1))))
                     (unless (eq '## symbol)
                       (push symbol must-have))))
                  (?-
                   (let ((symbol (intern (substring element 1))))
                     (unless (eq '## symbol)
                       (push symbol must-not-have))))
                  (?@ (cl-multiple-value-bind (a b)
                          (split-string (substring element 1) "--")
                        (let ((duration-a (elfeed-time-duration a))
                              (duration-b (and b (elfeed-time-duration b))))
                          (when (and duration-b (> duration-b duration-a))
                            (cl-rotatef duration-a duration-b))
                          (when duration-b (setf before duration-b))
                          (setf after duration-a))))
                  (?! (let ((re (substring element 1)))
                        (when (elfeed-valid-regexp-p re)
                          (push re not-matches))))
                  (?# (setf limit (string-to-number (substring element 1))))
                  (?= (let ((re (substring element 1)))
                        (when (elfeed-valid-regexp-p re)
                          (push re feeds))))
                  (?~ (let ((re (substring element 1)))
                        (when (elfeed-valid-regexp-p re)
                          (push re not-feeds))))
                  (otherwise (when (elfeed-valid-regexp-p element)
                               (push element matches)))))
    `(,@(when before
          (list :before before))
      ,@(when after
          (list :after after))
      ,@(when must-have
          (list :must-have must-have))
      ,@(when must-not-have
          (list :must-not-have must-not-have))
      ,@(when matches
          (list :matches matches))
      ,@(when not-matches
          (list :not-matches not-matches))
      ,@(when limit
          (list :limit limit))
      ,@(when feeds
          (list :feeds feeds))
      ,@(when not-feeds
          (list :not-feeds not-feeds)))))

(defun elfeed-search--recover-time (seconds)
  "Pick a reasonable filter representation for SECONDS."
  (let ((units '((60   1 "minute")
                 (60   1 "hour")
                 (24   1 "day")
                 (7    1 "week")
                 (30   7 "month")
                 (1461 120 "year")))
        (value (float seconds))
        (name "second"))
    (cl-loop for (n d unit) in units
             for next-value = (/ (* value d) n)
             when (< next-value 1.0)
             return t
             do (setf name unit
                      value next-value))
    (let ((count (format "%.4g" value)))
      (format "%s-%s%s-ago" count name (if (equal count "1") "" "s")))))

(defun elfeed-search--recover-units (after-seconds &optional before-seconds)
  "Stringify the age or optionally the date range specified by
AFTER-SECONDS and BEFORE-SECONDS."
  (apply 'concat "@"
          (elfeed-search--recover-time after-seconds)
          (when before-seconds
            (list "--"(elfeed-search--recover-time before-seconds)))))

(defun elfeed-search-unparse-filter (filter)
  "Inverse of `elfeed-search-parse-filter', returning a string.

The time (@n-units-ago) filter may not exactly match the
original, but will be equal in its effect."
  (let ((output ()))
    (cl-destructuring-bind (&key after     before
                                 must-have must-not-have
                                 matches   not-matches
                                 feeds     not-feeds
                                 limit &allow-other-keys)
        filter
      (when after
        (push (elfeed-search--recover-units after before) output))
      (dolist (tag must-have)
        (push (format "+%S" tag) output))
      (dolist (tag must-not-have)
        (push (format "-%S" tag) output))
      (dolist (re matches)
        (push re output))
      (dolist (re not-matches)
        (push (concat "!" re) output))
      (when limit
        (push (format "#%d" limit) output))
      (dolist (feed feeds)
        (push (format "=%s" feed) output))
      (dolist (feed not-feeds)
        (push (format "~%s" feed) output))
      (mapconcat #'identity (nreverse output) " "))))

(defun elfeed-search-filter (filter entry feed &optional count)
  "Return non-nil if ENTRY and FEED pass FILTER.

COUNT is the total number of entries collected so far, for
filtering against a limit filter (ex. #10).

See `elfeed-search-set-filter' for format/syntax documentation.
This function must *only* be called within the body of
`with-elfeed-db-visit' because it may perform a non-local exit."
  (cl-destructuring-bind (&key must-have must-not-have
                               matches   not-matches
                               feeds     not-feeds
                               after limit &allow-other-keys)
      filter
    (let* ((tags (elfeed-entry-tags entry))
           (date (elfeed-entry-date entry))
           (age (- (float-time) date))
           (title (or (elfeed-meta entry :title) (elfeed-entry-title entry)))
           (link (elfeed-entry-link entry))
           (feed-title
            (or (elfeed-meta feed :title) (elfeed-feed-title feed) ""))
           (feed-id (elfeed-feed-id feed)))
      (when (or (and after (> age after))
                (and limit (<= limit 0))
                (and limit count (>= count limit)))
        (elfeed-db-return))
      (and (cl-every  (lambda (tag) (memq tag tags)) must-have)
           (cl-notany (lambda (tag) (memq tag tags)) must-not-have)
           (or (null matches)
               (cl-every
                (lambda (m)
                  (or (and title      (string-match-p m title))
                      (and link       (string-match-p m link))))
                matches))
           (cl-notany (lambda (m)
                        (or (and title      (string-match-p m title))
                            (and link       (string-match-p m link))))
                      not-matches)
           (or (null feeds)
               (cl-some (lambda (f)
                          (or (string-match-p f feed-id)
                              (string-match-p f feed-title)))
                        feeds))
           (cl-notany (lambda (f)
                        (or (string-match-p f feed-id)
                            (string-match-p f feed-title)))
                      not-feeds)))))

(defun elfeed-search-compile-filter (filter)
  "Compile FILTER into a lambda function for `byte-compile'.

Executing a filter in bytecode form is generally faster than
\"interpreting\" the filter with `elfeed-search-filter'."
  (cl-destructuring-bind (&key after     before
                               must-have must-not-have
                               matches   not-matches
                               feeds     not-feeds
                               limit &allow-other-keys)
      filter
    `(lambda (,(if (or after matches not-matches must-have must-not-have)
                   'entry
                 '_entry)
              ,(if (or feeds not-feeds)
                   'feed
                 '_feed)
              ,(if limit
                   'count
                 '_count))
       (let* (,@(when after
                  '((date (elfeed-entry-date entry))
                    (age (- (float-time) date))))
              ,@(when (or must-have must-not-have)
                  '((tags (elfeed-entry-tags entry))))
              ,@(when (or matches not-matches)
                  '((title (or (elfeed-meta entry :title)
                               (elfeed-entry-title entry)))
                    (link (elfeed-entry-link entry))))
              ,@(when (or feeds not-feeds)
                  '((feed-id (elfeed-feed-id feed))
                    (feed-title (or (elfeed-meta feed :title)
                                    (elfeed-feed-title feed) "")))))
         ,@(when after
             `((when (> age ,after)
                 (elfeed-db-return))))
         ,@(when limit
             `((when (>= count ,limit)
                 (elfeed-db-return))))
         (and ,@(cl-loop for forbid in must-not-have
                         collect `(not (memq ',forbid tags)))
              ,@(cl-loop for forbid in must-have
                         collect `(memq ',forbid tags))
              ,@(cl-loop for regex in matches collect
                         `(or (string-match-p ,regex title)
                              (string-match-p ,regex link)))
              ,@(cl-loop for regex in not-matches collect
                         `(not
                           (or (string-match-p ,regex title)
                               (string-match-p ,regex link))))
              ,@(when feeds
                  `((or ,@(cl-loop
                           for regex in feeds
                           collect `(string-match-p ,regex feed-id)
                           collect `(string-match-p ,regex feed-title)))))
              ,@(when not-feeds
                  `((not
                     (or ,@(cl-loop
                            for regex in not-feeds
                            collect `(string-match-p ,regex feed-id)
                            collect `(string-match-p ,regex feed-title))))))
              ,@(when before
                  `((> age ,before))))))))

(defun elfeed-search--prompt (current)
  "Prompt for a new filter, starting with CURRENT."
  (read-from-minibuffer
   "Filter: "
   (if (or (string= "" current)
           (string-match-p " $" current))
       current
     (concat current " "))
   nil nil 'elfeed-search-filter-history))

(defun elfeed-search-clear-filter ()
  "Reset the search filter to the default value of `elfeed-search-filter'."
  (interactive)
  (setf elfeed-search-filter (default-value 'elfeed-search-filter))
  (elfeed-search-update--force))

(defun elfeed-search-set-filter (new-filter)
  "Set a new search filter for the elfeed-search buffer.

When NEW-FILTER is nil, reset the filter to the default value.

When given a prefix argument, the current filter is not displayed
in the minibuffer when prompting for a new filter.

Any component beginning with a + or - is treated as a tag. If +
the tag must be present on the entry. If - the tag must *not* be
present on the entry. Ex. \"+unread\" or \"+unread -comic\".

Any component beginning with an @ is an age limit or an age
range. If a limit, no posts older than this are allowed. If a
range, posts dates have to be inbetween the specified date
range. Examples:
- \"@3-days-ago\"
- \"@1-year-old\"
- \"@2019-06-24\"
- \"@2019-06-24--2019-06-24\"
- \"@5-days-ago--1-day-ago\"

Any component beginning with a # is an entry count maximum. The
number following # determines the maxiumum number of entries
to be shown (descending by date). Ex. \"#20\" or \"#100\".

Any component beginning with a = is a regular expression matching
the entry's feed (title or URL). Only entries belonging to a feed
that match at least one of the = expressions will be shown.

Every other space-seperated element is treated like a regular
expression, matching against entry link, title, and feed title."
  (interactive
   (let ((elfeed-search-filter-active :non-interactive))
     (list (elfeed-search--prompt
            (if current-prefix-arg "" elfeed-search-filter)))))
  (with-current-buffer (elfeed-search-buffer)
    (setf elfeed-search-filter
          (or new-filter (default-value 'elfeed-search-filter)))
    (elfeed-search-update :force)))

(defun elfeed-search--update-list ()
  "Update `elfeed-search-filter' list."
  (let* ((filter (elfeed-search-parse-filter elfeed-search-filter))
         (head (list nil))
         (tail head)
         (count 0))
    (if elfeed-search-compile-filter
        ;; Force lexical bindings regardless of the current
        ;; buffer-local value. Lexical scope uses the faster
        ;; stack-ref opcode instead of the traditional varref opcode.
        (let ((lexical-binding t)
              (func (byte-compile (elfeed-search-compile-filter filter))))
          (with-elfeed-db-visit (entry feed)
            (when (funcall func entry feed count)
              (setf (cdr tail) (list entry)
                    tail (cdr tail)
                    count (1+ count)))))
      (with-elfeed-db-visit (entry feed)
        (when (elfeed-search-filter filter entry feed count)
          (setf (cdr tail) (list entry)
                tail (cdr tail)
                count (1+ count)))))
    ;; Determine the final list order
    (let ((entries (cdr head)))
      (when elfeed-search-sort-function
        (setf entries (sort entries elfeed-search-sort-function)))
      (when (eq elfeed-sort-order 'ascending)
        (setf entries (nreverse entries)))
      (setf elfeed-search-entries
            entries))))

(defmacro elfeed-save-excursion (&rest body)
  "Like `save-excursion', but by entry/line/column instead of point."
  (declare (indent defun))
  `(let ((entry (elfeed-search-selected :single))
         (line (line-number-at-pos))
         (column (current-column)))
     (unwind-protect
         (progn ,@body)
       (let ((entry-position (cl-position entry elfeed-search-entries)))
         (elfeed-goto-line (if entry-position
                               (+ elfeed-search--offset entry-position)
                             line))
         (move-to-column column)))))

(defun elfeed-search-update (&optional force)
  "Update the elfeed-search buffer listing to match the database.
When FORCE is non-nil, redraw even when the database hasn't changed."
  (interactive)
  (with-current-buffer (elfeed-search-buffer)
    (when (or force (and (not elfeed-search-filter-active)
                         (< elfeed-search-last-update (elfeed-db-last-update))))
      (elfeed-save-excursion
        (let ((inhibit-read-only t)
              (standard-output (current-buffer)))
          (erase-buffer)
          (elfeed-search--update-list)
          (dolist (entry elfeed-search-entries)
            (funcall elfeed-search-print-entry-function entry)
            (insert "\n"))
          (setf elfeed-search-last-update (float-time))))
      (when (zerop (buffer-size))
        ;; If nothing changed, force a header line update
        (force-mode-line-update))
      (run-hooks 'elfeed-search-update-hook))))

(defun elfeed-search-fetch (prefix)
  "Update all feeds via `elfeed-update', or only visible feeds with PREFIX.
Given a prefix, this function becomes `elfeed-search-fetch-visible'."
  (interactive "P")
  (if prefix
      (elfeed-search-fetch-visible)
    (elfeed-update)))

(defun elfeed-search-fetch-visible ()
  "Update any feed with an entry currently displayed in the search buffer."
  (interactive)
  (cl-loop with seen = (make-hash-table :test 'equal)
           for entry in elfeed-search-entries
           for feed = (elfeed-entry-feed entry)
           for url = (elfeed-feed-url feed)
           when (not (gethash url seen))
           do (elfeed-update-feed (setf (gethash url seen) url))))

(defun elfeed-search-update-line (&optional n)
  "Redraw the current line."
  (let ((inhibit-read-only t))
    (save-excursion
      (when n (elfeed-goto-line n))
      (let ((entry (elfeed-search-selected :ignore-region)))
        (when entry
          (elfeed-kill-line)
          (funcall elfeed-search-print-entry-function entry))))))

(defun elfeed-search-update-entry (entry)
  "Redraw a specific entry."
  (let ((n (cl-position entry elfeed-search-entries)))
    (when n (elfeed-search-update-line (+ elfeed-search--offset n)))))

(defun elfeed-search-selected (&optional ignore-region-p)
  "Return a list of the currently selected feeds.

If IGNORE-REGION-P is non-nil, only return the entry under point."
  (let ((use-region (and (not ignore-region-p) (use-region-p))))
    (let ((start (if use-region (region-beginning) (point)))
          (end   (if use-region (region-end)       (point))))
      (cl-loop for line from (line-number-at-pos start)
               to (line-number-at-pos end)
               for offset = (- line elfeed-search--offset)
               when (and (>= offset 0) (nth offset elfeed-search-entries))
               collect it into selected
               finally (return (if ignore-region-p
                                   (car selected)
                                 selected))))))

(defun elfeed-search-browse-url (&optional use-generic-p)
  "Visit the current entry in your browser using `browse-url'.
If there is a prefix argument, visit the current entry in the
browser defined by `browse-url-generic-program'."
  (interactive "P")
  (let ((buffer (current-buffer))
        (entries (elfeed-search-selected)))
    (cl-loop for entry in entries
             do (elfeed-untag entry 'unread)
             when (elfeed-entry-link entry)
             do (if use-generic-p
                    (browse-url-generic it)
                  (browse-url it)))
    ;; `browse-url' could have switched to another buffer if eww or another
    ;; internal browser is used, but the remainder of the functions needs to
    ;; run in the elfeed buffer.
    (with-current-buffer buffer
      (mapc #'elfeed-search-update-entry entries)
      (unless (or elfeed-search-remain-on-entry (use-region-p))
        (forward-line)))))

(defun elfeed-search-yank ()
  "Copy the selected feed items to clipboard and kill-ring."
  (interactive)
  (let* ((entries (elfeed-search-selected))
         (links (mapcar #'elfeed-entry-link entries))
         (links-str (mapconcat #'identity links " ")))
    (when entries
      (elfeed-untag entries 'unread)
      (kill-new links-str)
      (if (fboundp 'gui-set-selection)
          (gui-set-selection elfeed-search-clipboard-type links-str)
        (with-no-warnings
          (x-set-selection elfeed-search-clipboard-type links-str)))
      (message "Copied: %s" links-str)
      (mapc #'elfeed-search-update-entry entries)
      (unless (or elfeed-search-remain-on-entry (use-region-p))
        (forward-line)))))

(defun elfeed-search-tag-all (tag)
  "Apply TAG to all selected entries."
  (interactive (list (intern (read-from-minibuffer "Tag: "))))
  (let ((entries (elfeed-search-selected)))
    (elfeed-tag entries tag)
    (mapc #'elfeed-search-update-entry entries)
    (unless (or elfeed-search-remain-on-entry (use-region-p))
      (forward-line))))

(defun elfeed-search-untag-all (tag)
  "Remove TAG from all selected entries."
  (interactive (list (intern (read-from-minibuffer "Tag: "))))
  (let ((entries (elfeed-search-selected)))
    (elfeed-untag entries tag)
    (mapc #'elfeed-search-update-entry entries)
    (unless (or elfeed-search-remain-on-entry (use-region-p))
      (forward-line))))

(defun elfeed-search-toggle-all (tag)
  "Toggle TAG on all selected entries."
  (interactive (list (intern (read-from-minibuffer "Tag: "))))
  (let ((entries (elfeed-search-selected)) entries-tag entries-untag)
    (cl-loop for entry in entries
             when (elfeed-tagged-p tag entry)
             do (push entry entries-untag)
             else do (push entry entries-tag))
    (elfeed-tag entries-tag tag)
    (elfeed-untag entries-untag tag)
    (mapc #'elfeed-search-update-entry entries)
    (unless (or elfeed-search-remain-on-entry (use-region-p))
      (forward-line))))

(defun elfeed-search-show-entry (entry)
  "Display the currently selected item in a buffer."
  (interactive (list (elfeed-search-selected :ignore-region)))
  (require 'elfeed-show)
  (when (elfeed-entry-p entry)
    (elfeed-untag entry 'unread)
    (elfeed-search-update-entry entry)
    (unless elfeed-search-remain-on-entry (forward-line))
    (elfeed-show-entry entry)))

(defun elfeed-search-set-entry-title (title)
  "Manually set the title for the entry under point.
Sets the :title key of the entry's metadata. See `elfeed-meta'."
  (interactive "sTitle: ")
  (let ((entry (elfeed-search-selected :ignore-region)))
    (unless entry
      (error "No entry selected!"))
    (setf (elfeed-meta entry :title) title)
    (elfeed-search-update-entry entry)))

(defun elfeed-search-set-feed-title (title)
  "Manually set the title for the feed belonging to the entry under point.
Sets the :title key of the feed's metadata. See `elfeed-meta'."
  (interactive "sTitle: ")
  (let ((entry (elfeed-search-selected :ignore-region)))
    (unless entry
      (error "No entry selected!"))
    (let ((feed (elfeed-entry-feed entry)))
      (setf (elfeed-meta feed :title) title)
      (dolist (to-fix elfeed-search-entries)
        (elfeed-search-update-entry to-fix)))))

;; Live Filters

(defvar elfeed-search-filter-syntax-table
  (let ((table (make-syntax-table)))
    (prog1 table
      (modify-syntax-entry ?+ "w" table)
      (modify-syntax-entry ?- "w" table)
      (modify-syntax-entry ?= "w" table)
      (modify-syntax-entry ?@ "w" table)))
  "Syntax table active when editing the filter in the minibuffer.")

(defun elfeed-search--minibuffer-setup ()
  "Set up the minibuffer for live filtering."
  (when elfeed-search-filter-active
    (set-syntax-table elfeed-search-filter-syntax-table)
    (when (eq :live elfeed-search-filter-active)
      (add-hook 'post-command-hook 'elfeed-search--live-update nil :local))))

(add-hook 'minibuffer-setup-hook 'elfeed-search--minibuffer-setup)

(defun elfeed-search--live-update ()
  "Update the elfeed-search buffer based on the contents of the minibuffer."
  (when (eq :live elfeed-search-filter-active)
    (let ((buffer (elfeed-search-buffer))
          (current-filter (minibuffer-contents-no-properties)))
      (when buffer
        (with-current-buffer buffer
          (let* ((window (get-buffer-window (elfeed-search-buffer)))
                 (height (window-total-height window))
                 (limiter (if window
                              (format "#%d " height)
                            "#1 "))
                 (elfeed-search-filter (concat limiter current-filter)))
            (elfeed-search-update :force)
            (setf elfeed-search-filter-overflowing
                  (= (length elfeed-search-entries)
                     height))))))))

(defun elfeed-search-live-filter ()
  "Filter the elfeed-search buffer as the filter is written."
  (interactive)
  (unwind-protect
      (let ((elfeed-search-filter-active :live))
        (setq elfeed-search-filter
              (read-from-minibuffer "Filter: " elfeed-search-filter)))
    (elfeed-search-update :force)))

;; Bookmarks

;;;###autoload
(defun elfeed-search-bookmark-handler (record)
  "Jump to an elfeed-search bookmarked location."
  (elfeed)
  (elfeed-search-set-filter (bookmark-prop-get record 'location)))

(defun elfeed-search-bookmark-make-record ()
  "Return a bookmark record for the current elfeed-search buffer."
  (let* ((filter (elfeed-search-parse-filter elfeed-search-filter))
         (tags (plist-get filter :must-have)))
    `(,(format "elfeed %s" elfeed-search-filter)
      (location . ,elfeed-search-filter)
      (tags ,@(mapcar #'symbol-name tags))
      (handler . elfeed-search-bookmark-handler))))

;; Desktop Save

(defun elfeed-search-desktop-save (_desktop-dirname)
  "Save the state of the current elfeed-search buffer so that it
  may be restored as part of a saved desktop. Also save the state
  of the db for when `desktop-auto-save-timeout' is enabled."
  (elfeed-db-save)
  elfeed-search-filter)

;;;###autoload
(defun elfeed-search-desktop-restore (_file-name _buffer-name search-filter)
  "Restore the state of an elfeed-search buffer on desktop restore."
  (elfeed)
  (elfeed-search-set-filter search-filter)
  (current-buffer))

;;;###autoload
(add-to-list 'desktop-buffer-mode-handlers
             '(elfeed-search-mode . elfeed-search-desktop-restore))

(provide 'elfeed-search)

;;; elfeed-search.el ends here
