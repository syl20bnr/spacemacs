;;; elfeed-db.el --- database and model for elfeed -*- lexical-binding: t; -*-

;; This is free and unencumbered software released into the public domain.

;;; Commentary:

;; Elfeed is aware of two type of things: feeds and entries. All dates
;; are stored as floating point epoch seconds.

;; Feeds are keyed by their user-provided feed URL, which acts as the
;; feed identity regardless of any other stated identity. Feeds have a
;; list of entries.

;; Entries are keyed in order of preference by id (Atom), guid (RSS),
;; or link. To avoid circular references, entries refer to their
;; parent feeds by URL.

;; Feed content is stored in a content-addressable loose-file
;; database, very similar to an unpacked Git object database. Entries
;; have references to items in this database (elfeed-ref), keeping the
;; actual entry struct memory footprint small. Most importantly, this
;; keeps the core index small so that it can quickly be written as a
;; whole to the filesystem. The wire format is just the s-expression
;; print form of the top-level hash table.

;; The database can be compacted into a small number of compressed
;; files with the interactive function `elfeed-db-compact'. This could
;; be used as a kill-emacs hook.

;; An AVL tree containing all database entries ordered by date is
;; maintained as part of the database. We almost always want to look
;; at entries ordered by date and this step accomplished that very
;; efficiently with the AVL tree. This is the reasoning behind the
;; `with-elfeed-db-visit' interface.

;; Unfortunately there's a nasty bug (bug#15190) in the reader that
;; makes hash tables and `print-circle' incompatible. It's been fixed
;; in trunk, but many users will likely be stuck with this bug for the
;; next few years. This means the database format can't exploit
;; circular references.

;; Entry and feed objects can have arbitrary metadata attached,
;; automatically stored in the database. The setf-able `elfeed-meta'
;; function is used to access these.

;;; Code:

(require 'cl-lib)
(require 'avl-tree)
(require 'elfeed-lib)

(defcustom elfeed-db-directory "~/.elfeed"
  "Directory where elfeed will store its database."
  :group 'elfeed
  :type 'directory)

(defvar elfeed-db nil
  "The core database for elfeed.")

(defvar elfeed-db-feeds nil
  "Feeds hash table, part of `elfeed-db'.")

(defvar elfeed-db-entries nil
  "Entries hash table, part of `elfeed-db'.")

(defvar elfeed-db-index nil
  "Collection of all entries sorted by date, part of `elfeed-db'.")

(defvar elfeed-db-version
  ;; If records are avaiable (Emacs 26), use the newer database format
  (if (functionp 'record)
      4
    "0.0.3")
  "The database version this version of Elfeed expects to use.")

(defvar elfeed-new-entry-hook ()
  "Functions in this list are called with the new entry as its argument.
This is a chance to add custom tags to new entries.")

(defvar elfeed-db-update-hook ()
  "Functions in this list are called with no arguments any time
the :last-update time is updated.")

(defvar elfeed-db-unload-hook ()
  "Hook to run immediately after `elfeed-db-unload'.")

;; Data model:

(cl-defstruct (elfeed-feed (:constructor elfeed-feed--create))
  "A web feed, contains elfeed-entry structs."
  id url title author meta)

(cl-defstruct (elfeed-entry (:constructor elfeed-entry--create))
  "A single entry from a feed, normalized towards Atom."
  id title link date content content-type enclosures tags feed-id meta)

(defun elfeed-entry-merge (a b)
  "Merge B into A, preserving A's tags. Return true if an actual
update occurred, not counting content."
  (setf (elfeed-entry-tags b) (elfeed-entry-tags a)
        (elfeed-entry-content a) (elfeed-entry-content b))
  (cl-loop for (key value) on (elfeed-entry-meta b) by #'cddr
           do (setf (elfeed-entry-meta a)
                    (plist-put (elfeed-entry-meta a) key value)))
  (not
   (zerop
    (cl-loop for i from 1 below (1- (length a))
             for part-a = (aref a i)
             for part-b = (aref b i)
             count (not (equal part-a part-b))
             do (setf (aref a i) part-b)))))

(defun elfeed-db-get-feed (id)
  "Get/create the feed for ID."
  (elfeed-db-ensure)
  (let ((feed (gethash id elfeed-db-feeds)))
    (or feed
        (setf (gethash id elfeed-db-feeds)
              (elfeed-feed--create :id id)))))

(defun elfeed-db-get-entry (id)
  "Get the entry for ID."
  (elfeed-db-ensure)
  (gethash id elfeed-db-entries))

(defun elfeed-db-compare (a b)
  "Return true if entry A is newer than entry B."
  (let* ((entry-a (elfeed-db-get-entry a))
         (entry-b (elfeed-db-get-entry b))
         (date-a (elfeed-entry-date entry-a))
         (date-b (elfeed-entry-date entry-b)))
    (if (= date-a date-b)
        (string< (prin1-to-string b) (prin1-to-string a))
      (> date-a date-b))))

(defun elfeed-db-set-update-time ()
  "Update the database last-update time."
  (setf elfeed-db (plist-put elfeed-db :last-update (float-time)))
  (run-hooks 'elfeed-db-update-hook))

(defun elfeed-db-add (entries)
  "Add ENTRIES to the database."
  (elfeed-db-ensure)
  (cl-loop for entry in entries
           for id = (elfeed-entry-id entry)
           for original = (gethash id elfeed-db-entries)
           for new-date = (elfeed-entry-date entry)
           for original-date = (and original (elfeed-entry-date original))
           do (elfeed-deref-entry entry)
           when original count
           (if (= new-date original-date)
               (elfeed-entry-merge original entry)
             (avl-tree-delete elfeed-db-index id)
             (prog1 (elfeed-entry-merge original entry)
               (avl-tree-enter elfeed-db-index id)))
           into change-count
           else count
           (setf (gethash id elfeed-db-entries) entry)
           into change-count
           and do
           (progn
             (avl-tree-enter elfeed-db-index id)
             (cl-loop for hook in elfeed-new-entry-hook
                      do (funcall hook entry)))
           finally
           (unless (zerop change-count)
             (elfeed-db-set-update-time)))
  :success)

(defun elfeed-entry-feed (entry)
  "Get the feed struct for ENTRY."
  (elfeed-db-get-feed (elfeed-entry-feed-id entry)))

(defun elfeed-normalize-tags (tags &rest more-tags)
  "Return the normalized tag list for TAGS."
  (let ((all (apply #'append tags (nconc more-tags (list ())))))
    (cl-delete-duplicates (cl-sort all #'string< :key #'symbol-name))))

(defun elfeed-tag-1 (entry &rest tags)
  "Add TAGS to ENTRY."
  (let ((current (elfeed-entry-tags entry)))
    (setf (elfeed-entry-tags entry)
          (elfeed-normalize-tags (append tags current)))))

(defun elfeed-untag-1 (entry &rest tags)
  "Remove TAGS from ENTRY."
  (setf (elfeed-entry-tags entry)
        (cl-loop for tag in (elfeed-entry-tags entry)
                 unless (memq tag tags) collect tag)))

(defun elfeed-tag (entry-or-entry-list &rest tags)
  "Add TAGS to ENTRY-OR-ENTRY-LIST and run `elfeed-tag-hooks'."
  (let* ((entries (if (elfeed-entry-p entry-or-entry-list)
                      (list entry-or-entry-list)
                    entry-or-entry-list)))
    (run-hook-with-args 'elfeed-tag-hooks entries tags)
    (cl-loop for entry in entries do (apply #'elfeed-tag-1 entry tags))))

(defun elfeed-untag (entry-or-entry-list &rest tags)
  "Remove TAGS from ENTRY-OR-ENTRY-LIST and run `elfeed-untag-hooks'."
  (let* ((entries (if (elfeed-entry-p entry-or-entry-list)
                      (list entry-or-entry-list)
                    entry-or-entry-list)))
    (run-hook-with-args 'elfeed-untag-hooks entries tags)
    (cl-loop for entry in entries do (apply #'elfeed-untag-1 entry tags))))

(defun elfeed-tagged-p (tag entry)
  "Return true if ENTRY is tagged by TAG."
  (memq tag (elfeed-entry-tags entry)))

(defun elfeed-db-last-update ()
  "Return the last database update time in (`float-time') seconds."
  (elfeed-db-ensure)
  (or (plist-get elfeed-db :last-update) 0))

(defmacro with-elfeed-db-visit (entry-and-feed &rest body)
  "Visit each entry in the database from newest to oldest.
Use `elfeed-db-return' to exit early and optionally return data.

  (with-elfeed-db-visit (entry feed)
    (do-something entry)
    (when (some-date-criteria-p entry)
      (elfeed-db-return)))"
  (declare (indent defun))
  `(catch 'elfeed-db-done
     (prog1 nil
       (elfeed-db-ensure)
       (avl-tree-mapc
        (lambda (id)
          (let* ((,(cl-first entry-and-feed) (elfeed-db-get-entry id))
                 (,(cl-second entry-and-feed)
                  (elfeed-entry-feed ,(cl-first entry-and-feed))))
            ,@body))
        elfeed-db-index))))

(defun elfeed-feed-entries (feed-or-id)
  "Return a list of all entries for a particular feed.
The FEED-OR-ID may be a feed struct or a feed ID (url)."
  (let ((feed-id (if (elfeed-feed-p feed-or-id)
                     (elfeed-feed-id feed-or-id)
                   feed-or-id)))
    (let ((entries))
      (with-elfeed-db-visit (entry feed)
        (when (equal (elfeed-feed-id feed) feed-id)
          (push entry entries)))
      (nreverse entries))))

(defun elfeed-apply-hooks-now ()
  "Apply `elfeed-new-entry-hook' to all entries in the database."
  (interactive)
  (with-elfeed-db-visit (entry _)
    (cl-loop for hook in elfeed-new-entry-hook
             do (funcall hook entry))))

(defmacro elfeed-db-return (&optional value)
  "Use this to exit early and return VALUE from `with-elfeed-db-visit'."
  `(throw 'elfeed-db-done ,value))

(defun elfeed-db-get-all-tags ()
  "Return a list of all tags currently in the database."
  (let ((table (make-hash-table :test 'eq)))
    (with-elfeed-db-visit (e _)
      (dolist (tag (elfeed-entry-tags e))
        (setf (gethash tag table) tag)))
    (let ((tags ()))
      (maphash (lambda (k _) (push k tags)) table)
      (cl-sort tags #'string< :key #'symbol-name))))

;; Saving and Loading:

(defun elfeed-db-save ()
  "Write the database index to the filesystem."
  (elfeed-db-ensure)
  (setf elfeed-db (plist-put elfeed-db :version elfeed-db-version))
  (mkdir elfeed-db-directory t)
  (let ((coding-system-for-write 'utf-8))
    (with-temp-file (expand-file-name "index" elfeed-db-directory)
      (let ((standard-output (current-buffer))
            (print-level nil)
            (print-length nil)
            (print-circle nil))
        (princ (format ";;; Elfeed Database Index (version %s)\n\n"
                       elfeed-db-version))
        (when (eql elfeed-db-version 4)
          ;; Put empty dummy index in front
          (princ ";; Dummy index for backwards compatablity:\n")
          (prin1 (elfeed-db--dummy))
          (princ "\n\n;; Real index:\n"))
        (prin1 elfeed-db)
        :success))))

(defun elfeed-db-save-safe ()
  "Run `elfeed-db-save' without triggering any errors, for use as a safe hook."
  (ignore-errors (elfeed-db-save)))

(defun elfeed-db-upgrade (db)
  "Upgrade the database from a previous format."
  (if (not (vectorp (plist-get db :index)))
      db  ; Database is already in record format
    (let* ((new-db (elfeed-db--empty))
           ;; Dynamically bind for other functions
           (elfeed-db-feeds (plist-get new-db :feeds))
           (elfeed-db-entries (plist-get new-db :entries))
           (elfeed-db-index (plist-get new-db :index)))
      ;; Fix up feeds
      (cl-loop with table = (plist-get new-db :feeds)
               for feed hash-values of (plist-get db :feeds)
               for id = (aref feed 1)
               for fixed = (elfeed-feed--create
                            :id id
                            :url (aref feed 2)
                            :title (aref feed 3)
                            :author (aref feed 4)
                            :meta (aref feed 5))
               do (setf (gethash id table) fixed))
      ;; Fix up entries
      (cl-loop with table = (plist-get new-db :entries)
               with index = (plist-get new-db :index)
               for entry hash-values of (plist-get db :entries)
               for id = (aref entry 1)
               for content = (aref entry 5)
               for fixed = (elfeed-entry--create
                            :id id
                            :title (aref entry 2)
                            :link (aref entry 3)
                            :date (aref entry 4)
                            :content (if (vectorp content)
                                         (elfeed-ref--create
                                          :id (aref content 1))
                                       content)
                            :content-type (aref entry 6)
                            :enclosures (aref entry 7)
                            :tags (aref entry 8)
                            :feed-id (aref entry 9)
                            :meta (aref entry 10))
               do (setf (gethash id table) fixed)
               do (avl-tree-enter index id))
      (plist-put new-db :last-update (plist-get db :last-update)))))

(defun elfeed-db--empty ()
  "Create an empty database object."
  `(:version ,elfeed-db-version
    :feeds ,(make-hash-table :test 'equal)
    :entries ,(make-hash-table :test 'equal)
    ;; Compiler may warn about this (bug#15327):
    :index ,(avl-tree-create #'elfeed-db-compare)))

(defun elfeed-db--dummy ()
  "Create an empty dummy database for Emacs 25 and earlier."
  (list :version "0.0.3"
        :feeds #s(hash-table size 65
                             test equal
                             rehash-size 1.5
                             rehash-threshold 0.8
                             data ())
        :entries #s(hash-table size 65
                               test equal
                               rehash-size 1.5
                               rehash-threshold 0.8
                               data ())
        :index [cl-struct-avl-tree- [nil nil nil 0] elfeed-db-compare]))

;; To cope with the incompatible struct changes in Emacs 26, Elfeed
;; uses version 4 of the database format when run under Emacs 26. This
;; version saves a dummy, empty index in front of the real database. A
;; user going from Emacs 26 to Emacs 25 will quietly load an empty
;; index since it's unreasonable to downgrade (would require rewriting
;; the Emacs reader from scratch).

(defun elfeed-db-load ()
  "Load the database index from the filesystem."
  (let ((index (expand-file-name "index" elfeed-db-directory))
        (enable-local-variables nil)) ; don't set local variables from index!
    (if (not (file-exists-p index))
        (setf elfeed-db (elfeed-db--empty))
      ;; Override the default value for major-mode. There is no
      ;; preventing find-file-noselect from starting the default major
      ;; mode while also having it handle buffer conversion. Some
      ;; major modes crash Emacs when enabled in large buffers (e.g.
      ;; org-mode). This includes the Elfeed index, so we must not let
      ;; this happen.
      (cl-letf (((default-value 'major-mode) 'fundamental-mode))
        (with-current-buffer (find-file-noselect index :nowarn)
          (goto-char (point-min))
          (if (eql elfeed-db-version 4)
              ;; May need to skip over dummy database
              (let ((db-1 (read (current-buffer)))
                    (db-2 (ignore-errors (read (current-buffer)))))
                (setf elfeed-db (or db-2 db-1)))
            ;; Just load first database
            (setf elfeed-db (read (current-buffer))))
          (kill-buffer))))
    ;; Perform an upgrade if necessary and possible
    (unless (equal (plist-get elfeed-db :version) elfeed-db-version)
      (ignore-errors
        (copy-file index (concat index ".backup")))
      (message "Upgrading Elfeed index for Emacs 26 ...")
      (setf elfeed-db (elfeed-db-upgrade elfeed-db))
      (message "Elfeed index upgrade complete."))
    (setf elfeed-db-feeds (plist-get elfeed-db :feeds)
          elfeed-db-entries (plist-get elfeed-db :entries)
          elfeed-db-index (plist-get elfeed-db :index)
          ;; Internal function use required for security!
          (avl-tree--cmpfun elfeed-db-index) #'elfeed-db-compare)))

(defun elfeed-db-unload ()
  "Unload the database so that it can be operated on externally.

Runs `elfeed-db-unload-hook' after unloading the database."
  (interactive)
  (elfeed-db-save)
  (setf elfeed-db nil
        elfeed-db-feeds nil
        elfeed-db-entries nil
        elfeed-db-index nil)
  (run-hooks 'elfeed-db-unload-hook))

(defun elfeed-db-ensure ()
  "Ensure that the database has been loaded."
  (when (null elfeed-db) (elfeed-db-load)))

(defun elfeed-db-size ()
  "Return a count of the number of entries in the database."
  (let ((count-table (hash-table-count elfeed-db-entries))
        (count-tree (avl-tree-size elfeed-db-index)))
    (if (= count-table count-tree)
        count-table
      (error "Elfeed database error: entry count mismatch."))))

;; Metadata:

(defun elfeed-meta--plist (thing)
  "Get the metadata plist for THING."
  (cl-typecase thing
    (elfeed-feed  (elfeed-feed-meta  thing))
    (elfeed-entry (elfeed-entry-meta thing))
    (otherwise (error "Don't know how to access metadata on %S" thing))))

(defun elfeed-meta--set-plist (thing plist)
  "Set the metadata plist on THING to PLIST."
  (cl-typecase thing
    (elfeed-feed  (setf (elfeed-feed-meta thing) plist))
    (elfeed-entry (setf (elfeed-entry-meta thing) plist))
    (otherwise (error "Don't know how to access metadata on %S" thing))))

(defun elfeed-db--plist-fixup (plist)
  "Remove nil values from PLIST."
  (cl-loop for (k v) on plist by #'cddr
           when (not (null v))
           collect k and collect v))

(defun elfeed-meta (thing key &optional default)
  "Access metadata for THING (entry, feed) under KEY."
  (or (plist-get (elfeed-meta--plist thing) key)
      default))

(defun elfeed-meta--put (thing key value)
  "Set metadata to VALUE on THING under KEY."
  (when (not (elfeed-readable-p value)) (error "New value must be readable."))
  (let ((new-plist (plist-put (elfeed-meta--plist thing) key value)))
    (prog1 value
      (elfeed-meta--set-plist thing (elfeed-db--plist-fixup new-plist)))))

(gv-define-setter elfeed-meta (value thing key &optional _default)
  `(elfeed-meta--put ,thing ,key ,value))

;; Filesystem storage:

(defvar elfeed-ref-archive nil
  "Index of archived/packed content.")

(defvar elfeed-ref-cache nil
  "Temporary storage of the full archive content.")

(cl-defstruct (elfeed-ref (:constructor elfeed-ref--create))
  id)

(defun elfeed-ref--file (ref)
  "Determine the storage filename for REF."
  (let* ((id (elfeed-ref-id ref))
         (root (expand-file-name "data" elfeed-db-directory))
         (subdir (expand-file-name (substring id 0 2) root)))
    (expand-file-name id subdir)))

(cl-defun elfeed-ref-archive-filename (&optional (suffix ""))
  "Return the base filename of the archive files."
  (concat (expand-file-name "data/archive" elfeed-db-directory) suffix))

(defun elfeed-ref-archive-load ()
  "Load the archived ref index."
  (let ((archive-index (elfeed-ref-archive-filename ".index")))
    (if (file-exists-p archive-index)
        (with-temp-buffer
          (insert-file-contents archive-index)
          (setf elfeed-ref-archive (read (current-buffer))))
      (setf elfeed-ref-archive :empty))))

(defun elfeed-ref-archive-ensure ()
  "Ensure that the archive index is loaded."
  (when (null elfeed-ref-archive) (elfeed-ref-archive-load)))

(defun elfeed-ref-exists-p (ref)
  "Return true if REF can be dereferenced."
  (elfeed-ref-archive-ensure)
  (or (and (hash-table-p elfeed-ref-archive)
           (not (null (gethash (elfeed-ref-id ref) elfeed-ref-archive))))
      (file-exists-p (elfeed-ref--file ref))))

(defun elfeed-deref (ref)
  "Fetch the content behind the reference, or nil if non-existent."
  (elfeed-ref-archive-ensure)
  (if (not (elfeed-ref-p ref))
      ref
    (let ((index (and (hash-table-p elfeed-ref-archive)
                      (gethash (elfeed-ref-id ref) elfeed-ref-archive)))
          (archive-file (elfeed-ref-archive-filename ".gz"))
          (coding-system-for-read 'utf-8))
      (if (and index (file-exists-p archive-file))
          (progn
            (when (null elfeed-ref-cache)
              (with-temp-buffer
                (insert-file-contents archive-file)
                (setf elfeed-ref-cache (buffer-string)))
              ;; Clear cache on next turn.
              (run-at-time 0 nil (lambda () (setf elfeed-ref-cache nil))))
            (substring elfeed-ref-cache (car index) (cdr index)))
        (let ((file (elfeed-ref--file ref)))
          (when (file-exists-p file)
            (with-temp-buffer
              (insert-file-contents file)
              (buffer-string))))))))

(defun elfeed-ref (content)
  "Create a reference to CONTENT, to be persistently stored."
  (if (elfeed-ref-p content)
      content
    (let* ((id (secure-hash 'sha1 (encode-coding-string content 'utf-8 t)))
           (ref (elfeed-ref--create :id id))
           (file (elfeed-ref--file ref)))
      (prog1 ref
        (unless (elfeed-ref-exists-p ref)
          (mkdir (file-name-directory file) t)
          (let ((coding-system-for-write 'utf-8)
                ;; Content data loss is a tolerable risk.
                ;; Fsync will occur soon on index write anyway.
                (write-region-inhibit-fsync t))
            (with-temp-file file
              (insert content))))))))

(defun elfeed-deref-entry (entry)
  "Move ENTRY's content to filesystem storage. Return the entry."
  (let ((content (elfeed-entry-content entry)))
    (prog1 entry
      (when (stringp content)
        (setf (elfeed-entry-content entry) (elfeed-ref content))))))

(defun elfeed-ref-delete (ref)
  "Remove the content behind REF from the database."
  (ignore-errors
    (delete-file (elfeed-ref--file ref))))

(defun elfeed-db-gc-empty-feeds ()
  "Remove feeds with no entries from the database."
  (let ((seen (make-hash-table :test 'equal)))
    (with-elfeed-db-visit (entry feed)
      (setf (gethash (elfeed-feed-id feed) seen) feed))
    (maphash (lambda (id _)
               (unless (gethash id seen)
                 (remhash id elfeed-db-feeds)))
             elfeed-db-feeds)))

(defun elfeed-db-gc (&optional stats-p)
  "Clean up unused content from the content database.
If STATS is true, return the space cleared in bytes."
  (elfeed-db-gc-empty-feeds)
  (let* ((data (expand-file-name "data" elfeed-db-directory))
         (dirs (directory-files data t "^[0-9a-z]\\{2\\}$"))
         (ids (cl-mapcan (lambda (d) (directory-files d nil nil t)) dirs))
         (table (make-hash-table :test 'equal)))
    (dolist (id ids)
      (setf (gethash id table) nil))
    (with-elfeed-db-visit (entry _)
      (let ((content (elfeed-entry-content entry)))
        (when (elfeed-ref-p content)
          (setf (gethash (elfeed-ref-id content) table) t))))
    (cl-loop for id hash-keys of table using (hash-value used)
             for used-p = (or used (member id '("." "..")))
             when (and (not used-p) stats-p)
             sum (let* ((ref (elfeed-ref--create :id id))
                        (file (elfeed-ref--file ref)))
                   (* 1.0 (nth 7 (file-attributes file))))
             unless used-p
             do (elfeed-ref-delete (elfeed-ref--create :id id))
             finally (cl-loop for dir in dirs
                              when (elfeed-directory-empty-p dir)
                              do (delete-directory dir)))))

(defun elfeed-db-pack ()
  "Pack all content into a single archive for efficient storage."
  (let ((coding-system-for-write 'utf-8)
        (next-archive (make-hash-table :test 'equal))
        (packed ()))
    (make-directory (expand-file-name "data" elfeed-db-directory) t)
    (with-temp-file (elfeed-ref-archive-filename ".gz")
      (with-elfeed-db-visit (entry _)
        (let ((ref (elfeed-entry-content entry))
              (start (1- (point))))
          (when (elfeed-ref-p ref)
            (let ((content (elfeed-deref ref)))
              (when content
                (push ref packed)
                (insert content)
                (setf (gethash (elfeed-ref-id ref) next-archive)
                      (cons start (1- (point))))))))))
    (with-temp-file (elfeed-ref-archive-filename ".index")
      (let ((standard-output (current-buffer))
            (print-level nil)
            (print-length nil)
            (print-circle nil))
        (prin1 next-archive)))
    (setf elfeed-ref-cache nil)
    (setf elfeed-ref-archive next-archive)
    (mapc #'elfeed-ref-delete packed)
    :success))

(defun elfeed-db-compact ()
  "Minimize the Elfeed database storage size on the filesystem.
This requires that auto-compression-mode can handle
gzip-compressed files, so the gzip program must be in your PATH."
  (interactive)
  (unless (elfeed-gzip-supported-p)
    (error "aborting compaction: gzip auto-compression-mode unsupported"))
  (elfeed-db-pack)
  (elfeed-db-gc))

(defun elfeed-db-gc-safe ()
  "Run `elfeed-db-gc' without triggering any errors, for use as a safe hook."
  (ignore-errors (elfeed-db-gc)))

(unless noninteractive
  (add-hook 'kill-emacs-hook #'elfeed-db-gc-safe :append)
  (add-hook 'kill-emacs-hook #'elfeed-db-save-safe))

(provide 'elfeed-db)

;;; elfeed-db.el ends here
