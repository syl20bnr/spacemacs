;;; elfeed-csv.el --- export database to CSV files -*- lexical-binding: t; -*-

;;; Commentary:

;; The `elfeed-csv-export' docstring has a SQL schema recommendation.
;; Given these schemas, these CSV files are trivially imported into a
;; SQLite database using the sqlite3 command line program:

;;   sqlite> .mode csv
;;   sqlite> .import feeds.csv feeds
;;   sqlite> .import entries.csv entries
;;   sqlite> .import tags.csv tags

;; Note: nil values won't be imported as NULL, but as empty strings.

;; Here are a few interesting queries to make on your own data:

;; For each tag in your database, compute a histogram of posts with
;; 1-hour bins across the the day (0-23), in your local timezone.

;; SELECT tag,
;;        cast(strftime('%H', date, 'unixepoch', 'localtime') AS INT) AS hour,
;;        count(id) AS count
;; FROM entries
;; JOIN tags ON tags.entry = entries.id AND tags.feed = entries.feed
;; GROUP BY tag, hour;

;; Like above, but per week-day (0-6).

;; SELECT tag,
;;        cast(strftime('%w', date, 'unixepoch', 'localtime') AS INT) AS day,
;;        count(id) AS count
;; FROM entries
;; JOIN tags ON tags.entry = entries.id AND tags.feed = entries.feed
;; GROUP BY tag, day;

;; For each feed, compute the number of entries and last entry date.

;; SELECT feeds.title AS title,
;;        count(url) AS entry_count,
;;        datetime(max(date), 'unixepoch') AS last_entry_date
;; FROM feeds
;; JOIN entries ON feeds.url = entries.feed
;; GROUP BY url
;; ORDER BY max(date) DESC;

;; Compute a histogram of entry title lengths.

;; SELECT length(title) AS length,
;;        count(*) AS count
;; FROM entries
;; GROUP BY length
;; ORDER BY length;

;; Again, but this time group by tag.

;; SELECT tag,
;;        length(title) AS length,
;;        count(*) AS count
;; FROM entries
;; JOIN tags ON tags.entry = entries.id AND tags.feed = entries.feed
;; GROUP BY tag, length
;; ORDER BY length;

;; What's the relationship between title length and time of day of an
;; entry? (Scatter plot this result.)

;; SELECT (date % (24*60*60)) / (24*60*60) AS day_time,
;;        length(title) AS length
;; FROM entries
;; JOIN tags ON tags.entry = entries.id AND tags.feed = entries.feed;

;;; Code:

(require 'cl-lib)
(require 'elfeed-db)

(defvar elfeed-csv-nil ""
  "The string representation to use for nil.
Consider let-binding this around your `elfeed-csv-quote' call.")

(defun elfeed-csv-quote (sexp)
  "Return CSV string representation of SEXP."
  (cond ((null sexp)
         elfeed-csv-nil)
        ((not (stringp sexp))
         (elfeed-csv-quote (prin1-to-string sexp)))
        ((string-match-p "[\"\n,]" sexp)
         (concat "\"" (replace-regexp-in-string "\"" "\"\"" sexp) "\""))
        (sexp)))

(defun elfeed-csv-insert (seq)
  "Insert a row of CSV data to the current buffer."
  (cl-loop for value being the elements of seq
           for column upfrom 0
           when (> column 0)
           do (insert ",")
           do (insert (elfeed-csv-quote value))
           finally (newline)))

(cl-defun elfeed-csv-export (feeds-file entries-file tags-file &key headers-p)
  "Create separate CSV files for feeds, entries, and tags.

These CSV files are intended for an analysis of an Elfeed
database. They are suitable for importing as tables into a
relational database such as SQLite. Here's the recommended SQL
schema, reflecting the structure of the data.

CREATE TABLE feeds (
    url TEXT PRIMARY KEY,
    title TEXT,
    canonical_url TEXT,
    author TEXT
);

CREATE TABLE entries (
    id TEXT NOT NULL,
    feed TEXT NOT NULL REFERENCES feeds (url),
    title TEXT,
    link TEXT NOT NULL,
    date REAL NOT NULL,
    PRIMARY KEY (id, feed)
);

CREATE TABLE tags (
    entry TEXT NOT NULL,
    feed TEXT NOT NULL,
    tag TEXT NOT NULL,
    FOREIGN KEY (entry, feed) REFERENCES entries (id, feed)
);"
  (let ((feeds-buffer (generate-new-buffer " *csv-feeds*"))
        (entries-buffer (generate-new-buffer " *csv-entries*"))
        (tags-buffer (generate-new-buffer " *csv-tags*"))
        (seen (make-hash-table :test 'eq)))
    ;; Write headers
    (when headers-p
      (with-current-buffer feeds-buffer
        (elfeed-csv-insert [url title canonical-url author]))
      (with-current-buffer entries-buffer
        (elfeed-csv-insert [id feed title link date]))
      (with-current-buffer tags-buffer
        (elfeed-csv-insert [entry feed tag])))
    ;; Write data
    (with-elfeed-db-visit (entry feed)
      (unless (gethash feed seen)
        (setf (gethash feed seen) t)
        (let ((url (elfeed-feed-url feed))
              (title (elfeed-feed-title feed))
              (canonical-url (elfeed-meta feed :canonical-url))
              (author (elfeed-feed-author feed)))
          (with-current-buffer feeds-buffer
            (elfeed-csv-insert (list url title canonical-url author)))))
      (let ((id (cdr (elfeed-entry-id entry)))
            (feed-id (elfeed-entry-feed-id entry))
            (title (elfeed-entry-title entry))
            (link (elfeed-entry-link entry))
            (date (elfeed-entry-date entry)))
        (with-current-buffer entries-buffer
          (elfeed-csv-insert (list id feed-id title link date)))
        (with-current-buffer tags-buffer
          (dolist (tag (elfeed-entry-tags entry))
            (elfeed-csv-insert (list id feed-id tag))))))
    ;; Write files
    (with-current-buffer tags-buffer
      (write-region nil nil tags-file nil 0)
      (kill-buffer))
    (with-current-buffer entries-buffer
      (write-region nil nil entries-file nil 0)
      (kill-buffer))
    (with-current-buffer feeds-buffer
      (write-region nil nil feeds-file nil 0)
      (kill-buffer))))

(provide 'elfeed-csv)

;;; elfeed-csv.el ends here
