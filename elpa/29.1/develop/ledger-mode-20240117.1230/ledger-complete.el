;;; ledger-complete.el --- Helper code for use with the "ledger" command-line tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2003-2016 John Wiegley (johnw AT gnu DOT org)

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301 USA.

;;; Commentary:
;; Functions providing payee and account auto complete.

(require 'cl-lib)
(eval-when-compile
  (require 'subr-x))

;; In-place completion support

;;; Code:
(require 'ledger-context)
(require 'ledger-xact)
(require 'ledger-schedule)

(defcustom ledger-accounts-file nil
  "The path to an optional file in which all accounts are used or declared.
This file will then be used as a source for account name completions."
  :type 'file
  :group 'ledger)

(defcustom ledger-accounts-exclude-function nil
  "Function to exclude accounts from completion.
Should be a predicate function that accepts one argument, an
element of `ledger-accounts-list-in-buffer'."
  :type 'function
  :group 'ledger
  :package-version '(ledger-mode . "2019-08-14"))

(defcustom ledger-complete-in-steps nil
  "When non-nil, `ledger-complete-at-point' completes account names in steps.
If nil, full account names are offered for completion."
  :type 'boolean
  :group 'ledger
  :package-version '(ledger-mode . "4.0.0"))

(defun ledger-parse-arguments ()
  "Parse whitespace separated arguments in the current region."
  ;; FIXME: We don't use pcomplete anymore.
  ;; This is more complex than it appears
  ;; to need, so that it can work with pcomplete.  See
  ;; pcomplete-parse-arguments-function for details
  (let* ((begin (save-match-data
                  (if (looking-back (concat "^\\(" ledger-iso-date-regexp "=\\|\\)"
                                            ledger-incomplete-date-regexp) nil)
                      (match-end 1)
                    (save-excursion
                      (ledger-thing-at-point) ;; leave point at beginning of thing under point
                      (point)))))
         (end (point))
         begins args)
    ;; to support end of line metadata
    (save-excursion
      (when (search-backward ";"
                             (line-beginning-position) t)
        (setq begin (match-beginning 0))))
    (save-excursion
      (goto-char begin)
      (when (< (point) end)
        (skip-chars-forward " \t\n")
        (setq begins (cons (point) begins))
        (setq args (cons (buffer-substring-no-properties
                          (car begins) end)
                         args)))
      (cons (reverse args) (reverse begins)))))


(defun ledger-payees-in-buffer ()
  "Scan buffer and return list of all payees."
  (let ((origin (point))
        payees-list)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              ledger-payee-any-status-regex nil t)  ;; matches first line
        (unless (and (>= origin (match-beginning 0))
                     (< origin (match-end 0)))
          (setq payees-list (cons (match-string-no-properties 3)
                                  payees-list)))))  ;; add the payee
    ;; to the list
    (sort (delete-dups payees-list) #'string-lessp)))

(defun ledger-accounts-in-buffer ()
  "Return an alist of accounts in the current buffer.
The `car' of each element is the account name and the `cdr' is an
alist where the key is a subdirective such as \"assert\" and the
value (if any) is the associated data.  In other words, if you've
declared an account like so:

account Assets:Checking
    assert commodity == \"$\"
    default

Then one of the elements this function returns will be
\(\"Assets:Checking\"
  (\"default\")
  (\"assert\" . \"commodity == \"$\"\"))"
  (save-excursion
    (goto-char (point-min))
    (let (account-list
          (seen (make-hash-table :test #'equal :size 1)))
      ;; First, consider accounts declared with "account" directives, which may or
      ;; may not have associated data. The data is on the following lines up to a
      ;; line not starting with whitespace.
      (while (re-search-forward ledger-account-directive-regex nil t)
        (let ((account (match-string-no-properties 1))
              (lines (buffer-substring-no-properties
                      (point)
                      (progn (ledger-navigate-next-xact-or-directive)
                             (point))))
              data)
          (dolist (d (split-string lines "\n"))
            (setq d (string-trim d))
            (unless (string= d "")
              (if (string-match " " d)
                  (push (cons (substring d 0 (match-beginning 0))
                              (substring d (match-end 0) nil))
                        data)
                (push (cons d nil) data))))
          (push (cons account data) account-list)
          (puthash account t seen)))
      ;; Next, gather all accounts declared in postings
      (unless
          ;; FIXME: People who have set `ledger-flymake-be-pedantic' to non-nil
          ;; probably don't want accounts from postings, just those declared
          ;; with directives.  But the name is a little misleading.  Should we
          ;; make a ledger-mode-be-pedantic and use that instead?
          (bound-and-true-p ledger-flymake-be-pedantic)
        (ledger-xact-iterate-transactions
         (lambda (_pos _date _state _payee)
           (let ((end (save-excursion (ledger-navigate-end-of-xact))))
             (forward-line)
             (while (re-search-forward ledger-account-name-or-directive-regex end t)
               (let ((account (match-string-no-properties 1)))
                 (unless (gethash account seen)
                   (puthash account t seen)
                   (push (cons account nil) account-list))))))))
      (sort account-list (lambda (a b) (string-lessp (car a) (car b)))))))

(defun ledger-accounts-list-in-buffer ()
  "Return a list of all known account names in the current buffer as strings.
Considers both accounts listed in postings and those declared
with \"account\" directives."
  (let ((accounts (ledger-accounts-in-buffer)))
    (when ledger-accounts-exclude-function
      (setq accounts (cl-remove-if ledger-accounts-exclude-function accounts)))
    (mapcar #'car accounts)))

(defun ledger-accounts-list ()
  "Return a list of all known account names as strings.
Looks in `ledger-accounts-file' if set, otherwise the current buffer."
  (if ledger-accounts-file
      (let ((f ledger-accounts-file))
        (with-temp-buffer
          (insert-file-contents f)
          (ledger-accounts-list-in-buffer)))
    (ledger-accounts-list-in-buffer)))

(defun ledger-find-accounts-in-buffer ()
  (let ((account-tree (list t))
        (account-elements nil)
        (prefix ""))
    (save-excursion
      (goto-char (point-min))

      (dolist (account
               (cl-remove-if-not (lambda (c) (string-prefix-p prefix c))
                                 (ledger-accounts-list)))
        (let ((root account-tree))
          (setq account-elements
                (split-string
                 account ":"))
          (while account-elements
            (let ((xact (assoc (car account-elements) root)))
              (if xact
                  (setq root (cdr xact))
                (setq xact (cons (car account-elements) (list t)))
                (nconc root (list xact))
                (setq root (cdr xact))))
            (setq account-elements (cdr account-elements))))))
    account-tree))

(defun ledger-accounts-tree ()
  "Return a tree of all accounts in the buffer."
  (let* ((current (caar (ledger-parse-arguments)))
         (elements (and current (split-string current ":")))
         (root (ledger-find-accounts-in-buffer))
         (prefix nil))
    (while (cdr elements)
      (let ((xact (assoc (car elements) root)))
        (if xact
            (setq prefix (concat prefix (and prefix ":")
                                 (car elements))
                  root (cdr xact))
          (setq root nil elements nil)))
      (setq elements (cdr elements)))
    (setq root (delete (list (car elements) t) root))
    (and root
         (sort
          (mapcar (function
                   (lambda (x)
                     (let ((term (if prefix
                                     (concat prefix ":" (car x))
                                   (car x))))
                       (if (> (length (cdr x)) 1)
                           (concat term ":")
                         term))))
                  (cdr root))
          'string-lessp))))

(defun ledger-complete-date (month-string day-string)
  "Complete a date."
  (let*
      ((now (current-time))
       (decoded (decode-time now))
       (this-month (nth 4 decoded))
       (this-year (nth 5 decoded))
       (last-month (if (> this-month 1) (1- this-month) 12))
       (last-year (1- this-year))
       (last-month-year (if (> this-month 1) this-year last-year))
       (month (and month-string
                   (string-to-number month-string)))
       (day (string-to-number day-string))
       (dates (list (encode-time 0 0 0 day (or month this-month) this-year)
                    (if month
                        (encode-time 0 0 0 day month last-year)
                      (encode-time 0 0 0 day last-month last-month-year)))))
    (lambda (_string _predicate _all)
      (concat (ledger-format-date
               (cl-find-if (lambda (date) (not (time-less-p now date))) dates))
              (and (= (point) (line-end-position)) " ")))))

(defun ledger-complete-effective-date
    (tx-year-string tx-month-string tx-day-string
                    month-string day-string)
  "Complete an effective date."
  (let*
      ((tx-year (string-to-number tx-year-string))
       (tx-month (string-to-number tx-month-string))
       (tx-day (string-to-number tx-day-string))
       (tx-date (encode-time 0 0 0 tx-day tx-month tx-year))
       (next-month (if (< tx-month 12) (1+ tx-month) 1))
       (next-year (1+ tx-year))
       (next-month-year (if (< tx-month 12) tx-year next-year))
       (month (and month-string
                   (string-to-number month-string)))
       (day (string-to-number day-string))
       (dates (list (encode-time 0 0 0 day (or month tx-month) tx-year)
                    (if month
                        (encode-time 0 0 0 day month next-year)
                      (encode-time 0 0 0 day next-month next-month-year)))))
    (lambda (_string _predicate _all)
      (concat (ledger-format-date
               (cl-find-if (lambda (date) (not (time-less-p date tx-date))) dates))
              (and (= (point) (line-end-position)) " ")))))

(defun ledger-complete-at-point ()
  "Do appropriate completion for the thing at point."
  (let ((end (point))
        start collection
        realign-after
        delete-suffix)
    (cond (;; Date
           (looking-back (concat "^" ledger-incomplete-date-regexp) (line-beginning-position))
           (setq collection (ledger-complete-date (match-string 1) (match-string 2))
                 start (match-beginning 0)
                 delete-suffix (save-match-data
                                 (when (looking-at (rx (one-or-more (or digit (any ?/ ?-)))))
                                   (length (match-string 0))))))
          (;; Effective dates
           (looking-back (concat "^" ledger-iso-date-regexp "=" ledger-incomplete-date-regexp)
                         (line-beginning-position))
           (setq start (line-beginning-position))
           (setq collection (ledger-complete-effective-date
                             (match-string 2) (match-string 3) (match-string 4)
                             (match-string 5) (match-string 6))))
          (;; Payees
           (eq (save-excursion (ledger-thing-at-point)) 'transaction)
           (setq start (save-excursion (backward-word) (point)))
           (setq collection #'ledger-payees-in-buffer))
          (;; Accounts
           (save-excursion
             (back-to-indentation)
             (skip-chars-forward "([") ;; for virtual accounts
             (setq start (point)))
           (setq delete-suffix (save-excursion
                                 (when (search-forward-regexp (rx (or eol (or ?\t (repeat 2 space)))) (line-end-position) t)
                                   (- (match-beginning 0) end)))
                 realign-after t
                 collection (if ledger-complete-in-steps
                                #'ledger-accounts-tree
                              #'ledger-accounts-list))))
    (when collection
      (let ((prefix (buffer-substring-no-properties start end)))
        (list start end
              (if (functionp collection)
                  (completion-table-with-cache
                   (lambda (_)
                     (cl-remove-if (apply-partially 'string= prefix) (funcall collection))))
                collection)
              :exit-function (lambda (&rest _)
                               (when delete-suffix
                                 (delete-char delete-suffix))
                               (when (and realign-after ledger-post-auto-align)
                                 (ledger-post-align-postings (line-beginning-position) (line-end-position))))
              'ignore)))))

(defun ledger-trim-trailing-whitespace (str)
  (replace-regexp-in-string "[ \t]*$" "" str))

(defun ledger-fully-complete-xact ()
  "Completes a transaction if there is another matching payee in the buffer.

Interactively, if point is after a payee, complete the
transaction with the details from the last transaction to that
payee."
  (interactive)
  (let* ((name (ledger-trim-trailing-whitespace (caar (ledger-parse-arguments))))
         (rest-of-name name)
         xacts)
    (save-excursion
      (when (eq 'transaction (ledger-thing-at-point))
        (delete-region (point) (+ (length name) (point)))
        ;; Search backward for a matching payee
        (when (re-search-backward
               (concat "^[0-9/.=-]+\\(\\s-+\\*\\)?\\(\\s-+(.*?)\\)?\\s-+\\(.*"
                       (regexp-quote name) ".*\\)" ) nil t)
          (setq rest-of-name (match-string 3))
          ;; Start copying the postings
          (forward-line)
          (setq xacts (buffer-substring-no-properties (point) (ledger-navigate-end-of-xact))))))
    ;; Insert rest-of-name and the postings
    (save-excursion
      (insert rest-of-name ?\n)
      (insert xacts)
      (unless (looking-at-p "\n\n")
        (insert "\n")))
    (forward-line)
    (goto-char (line-end-position))
    (when (re-search-backward "\\(\t\\| [ \t]\\)" nil t)
      (goto-char (match-end 0)))))

(provide 'ledger-complete)

;;; ledger-complete.el ends here
