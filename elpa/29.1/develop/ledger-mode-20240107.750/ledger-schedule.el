;;; ledger-schedule.el --- Helper code for use with the "ledger" command-line tool  -*- lexical-binding: t; -*-

;; Copyright (C) 2013 Craig Earls (enderw88 at gmail dot com)

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
;; MA 02110-1301 USA.

;;; Commentary:
;;
;; This module provides for automatically adding transactions to a
;; ledger buffer on a periodic basis.  Recurrence expressions are
;; inspired by Martin Fowler's "Recurring Events for Calendars",
;; martinfowler.com/apsupp/recurring.pdf

;; use (fset 'VARNAME (macro args)) to put the macro definition in the
;; function slot of the symbol VARNAME.  Then use VARNAME as the
;; function without have to use funcall.


(require 'ledger-init)
(require 'cl-lib)

(declare-function ledger-mode "ledger-mode")
;;; Code:

(defgroup ledger-schedule nil
  "Support for automatically recommendation transactions."
  :group 'ledger)

(defcustom ledger-schedule-buffer-name "*Ledger Schedule*"
  "Name for the schedule buffer."
  :type 'string
  :group 'ledger-schedule)

(defcustom ledger-schedule-look-backward 7
  "Number of days to look back in time for transactions."
  :type 'integer
  :group 'ledger-schedule)

(defcustom ledger-schedule-look-forward 14
  "Number of days auto look forward to recommend transactions."
  :type 'integer
  :group 'ledger-schedule)

(defcustom ledger-schedule-file "~/ledger-schedule.ledger"
  "File to find scheduled transactions."
  :type 'file
  :group 'ledger-schedule)

(defcustom ledger-schedule-week-days '(("Mo" 1)
                                       ("Tu" 2)
                                       ("We" 3)
                                       ("Th" 4)
                                       ("Fr" 5)
                                       ("Sa" 6)
                                       ("Su" 0))
  "List of weekday abbreviations.
There must be exactly seven entries each with a two character
abbreviation for a day and the number of that day in the week."
  :type '(alist :value-type (group integer))
  :group 'ledger-schedule)

(defsubst ledger-between (val low high)
  "Return TRUE if VAL >= LOW and <= HIGH."
  (declare (obsolete <= "Ledger-mode v4.0.1"))
  (<= low val high))

(defun ledger-schedule-days-in-month (month year)
  "Return number of days in the MONTH, MONTH is from 1 to 12.
If YEAR is nil, assume it is not a leap year"
  (if (<= 1 month 12)
      (if (and year (date-leap-year-p year) (= 2 month))
          29
        (nth (1- month) '(31 28 31 30 31 30 31 31 30 31 30 31)))
    (error "Month out of range, MONTH=%S" month)))

(defun ledger-schedule-encode-day-of-week (day-string)
  "Return the numerical day of week corresponding to DAY-STRING."
  (cadr (assoc day-string ledger-schedule-week-days)))

;; Macros to handle date expressions

(defun ledger-schedule-constrain-day-in-month (count day-of-week)
  "Return a form that returns TRUE for the  the COUNT DAY-OF-WEEK.
For example, return true if date is the 3rd Thursday of the
month.  Negative COUNT starts from the end of the month. (EQ
COUNT 0) means EVERY day-of-week (eg. every Saturday)"
  (if (and (<= -6 count 6) (<= 0 day-of-week 6))
      (cond ((zerop count) ;; Return true if day-of-week matches
             `(eq (nth 6 (decode-time date)) ,day-of-week))
            ((> count 0) ;; Positive count
             (let ((decoded (cl-gensym)))
               `(let ((,decoded (decode-time date)))
                  (and (eq (nth 6 ,decoded) ,day-of-week)
                       (<= ,(* (1- count) 7)
                           (nth 3 ,decoded)
                           ,(* count 7))))))
            ((< count 0)
             (let ((days-in-month (cl-gensym))
                   (decoded (cl-gensym)))
               `(let* ((,decoded (decode-time date))
                       (,days-in-month (ledger-schedule-days-in-month
                                        (nth 4 ,decoded)
                                        (nth 5 ,decoded))))
                  (and (eq (nth 6 ,decoded) ,day-of-week)
                       (<= (+ ,days-in-month ,(* count 7))
                           (nth 3 ,decoded)
                           (+ ,days-in-month ,(* (1+ count) 7)))))))
            (t
             (error "COUNT out of range, COUNT=%S" count)))
    (error "Invalid argument to ledger-schedule-day-in-month-macro %S %S"
           count
           day-of-week)))

(defun ledger-schedule-constrain-every-count-day (day-of-week skip start-date)
  "Return a form that is true for every DAY-OF-WEEK.
Skips SKIP, and starts on START-DATE.
For example every second Friday, regardless of month."
  (let ((start-day (nth 6 (decode-time start-date))))
    (if (eq start-day day-of-week)  ;; good, can proceed
        `(zerop (mod (- (time-to-days date) ,(time-to-days start-date)) ,(* skip 7)))
      (error "START-DATE day of week doesn't match DAY-OF-WEEK"))))

(defun ledger-schedule-constrain-date-range (month1 day1 month2 day2)
  "Return a form of DATE that is true if DATE falls between two dates.
The dates are given by the pairs MONTH1 DAY1 and MONTH2 DAY2."
  (let ((decoded (cl-gensym))
        (target-month (cl-gensym))
        (target-day (cl-gensym)))
    `(let* ((,decoded (decode-time date))
            (,target-month (nth 4 decoded))
            (,target-day (nth 3 decoded)))
       (and (and (> ,target-month ,month1)
                 (< ,target-month ,month2))
            (and (> ,target-day ,day1)
                 (< ,target-day ,day2))))))



(defun ledger-schedule-scan-transactions (schedule-file)
  "Scan SCHEDULE-FILE and return a list of transactions with date predicates.
The car of each item is a function of date that returns true if
the transaction should be logged for that day."
  (interactive "fFile name: ")
  (let ((xact-list (list)))
    (with-current-buffer
        (find-file-noselect schedule-file)
      (goto-char (point-min))
      (while (re-search-forward "^\\[\\(.*\\)\\] " nil t)
        (let ((date-descriptor "")
              (transaction nil)
              (xact-start (match-end 0)))
          (setq date-descriptor
                (ledger-schedule-read-descriptor-tree
                 (buffer-substring-no-properties
                  (match-beginning 0)
                  (match-end 0))))
          (forward-paragraph)
          (setq transaction (list date-descriptor
                                  (buffer-substring-no-properties
                                   xact-start
                                   (point))))
          (setq xact-list (cons transaction xact-list))))
      xact-list)))

(defun ledger-schedule-read-descriptor-tree (descriptor-string)
  "Read DESCRIPTOR-STRING and return a form that evaluates dates."
  (ledger-schedule-transform-auto-tree
   (split-string
    (substring descriptor-string 1 (string-match "]" descriptor-string)) " ")))

(defun ledger-schedule-transform-auto-tree (descriptor-string-list)
  "Take DESCRIPTOR-STRING-LIST, and return a string with a lambda function of date."
  ;; use funcall to use the lambda function spit out here
  (if (consp descriptor-string-list)
      (let (result)
        (while (consp descriptor-string-list)
          (let ((newcar (car descriptor-string-list)))
            (if (consp newcar)
                (setq newcar (ledger-schedule-transform-auto-tree (car descriptor-string-list))))
            ;; newcar may be a cons now, after ledger-schedule-transfrom-auto-tree
            (if (consp newcar)
                (push newcar result)
              ;; this is where we actually turn the string descriptor into useful lisp
              (push (ledger-schedule-compile-constraints newcar) result)) )
          (setq descriptor-string-list (cdr descriptor-string-list)))

        ;; tie up all the clauses in a big or lambda, and return
        ;; the lambda function as list to be executed by funcall
        `(lambda (date)
           ,(nconc (list 'or) (nreverse result) descriptor-string-list)))))

(defun ledger-schedule-compile-constraints (descriptor-string)
  "Return a list with the year, month and day fields split."
  (let ((fields (split-string descriptor-string "[/\\-]" t)))
    (list 'and
          (ledger-schedule-constrain-day (nth 0 fields) (nth 1 fields) (nth 2 fields))
          (ledger-schedule-constrain-year (nth 0 fields) (nth 1 fields) (nth 2 fields))
          (ledger-schedule-constrain-month (nth 0 fields) (nth 1 fields) (nth 2 fields)))))

(defun ledger-schedule-constrain-year (year-desc month-desc day-desc)
  "Return a form that constrains the year.

YEAR-DESC, MONTH-DESC, and DAY-DESC are the string portions of the
date descriptor."
  (cond
   ((string-match "[A-Za-z]" day-desc) t) ; there is an advanced day descriptor which overrides the year
   ((string= year-desc "*") t)
   ((/= 0 (string-to-number year-desc))
    `(memq (nth 5 (decode-time date)) ',(mapcar 'string-to-number (split-string year-desc ","))))
   (t
    (error "Improperly specified year constraint: %s %s %s" year-desc month-desc day-desc))))

(defun ledger-schedule-constrain-month (year-desc month-desc day-desc)
  "Return a form that constrains the month.

YEAR-DESC, MONTH-DESC, and DAY-DESC are the string portions of the
date descriptor."
  (cond
   ((string-match "[A-Za-z]" day-desc) t) ; there is an advanced day descriptor which overrides the month
   ((string= month-desc "*")
    t)  ;; always match
   ((string= month-desc "E")  ;; Even
    `(cl-evenp (nth 4 (decode-time date))))
   ((string= month-desc "O")  ;; Odd
    `(cl-oddp (nth 4 (decode-time date))))
   ((/= 0 (string-to-number month-desc)) ;; Starts with number
    `(memq (nth 4 (decode-time date)) ',(mapcar 'string-to-number (split-string month-desc ","))))
   (t
    (error "Improperly specified month constraint: %s %s %s" year-desc month-desc day-desc))))

(defun ledger-schedule-constrain-day (year-desc month-desc day-desc)
  "Return a form that constrains the day.

YEAR-DESC, MONTH-DESC, and DAY-DESC are the string portions of the
date descriptor."
  (cond ((string= day-desc "*")
         t)
        ((string= day-desc "L")
         `(= (nth 3 (decode-time date)) (ledger-schedule-days-in-month (nth 4 (decode-time date)) (nth 5 (decode-time date)))))
        ((string-match "[A-Za-z]" day-desc)  ;; There is something other than digits and commas
         (ledger-schedule-parse-complex-date year-desc month-desc day-desc))
        ((/= 0 (string-to-number day-desc))
         `(memq (nth 3 (decode-time date)) ',(mapcar 'string-to-number (split-string day-desc ","))))
        (t
         (error "Improperly specified day constraint: %s %s %s" year-desc month-desc day-desc))))



(defun ledger-schedule-parse-complex-date (year-desc month-desc day-desc)
  "Parse day descriptors that have repeats."
  (let ((years (mapcar 'string-to-number (split-string year-desc ",")))
        (months (mapcar 'string-to-number (split-string month-desc ",")))
        (day-parts (split-string day-desc "\\+"))
        (every-nth (string-match "\\+" day-desc)))
    (if every-nth
        (let ((base-day (string-to-number (car day-parts)))
              (increment (string-to-number (substring (cadr day-parts) 0
                                                      (string-match "[A-Za-z]" (cadr day-parts)))))
              (day-of-week (ledger-schedule-encode-day-of-week
                            (substring (cadr day-parts) (string-match "[A-Za-z]" (cadr day-parts))))))
          (ledger-schedule-constrain-every-count-day day-of-week increment (encode-time 0 0 0 base-day (car months) (car years))))
      (let ((count (string-to-number (substring (car day-parts) 0 1)))
            (day-of-week (ledger-schedule-encode-day-of-week
                          (substring (car day-parts) (string-match "[A-Za-z]" (car day-parts))))))
        (ledger-schedule-constrain-day-in-month count day-of-week)))))

(defun ledger-schedule-list-upcoming-xacts (candidate-items early horizon)
  "Search CANDIDATE-ITEMS for xacts that occur within the given period.
The period runs from (today - EARLY) to (today + HORIZON)."
  (let ((start-date (time-subtract (current-time) (days-to-time early)))
        test-date items)
    (cl-loop for day from 0 to (+ early horizon) by 1 do
             (setq test-date (time-add start-date (days-to-time day)))
             (dolist (candidate candidate-items items)
               (if (funcall (car candidate) test-date)
                   (setq items (append items (list (list test-date (cadr candidate))))))))
    items))

(defun ledger-schedule-create-auto-buffer (candidate-items early horizon)
  "Format CANDIDATE-ITEMS for display."
  (let ((candidates (ledger-schedule-list-upcoming-xacts candidate-items early horizon))
        (schedule-buf (get-buffer-create ledger-schedule-buffer-name)))
    (with-current-buffer schedule-buf
      (erase-buffer)
      (dolist (candidate candidates)
        (insert (ledger-format-date (car candidate) ) " " (cadr candidate) "\n"))
      (ledger-mode))
    (length candidates)))

(defun ledger-schedule-upcoming (file look-backward look-forward)
  "Generate upcoming transactions.

FILE is the file containing the scheduled transaction,
default to `ledger-schedule-file'.
LOOK-BACKWARD is the number of days in the past to look at
default to `ledger-schedule-look-backward'
LOOK-FORWARD is the number of days in the future to look at
default to `ledger-schedule-look-forward'

Use a prefix arg to change the default value"
  (interactive (if current-prefix-arg
                   (list (read-file-name "Schedule File: " () ledger-schedule-file t)
                         (read-number "Look backward: " ledger-schedule-look-backward)
                         (read-number "Look forward: " ledger-schedule-look-forward))
                 (list ledger-schedule-file ledger-schedule-look-backward ledger-schedule-look-forward)))
  (if (and file
           (file-exists-p file))
      (progn
        (ledger-schedule-create-auto-buffer
         (ledger-schedule-scan-transactions file)
         look-backward
         look-forward)
        (pop-to-buffer ledger-schedule-buffer-name))
    (error "Could not find ledger schedule file at %s" file)))


(provide 'ledger-schedule)

;;; ledger-schedule.el ends here
