;;; org-invoice.el --- Help manage client invoices in OrgMode
;;
;; Copyright (C) 2008-2014, 2021 pmade inc. (Peter Jones pjones@pmade.com)
;;
;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Building on top of the terrific OrgMode, org-invoice tries to
;; provide functionality for managing invoices.  Currently, it does
;; this by implementing an OrgMode dynamic block where invoice
;; information is aggregated so that it can be exported.
;;
;; It also provides a library of functions that can be used to collect
;; this invoice information and use it in other ways, such as
;; submitting it to on-line invoicing tools.
;;
;; I'm already working on an elisp package to submit this invoice data
;; to the FreshBooks on-line accounting tool.
;;
;; Usage:
;;
;; In your ~/.emacs:
;; (autoload 'org-invoice-report "org-invoice")
;; (autoload 'org-dblock-write:invoice "org-invoice")
;;
;; See the documentation in the following functions:
;;
;; `org-invoice-report'
;; `org-dblock-write:invoice'
;;
;; Latest version:
;;
;; git clone git://pmade.com/elisp
(eval-when-compile
  (require 'cl)
  (require 'org))

(declare-function org-duration-from-minutes "org-duration" (minutes &optional fmt fractional))

(defgroup org-invoice nil
  "OrgMode Invoice Helper"
  :tag "Org-Invoice" :group 'org)

(defcustom org-invoice-long-date-format "%A, %B %d, %Y"
  "The format string for long dates."
  :type 'string :group 'org-invoice)

(defcustom org-invoice-strip-ts t
  "Remove org timestamps that appear in headings."
  :type 'boolean :group 'org-invoice)

(defcustom org-invoice-default-level 2
  "The heading level at which a new invoice starts.  This value
is used if you don't specify a scope option to the invoice block,
and when other invoice helpers are trying to find the heading
that starts an invoice.

The default is 2, assuming that you structure your invoices so
that they fall under a single heading like below:

* Invoices
** This is invoice number 1...
** This is invoice number 2...

If you don't structure your invoices using those conventions,
change this setting to the number that corresponds to the heading
at which an invoice begins."
  :type 'integer :group 'org-invoice)

(defcustom org-invoice-start-hook nil
  "Hook called when org-invoice is about to collect data from an
invoice heading.  When this hook is called, point will be on the
heading where the invoice begins.

When called, `org-invoice-current-invoice' will be set to the
alist that represents the info for this invoice."
  :type 'hook :group 'org-invoice)

  (defcustom org-invoice-heading-hook nil
  "Hook called when org-invoice is collecting data from a
heading. You can use this hook to add additional information to
the alist that represents the heading.

When this hook is called, point will be on the current heading
being processed, and `org-invoice-current-item' will contain the
alist for the current heading.

This hook is called repeatedly for each invoice item processed."
  :type 'hook :group 'org-invoice)

(defvar org-invoice-current-invoice nil
  "Information about the current invoice.")

(defvar org-invoice-current-item nil
  "Information about the current invoice item.")

(defvar org-invoice-table-params nil
  "The table parameters currently being used.")

(defvar org-invoice-total-time nil
  "The total invoice time for the summary line.")

(defvar org-invoice-total-price nil
  "The total invoice price for the summary line.")

(defconst org-invoice-version "1.0.0"
  "The org-invoice version number.")

(defun org-invoice-goto-tree (&optional tree)
  "Move point to the heading that represents the head of the
current invoice.  The heading level will be taken from
`org-invoice-default-level' unless tree is set to a string that
looks like tree2, where the level is 2."
  (let ((level org-invoice-default-level))
    (save-match-data
      (when (and tree (string-match "^tree\\([0-9]+\\)$" tree))
        (setq level (string-to-number (match-string 1 tree)))))
    (org-back-to-heading)
    (while (and (> (org-reduced-level (org-outline-level)) level)
                (org-up-heading-safe)))))

(defun org-invoice-heading-info ()
  "Return invoice information from the current heading."
  (let ((title   (org-no-properties (org-get-heading t)))
        (date    (org-entry-get nil "TIMESTAMP" 'selective))
        (work    (org-entry-get nil "WORK" nil))
        (rate    (or (org-entry-get nil "RATE" t) "0"))
        (level   (org-outline-level))
        raw-date long-date)
    (unless date (setq date (org-entry-get nil "TIMESTAMP_IA" 'selective)))
    (unless date (setq date (org-entry-get nil "TIMESTAMP" t)))
    (unless date (setq date (org-entry-get nil "TIMESTAMP_IA" t)))
    (unless work (setq work (org-entry-get nil "CLOCKSUM" nil)))
    (unless work (setq work "00:00"))
    (when date
      (setq raw-date (apply 'encode-time (org-parse-time-string date)))
      (setq long-date (format-time-string org-invoice-long-date-format raw-date)))
    (when (and org-invoice-strip-ts (string-match org-ts-regexp-both title))
      (setq title (replace-match "" nil nil title)))
    (when (string-match "^[ \t]+" title)
      (setq title (replace-match "" nil nil title)))
    (when (string-match "[ \t]+$" title)
      (setq title (replace-match "" nil nil title)))
    (setq work (org-duration-to-minutes work))
    (setq rate (string-to-number rate))
    (setq org-invoice-current-item (list (cons 'title title)
          (cons 'date date)
          (cons 'raw-date raw-date)
          (cons 'long-date long-date)
          (cons 'work work)
          (cons 'rate rate)
          (cons 'level level)
          (cons 'price (* rate (/ work 60.0)))))
    (run-hook-with-args 'org-invoice-heading-hook)
    org-invoice-current-item))

(defun org-invoice-level-min-max (ls)
  "Return a list where the car is the min level, and the cdr the max."
  (let ((max 0) min level)
    (dolist (info ls)
      (when (cdr (assq 'date info))
        (setq level (cdr (assq 'level info)))
        (when (or (not min) (< level min)) (setq min level))
        (when (> level max) (setq max level))))
    (cons (or min 0) max)))

(defun org-invoice-collapse-list (ls)
  "Reorganize the given list by dates."
  (let ((min-max (org-invoice-level-min-max ls)) new)
    (dolist (info ls)
      (let* ((date (cdr (assq 'date info)))
             (work (cdr (assq 'work info)))
             (price (cdr (assq 'price info)))
             (long-date (cdr (assq 'long-date info)))
             (level (cdr (assq 'level info)))
             (bucket (cdr (assoc date new))))
        (if (and (/= (car min-max) (cdr min-max))
                   (=  (car min-max) level)
                   (=  work 0) (not bucket) date)
            (progn
              (setq info (assq-delete-all 'work info))
              (push (cons 'total-work 0) info)
              (push (cons date (list info)) new)
              (setq bucket (cdr (assoc date new))))
          (when (and date (not bucket))
            (setq bucket (list (list (cons 'date date)
                                     (cons 'title long-date)
                                     (cons 'total-work 0)
                                     (cons 'price 0))))
            (push (cons date bucket) new)
            (setq bucket (cdr (assoc date new))))
          (when (and date bucket)
            (setcdr (assq 'total-work (car bucket))
                    (+ work (cdr (assq 'total-work (car bucket)))))
            (setcdr (assq 'price (car bucket))
                    (+ price (cdr (assq 'price (car bucket)))))
            (nconc bucket (list info))))))
    (nreverse new)))

(defun org-invoice-info-to-table (info)
  "Create a single org table row from the given info alist."
  (let ((title (cdr (assq 'title info)))
        (total (cdr (assq 'total-work info)))
        (work  (cdr (assq 'work info)))
        (price (cdr (assq 'price info)))
        (with-price (plist-get org-invoice-table-params :price)))
    (unless total
      (setq
       org-invoice-total-time (+ org-invoice-total-time work)
       org-invoice-total-price (+ org-invoice-total-price price)))
    (setq total (and total (org-duration-from-minutes total)))
    (setq work  (and work  (org-duration-from-minutes work)))
    (insert-before-markers
     (concat "|" title
             (cond
              (total (concat "|" total))
              (work  (concat "|" work)))
             (and with-price price (concat "|" (format "%.2f" price)))
             "|" "\n"))))

(defun org-invoice-list-to-table (ls)
  "Convert a list of heading info to an org table"
  (let ((with-price (plist-get org-invoice-table-params :price))
        (with-summary (plist-get org-invoice-table-params :summary))
        (with-header (plist-get org-invoice-table-params :headers))
        (org-invoice-total-time 0)
        (org-invoice-total-price 0))
    (insert-before-markers
     (concat "| Task / Date | Time" (and with-price "| Price") "|\n"))
    (dolist (info ls)
      (insert-before-markers "|-\n")
      (mapc 'org-invoice-info-to-table (if with-header (cdr info) (cdr (cdr info)))))
    (when with-summary
      (insert-before-markers
       (concat "|-\n|Total:|"
               (org-duration-from-minutes org-invoice-total-time)
               (and with-price (concat "|" (format "%.2f" org-invoice-total-price)))
               "|\n")))))

(defun org-invoice-collect-invoice-data ()
  "Collect all the invoice data from the current OrgMode tree and
return it.  Before you call this function, move point to the
heading that begins the invoice data, usually using the
`org-invoice-goto-tree' function."
  (let ((org-invoice-current-invoice
         (list (cons 'point (point)) (cons 'buffer (current-buffer))))
        (org-invoice-current-item nil))
    (save-restriction
      (org-narrow-to-subtree)
      (org-clock-sum)
      (run-hook-with-args 'org-invoice-start-hook)
      (cons org-invoice-current-invoice
            (org-invoice-collapse-list
             (org-map-entries 'org-invoice-heading-info t 'tree 'archive))))))

(defun org-dblock-write:invoice (params)
  "Function called by OrgMode to write the invoice dblock.  To
create an invoice dblock you can use the `org-invoice-report'
function.

The following parameters can be given to the invoice block (for
information about dblock parameters, please see the Org manual):

:scope Allows you to override the `org-invoice-default-level'
       variable.  The only supported values right now are ones
       that look like :tree1, :tree2, etc.

:prices Set to nil to turn off the price column.

:headers Set to nil to turn off the group headers.

:summary Set to nil to turn off the final summary line."
  (let ((scope (plist-get params :scope))
        (org-invoice-table-params params)
        (zone (point-marker))
        table)
    (unless scope (setq scope 'default))
    (unless (plist-member params :price) (plist-put params :price t))
    (unless (plist-member params :summary) (plist-put params :summary t))
    (unless (plist-member params :headers) (plist-put params :headers t))
    (save-excursion
      (cond
       ((eq scope 'tree) (org-invoice-goto-tree "tree1"))
       ((eq scope 'default) (org-invoice-goto-tree))
       ((symbolp scope) (org-invoice-goto-tree (symbol-name scope))))
      (setq table (org-invoice-collect-invoice-data))
      (goto-char zone)
      (org-invoice-list-to-table (cdr table))
      (goto-char zone)
      (org-table-align)
      (move-marker zone nil))))

(defun org-invoice-in-report-p ()
  "Check to see if point is inside an invoice report."
  (let ((pos (point)) start)
    (save-excursion
      (end-of-line 1)
      (and (re-search-backward "^#\\+BEGIN:[ \t]+invoice" nil t)
	   (setq start (match-beginning 0))
	   (re-search-forward "^#\\+END:.*" nil t)
	   (>= (match-end 0) pos)
	   start))))

(defun org-invoice-report (&optional jump)
  "Create or update an invoice dblock report.  If point is inside
an existing invoice report, the report is updated.  If point
isn't inside an invoice report, a new report is created.

When called with a prefix argument, move to the first invoice
report after point and update it.

For information about various settings for the invoice report,
see the `org-dblock-write:invoice' function documentation.

An invoice report is created by reading a heading tree and
collecting information from various properties.  It is assumed
that all invoices start at a second level heading, but this can
be configured using the `org-invoice-default-level' variable.

Here is an example, where all invoices fall under the first-level
heading Invoices:

* Invoices
** Client Foo (Jan 01 - Jan 15)
*** [2008-01-01 Tue] Built New Server for Production
*** [2008-01-02 Wed] Meeting with Team to Design New System
** Client Bar (Jan 01 - Jan 15)
*** [2008-01-01 Tue] Searched for Widgets on Google
*** [2008-01-02 Wed] Billed You for Taking a Nap

In this layout, invoices begin at level two, and invoice
items (tasks) are at level three.  You'll notice that each level
three heading starts with an inactive timestamp.  The timestamp
can actually go anywhere you want, either in the heading, or in
the text under the heading.  But you must have a timestamp
somewhere so that the invoice report can group your items by
date.

Properties are used to collect various bits of information for
the invoice.  All properties can be set on the invoice item
headings, or anywhere in the tree.  The invoice report will scan
up the tree looking for each of the properties.

Properties used:

CLOCKSUM: You can use the Org clock-in and clock-out commands to
          create a CLOCKSUM property.  Also see WORK.

WORK: An alternative to the CLOCKSUM property.  This property
      should contain the amount of work that went into this
      invoice item formatted as HH:MM (e.g. 01:30).

RATE: Used to calculate the total price for an invoice item.
      Should be the price per hour that you charge (e.g. 45.00).
      It might make more sense to place this property higher in
      the hierarchy than on the invoice item headings.

Using this information, a report is generated that details the
items grouped by days.  For each day you will be able to see the
total number of hours worked, the total price, and the items
worked on.

You can place the invoice report anywhere in the tree you want.
I place mine under a third-level heading like so:

* Invoices
** An Invoice Header
*** [2008-11-25 Tue] An Invoice Item
*** Invoice Report
#+BEGIN: invoice
#+END:"
  (interactive "P")
  (let ((report (org-invoice-in-report-p)))
    (when (and (not report) jump)
      (when (re-search-forward "^#\\+BEGIN:[ \t]+invoice" nil t)
        (org-show-entry)
        (beginning-of-line)
        (setq report (point))))
    (if report (goto-char report)
      (org-create-dblock (list :name "invoice")))
    (org-update-dblock)))

(provide 'org-invoice)
