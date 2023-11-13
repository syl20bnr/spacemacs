;;; orgtbl-sqlinsert.el --- orgtbl to SQL insert statements.

;; Copyright (C) 2008-2021  Free Software Foundation, Inc.

;; Author: Jason Riedy <jason@acm.org>
;; Keywords: org, tables, sql

;; This file is not part of GNU Emacs.

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

;; Converts an orgtbl to a sequence of SQL insertion commands.
;; Table cells are quoted and escaped very conservatively.

;;; Code:

(defun orgtbl-to-sqlinsert (table params)
  "Convert the orgtbl-mode TABLE to SQL insert statements.
TABLE is a list, each entry either the symbol `hline' for a horizontal
separator line, or a list of fields for that line.
PARAMS is a property list of parameters that can influence the conversion.

Names and strings are modified slightly by default.  Single-ticks
are doubled as per SQL's standard mechanism.  Backslashes and
dollar signs are deleted.  And tildes are changed to spaces.
These modifications were chosen for use with TeX.  See
ORGTBL-SQL-STRIP-AND-QUOTE.

Supports all parameters from ORGTBL-TO-GENERIC.  New to this function
are:

:sqlname   The name of the database table; defaults to the name of the
           target region.

:nowebname If not nil, used as a wrapping noweb fragment name.

The most important parameters of ORGTBL-TO-GENERIC for SQL are:

:splice    When set to t, return only insert statements, don't wrap
           them in a transaction.  Default is nil.

:tstart, :tend
           The strings used to begin and commit the transaction.

:hfmt      A function that gathers the quoted header names into a
           dynamically scoped variable HDRLIST.  Probably should
           not be changed by the user.

The general parameters :skip and :skipcols have already been applied when
this function is called."
  (let* (hdrlist
	 (alignment (mapconcat (lambda (x) (if x "r" "l"))
			       org-table-last-alignment ""))
	 (nowebname (plist-get params :nowebname))
	 (breakvals (plist-get params :breakvals))
         (firstheader t)
         (*orgtbl-default-fmt* 'orgtbl-sql-strip-and-quote)
	 (params2
	  (list
	   :sqlname (plist-get params :sqlname)
	   :tstart (lambda () (concat (if nowebname
					  (format "<<%s>>= \n" nowebname)
					"")
				      "BEGIN TRANSACTION;"))
	   :tend (lambda () (concat "COMMIT;" (if nowebname "\n@ " "")))
	   :hfmt (lambda (f) (progn (if firstheader (push f hdrlist) "")))
	   :hlfmt (lambda (&rest cells) (setq firstheader nil))
	   :lstart (lambda () (concat "INSERT INTO "
				      sqlname "( "
				      (mapconcat 'identity (reverse hdrlist)
						 ", ")
				      " )" (if breakvals "\n" " ")
				      "VALUES ( "))
	   :lend " );"
	   :sep " , "
	   :hline nil
	   :remove-nil-lines t))
	 (params (org-combine-plists params2 params))
         (sqlname (plist-get params :sqlname)))
    (orgtbl-to-generic table params)))

(defun orgtbl-sql-quote (str)
  "Convert single ticks to doubled single ticks and wrap in single ticks."
  (concat "'" (mapconcat 'identity (split-string str "'") "''") "'"))

(defun orgtbl-sql-strip-dollars-escapes-tildes (str)
  "Strip dollarsigns and backslash escapes, replace tildes with spaces."
  (mapconcat 'identity
	     (split-string (mapconcat 'identity
				      (split-string str "\\$\\|\\\\")
				      "")
			   "~")
	     " "))

(defun orgtbl-sql-strip-and-quote (str)
  "Apply ORGBTL-SQL-QUOTE and ORGTBL-SQL-STRIP-DOLLARS-ESCAPES-TILDES
to sanitize STR for use in SQL statements."
  (cond ((stringp str)
         (orgtbl-sql-quote (orgtbl-sql-strip-dollars-escapes-tildes str)))
        ((sequencep str) (mapcar 'orgtbl-sql-strip-and-quote str))
        (t nil)))

(provide 'orgtbl-sqlinsert)

;;; orgtbl-sqlinsert.el ends here
