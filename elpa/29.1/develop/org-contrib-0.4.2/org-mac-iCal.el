;;; org-mac-iCal.el --- Imports events from iCal.app to the Emacs diary

;; Copyright (C) 2009-2014, 2021 Christopher Suckling

;; Author: Christopher Suckling <suckling at gmail dot com>
;; Version: 0.1057.104
;; Keywords: outlines, calendar

;; This file is not part of GNU Emacs.

;; This program is Free Software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file provides the import of events from Mac OS X 10.5 iCal.app
;; into the Emacs diary (it is not compatible with OS X < 10.5). The
;; function org-mac-iCal will import events in all checked iCal.app
;; calendars for the date range org-mac-iCal-range months, centered
;; around the current date.
;;
;; CAVEAT: This function is destructive; it will overwrite the current
;; contents of the Emacs diary.
;;
;; Installation: add (require 'org-mac-iCal) to your .emacs.
;;
;; If you view Emacs diary entries in org-agenda, the following hook
;; will ensure that all-day events are not orphaned below TODO items
;; and that any supplementary fields to events (e.g. Location) are
;; grouped with their parent event
;;
;; (add-hook 'org-agenda-cleanup-fancy-diary-hook
;; 	  (lambda ()
;; 	    (goto-char (point-min))
;; 	    (save-excursion
;; 	      (while (re-search-forward "^[a-z]" nil t)
;; 		(goto-char (match-beginning 0))
;; 		(insert "0:00-24:00 ")))
;; 	    (while (re-search-forward "^ [a-z]" nil t)
;; 	      (goto-char (match-beginning 0))
;; 	      (save-excursion
;; 		(re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
;; 	      (insert (match-string 0)))))

;;; Code:

(defcustom org-mac-iCal-range 2
  "The range in months to import iCal.app entries into the Emacs
diary. The import is centered around today's date; thus a value
of 2 imports entries for one month before and one month after
today's date"
  :group 'org-time
  :type 'integer)

(defun org-mac-iCal ()
  "Selects checked calendars in iCal.app and imports them into
the the Emacs diary"
  (interactive)

  ;; kill diary buffers then empty diary files to avoid duplicates
  (setq currentBuffer (buffer-name))
  (setq openBuffers (mapcar (function buffer-name) (buffer-list)))
  (omi-kill-diary-buffer openBuffers)
  (with-temp-buffer
    (insert-file-contents diary-file)
    (delete-region (point-min) (point-max))
    (write-region (point-min) (point-max) diary-file))

  ;; determine available calendars
  (setq caldav-folders (directory-files "~/Library/Calendars" 1 ".*caldav$"))
  (setq caldav-calendars nil)
  (mapc
     (lambda (x)
       (setq caldav-calendars (nconc caldav-calendars (directory-files x 1 ".*calendar$"))))
     caldav-folders)

  (setq local-calendars nil)
  (setq local-calendars (directory-files "~/Library/Calendars" 1 ".*calendar$"))

  (setq all-calendars (append caldav-calendars local-calendars))

  ;; parse each calendar's Info.plist to see if calendar is checked in iCal
  (setq all-calendars (delq 'nil (mapcar
				    (lambda (x)
				      (omi-checked x))
				    all-calendars)))

  ;; for each calendar, concatenate individual events into a single ics file
  (with-temp-buffer
    (shell-command "sw_vers" (current-buffer))
    (when (re-search-backward "10\\.[5678]" nil t)
      (omi-concat-leopard-ics all-calendars)))

  ;; move all caldav ics files to the same place as local ics files
  (mapc
   (lambda (x)
     (mapc
      (lambda (y)
        (rename-file (concat x "/" y);
                     (concat "~/Library/Calendars/" y)))
      (directory-files x nil ".*ics$")))
   caldav-folders)

  ;; check calendar has contents and import
  (setq import-calendars (directory-files "~/Library/Calendars" 1 ".*ics$"))
  (mapc
   (lambda (x)
     (when (/= (nth 7 (file-attributes x 'string)) 0)
       (omi-import-ics x)))
   import-calendars)

  ;; tidy up intermediate files and buffers
  (setq usedCalendarsBuffers (mapcar (function buffer-name) (buffer-list)))
  (omi-kill-ics-buffer usedCalendarsBuffers)
  (setq usedCalendarsFiles (directory-files "~/Library/Calendars" 1 ".*ics$"))
  (omi-delete-ics-file usedCalendarsFiles)

  (org-pop-to-buffer-same-window currentBuffer))

(defun omi-concat-leopard-ics (list)
  "Leopard stores each iCal.app event in a separate ics file.
Whilst useful for Spotlight indexing, this is less helpful for
icalendar-import-file. omi-concat-leopard-ics concatenates these
individual event files into a single ics file"
  (mapc
   (lambda (x)
     (setq omi-leopard-events (directory-files (concat x "/Events") 1 ".*ics$"))
     (with-temp-buffer
       (mapc
	(lambda (y)
	  (insert-file-contents (expand-file-name y)))
	omi-leopard-events)
       (write-region (point-min) (point-max) (concat (expand-file-name x) ".ics"))))
   list))

(defun omi-import-ics (string)
  "Imports an ics file into the Emacs diary. First tidies up the
ics file so that it is suitable for import and selects a sensible
date range so that Emacs calendar view doesn't grind to a halt"
  (with-temp-buffer
    (insert-file-contents string)
    (goto-char (point-min))
    (while
	(re-search-forward "^BEGIN:VCALENDAR$" nil t)
      (setq startEntry (match-beginning 0))
      (re-search-forward "^END:VCALENDAR$" nil t)
      (setq endEntry (match-end 0))
      (save-restriction
	(narrow-to-region startEntry endEntry)
	(goto-char (point-min))
	(re-search-forward "\\(^DTSTART;.*:\\)\\([0-9][0-9][0-9][0-9]\\)\\([0-9][0-9]\\)" nil t)
	(if (or (eq (match-string 2) nil) (eq (match-string 3) nil))
	    (progn
	      (setq yearEntry 1)
	      (setq monthEntry 1))
	  (setq yearEntry (string-to-number (match-string 2)))
	  (setq monthEntry (string-to-number (match-string 3))))
	(setq year (string-to-number (format-time-string "%Y")))
	(setq month (string-to-number (format-time-string "%m")))
        (setq now (list month 1 year))
        (setq entryDate (list monthEntry 1 yearEntry))
        ;; Check to see if this is a repeating event
        (goto-char (point-min))
        (setq isRepeating (re-search-forward "^RRULE:" nil t))
	;; Delete if outside range and not repeating
        (when (and
               (not isRepeating)
               (> (abs (- (calendar-absolute-from-gregorian now)
                          (calendar-absolute-from-gregorian entryDate)))
                  (* (/ org-mac-iCal-range 2) 30))
	  (delete-region startEntry endEntry)))
          (goto-char (point-max))))
    (while
	(re-search-forward "^END:VEVENT$" nil t)
      (delete-blank-lines))
    (goto-line 1)
    (insert "BEGIN:VCALENDAR\n\n")
    (goto-line 2)
    (while
	(re-search-forward "^BEGIN:VCALENDAR$" nil t)
      (replace-match "\n"))
    (goto-line 2)
    (while
	(re-search-forward "^END:VCALENDAR$" nil t)
      (replace-match "\n"))
    (insert "END:VCALENDAR")
    (goto-line 1)
    (delete-blank-lines)
    (while
	(re-search-forward "^END:VEVENT$" nil t)
      (delete-blank-lines))
    (goto-line 1)
    (while
	(re-search-forward "^ORG.*" nil t)
      (replace-match "\n"))
    (goto-line 1)
    (write-region (point-min) (point-max) string))

  (icalendar-import-file string diary-file))

(defun omi-kill-diary-buffer (list)
  (mapc
   (lambda (x)
     (if (string-match "^diary" x)
	 (kill-buffer x)))
   list))

(defun omi-kill-ics-buffer (list)
  (mapc
   (lambda (x)
     (if (string-match "ics$" x)
	 (kill-buffer x)))
   list))

(defun omi-delete-ics-file (list)
  (mapc
   (lambda (x)
     (delete-file x))
   list))

(defun omi-checked (directory)
  "Parse Info.plist in iCal.app calendar folder and determine
whether Checked key is 1. If Checked key is not 1, remove
calendar from list of calendars for import"
  (let* ((root (xml-parse-file (car (directory-files directory 1 "Info.plist"))))
	 (plist (car root))
	 (dict (car (xml-get-children plist 'dict)))
	 (keys (cdr (xml-node-children dict)))
	 (keys (mapcar
		(lambda (x)
		  (cond ((listp x)
			 x)))
		keys))
	 (keys (delq 'nil keys)))
    (when (equal "1" (car (cddr (lax-plist-get keys '(key nil "Checked")))))
      directory)))

(provide 'org-mac-iCal)

;;; org-mac-iCal.el ends here
