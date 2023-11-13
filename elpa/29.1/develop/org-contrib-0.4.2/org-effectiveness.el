;;; org-effectiveness.el --- Measuring the personal effectiveness

;; Copyright (C) 2013-2021 Free Software Foundation, Inc.

;; Author: David Arroyo Menéndez <davidam@es.gnu.org>
;; Keywords: effectiveness, plot
;; Homepage: https://git.sr.ht/~bzg/org-contrib
;;
;; This file is not part of GNU Emacs.
;;
;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This file implements functions to measure the effectiveness in org.
;; Org-mode doesn't load this module by default - if this is not what
;; you want, configure the variable `org-modules'. Thanks to #emacs-es
;; irc channel for your support.

;;; Code:

(require 'org)

(defcustom org-effectiveness-max-todo 50
  "This variable is useful to advice to the user about
many TODO pending"
  :type 'integer
  :group 'org-effectiveness)

(defun org-effectiveness-advice()
  "Advicing about a possible excess of TODOS"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (< org-effectiveness-max-todo (count-matches "* TODO"))
	(message "An excess of TODOS!"))))

;; Check advice starting an org file
(add-hook 'org-mode-hook 'org-effectiveness-advice)

(defun org-effectiveness-count-keyword(keyword)
  "Print a message with the number of keyword outline in the current buffer"
  (interactive "sKeyword: ")
  (save-excursion
    (goto-char (point-min))
    (message "Number of %s: %d" keyword (count-matches (concat "* " keyword)))))

(defun org-effectiveness-count-todo()
  "Print a message with the number of todo tasks in the current buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (message "Number of TODO: %d" (count-matches "* TODO"))))

(defun org-effectiveness-count-done()
  "Print a message with the number of done tasks in the current buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (message "Number of DONE: %d" (count-matches "* DONE"))))

(defun org-effectiveness-count-canceled()
  "Print a message with the number of canceled tasks in the current buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (message "Number of Canceled: %d" (count-matches "* CANCEL+ED"))))

(defun org-effectiveness-count-task()
  "Print a message with the number of tasks and subtasks in the current buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (message "Number of tasks: %d" (count-matches "^*"))))

(defun org-effectiveness()
  "Returns the effectiveness in the current org buffer"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((done (float (count-matches "* DONE.*\n.*")))
	  (canc (float (count-matches "* CANCEL+ED.*\n.*"))))
      (if (and (= done canc) (zerop done))
	  (setq effectiveness 0)
	(setq effectiveness (* 100 (/ done (+ done canc)))))
      (message "Effectiveness: %f" effectiveness))))


(defun org-effectiveness-keywords-in-date(keyword date)
  (interactive "sKeyword: \nsDate: " keyword date)
  (setq count (count-matches (concat keyword ".*\n.*" date)))
  (message (concat "%sS: %d" keyword count)))

(defun org-effectiveness-dones-in-date(date &optional notmessage)
  (interactive "sGive me a date: " date)
  (save-excursion
    (goto-char (point-min))
    (let ((count (count-matches (concat "DONE.*\n.*" date))))
      (if (eq notmessage 1)
	  (message "%d" count)
	(message "DONES: %d " count)))))

(defun org-effectiveness-todos-in-date(date)
  (interactive "sGive me a date: " date)
  (save-excursion
    (goto-char (point-min))
    (setq count (count-matches (concat "TODO.*\n.*" date)))
    (message "TODOS: %d" count)))

(defun org-effectiveness-canceled-in-date(date)
  (interactive "sGive me a date: " date)
  (save-excursion
    (goto-char (point-min))
    (setq count (count-matches (concat "CANCEL+ED.*\n.*" date)))
    (message "CANCELEDS: %d" count)))

(defun org-effectiveness-ntasks-in-date(date &optional notmessage)
  (interactive "sGive me a date: " date)
  (save-excursion
    (goto-char (point-min))
    (let ((tasks (float (count-matches (concat "^*.*\n.*" date)))))
      (message "%d" tasks))))

(defun org-effectiveness-in-date(date &optional notmessage)
  (interactive "sGive me a date: " date)
  (save-excursion
    (goto-char (point-min))
    (let ((done (float (count-matches (concat "* DONE.*\n.*" date))))
	  (canc (float (count-matches (concat "* CANCEL+ED.*\n.*" date)))))
      (if (and (= done canc) (zerop done))
	  (setq effectiveness 0)
	(setq effectiveness (* 100 (/ done (+ done canc)))))
      (if (eq notmessage 1)
	  (message "%d" effectiveness)
	(message "Effectiveness: %d " effectiveness)))))

(defun org-effectiveness-month-to-string (m)
  (if (< m 10)
      (concat "0" (number-to-string m))
    (number-to-string m)))

(defun org-effectiveness-plot(startdate enddate &optional save)
  (interactive "sGive me the start date: \nsGive me the end date: " startdate enddate)
  (setq dates (org-effectiveness-check-dates startdate enddate))
  (setq syear (cadr (assq 'startyear dates)))
  (setq smonth (cadr (assq 'startmonth dates)))
  (setq eyear (cadr (assq 'endyear dates)))
  (setq emonth (assq 'endmonth dates))
;; Checking the format of the dates
  (if (not (string-match "[0-9][0-9][0-9][0-9]-[0-9][0-9]" startdate))
      (message "The start date must have the next format YYYY-MM"))
  (if (not (string-match "[0-9][0-9][0-9][0-9]-[0-9][0-9]" enddate))
      (message "The end date must have the next format YYYY-MM"))
;; Checking if startdate < enddate
  (if (string-match "^[0-9][0-9][0-9][0-9]" startdate)
      (setq startyear (string-to-number (match-string 0 startdate))))
  (if (string-match "[0-9][0-9]$" startdate)
      (setq startmonth (string-to-number (match-string 0 startdate))))
  (if (string-match "^[0-9][0-9][0-9][0-9]" enddate)
      (setq endyear (string-to-number (match-string 0 enddate))))
  (if (string-match "[0-9][0-9]$" enddate)
      (setq endmonth (string-to-number (match-string 0 enddate))))
  (if (> startyear endyear)
       (message "The start date must be before that end date"))
  (if (and (= startyear endyear) (> startmonth endmonth))
      (message "The start date must be before that end date"))
;; Create a file
  (let ((month startmonth)
	(year startyear)
	(str ""))
    (while (or (> endyear year) (and (= endyear year) (>= endmonth month)))
      (setq str (concat str (number-to-string year) "-" (org-effectiveness-month-to-string month) " " (org-effectiveness-in-date (concat (number-to-string year) "-" (org-effectiveness-month-to-string month)) 1) "\n"))
      (if (= month 12)
	  (progn
	    (setq year (+ 1 year))
	    (setq month 1))
	(setq month (+ 1 month))))
      (write-region str nil "/tmp/org-effectiveness"))
;; Create the bar graph
  (if (eq save t)
      (setq strplot "/usr/bin/gnuplot -e 'set term png; set output \"/tmp/org-effectiveness.png\"; plot \"/tmp/org-effectiveness\" using 2:xticlabels(1) with histograms' -p")
    (setq strplot "/usr/bin/gnuplot -e 'plot \"/tmp/org-effectiveness\" using 2:xticlabels(1) with histograms' -p"))
  (if (file-exists-p "/usr/bin/gnuplot")
      (call-process "/bin/bash" nil t nil "-c" strplot)
    (message "gnuplot is not installed")))

(defun org-effectiveness-plot-save(startdate enddate &optional save)
  (interactive "sGive me the start date: \nsGive me the end date: " startdate enddate)
  (org-effectiveness-plot startdate enddate t))

;; (defun org-effectiveness-plot(startdate enddate)


(defun org-effectiveness-ascii-bar(n &optional label)
  "Print a bar with the percentage from 0 to 100 printed in ascii"
  (interactive "nPercentage: \nsLabel: ")
  (if (or (< n 0) (> n 100))
      (message "The percentage must be between 0 to 100")
    (let ((x 0)
	  (y 0)
	  (z 0))
      (insert (format "\n### %s ###" label))
      (insert "\n-")
      (while (< x n)
	(insert "-")
	(setq x (+ x 1)))
      (insert "+\n")
      (insert (format "%d" n))
      (if (> n 10)
	  (setq y (+ y 1)))
      (while (< y n)
	(insert " ")
	(setq y (+ y 1)))
      (insert "|\n")
      (insert "-")
      (while (< z n)
	(insert "-")
	(setq z (+ z 1)))
      (insert "+"))))

(defun org-effectiveness-html-bar(n &optional label)
  "Print a bar with the percentage from 0 to 100 printed in html"
  (interactive "nPercentage: \nsLabel: ")
  (if (or (< n 0) (> n 100))
      (message "The percentage must be between 0 to 100")
    (let ((x 0)
	  (y 0)
	  (z 0))
      (insert (format "\n<div class='percentage-%d'>%d</div>" n n))
)))


(defun org-effectiveness-check-dates (startdate enddate)
  "Generate a list with ((startyear startmonth) (endyear endmonth))"
  (setq str nil)
  (if (not (string-match "[0-9][0-9][0-9][0-9]-[0-9][0-9]" startdate))
      (setq str "The start date must have the next format YYYY-MM"))
  (if (not (string-match "[0-9][0-9][0-9][0-9]-[0-9][0-9]" enddate))
      (setq str "The end date must have the next format YYYY-MM"))
;; Checking if startdate < enddate
  (if (string-match "^[0-9][0-9][0-9][0-9]" startdate)
      (setq startyear (string-to-number (match-string 0 startdate))))
  (if (string-match "[0-9][0-9]$" startdate)
      (setq startmonth (string-to-number (match-string 0 startdate))))
  (if (string-match "^[0-9][0-9][0-9][0-9]" enddate)
      (setq endyear (string-to-number (match-string 0 enddate))))
  (if (string-match "[0-9][0-9]$" enddate)
      (setq endmonth (string-to-number (match-string 0 enddate))))
  (if (> startyear endyear)
      (setq str "The start date must be before that end date"))
  (if (and (= startyear endyear) (> startmonth endmonth))
      (setq str "The start date must be before that end date"))
  (if str
      (message str)
;;    (list (list startyear startmonth) (list endyear endmonth))))
    (list (list 'startyear startyear) (list 'startmonth startmonth) (list 'endyear endyear) (list 'endmonth endmonth))))

(defun org-effectiveness-plot-ascii (startdate enddate)
  (interactive "sGive me the start date: \nsGive me the end date: " startdate enddate)
  (setq dates (org-effectiveness-check-dates startdate enddate))
  (let ((syear (cadr (assq 'startyear dates)))
	(smonth (cadr (assq 'startmonth dates)))
  	(year (cadr (assq 'startyear dates)))
	(month (cadr (assq 'startmonth dates)))
	(emonth (cadr (assq 'endmonth dates)))
	(eyear (cadr (assq 'endyear dates)))
	(buffer (current-buffer))
  	(str ""))
    (while (or (> eyear year) (and (= eyear year) (>= emonth month)))
      (setq str (org-effectiveness-in-date (concat (number-to-string year) "-" (org-effectiveness-month-to-string month)) 1))
      (switch-to-buffer "*org-effectiveness*")
      (org-effectiveness-ascii-bar (string-to-number str) (format "%s-%s" year month))
      (switch-to-buffer buffer)
      (if (eq month 12)
  	  (progn
  	    (setq year (+ 1 year))
  	    (setq month 1))
  	(setq month (+ 1 month)))))
  (switch-to-buffer "*org-effectiveness*"))


(defun org-effectiveness-plot-ascii-ntasks (startdate enddate)
  (interactive "sGive me the start date: \nsGive me the end date: " startdate enddate)
  (setq dates (org-effectiveness-check-dates startdate enddate))
  (let ((syear (cadr (assq 'startyear dates)))
	(smonth (cadr (assq 'startmonth dates)))
  	(year (cadr (assq 'startyear dates)))
	(month (cadr (assq 'startmonth dates)))
	(emonth (cadr (assq 'endmonth dates)))
	(eyear (cadr (assq 'endyear dates)))
	(buffer (current-buffer))
  	(str ""))
    (while (or (> eyear year) (and (= eyear year) (>= emonth month)))
      (setq str (org-effectiveness-ntasks-in-date (concat (number-to-string year) "-" (org-effectiveness-month-to-string month)) 1))
      (switch-to-buffer "*org-effectiveness*")
      (org-effectiveness-ascii-bar (string-to-number str) (format "%s-%s" year month))
      (switch-to-buffer buffer)
      (if (eq month 12)
  	  (progn
  	    (setq year (+ 1 year))
  	    (setq month 1))
  	(setq month (+ 1 month)))))
  (switch-to-buffer "*org-effectiveness*"))

(defun org-effectiveness-plot-ascii-dones (startdate enddate)
  (interactive "sGive me the start date: \nsGive me the end date: " startdate enddate)
  (setq dates (org-effectiveness-check-dates startdate enddate))
  (let ((syear (cadr (assq 'startyear dates)))
	(smonth (cadr (assq 'startmonth dates)))
  	(year (cadr (assq 'startyear dates)))
	(month (cadr (assq 'startmonth dates)))
	(emonth (cadr (assq 'endmonth dates)))
	(eyear (cadr (assq 'endyear dates)))
	(buffer (current-buffer))
  	(str ""))
    (while (or (> eyear year) (and (= eyear year) (>= emonth month)))
      (setq str (org-effectiveness-dones-in-date (concat (number-to-string year) "-" (org-effectiveness-month-to-string month)) 1))
      (switch-to-buffer "*org-effectiveness*")
      (org-effectiveness-ascii-bar (string-to-number str) (format "%s-%s" year month))
      (switch-to-buffer buffer)
      (if (eq month 12)
  	  (progn
  	    (setq year (+ 1 year))
  	    (setq month 1))
  	(setq month (+ 1 month)))))
  (switch-to-buffer "*org-effectiveness*"))


(defun org-effectiveness-plot-html (startdate enddate)
  "Print html bars about the effectiveness in a buffer"
  (interactive "sGive me the start date: \nsGive me the end date: " startdate enddate)
  (setq dates (org-effectiveness-check-dates startdate enddate))
  (let ((syear (cadr (assq 'startyear dates)))
	(smonth (cadr (assq 'startmonth dates)))
  	(year (cadr (assq 'startyear dates)))
	(month (cadr (assq 'startmonth dates)))
	(emonth (cadr (assq 'endmonth dates)))
	(eyear (cadr (assq 'endyear dates)))
	(buffer (current-buffer))
  	(str ""))
    (switch-to-buffer "*org-effectiveness-html*")
    (insert "<html><head><title>Graphbar</title><meta http-equiv='Content-type' content='text/html; charset=utf-8'><link rel='stylesheet' type='text/css' href='graphbar.css' title='graphbar'></head><body>")
    (while (or (> eyear year) (and (= eyear year) (>= emonth month)))
      (setq str (org-effectiveness-in-date (concat (number-to-string year) "-" (org-effectiveness-month-to-string month)) 1))
      (switch-to-buffer "*org-effectiveness-html*")
      (org-effectiveness-html-bar (string-to-number str) (format "%s-%s" year month))
      (switch-to-buffer buffer)
      (format "%s-%s" year month)
      (if (eq month 12)
    	  (progn
    	    (setq year (+ 1 year))
    	    (setq month 1))
    	(setq month (+ 1 month))))
    (switch-to-buffer "*org-effectiveness-html*")
    (insert "</body></html>")))

(provide 'org-effectiveness)
