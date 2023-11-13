;;; org-secretary.el --- Team management with org-mode
;; Copyright (C) 2010-2014, 2021 Juan Reyero
;;
;; Author: Juan Reyero <juan _at_ juanreyero _dot_ com>
;; Keywords: outlines, tasks, team, management
;; Homepage: http://juanreyero.com/article/emacs/org-teams.html
;; Version: 0.02
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; THis file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; This module implements helper functions for team management.  It
;; makes it easy to keep track of the work of several people.  It
;; keeps context (with whom and where you are) and allows you to use
;; it to metadata to your notes, and to query the tasks associated
;; with the people you are with and the place.
;;
;; See http://juanreyero.com/article/emacs/org-teams.html for a full
;; explanation and configuration instructions.
;;
;;; Configuration
;;;;;;;;;;;;;;;;;
;;
;; In short; your todos use the TODO keyword, your team's use TASK.
;; Your org-todo-keywords should look something like this:
;;
;; (setq org-todo-keywords
;;       '((sequence "TODO(t)" "|" "DONE(d)" "CANCELLED(c)")
;;         (sequence "TASK(f)" "|" "DONE(d)")
;;         (sequence "MAYBE(m)" "|" "CANCELLED(c)")))
;;
;; It helps to distinguish them by color, like this:
;;
;; (setq org-todo-keyword-faces
;;       '(("TODO" . (:foreground "DarkOrange1" :weight bold))
;;         ("MAYBE" . (:foreground "sea green"))
;;         ("DONE" . (:foreground "light sea green"))
;;         ("CANCELLED" . (:foreground "forest green"))
;;         ("TASK" . (:foreground "blue"))))
;;
;; If you want to keep track of stuck projects you should tag your
;; projects with :prj:, and define:
;;
;; (setq org-tags-exclude-from-inheritance '("prj")
;;       org-stuck-projects '("+prj/-MAYBE-DONE"
;;                            ("TODO" "TASK") ()))
;;
;; Define a tag that marks TASK entries as yours:
;;
;; (setq org-sec-me "juanre")
;;
;; Finally, you add the special views to your org-agenda-custom-commands:
;;
;; (setq org-agenda-custom-commands
;;       '(("h" "Work todos" tags-todo
;;          "-personal-doat={.+}-dowith={.+}/!-TASK"
;;          ((org-agenda-todo-ignore-scheduled t)))
;;         ("H" "All work todos" tags-todo "-personal/!-TASK-MAYBE"
;;          ((org-agenda-todo-ignore-scheduled nil)))
;;         ("A" "Work todos with doat or dowith" tags-todo
;;          "-personal+doat={.+}|dowith={.+}/!-TASK"
;;          ((org-agenda-todo-ignore-scheduled nil)))
;;         ("j" "TODO dowith and TASK with"
;;          ((org-sec-with-view "TODO dowith")
;;           (org-sec-where-view "TODO doat")
;;           (org-sec-assigned-with-view "TASK with")
;;           (org-sec-stuck-with-view "STUCK with")))
;;         ("J" "Interactive TODO dowith and TASK with"
;;          ((org-sec-who-view "TODO dowith")))))
;;
;;; Usage
;;;;;;;;;
;;
;;  Do C-c w to say with whom you are meeting (a space-separated list
;;  of names). Maybe do also C-c W to say where you are.  Then do C-c a
;;  j to see:
;;     - Todo items defined with TODO (ie, mine) in which the
;;       =dowith= property matches any of the people with me.
;;     - Todo items defined with TODO in which the =doat= property
;;       matches my current location.
;;     - Todo items defined with TASK that are tagged with the name
;;       of any of the people with me (this is, assigned to them).
;;     - Stuck projects tagged with the name of the people with me.
;;
;; Use C-c j to add meta-data with the people with me, the
;; location and the time to entries.

(require 'org)

(defvar org-sec-me nil
  "Tag that defines TASK todo entries associated to me")

(defvar org-sec-with nil
  "Value of the :with: property when doing an
   org-sec-tag-entry. Change it with org-sec-set-with,
   set to C-c w.  Defaults to org-sec-me")

(defvar org-sec-where ""
  "Value of the :at: property when doing an
   org-sec-tag-entry. Change it with org-sec-set-with,
   set to C-c W")

(defvar org-sec-with-history '()
  "History list of :with: properties")

(defvar org-sec-where-history '()
  "History list of :where: properties")

(defun org-sec-set-with ()
  "Changes the value of the org-sec-with variable for use in the
   next call of org-sec-tag-entry.  Leave it empty to default to
   org-sec-me (you)."
  (interactive)
  (setq org-sec-with (let ((w (read-string "With: " nil
                                           'org-sec-with-history "")))
                       (if (string= w "")
                           nil
                         w))))
(global-set-key "\C-cw" 'org-sec-set-with)

(defun org-sec-set-where ()
  "Changes the value of the org-sec-where variable for use
   in the next call of org-sec-tag-entry."
  (interactive)
  (setq org-sec-where
        (read-string "Where: " nil
                     'org-sec-where-history "")))
(global-set-key "\C-cW" 'org-sec-set-where)

(defun org-sec-set-dowith ()
  "Sets the value of the dowith property."
  (interactive)
  (let ((do-with
         (read-string "Do with: "
                      nil 'org-sec-dowith-history "")))
    (unless (string= do-with "")
      (org-entry-put nil "dowith" do-with))))
(global-set-key "\C-cd" 'org-sec-set-dowith)

(defun org-sec-set-doat ()
  "Sets the value of the doat property."
  (interactive)
  (let ((do-at (read-string "Do at: "
                            nil 'org-sec-doat-history "")))
    (unless (string= do-at "")
      (org-entry-put nil "doat" do-at))))
(global-set-key "\C-cD" 'org-sec-set-doat)

(defun org-sec-tag-entry ()
  "Adds a :with: property with the value of org-sec-with if
   defined, an :at: property with the value of org-sec-where
   if defined, and an :on: property with the current time."
  (interactive)
  (save-excursion
    (org-entry-put nil "on" (format-time-string
                             (org-time-stamp-format 'long)
                             (current-time)))
    (unless (string= org-sec-where "")
      (org-entry-put nil "at" org-sec-where))
    (if org-sec-with
        (org-entry-put nil "with" org-sec-with))))
(global-set-key "\C-cj" 'org-sec-tag-entry)

(defun join (lst sep &optional pre post)
  (mapconcat (lambda (x) (concat pre x post)) lst sep))

(defun org-sec-get-with ()
  (if org-sec-with
      org-sec-with
    org-sec-me))

(defun org-sec-with-view (par &optional who)
  "Select tasks marked as dowith=who, where who
   defaults to the value of org-sec-with."
  (org-tags-view '(4) (join (split-string (if who
                                              who
                                            (org-sec-get-with)))
                            "|" "dowith=\"" "\"")))

(defun org-sec-where-view (par)
  "Select tasks marked as doat=org-sec-where."
  (org-tags-view '(4) (concat "doat={" org-sec-where "}")))

(defun org-sec-assigned-with-view (par &optional who)
  "Select tasks assigned to who, by default org-sec-with."
  (org-tags-view '(4)
                 (concat (join (split-string (if who
                                                 who
                                               (org-sec-get-with)))
                               "|")
                         "/TASK")))

(defun org-sec-stuck-with-view (par &optional who)
  "Select stuck projects assigned to who, by default
   org-sec-with."
  (let ((org-stuck-projects
         `(,(concat "+prj+"
                    (join (split-string (if who
                                            who
                                          (org-sec-get-with))) "|")
                    "/-MAYBE-DONE")
           ("TODO" "TASK") ())))
    (org-agenda-list-stuck-projects)))

(defun org-sec-who-view (par)
  "Builds agenda for a given user.  Queried. "
  (let ((who (read-string "Build todo for user/tag: "
                          "" "" "")))
    (org-sec-with-view "TODO dowith" who)
    (org-sec-assigned-with-view "TASK with" who)
    (org-sec-stuck-with-view "STUCK with" who)))

(provide 'org-secretary)

;;; org-secretary.el ends here
