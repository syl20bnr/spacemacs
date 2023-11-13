;;; paradox-commit-list.el --- listing commits for a package's repository -*- lexical-binding:t -*-

;; Copyright (C) 2014-2015 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; Prefix: paradox
;; Separator: -

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;


;;; Code:
(require 'subr-x)
(require 'cl-lib)
(require 'package)

(require 'paradox-github)

(defgroup paradox-commit-list nil
  "Buffer used by paradox to list commits for a package."
  :prefix "paradox-"
  :package-version '(paradox . "2.0")
  :group 'paradox)


;;; Variables
(defcustom paradox-commit-list-query-max-pages 1
  "Max number of pages we read from github when fetching the commit-list.
Each page lists 100 commits, so 1 page should be more than enough
for most repositories.

Increasing this number consequently multiplies the time it takes
to load the commit list on repos which actually use that many
pages."
  :type 'integer
  :group 'paradox-commit-list
  :package-version '(paradox . "1.2.3"))

(defcustom paradox-date-format "%Y-%m-%d"
  "Format used for the date displayed on the commit list.
See `format-time-string' for more information.

Set it to \"%x\" for a more \"human\" date format."
  :type 'string
  :group 'paradox-commit-list
  :package-version '(paradox . "1.2.3"))

(defface paradox-commit-tag-face
  '((t :foreground "goldenrod4"
       :background "LemonChiffon1"
       :box 1))
  "Face used for tags on the commit list."
  :group 'paradox-commit-list)


;;; Variables
(defvar paradox--commit-message-face nil
  "Face currently being used on commit messages.
Gets dynamically changed to `font-lock-comment-face' on old commits.
nil means `default'.")

(defvar-local paradox--package-repo nil
  "Repo of the package in a commit-list buffer.")
(defvar-local paradox--package-name nil
  "Name of the package in a commit-list buffer.")
(defvar-local paradox--package-version nil
  "Installed version of the package in a commit-list buffer.")
(defvar-local paradox--package-tag-commit-alist nil
  "Alist of (COMMIT-SHA . TAG) for this package's repo.")

(define-button-type 'paradox-commit
  'action      #'paradox-commit-list-visit-commit
  'follow-link t)

;; Use `font-lock-face' on creation instead.
(button-type-put 'paradox-commit 'face nil)


;;; Functions
(defun paradox--get-tag-commit-alist (repo)
  "Get REPO's tag list and associate them to commit hashes."
  (require 'json)
  (mapcar
   (lambda (x)
     (cons
      (cdr (assoc 'sha (cdr (assoc 'commit x))))
      (cdr (assoc 'name x))))
   (let ((json-array-type 'list))
     (paradox--github-action
      (format "repos/%s/tags?per_page=100" repo)
      :reader #'json-read
      :max-pages paradox-commit-list-query-max-pages))))

(defun paradox--get-installed-version (pkg)
  "Return the installed version of PKG.
- If PKG isn't installed, return '(0).
- If it has a Melpa-like version (YYYYMMDD HHMM), return it as a
  time value.
- If it has a regular version number, return it as a string."
  (let ((desc (cadr (assoc pkg package-alist))))
    (if desc
        (let ((version (package-desc-version desc)))
          (if (> (car version) 19000000)
              (date-to-time
               (format "%8dT%02d:%02d"
                 (car version)
                 (/ (cadr version) 100)
                 (% (cadr version) 100)))
            ;; Regular version numbers.
            (mapconcat 'int-to-string version ".")))
      '(0 0))))

(defun paradox--commit-tabulated-list (repo)
  "Return the tabulated list for REPO's commit list."
  (require 'json)
  (let* ((paradox--commit-message-face nil)
         (json-array-type 'list)
         (feed (paradox--github-action
                (format "repos/%s/commits?per_page=100" repo)
                :reader #'json-read
                :max-pages paradox-commit-list-query-max-pages)))
    (apply 'append (mapcar 'paradox--commit-print-info feed))))

(defun paradox--commit-print-info (x)
  "Parse json in X into a tabulated list entry."
  (let* ((commit (cdr (assoc 'commit x)))
         (date  (date-to-time (cdr (assoc 'date (cdr (assoc 'committer commit))))))
         (title (split-string (cdr (assoc 'message commit)) "[\n\r][ \t]*" t))
         ;; (url   (cdr (assoc 'html_url commit)))
         (cc    (cdr (assoc 'comment_count commit)))
         (sha   (cdr (assoc 'sha x)))
         (tag   (cdr (assoc-string sha paradox--package-tag-commit-alist))))
    ;; Have we already crossed the installed commit, or is it not even installed?
    (unless (or paradox--commit-message-face
                (equal '(0) paradox--package-version))
      ;; Is this where we cross to old commits?
      (when (paradox--version<= date tag)
        (setq paradox--commit-message-face 'paradox-comment-face)))
    ;; Return the tabulated list entry.
    (cons
     ;; The ID
     (list `((is-old . ,(null paradox--commit-message-face))
             (lisp-date . ,date)
             ,@x)
           ;; The actual displayed data
           (vector
            (make-text-button
             (format-time-string paradox-date-format date) nil
             'type           'paradox-commit
             'font-lock-face (or paradox--commit-message-face 'button))
            (concat (if (> cc 0)
                        (propertize (format "(%s comments) " cc)
                                    'face 'font-lock-function-name-face)
                      "")
                    (if (stringp tag)
                        (propertize tag 'face 'paradox-commit-tag-face)
                      "")
                    (if (stringp tag) " " "")
                    (propertize (or (car-safe title) "")
                                'face paradox--commit-message-face))))
     (mapcar (lambda (m) (list x (vector "" (propertize m 'face paradox--commit-message-face))))
       (cdr title)))))

(defun paradox--version<= (date version)
  "Non-nil if commit at DATE tagged with VERSION is older or equal to `paradox--package-version'."
  ;; Melpa date-like versions
  (if (listp paradox--package-version)
      ;; Installed date >= to commit date
      (null (time-less-p paradox--package-version date))
    ;; Regular version numbers.
    (and version
         (ignore-errors (version<= version paradox--package-version)))))

(defun paradox--commit-list-update-entries ()
  "Update entries of current commit-list."
  (setq tabulated-list-entries
        (paradox--commit-tabulated-list paradox--package-repo)))


;;; Commands
(defun paradox-commit-list-visit-commit (&optional _)
  "Visit this commit on GitHub.
IGNORE is ignored."
  (interactive)
  (when (derived-mode-p 'paradox-commit-list-mode)
    (browse-url (cdr (assoc 'html_url (tabulated-list-get-id))))))

(defun paradox-previous-commit (&optional n)
  "Move to previous commit, which might not be the previous line.
With prefix N, move to the N-th previous commit."
  (interactive "p")
  (paradox-next-commit (- n)))

(defun paradox-next-commit (&optional n)
  "Move to next commit, which might not be the next line.
With prefix N, move to the N-th next commit."
  (interactive "p")
  (dotimes (_ (abs n))
    (let ((d (cl-signum n)))
      (forward-line d)
      (while (looking-at "  +")
        (forward-line d)))))


;;; Mode definition
(define-derived-mode paradox-commit-list-mode
  tabulated-list-mode "Paradox Commit List"
  "Major mode for browsing a list of commits.
Letters do not insert themselves; instead, they are commands.
\\<paradox-commit-list-mode-map>
\\{paradox-commit-list-mode-map}"
  (hl-line-mode 1)
  (setq tabulated-list-format
        `[("Date" ,(length (format-time-string paradox-date-format (current-time))) nil)
          ("Message" 0 nil)])
  (setq tabulated-list-padding 1)
  (setq tabulated-list-sort-key nil)
  (add-hook 'tabulated-list-revert-hook 'paradox--commit-list-update-entries nil t)
  (tabulated-list-init-header))

(define-key paradox-commit-list-mode-map "" #'paradox-commit-list-visit-commit)
(define-key paradox-commit-list-mode-map "p" #'paradox-previous-commit)
(define-key paradox-commit-list-mode-map "n" #'paradox-next-commit)


(provide 'paradox-commit-list)
;;; paradox-commit-list.el ends here.
