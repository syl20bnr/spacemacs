;;; helm-x-files.el --- helm auxiliary functions and sources. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2023 Thierry Volpiatto 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'helm-for-files)


;;; List of files gleaned from every dired buffer
;;
;;
(defvar dired-buffers)
(defvar directory-files-no-dot-files-regexp)
(defun helm-files-in-all-dired-candidates ()
  "Return a list of files from live `dired' buffers."
  (save-excursion
    (cl-loop for (f . b) in dired-buffers
          when (buffer-live-p b)
          append (let ((dir (with-current-buffer b dired-directory)))
                   (if (listp dir) (cdr dir)
                     (directory-files f t directory-files-no-dot-files-regexp))))))

;; (dired '("~/" "~/.emacs.d/.emacs-custom.el" "~/.emacs.d/.emacs.bmk"))

(defclass helm-files-dired-source (helm-source-sync helm-type-file)
  ((candidates :initform #'helm-files-in-all-dired-candidates)))

(defvar helm-source-files-in-all-dired
  (helm-make-source "Files in all dired buffer." 'helm-files-dired-source))

;;; session.el files
;;
;;  session (http://emacs-session.sourceforge.net/) is an alternative to
;;  recentf that saves recent file history and much more.
(defvar session-file-alist)
(defclass helm-source-session-class (helm-source-sync)
  ((candidates :initform (lambda ()
                           (cl-delete-if-not
                            (lambda (f)
                              (or (string-match helm-tramp-file-name-regexp f)
                                  (file-exists-p f)))
                            (mapcar 'car session-file-alist))))
   (keymap       :initform 'helm-generic-files-map)
   (help-message :initform 'helm-generic-file-help-message)
   (action       :initform 'helm-type-file-actions)))

(defvar helm-source-session nil
  "File list from emacs-session.")

(defcustom helm-session-fuzzy-match nil
  "Enable fuzzy matching in `helm-source-session' when non--nil."
  :group 'helm-files
  :type 'boolean
  :set (lambda (var val)
         (set var val)
         (setq helm-source-session
               (helm-make-source "Session" 'helm-source-session-class
                 :fuzzy-match val))))


;;; External searching file tools.
;;
;; Tracker desktop search

(defun helm-source-tracker-transformer (candidates _source)
  "Return file names from tracker CANDIDATES."
  ;; loop through tracker candidates selecting out file:// lines
  ;; then select part after file:// and url decode to get straight filenames
  (cl-loop for cand in candidates
           when (and (stringp cand)
                     (string-match "\\`[[:space:]]*file://\\(.*\\)" cand))
           collect (url-unhex-string (match-string 1 cand))))

(defvar helm-source-tracker-search
  (helm-build-async-source "Tracker Search"
    :candidates-process
     (lambda ()
       ;; the tracker-search command has been deprecated, now invoke via tracker
       ;; also, disable the contextual snippets which we don't currently use
       (start-process "tracker-search-process" nil
                      "tracker" "search"
                      "--disable-snippets"
                      "--disable-color"
                      "--limit=512"
                      helm-pattern))
    ;; new simplified transformer of tracker search results
    :filtered-candidate-transformer #'helm-source-tracker-transformer
    ;;(multiline) ; https://github.com/emacs-helm/helm/issues/529
    :keymap helm-generic-files-map
    :action 'helm-type-file-actions
    :action-transformer '(helm-transform-file-load-el
                          helm-transform-file-browse-url)
    :requires-pattern 3)
  "Source for the Tracker desktop search engine.")

;; Spotlight (MacOS X desktop search)
(defclass helm-mac-spotlight-source (helm-source-async helm-type-file)
  ((candidates-process :initform
                       (lambda ()
                         (start-process
                          "mdfind-process" nil "mdfind" helm-pattern)))
   (requires-pattern :initform 3)))

(defvar helm-source-mac-spotlight
  (helm-make-source "mdfind" 'helm-mac-spotlight-source)
  "Source for retrieving files via Spotlight's command line utility mdfind.")

(provide 'helm-x-files)

;;; helm-x-files.el ends here
