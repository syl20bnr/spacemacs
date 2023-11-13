;;; org-noter.el --- A synchronized, Org-mode, document annotator       -*- lexical-binding: t; -*-

;; Copyright (C) 2017-2019  Gonçalo Santos

;; Author: Gonçalo Santos (github.com/weirdNox) <in@bsentia>
;;	   Maintainer Dmitry M <dmitrym@gmail.com>
;; Maintainer: Peter Mao <peter.mao@gmail.com>
;;             Dmitry M <dmitrym@gmail.com>
;; Homepage: https://github.com/org-noter/org-noter
;; Keywords: lisp pdf interleave annotate external sync notes documents org-mode
;; Package-Requires: ((emacs "24.4") (cl-lib "0.6") (org "9.4"))
;; Version: 1.5.0

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; The idea is to let you create notes that are kept in sync when you scroll
;; through the document, but that are external to it - the notes themselves live
;; in an Org-mode file.  As such, this leverages the power of Org-mode (the
;; notes may have outlines, latex fragments, babel, etc...) while acting like
;; notes that are made /in/ the document.

;; Also, I must thank Sebastian for the original idea and inspiration!
;; Link to the original Interleave package:
;; https://github.com/rudolfochrist/interleave

;;; Code:
(require 'org-element)
(require 'cl-lib)

(require 'org-noter-core)

(declare-function org-entry-put "org")
(declare-function org-with-wide-buffer "org-macs")

(add-to-list 'load-path (concat (file-name-directory load-file-name) "modules"))
(when (or (memq 'doc-view-mode org-noter-supported-modes)
          (memq 'pdf-view-mode org-noter-supported-modes))
  (require 'org-noter-pdf))
(when (memq 'nov-mode org-noter-supported-modes)
  (require 'org-noter-nov))
(when (memq 'djvu-read-mode org-noter-supported-modes)
  (require 'org-noter-djvu))

;;;###autoload
(defun org-noter (&optional arg)
  "Start `org-noter' session.

There are two modes of operation.  You may create the session from:
- The Org notes file
- The document to be annotated (PDF, EPUB, ...)

- Creating the session from notes file
--------------------------------------
This will open a session for taking your notes, with indirect
buffers to the document and the notes side by side.  Your current
window configuration won't be changed, because this opens in a
new frame.

You only need to run this command inside a heading (which will
hold the notes for this document).  If no document path property is found,
this command will ask you for the target file.

With a prefix universal argument ARG, only check for the property
in the current heading, don't inherit from parents.

With 2 prefix universal arguments ARG, ask for a new document,
even if the current heading annotates one.

With a prefix number ARG:
- Greater than 0: Open the document like `find-file'
-     Equal to 0: Create session with `org-noter-always-create-frame' toggled
-    Less than 0: Open the folder containing the document

- Creating the session from the document
----------------------------------------
This will try to find a notes file in any of the parent folders.
The names it will search for are defined in
`org-noter-default-notes-file-names'.  It will also try to find a
notes file with the same name as the document, giving it the
maximum priority.

When it doesn't find anything, it will interactively ask you what
you want it to do.  The target notes file must be in a parent
folder (direct or otherwise) of the document.

You may pass a prefix ARG in order to make it let you choose the
notes file, even if it finds one."
  (interactive "P")
  (cond
   ;; NOTE(nox): Creating the session from notes file
   ((eq major-mode 'org-mode)
    (let* ((notes-file-path (buffer-file-name))
           (document-property (org-noter--get-or-read-document-property
                               (not (equal arg '(4)))
                               (equal arg '(16))))
           (org-noter-always-create-frame
            (if (and (numberp arg) (= arg 0))
                (not org-noter-always-create-frame)
              org-noter-always-create-frame))
           (ast (org-noter--parse-root (vector (current-buffer) document-property)))
           (session-id (get-text-property (org-element-property :begin ast) org-noter--id-text-property))
           session)

      ;; Check for prefix value
      (if (or (numberp arg) (eq arg '-))
          ;; Yes, user's given a prefix value.
          (cond ((> (prefix-numeric-value arg) 0)
                 ;; Is the prefix value greater than 0?
                 (find-file document-property))
                ;; Open the document like `find-file'.

                ;; Is the prefix value less than 0?
                ((< (prefix-numeric-value arg) 0)
                 ;; Open the folder containing the document.
                 (find-file (file-name-directory document-property))))

        ;; No, user didn't give a prefix value
        ;; NOTE(nox): Check if it is an existing session
        (when session-id
          (setq session (cl-loop for session in org-noter--sessions
                                 when (= (org-noter--session-id session) session-id)
                                 return session))))

      (if session
          (let* ((org-noter--session session)
                 (location (org-noter--parse-location-property
                            (org-noter--get-containing-element))))
            (org-noter--setup-windows session)
            (when location (org-noter--doc-goto-location location))
            (select-frame-set-input-focus (org-noter--session-frame session)))
        ;; It's not an existing session, create a new session.
        (org-noter--create-session ast document-property notes-file-path))))

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; NOTE(nox): Creating the session from the annotated document
   ;;
   ;; eg: M-x org-noter from a pdf document
   ((memq major-mode org-noter-supported-modes)
    ;; if an org-noter sesseion already exists
    (if (org-noter--valid-session org-noter--session)
        (progn (org-noter--setup-windows org-noter--session)
               (select-frame-set-input-focus (org-noter--session-frame org-noter--session)))
      (run-hook-with-args-until-success 'org-noter-create-session-from-document-hook arg buffer-file-name)))))


(defun org-noter--create-session-from-document-file-default (&optional arg document-file-name)
  "Create a new org-noter session from an open document file.
This is the default implementation that is called by
`org-noter-create-session-from-document-hook`.
ARG is the prefix argument passed to `org-noter`
DOCUMENT-FILE-NAME is the document filename."
      ;; NOTE(nox): `buffer-file-truename' is a workaround for modes that delete
      ;; `document-file-name', and may not have the same results
      (let* ((document-file-name (or (run-hook-with-args-until-success 'org-noter-get-buffer-file-name-hook major-mode)
                                   document-file-name))
             (document-path (or document-file-name buffer-file-truename
                                (error "This buffer does not seem to be visiting any file")))
             (document-name (file-name-nondirectory document-path))
             (document-base (file-name-base document-name))
             (document-directory (if document-file-name
                                     (file-name-directory document-file-name)
                                   (if (file-equal-p document-name buffer-file-truename)
                                       default-directory
                                     (file-name-directory buffer-file-truename))))
             ;; NOTE(nox): This is the path that is actually going to be used, and should
             ;; be the same as `document-file-name', but is needed for the truename workaround
             (document-used-path (expand-file-name document-name document-directory))

             (search-names (remove nil (append org-noter-default-notes-file-names
                                       (list (concat document-base ".org"))
                                       (list (run-hook-with-args-until-success 'org-noter-find-additional-notes-functions document-path)))))
             notes-files-annotating ; List of files annotating document
             notes-files ; List of found notes files (annotating or not)

             (document-location (org-noter--doc-approx-location)))

        ;; NOTE(nox): Check the search path
        (dolist (path org-noter-notes-search-path)
          (dolist (name search-names)
            (let ((file-name (expand-file-name name path)))
              (when (file-exists-p file-name)
                (push file-name notes-files)
                (when (org-noter--check-if-document-is-annotated-on-file document-path file-name)
                  (push file-name notes-files-annotating))))))

        ;; NOTE(nox): `search-names' is in reverse order, so we only need to (push ...)
        ;; and it will end up in the correct order
        (dolist (name search-names)
          (let ((directory (locate-dominating-file document-directory name))
                file)
            (when directory
              (setq file (expand-file-name name directory))
              (unless (member file notes-files) (push file notes-files))
              (when (org-noter--check-if-document-is-annotated-on-file document-path file)
                (push file notes-files-annotating)))))

        (setq search-names (nreverse search-names))

        (when (or arg (not notes-files-annotating))
          (when (or arg (not notes-files))
            (let* ((notes-file-name (completing-read "What name do you want the notes to have? "
                                                     search-names nil t))
                   list-of-possible-targets
                   target)

              ;; NOTE(nox): Create list of targets from current path
              (catch 'break
                (let ((current-directory document-directory)
                      file-name)
                  (while t
                    (setq file-name (expand-file-name notes-file-name current-directory))
                    (when (file-exists-p file-name)
                      (setq file-name (propertize file-name 'display
                                                  (concat file-name
                                                          (propertize " -- Exists!" 'face '(:foregorund "green")))))
                      (push file-name list-of-possible-targets)
                      (throw 'break nil))

                    (push file-name list-of-possible-targets)

                    (when (string= current-directory
                                   (setq current-directory
                                         (file-name-directory (directory-file-name current-directory))))
                      (throw 'break nil)))))
              (setq list-of-possible-targets (nreverse list-of-possible-targets))

              ;; NOTE(nox): Create list of targets from search path
              (dolist (path org-noter-notes-search-path)
                (when (file-exists-p path)
                  (let ((file-name (expand-file-name notes-file-name path)))
                    (unless (member file-name list-of-possible-targets)
                      (when (file-exists-p file-name)
                        (setq file-name (propertize file-name 'display
                                                    (concat file-name
                                                            (propertize " -- Exists!" 'face '(:foreground "green"))))))
                      (push file-name list-of-possible-targets)))))

              (setq target (completing-read "Where do you want to save it? " list-of-possible-targets
                                            nil t))
              (set-text-properties 0 (length target) nil target)
              (unless (file-exists-p target) (write-region "" nil target))

              (setq notes-files (list target))))

          (when (> (length notes-files) 1)
            (setq notes-files (list (completing-read "In which notes file should we create the heading? "
                                                     notes-files nil t))))

          (if (member (car notes-files) notes-files-annotating)
              ;; NOTE(nox): This is needed in order to override with the arg
              (setq notes-files-annotating notes-files)
            (with-current-buffer (find-file-noselect (car notes-files))
              (goto-char (point-max))
              (insert (if (save-excursion (beginning-of-line) (looking-at "[[:space:]]*$")) "" "\n")
                      "* "
                      org-noter-headline-title-decoration
                      document-base
                      org-noter-headline-title-decoration)
              (org-entry-put nil org-noter-property-doc-file
                             (file-relative-name document-used-path
                                                 (file-name-directory (car notes-files)))))
            (setq notes-files-annotating notes-files)))

        (when (> (length (delete-dups notes-files-annotating)) 1)
          (setq notes-files-annotating (list (completing-read "Which notes file should we open? "
                                                              notes-files-annotating nil t))))

        (with-current-buffer (find-file-noselect (car notes-files-annotating))
          (org-with-point-at (point-min)
            (catch 'break
              (while (re-search-forward (org-re-property org-noter-property-doc-file) nil)
                (when (file-equal-p (expand-file-name (match-string 3)
                                                      (file-name-directory (car notes-files-annotating)))
                                    document-path)
                  (if-let ((saved-location (org-entry-get nil org-noter-property-note-location)))
                      (setq document-location (cons (string-to-number saved-location) 0)))
                  (let ((org-noter--start-location-override document-location))
                    (org-noter arg))
                  (throw 'break t))))))))

;;;###autoload
(defun org-noter-start-from-dired ()
  "In Dired, open sessions for marked files or file at point.

If there are multiple marked files, focus will be on the last
marked file."
  (interactive)
  (let ((files (or (dired-get-marked-files)
                   (dired-get-filename))))
    (dolist (filename files)
      (find-file filename)
      (save-excursion (org-noter))
      (bury-buffer))
    (other-frame 1)))


(defun org-noter-enable-org-roam-integration ()
  "Enable org-roam integration."
  (interactive)
  (load "org-noter-org-roam")
  (setq org-noter-create-session-from-document-hook
      '(org-noter--create-session-from-document-file-supporting-org-roam)))

(provide 'org-noter)

;;; org-noter.el ends here
