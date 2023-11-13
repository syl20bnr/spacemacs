;;; org-noter-djvu.el --- Module for DJVU            -*- lexical-binding: t; -*-

;; Copyright (C) 2022  c1-g

;; Author: c1-g <char1iegordon@protonmail.com>
;; Keywords: multimedia

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

;;

;;; Code:
(require 'org-noter-core)

(eval-when-compile ; ensure that the compiled code knows about DJVU, if installed
  (condition-case nil
      (require 'djvu)
    (error (message "`djvu' package not found"))))
(condition-case nil ; run time warning
    (require 'djvu)
  (error (message "ATTENTION: org-noter-djvu needs the package `djvu'")))

(push "djvu" org-noter--doc-extensions)

(defun org-noter-djvu--pretty-print-location (location)
  (org-noter--with-valid-session
   (when (eq (org-noter--session-doc-mode session) 'djvu-read-mode)
     (format "%s" (if (or (not (org-noter--get-location-top location)) (<= (org-noter--get-location-top location) 0))
                      (car location)
                    location)))))

(add-to-list 'org-noter--pretty-print-location-hook #'org-noter-djvu--pretty-print-location)
(add-to-list 'org-noter--pretty-print-location-for-title-hook #'org-noter-djvu--pretty-print-location)

(defun org-noter-djvu--approx-location-cons (mode &optional precise-info _force-new-ref)
  (when (eq mode 'djvu-read-mode)
    (cons djvu-doc-page (if (or (numberp precise-info)
                                (and (consp precise-info)
                                     (numberp (car precise-info))
                                     (numberp (cdr precise-info))))
                            precise-info
                          (max 1 (/ (+ (window-start) (window-end nil t)) 2))))))

(add-to-list 'org-noter--doc-approx-location-hook #'org-noter-djvu--approx-location-cons)

(defun org-noter-djvu--get-precise-info (mode window)
  (when (eq mode 'djvu-read-mode)
    (if (region-active-p)
        (cons (mark) (point))
      (let ((event nil))
        (while (not (and (eq 'mouse-1 (car event))
                         (eq window (posn-window (event-start event)))))
          (setq event (read-event "Click where you want the start of the note to be!")))
        (posn-point (event-start event))))))

(add-to-list 'org-noter--get-precise-info-hook #'org-noter-djvu--get-precise-info)

(defun org-noter-djvu--setup-handler (mode)
  (when (eq mode 'djvu-read-mode)
    (advice-add 'djvu-init-page :after 'org-noter--location-change-advice)
    t))

(add-to-list 'org-noter-set-up-document-hook #'org-noter-djvu--setup-handler)

(defun org-noter-djvu--goto-location (mode location &optional window)
  "DJVU mode function for `org-noter--doc-goto-location-hook'.
MODE is the document mode and LOCATION is the note location.
WINDOW is required by the hook, but not used in this function."
  (when (eq mode 'djvu-read-mode)
    (djvu-goto-page (car location))
    (goto-char (org-noter--get-location-top location))))

(add-to-list 'org-noter--doc-goto-location-hook #'org-noter-djvu--goto-location)

(defun org-noter-djvu--get-current-view (mode)
  (when (eq mode 'djvu-read-mode)
    (vector 'paged (car (org-noter-djvu--approx-location-cons mode)))))

(add-to-list 'org-noter--get-current-view-hook #'org-noter-djvu--get-current-view)

(defun org-noter-djvu--get-selected-text (mode)
  (when (and (eq mode 'djvu-read-mode)
             (region-active-p))
    (buffer-substring-no-properties (mark) (point))))

(add-to-list 'org-noter-get-selected-text-hook #'org-noter-djvu--get-selected-text)

(defun org-noter-djvu--create-skeleton (mode)
  (when (eq mode 'djvu-read-mode)
    (org-noter--with-valid-session
     (let* ((ast (org-noter--parse-root))
            (top-level (or (org-element-property :level ast) 0))
            output-data)
       (require 'thingatpt)
       (with-current-buffer (djvu-ref outline-buf)
         (unless (string= (buffer-string) "")
           (push (vector "Skeleton" nil 1) output-data)
           (save-excursion
             (goto-char (point-min))
             (while (not (looking-at "^$"))
               (push (vector (string-trim-right (string-trim (thing-at-point 'line t)) " [[:digit:]]+")
                             (list (string-trim-left (string-trim (thing-at-point 'line t)) ".* "))
                             (+ 2 (how-many "  " (point-at-bol) (point-at-eol)))) output-data)
               (forward-line)))))

       (with-current-buffer (org-noter--session-notes-buffer session)
         ;; NOTE(nox): org-with-wide-buffer can't be used because we want to reset the
         ;; narrow region to include the new headings
         (widen)
         (save-excursion
           (goto-char (org-element-property :end ast))

           (let (last-absolute-level
                 title location relative-level
                 level)

             (dolist (data (nreverse output-data))
               (setq title (aref data 0)
                     location (aref data 1)
                     relative-level (aref data 2))

               (setq last-absolute-level (+ top-level relative-level)
                     level last-absolute-level)

               (org-noter--insert-heading level title)

               (when location
                 (org-entry-put nil org-noter-property-note-location (org-noter--pretty-print-location location)))

               (when org-noter-doc-property-in-notes
                 (org-entry-put nil org-noter-property-doc-file (org-noter--session-property-text session))
                 (org-entry-put nil org-noter--property-auto-save-last-location "nil"))))

           (setq ast (org-noter--parse-root))
           (org-noter--narrow-to-root ast)
           (goto-char (org-element-property :begin ast))
           (when (org-at-heading-p) (outline-hide-subtree))
           (org-show-children 2)))
       output-data))))

(add-to-list 'org-noter-create-skeleton-functions #'org-noter-djvu--create-skeleton)

(provide 'org-noter-djvu)
;;; org-noter-djvu.el ends here
