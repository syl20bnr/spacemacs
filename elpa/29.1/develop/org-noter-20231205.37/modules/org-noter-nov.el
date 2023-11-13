;;; org-noter-nov.el --- Integration with Nov.el     -*- lexical-binding: t; -*-

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

(eval-when-compile ; ensure that the compiled code knows about NOV, if installed
  (condition-case nil
      (require 'nov)
    (error (message "`nov' package not found"))))
(condition-case nil ; run time warning
    (require 'nov)
  (error (message "ATTENTION: org-noter-nov needs the package `nov'")))

(push "epub" org-noter--doc-extensions)

(defvar nov-documents-index)
(defvar nov-file-name)
(defvar-local org-noter--nov-timer nil
  "Timer for synchronizing notes after scrolling.")

(defun org-noter-nov--get-buffer-file-name (&optional _mode)
  (bound-and-true-p nov-file-name))

(add-to-list 'org-noter-get-buffer-file-name-hook #'org-noter-nov--get-buffer-file-name)

(defun org-noter-nov--approx-location-cons (mode &optional precise-info _force-new-ref)
  (when (eq mode 'nov-mode)
    (cons nov-documents-index (if (or (numberp precise-info)
                                      (and (consp precise-info)
                                           (numberp (car precise-info))
                                           (numberp (cdr precise-info))))
                                  precise-info
                                (max 1 (/ (+ (window-start) (window-end nil t)) 2))))))

(add-to-list 'org-noter--doc-approx-location-hook #'org-noter-nov--approx-location-cons)

(defun org-noter-nov--scroll-handler (&rest _)
  (when org-noter--nov-timer (cancel-timer org-noter--nov-timer))
  (unless org-noter--inhibit-location-change-handler
    (setq org-noter--nov-timer (run-with-timer 0.25 nil 'org-noter--doc-location-change-handler))))

(defun org-noter-nov--setup-handler (mode)
  (when (eq mode 'nov-mode)
    (advice-add 'nov-render-document :after 'org-noter-nov--scroll-handler)
    (add-hook 'window-scroll-functions 'org-noter-nov--scroll-handler nil t)
    t))

(add-to-list 'org-noter-set-up-document-hook #'org-noter-nov--setup-handler)

(defun org-noter-nov--no-sessions-remove-advice ()
  "Remove nov-specific advice when all sessions are closed."
  (advice-remove 'nov-render-document 'org-noter-nov--scroll-handler))

(add-to-list 'org-noter--no-sessions-remove-advice-hooks #'org-noter-nov--no-sessions-remove-advice)

(defun org-noter-nov--pretty-print-location (location)
  (org-noter--with-valid-session
   (when (eq (org-noter--session-doc-mode session) 'nov-mode)
     (format "%s" (if (or (not (org-noter--get-location-top location)) (<= (org-noter--get-location-top location) 1))
                      (org-noter--get-location-page location)
                    location)))))

(add-to-list 'org-noter--pretty-print-location-hook #'org-noter-nov--pretty-print-location)
(add-to-list 'org-noter--pretty-print-location-for-title-hook #'org-noter-nov--pretty-print-location)

(defun org-noter-nov--get-precise-info (mode window)
  (when (eq mode 'nov-mode)
    (if (region-active-p)
        (cons (mark) (point))
      (let ((event nil))
        (while (not (and (eq 'mouse-1 (car event))
                         (eq window (posn-window (event-start event)))))
          (setq event (read-event "Click where you want the start of the note to be!")))
        (posn-point (event-start event))))))

(add-to-list 'org-noter--get-precise-info-hook #'org-noter-nov--get-precise-info)

(defun org-noter-nov--goto-location (mode location &optional _window)
  (when (eq mode 'nov-mode)
    (setq nov-documents-index (org-noter--get-location-page location))
    (nov-render-document)
    (goto-char (org-noter--get-location-top location))
    ;; NOTE(nox): This needs to be here, because it would be issued anyway after
    ;; everything and would run org-noter--nov-scroll-handler.
    (recenter)))

(add-to-list 'org-noter--doc-goto-location-hook #'org-noter-nov--goto-location)

(defun org-noter-nov--get-current-view (mode)
  (when (eq mode 'nov-mode)
    (vector 'nov
            (org-noter-nov--approx-location-cons mode (window-start))
            (org-noter-nov--approx-location-cons mode (window-end nil t)))))

(add-to-list 'org-noter--get-current-view-hook #'org-noter-nov--get-current-view)

(defun org-noter-nov--get-selected-text (mode)
  (when (and (eq mode 'nov-mode) (region-active-p))
    (buffer-substring-no-properties (mark) (point))))

(add-to-list 'org-noter-get-selected-text-hook #'org-noter-nov--get-selected-text)


;; Shamelessly stolen code from Yuchen Li.
;; This code is originally from org-noter-plus package.
;; At https://github.com/yuchen-lea/org-noter-plus

(defun org-noter-nov--handle-toc-item (ol depth)
  (mapcar (lambda (li)
            (mapcar (lambda (a-or-ol)
                      (pcase-exhaustive (dom-tag a-or-ol)
                        ('a
                         (vector :depth depth
                                 :title (dom-text a-or-ol)
                                 :href (esxml-node-attribute 'href a-or-ol)))
                        ('ol
                         (org-noter-nov--handle-toc-item a-or-ol
                                                         (1+ depth)))))
                    (dom-children li)))
          (dom-children ol)))

(defun org-noter-nov--create-skeleton-epub (mode)
  "Epub outline with nov link."
  (when (eq mode 'nov-mode)
    (require 'esxml)
    (require 'nov)
    (require 'dom)
    (org-noter--with-valid-session
     (let* ((ast (org-noter--parse-root))
            (top-level (or (org-element-property :level ast) 0))
            output-data)
       (with-current-buffer (org-noter--session-doc-buffer session)
         (let* ((toc-path (cdr (aref nov-documents 0)))
                (toc-tree (with-temp-buffer
                            (insert (nov-ncx-to-html toc-path))
                            (goto-char (point-min))
                            (while (re-search-forward "\n" nil t)
                              (replace-match "" nil nil))
                            (libxml-parse-html-region (point-min)
                                                      (point-max))))
                (origin-index nov-documents-index)
                (origin-point (point)))
           (dolist (item
                    (nreverse (flatten-tree (org-noter-nov--handle-toc-item toc-tree 1))))
             (let ((relative-level  (aref item 1))
                   (title  (aref item 3))
                   (url (aref item 5)))
               (apply 'nov-visit-relative-file
                      (nov-url-filename-and-target url))
               (when (not (integerp nov-documents-index))
                 (setq nov-documents-index 0))
               (push (vector title (list nov-documents-index (point)) relative-level) output-data)))
           (push (vector "Skeleton" (list 0) 1) output-data)

           (nov-goto-document origin-index)
           (goto-char origin-point)))
       (save-excursion
         (goto-char (org-element-property :end ast))
         (with-current-buffer (org-noter--session-notes-buffer session)
           (dolist (data output-data)
             (let* ((title          (aref data 0))
                    (location       (aref data 1))
                    (relative-level (aref data 2))
                    (last-absolute-level (+ top-level relative-level))
                    (level last-absolute-level))

               (org-noter--insert-heading level title)

               (when location
                 (org-entry-put nil org-noter-property-note-location (org-noter--pretty-print-location location)))

               (when org-noter-doc-property-in-notes
                 (org-entry-put nil org-noter-property-doc-file (org-noter--session-property-text session))
                 (org-entry-put nil org-noter--property-auto-save-last-location "nil"))))
           (setq ast (org-noter--parse-root))
           (org-noter--narrow-to-root ast)
           (goto-char (org-element-property :begin ast))
           (outline-hide-subtree)
           (org-show-children 2)))
       output-data))))

(add-to-list 'org-noter-create-skeleton-functions #'org-noter-nov--create-skeleton-epub)

(provide 'org-noter-nov)
;;; org-noter-nov.el ends here
