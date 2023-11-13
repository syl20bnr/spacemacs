;;; org-noter-pdf.el --- Modules for PDF-Tools and DocView mode  -*- lexical-binding: t; -*-

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
(eval-when-compile (require 'subr-x))
(require 'cl-lib)
(require 'org-noter-core)
(eval-when-compile ; ensure that the compiled code knows about PDF-TOOLS, if installed
  (condition-case nil
      (require 'pdf-tools)
    (error (message "`pdf-tools' package not found"))))
(condition-case nil ; inform user at run time if pdf-tools is missing
    (require 'pdf-tools)
  (error (message "ATTENTION: org-noter-pdf has many featues that depend on the package `pdf-tools'")))

(push "pdf" org-noter--doc-extensions)
(cl-defstruct pdf-highlight page coords)

(defun org-noter-pdf--get-highlight ()
  "If there's an active pdf selection, returns a  that contains all
the relevant info (page, coordinates)

Otherwise returns nil"
    (if-let* ((_ (pdf-view-active-region-p))
               (page (image-mode-window-get 'page))
               (coords (pdf-view-active-region)))
       (make-pdf-highlight :page page :coords coords)
      nil))

(add-to-list 'org-noter--get-highlight-location-hook 'org-noter-pdf--get-highlight)

(defun org-noter-pdf--pretty-print-highlight (highlight-info)
  (format "%s" highlight-info))

(add-to-list 'org-noter--pretty-print-highlight-location-hook #'org-noter-pdf--pretty-print-highlight)

(defun org-noter-pdf--approx-location-cons (mode &optional precise-info _force-new-ref)
  "Return location as a cons cell.
Runs when MODE is `doc-view-mode' or `pdf-view-mode'

Returns page location as (page . 0).  When processing
PRECISE-INFO, return (page v-pos) or (page v-pos . h-pos)."
  (when (memq mode '(doc-view-mode pdf-view-mode))
    (cons (image-mode-window-get 'page) (if (or (numberp precise-info)
                                                (and (consp precise-info)
                                                     (numberp (car precise-info))
                                                     (numberp (cdr precise-info))))
                                            precise-info 0))))

(add-to-list 'org-noter--doc-approx-location-hook #'org-noter-pdf--approx-location-cons)

(defun org-noter-pdf--get-buffer-file-name (&optional _mode)
  "Return the file naming backing the document buffer.

MODE (unused) is required for this type of hook."
  (bound-and-true-p pdf-file-name))

(add-to-list 'org-noter-get-buffer-file-name-hook #'org-noter-pdf--get-buffer-file-name)

(defun org-noter-pdf--pdf-view-setup-handler (mode)
  (when (eq mode 'pdf-view-mode)
    ;; (setq buffer-file-name document-path)
    (pdf-view-mode)
    (add-hook 'pdf-view-after-change-page-hook 'org-noter--doc-location-change-handler nil t)
    t))

(add-to-list 'org-noter-set-up-document-hook #'org-noter-pdf--pdf-view-setup-handler)

(defun org-noter-pdf--doc-view-setup-handler (mode)
  (when (eq mode 'doc-view-mode)
    ;; (setq buffer-file-name document-path)
    (doc-view-mode)
    (advice-add 'doc-view-goto-page :after 'org-noter--location-change-advice)
    t))

(add-to-list 'org-noter-set-up-document-hook #'org-noter-pdf--doc-view-setup-handler)

(defun org-noter-pdf--no-sessions-remove-advice ()
  "Remove doc-view-specific advice when all sessions are closed."
  (advice-remove 'doc-view-goto-page 'org-noter--location-change-advice))

(add-to-list 'org-noter--no-sessions-remove-advice-hooks #'org-noter-pdf--no-sessions-remove-advice)

(defun org-noter-pdf--pretty-print-location (location)
  "Formats LOCATION with full precision for property drawers."
  (org-noter--with-valid-session
   (when (memq (org-noter--session-doc-mode session) '(doc-view-mode pdf-view-mode))
     (format "%s" (if (or (not (org-noter--get-location-top location)) (<= (org-noter--get-location-top location) 0))
                      (car location)
                    location)))))

(add-to-list 'org-noter--pretty-print-location-hook #'org-noter-pdf--pretty-print-location)

(defun org-noter-pdf--pretty-print-location-for-title (location)
  "Convert LOCATION to a human readable format.
With `pdf-view-mode', the format uses pagelabel and vertical and
horizontal percentages.  With `doc-view-mode', this falls back to
original pretty-print function."
  (org-noter--with-valid-session
   (let ((mode (org-noter--session-doc-mode session))
         (vpos (org-noter--get-location-top location))
         (hpos (org-noter--get-location-left location))
         (vtxt "") (htxt "")
         pagelabel)
     (cond ((eq mode 'pdf-view-mode) ; for default title, reference pagelabel instead of page
            (if (> hpos 0)
                (setq htxt (format " H: %d%%" (round (* 100 hpos)))))
            (if (or (> vpos 0) (> hpos 0))
                (setq vtxt (format " V: %d%%" (round (* 100 vpos)))))
            (select-window (org-noter--get-doc-window))
            (setq pagelabel (pdf-view-current-pagelabel))
            (select-window (org-noter--get-notes-window))
            (format "%s%s%s" pagelabel vtxt htxt))
           ((eq mode 'doc-view-mode) ; fall back to original pp for doc-mode
            (org-noter-pdf--pretty-print-location location))))))

(add-to-list 'org-noter--pretty-print-location-for-title-hook #'org-noter-pdf--pretty-print-location-for-title)

(defun org-noter-pdf--pdf-view-get-precise-info (mode window)
  (when (eq mode 'pdf-view-mode)
    (let (v-position h-position)
      (if (pdf-view-active-region-p)
          (let ((edges (car (pdf-view-active-region))))
            (setq v-position (min (nth 1 edges) (nth 3 edges))
                  h-position (min (nth 0 edges) (nth 2 edges))))

        (let ((event nil))
          (while (not (and (eq 'mouse-1 (car event))
                           (eq window (posn-window (event-start event)))))
            (setq event (read-event "Click where you want the start of the note to be!")))
          (let* ((col-row (posn-col-row (event-start event)))
                 (click-position (org-noter--conv-page-scroll-percentage (+ (window-vscroll) (cdr col-row))
                                                                         (+ (window-hscroll) (car col-row)))))
            (setq v-position (car click-position)
                  h-position (cdr click-position)))))
      (cons v-position h-position))))

(add-to-list 'org-noter--get-precise-info-hook #'org-noter-pdf--pdf-view-get-precise-info)

(defun org-noter-pdf--doc-view-get-precise-info (mode window)
  (when (eq mode 'doc-view-mode)
    (let ((event nil))
      (while (not (and (eq 'mouse-1 (car event))
                       (eq window (posn-window (event-start event)))))
        (setq event (read-event "Click where you want the start of the note to be!")))
      (org-noter--conv-page-scroll-percentage (+ (window-vscroll)
                                                 (cdr (posn-col-row (event-start event))))))))

(add-to-list 'org-noter--get-precise-info-hook #'org-noter-pdf--doc-view-get-precise-info)

(defun org-noter-pdf--goto-location (mode location window)
  (when (memq mode '(doc-view-mode pdf-view-mode))
    (let ((top (org-noter--get-location-top location))
          (left (org-noter--get-location-left location)))

      (if (eq mode 'doc-view-mode)
          (doc-view-goto-page (org-noter--get-location-page location))
        (pdf-view-goto-page (org-noter--get-location-page location))
        ;; NOTE(nox): This timer is needed because the tooltip may introduce a delay,
        ;; so syncing multiple pages was slow
        (when (>= org-noter-arrow-delay 0)
          (when org-noter--arrow-location (cancel-timer (aref org-noter--arrow-location 0)))
          (setq org-noter--arrow-location
                (vector (run-with-idle-timer org-noter-arrow-delay nil 'org-noter--show-arrow)
                        window
                        top
                        left))))
      (image-scroll-up (- (org-noter--conv-page-percentage-scroll top)
                          (floor (+ (window-vscroll) org-noter-vscroll-buffer)))))))

(add-to-list 'org-noter--doc-goto-location-hook #'org-noter-pdf--goto-location)

(defun org-noter-pdf--get-current-view (mode)
  (when (memq mode '(doc-view-mode pdf-view-mode))
    (vector 'paged (car (org-noter-pdf--approx-location-cons mode)))))

(add-to-list 'org-noter--get-current-view-hook #'org-noter-pdf--get-current-view)

(defun org-noter-pdf--get-selected-text (mode)
  (when (and (eq mode 'pdf-view-mode)
             (pdf-view-active-region-p))
    (mapconcat 'identity (pdf-view-active-region-text) ? )))

(add-to-list 'org-noter-get-selected-text-hook #'org-noter-pdf--get-selected-text)

;; NOTE(nox): From machc/pdf-tools-org
(defun org-noter-pdf--edges-to-region (edges)
  "Get 4-entry region (LEFT TOP RIGHT BOTTOM) from several EDGES."
  (when edges
    (let ((left0 (nth 0 (car edges)))
          (top0 (nth 1 (car edges)))
          (bottom0 (nth 3 (car edges)))
          (top1 (nth 1 (car (last edges))))
          (right1 (nth 2 (car (last edges))))
          (bottom1 (nth 3 (car (last edges)))))
      (list left0
            (+ top0 (/ (- bottom0 top0) 3))
            right1
            (- bottom1 (/ (- bottom1 top1) 3))))))

(defalias 'org-noter--pdf-tools-edges-to-region 'org-noter-pdf--edges-to-region
  "For ORG-NOTER-PDFTOOLS backward compatiblity.  The name of the
underlying function is currently under discussion")

(defun org-noter-pdf--create-skeleton (mode)
  "Create notes skeleton with the PDF outline or annotations."
  (when (eq mode 'pdf-view-mode)
    (org-noter--with-valid-session
     (let* ((ast (org-noter--parse-root))
            (top-level (or (org-element-property :level ast) 0))
            (options '(("Outline" . (outline))
                       ("Annotations" . (annots))
                       ("Both" . (outline annots))))
            answer output-data)
       (with-current-buffer (org-noter--session-doc-buffer session)
         (setq answer (assoc (completing-read "What do you want to import? " options nil t) options))

         (when (memq 'outline answer)
           (dolist (item (pdf-info-outline))
             (let ((type (alist-get 'type item))
                   (page (alist-get 'page item))
                   (depth (alist-get 'depth item))
                   (title (alist-get 'title item))
                   (top (alist-get 'top item)))
               (when (and (eq type 'goto-dest) (> page 0))
                 (push (vector title (cons page top) (1+ depth) nil) output-data)))))

         (when (memq 'annots answer)
           (let ((possible-annots (list '("Highlights" . highlight)
                                        '("Underlines" . underline)
                                        '("Squigglies" . squiggly)
                                        '("Text notes" . text)
                                        '("Strikeouts" . strike-out)
                                        '("Links" . link)
                                        '("ALL" . all)))
                 chosen-annots insert-contents pages-with-links)
             (while (> (length possible-annots) 1)
               (let* ((chosen-string (completing-read "Which types of annotations do you want? "
                                                      possible-annots nil t))
                      (chosen-pair (assoc chosen-string possible-annots)))
                 (cond ((eq (cdr chosen-pair) 'all)
                        (dolist (annot possible-annots)
                          (when (and (cdr annot) (not (eq (cdr annot) 'all)))
                            (push (cdr annot) chosen-annots)))
                        (setq possible-annots nil))
                       ((cdr chosen-pair)
                        (push (cdr chosen-pair) chosen-annots)
                        (setq possible-annots (delq chosen-pair possible-annots))
                        (when (= 1 (length chosen-annots)) (push '("DONE") possible-annots)))
                       (t
                        (setq possible-annots nil)))))

             (setq insert-contents (y-or-n-p "Should we insert the annotations contents? "))

             (dolist (item (pdf-info-getannots))
               (let* ((type (alist-get 'type item))
                      (page (alist-get 'page item))
                      (edges (or (org-noter-pdf--edges-to-region (alist-get 'markup-edges item))
                                 (alist-get 'edges item)))
                      (top (nth 1 edges))
                      (item-subject (alist-get 'subject item))
                      (item-contents (alist-get 'contents item))
                      name contents)
                 (when (and (memq type chosen-annots) (> page 0))
                   (if (eq type 'link)
                       (cl-pushnew page pages-with-links)
                     (setq name (cond ((eq type 'highlight) "Highlight")
                                      ((eq type 'underline) "Underline")
                                      ((eq type 'squiggly) "Squiggly")
                                      ((eq type 'text) "Text note")
                                      ((eq type 'strike-out) "Strikeout")))

                     (when insert-contents
                       (setq contents (cons (pdf-info-gettext page edges)
                                            (and (or (and item-subject (> (length item-subject) 0))
                                                     (and item-contents (> (length item-contents) 0)))
                                                 (concat (or item-subject "")
                                                         (if (and item-subject item-contents) "\n" "")
                                                         (or item-contents ""))))))

                     (push (vector (format "%s on page %d" name page) (cons page top) 'inside contents)
                           output-data)))))

             (dolist (page pages-with-links)
               (let ((links (pdf-info-pagelinks page))
                     type)
                 (dolist (link links)
                   (setq type (alist-get 'type link))
                   (unless (eq type 'goto-dest) ;; NOTE(nox): Ignore internal links
                     (let* ((edges (alist-get 'edges link))
                            (title (alist-get 'title link))
                            (top (nth 1 edges))
                            (target-page (alist-get 'page link))
                            target heading-text)

                       (unless (and title (> (length title) 0)) (setq title (pdf-info-gettext page edges)))

                       (cond
                        ((eq type 'uri)
                         (setq target (alist-get 'uri link)
                               heading-text (format "Link on page %d: [[%s][%s]]" page target title)))

                        ((eq type 'goto-remote)
                         (setq target (concat "file:" (alist-get 'filename link))
                               heading-text (format "Link to document on page %d: [[%s][%s]]" page target title))
                         (when target-page
                           (setq heading-text (concat heading-text (format " (target page: %d)" target-page)))))

                        (t (error "Unexpected link type")))

                       (push (vector heading-text (cons page top) 'inside nil) output-data))))))))


         (when output-data
           (if (memq 'annots answer)
               (setq output-data
                     (sort output-data
                           (lambda (e1 e2)
                             (or (not (aref e1 1))
                                 (and (aref e2 1)
                                      (org-noter--compare-locations '< (aref e1 1) (aref e2 1)))))))
             (setq output-data (nreverse output-data)))

           (push (vector "Skeleton" nil 1 nil) output-data)))

       (with-current-buffer (org-noter--session-notes-buffer session)
         ;; NOTE(nox): org-with-wide-buffer can't be used because we want to reset the
         ;; narrow region to include the new headings
         (widen)
         (save-excursion
           (goto-char (org-element-property :end ast))

           (let (last-absolute-level
                 title location relative-level contents
                 level)
             (dolist (data output-data)
               (setq title (aref data 0)
                     location (aref data 1)
                     relative-level (aref data 2)
                     contents (aref data 3))

               (if (symbolp relative-level)
                   (setq level (1+ last-absolute-level))
                 (setq last-absolute-level (+ top-level relative-level)
                       level last-absolute-level))

               (org-noter--insert-heading level title)

               (when location
                 (org-entry-put nil org-noter-property-note-location (org-noter--pretty-print-location location)))

               (when org-noter-doc-property-in-notes
                 (org-entry-put nil org-noter-property-doc-file (org-noter--session-property-text session))
                 (org-entry-put nil org-noter--property-auto-save-last-location "nil"))

               (when (car contents)
                 (org-noter--insert-heading (1+ level) "Contents")
                 (insert (car contents)))
               (when (cdr contents)
                 (org-noter--insert-heading (1+ level) "Comment")
                 (insert (cdr contents)))))

           (setq ast (org-noter--parse-root))
           (org-noter--narrow-to-root ast)
           (goto-char (org-element-property :begin ast))
           (outline-hide-subtree)
           (org-show-children 2)))
       output-data))))

(add-to-list 'org-noter-create-skeleton-functions #'org-noter-pdf--create-skeleton)

(defun org-noter-pdf--create-missing-annotation ()
  "Add a highlight from a selected note."
  (let ((location (org-noter--parse-location-property (org-noter--get-containing-element)))
        (window (org-noter--get-doc-window)))
    (org-noter-pdf--goto-location 'pdf-view-mode location window)
    (pdf-annot-add-highlight-markup-annotation (cdr location))))

(defun org-noter-pdf--highlight-location (mode precise-location)
  "Highlight a precise location in PDF."
  (message "---> %s %s" mode precise-location)
  (when (and (memq mode '(doc-view-mode pdf-view-mode))
             (pdf-view-active-region-p))
    (pdf-annot-add-highlight-markup-annotation (pdf-view-active-region))))

(add-to-list 'org-noter--add-highlight-hook #'org-noter-pdf--highlight-location)

(defun org-noter-pdf--convert-to-location-cons (location)
  "Encode precise LOCATION as a cons cell for note insertion ordering.
Converts (page v . h) precise locations to (page v') such that
v' represents the fractional distance through the page along
columns, so it takes values between 0 and the number of columns.
Each column is specified by its right edge as a fractional
horizontal position.  Output is nil for standard notes and (page
v') for precise notes."
  (if-let* ((_ (and (consp location) (consp (cdr location))))
            (column-edges-string (when (derived-mode-p 'org-mode) (org-entry-get nil "COLUMN_EDGES" t)))
            (right-edge-list (car (read-from-string column-edges-string)))
            ;;(ncol (length left-edge-list))
            (page (car location))
            (v-pos (cadr location))
            (h-pos (cddr location))
            (column-index (seq-position right-edge-list h-pos #'>=)))
      (cons page (+ v-pos column-index))))

(add-to-list 'org-noter--convert-to-location-cons-hook #'org-noter-pdf--convert-to-location-cons)

(defun org-noter-pdf--show-arrow ()
  ;; From `pdf-util-tooltip-arrow'.
  (pdf-util-assert-pdf-window)
  (let* (x-gtk-use-system-tooltips
         (arrow-top  (aref org-noter--arrow-location 2)) ; % of page
         (arrow-left (aref org-noter--arrow-location 3))
         (image-top  (if (floatp arrow-top)
                         (round (* arrow-top  (cdr (pdf-view-image-size)))))) ; pixel location on page (magnification-dependent)
         (image-left (if (floatp arrow-left)
                         (floor (* arrow-left (car (pdf-view-image-size))))))
         (dx (or image-left
                 (+ (or (car (window-margins)) 0)
                    (car (window-fringes)))))
         (dy (or image-top 0))
         (pos (list dx dy dx (+ dy (* 2 (frame-char-height)))))
         (vscroll (pdf-util-required-vscroll pos))
         (tooltip-frame-parameters
          `((border-width . 0)
            (internal-border-width . 0)
            ,@tooltip-frame-parameters))
         (tooltip-hide-delay 3))

    (when vscroll
      (image-set-window-vscroll vscroll))
    (setq dy (max 0 (- dy
                       (cdr (pdf-view-image-offset))
                       (window-vscroll nil t)
                       (frame-char-height))))
    (when (overlay-get (pdf-view-current-overlay) 'before-string)
      (let* ((e (window-inside-pixel-edges))
             (xw (pdf-util-with-edges (e) e-width))
             (display-left-margin (/ (- xw (car (pdf-view-image-size t))) 2)))
        (cl-incf dx display-left-margin)))
    (setq dx (max 0 (+ dx org-noter-arrow-horizontal-offset)))
    (pdf-util-tooltip-in-window
     (propertize
      " " 'display (propertize
                    "\u2192" ;; right arrow
                    'display '(height 2)
                    'face `(:foreground
                            ,org-noter-arrow-foreground-color
                            :background
                            ,(if (bound-and-true-p pdf-view-midnight-minor-mode)
                                 (cdr pdf-view-midnight-colors)
                               org-noter-arrow-background-color))))
     dx dy)))

(add-to-list 'org-noter--show-arrow-hook #'org-noter-pdf--show-arrow)

(defun org-noter-pdf-set-columns (num-columns)
  "Interactively set the COLUMN_EDGES property for the current heading.
NUM-COLUMNS can be given as an integer prefix or in the
minibuffer.  The user is then prompted to click on the right edge
of each column, except for the last one.  Subheadings of the
current heading inherit the COLUMN_EDGES property."
  (interactive "NEnter number of columns: ")
  (select-window (org-noter--get-doc-window))
  (let (event
        edge-list
        (window (car (window-list))))
    (dotimes (ii (1- num-columns))
      (while (not (and (eq 'mouse-1 (car event))
                       (eq window (posn-window (event-start event)))))
        (setq event (read-event (format "Click on the right boundary of column %d" (1+ ii)))))
      (let* ((col-row (posn-col-row (event-start event)))
             (click-position (org-noter--conv-page-scroll-percentage (+ (window-vscroll) (cdr col-row))
                                                                     (+ (window-hscroll) (car col-row))))
             (h-position (cdr click-position)))
        (setq event nil)
        (setq edge-list (append edge-list (list h-position)))))
    (setq edge-list (append edge-list '(1)))
    (select-window (org-noter--get-notes-window))
    (org-entry-put nil "COLUMN_EDGES" (format "%s" (princ edge-list)))))

;;; override some deleterious keybindings in pdf-view-mode.
(define-key org-noter-doc-mode-map (kbd "C-c C-c")
  (defun org-noter-pdf--execute-CcCc-in-notes ()
    "Override C-c C-c in pdf document buffer."
    (interactive)
    (select-window (org-noter--get-notes-window))
    (org-ctrl-c-ctrl-c)))

(define-key org-noter-doc-mode-map (kbd "C-c C-x")
  (defun org-noter-pdf--execute-CcCx-in-notes ()
    "Override C-c C-x <event> in pdf document buffer."
    (interactive)
    (let ((this-CxCc-cmd (vector (read-event))))
      (select-window (org-noter--get-notes-window))
      (execute-kbd-macro
       (vconcat (kbd "C-c C-x") this-CxCc-cmd)))))

(provide 'org-noter-pdf)
;;; org-noter-pdf.el ends here
