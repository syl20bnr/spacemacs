;;; org-pdftools.el --- Support for links to documents in pdfview mode  -*- lexical-binding: t; -*-
;; Copyright (C) 2020 Alexander Fu Xi

;; Author: Alexander Fu Xi <fuxialexander@gmail.com>
;; Maintainer: Alexander Fu Xi <fuxialexnader@gmail.com>
;; Homepage: https://github.com/fuxialexander/org-pdftools
;; Version: 1.0
;; Keywords: convenience
;; Package-Requires: ((emacs "26.1") (org "9.3.6") (pdf-tools "0.8") (org-noter "1.4.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Add support for org links from pdftools buffers with more precise location control.
;;
;; https://github.com/fuxialexander/org-pdftools/


;;; Code:
(require 'subr-x)
(require 'cl-lib)
(require 'org)
(require 'org-refile nil t)
(require 'org-noter)
(require 'pdf-tools)
(require 'pdf-view)
(require 'pdf-annot)
(require 'pdf-occur)

(defgroup org-pdftools nil
  "Tools for adding pdftools link support in Org mode."
  :group 'tools)

(defcustom org-pdftools-get-desc-function #'org-pdftools-get-desc-default
  "A function that takes 3 arguments and output a link description.
- `file': basename of the PDF file
- `page': current page number converted to string
- `text' (should have optional tag): additional text infomation like highlighted text or isearch string.
See `org-pdftools-get-desc-default' as an example."

  :group 'org-pdftools
  :type 'function)
(defcustom org-pdftools-path-generator #'abbreviate-file-name
"Translate your PDF file path the way you like. Take buffer-file-name as the argument."
  :group 'org-pdftools
  :type 'function)
(defcustom org-pdftools-path-resolver #'expand-file-name
"Resolve your translated PDF file path back to an absolute path."
  :group 'org-pdftools
  :type 'function)
(defcustom org-pdftools-open-custom-open nil
  "Custom function to open linked pdf files."
  :group 'org-pdftools
  :type '(choice function nil))

(defcustom org-pdftools-use-freepointer-annot nil
  "Whether prompt to use freepointer annotation or not. "
  :group 'org-pdftools
  :type 'boolean)

(defcustom org-pdftools-use-isearch-link nil
  "Whether prompt to use isearch link or not. "
  :group 'org-pdftools
  :type 'boolean)

(defcustom org-pdftools-export-style 'pdftools
  "Export style of org-pdftools links.
- pdftools :: export the link as is
- protocol :: export the link as a org-protocal link such that it could open pdf-tools when clicked
"
  :group 'org-pdftools
  :type 'symbol)
(defcustom org-pdftools-markup-pointer-function 'pdf-annot-add-underline-markup-annotation
  "Color for markup pointer annotations.
Can be one of highlight/underline/strikeout/squiggly."
  :group 'org-pdftools
  :type 'function)
(defcustom org-pdftools-markup-pointer-color "#A9A9A9"
  "Color for markup pointer annotations."
  :group 'org-pdftools
  :type 'string)
(defcustom org-pdftools-markup-pointer-opacity 1.0
  "Opacity for markup pointer annotations."
  :group 'org-pdftools
  :type 'float)
(defcustom org-pdftools-free-pointer-icon "Circle"
  "Color for free pointer annotations. Refer to `pdf-annot-standard-text-icons`."
  :group 'org-pdftools
  :type 'string)
(defcustom org-pdftools-link-prefix "pdf"
  "Prefix for org-pdftools link"
  :group 'org-pdftools
  :type 'string)
(defcustom org-pdftools-search-string-separator "??"
  "Separator of search-string."
  :group 'org-pdftools
  :type 'string)
(defcustom org-pdftools-free-pointer-color "#FFFFFF"
  "Color for free pointer annotations."
  :group 'org-pdftools
  :type 'string)
(defcustom org-pdftools-free-pointer-opacity 1.0
  "Opacity for free pointer annotations."
  :group 'org-pdftools
  :type 'float)


;; pdf://path::page++height_percent;;annot_id??isearch_string or @@occur_search_string
(defun org-pdftools-open-pdftools (link)
  "Internal function to open org-pdftools LINK."
  (let ((link-regexp
         (concat "\\(.*\\)::\\([0-9]*\\)\\(\\+\\+\\)?\\([[0-9]\\.*[0-9]*\\)?\\(;;\\|"
                 (regexp-quote org-pdftools-search-string-separator)
                 "\\)?\\(.*\\)")))
    (cond ((string-match link-regexp link)
           (let ((path (match-string 1 link))
                 (page (match-string 2 link))
                 (height (match-string 4 link))
                 annot-id
                 search-string)
             (cond ((string-equal
                     (match-string 5 link)
                     ";;")
                    (setq annot-id
                          (match-string 6 link)))
                   ((string-equal
                     (match-string 5 link)
                     org-pdftools-search-string-separator)
                    (setq search-string
                          (replace-regexp-in-string
                           "%20"
                           " "
                           (match-string 6 link)))))
             (when (and path
                        (not (string-empty-p path)))
               (if (bound-and-true-p org-noter--session)
                   (org-noter--with-valid-session
                    (let ((doc (with-selected-window
                                   (org-noter--get-doc-window)
                                 buffer-file-name))
                          (fullpath (funcall org-pdftools-path-resolver path)))
                      (if (string-equal doc fullpath)
                          (select-window
                           (org-noter--get-doc-window))
                        (let ((org-link-frame-setup
                               (cl-acons 'file 'find-file-other-frame org-link-frame-setup)))
                          (org-open-file fullpath 1)))))
                 (org-open-file (funcall org-pdftools-path-resolver path) 1)))
             (if (and page
                      (not (string-empty-p page)))
                 (progn
                   (setq page (string-to-number page))
                   (if (bound-and-true-p org-noter--session)
                       (org-noter--with-valid-session
                        (with-selected-window
                            (org-noter--get-doc-window)
                          (pdf-view-goto-page page)))
                     (pdf-view-goto-page page)))
               (setq page nil))
             (when (and height
                        (not (string-empty-p height)))
               (if (bound-and-true-p org-noter--session)
                   (org-noter--with-valid-session
                    (with-selected-window
                        (org-noter--get-doc-window)
                      (image-set-window-vscroll
                       (round
                        (/
                         (*
                          (string-to-number height)
                          (cdr (pdf-view-image-size)))
                         (frame-char-height))))))
                 (image-set-window-vscroll
                  (round
                   (/
                    (*
                     (string-to-number height)
                     (cdr (pdf-view-image-size)))
                    (frame-char-height))))))
             (when (and annot-id
                        (not (string-empty-p annot-id)))
               (if (bound-and-true-p org-noter--session)
                   (org-noter--with-valid-session
                    (with-selected-window
                        (org-noter--get-doc-window)
                      (pdf-annot-show-annotation
                       (pdf-info-getannot annot-id)
                       t)))
                 (pdf-annot-show-annotation
                  (pdf-info-getannot annot-id)
                  t)))
             (when (and search-string
                        (not (string-empty-p search-string)))
               (if (bound-and-true-p org-noter--session)
                   (org-noter--with-valid-session
                    (with-selected-window
                        (org-noter--get-doc-window)
                      (isearch-mode t)
                      (let (pdf-isearch-narrow-to-page t)
                        (isearch-yank-string search-string))
                        ))
                 (isearch-mode t)
                 (let (pdf-isearch-narrow-to-page t)
                   (isearch-yank-string search-string))))))
          ((string-match
            "\\(.*\\)@@\\(.*\\)"
            link)
           (let* ((paths (match-string 1 link))
                  (occur-search-string (match-string 2 link))
                  (pathlist (split-string paths "%&%")))
             (pdf-occur-search
              pathlist
              occur-search-string)))
          ((org-open-file link 1)))))

(defun org-pdftools-get-link ()
  "Get link from the active pdf buffer."
  (let* ((path
          (with-current-buffer (current-buffer)
              (funcall org-pdftools-path-generator (buffer-file-name))))
         (page (pdf-view-current-page))
         (annot-id (if (pdf-view-active-region-p)
                       (pdf-annot-get-id
                        (funcall
                         org-pdftools-markup-pointer-function
                         (pdf-view-active-region)
                         org-pdftools-markup-pointer-color
                         `((opacity . ,org-pdftools-markup-pointer-opacity))))
                     (if (and (not (bound-and-true-p org-noter--session))
                              (pdf-annot-getannots page))
                         (condition-case nil
                             (pdf-annot-get-id
                              (pdf-annot-read-annotation
                               "Click the annotation that you want to link to."))
                           (error
                            (if org-pdftools-use-freepointer-annot
                                (pdf-annot-get-id
                                 (funcall-interactively
                                  #'pdf-annot-add-text-annotation
                                  (pdf-util-read-image-position
                                   "Click where a new text annotation should be added ...")
                                  org-pdftools-free-pointer-icon
                                  `((color . ,org-pdftools-free-pointer-color)
                                    (opacity . ,org-pdftools-free-pointer-opacity))))
                              nil)))
                       (if org-pdftools-use-freepointer-annot
                           (pdf-annot-get-id
                            (funcall-interactively
                             #'pdf-annot-add-text-annotation
                             (pdf-util-read-image-position
                              "Click where a new text annotation should be added ...")
                             org-pdftools-free-pointer-icon
                             `((color . ,org-pdftools-free-pointer-color)
                               (opacity . ,org-pdftools-free-pointer-opacity))))
                         nil))))
         (height (cond ((bound-and-true-p annot-id)
                        (nth 1 (pdf-annot-get
                                (pdf-info-getannot
                                 annot-id
                                 path)
                                'edges)))
                       (t
                        (/
                         (*
                          (or (image-mode-window-get
                               'vscroll)
                              0)
                          (frame-char-height))
                         (float
                          (cdr (pdf-view-image-size)))))))
         ;; pdf://path::page++height_percent;;annot_id\\|??search-string
         (search-string (if (and (not annot-id)
                                 org-pdftools-use-isearch-link)
                            isearch-string
                          ""))
         (link (concat

                org-pdftools-link-prefix ":"
                path
                "::"
                (number-to-string page)
                "++"
                (format "%.2f" height)
                (if annot-id
                    (concat
                     ";;"
                     (symbol-name annot-id))
                  (if (not (string-empty-p search-string))
                      (concat
                       org-pdftools-search-string-separator
                       (replace-regexp-in-string
                        " "
                        "%20"
                        search-string))
                    (message
                     "   Reminder: You haven't performed a isearch!") "")))))
    link))

(defun org-pdftools-get-desc-default (file page &optional text)
  (concat file ".pdf: Page " page (when text (concat "; Quoting: " text))))

;;;###autoload
(defun org-pdftools-open (link)
  "Function to open org-pdftools LINK."
  (if (and (display-graphic-p)
           (featurep 'pdf-tools))
      (org-pdftools-open-pdftools
       link)
    (if (bound-and-true-p org-pdftools-open-custom-open)
        (funcall org-pdftools-open-custom-open link)
      (let* ((path (when (string-match
                          "\\(.+\\)::.+" link)
                     (match-string 1 link))))
        (org-open-file path)))))

;;;###autoload
(defun org-pdftools-store-link ()
  "Store a link to a pdfview/pdfoccur buffer."
  (cond ((eq major-mode 'pdf-view-mode)
         ;; This buffer is in pdf-view-mode
         (let* ((file (file-name-base (pdf-view-buffer-file-name)))
                (quot (if (pdf-view-active-region-p)
                          (replace-regexp-in-string "\n" " "
                                                    (mapconcat 'identity (pdf-view-active-region-text) ? ))))
                (page (number-to-string (pdf-view-current-page)))
                (link (org-pdftools-get-link))
                (isearchstr (if (string-match (concat ".*" (regexp-quote org-pdftools-search-string-separator) "\\(.*\\)") link)
                                (match-string 1 link)))
                (desc (funcall org-pdftools-get-desc-function file page (or quot isearchstr))))
           (org-link-store-props
            :type org-pdftools-link-prefix
            :link link
            :description desc)))
        ((eq major-mode 'pdf-occur-buffer-mode)
         (let* ((paths (mapconcat #'identity (mapcar #'car
                         pdf-occur-search-documents) "%&%"))
                (occur-search-string pdf-occur-search-string)
                (link (concat org-pdftools-link-prefix ":"
                       paths "@@" occur-search-string)))
           (org-link-store-props
            :type org-pdftools-link-prefix
            :link link
            :description (concat "Search: " occur-search-string))))))

;;;###autoload
(defun org-pdftools-export (link description format)
  "Export the pdfview LINK with DESCRIPTION for FORMAT from Org files."
  (let* (path loc page)
    (if (string-match "\\(.+\\)::\\(.*\\)" link)
        (progn
          (setq path (match-string 1 link))
          (setq loc (match-string 2 link))
          (if (string-match "\\([0-9]+\\)++\\(.*\\)" loc)
              (setq page (match-string 1 loc))
            (setq page loc)))
      (setq path link))

    ;; `org-export-file-uri` expands the filename correctly
    (setq path (org-export-file-uri (org-link-escape path)))

    (cond ((eq format 'html)
           (format
            "<a href=\"%s#page=%s\">%s</a>"
            path
            page
            description))
          ((eq format 'latex)
           (format
            "\\href{%s}{%s}"
            path
            description))
          ((eq format 'ascii)
           (format "%s (%s)" description path))
          (t path))))

;;;###autoload
(defun org-pdftools-setup-link (&optional prefix)
  "Set up pdf: links in org-mode."
  (setq org-pdftools-prefix (or prefix org-pdftools-link-prefix))
  (org-link-set-parameters org-pdftools-prefix
                           :follow #'org-pdftools-open
                           :complete #'org-pdftools-complete-link
                           :store #'org-pdftools-store-link
                           :export #'org-pdftools-export))

;;;###autoload
(defun org-pdftools-complete-link (&optional arg)
  "Use the existing file name completion for file.
Links to get the file name, then ask the user for the page number
and append it. ARG is passed to `org-link-complete-file'."
  (let* ((pdf-or-dir-p '(lambda (file-name)
                          (string-match "/$\\|.pdf$" file-name)))
         ;; pdf-or-dir-p, a predicate returns t when file's path
         ;; is a directory or ends with .pdf.
         (current-read-file-name-function read-file-name-function)
         ;; Replace the `read-file-name-function' temporarily,
         ;; See its docstring for more.
         (read-file-name-function
          ;; This is the replacement, it's just a wrapper, it passes
          ;; the same argument to the old read file name function, one
          ;; difference is the PREDICATE arguement being our defined
          ;; pdf-or-dir-p.
          ;; Why?: So that `org-link-complete-file' only "show" pdf file
          ;; or directory.
          (lambda
            (prompt &optional dir default-filename mustmatch initial
                    predicate)
            (funcall current-read-file-name-function
                     prompt dir default-filename mustmatch initial
                     pdf-or-dir-p))))                     
    (concat
     (replace-regexp-in-string
      "^file:"
      (concat org-pdftools-link-prefix ":")
      (org-link-complete-file arg))
     "::"
     (read-from-minibuffer
      "Page:"
      "1"))))

(provide 'org-pdftools)
;;; org-pdftools.el ends here
