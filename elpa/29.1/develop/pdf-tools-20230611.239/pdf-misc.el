;;; pdf-misc.el --- Miscellaneous commands for PDF buffer.  -*- lexical-binding: t; -*-

;; Copyright (C) 2013, 2014  Andreas Politz

;; Author: Andreas Politz <politza@fh-trier.de>
;; Keywords: files, multimedia

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
;;


(require 'pdf-view)
(require 'pdf-util)
(require 'imenu)



;;; Code:

(defvar pdf-misc-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "I") 'pdf-misc-display-metadata)
    (define-key map (kbd "C-c C-p") 'pdf-misc-print-document)
    map)
  "Keymap used in `pdf-misc-minor-mode'.")

;;;###autoload
(define-minor-mode pdf-misc-minor-mode
  "FIXME:  Not documented."
  :group 'pdf-misc)

;;;###autoload
(define-minor-mode pdf-misc-size-indication-minor-mode
  "Provide a working size indication in the mode-line."
  :group 'pdf-misc
  (pdf-util-assert-pdf-buffer)
  (cond
   (pdf-misc-size-indication-minor-mode
    (unless (assq 'pdf-misc-size-indication-minor-mode
                  mode-line-position)
      (setq mode-line-position
            `((pdf-misc-size-indication-minor-mode
               (:eval (pdf-misc-size-indication)))
              ,@mode-line-position))))
   (t
    (setq mode-line-position
          (cl-remove 'pdf-misc-size-indication-minor-mode
                     mode-line-position :key 'car-safe)))))

(defun pdf-misc-size-indication ()
  "Return size indication string for the mode-line."
  (let ((top (= (window-vscroll nil t) 0))
        (bot (>= (+ (- (nth 3 (window-inside-pixel-edges))
                       (nth 1 (window-inside-pixel-edges)))
                    (window-vscroll nil t))
                 (cdr (pdf-view-image-size t)))))
    (cond
     ((and top bot) " All")
     (top " Top")
     (bot " Bot")
     (t (format
         " %d%%%%"
         (ceiling
          (* 100 (/ (float (window-vscroll nil t))
                    (cdr (pdf-view-image-size t))))))))))

(defvar pdf-misc-menu-bar-minor-mode-map (make-sparse-keymap)
  "The keymap used in `pdf-misc-menu-bar-minor-mode'.")

(easy-menu-define nil pdf-misc-menu-bar-minor-mode-map
  "Menu for PDF Tools."
  `("PDF Tools"
    ["Go Backward" pdf-history-backward
     :visible (bound-and-true-p pdf-history-minor-mode)
     :active (and (bound-and-true-p pdf-history-minor-mode)
                  (not (pdf-history-end-of-history-p)))]
    ["Go Forward" pdf-history-forward
     :visible (bound-and-true-p pdf-history-minor-mode)
     :active (not (pdf-history-end-of-history-p))]
    ["--" nil
     :visible (derived-mode-p 'pdf-virtual-view-mode)]
    ["Next file" pdf-virtual-buffer-forward-file
     :visible  (derived-mode-p 'pdf-virtual-view-mode)
     :active (pdf-virtual-document-next-file
              (pdf-view-current-page))]
    ["Previous file" pdf-virtual-buffer-backward-file
     :visible (derived-mode-p 'pdf-virtual-view-mode)
     :active (not (eq 1 (pdf-view-current-page)))]
    ["--" nil
     :visible (bound-and-true-p pdf-history-minor-mode)]
    ["Add text annotation" pdf-annot-mouse-add-text-annotation
     :visible (bound-and-true-p pdf-annot-minor-mode)
     :keys "\\[pdf-annot-add-text-annotation]"]
    ("Add markup annotation"
     :active (pdf-view-active-region-p)
     :visible (and (bound-and-true-p pdf-annot-minor-mode)
                   (pdf-info-markup-annotations-p))
     ["highlight" pdf-annot-add-highlight-markup-annotation]
     ["squiggly" pdf-annot-add-squiggly-markup-annotation]
     ["underline" pdf-annot-add-underline-markup-annotation]
     ["strikeout" pdf-annot-add-strikeout-markup-annotation])
    ["--" nil :visible (bound-and-true-p pdf-annot-minor-mode)]
    ["Display Annotations" pdf-annot-list-annotations
     :help "List all annotations"
     :visible (bound-and-true-p pdf-annot-minor-mode)]
    ["Display Attachments" pdf-annot-attachment-dired
     :help "Display attachments in a dired buffer"
     :visible (featurep 'pdf-annot)]
    ["Display Metadata" pdf-misc-display-metadata
     :help "Display information about the document"
     :visible (featurep 'pdf-misc)]
    ["Display Outline" pdf-outline
     :help "Display documents outline"
     :visible (featurep 'pdf-outline)]
    "--"
    ("Render Options"
     ["Printed Mode" (lambda ()
                       (interactive)
                       (pdf-view-printer-minor-mode 'toggle))
      :style toggle
      :selected pdf-view-printer-minor-mode
      :help "Display the PDF as it would be printed."]
     ["Midnight Mode" (lambda ()
                        (interactive)
                        (pdf-view-midnight-minor-mode 'toggle))
      :style toggle
      :selected pdf-view-midnight-minor-mode
      :help "Apply a color-filter appropriate for past midnight reading."])
    "--"
    ["Copy region" pdf-view-kill-ring-save
     :keys "\\[kill-ring-save]"
     :active (pdf-view-active-region-p)]
    ("Selection style"
     ["Glyph" (pdf-view-set-selection-style 'glyph)
      :style radio
      :selected (eq pdf-view-selection-style 'glyph)
      :help "When dragging the mouse, select individual characters."]
     ["Word" (pdf-view-set-selection-style 'word)
      :style radio
      :selected (eq pdf-view-selection-style 'word)
      :help "When dragging the mouse, select entire words."]
     ["Line" (pdf-view-set-selection-style 'line)
      :style radio
      :selected (eq pdf-view-selection-style 'line)
      :help "When dragging the mouse, select entire lines."])
    "--"
    ["Isearch document" isearch-forward
     :visible (bound-and-true-p pdf-isearch-minor-mode)]
    ["Occur document" pdf-occur
     :visible (featurep 'pdf-occur)]
    "--"
    ["Locate TeX source" pdf-sync-backward-search-mouse
     :visible (and (featurep 'pdf-sync)
                   (equal last-command-event
                          last-nonmenu-event))]
    ["--" nil :visible (and (featurep 'pdf-sync)
                            (equal last-command-event
                                   last-nonmenu-event))]
    ["Print" pdf-misc-print-document
     :active (and (pdf-view-buffer-file-name)
                  (file-readable-p (pdf-view-buffer-file-name)))]
    ["Create image" pdf-view-extract-region-image
     :help "Create an image of the page or the selected region(s)."]
    ["Create virtual PDF" pdf-virtual-buffer-create
     :help "Create a PDF containing all documents in this directory."
     :visible (bound-and-true-p pdf-virtual-global-minor-mode)]
    "--"
    ["Revert buffer" pdf-view-revert-buffer
     :visible (pdf-info-writable-annotations-p)]
    "--"
    ["Customize" pdf-tools-customize]))

;;;###autoload
(define-minor-mode pdf-misc-menu-bar-minor-mode
  "Display a PDF Tools menu in the menu-bar."
  :group 'pdf-misc
  (pdf-util-assert-pdf-buffer))

(defvar pdf-misc-context-menu-minor-mode-map
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap [down-mouse-3] 'pdf-misc-popup-context-menu)
    kmap))

;;;###autoload
(define-minor-mode pdf-misc-context-menu-minor-mode
  "Provide a right-click context menu in PDF buffers.

\\{pdf-misc-context-menu-minor-mode-map}"
  :group 'pdf-misc
  (pdf-util-assert-pdf-buffer))

(defun pdf-misc-popup-context-menu (_event)
  "Popup a context menu at position."
  (interactive "@e")
  (popup-menu
   (cons 'keymap
         (cddr (or (lookup-key pdf-misc-menu-bar-minor-mode-map
                               [menu-bar PDF\ Tools])
                   (lookup-key pdf-misc-menu-bar-minor-mode-map
                               [menu-bar pdf\ tools]))))))

(defun pdf-misc-display-metadata ()
  "Display all available metadata in a separate buffer."
  (interactive)
  (pdf-util-assert-pdf-buffer)
  (let* ((buffer (current-buffer))
         (md (pdf-info-metadata)))
    (with-current-buffer (get-buffer-create "*PDF-Metadata*")
      (let* ((inhibit-read-only t)
             (pad (apply' max (mapcar (lambda (d)
                                        (length (symbol-name (car d))))
                                      md)))
             (fmt (format "%%%ds:%%s\n" pad)))
        (erase-buffer)
        (setq header-line-format (buffer-name buffer)
              buffer-read-only t)
        (font-lock-mode 1)
        (font-lock-add-keywords nil
          '(("^ *\\(\\(?:\\w\\|-\\)+\\):"
             (1 font-lock-keyword-face))))
        (dolist (d md)
          (let ((key (car d))
                (val (cdr d)))
            (cl-case key
              (keywords
               (setq val (mapconcat 'identity val ", "))))
            (let ((beg (+ (length (symbol-name key)) (point) 1))
                  (fill-prefix
                   (make-string (1+ pad) ?\s)))
              (insert (format fmt key val))
              (fill-region beg (point) )))))
      (goto-char 1)
      (display-buffer (current-buffer)))
    md))

(defgroup pdf-misc nil
  "Miscellaneous options for PDF documents."
  :group 'pdf-tools)

(define-obsolete-variable-alias 'pdf-misc-print-programm
  'pdf-misc-print-program-executable "1.0")
(defcustom pdf-misc-print-program-executable nil
  "The program used for printing.

It is called with one argument, the PDF file."
  :group 'pdf-misc
  :type 'file)

(define-obsolete-variable-alias 'pdf-misc-print-programm-args
  'pdf-misc-print-program-args "1.0")
(defcustom pdf-misc-print-program-args nil
  "List of additional arguments passed to `pdf-misc-print-program'."
  :group 'pdf-misc
  :type '(repeat string))

(define-obsolete-function-alias 'pdf-misc-print-programm
  'pdf-misc-print-program "1.0")
(defun pdf-misc-print-program (&optional interactive-p)
  "Return the program used to print PDFs (if the executable is installed).

If INTERACTIVE-P is non-nil, ask the user for which program to
use when printing the PDF. Optionally, save the choice"
  (or (and pdf-misc-print-program-executable
           (executable-find pdf-misc-print-program-executable))
      (when interactive-p
        (let* ((default (car (delq nil (mapcar
                                        'executable-find
                                        '("gtklp" "xpp" "gpr")))))
               buffer-file-name
               (program
                (expand-file-name
                 (read-file-name
                  "Print with: " default nil t nil 'file-executable-p))))
          (when (and program
                     (executable-find program))
            (when (y-or-n-p "Save choice using customize? ")
              (customize-save-variable
               'pdf-misc-print-program-executable program))
            (setq pdf-misc-print-program-executable program))))))

(defun pdf-misc-print-document (filename &optional interactive-p)
  "Print the PDF doc FILENAME.

`pdf-misc-print-program' handles the print program, which see for
definition of INTERACTIVE-P."
  (interactive
   (list (pdf-view-buffer-file-name) t))
  (cl-check-type filename (and string (satisfies file-readable-p)))
  (let ((program (pdf-misc-print-program interactive-p))
        (args (append pdf-misc-print-program-args (list filename))))
    (unless program
      (error "No print program available"))
    (apply #'start-process "printing" nil program args)
    (message "Print job started: %s %s"
             program (mapconcat #'identity args " "))))


(provide 'pdf-misc)

;;; pdf-misc.el ends here
