;;; bookmarky.el --- List bookmarks organized with Taxy  -*- lexical-binding: t; -*-

;; Copyright (C) 2021  Free Software Foundation, Inc.

;; Author: Adam Porter <adam@alphapapa.net>
;; Keywords: convenience, lisp
;; Package-Requires: ((emacs "27.2") (taxy "0.7"))

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

;; This library provides commands that show bookmarks organized in a
;; flexible, customizeable hierarchy and table.

;;; Code:

(require 'bookmark)
(require 'cl-lib)
(require 'subr-x)

(require 'taxy)
(require 'taxy-magit-section)

(defgroup bookmarky nil
  "List bookmarks grouped with Taxy."
  :group 'bookmark)

;;;; Keys

(cl-eval-when (compile load eval)
  ;; I don't understand why using `cl-eval-when' is necessary, but it
  ;; seems to be.
  (taxy-define-key-definer bookmarky-define-key bookmarky-keys "bookmarky-key"
    ;; FIXME: Docstring.
    ""))

(bookmarky-define-key directory (&optional directory &key descendant-p name)
  "Return key string for ITEM's directory, or nil.
If DIRECTORY is specified, return key string if ITEM's `filename'
is in DIRECTORY.  If DESCENDANT-P, return key string if ITEM's
`filename' is a descendant of DIRECTORY.  DIRECTORY should end in
a slash."
  ;; It seems like undesirable overhead to `file-truename' every
  ;; time this function is called, but avoiding that wouldn't be easy.
  (when-let (filename (bookmark-prop-get item 'filename))
    (setf filename (expand-file-name filename))
    (pcase directory
      ('nil (concat "Directory: " (file-name-directory filename)))
      (_
       (cl-assert (directory-name-p directory) t
                  "DIRECTORY should end in a directory separator character (i.e. a slash)")
       (setf directory (file-truename directory))
       (pcase descendant-p
         ('nil (when (equal directory (file-truename filename))
                 (or name (concat "Directory: " directory))))
         (_ (when (string-prefix-p directory filename)
              (or name (concat "Directory: " directory)))))))))

(bookmarky-define-key filename (&key name regexp)
  "Return NAME if bookmark ITEM's filename matches REGEXP, or without REGEXP, the filename."
  (when-let (filename (bookmark-prop-get item 'filename))
    (pcase regexp
      (`nil filename)
      (_ (when (string-match-p regexp filename)
           name)))))

(bookmarky-define-key handler (handlers &key name)
  "Return NAME if bookmark ITEM's handler is in HANDLERS."
  (when-let (handler (bookmark-prop-get item 'handler))
    (when (member handler handlers)
      name)))

(bookmarky-define-key name (&key name regexp)
  "Return NAME if bookmark ITEM's name matches REGEXP."
  (when (string-match-p regexp (car item))
    name))

(defvar bookmarky-default-keys
  '(
    ((handler '(burly-bookmark-handler) :name "Burly"))
    ((directory "~/src/emacs/" :name "Emacs" :descendant-p t)))
  "Default keys.")

;;;; Columns

(cl-eval-when (compile load eval)
  ;; I don't understand why using `cl-eval-when' is necessary, but it
  ;; seems to be.
  (taxy-magit-section-define-column-definer "bookmarky"))

(bookmarky-define-column "Name" (:max-width 45 :face bookmark-menu-bookmark)
  (car item))

(bookmarky-define-column "File" (:max-width nil :face font-lock-doc-face)
  (bookmark-prop-get item 'filename))

(unless bookmarky-columns
  ;; TODO: Automate this or document it
  (setq-default bookmarky-columns
		(get 'bookmarky-columns 'standard-value)))

;;;; Variables

(defvar bookmarky-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'bookmarky-RET)
    (define-key map [mouse-1] #'bookmarky-mouse-1)
    map))

;;;; Commands

;;;###autoload
(cl-defun bookmarky (&key (keys bookmarky-default-keys)
		          (buffer-name "*Bookmarky*")
		          visibility-fn display-buffer-action)
  "Show bookmarks grouped with Taxy."
  (interactive)
  (let (format-table column-sizes)
    (cl-labels ((format-item (item) (gethash item format-table))
		(make-fn (&rest args)
			 (apply #'make-taxy-magit-section
				:make #'make-fn
				:format-fn #'format-item
				:level-indent bookmarky-level-indent
				:visibility-fn visibility-fn
				args)))
      (with-current-buffer (get-buffer-create buffer-name)
	(bookmarky-mode)
	(let* ((taxy (thread-last
			 (make-fn
			  :name "Bookmarky"
			  :take (taxy-make-take-function keys bookmarky-keys))
		       (taxy-fill bookmark-alist)
		       (taxy-sort* #'string< #'taxy-name)
		       (taxy-sort #'string< #'car)))
	       (taxy-magit-section-insert-indent-items nil)
	       (inhibit-read-only t)
	       (format-cons (taxy-magit-section-format-items
			     bookmarky-columns bookmarky-column-formatters taxy)))
	  (setf format-table (car format-cons)
		column-sizes (cdr format-cons)
		header-line-format (taxy-magit-section-format-header
				    column-sizes bookmarky-column-formatters))
          (delete-all-overlays)
          (erase-buffer)
	  (save-excursion
	    (taxy-magit-section-insert taxy :items 'last
	      ;; :blank-between-depth bufler-taxy-blank-between-depth
	      :initial-depth 0))))
      (pop-to-buffer buffer-name display-buffer-action))))

(defun bookmarky-revert (_ignore-auto _noconfirm)
  "Revert current Bookmarky buffer."
  (interactive)
  (bookmarky))

(defun bookmarky-mouse-1 (event)
  "Call `bookmarky-RET' with point at EVENT's position."
  (interactive "e")
  (mouse-set-point event)
  (call-interactively #'bookmarky-RET))

(defun bookmarky-RET ()
  "Go to bookmark at point, or expand section at point."
  (interactive)
  (cl-etypecase (oref (magit-current-section) value)
    (taxy-magit-section (call-interactively #'magit-section-cycle))
    (null nil)
    (list (bookmark-jump (oref (magit-current-section) value)))))

(define-derived-mode bookmarky-mode magit-section-mode "Bookmarky"
  :global nil
  (setq-local revert-buffer-function #'bookmarky-revert))

(provide 'bookmarky)

;;; bookmarky.el ends here
