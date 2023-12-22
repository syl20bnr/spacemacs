;;; company-dabbrev.el --- dabbrev-like company-mode completion backend  -*- lexical-binding: t -*-

;; Copyright (C) 2009-2011, 2013-2018, 2021-2023  Free Software Foundation, Inc.

;; Author: Nikolaj Schumacher

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:
;;

;;; Code:

(require 'company)
(require 'cl-lib)

(defgroup company-dabbrev nil
  "dabbrev-like completion backend."
  :group 'company)

(defcustom company-dabbrev-other-buffers 'all
  "Determines whether `company-dabbrev' should search other buffers.
If `all', search all other buffers, except the ignored ones.  If t, search
buffers with the same major mode.  See also `company-dabbrev-time-limit'."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Same major mode" t)
                 (const :tag "All" all)))

(defcustom company-dabbrev-ignore-buffers "\\`[ *]"
  "Regexp matching the names of buffers to ignore.
Or a function that returns non-nil for such buffers."
  :type '(choice (regexp :tag "Regexp")
                 (function :tag "Predicate"))
  :package-version '(company . "0.9.0"))

(defcustom company-dabbrev-time-limit .1
  "Determines how many seconds `company-dabbrev' should look for matches."
  :type '(choice (const :tag "Off" nil)
                 (number :tag "Seconds")))

(defcustom company-dabbrev-char-regexp "\\sw"
  "A regular expression matching the characters `company-dabbrev' looks for."
  :type 'regexp)

(defcustom company-dabbrev-ignore-case 'keep-prefix
  "Non-nil to ignore case when collecting completion candidates.
When it's `keep-prefix', the text before point will remain unchanged after
candidate is inserted, even some of its characters have different case."
  :type '(choice
          (const :tag "Don't ignore case" nil)
          (const :tag "Ignore case" t)
          (const :tag "Keep case before point" keep-prefix)))

(defcustom company-dabbrev-downcase 'case-replace
  "Whether to downcase the returned candidates.

The value of nil means keep them as-is.
`case-replace' means use the value of `case-replace'.
Any other value means downcase."
  :type '(choice
          (const :tag "Keep as-is" nil)
          (const :tag "Downcase" t)
          (const :tag "Use case-replace" case-replace)))

(defcustom company-dabbrev-minimum-length 4
  "The minimum length for the completion candidate to be included.
This variable affects both `company-dabbrev' and `company-dabbrev-code'."
  :type 'integer
  :package-version '(company . "0.8.3"))

(defcustom company-dabbrev-ignore-invisible nil
  "Non-nil to skip invisible text."
  :type 'boolean
  :package-version '(company . "0.9.0"))

(defmacro company-dabbrev--time-limit-while (test start limit freq &rest body)
  (declare (indent 3) (debug t))
  `(let ((company-time-limit-while-counter 0))
     (catch 'done
       (while ,test
         ,@body
         (and ,limit
              (= (cl-incf company-time-limit-while-counter) ,freq)
              (setq company-time-limit-while-counter 0)
              (> (float-time (time-since ,start)) ,limit)
              (throw 'done 'company-time-out))))))

(defun company-dabbrev--make-regexp ()
  (concat "\\(?:" company-dabbrev-char-regexp "\\)+"))

(defun company-dabbrev--search-buffer (regexp pos symbols start limit
                                       ignore-comments)
  (save-excursion
    (cl-labels ((maybe-collect-match
                 ()
                 (let ((match (match-string-no-properties 0)))
                   (when (and (>= (length match) company-dabbrev-minimum-length)
                              (not (and company-dabbrev-ignore-invisible
                                        (invisible-p (match-beginning 0)))))
                     (puthash match t symbols)))))
      (goto-char (if pos (1- pos) (point-min)))
      ;; Search before pos.
      (let ((tmp-end (point)))
        (company-dabbrev--time-limit-while (and (not (input-pending-p))
                                                (> tmp-end (point-min)))
            start limit 1
          (ignore-errors
            (forward-char -10000))
          (forward-line 0)
          (save-excursion
            ;; Before, we used backward search, but it matches non-greedily, and
            ;; that forced us to use the "beginning/end of word" anchors in
            ;; `company-dabbrev--make-regexp'.  It's also about 2x slower.
            (while (and (not (input-pending-p))
                        (re-search-forward regexp tmp-end t))
              (if (and ignore-comments (save-match-data (company-in-string-or-comment)))
                  (re-search-forward "\\s>\\|\\s!\\|\\s\"" tmp-end t)
                (maybe-collect-match))))
          (setq tmp-end (point))))
      (goto-char (or pos (point-min)))
      ;; Search after pos.
      (company-dabbrev--time-limit-while (and (not (input-pending-p))
                                              (re-search-forward regexp nil t))
          start limit 25
        (if (and ignore-comments (save-match-data (company-in-string-or-comment)))
            (re-search-forward "\\s>\\|\\s!\\|\\s\"" nil t)
          (maybe-collect-match)))
      symbols)))

(defun company-dabbrev--search (regexp &optional limit other-buffer-modes
                                ignore-comments)
  (let* ((start (current-time))
         (symbols (company-dabbrev--search-buffer regexp (point)
                                                  (make-hash-table :test 'equal)
                                                  start limit
                                                  ignore-comments)))
    (when other-buffer-modes
      (cl-dolist (buffer (delq (current-buffer) (buffer-list)))
        (unless (if (stringp company-dabbrev-ignore-buffers)
                    (string-match-p company-dabbrev-ignore-buffers
                                    (buffer-name buffer))
                  (funcall company-dabbrev-ignore-buffers buffer))
          (with-current-buffer buffer
            (when (or (eq other-buffer-modes 'all)
                      (cl-some #'derived-mode-p other-buffer-modes))
              (setq symbols
                    (company-dabbrev--search-buffer regexp nil symbols start
                                                    limit ignore-comments)))))
        (and limit
             (> (float-time (time-since start)) limit)
             (cl-return))))
    symbols))

(defun company-dabbrev--prefix ()
  ;; Not in the middle of a word.
  (unless (looking-at company-dabbrev-char-regexp)
    ;; Emacs can't do greedy backward-search.
    (company-grab-line (format "\\(?:^\\| \\)[^ ]*?\\(\\(?:%s\\)*\\)"
                               company-dabbrev-char-regexp)
                       1)))

(defun company-dabbrev--filter (prefix candidates)
  (let* ((completion-ignore-case company-dabbrev-ignore-case)
         (filtered (all-completions prefix candidates))
         (lp (length prefix))
         (downcase (if (eq company-dabbrev-downcase 'case-replace)
                       case-replace
                     company-dabbrev-downcase)))
    (when downcase
      (let ((ptr filtered))
        (while ptr
          (setcar ptr (downcase (car ptr)))
          (setq ptr (cdr ptr)))))
    (if (and (eq company-dabbrev-ignore-case 'keep-prefix)
             (not (= lp 0)))
        (company-substitute-prefix prefix filtered)
      filtered)))

(defun company-dabbrev--fetch ()
  (company-dabbrev--search (company-dabbrev--make-regexp)
                           company-dabbrev-time-limit
                           (pcase company-dabbrev-other-buffers
                             (`t (list major-mode))
                             (`all `all))))

;;;###autoload
(defun company-dabbrev (command &optional arg &rest _ignored)
  "dabbrev-like `company-mode' completion backend."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-dabbrev))
    (prefix (company-dabbrev--prefix))
    (candidates
     (company-dabbrev--filter
      arg
      (company-cache-fetch 'dabbrev-candidates #'company-dabbrev--fetch
                           :expire t)))
    (kind 'text)
    (no-cache t)
    (ignore-case (and company-dabbrev-ignore-case t))
    (duplicates t)))

(provide 'company-dabbrev)
;;; company-dabbrev.el ends here
