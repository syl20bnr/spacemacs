;;; pylookup.el --- Look up python documents (reference) in Emacs

;; Copyright (C) 2010-2013 Taesoo Kim

;; Author: Taesoo Kim <taesoo@mit.edu>
;; Maintainer: Taesoo Kim <taesoo@mit.edu>
;; Created: 19 June 2009
;; Keywords: python,reference,document,help

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(eval-when-compile
  (require 'browse-url)
  (require 'simple)
  (require 'cl)
  (require 'ido))

;;=================================================================
;; user options
;;=================================================================

(defvar pylookup-db-file "pylookup.db" "Pylookup database file")
(defvar pylookup-program "pylookup.py" "Pylookup execution file")
(defvar pylookup-search-options nil
  "Pylookup search options (see ./pylookup.py -h)")

;;=================================================================
;; internal variables
;;=================================================================

(defvar pylookup-html-locations '("http://docs.python.org"))
(defvar pylookup-history nil)
(defvar pylookup-cache nil)
(defvar pylookup-return-window-config nil)
(defvar pylookup-temp-buffer-name "*Pylookup Completions*")

(defvar pylookup-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [mouse-1]  'pylookup-mode-lookup-and-leave)
    (define-key map [mouse-2]  'pylookup-mode-lookup)
    (define-key map "\C-m"     'pylookup-mode-lookup-and-leave)
    (define-key map " "        'pylookup-mode-lookup-and-leave)
    (define-key map "f"        'pylookup-mode-lookup)
    (define-key map "q"        'pylookup-mode-quit-window)
    (define-key map "n"        'pylookup-mode-next-line)
    (define-key map "j"        'pylookup-mode-next-line)
    (define-key map "p"        'pylookup-move-prev-line)
    (define-key map "k"        'pylookup-move-prev-line)
    (define-key map "/"        'isearch-forward)

    (define-key map "v"        'scroll-down)
    (define-key map "V"        'scroll-up)
    (define-key map "l"        'recenter)
    (define-key map "<"        'beginning-of-buffer)
    (define-key map ">"        'end-of-buffer)
    (define-key map "v"        'scroll-down)
    
    map)
  "Keymap for `pylookup-mode-mode'.")

(put 'pylookup-mode 'mode-class 'special)

(defvar pylookup-completing-read 
  (if (null ido-mode) 'completing-read 'ido-completing-read)
  "Ido support with convenience")

;;=================================================================
;; pylookup mode specific interactive functions
;;=================================================================

(defun pylookup-trim (desc n)
  "Trim desc string to fit into the length, n"

  (if (> (length desc) (- n 2))
      (concat (substring desc 0 (- n 2)) "..")
    desc))

(defun pylookup-mode ()
  "Major mode for output from \\[pylookup-lookup]."
  (interactive)
  
  (kill-all-local-variables)
  (use-local-map pylookup-mode-map)
  (setq major-mode 'pylookup-mode)
  (setq mode-name "Pylookup")
  (setq buffer-read-only t)
  (run-mode-hooks))

(defun pylookup-move-prev-line ()
  "Move to previous entry"
  (interactive)
  
  (when (< 3 (line-number-at-pos))
    (call-interactively 'previous-line)))

(defun pylookup-mode-next-line ()
  "Move to next entry"
  (interactive)
  
  (when (< (line-number-at-pos)
           (- (line-number-at-pos (point-max)) 1))
    (call-interactively 'next-line)))

(defun pylookup-mode-lookup-and-leave ()
  "Lookup the current line in a browser and leave the completions window."
  (interactive)
  
  (call-interactively 'pylookup-mode-lookup)
  (pylookup-mode-quit-window))

(defun pylookup-mode-lookup ()
  "Lookup the current line in a browser."
  (interactive)
  
  (let ((url (get-text-property (point) 'pylookup-target-url)))
    (if url
        (progn
          (beginning-of-line)
          (message "Browsing: \"%s\"" url)
          (browse-url url))
      (error "No URL on this line"))))

(defun pylookup-mode-quit-window ()
  "Leave the completions window."
  (interactive)

  (set-window-configuration pylookup-return-window-config))

;;=================================================================
;; execute pylookup
;;=================================================================

(defun pylookup-exec-get-cache ()
  "Run a pylookup process and get a list of cache (db key)"

  (split-string
   (with-output-to-string
     (call-process pylookup-program nil standard-output nil 
           "-d" (expand-file-name pylookup-db-file)
           "-c"))))

(defun pylookup-exec-lookup (search-term)
  "Runs a pylookup process and returns a list of (term, url) pairs."

  (mapcar
   (lambda (x) (split-string x ";"))
   (split-string
     (with-output-to-string
         (apply 'call-process pylookup-program nil standard-output nil
                "-d" (expand-file-name pylookup-db-file)
                "-l" search-term
                "-f" "Emacs"
                pylookup-search-options))
     "\n" t)))

;;=================================================================
;; interactive user interfaces
;;=================================================================

;;;###autoload
(defun pylookup-lookup (search-term)
  "Lookup SEARCH-TERM in the Python HTML indexes."
  (interactive
   (list 
    (let ((initial (thing-at-point 'word)))
      (funcall pylookup-completing-read
               "Search: "
               (if pylookup-cache 
                   pylookup-cache 
                 (setq pylookup-cache (pylookup-exec-get-cache)))
               nil nil initial 'pylookup-history))
    ))

  (let ((matches (pylookup-exec-lookup search-term)))
    (cond

      ;; 0. No results.
      ((eq matches nil)
       (message "No matches for \"%s\"." search-term))

      ;; 1. A single result.
      ((= (length matches) 1)  
       ;; Point the browser at the unique result and get rid of the buffer
       (let ((data (car matches)))
         (message "Browsing: \"%s\"" (car data))
         (browse-url (cadr data))))

      ;; N. Multiple results.
      (t
       ;; Decorate the temporary buffer lines with appropriate properties for
       ;; selection.
       (let* ((cur-window-conf (current-window-configuration))
              (tmpbuf (get-buffer-create pylookup-temp-buffer-name))
              (index 0))
    
         (display-buffer tmpbuf)
         (pop-to-buffer tmpbuf)

         (setq buffer-read-only nil)
         (erase-buffer)

         ;; Insert the text in the buffer
         (insert (format "Python index matches for %s:\n\n" search-term))
         (dolist (x matches)
            ;; split like
            ;; waitpid() (in module os) [lib]
            ;; --------- -------------- -----
            ;; =>
            ;; waitpid     (in module os)  [lib]
            ;; api         module          type

            (let* ((tokens (split-string (car x)))
                   (api (car tokens))
                   (iter (cdr tokens))
                   (type (car (last tokens)))
                   (module ""))

              (while (not (or (equal iter nil)
                              (string= (car iter) type)))
                (setq module (concat module " " (car iter)))
                (setq iter (cdr iter)))

              (incf index)
              (insert (format " %03d) %-25s %-30s %10s" 
                  index 
                  (pylookup-trim api 25)
                  (pylookup-trim module 30)
                  (pylookup-trim type 10))))

            (put-text-property
             (line-beginning-position) (line-end-position)
             'pylookup-target-url (cadr x))
            (insert "\n"))

         ;; goto first entry
         (goto-line 3)

         ;; turn mode on
         (pylookup-mode)

         ;; highlighting
         (font-lock-add-keywords nil `((,(format "\\(%s\\|%s\\|%s\\)" 
                         search-term 
                         (upcase search-term)
                         (upcase-initials search-term))
                                         1
                                         font-lock-keyword-face prepend)))

         (font-lock-add-keywords nil '(("\\<\\(lib\\)"
                                         1
                                         font-lock-constant-face prepend)))

         (font-lock-add-keywords nil '(("\\<\\(in module.*)\\)"
                                         1
                                         font-lock-doc-face prepend)))

         ;; store window configuration
         (set (make-local-variable 'pylookup-return-window-config) cur-window-conf)

         ;; make fit to screen
         (shrink-window-if-larger-than-buffer (get-buffer-window tmpbuf)))))))

;;;###autoload
(defun pylookup-set-search-option (option-string)
  "Set search option interactively"
  (interactive
   (list (read-string "Search option: "
                      (mapconcat 'identity pylookup-search-options " "))))
  (setq pylookup-search-options (split-string option-string " ")))

;;;###autoload
(defun pylookup-update (src &optional append)
  "Run pylookup-update and create the database at `pylookup-db-file'."
  (interactive 
   (list (funcall pylookup-completing-read
                  "Python Html Documentation source: "
                  pylookup-html-locations)))
  
  ;; pylookup.py -d /home/myuser/.pylookup/pylookup.db -l <URL>
  (message (with-output-to-string
             (call-process pylookup-program nil standard-output nil
                  "-u" src
                  "-d" (expand-file-name pylookup-db-file)
                  (if append
                      "-a"
                    "")))))

;;;###autoload
(defun pylookup-update-all ()
  "Run pylookup-update for all sources and create the database at `pylookup-db-file'."
  (interactive)
  ;; truncate db file
  (with-temp-buffer (write-file pylookup-db-file))
  (mapc (lambda (s) (pylookup-update s t)) pylookup-html-locations))

;;;###autoload
(defun pylookup-lookup-at-point ()
  "Query the for string with help of word read at point and call `pylookup-lookup'"
  (interactive)
  (let* ((default-word (thing-at-point 'word))
         (default-prompt (concat "Lookup Word "
                                 (if default-word
                                     (concat "(" default-word ")") nil)
                                 ": "))
         (pylookup-query
          (funcall #'(lambda (str)
                       "Remove Whitespace from beginning and end of a string."
                       (replace-regexp-in-string "^[ \n\t]*\\(.*?\\)[ \n\t]*$"
                                                 "\\1"
                                                 str))
                   (read-string default-prompt nil nil default-word))))
    (if (= (length pylookup-query) 0) nil
      (pylookup-lookup pylookup-query))))

(provide 'pylookup)
;;; pylookup.el ends here
