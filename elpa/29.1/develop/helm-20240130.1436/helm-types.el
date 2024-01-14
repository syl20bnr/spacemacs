;;; helm-types.el --- Helm types classes and methods. -*- lexical-binding: t -*-

;; Copyright (C) 2015 ~ 2020  Thierry Volpiatto 

;; Author: Thierry Volpiatto 
;; URL: http://github.com/emacs-helm/helm

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


;;; Code:

(require 'cl-lib)
(require 'eieio)
(eval-when-compile (require 'helm-source))

(defvar helm-map)
(defvar helm-mode-line-string)
(defvar helm-bookmark-map)
(declare-function helm-make-actions "helm-lib")
(declare-function helm-ediff-marked-buffers "helm-buffers")
(declare-function helm-make-type "helm-source")


;;  Files
(defclass helm-type-file (helm-source) ()
  "A class to define helm type file.")

(cl-defmethod helm-source-get-action-from-type ((object helm-type-file))
  (slot-value object 'action))

(defun helm-actions-from-type-file ()
  (let ((source (make-instance 'helm-type-file)))
    (helm--setup-source source)
    (helm-source-get-action-from-type source)))

(defvar helm-generic-files-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-]")     'helm-ff-run-toggle-basename)
    (define-key map (kbd "C-s")     'helm-ff-run-grep)
    (define-key map (kbd "M-g s")   'helm-ff-run-grep)
    (define-key map (kbd "M-g z")   'helm-ff-run-zgrep)
    (define-key map (kbd "M-g p")   'helm-ff-run-pdfgrep)
    (define-key map (kbd "M-R")     'helm-ff-run-rename-file)
    (define-key map (kbd "M-C")     'helm-ff-run-copy-file)
    (define-key map (kbd "M-B")     'helm-ff-run-byte-compile-file)
    (define-key map (kbd "M-L")     'helm-ff-run-load-file)
    (define-key map (kbd "M-S")     'helm-ff-run-symlink-file)
    (define-key map (kbd "M-H")     'helm-ff-run-hardlink-file)
    (define-key map (kbd "M-D")     'helm-ff-run-delete-file)
    (define-key map (kbd "C-=")     'helm-ff-run-ediff-file)
    (define-key map (kbd "C-c =")   'helm-ff-run-ediff-merge-file)
    (define-key map (kbd "C-c o")   'helm-ff-run-switch-other-window)
    (define-key map (kbd "C-c r")   'helm-ff-run-find-file-as-root)
    (define-key map (kbd "C-c C-o") 'helm-ff-run-switch-other-frame)
    (define-key map (kbd "M-i")     'helm-ff-properties-persistent)
    (define-key map (kbd "C-c C-x") 'helm-ff-run-open-file-externally)
    (define-key map (kbd "C-c X")   'helm-ff-run-open-file-with-default-tool)
    (define-key map (kbd "C-c @")   'helm-ff-run-insert-org-link)
    (define-key map (kbd "C-x C-q") 'helm-ff-run-marked-files-in-dired)
    (define-key map (kbd "C-c C-a") 'helm-ff-run-mail-attach-files)
    map)
  "Generic Keymap for files.")

(defcustom helm-type-file-actions
  (helm-make-actions
    "Find file"                               'helm-find-file-or-marked
    "Find file as root"                       'helm-find-file-as-root
    "Find file other window"                  'helm-find-files-other-window
    "Find file other frame"                   'find-file-other-frame
    "Open dired in file's directory"          'helm-open-dired
    "Attach file(s) to mail buffer `C-c C-a'" 'helm-ff-mail-attach-files
    "Marked files in dired"                   'helm-marked-files-in-dired
    "Grep File(s) `C-u recurse'"              'helm-find-files-grep
    "Zgrep File(s) `C-u Recurse'"             'helm-ff-zgrep
    "Pdfgrep File(s)"                         'helm-ff-pdfgrep
    "Insert as org link"                      'helm-files-insert-as-org-link
    "Checksum File"                           'helm-ff-checksum
    "Ediff File"                              'helm-find-files-ediff-files
    "Ediff Merge File"                        'helm-find-files-ediff-merge-files
    "View file"                               'view-file
    "Insert file"                             'insert-file
    "Add marked files to file-cache"          'helm-ff-cache-add-file
    "Delete file(s)"                          'helm-ff-delete-files
    "Copy file(s) `M-C, C-u to follow'"       'helm-find-files-copy
    "Rename file(s) `M-R, C-u to follow'"     'helm-find-files-rename
    "Symlink files(s) `M-S, C-u to follow'"   'helm-find-files-symlink
    "Relsymlink file(s) `C-u to follow'"      'helm-find-files-relsymlink
    "Hardlink file(s) `M-H, C-u to follow'"   'helm-find-files-hardlink
    "Open file externally (C-u to choose)"    'helm-open-file-externally
    "Open file with default tool"             'helm-open-file-with-default-tool
    "Find file in hex dump"                   'hexl-find-file)
  "Default actions for type files."
  :group 'helm-files
  :type '(alist :key-type string :value-type function))

(cl-defmethod helm--setup-source ((_source helm-type-file)))

(cl-defmethod helm--setup-source :before ((source helm-type-file))
  (setf (slot-value source 'action) 'helm-type-file-actions)
  (setf (slot-value source 'persistent-help) "Show this file")
  (setf (slot-value source 'action-transformer)
        '(helm-transform-file-load-el
          helm-transform-file-browse-url
          helm-transform-file-cache))
  (setf (slot-value source 'candidate-transformer)
        '(helm-skip-boring-files
          helm-w32-pathname-transformer))
  (setf (slot-value source 'filtered-candidate-transformer)
        'helm-highlight-files)
  (setf (slot-value source 'help-message) 'helm-generic-file-help-message)
  (setf (slot-value source 'mode-line) (list "File(s)" helm-mode-line-string))
  (setf (slot-value source 'keymap) helm-generic-files-map)
  (setf (slot-value source 'group) 'helm-files))


;; Bookmarks
(defclass helm-type-bookmark (helm-source) ()
  "A class to define type bookmarks.")

(defcustom helm-type-bookmark-actions
  (helm-make-actions
   "Jump to bookmark" 'helm-bookmark-jump
   "Jump to BM other window" 'helm-bookmark-jump-other-window
   "Jump to BM other frame" 'helm-bookmark-jump-other-frame
   "Jump to BM other tab" 'helm-bookmark-jump-other-tab
   "Bookmark edit annotation" 'bookmark-edit-annotation
   "Bookmark show annotation" 'bookmark-show-annotation
   "Delete bookmark(s)" 'helm-delete-marked-bookmarks
   "Edit Bookmark" 'helm-bookmark-edit-bookmark
   "Rename bookmark" 'helm-bookmark-rename
   "Relocate bookmark" 'bookmark-relocate)
  "Default actions for type bookmarks."
  :group 'helm-bookmark
  :type '(alist :key-type string
                   :value-type function))

(cl-defmethod helm-source-get-action-from-type ((object helm-type-bookmark))
  (slot-value object 'action))

(cl-defmethod helm--setup-source ((_source helm-type-bookmark)))

(cl-defmethod helm--setup-source :before ((source helm-type-bookmark))
  (setf (slot-value source 'action) 'helm-type-bookmark-actions)
  (setf (slot-value source 'keymap) helm-bookmark-map)
  (setf (slot-value source 'mode-line) (list "Bookmark(s)" helm-mode-line-string))
  (setf (slot-value source 'help-message) 'helm-bookmark-help-message)
  (setf (slot-value source 'migemo) t)
  (setf (slot-value source 'follow) 'never)
  (setf (slot-value source 'group) 'helm-bookmark))


;; Buffers
(defclass helm-type-buffer (helm-source) ()
  "A class to define type buffer.")

(defcustom helm-type-buffer-actions
  (helm-make-actions
   "Switch to buffer(s)" 'helm-buffer-switch-buffers
   "Switch to buffer(s) other window `C-c o'"
   'helm-buffer-switch-buffers-other-window
   "Switch to buffer(s) other frame `C-c C-o'"
   'helm-buffer-switch-to-buffer-other-frame
   "Raise buffer frame maybe"
   'helm-buffers-maybe-raise-buffer-frame
   (lambda () (and (fboundp 'tab-bar-mode)
                   "Switch to buffer(s) other tab `C-c C-t'"))
   'helm-buffers-switch-to-buffer-other-tab
   "Switch to buffer at line number"
   'helm-switch-to-buffer-at-linum
   "Browse project `C-x C-d'"
   'helm-buffers-browse-project
   "Switch to shell"
   'helm-buffer-switch-to-shell
   "Query replace regexp `C-M-%'"
   'helm-buffer-query-replace-regexp
   "Query replace `M-%'" 'helm-buffer-query-replace
   "View buffer" 'view-buffer
   "Display buffer" 'display-buffer
   "Rename buffer `M-R'" 'helm-buffers-rename-buffer
   "Grep buffer(s) `M-g s' (C-u grep all buffers)"
   'helm-zgrep-buffers
   "Multi occur buffer(s) `C-s (C-u search also in current)'"
   'helm-multi-occur-as-action
   "Revert buffer(s) `M-G'" 'helm-revert-marked-buffers
   "Insert buffer" 'insert-buffer
   "Kill buffer(s) `M-D'" 'helm-kill-marked-buffers
   "Diff with file `C-='" 'diff-buffer-with-file
   "Ediff Marked buffers `C-c ='" 'helm-ediff-marked-buffers
   "Ediff Merge marked buffers `M-='"
   (lambda (candidate)
     (helm-ediff-marked-buffers candidate t)))
  "Default actions for type buffers."
  :group 'helm-buffers
  :type '(alist :key-type string :value-type function))

(cl-defmethod helm-source-get-action-from-type ((object helm-type-buffer))
  (slot-value object 'action))

(cl-defmethod helm--setup-source ((_source helm-type-buffer)))

(cl-defmethod helm--setup-source :before ((source helm-type-buffer))
  (setf (slot-value source 'action) 'helm-type-buffer-actions)
  (setf (slot-value source 'persistent-help) "Show this buffer")
  (setf (slot-value source 'mode-line)
        ;; Use default-value of `helm-mode-line-string' in case user
        ;; starts with a helm buffer as current-buffer otherwise the
        ;; local value of this helm buffer is used (bug#1517, bug#2377).
        (list "Buffer(s)" (default-value 'helm-mode-line-string)))
  (setf (slot-value source 'filtered-candidate-transformer)
        '(helm-skip-boring-buffers
          helm-buffers-sort-transformer
          helm-highlight-buffers))
  (setf (slot-value source 'group) 'helm-buffers))

;; Functions
(defclass helm-type-function (helm-source) ()
  "A class to define helm type function.")

(defcustom helm-type-function-actions
  (helm-make-actions
   "Describe function" 'helm-describe-function
   "Find function (C-u for source)" 'helm-find-function
   "Info lookup" 'helm-info-lookup-symbol
   "Debug on entry" 'debug-on-entry
   "Cancel debug on entry" 'cancel-debug-on-entry
   "Trace function" 'trace-function
   "Trace function (background)" 'trace-function-background
   "Untrace function" 'untrace-function)
    "Default actions for type functions."
  :group 'helm-elisp
  ;; Use symbol as value type because some functions may not be
  ;; autoloaded (like untrace-function).
  :type '(alist :key-type string :value-type symbol))

(cl-defmethod helm-source-get-action-from-type ((object helm-type-function))
  (slot-value object 'action))

(defun helm-actions-from-type-function ()
  (let ((source (make-instance 'helm-type-function)))
    (helm--setup-source source)
    (helm-source-get-action-from-type source)))

(cl-defmethod helm--setup-source ((_source helm-type-function)))

(cl-defmethod helm--setup-source :before ((source helm-type-function))
  (setf (slot-value source 'action) 'helm-type-function-actions)
  (setf (slot-value source 'action-transformer)
        'helm-transform-function-call-interactively)
  (setf (slot-value source 'candidate-transformer)
        'helm-mark-interactive-functions)
  (setf (slot-value source 'coerce) 'helm-symbolify))


;; Commands
(defclass helm-type-command (helm-source) ()
  "A class to define helm type command.")

(defun helm-actions-from-type-command ()
  (let ((source (make-instance 'helm-type-command)))
    (helm--setup-source source)
    (helm-source-get-action-from-type source)))

(defcustom helm-type-command-actions
  (append (helm-make-actions
           "Execute command" 'helm-M-x-execute-command)
          (symbol-value
           (helm-actions-from-type-function)))
  "Default actions for type command."
  :group 'helm-command
  :type '(alist :key-type string :value-type symbol))

(cl-defmethod helm--setup-source ((_source helm-type-command)))

(cl-defmethod helm--setup-source :before ((source helm-type-command))
  (setf (slot-value source 'action) 'helm-type-command-actions)
  (setf (slot-value source 'coerce) 'helm-symbolify)
  (setf (slot-value source 'persistent-action) 'helm-M-x-persistent-action)
  (setf (slot-value source 'persistent-help) "Describe this command")
  (setf (slot-value source 'group) 'helm-command))

;; Timers
(defclass helm-type-timers (helm-source) ()
  "A class to define helm type timers.")

(defcustom helm-type-timers-actions
  '(("Cancel Timer" . (lambda (_timer)
                        (let ((mkd (helm-marked-candidates)))
                          (cl-loop for timer in mkd
                                   do (cancel-timer timer)))))
    ("Describe Function" . (lambda (tm)
                             (describe-function (timer--function tm))))
    ("Find Function" . (lambda (tm)
                         (helm-aif (timer--function tm)
                             (if (or (byte-code-function-p it)
                                     (helm-subr-native-elisp-p it))
                                 (message "Can't find anonymous function `%s'" it)
                                 (find-function it))))))
  "Default actions for type timers."
  :group 'helm-elisp
  :type '(alist :key-type string :value-type function))

(cl-defmethod helm--setup-source ((_source helm-type-timers)))

(cl-defmethod helm--setup-source :before ((source helm-type-timers))
  (setf (slot-value source 'action) 'helm-type-timers-actions)
  (setf (slot-value source 'persistent-action)
        (lambda (tm)
          (describe-function (timer--function tm))))
  (setf (slot-value source 'persistent-help) "Describe Function")
  (setf (slot-value source 'group) 'helm-elisp))

;; Builders.
(defun helm-build-type-file ()
  (helm-make-type 'helm-type-file))

(defun helm-build-type-function ()
  (helm-make-type 'helm-type-function))

(defun helm-build-type-command ()
  (helm-make-type 'helm-type-command))

(provide 'helm-types)

;;; helm-types.el ends here
