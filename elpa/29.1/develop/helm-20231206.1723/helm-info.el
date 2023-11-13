;;; helm-info.el --- Browse info index with helm -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2023 Thierry Volpiatto 

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
(require 'info)
;; helm-utils is requiring helm which is requiring helm-lib, but let's require
;; them explicitely anyway to make it clear what we need. helm-core is needed to
;; build all the helm-info-* commands and sources.
(require 'helm)
(require 'helm-lib)
(require 'helm-utils) ; for `helm-goto-line'.

(declare-function Info-index-nodes "info" (&optional file))
(declare-function Info-goto-node "info" (&optional fork))
(declare-function Info-find-node "info" (filename nodename &optional no-going-back))
(declare-function ring-insert "ring")
(declare-function ring-empty-p "ring")
(declare-function ring-ref "ring")
(defvar Info-history)
(defvar Info-directory-list)
;; `Info-minibuf-history' is not declared in Emacs, see emacs bug/58786.
(when (and (> emacs-major-version 28)
           (not (boundp 'Info-minibuf-history)))
  (defvar Info-minibuf-history nil))


;;; Customize

(defgroup helm-info nil
  "Info-related applications and libraries for Helm."
  :group 'helm)

(defcustom helm-info-default-sources
  '(helm-source-info-elisp
    helm-source-info-cl
    helm-source-info-eieio
    helm-source-info-pages)
  "Default sources to use for looking up symbols at point in Info
files with `helm-info-at-point'."
  :group 'helm-info
  :type '(repeat (choice symbol)))

;;; Build info-index sources with `helm-info-source' class.

(cl-defun helm-info-init (&optional (file (helm-get-attr 'info-file)))
  "Initialize candidates for info FILE.
If FILE have nodes, loop through all nodes and accumulate candidates
found in each node, otherwise scan only the current info buffer."
  ;; Allow reinit candidate buffer when using edebug.
  (helm-aif (and debug-on-error
                 (helm-candidate-buffer))
      (kill-buffer it))
  (unless (helm-candidate-buffer)
    (save-window-excursion
      (info file " *helm info temp buffer*")
      (let ((tobuf (helm-candidate-buffer 'global))
            Info-history)
        (helm-aif (Info-index-nodes)
            (dolist (node it)
              (Info-goto-node node)
              (helm-info-scan-current-buffer tobuf))
          (helm-info-scan-current-buffer tobuf))
        (bury-buffer)))))

(defun helm-info-scan-current-buffer (tobuf)
  "Scan current info buffer and print lines to TOBUF.
Argument TOBUF is the `helm-candidate-buffer'."
  (let (start end line)
    (goto-char (point-min))
    (while (search-forward "\n* " nil t)
      (unless (search-forward "Menu:\n" (1+ (pos-eol)) t)
        (setq start (pos-bol)
              ;; Fix Bug#1503 by getting the invisible
              ;; info displayed on next line in long strings.
              ;; e.g "* Foo.\n   (line 12)" instead of
              ;;     "* Foo.(line 12)"
              end (or (save-excursion
                        (goto-char (pos-bol))
                        (re-search-forward "(line +[0-9]+)" nil t))
                      (pos-eol))
              ;; Long string have a new line inserted before the
              ;; invisible spec, remove it.
              line (replace-regexp-in-string
                    "\n" "" (buffer-substring start end)))
        (with-current-buffer tobuf
          (insert line)
          (insert "\n"))))))

(defun helm-info-goto (node-line)
  "The helm-info action to jump to NODE-LINE."
  (require 'helm-utils)
  (let ((alive (buffer-live-p (get-buffer "*info*"))))
    (Info-goto-node (car node-line))
    (when alive (revert-buffer nil t))
    (helm-goto-line (cdr node-line))))

(defvar helm-info--node-regexp
  "^\\* +\\(.+\\):[[:space:]]+\\(.*\\)\\(?:[[:space:]]*\\)(line +\\([0-9]+\\))"
  "A regexp that should match file name, node name and line number in
a line like this:

\* bind:                                  Bash Builtins.       (line  21).")

(defun helm-info-display-to-real (line)
  "Transform LINE to an acceptable argument for `info'.
If line have a node use the node, otherwise use directly first name found."
  (let ((info-file (helm-get-attr 'info-file))
        nodename linum)
    (when (string-match helm-info--node-regexp line)
      (setq nodename (match-string 2 line)
            linum    (match-string 3 line)))
    (if nodename
        (cons (format "(%s)%s"
                      info-file
                      (replace-regexp-in-string ":\\'" "" nodename))
              (string-to-number (or linum "1")))
      (cons (format "(%s)%s"
                    info-file
                    (helm-aand (replace-regexp-in-string "^* " "" line)
                               (replace-regexp-in-string "::?.*\\'" "" it)))
            1))))

(defclass helm-info-source (helm-source-in-buffer)
  ((info-file :initarg :info-file
              :initform nil
              :custom 'string)
   (init :initform #'helm-info-init)
   (display-to-real :initform #'helm-info-display-to-real)
   (get-line :initform #'buffer-substring)
   (action :initform '(("Goto node" . helm-info-goto)))))

(defmacro helm-build-info-source (fname &rest args)
  `(helm-make-source (concat "Info Index: " ,fname) 'helm-info-source
     :info-file ,fname ,@args))

(defun helm-build-info-index-command (name doc source buffer)
  "Define a Helm command NAME with documentation DOC.
Arg SOURCE will be an existing helm source named
`helm-source-info-<NAME>' and BUFFER a string buffer name."
  (defalias (intern (concat "helm-info-" name))
      (lambda ()
        (interactive)
        (helm :sources source
              :buffer buffer
              :candidate-number-limit 1000))
    doc))

(defun helm-define-info-index-sources (info-list &optional commands)
  "Define Helm info sources for all entries in INFO-LIST.

Sources will be named named helm-source-info-<NAME> where NAME is an element of
INFO-LIST.

Sources are generated for all entries of `helm-default-info-index-list' which is
generated by `helm-get-info-files'.

If COMMANDS arg is non-nil, also build commands named `helm-info-<NAME>'."
  (cl-loop for str in info-list
           for sym = (intern (concat "helm-source-info-" str))
           do (set sym (helm-build-info-source str))
           when commands
           do (helm-build-info-index-command
               str (format "Predefined helm for %s info." str)
               sym (format "*helm info %s*" str))))

(defun helm-info-index-set (var value)
  (set var value)
  (helm-define-info-index-sources value t))

;;; Search Info files

;; `helm-info' is the main entry point here. It prompts the user for an Info
;; file, then a term in the file's index to jump to.

(defvar helm-info-searched (make-ring 32)
  "Ring of previously searched Info files.")

(defun helm-get-info-files ()
  "Return list of Info files to use for `helm-info'.

Elements of the list are strings of Info file names without
extensions (e.g., \"emacs\" for file \"emacs.info.gz\").  Info
files are found by searching directories in
`Info-directory-list'."
  (info-initialize) ; Build Info-directory-list from INFOPATH (Bug#2118)
  (let ((files (cl-loop for d in (or Info-directory-list
                                     Info-default-directory-list)
                        when (file-directory-p d)
                        append (directory-files d nil "\\.info"))))
    (helm-fast-remove-dups
     (cl-loop for f in files collect
              (helm-file-name-sans-extension f))
     :test 'equal)))

(defcustom helm-default-info-index-list
  (helm-get-info-files)
  "Info files to search in with `helm-info'."
  :group 'helm-info
  :type  '(repeat (choice string))
  :set   'helm-info-index-set)

(defun helm-info-search-index (candidate)
  "Search the index of CANDIDATE's Info file using the function
helm-info-<CANDIDATE>."
  (let ((helm-info-function
         (intern-soft (concat "helm-info-" candidate))))
    (when (fboundp helm-info-function)
      (funcall helm-info-function)
      (ring-insert helm-info-searched candidate))))

(defun helm-def-source--info-files ()
  "Return a Helm source for Info files."
  (helm-build-sync-source "Helm Info"
    :candidates
    (lambda () (copy-sequence helm-default-info-index-list))
    :candidate-number-limit 999
    :candidate-transformer
    (lambda (candidates)
      (sort candidates #'string-lessp))
    :nomark t
    :action '(("Search index" . helm-info-search-index))))

;;;###autoload
(defun helm-info (&optional refresh)
  "Preconfigured `helm' for searching Info files' indices.

With a prefix argument \\[universal-argument], set REFRESH to
non-nil.

Optional parameter REFRESH, when non-nil, re-evaluates
`helm-default-info-index-list'.  If the variable has been
customized, set it to its saved value.  If not, set it to its
standard value. See `custom-reevaluate-setting' for more.

REFRESH is useful when new Info files are installed.  If
`helm-default-info-index-list' has not been customized, the new
Info files are made available."
  (interactive "P")
  (let ((default (unless (ring-empty-p helm-info-searched)
                   (ring-ref helm-info-searched 0))))
    (when refresh
      (custom-reevaluate-setting 'helm-default-info-index-list))
    (helm :sources (helm-def-source--info-files)
          :buffer "*helm Info*"
          :preselect (and default
                          (concat "\\_<" (regexp-quote default) "\\_>")))))

;;;; Info at point

;; `helm-info-at-point' is the main entry point here. It searches for the
;; symbol at point through the Info sources defined in
;; `helm-info-default-sources' and jumps to it.

(defvar helm-info--pages-cache nil
  "Cache for all Info pages on the system.")

(defvar helm-source-info-pages
  (helm-build-sync-source "Info Pages"
    :init #'helm-info-pages-init
    :candidates (lambda () helm-info--pages-cache)
    :action '(("Show with Info" .
               (lambda (node-str)
                 (info (replace-regexp-in-string
                        "^[^:]+: " "" node-str)))))
    :requires-pattern 2)
  "Helm source for Info pages.")

(defun helm-info-pages-init ()
  "Collect candidates for initial Info node Top."
  (or helm-info--pages-cache
      (let ((info-topic-regexp "\\* +\\([^:]+: ([^)]+)[^.]*\\)\\."))
        (save-selected-window
          (info "dir" " *helm info temp buffer*")
          (Info-find-node "dir" "top")
          (goto-char (point-min))
          (while (re-search-forward info-topic-regexp nil t)
            (push (match-string-no-properties 1)
                  helm-info--pages-cache))
          (kill-buffer)))))

;;;###autoload
(defun helm-info-at-point ()
  "Preconfigured `helm' for searching info at point."
  (interactive)
  ;; Symbol at point is used as default as long as one of the sources
  ;; in `helm-info-default-sources' is member of
  ;; `helm-sources-using-default-as-input'.
  (cl-loop for src in helm-info-default-sources
           for name = (if (symbolp src)
                          (assoc 'name (symbol-value src))
                        (assoc 'name src))
           unless name
           do (warn "Couldn't build source `%S' without its info file" src))
  (helm :sources helm-info-default-sources
        :buffer "*helm info*"))

(provide 'helm-info)

;;; helm-info.el ends here
