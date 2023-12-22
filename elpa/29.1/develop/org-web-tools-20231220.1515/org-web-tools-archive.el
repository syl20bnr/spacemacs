;;; org-web-tools-archive.el --- Tools for archive.is  -*- lexical-binding: t -*-

;; Copyright (C) 2018-2023  Adam Porter

;; Author: Adam Porter <adam@alphapapa.net>
;; Url: http://github.com/alphapapa/org-web-tools

;;; Commentary:

;; This file contains code for retrieving archived content from archive.is.

;;; License:

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

;; TODO: Add new org link type "attachment:" that can link to entry attachments.

;;;; Requirements

(require 'browse-url)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'url-handlers)
(require 'url-util)

(require 'org-attach)

(require 'dash)
(require 'esxml-query)
(require 'request)

(require 'org-web-tools)

;;;; Variables

(defvar org-web-tools-archive-debug-level nil
  "See `request-log-level'.")

(defvar org-web-tools-attach-url-archive-attempts 0
  "Current number of attempts in a retry chain.")

(defvar org-web-tools-attach-url-archive-attempted-fns nil
  "Functions used to attempt archive download.")

;;;; Customization

(defgroup org-web-tools-archive nil
  "Options for archiving commands."
  :group 'org-web-tools)

(defcustom org-web-tools-archive-hostname "archive.today"
  "Domain name to make requests for \"archive.is\" to.
The service redirects to various domains."
  :type 'string)

(defcustom org-web-tools-attach-archive-retry 15
  "Retry attaching archives that aren't yet available."
  :type '(choice (integer :tag "Retry asynchronously after N seconds")
                 (const :tag "Don't retry, just give an error" nil)))

(defcustom org-web-tools-attach-archive-max-attempts 6
  "Number of times to try to attach archives asynchronously.
If you use archive.is, it often requires a minute or two to fully
archive a page, so consider the number of seconds set in
`org-web-tools-attach-archive-retry' when setting this."
  :type 'integer)

(defcustom org-web-tools-attach-archive-retry-fallback 'org-web-tools-archive-fn
  "Try other functions if retry limit is exceeded."
  :type '(choice (const :tag "Other functions in `org-web-tools-archive-fn'" org-web-tools-archive-fn)
                 (const :tag "Don't try other functions" nil)
                 (repeat :tag "Custom functions" function)))

(defcustom org-web-tools-archive-fn #'org-web-tools-archive--wget-tar
  "Function used to archive web pages."
  :type '(choice (const :tag "archive.is" org-web-tools-archive--archive.is)
                 (const :tag "wget | tar (with page resources)" org-web-tools-archive--wget-tar)
                 (const :tag "wget | tar (HTML only)" org-web-tools-archive--wget-tar-html-only)
                 (function :tag "Custom function")))

(defcustom org-web-tools-archive-compressor "xz"
  "Compressor for archives saved with Wget.
Filename extension for files made with tar-compatible
compressor (without \".tar.\").  Tar will call the appropriate
program for the extension."
  :type '(choice (const "xz")
                 (const "bzip2")
                 (const "gz")
                 (string :tag "Custom")))

(defcustom org-web-tools-archive-wget-options
  (list "--ignore-tags=script,iframe"
        "--reject=eot,ttf,svg,otf,*.woff*"
        "--execute" "robots=off"
        "--adjust-extension"
        "--span-hosts"
        "--convert-links"
        "--page-requisites"
        "--timestamping"
        "--no-directories")
  "Options passed to Wget.
Options which take arguments should have the option and argument
passed as separate strings, or with the argument separated by
\"=\".  Certain options are added automatically to facilitate
subsequent archiving, like \"--directory-prefix\"; options which
don't interfere with that are safe to add here."
  :type '(repeat string))

(defcustom org-web-tools-archive-wget-html-only-options
  (list "--execute robots=off"
        "--adjust-extension"
        "--timestamping"
        "--no-directories")
  "Options passed to Wget when only downloading HTML.
Options which take arguments should have the option and argument
passed as separate strings, or with the argument separated by
\"=\".  Certain options are added automatically to facilitate
subsequent archiving, like \"--directory-prefix\"; options which
don't interfere with that are safe to add here."
  :type '(repeat string))

;;;; Commands

(declare-function archive-find-type "arc-mode")
(declare-function org-web-tools--read-url "org-web-tools")

;;;###autoload
(defun org-web-tools-archive-attach (url &optional choose-fn-p view)
  "Download archive of page at URL and attach with `org-attach'.
If CHOOSE-FN-P is non-nil (interactively, with universal prefix),
prompt for the archive function to use.  If VIEW is
non-nil (interactively, with two universal prefixes), view the
archive immediately after attaching."
  (interactive (list (org-web-tools--read-url)
                     current-prefix-arg
                     (> (prefix-numeric-value current-prefix-arg) 4)))
  (let ((org-web-tools-archive-fn (if choose-fn-p
                                      (org-web-tools-archive--choose-archive-fn)
                                    org-web-tools-archive-fn)))
    (pcase-exhaustive (org-web-tools-attach-url-archive--1 url)
      ((and (pred stringp) size)
       (message "Attached %s archive of %s%s" size url
                (if org-web-tools-attach-url-archive-attempted-fns
                    (format " (retried with function %s)" org-web-tools-archive-fn)
                  ""))
       (when view
         ;; TODO: Pass the filename directly in case of multiple attachments, so the user doesn't have to pick the right one.
         (message "VIEWING")
         (org-web-tools-archive-view)))
      ('retrying (message "Archive not yet available.  Retrying in %s seconds (%s/%s attempts)"
                          org-web-tools-attach-archive-retry
                          ;; Increment attempts by one, because this function is
                          ;; first called outside of the lexical rebinding that
                          ;; increments it.
                          (1+ org-web-tools-attach-url-archive-attempts)
                          org-web-tools-attach-archive-max-attempts))
      ('retries-exceeded (if (not org-web-tools-attach-archive-retry-fallback)
                             (progn
                               (pop-to-buffer (current-buffer))
                               (error "Retry limit exceeded when attaching archive of %s.  Try again manually" url))
                           ;; Retry with other functions
                           (if-let* ((org-web-tools-attach-archive-max-attempts 0)
                                     (org-web-tools-archive-fn
                                      ;; Bind to untried function
                                      (car (seq-difference
                                            (pcase org-web-tools-attach-archive-retry-fallback
                                              ('org-web-tools-archive-fn
                                               ;; List default choices and current choice
                                               (-uniq (append (->> (get 'org-web-tools-archive-fn 'custom-type)
                                                                   cdr
                                                                   (--select (eq (car it) 'const))
                                                                   (-map #'-last-item))
                                                              (cdar (get 'org-web-tools-archive-fn 'customized-value)))))
                                              ((pred listp) org-web-tools-attach-archive-retry-fallback))
                                            org-web-tools-attach-url-archive-attempted-fns)))
                                     (org-web-tools-attach-url-archive-attempted-fns (cons org-web-tools-archive-fn org-web-tools-attach-url-archive-attempted-fns)))
                               (progn
                                 (message "Retrying with other functions...")
                                 (org-web-tools-archive-attach url))
                             (error "Unable to attach archive of %s, no functions left to try" url))))
      ('nil (error "Unable to archive %s.  Retry manually in a few seconds" url)))))

;;;###autoload
(defun org-web-tools-archive-view ()
  "Open Zip file archive of web page.
Extracts to a temp directory and opens with
`browse-url-default-browser'.  Note: the extracted files are left
on-disk in the temp directory."
  (interactive)
  (unless (executable-find "unzip")
    (error "Can't find unzip command"))
  (let* ((attach-dir (org-attach-dir t))
	 (files (org-attach-file-list attach-dir))
	 (file (if (= (length files) 1)
		   (car files)
		 (completing-read "Open attachment: "
				  (mapcar #'list files) nil t)))
         (extension (file-name-extension file))
         (archive-path (expand-file-name file attach-dir))
         (temp-dir (make-temp-file "org-web-tools-view-archive-" 'dir)))
    (with-temp-buffer
      (unless (zerop (pcase extension
                       ;; TODO: If/when we want to support only Emacs 26+, we
                       ;; can use the `rx' matcher instead of `file-name-extension',
                       ;; and easily test for e.g. ".tar.xz".
                       ("zip" (call-process (executable-find "unzip") nil t nil
                                            archive-path "-d" temp-dir))
                       ;; Assume that if it's not a zip file, it's a tar archive
                       ;; (`extension' will be just, e.g. "xz").
                       (_ (call-process (executable-find "tar") nil t nil
                                        "--auto-compress"
                                        "--extract"
                                        "--directory" temp-dir
                                        "--file" archive-path))))
        (error "Extraction of file failed: %s" (buffer-string))))
    (->> (directory-files temp-dir 'full-path (rx ".html" eos))
         (-map #'org-web-tools-archive-view--escape-filename)
         (--map (concat "file://" it))
         (-map #'browse-url-default-browser))
    (message "Files extracted to: %s" temp-dir)))

;;;; Functions

(defun org-web-tools-archive--choose-archive-fn ()
  "Return archive function.
Selects from `custom-type' values of `org-web-tools-archive-fn'."
  (let ((choices (cl-loop for choice in (cdr (plist-get (symbol-plist 'org-web-tools-archive-fn)
                                                        'custom-type))
                          for fn = (nth 3 choice)
                          when fn
                          collect (cons (plist-get (cdr choice) :tag)
                                        fn))))
    (alist-get (completing-read "Archive with: " choices) choices nil nil #'string=)))

(defun org-web-tools-archive-view--escape-filename (path)
  "Return PATH with filename component escaped.
In case it contains URL-unfriendly characters."
  (let* ((directory (file-name-directory path))
         (filename (file-name-nondirectory path)))
    (expand-file-name (url-hexify-string filename) directory)))

(defun org-web-tools-attach-url-archive--1 (url)
  "Return size in bytes if archive of URL is attached to entry at point.
Return `retrying' if attempt failed and retry timer was started.
Return nil if unsuccessful."
  ;; Rather than forcing `org-attach' to load when this package is loaded, we'll just load it here,
  ;; because `org-attach-attach' is not autoloaded.
  (require 'org-attach)
  (pcase (funcall org-web-tools-archive-fn url)
    ((and (pred stringp) local-path)
     ;; Archive returned: attach and return size
     (prog1 (file-size-human-readable (nth 7 (file-attributes local-path)))
       (org-attach-attach local-path nil 'mv)))
    ('nil
     ;; Archive failed
     (pcase-exhaustive org-web-tools-attach-archive-retry
       ('nil nil)       ;; No retry
       ((pred integerp) ;; Retry
        (let ((attempts org-web-tools-attach-url-archive-attempts)
              (id (org-id-get nil 'create)))
          (if (>= (cl-incf attempts) org-web-tools-attach-archive-max-attempts)
              'retries-exceeded
            (when (org-web-tools-archive--retry :id id :url url
                    :delay org-web-tools-attach-archive-retry
                    :attempts attempts)
              'retrying))))))))

(cl-defun org-web-tools-archive--retry (&key id url delay attempts)
  "Start and return a timer for arguments.
Timer calls FN to attach archive of URL to entry with ID after
DELAY seconds."
  (declare (indent defun))
  (let ((fn (lambda ()
              (let ((org-web-tools-attach-url-archive-attempts attempts))
                (org-with-point-at (or (org-id-find id 'marker)
                                       (error "Can't find entry %s to attach archive of %s at" id url))
                  (org-web-tools-archive-attach url))))))
    (run-at-time delay nil fn)))

;;;;; wget

(cl-defun org-web-tools-archive--wget-tar (url)
  "Return path to local archive of URL retrieved with wget and archived with tar.

Temporary files downloaded with wget are deleted, but the
temporary directory is not, because the archive is inside it."
  (cl-macrolet ((call-tar ()
                  `(progn
                     (cd "files")
                     (if (zerop (apply #'call-process "tar" nil t nil tar-args))
                         archive-path
                       (warn "tar failed: %s" (buffer-string))))))
    (when-let* ((temp-dir (make-temp-file "org-web-tools-archive-" 'dir))
                ;; TODO: Make archiver configurable.
                (archive-name (concat (url-hexify-string url)
                                      "--" (s-chop-prefix "org-web-tools-archive-"
                                                          (file-name-nondirectory (directory-file-name temp-dir)))
                                      ".tar." org-web-tools-archive-compressor))
                (archive-path (expand-file-name archive-name temp-dir))
                (wget-args (append (list "--no-directories" "--directory-prefix" "files")
                                   org-web-tools-archive-wget-options
                                   (list url)))
                (tar-args (list "--create" "--auto-compress" "--file" archive-path "./")))
      (unwind-protect
          (with-temp-buffer
            (cd temp-dir)
            (pcase (apply #'call-process "wget" nil t nil wget-args)
              (0 (call-tar))
              (code (message "%s" (prin1 (concat "wget output:\n\n" (buffer-string))))
                    (warn "wget exited with code %s, meaning that some errors were encountered.  They might be just 404s for some images.  Check the saved archived to be sure it was archived to your satisfaction.  The full output from wget is in the \"*Messages*\" buffer." code)
                    (call-tar))))
        (delete-directory (expand-file-name "files" temp-dir) 'recursive)))))

(defun org-web-tools-archive--wget-tar-html-only (url)
  "Return path to local archive of URL retrieved with wget and archived with tar.
Calls `org-web-tools-archive--wget-tar', but adjusts
`org-web-tools-archive-wget-options' to only download HTML, not
page requisites."
  (let ((org-web-tools-archive-wget-options org-web-tools-archive-wget-html-only-options))
    (org-web-tools-archive--wget-tar url)))

;;;;; archive.is

(defun org-web-tools-archive--archive.is (url)
  "Return path to local archive of URL retrieved from archive.is.

Caller is responsible for deleting archive's directory after
moving it."
  ;; Require `arc-mode' here for `archive-find-type'.  This avoids loading those packages until they are actually used.
  (require 'arc-mode)
  (when-let* ((archive-url (org-web-tools-archive--archive.is-archive-url url))
              (temp-dir (make-temp-file "org-web-tools-archive-" 'dir))
              (encoded-url (url-hexify-string url))
              (basename (concat encoded-url "--" (s-chop-prefix "org-web-tools-archive-"
                                                                (file-name-nondirectory (directory-file-name archive-url)))))
              (local-path (expand-file-name basename temp-dir)))
    (when (url-copy-file archive-url local-path 'ok-if-exists 'keep-time)
      (pcase (ignore-errors
               (with-temp-buffer
                 (insert-file-contents-literally local-path)
                 (archive-find-type)))
        ('zip local-path)
        (_ nil)))))

(defun org-web-tools-archive--archive.is-archive-url (url)
  "Return URL to Zip archive of URL."
  (when-let* ((id (org-web-tools-archive--archive.is-url-id url)))
    (concat "http://" org-web-tools-archive-hostname "/download/" id ".zip")))

(defun org-web-tools-archive--archive.is-url-id (url)
  "Return ID of most recent archive of URL."
  (let* ((submitid (org-web-tools-archive--archive.is-submitid))
         (submit-url (concat "https://" org-web-tools-archive-hostname "/submit/"))
         (data (list (cons "anyway" 1)
                     (cons "submitid" submitid)
                     (cons "url" url)))
         (response (org-web-tools-archive--request submit-url
                     :type "POST"
                     :data data
                     :timeout 10
                     :sync t))
         (refresh (request-response-header response "Refresh")))
    (when (string-match (rx "url=http" (optional "s") "://"
                            (1+ (not (any "/"))) "/"   ; hostname
                            (group (1+ anything)))     ; ID
                        refresh)
      (match-string 1 refresh))))

(defun org-web-tools-archive--archive.is-submitid ()
  "Return new submission ID string.
Signal error if unable to get it."
  (let* ((url (concat "https://" org-web-tools-archive-hostname "/"))
         (parser (lambda ()
                   (-let* ((tree (libxml-parse-html-region (point) (point-max)))
                           ((_element . (attrs)) (esxml-query "input[name=submitid]" tree)))
                     (alist-get 'value attrs))))
         (response (org-web-tools-archive--request url
                     :sync t
                     :parser parser
                     :success (cl-function
                               (lambda (&key data &allow-other-keys)
                                 data)))))
    (or (request-response-data response)
        (error "Unable to get submitid"))))

(defun org-web-tools-archive--request (&rest args)
  "Wrapper for `request'.
Passes ARGS."
  (declare (indent defun))
  ;; When using the curl backend with "POST", `request' always returns before
  ;; the request actually completes.  So we use the `url-retrieve' backend,
  ;; which seems to work correctly.
  (let ((request-log-level org-web-tools-archive-debug-level)
        (request-backend 'url-retrieve))
    (apply #'request args)))

;;;; Footer

(provide 'org-web-tools-archive)

;;; org-web-tools-archive.el ends here

;; Local Variables:
;; fill-column: 80
;; End:
