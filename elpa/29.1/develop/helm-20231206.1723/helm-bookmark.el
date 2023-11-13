;;; helm-bookmark.el --- Helm for Emacs regular Bookmarks. -*- lexical-binding: t -*-

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
(require 'bookmark)
(require 'helm)
(require 'helm-lib)
(require 'helm-help)
(require 'helm-types)
(require 'helm-utils)
(require 'helm-info)
(require 'helm-adaptive)
(require 'helm-net)

(declare-function helm-browse-project "helm-files" (arg))
(declare-function addressbook-bookmark-edit "ext:addressbook-bookmark.el" (bookmark))
(declare-function all-the-icons-fileicon     "ext:all-the-icons.el")
(declare-function all-the-icons-icon-for-file"ext:all-the-icons.el")
(declare-function all-the-icons-octicon      "ext:all-the-icons.el")
(declare-function all-the-icons-match-to-alist "ext:all-the-icons.el")

(defvar all-the-icons-dir-icon-alist)


(defgroup helm-bookmark nil
  "Predefined configurations for `helm.el'."
  :group 'helm)

(defcustom helm-bookmark-show-location nil
  "Show location of bookmark on display."
  :type 'boolean)

(defcustom helm-bookmark-default-filtered-sources
  (append '(helm-source-bookmark-org
            helm-source-bookmark-files&dirs
            helm-source-bookmark-helm-find-files
            helm-source-bookmark-info
            helm-source-bookmark-gnus
            helm-source-bookmark-mu4e
            helm-source-bookmark-man
            helm-source-bookmark-images
            helm-source-bookmark-w3m)
          (list 'helm-source-bookmark-uncategorized
                'helm-source-bookmark-set))
  "List of sources to use in `helm-filtered-bookmarks'."
  :type '(repeat (choice symbol)))

(defcustom helm-bookmark-use-icon nil
  "Display candidates with an icon with `all-the-icons' when non nil.
Don't use `setq' to set this."
  :type 'boolean
  :set (lambda (var val)
         (if (require 'all-the-icons nil t)
             (set var val)
           (set var nil))))

(defcustom helm-bookmark-default-sort-method 'adaptive
  "Sort method for `helm-filtered-bookmarks'.

Value can be either \\='native' or \\='adaptive'.

Once you use \\='native' the bookmark variable `bookmark-sort-flag'
will be honored."
  :type '(choice
          (symbol :tag "Helm adaptive sort method" adaptive)
          (symbol :tag "Native bookmark sort method" native))
  ;; Don't use the :set function until functions and variables below
  ;; are not loaded i.e. use set-default only for now.
  :initialize 'custom-initialize-changed
  :set (lambda (var val)
         (set var val)
         (cl-loop for s in (remove 'helm-source-bookmark-set
                                   helm-bookmark-default-filtered-sources)
                  for fn = (intern (format "%s-builder" s))
                  do (set s (funcall fn)))))

(defgroup helm-bookmark-faces nil
  "Customize the appearance of helm-bookmark."
  :prefix "helm-"
  :group 'helm-bookmark
  :group 'helm-faces)

(defface helm-bookmark-info
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "green"))
  "Face used for W3m Emacs bookmarks (not w3m bookmarks)."
  :group 'helm-bookmark-faces)

(defface helm-bookmark-w3m
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "yellow"))
  "Face used for W3m Emacs bookmarks (not w3m bookmarks)."
  :group 'helm-bookmark-faces)

(defface helm-bookmark-gnus
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "magenta"))
  "Face used for Gnus bookmarks."
  :group 'helm-bookmark-faces)

(defface helm-bookmark-man
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "Orange4"))
  "Face used for Woman/man bookmarks."
  :group 'helm-bookmark-faces)

(defface helm-bookmark-file
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "Deepskyblue2"))
  "Face used for file bookmarks."
  :group 'helm-bookmark-faces)

(defface helm-bookmark-file-not-found
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "Slategray4"))
  "Face used for file bookmarks."
  :group 'helm-bookmark-faces)

(defface helm-bookmark-directory
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :inherit helm-ff-directory))
  "Face used for file bookmarks."
  :group 'helm-bookmark-faces)

(defface helm-bookmark-addressbook
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "tomato"))
  "Face used for addressbook bookmarks."
  :group 'helm-bookmark-faces)


(defvar helm-bookmark-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c o")   #'helm-bookmark-run-jump-other-window)
    (define-key map (kbd "C-c C-o") #'helm-bookmark-run-jump-other-frame)
    (define-key map (kbd "C-c C-t") #'helm-bookmark-run-jump-other-tab)
    (define-key map (kbd "C-d")     #'helm-bookmark-run-delete)
    (define-key map (kbd "C-]")     #'helm-bookmark-toggle-filename)
    (define-key map (kbd "M-e")     #'helm-bookmark-run-edit)
    map)
  "Generic Keymap for Emacs bookmark sources.")

(defclass helm-source-basic-bookmarks (helm-source-in-buffer helm-type-bookmark)
   ((init :initform (lambda ()
                      (bookmark-maybe-load-default-file)
                      (helm-init-candidates-in-buffer
                          'global
                        (if (and (fboundp 'bookmark-maybe-sort-alist)
                                 (fboundp 'bookmark-name-from-full-record))
                            (mapcar 'bookmark-name-from-full-record
                                    (bookmark-maybe-sort-alist))
                          (bookmark-all-names)))))
    (filtered-candidate-transformer :initform 'helm-bookmark-transformer)
    (find-file-target :initform #'helm-bookmarks-quit-an-find-file-fn)))

(defvar helm-source-bookmarks
  (helm-make-source "Bookmarks" 'helm-source-basic-bookmarks)
  "See (info \"(emacs)Bookmarks\").")

(defun helm-bookmark-transformer (candidates _source)
  (cl-loop for i in candidates
        for loc = (bookmark-location i)
        for len =  (string-width i)
        for trunc = (if (> len bookmark-bmenu-file-column)
                        (helm-substring i bookmark-bmenu-file-column)
                      i)
        for sep = (make-string (- (+ bookmark-bmenu-file-column 2)
                                  (length trunc))
                               ? )
        if helm-bookmark-show-location
        collect (cons (concat trunc sep (if (listp loc) (car loc) loc)) i)
        else collect i))

(defun helm-bookmark-toggle-filename-1 (_candidate)
  (let* ((real (helm-get-selection helm-buffer))
         (trunc (if (> (string-width real) bookmark-bmenu-file-column)
                    (helm-substring real bookmark-bmenu-file-column)
                    real)))
    (setq helm-bookmark-show-location (not helm-bookmark-show-location))
    (helm-update (if helm-bookmark-show-location
                     (regexp-quote trunc)
                   (regexp-quote real)))))

(helm-make-persistent-command-from-action helm-bookmark-toggle-filename
  "Toggle bookmark location visibility."
  'toggle-filename 'helm-bookmark-toggle-filename-1)

(defun helm-bookmark-jump-1 (candidate &optional fn)
  (let (;; FIXME Why is prefarg necessary here?
        (current-prefix-arg helm-current-prefix-arg)
        non-essential)
    (bookmark-jump candidate fn)))

(defun helm-bookmark-jump (candidate)
  "Jump to bookmark action."
  (helm-bookmark-jump-1 candidate))

(defun helm-bookmark-jump-other-frame (candidate)
  "Jump to bookmark in other frame action."
  (helm-bookmark-jump-1 candidate #'switch-to-buffer-other-frame))

(defun helm-bookmark-jump-other-window (candidate)
  "Jump to bookmark in other window action."
  (helm-bookmark-jump-1 candidate #'switch-to-buffer-other-window))

(defun helm-bookmark-jump-other-tab (candidate)
  "Jump to bookmark action."
  (cl-assert (fboundp 'tab-bar-mode) nil "Tab-bar-mode not available")
  (helm-bookmark-jump-1 candidate #'switch-to-buffer-other-tab))

;;; bookmark-set
;;
(defvar helm-source-bookmark-set
  (helm-build-dummy-source "Set Bookmark"
    :filtered-candidate-transformer
    (lambda (_candidates _source)
      (list (or (and (not (string= helm-pattern ""))
                     helm-pattern)
                "Enter a bookmark name to record")))
    :action '(("Set bookmark" . (lambda (candidate)
                                  (if (string= helm-pattern "")
                                      (message "No bookmark name given for record")
                                      (bookmark-set candidate))))))
  "See (info \"(emacs)Bookmarks\").")


;;; Predicates
;;
(defconst helm-bookmark--non-file-filename "   - no file -"
  "Name to use for `filename' entry, for non-file bookmarks.")

(defun helm-bookmark-gnus-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a Gnus bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (or (eq (bookmark-get-handler bookmark) 'bmkext-jump-gnus)
      (eq (bookmark-get-handler bookmark) 'gnus-summary-bookmark-jump)
      (eq (bookmark-get-handler bookmark) 'bookmarkp-jump-gnus)))

(defun helm-bookmark-mu4e-bookmark-p (bookmark)
  "Return non nil if BOOKMARK is a mu4e bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (memq (bookmark-get-handler bookmark)
        '(mu4e-bookmark-jump mu4e--jump-to-bookmark)))

(defun helm-bookmark-w3m-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a W3m bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (or (eq (bookmark-get-handler bookmark) 'bmkext-jump-w3m)
      (eq (bookmark-get-handler bookmark) 'bookmark-w3m-bookmark-jump)
      (eq (bookmark-get-handler bookmark) 'bookmarkp-jump-w3m)))

(defun helm-bookmark-woman-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a Woman bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (or (eq (bookmark-get-handler bookmark) 'bmkext-jump-woman)
      (eq (bookmark-get-handler bookmark) 'woman-bookmark-jump)
      (eq (bookmark-get-handler bookmark) 'bookmarkp-jump-woman)))

(defun helm-bookmark-man-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a Man bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (or (eq (bookmark-get-handler bookmark) 'bmkext-jump-man)
      (eq (bookmark-get-handler bookmark) 'Man-bookmark-jump)
      (eq (bookmark-get-handler bookmark) 'bookmarkp-jump-man)))

(defun helm-bookmark-woman-man-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is a Man or Woman bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (or (helm-bookmark-man-bookmark-p bookmark)
      (helm-bookmark-woman-bookmark-p bookmark)))

(defun helm-bookmark-info-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK is an Info bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (eq (bookmark-get-handler bookmark) 'Info-bookmark-jump))

(defun helm-bookmark-image-bookmark-p (bookmark)
  "Return non-nil if BOOKMARK bookmarks an image file."
  (if (stringp bookmark)
      (assq 'image-type (assq bookmark bookmark-alist))
    (assq 'image-type bookmark)))

(defun helm-bookmark-file-p (bookmark)
  "Return non-nil if BOOKMARK bookmarks a file or directory.
BOOKMARK is a bookmark name or a bookmark record.
This excludes bookmarks of a more specific kind (Info, Gnus, and W3m)."
  (let* ((filename   (bookmark-get-filename bookmark))
         (isnonfile  (equal filename helm-bookmark--non-file-filename)))
    (and filename (not isnonfile) (not (bookmark-get-handler bookmark)))))

(defun helm-bookmark-org-file-p (bookmark)
  (let* ((filename (bookmark-get-filename bookmark)))
    (or (string-suffix-p ".org" filename t)
        (string-suffix-p ".org_archive" filename t))))

(defun helm-bookmark-helm-find-files-p (bookmark)
  "Return non-nil if BOOKMARK bookmarks a `helm-find-files' session.
BOOKMARK is a bookmark name or a bookmark record."
  (eq (bookmark-get-handler bookmark) 'helm-ff-bookmark-jump))

(defun helm-bookmark-addressbook-p (bookmark)
  "Return non--nil if BOOKMARK is a contact recorded with addressbook-bookmark.
BOOKMARK is a bookmark name or a bookmark record."
  (if (listp bookmark)
      (string= (assoc-default 'type bookmark) "addressbook")
    (string= (assoc-default
              'type (assoc bookmark bookmark-alist)) "addressbook")))

(defun helm-bookmark-uncategorized-bookmark-p (bookmark)
  "Return non--nil if BOOKMARK match no known category."
  (cl-loop for pred in '(helm-bookmark-org-file-p
                         helm-bookmark-addressbook-p
                         helm-bookmark-gnus-bookmark-p
                         helm-bookmark-mu4e-bookmark-p
                         helm-bookmark-w3m-bookmark-p
                         helm-bookmark-woman-man-bookmark-p
                         helm-bookmark-info-bookmark-p
                         helm-bookmark-image-bookmark-p
                         helm-bookmark-file-p
                         helm-bookmark-helm-find-files-p
                         helm-bookmark-addressbook-p)
           never (funcall pred bookmark)))

(defun helm-bookmark-filter-setup-alist (fn)
  "Return a filtered `bookmark-alist' sorted alphabetically."
  (cl-loop for b in (if (and (fboundp 'bookmark-maybe-sort-alist)
                             (eq helm-bookmark-default-sort-method 'native))
                        (bookmark-maybe-sort-alist)
                      bookmark-alist)
           for name = (car b)
           when (funcall fn b) collect
           (propertize name 'location (bookmark-location name))))

;;; Bookmark handlers
;;
(defvar w3m-async-exec)
(defun helm-bookmark-jump-w3m (bookmark)
  "Jump to W3m bookmark BOOKMARK, setting a new tab.
If `browse-url-browser-function' is set to something else than
`w3m-browse-url' use it."
  (require 'helm-net)
  (let* ((file  (or (bookmark-prop-get bookmark 'filename)
                    (bookmark-prop-get bookmark 'url)))
         (buf   (generate-new-buffer-name "*w3m*"))
         (w3m-async-exec nil)
         ;; If user don't have anymore w3m installed let it browse its
         ;; bookmarks with default browser otherwise assume bookmark
         ;; have been bookmarked from w3m and use w3m.
         (browse-url-browser-function (or (and (fboundp 'w3m-browse-url)
                                               (executable-find "w3m")
                                               'w3m-browse-url)
                                          browse-url-browser-function))
         (really-use-w3m (equal browse-url-browser-function 'w3m-browse-url)))
    (helm-browse-url file really-use-w3m)
    (when really-use-w3m
      (bookmark-default-handler
       `("" (buffer . ,buf) . ,(bookmark-get-bookmark-record bookmark))))))

;; All bookmarks recorded with the handler provided with w3m
;; (`bookmark-w3m-bookmark-jump') will use our handler which open
;; the bookmark in a new tab or in an external browser depending
;; on `browse-url-browser-function'.
(defalias 'bookmark-w3m-bookmark-jump #'helm-bookmark-jump-w3m)

;; Provide compatibility with old handlers provided in external
;; packages bookmark-extensions.el and bookmark+.
(defalias 'bmkext-jump-woman #'woman-bookmark-jump)
(defalias 'bmkext-jump-man #'Man-bookmark-jump)
(defalias 'bmkext-jump-w3m #'helm-bookmark-jump-w3m)
(defalias 'bmkext-jump-gnus #'gnus-summary-bookmark-jump)
(defalias 'bookmarkp-jump-gnus #'gnus-summary-bookmark-jump)
(defalias 'bookmarkp-jump-w3m #'helm-bookmark-jump-w3m)
(defalias 'bookmarkp-jump-woman #'woman-bookmark-jump)
(defalias 'bookmarkp-jump-man #'Man-bookmark-jump)


;;;; Filtered bookmark sources
;;
;;
(defclass helm-source-filtered-bookmarks (helm-source-in-buffer helm-type-bookmark)
  ((filtered-candidate-transformer
    :initform (delq nil
                    `(,(and (eq helm-bookmark-default-sort-method 'adaptive)
                            'helm-adaptive-sort)
                       helm-highlight-bookmark)))
   (find-file-target :initform #'helm-bookmarks-quit-an-find-file-fn)))

(defun helm-bookmarks-quit-an-find-file-fn (source)
  (let* ((sel (helm-get-selection nil nil source))
         (bmk (assoc (replace-regexp-in-string "\\`\\*" "" sel)
                     bookmark-alist)))
    (helm-aif (bookmark-get-filename bmk)
        (if (and helm--url-regexp
                 (string-match helm--url-regexp it))
            it (expand-file-name it))
      (expand-file-name default-directory))))

(defun helm-bookmark-build-source (name buildfn &optional class &rest args)
  (apply #'helm-make-source name
         (or class 'helm-source-filtered-bookmarks)
         :init (lambda ()
                 (bookmark-maybe-load-default-file)
                 (helm-init-candidates-in-buffer
                     'global (funcall buildfn)))
         args))

;;; W3m bookmarks.
;;
(defun helm-bookmark-w3m-setup-alist ()
  "Specialized filter function for bookmarks w3m."
  (helm-bookmark-filter-setup-alist 'helm-bookmark-w3m-bookmark-p))

(defun helm-source-bookmark-w3m-builder ()
  (helm-bookmark-build-source "Bookmark W3m" #'helm-bookmark-w3m-setup-alist))

(defvar helm-source-bookmark-w3m (helm-source-bookmark-w3m-builder))

;;; Images
;;
(defun helm-bookmark-images-setup-alist ()
  "Specialized filter function for images bookmarks."
  (helm-bookmark-filter-setup-alist 'helm-bookmark-image-bookmark-p))

(defun helm-source-bookmark-images-builder ()
  (helm-bookmark-build-source "Bookmark Images" #'helm-bookmark-images-setup-alist))

(defvar helm-source-bookmark-images (helm-source-bookmark-images-builder))

;;; Woman Man
;;
(defun helm-bookmark-man-setup-alist ()
  "Specialized filter function for bookmarks w3m."
  (helm-bookmark-filter-setup-alist 'helm-bookmark-woman-man-bookmark-p))

(defun helm-source-bookmark-man-builder ()
  (helm-bookmark-build-source "Bookmark Woman&Man" #'helm-bookmark-man-setup-alist))

(defvar helm-source-bookmark-man (helm-source-bookmark-man-builder))

;;; Org files
;;
(defun helm-bookmark-org-setup-alist ()
  "Specialized filter function for Org file bookmarks."
  (helm-bookmark-filter-setup-alist 'helm-bookmark-org-file-p))

(defun helm-source-bookmark-org-builder ()
  (helm-bookmark-build-source "Bookmark Org files" #'helm-bookmark-org-setup-alist))

(defvar helm-source-bookmark-org (helm-source-bookmark-org-builder))

;;; Gnus
;;
(defun helm-bookmark-gnus-setup-alist ()
  "Specialized filter function for bookmarks gnus."
  (helm-bookmark-filter-setup-alist 'helm-bookmark-gnus-bookmark-p))

(defun helm-source-bookmark-gnus-builder ()
  (helm-bookmark-build-source "Bookmark Gnus" #'helm-bookmark-gnus-setup-alist))

(defvar helm-source-bookmark-gnus (helm-source-bookmark-gnus-builder))

;;; Mu4e
;;
(defun helm-bookmark-mu4e-setup-alist ()
  (helm-bookmark-filter-setup-alist 'helm-bookmark-mu4e-bookmark-p))

(defun helm-source-bookmark-mu4e-builder ()
  (helm-bookmark-build-source "Bookmark Mu4e" #'helm-bookmark-mu4e-setup-alist))

(defvar helm-source-bookmark-mu4e (helm-source-bookmark-mu4e-builder))

;;; Info
;;
(defun helm-bookmark-info-setup-alist ()
  "Specialized filter function for bookmarks info."
  (helm-bookmark-filter-setup-alist 'helm-bookmark-info-bookmark-p))

(defun helm-source-bookmark-info-builder ()
  (helm-bookmark-build-source "Bookmark Info" #'helm-bookmark-info-setup-alist))

(defvar helm-source-bookmark-info (helm-source-bookmark-info-builder))

;;; Files and directories
;;
(defun helm-bookmark-local-files-setup-alist ()
  "Specialized filter function for bookmarks locals files."
  (helm-bookmark-filter-setup-alist 'helm-bookmark-file-p))

(defun helm-source-bookmark-files&dirs-builder ()
  (helm-bookmark-build-source
   "Bookmark Files&Directories" #'helm-bookmark-local-files-setup-alist))

(defvar helm-source-bookmark-files&dirs
  (helm-source-bookmark-files&dirs-builder))

;;; Helm find files sessions.
;;
(defun helm-bookmark-helm-find-files-setup-alist ()
  "Specialized filter function for `helm-find-files' bookmarks."
  (helm-bookmark-filter-setup-alist 'helm-bookmark-helm-find-files-p))

(defun helm-bookmark-browse-project (candidate)
  "Run `helm-browse-project' from action."
  (with-helm-default-directory
      (bookmark-get-filename candidate)
      (helm-browse-project nil)))

(helm-make-command-from-action helm-bookmark-run-browse-project
  "Run `helm-bookmark-browse-project' from keyboard."
  'helm-bookmark-browse-project)

(defvar helm-bookmark-find-files-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-bookmark-map)
    (define-key map (kbd "C-x C-d") #'helm-bookmark-run-browse-project)
    map))

;; Same as `helm-source-filtered-bookmarks' but override actions and keymap
;; specifically for helm-find-files bookmarks.
(defclass helm-bookmark-override-inheritor (helm-source) ())

(cl-defmethod helm--setup-source ((source helm-bookmark-override-inheritor))
  ;; Ensure `helm-source-in-buffer' method is called.
  (cl-call-next-method)
  (setf (slot-value source 'action)
        (helm-append-at-nth
         (cl-loop for (name . action) in helm-type-bookmark-actions
                  ;; We don't want those actions in helm-find-files bookmarks.
                  unless (memq action '(helm-bookmark-jump-other-frame
                                        helm-bookmark-jump-other-window
                                        helm-bookmark-jump-other-tab))
                  collect (cons name action))
         '(("Browse project" . helm-bookmark-browse-project)) 1))
  (setf (slot-value source 'keymap) helm-bookmark-find-files-map))

(defclass helm-bookmark-find-files-class (helm-source-filtered-bookmarks
                                          helm-bookmark-override-inheritor)
  ())

(defun helm-source-bookmark-helm-find-files-builder ()
  (helm-bookmark-build-source
   "Bookmark helm-find-files sessions"
   #'helm-bookmark-helm-find-files-setup-alist
   'helm-bookmark-find-files-class
   :persistent-action (lambda (_candidate) (ignore))
   :persistent-help "Do nothing"))

(defvar helm-source-bookmark-helm-find-files
  (helm-source-bookmark-helm-find-files-builder))

;;; Uncategorized bookmarks
;;
(defun helm-bookmark-uncategorized-setup-alist ()
  "Specialized filter function for uncategorized bookmarks."
  (helm-bookmark-filter-setup-alist 'helm-bookmark-uncategorized-bookmark-p))

(defun helm-source-bookmark-uncategorized-builder ()
  (helm-bookmark-build-source
   "Bookmark uncategorized" #'helm-bookmark-uncategorized-setup-alist))

(defvar helm-source-bookmark-uncategorized
  (helm-source-bookmark-uncategorized-builder))


;;; Transformer
;;
(defun helm-highlight-bookmark (bookmarks _source)
  "Used as `filtered-candidate-transformer' to colorize bookmarks."
  (let ((non-essential t))
    (cl-loop for i in bookmarks
          for isfile        = (bookmark-get-filename i)
          for hff           = (helm-bookmark-helm-find-files-p i)
          for handlerp      = (and (fboundp 'bookmark-get-handler)
                                   (bookmark-get-handler i))
          for isw3m         = (and (fboundp 'helm-bookmark-w3m-bookmark-p)
                                   (helm-bookmark-w3m-bookmark-p i))
          for isgnus        = (and (fboundp 'helm-bookmark-gnus-bookmark-p)
                                   (helm-bookmark-gnus-bookmark-p i))
          for ismu4e        = (and (fboundp 'helm-bookmark-mu4e-bookmark-p)
                                   (helm-bookmark-mu4e-bookmark-p i))
          for isman         = (and (fboundp 'helm-bookmark-man-bookmark-p) ; Man
                                   (helm-bookmark-man-bookmark-p i))
          for iswoman       = (and (fboundp 'helm-bookmark-woman-bookmark-p) ; Woman
                                   (helm-bookmark-woman-bookmark-p i))
          for isannotation  = (bookmark-get-annotation i)
          for isabook       = (string= (bookmark-prop-get i 'type)
                                       "addressbook")
          for isinfo        = (eq handlerp 'Info-bookmark-jump)
          for loc = (bookmark-location i)
          for len =  (string-width i)
          for trunc = (if (and helm-bookmark-show-location
                               (> len bookmark-bmenu-file-column))
                          (helm-substring
                           i bookmark-bmenu-file-column)
                        i)
          for icon = (when helm-bookmark-use-icon
                       (cond ((and isfile hff)
                              (helm-aif (or (all-the-icons-match-to-alist
                                             (helm-basename (helm-basedir isfile t))
                                             all-the-icons-dir-icon-alist)
                                            (all-the-icons-match-to-alist
                                             (helm-basename isfile)
                                             all-the-icons-dir-icon-alist))
                                  (apply (car it) (cdr it))
                                (all-the-icons-octicon "file-directory")))
                             ((and isfile isinfo) (all-the-icons-octicon "info"))
                             (isfile (all-the-icons-icon-for-file (helm-basename isfile)))
                             ((or iswoman isman)
                              (all-the-icons-fileicon "man-page"))
                             ((or isgnus ismu4e)
                              (all-the-icons-octicon "mail-read"))))
          ;; Add a * if bookmark have annotation
          if (and isannotation (not (string-equal isannotation "")))
          do (setq trunc (concat "*" (if helm-bookmark-show-location trunc i)))
          for sep = (and helm-bookmark-show-location
                         (make-string (- (+ bookmark-bmenu-file-column 2)
                                         (string-width trunc))
                                      ? ))
          for bmk = (cond ( ;; info buffers
                           isinfo
                           (propertize trunc 'face 'helm-bookmark-info
                                       'help-echo isfile))
                          ( ;; w3m buffers
                           isw3m
                           (propertize trunc 'face 'helm-bookmark-w3m
                                       'help-echo isfile))
                          ( ;; gnus buffers
                           isgnus
                           (propertize trunc 'face 'helm-bookmark-gnus
                                       'help-echo isfile))
                          ( ;; Man Woman
                           (or iswoman isman)
                           (propertize trunc 'face 'helm-bookmark-man
                                       'help-echo isfile))
                          ( ;; Addressbook
                           isabook
                           (propertize trunc 'face 'helm-bookmark-addressbook))
                          (;; Directories (helm-find-files)
                           hff
                           (if (and (file-remote-p isfile)
                                    (not (file-remote-p isfile nil t)))
                               (propertize trunc 'face 'helm-bookmark-file-not-found
                                       'help-echo isfile)
                             (propertize trunc 'face 'helm-bookmark-directory
                                         'help-echo isfile)))
                          ( ;; Directories (dired)
                           (and isfile
                                ;; This is needed because `non-essential'
                                ;; is not working on Emacs-24.2 and the behavior
                                ;; of tramp seems to have changed since previous
                                ;; versions (Need to reenter password even if a
                                ;; first connection have been established,
                                ;; probably when host is named differently
                                ;; i.e machine/localhost)
                                (and (not (file-remote-p isfile))
                                     (file-directory-p isfile)))
                           (propertize trunc 'face 'helm-bookmark-directory
                                       'help-echo isfile))
                          ( ;; Non existing files.
                           (and isfile
                                ;; Be safe and call `file-exists-p'
                                ;; only if file is not remote or
                                ;; remote but connected.
                                (or (and (file-remote-p isfile)
                                         (not (file-remote-p isfile nil t)))
                                    (not (file-exists-p isfile))))
                           (propertize trunc 'face 'helm-bookmark-file-not-found
                                       'help-echo isfile))
                          ( ;; regular files
                           t
                           (propertize trunc 'face 'helm-bookmark-file
                                       'help-echo isfile)))
          collect (if helm-bookmark-show-location
                      (cons (concat (and icon (propertize " " 'display (concat icon " ")))
                                    bmk
                                    (propertize
                                     " " 'display
                                     (concat sep (if (listp loc) (car loc) loc))))
                            i)
                    (cons (concat (and icon (propertize " " 'display (concat icon " ")))
                                  bmk)
                          i)))))


;;; Edit/rename/save bookmarks.
;;
;;
(defun helm-bookmark-edit-bookmark (bookmark-name)
  "Edit bookmark's name and file name, and maybe save them.
BOOKMARK-NAME is the current (old) name of the bookmark to be
renamed."
  (let ((bmk (helm-bookmark-get-bookmark-from-name bookmark-name))
        (handler (bookmark-prop-get bookmark-name 'handler)))
    (if (eq handler 'addressbook-bookmark-jump)
        (addressbook-bookmark-edit
         (assoc bmk bookmark-alist))
      (helm-bookmark-edit-bookmark-1 bookmark-name handler))))

(defun helm-bookmark-edit-bookmark-1 (bookmark-name handler)
  (let* ((helm--reading-passwd-or-string t)
         (bookmark-fname (bookmark-get-filename bookmark-name))
         (bookmark-loc   (bookmark-prop-get bookmark-name 'location))
         (message-id     (bookmark-prop-get bookmark-name 'message-id))
         (new-name       (read-from-minibuffer "Name: " bookmark-name))
         (new-loc        (and (or bookmark-fname bookmark-loc)
                              (read-from-minibuffer "FileName or Location: "
                                                    (or bookmark-fname
                                                        (if (consp bookmark-loc)
                                                            (car bookmark-loc)
                                                          bookmark-loc)))))
         (new-message-id (and (memq handler '(mu4e--jump-to-bookmark
                                              mu4e-bookmark-jump))
                              (read-string "Message-id: " message-id))))
    (when (and (not (equal new-name ""))
               (or (not (equal new-loc ""))
                   (not (equal new-message-id "")))
               (y-or-n-p "Save changes? "))
      (if bookmark-fname
          (progn
            (helm-bookmark-rename bookmark-name new-name 'batch)
            (bookmark-set-filename new-name new-loc))
        (bookmark-prop-set
         (bookmark-get-bookmark bookmark-name)
         (cond (new-loc 'location)
               (new-message-id 'message-id))
         (or new-loc new-message-id))
        (helm-bookmark-rename bookmark-name new-name 'batch))
      (helm-bookmark-maybe-save-bookmark)
      (list new-name new-loc))))

(defun helm-bookmark-maybe-save-bookmark ()
  "Increment save counter and maybe save `bookmark-alist'."
  (setq bookmark-alist-modification-count (1+ bookmark-alist-modification-count))
  (when (bookmark-time-to-save-p) (bookmark-save)))

(defun helm-bookmark-rename (old &optional new batch)
  "Change bookmark's name from OLD to NEW.
Interactively:
 If called from the keyboard, then prompt for OLD.
 If called from the menubar, select OLD from a menu.
If NEW is nil, then prompt for its string value.

If BATCH is non-nil, then do not rebuild the menu list.

While the user enters the new name, repeated `C-w' inserts
consecutive words from the buffer into the new bookmark name."
  (interactive (list (bookmark-completing-read "Old bookmark name")))
  (bookmark-maybe-historicize-string old)
  (bookmark-maybe-load-default-file)
  (save-excursion (skip-chars-forward " ") (setq bookmark-yank-point (point)))
  (setq bookmark-current-buffer (current-buffer))
  (let ((newname  (or new  (read-from-minibuffer
                            "New name: " nil
                            (let ((now-map  (copy-keymap minibuffer-local-map)))
                              (define-key now-map  "\C-w" #'bookmark-yank-word)
                              now-map)
                            nil 'bookmark-history))))
    (bookmark-set-name old newname)
    (setq bookmark-current-bookmark  newname)
    (unless batch (bookmark-bmenu-surreptitiously-rebuild-list))
    (helm-bookmark-maybe-save-bookmark) newname))

(helm-make-command-from-action helm-bookmark-run-edit
  "Run `helm-bookmark-edit-bookmark' from keyboard."
  'helm-bookmark-edit-bookmark)


(helm-make-command-from-action helm-bookmark-run-jump-other-frame
  "Jump to bookmark other frame from keyboard."
  'helm-bookmark-jump-other-frame)

(helm-make-command-from-action helm-bookmark-run-jump-other-window
  "Jump to bookmark other window from keyboard."
  'helm-bookmark-jump-other-window)

(helm-make-command-from-action helm-bookmark-run-jump-other-tab
  "Jump to bookmark other tab from keyboard."
  'helm-bookmark-jump-other-tab)

(helm-make-command-from-action helm-bookmark-run-delete
  "Delete bookmark from keyboard."
  'helm-delete-marked-bookmarks)

(defun helm-bookmark-get-bookmark-from-name (bmk)
  "Return bookmark name even if it is a bookmark with annotation.
E.g. prepended with *."
  (let ((bookmark (replace-regexp-in-string "\\`\\*" "" bmk)))
    (if (assoc bookmark bookmark-alist) bookmark bmk)))

(defun helm-delete-marked-bookmarks (_ignore)
  "Delete this bookmark or all marked bookmarks."
  (dolist (i (helm-marked-candidates))
    (bookmark-delete (helm-bookmark-get-bookmark-from-name i)
                     'batch)))


;;;###autoload
(defun helm-bookmarks ()
  "Preconfigured `helm' for bookmarks."
  (interactive)
  (helm :sources '(helm-source-bookmarks
                   helm-source-bookmark-set)
        :buffer "*helm bookmarks*"
        :default (buffer-name helm-current-buffer)))

;;;###autoload
(defun helm-filtered-bookmarks ()
  "Preconfigured `helm' for bookmarks (filtered by category).
Optional source `helm-source-bookmark-addressbook' is loaded only
if external addressbook-bookmark package is installed."
  (interactive)
  (helm :sources helm-bookmark-default-filtered-sources
        :prompt "Search Bookmark: "
        :buffer "*helm filtered bookmarks*"
        :default (list (thing-at-point 'symbol)
                       (buffer-name helm-current-buffer))))

(provide 'helm-bookmark)

;;; helm-bookmark.el ends here
