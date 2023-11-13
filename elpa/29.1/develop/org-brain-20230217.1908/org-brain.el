;;; org-brain.el --- Org-mode concept mapping         -*- lexical-binding: t; -*-

;; Copyright (C) 2017--2020 Erik Sjöstrand
;; MIT License

;; Author: Erik Sjöstrand <sjostrand.erik@gmail.com>
;; URL: http://github.com/Kungsgeten/org-brain
;; Keywords: outlines hypermedia
;; Package-Requires: ((emacs "25.1") (org "9.2"))
;; Version: 0.94

;;; Commentary:

;; org-brain implements a variant of concept mapping with org-mode, it is
;; inspired by The Brain software (http://thebrain.com).  An org-brain is a
;; network of org-mode entries, where each entry is a file or a headline, and
;; you can get a visual overview of the relationships between the entries:
;; parents, children, siblings and friends.  This visual overview can also be
;; used to browse your entries.  You can think of entries as nodes in a mind map,
;; or pages in a wiki.

;; All org files put into your `org-brain-path' directory will be considered
;; entries in your org-brain.  Headlines with an ID property in your entry file(s)
;; are also considered as entries.

;; Use `org-brain-visualize' to see the relationships between entries, quickly
;; add parents/children/friends/pins to an entry, and open them for editing.

;;; Code:

(require 'org-element)
(require 'org-attach)
(require 'org-agenda)
(require 'org-macs)
(require 'org-id)
(require 'picture)
(require 'subr-x)
(require 'seq)

(defgroup org-brain nil
  "Org-mode concept mapping"
  :prefix "org-brain-"
  :group 'org)

;;;; Custom vars

(defcustom org-brain-path (file-truename (expand-file-name "brain" org-directory))
  "The root directory of your org-brain.

`org-mode' files placed in this directory, or its subdirectories,
will be considered org-brain entries."
  :group 'org-brain
  :type '(directory))

(defcustom org-brain-scan-directories-recursively t
  "If subdirectories inside `org-brain-path' are considered part of the brain or not."
  :group 'org-brain
  :type '(boolean))

(defcustom org-brain-files-extension "org"
  "The extension for entry files in `org-brain-path'."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-ignored-resource-links '("fuzzy" "radio" "brain-child" "brain-parent" "brain-friend")
  "`org-link-types' which shouldn't be shown as resources in `org-brain-visualize'."
  :group 'org-brain
  :type '(repeat string))

(defcustom org-brain-backlink nil
  "If backlink resource should be added when creating a brain org-link.
This only works when completing the link via `org-insert-link'.
Example: If you create a brain-link in A to B, setting this
variable to non-nil would also create A as a resource in B.

If this variable is a string it will be added as a prefix in the backlink.
Example: \"<--\" would add \"<--A\" in the example above."
  :group 'org-brain
  :type '(restricted-sexp :match-alternatives
           (stringp 't 'nil)))

(defcustom org-brain-backlink-heading t
  "If the org heading should be used when creating a backlink.

Example: Creating a brain-link in A to B and A is an org file with the headings:
* Parent header
** Child
[brain:linkToB]

Setting this variable to t will create the following backlink in B:
[[file:A.org::*Child][Parent header > Child]]."
  :group 'org-brain
  :type '(boolean))

(make-obsolete-variable 'org-brain-suggest-stored-link-as-resource
                        "org-brain-suggest-stored-link-as-resource isn't needed because of `org-insert-link-global'."
                        "0.6")

(defcustom org-brain-data-file (file-truename (expand-file-name ".org-brain-data.el" org-brain-path))
  "Where org-brain data is saved."
  :group 'org-brain
  :type '(directory))

(load org-brain-data-file t t)

(defcustom org-brain-visualize-default-choices 'all
  "Which entries to choose from when using `org-brain-visualize'.
If 'all, choose from all file and headline entries.
If 'files, only choose from file entries.
If 'root, only choose from file entries in `org-brain-path' (non-recursive)."
  :group 'org-brain
  :type '(choice
          (const :tag "All entries" all)
          (const :tag "Only file entries" files)
          (const :tag "Only root file entries" root)))

(defcustom org-brain-include-file-entries t
  "If set to nil `org-brain' is optimized for headline entries.
Only headlines will be considered as entries when visualizing."
  :group 'org-brain
  :type '(boolean))

(make-obsolete-variable
 'org-brain-file-from-input-function
 "`org-brain-default-file-parent' can be used as a better alternative."
 "0.92")

(defcustom org-brain-default-file-parent nil
  "Where to store new entries with unspecified local parent.
For instance if creating a new entry with `org-brain-visualize'.
If nil, create the new entry as a file entry relative to `org-brain-path'.
If set to a string it should be a file entry. That entry will be used as the
local parent and the new entry will be a headline."
  :group 'org-brain
  :type '(choice string (const nil)))

(defcustom org-brain-show-full-entry nil
  "Always show entire entry contents?"
  :group 'org-brain
  :type '(boolean))

(defcustom org-brain-show-resources t
  "Should entry resources be shown in `org-brain-visualize'?"
  :group 'org-brain
  :type '(boolean))

(defcustom org-brain-show-text t
  "Should the entry text be shown in `org-brain-visualize'?"
  :group 'org-brain
  :type '(boolean))

(defcustom org-brain-show-history t
  "Should the navigation history be shown in `org-brain-visualize'?"
  :group 'org-brain
  :type '(boolean))

(defcustom org-brain-show-icons t
  "Should icons from `org-agenda-category-icon-alist' be shown when visualizing?"
  :group 'org-brain
  :type '(boolean))

(defcustom org-brain-category-icon-width 2
  "The character width of icons."
  :group 'org-brain
  :type '(integer))

(defcustom org-brain-quit-after-goto nil
  "Should the *org-brain* buffer window close itself after executing a goto command?"
  :group 'org-brain
  :type '(boolean))

(defcustom org-brain-headline-links-only-show-visible t
  "Only show visible parts (descriptions) of headline links.

See the docstring for `org-brain-headline-at' for more info
on how this is implemented."
  :group 'org-brain
  :type '(boolean))

(defcustom org-brain-file-entries-use-title t
  "If file entries should show their title, when choosing entries from a list.
This can potentially be slow.  If set to nil, the relative
filenames will be shown instead, which is faster."
  :group 'org-brain
  :type '(boolean))

(defcustom org-brain-scan-for-header-entries t
  "If org-brain should scan for header entries inside files.
Useful if you don't tend to use header entries in your workflow,
since scanning can be slow in long file entries.
This only affects selection prompts and not functions like `org-brain-headline-to-file'."
  :group 'org-brain
  :type '(boolean))

(defcustom org-brain-headline-entry-name-format-string "%s::%s"
  "How headline entries are represented when choosing entries.
This `format' string is used in `org-brain-entry-name' for headline entries.
`format' gets two objects: the file and the headline."
  :group 'org-brain
  :type '(string))
(defcustom org-brain-visualize-text-hook nil
  "Hook runs after inserting `org-brain-text' in `org-brain-visualize'.

Can be used to prettify the entry text, e.g.
`org-display-inline-images'."
  :group 'org-brain
  :type 'hook)

(defcustom org-brain-after-visualize-hook nil
  "Hook run after `org-brain-visualize', but before `org-brain-text'.
Can be used to prettify the buffer output, e.g. `ascii-art-to-unicode'."
  :group 'org-brain
  :type 'hook)

(defcustom org-brain-new-entry-hook nil
  "Hook run after a new headline entry has been created."
  :group 'org-brain
  :type 'hook)

(defcustom org-brain-visualize-follow-hook nil
  "Hook run after viewing an entry by means of `org-brain-visualize-follow'."
  :group 'org-brain
  :type 'hook)

(defcustom org-brain-after-resource-button-functions nil
  "Hook run during `org-brain-insert-resource-button'.
Insert a bullet, then run hook functions, then insert the actual button.
Each function must take a single argument: the org link to the resource.
Can for instance be used in combination with `all-the-icons'."
  :group 'org-brain
  :type 'hook)

(defcustom org-brain-vis-title-prepend-functions '(org-brain-entry-icon)
  "Functions which `org-brain-vis-title' use before inserting the entry title.
Each function should take the entry as the only argument, and
should return a string. The strings are prepended to the entry title."
  :group 'org-brain
  :type 'hook
  :options '(org-brain-entry-icon
             org-brain-entry-todo-state
             org-brain-entry-tags-string))

(defcustom org-brain-vis-title-append-functions '()
  "Functions which `org-brain-vis-title' use after inserting the entry title.
Each function should take the entry as the only argument, and
should return a string. The strings are appended to the entry title."
  :group 'org-brain
  :type 'hook
  :options '(org-brain-entry-icon
             org-brain-entry-todo-state
             org-brain-entry-tags-string))

(defcustom org-brain-vis-current-title-prepend-functions '()
  "Like `org-brain-vis-title-prepend-functions' for the current visualized entry.
First `org-brain-vis-title-prepend-functions' are ran, and then these."
  :group 'org-brain
  :type 'hook
  :options '(org-brain-entry-icon
             org-brain-entry-todo-state
             org-brain-entry-tags-string))

(defcustom org-brain-vis-current-title-append-functions '()
  "Like `org-brain-vis-title-append-functions' for the current visualized entry.
First `org-brain-vis-title-append-functions' are ran, and then these."
  :group 'org-brain
  :type 'hook
  :options '(org-brain-entry-icon
             org-brain-entry-todo-state
             org-brain-entry-tags-string))

(defcustom org-brain-exclude-text-tag "notext"
  "`org-mode' tag stopping `org-brain-visualize' from fetching entry text.
Only applies to headline entries."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-exclude-resouces-tag "resourceless"
  "`org-mode' tag stopping `org-brain-visualize' from fetching entry resources.
Only applies to headline entries."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-exclude-children-tag "childless"
  "`org-mode' tag which exclude the headline's children from org-brain's entries."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-show-children-tag "showchildren"
  "`org-mode' tag which get entire subtree from headline entry during `org-brain-text'."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-exclude-tree-tag "nobrain"
  "`org-mode' tag which exclude the headline and its children from org-brain's entries."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-exclude-siblings-tag "nosiblings"
  "`org-mode' tag which prevents the siblings of children of this node from being displayed."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-exclude-local-parent-tag "nolocalparent"
  "`org-mode' tag which prevents this node to be displayed as a local parent."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-each-child-on-own-line-tag "ownline"
  "`org-mode' tag which makes each child of the headline entry be listed on its own line."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-no-sort-children-tag "nosort"
  "`org-mode' tag which makes the children of the headline entry appear in file order rather than sorted."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-wander-interval 3
  "Seconds between randomized entries, when using `org-brain-visualize-wander'."
  :group 'org-brain
  :type 'integer)

(defcustom org-brain-title-max-length 0
  "If a title is longer than this, it'll be capped during `org-brain-visualize'.
If 0 or a negative value, the title won't be capped."
  :group 'org-brain
  :type 'integer)

(defcustom org-brain-cap-mind-map-titles nil
  "Whether to cap entries longer than org-brain-title-max-length in mind map visualization mode."
  :group 'org-brain
  :type '(boolean))

(defcustom org-brain-entry-separator ";"
  "Can be used as a separator when adding children, parents, or friends.
Doing so allows for adding multiple entries at once."
  :group 'org-brain
  :type '(string))

(make-obsolete-variable
 'org-brain-visualize-one-child-per-line
 "Setting `org-brain-child-linebreak-sexp' to 0 visualizes one child per line."
 "0.7")

(defcustom org-brain-child-linebreak-sexp 'fill-column
  "Where to break lines when visualizing children?
Reasonable values include:

'0: every child will be on its own line
'fill-column: lines will break at `fill-column'
'(window-width): lines will break at the width of the window
'most-positive-fixnum: All children will be on one line"
  :group 'org-brain
  :type '(sexp))

(defcustom org-brain-refile-max-level 1
  "The default max-level used by `org-brain-refile'."
  :group 'org-brain
  :type 'integer)

(defcustom org-brain-child-link-name "brain-child"
  "The name for `org-mode' links, creating child relationships.
Must be set before `org-brain' is loaded.
Insert links using `org-insert-link'."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-parent-link-name "brain-parent"
  "The name for `org-mode' links, creating parent relationships.
Must be set before `org-brain' is loaded.
Insert links using `org-insert-link'."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-friend-link-name "brain-friend"
  "The name for `org-mode' links, creating friend relationships.
Must be set before `org-brain' is loaded.
Insert links using `org-insert-link'."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-children-property-name "BRAIN_CHILDREN"
  "The name for the org-mode property in which child relationships are stored.
Must be set before `org-brain' is loaded."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-parents-property-name "BRAIN_PARENTS"
  "The name for the org-mode property in which brain relationships are stored.
Must be set before `org-brain' is loaded."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-friends-property-name "BRAIN_FRIENDS"
  "The name for the org-mode property in which friend relationships are stored.
Must be set before `org-brain' is loaded."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-edge-property-prefix-name "BRAIN_EDGE"
  "The prefix for the org-mode property in which edge annotations are stored.
Must be set before `org-brain' is loaded."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-resources-drawer-name "RESOURCES"
  "The org-mode drawer name in which resources of an entry are stored.
Must be set before `org-brain' is loaded."
  :group 'org-brain
  :type '(string))

(defcustom org-brain-open-same-window nil
  "Should `org-brain-visualize' open up in the same window it was launched in?"
  :group 'org-brain
  :type '(boolean))

(defcustom org-brain-completion-system 'default
  "The completion system to be used by `org-brain'."
  :group 'org-brain
  :type '(radio
          (const :tag "Ido" ido)
          (const :tag "Helm" helm)
          (const :tag "Ivy" ivy)
          (const :tag "Default" default)
          (function :tag "Custom function")))

;;;;; Faces and face helper functions

(defface org-brain-title
  '((t . (:inherit 'org-level-1)))
  "Face for the currently selected entry.")

(defface org-brain-wires
  `((t . (:inherit 'font-lock-comment-face :italic nil)))
  "Face for the wires connecting entries.")

(defface org-brain-button
  '((t . (:inherit button)))
  "Face for header-entry buttons in the org-brain visualize buffer.
File entries also use this, but also applies `org-brain-file-face-template'.")

(defface org-brain-parent
  '((t . (:inherit (font-lock-builtin-face org-brain-button))))
  "Face for the entries' linked header-entry parent nodes.
File entries also use this, but also applies `org-brain-file-face-template'.")

(defface org-brain-local-parent
  '((t . (:inherit org-brain-parent :weight bold)))
  "Face for the entries' local header-entry parent nodes.
File entries also use this, but also applies `org-brain-file-face-template'.")

(defface org-brain-child
  '((t . (:inherit org-brain-button)))
  "Face for the entries' linked header-entry child nodes.
File entries also use this, but also applies `org-brain-file-face-template'.")

(defface org-brain-local-child
  '((t . (:inherit org-brain-child :weight bold)))
  "Face for the entries' local header-entry child nodes.
File entries also use this, but also applies `org-brain-file-face-template'.")

(defface org-brain-sibling
  '((t . (:inherit org-brain-child)))
  "Face for the entries' header-entry sibling nodes.
File entries also use this, but also applies `org-brain-file-face-template'.")

(defface org-brain-local-sibling
  '((t . (:inherit org-brain-sibling :weight bold)))
  "Face for the entries' local header-entry sibling nodes.
An entry is a local sibling of another entry if they share a local parent.
File entries also use this, but also applies `org-brain-file-face-template'.")

(defface org-brain-friend
  '((t . (:inherit org-brain-button)))
  "Face for the entries' header-entry friend nodes.
File entries also use this, but also applies `org-brain-file-face-template'.")

(defface org-brain-pinned
  '((t . (:inherit org-brain-button)))

  "Face for pinned header entries.
File entries also use this, but also applies `org-brain-file-face-template'.")

(defface org-brain-selected-list
  '((t . (:inherit org-brain-pinned)))
  "Face for header entries in the selection list.
File entries also use this, but also applies `org-brain-file-face-template'.")

(defface org-brain-history-list
  '((t . (:inherit org-brain-pinned)))
  "Face for header entries in the history list.
File entries also use this, but also applies `org-brain-file-face-template'.")

(defface org-brain-file-face-template
  '((t . (:slant italic)))
  "Attributes of this face are added to file-entry faces.")

(defface org-brain-edge-annotation-face-template
  '((t . (:box t)))
  "Attributes of this face are added to links which have an edge annotation
to the visualized entry.")

;; This needs to be here or defface complains that it is undefined.
(defun org-brain-specified-face-attrs (face &optional frame)
  "Return a plist of all face attributes of FACE that are not `unspecified'.
If FRAME is not specified, `selected-frame' is used."
  (cl-labels ((alist->plist (alist)
                            (pcase alist
                              ('nil nil)
                              (`((,h1 . ,h2) . ,tail) `(,h1 . (,h2 . ,(alist->plist tail)))))))
    (alist->plist (seq-filter
                   (lambda (f) (not (equal (cdr f) 'unspecified)))
                   (face-all-attributes face (or frame (selected-frame)))))))

(defun org-brain-display-face (entry &optional face edge)
  "Return the final display face for ENTRY.
Takes FACE as a starting face, or `org-brain-button' if FACE is not specified.
Applies the attributes in `org-brain-edge-annotation-face-template',
`org-brain-selected-face-template', and `org-brain-file-face-template'
as appropriate.
EDGE determines if `org-brain-edge-annotation-face-template' should be used."
  (let ((selected-face-attrs
         (when (member entry org-brain-selected)
           (org-brain-specified-face-attrs 'org-brain-selected-face-template)))
        (file-face-attrs
         (when (org-brain-filep entry)
           (org-brain-specified-face-attrs 'org-brain-file-face-template))))
    (append (list :inherit (or face 'org-brain-button))
            selected-face-attrs
            file-face-attrs
            (when edge
              (org-brain-specified-face-attrs 'org-brain-edge-annotation-face-template)))))

(defface org-brain-selected-face-template
  `((t . ,(org-brain-specified-face-attrs 'highlight)))
  "Attributes of this face are added to the faces of selected entries.")

;;;; API

;; An entry is either a string or a list of three strings.
;; If a string, then the entry is a file.
;; If a list, then the entry is a headline:
;; ("file entry" "headline title" "ID")
;; There's also a special entry type: Nicknames
;; In the case of headline nicknames the car of the list is a symbol (instead of a string)
;; ('alias "headline title" "ID")

(defvar org-brain--vis-entry nil
  "The last entry argument to `org-brain-visualize'.")

(defvar org-brain--vis-entry-keywords nil
  "The `org-brain-keywords' of `org-brain--vis-entry'.")

(defvar org-brain--vis-history nil
  "History previously visualized entries.  Newest first.")

(defvar org-brain-resources-start-re (concat "^[ \t]*:" org-brain-resources-drawer-name ":[ \t]*$")
  "Regular expression matching the first line of a resources drawer.")

(defvar org-brain-keyword-regex "^#\\+[a-zA-Z_]+:"
  "Regular expression matching org keywords.")

(defvar org-brain-pins nil "List of pinned org-brain entries.")

(defvar org-brain-selected nil "List of selected org-brain entries.")

(defvar org-brain-headline-cache (make-hash-table :test 'equal)
  "Cache for headline entries. Updates when files have been saved.")

;;;###autoload
(defun org-brain-update-id-locations ()
  "Scan `org-brain-files' using `org-id-update-id-locations'."
  (interactive)
  (org-id-update-id-locations (org-brain-files)))

;;;###autoload
(defun org-brain-get-id ()
  "Get ID of headline at point, creating one if it doesn't exist.
Run `org-brain-new-entry-hook' if a new ID is created."
  (interactive)
  (or (org-id-get)
      (progn
        (run-hooks 'org-brain-new-entry-hook)
        (org-id-get nil t))))

;;;###autoload
(defun org-brain-switch-brain (directory)
  "Choose another DIRECTORY to be your `org-brain-path'."
  (interactive "D")
  (if (file-equal-p directory org-brain-path)
      (message "Current brain already is %s, no switch" directory)
    (setq org-brain-path directory)
    (setq org-brain-data-file (file-truename (expand-file-name ".org-brain-data.el" org-brain-path)))
    (unless (file-exists-p org-brain-data-file)
      (org-brain-save-data))
    (setq org-brain-pins nil)
    (setq org-brain--vis-history nil)
    (load org-brain-data-file t)
    (org-brain-update-id-locations)
    (message "Switched org-brain to %s" directory)))

(defun org-brain-maybe-switch-brain ()
  "Switch brain to `default-directory' if a file named \".org-brain-data.el\" exists there."
  (when (and (not (file-equal-p default-directory org-brain-path))
             (file-exists-p (file-truename (expand-file-name ".org-brain-data.el" default-directory))))
    (org-brain-switch-brain default-directory)))

(defun org-brain-filep (entry)
  "Return t if the ENTRY is a (potential) brain file."
  (stringp entry))

(defun org-brain-save-data ()
  "Save data to `org-brain-data-file'."
  ;; Code adapted from Magnar Sveen's multiple-cursors
  (with-temp-file org-brain-data-file
    (emacs-lisp-mode)
    (dolist (data '(org-brain-pins))
      (insert "(setq " (symbol-name data) "\n"
              "      '(")
      (newline-and-indent)
      (mapc #'(lambda (value)
                (insert (format "%S" value))
                (newline-and-indent))
            (symbol-value data))
      (insert "))")
      (newline))))

(defun org-brain-path-entry-name (path)
  "Get PATH as an org-brain entry name."
  (string-remove-suffix (concat "." org-brain-files-extension)
                        (file-relative-name (file-truename path)
                                            (file-truename org-brain-path))))

(defun org-brain-entry-path (entry &optional check-title)
  "Get path of org-brain ENTRY.
If CHECK-TITLE is non-nil, consider that ENTRY might be a file entry title."
  (let ((name (if (org-brain-filep entry)
                  (or (and check-title
                           org-brain-file-entries-use-title
                           (cdr
                            (assoc entry
                                   (mapcar (lambda (x)
                                             (cons (concat (file-name-directory x)
                                                           (org-brain-title x))
                                                   x))
                                           (org-brain-files t)))))
                      entry)
                (car entry))))
    (file-truename (expand-file-name (org-link-unescape (format "%s.%s" name org-brain-files-extension))
                      org-brain-path))))

(defun org-brain-files (&optional relative)
  "Get all org files (recursively) in `org-brain-path'.
If RELATIVE is t, then return relative paths and remove file extension.
Ignores \"dotfiles\"."
  (make-directory org-brain-path t)
  (if relative
      (mapcar #'org-brain-path-entry-name (org-brain-files))
    (if org-brain-scan-directories-recursively
        (directory-files-recursively
         org-brain-path (format "^[^.].*\\.%s$" org-brain-files-extension))
      (directory-files
       org-brain-path t (format "^[^.].*\\.%s$" org-brain-files-extension)))))

(defvar org-brain-link-re
  "\\[\\[\\(\\(?:[^][\\]\\|\\\\\\(?:\\\\\\\\\\)*[][]\\|\\\\+[^][]\\)+\\)]\\(?:\\[\\(\\(?:.\\|\\)+?\\)]\\)?]"
  "Regex matching an `org-mode' link.
The first match is the URI, the second is the (optional) desciption.

This variable should be the same as `org-link-bracket-re'.
However the implementation changed in `org-mode' 9.3 and
the old `org-bracket-link-regexp' had different match groups.
The purpose of `org-brain-link-re' is protection against future changes.")

(defun org-brain-replace-links-with-visible-parts (raw-str)
  "Get RAW-STR with its links replaced by their descriptions."
  (let ((ret-str "")
        (start 0)
        match-start)
    (while (setq match-start (string-match org-brain-link-re raw-str start))
      (setq ret-str
            (concat ret-str
                    ;; Include everything not part of the string.
                    (substring-no-properties raw-str start match-start)
                    ;; Include either the link description, or the link
                    ;; destination.
                    (or (match-string-no-properties 2 raw-str)
                        (match-string-no-properties 1 raw-str))))
      (setq start (match-end 0)))
    (concat ret-str (substring-no-properties raw-str start nil))))

(defun org-brain-headline-at (&optional pom)
  "Return the full headline of the entry at POM.

If `org-brain-headline-links-only-show-visible' is nil, the links
will be returned raw (all of the bracket syntax visible.)

If `org-brain-headline-links-only-show-visible' is non-nil,
returns only the visible parts of links in the heading.  (For any
links that have descriptions, only the descriptions will be
returned.)

This is done via regex, and does not depend on org-mode's
visibility rendering/formatting in-buffer."
  (let ((pom (or pom (point))))
    (if org-brain-headline-links-only-show-visible
        (org-brain-replace-links-with-visible-parts (org-entry-get pom "ITEM"))
      (org-entry-get pom "ITEM"))))

(defun org-brain--headline-entry-at-point (&optional create-id)
  "Get headline entry at point.
If CREATE-ID is non-nil, call `org-brain-get-id' first."
  (if create-id (org-brain-get-id))
  (when-let ((id (org-entry-get (point) "ID")))
    (list (org-brain-path-entry-name buffer-file-name)
          (org-brain-headline-at (point)) id)))

(defun org-brain-entry-at-point-excludedp ()
  "Return t if the entry at point is tagged as being excluded from org-brain."
  (let ((tags (org-get-tags)))
    (or (member org-brain-exclude-tree-tag tags)
        (and (member org-brain-exclude-children-tag tags)
             (not (member org-brain-exclude-children-tag
                          (org-get-tags nil t)))))))

(defun org-brain-id-exclude-taggedp (id)
  "Return t if ID is tagged as being excluded from org-brain."
  (org-with-point-at (org-id-find id t)
    (org-brain-entry-at-point-excludedp)))

(defun org-brain--name-and-id-at-point ()
  "Get name and id of headline entry at point.
Respect excluded entries."
  (unless (org-brain-entry-at-point-excludedp)
    (when-let ((id (org-entry-get (point) "ID")))
      (list (org-brain-headline-at (point)) id))))

(defun org-brain--nicknames-at-point ()
  "Get  nicknames of the headline entry at point."
  (when-let ((id (org-entry-get (point) "ID")))
    (mapcar (lambda (nickname)
              (list 'nickname nickname id))
            (org-entry-get-multivalued-property (point) "NICKNAMES"))))

(defun org-brain-headline-entries-in-file (file &optional no-temp-buffer)
  "Get a list of all headline (and nicknames) entries in FILE.
If the entries are cached in `org-brain-headline-cache', get  them from there.
Else the FILE is inserted in a temp buffer and get scanned for entries.
If NO-TEMP-BUFFER is non-nil, run the scanning in the current buffer instead."
  (if no-temp-buffer
      (let ((cached (gethash file org-brain-headline-cache nil)))
        (if (or (not cached)
                (not (equal (car cached)
                            (file-attribute-modification-time
                             (file-attributes file)))))
            (let ((file-entry (org-brain-path-entry-name file)))
              (insert-file-contents file nil nil nil 'replace)
              (cdr (puthash file (cons (file-attribute-modification-time
                                        (file-attributes file))
                                       (apply #'append
                                              (mapcar (lambda (entry) (cons file-entry entry))
                                                      (remove nil (org-map-entries
                                                                   #'org-brain--name-and-id-at-point)))
                                              (remove nil (org-map-entries #'org-brain--nicknames-at-point))))
                            org-brain-headline-cache)))
          (cdr cached)))
    (with-temp-buffer
      (delay-mode-hooks
        (org-mode)
        (org-brain-headline-entries-in-file file t)))))

(defun org-brain-headline-entries (&optional include-nicknames)
  "Get all org-brain headline entries.
INCLUDE-NICKNAMES also return duplicates for headlines with NICKNAMES property."
  (with-temp-buffer
    (delay-mode-hooks
      (org-mode)
      (apply #'append
             (mapcar
              (lambda (file)
                (seq-filter
                 (if include-nicknames
                     #'identity
                   (lambda (x) (stringp (car x))))
                 (org-brain-headline-entries-in-file file t)))
              (org-brain-files))))))

(defun org-brain-entry-from-id (id)
  "Get entry from ID."
  (unless org-id-locations (org-id-locations-load))
  (when-let ((path (gethash id org-id-locations)))
    (list (org-brain-path-entry-name path)
          (org-brain-headline-at (org-id-find id t))
          id)))

(defun org-brain-entry-identifier (entry)
  "Get identifier of ENTRY.
The identifier is an id if ENTRY is a headline.
If ENTRY is file, then the identifier is the relative file name."
  (if (org-brain-filep entry)
      (org-entry-protect-space entry)
    (nth 2 entry)))

(defun org-brain-entry-at-pt (&optional create-id)
  "Get current org-brain entry.
CREATE-ID asks to create an ID öif  there isn't  one already."
  (cond ((eq major-mode 'org-mode)
         (unless (string-prefix-p (file-truename org-brain-path)
                                  (file-truename (buffer-file-name)))
           (error "Not in a brain file"))
         (if org-brain-scan-for-header-entries
             (if (ignore-errors (org-get-heading))
                 (or (org-brain--headline-entry-at-point)
                     (when create-id
                       (let ((closest-parent
                              (save-excursion
                                (let ((e))
                                  (while (and (not e) (org-up-heading-safe))
                                    (setq e (org-brain--headline-entry-at-point)))
                                  (or e
                                      (when org-brain-include-file-entries
                                        (org-brain-path-entry-name (buffer-file-name))))))))
                         (if (y-or-n-p
                              (format "'%s' has no ID, create one%s? "
                                      (org-brain-headline-at)
                                      (if closest-parent
                                          (format " [else use local parent '%s']"
                                                  (org-brain-title closest-parent))
                                        "")))
                             (org-brain--headline-entry-at-point t)
                           (or (org-brain-entry-at-pt) (error "No entry at pt"))))))
               (if org-brain-include-file-entries
                   (org-brain-path-entry-name (buffer-file-name))
                 (error "Not under an org headline, and org-brain-include-file-entries is nil")))
           (org-brain-path-entry-name (buffer-file-name))))
        ((eq major-mode 'org-brain-visualize-mode)
         org-brain--vis-entry)
        (t
         (error "Not in org-mode or org-brain-visualize"))))

(defun org-brain-entry-name (entry)
  "Get name string of ENTRY."
  (if (org-brain-filep entry)
      (if org-brain-file-entries-use-title
          (concat (file-name-directory entry) (org-brain-title entry))
        entry)
    (format org-brain-headline-entry-name-format-string
            (org-brain-entry-name (car entry)) (cadr entry))))

(defun org-brain-entry-data (entry)
  "Run `org-element-parse-buffer' on ENTRY text."
  (with-temp-buffer
    (insert (org-brain-text entry t))
    (org-element-parse-buffer)))

(defun org-brain--file-targets (file)
  "Return alist of (name . entry-id) for all entries in FILE.
The list also includes nicknames from the NICKNAMES keyword/properties.
Should only be used in a temp-buffer."
  (let* ((file-relative (org-brain-path-entry-name file))
         (file-entry-name (org-brain-entry-name file-relative)))
    (remove
     nil
     (append
      (when org-brain-include-file-entries
        (apply
         #'append
         (list (cons file-entry-name file-relative))
         (mapcar (lambda (x)
                   (list (cons (org-entry-restore-space x) file-relative)))
                 (when-let ((nicknames (assoc "NICKNAMES" (org-brain-keywords file-relative))))
                   (split-string (cdr nicknames) " " t)))))
      (mapcar
       (lambda (x)
         (cons (format org-brain-headline-entry-name-format-string
                       file-entry-name
                       (nth 1 x))
               (nth 2 x)))
       (org-brain-headline-entries-in-file file t))))))

(defun org-brain--all-targets ()
  "Get an alist with (name . entry-id) of all targets in org-brain.
`org-brain-include-file-entries' and `org-brain-scan-for-header-entries'
affect the fetched targets."
  (if org-brain-scan-for-header-entries
      (with-temp-buffer
        (delay-mode-hooks
          (org-mode)
          (mapcan #'org-brain--file-targets
                  (org-brain-files))))
    (mapcar (lambda (x) (cons (org-brain-entry-name x) x))
            (org-brain-files t))))

(defun org-brain-completing-read (prompt choices &optional predicate require-match initial-input hist def inherit-input)
  "A version of `completing-read' which is tailored to `org-brain-completion-system'."
  (let ((args (list prompt choices predicate require-match initial-input hist def inherit-input)))
    (or (pcase org-brain-completion-system
          ('default (apply #'completing-read args))
          ('ido (apply #'ido-completing-read args))
          ('ivy (apply #'ivy-completing-read args))
          ('helm (apply #'helm-completing-read-default-1
                        (append args '("org-brain" "*org-brain-helm*")))))
        (funcall org-brain-completion-system prompt choices))))

(defun org-brain-get-entry-from-title (title &optional targets)
  "Search for TITLE in TARGETS and return an entry. Create it if non-existing.
TARGETS is an alist of (title . entry-id).
If TARGETS is nil then use `org-brain--all-targets'."
  (unless org-id-locations (org-id-locations-load))
  (let* ((targets (or targets (org-brain--all-targets)))
         (id (or (cdr (assoc title targets)) title)))
    (or
     ;; Headline entry exists, return it
     (org-brain-entry-from-id id)
     ;; File entry
     (progn
       (setq id (split-string id "::" t))
       (let* ((entry-path (org-brain-entry-path (car id) t))
              (entry-file (org-brain-path-entry-name entry-path)))
         (unless (file-exists-p entry-path)
           (if (and org-brain-default-file-parent (equal (length id) 1))
               (setq entry-file org-brain-default-file-parent
                     id `(,org-brain-default-file-parent ,(car id)))
             (make-directory (file-name-directory entry-path) t)
             (write-region "" nil entry-path)))
         (if (or (not org-brain-include-file-entries)
                 (equal (length id) 2)
                 (not (equal (car id) entry-file)))
             ;; Create new headline entry in file
             (org-with-point-at (org-brain-entry-marker entry-file)
               (if (and (not org-brain-include-file-entries)
                        (or
                         ;; Search heading without tags
                         (save-excursion
                           (re-search-forward (concat "\n\\* +" (regexp-quote (car id)) "[ \t]*$") nil t))
                         ;; Search heading with tags
                         (save-excursion
                           (re-search-forward (concat "\n\\* +" (regexp-quote (car id)) "[ \t]+:.*:$") nil t))))
                   (org-brain-entry-at-pt)
                 (goto-char (point-max))
                 (insert (concat "\n* " (or (cadr id) (car id))))
                 (let ((new-id (org-brain-get-id)))
                   (save-buffer)
                   (list entry-file (or (cadr id) (car id)) new-id))))
           entry-file))))))

;;;###autoload
(defun org-brain-add-entry (title)
  "Add a new entry named TITLE."
  (interactive "sNew entry: ")
  (message "Added new entry: '%s'"
           (org-brain-entry-name (org-brain-get-entry-from-title title))))

(defun org-brain-choose-entries (prompt entries &optional predicate require-match initial-input hist def inherit-input-method)
  "PROMPT for one or more ENTRIES, separated by `org-brain-entry-separator'.
ENTRIES can be a list, or 'all which lists all headline and file entries.
Return the prompted entries in a list.
Very similar to `org-brain-choose-entry', but can return several entries.

For PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF and
INHERIT-INPUT-METHOD see `completing-read'."
  (let* ((targets (if (eq entries 'all)
                      (org-brain--all-targets)
                    (mapcar (lambda (x)
                              (cons (org-brain-entry-name x)
                                    (if (org-brain-filep x)
                                        x
                                      (nth 2 x))))
                            entries)))
         (choices (org-brain-completing-read prompt targets
                                             predicate require-match initial-input hist def inherit-input-method)))
    (mapcar (lambda (title) (org-brain-get-entry-from-title title targets))
            (if org-brain-entry-separator
                (split-string choices org-brain-entry-separator)
              (list choices)))))

(defun org-brain-choose-entry (prompt entries &optional predicate require-match initial-input hist def inherit-input-method)
  "PROMPT for an entry from ENTRIES and return it.
ENTRIES can be 'all, which lists all headline and file entries.
For PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF and INHERIT-INPUT-METHOD see `completing-read'."
  (let ((org-brain-entry-separator nil))
    (car (org-brain-choose-entries prompt entries predicate require-match initial-input hist def inherit-input-method))))

(defun org-brain-first-headline-position ()
  "Get position of first headline in buffer.  `point-max' if no headline exists."
  (save-excursion
    (goto-char (point-min))
    (or (looking-at-p org-heading-regexp)
        (outline-next-heading)
        (goto-char (point-max)))
    (point)))

(defun org-brain-keywords (entry)
  "Get alist of `org-mode' keywords and their values in file ENTRY."
  (if (org-brain-filep entry)
      (with-temp-buffer
        (insert
         (with-temp-buffer
           (ignore-errors (insert-file-contents (org-brain-entry-path entry)))
           (buffer-substring-no-properties (point-min) (org-brain-first-headline-position))))
        (org-element-map (org-element-parse-buffer) 'keyword
          (lambda (kw)
            (cons (org-element-property :key kw)
                  (org-element-property :value kw)))))
    (error "Only file entries have keywords")))

(defun org-brain-get-tags (entry &optional inherit)
  "Return the tags at ENTRY. Only use local tags unless INHERIT is non-nil.
Works for both file and headline entries."
  (if (org-brain-filep entry)
      (ignore-errors
        (split-string
         (cdr (assoc "FILETAGS" (org-brain-keywords entry))) ":" t))
    (org-with-point-at
        (org-brain-entry-marker entry)
      (org-get-tags nil (not inherit)))))

(defun org-brain-entry-tags-string (entry)
  "Get a string of ENTRY's local tags."
  (let ((tags (string-join (org-brain-get-tags entry) ":")))
    (if (string-empty-p tags)
        ""
      (concat ":" tags ":"))))

(defun org-brain-entry-todo-state (entry)
  "Get the todo-state of ENTRY.
Only works on headline entries."
  (if (org-brain-filep entry)
      ""
    (org-with-point-at (org-brain-entry-marker entry)
      (or (org-get-todo-state) ""))))

(defun org-brain--missing-id-error (entry)
  "Error message to be shown if id of ENTRY isn't found by `org-id-find'."
  (error "Couldn't find entry %s, try running org-brain-update-id-locations. "
         (org-brain-entry-name entry)))

(defun org-brain-entry-marker (entry)
  "Get marker to ENTRY."
  (if (org-brain-filep entry)
      (let ((path (org-brain-entry-path entry)))
        (if (file-exists-p path)
            (set-marker (make-marker) 0
                        (or (org-find-base-buffer-visiting path)
                            (find-file-noselect path)))
          ;; If file doesn't exists, it is probably an id
          (or (org-id-find entry t)
              (org-brain--missing-id-error entry))))
    (or (org-id-find (nth 2 entry) t)
        (org-brain--missing-id-error entry))))

(defun org-brain-title (entry &optional capped)
  "Get title of ENTRY.  If CAPPED is t, max length is `org-brain-title-max-length'."
  (let ((title
         (if (org-brain-filep entry)
             (or (cdr (assoc "TITLE" (org-brain-keywords entry)))
                 (car (last (split-string entry "/" t))))
           (nth 1 entry))))
    (if (and capped (> org-brain-title-max-length 0) (> (length title) org-brain-title-max-length))
        (concat (substring title 0 (1- org-brain-title-max-length)) "…")
      title)))

(defun org-brain-text-positions (entry &optional all-data)
  "Get the beginning and end position of the ENTRY text.
Only get the body text, unless ALL-DATA is t."
  (if (org-brain-filep entry)
      ;; File entry
      (with-temp-buffer
        (ignore-errors (insert-file-contents (org-brain-entry-path entry)))
        (goto-char (org-brain-first-headline-position))
        (list
         (if all-data
             (point-min)
           (or (save-excursion
                 (when (re-search-backward org-brain-keyword-regex nil t)
                   (end-of-line)
                   (point)))
               (point-min)))
         (if (let ((filetags (org-brain-get-tags entry)))
               (or org-brain-show-full-entry
                   (member org-brain-show-children-tag filetags)
                   (member org-brain-exclude-children-tag filetags)))
             (point-max)
           (point))))
    ;; Headline entry
    (org-with-point-at (org-brain-entry-marker entry)
      (let ((tags (org-get-tags nil t)))
        (unless (and (member org-brain-exclude-text-tag tags)
                     (not all-data))
          (unless all-data
            (goto-char (cdr (org-get-property-block)))
            (end-of-line))
          (let (end)
            (save-excursion
              (or (and (not org-brain-show-full-entry)
                       (not (member org-brain-exclude-children-tag tags))
                       (not (member org-brain-show-children-tag tags))
                       (org-goto-first-child))
                  (org-end-of-subtree t))
              (setq end (point)))
            (list (point) end)))))))

(defun org-brain-text (entry &optional all-data)
  "Get the text of ENTRY as string.
Only get the body text, unless ALL-DATA is t."
  (when-let ((entry-text
              (if (org-brain-filep entry)
                  ;; File entry
                  (with-temp-buffer
                    (ignore-errors (insert-file-contents (org-brain-entry-path entry)))
                    (apply #'buffer-substring-no-properties
                           (org-brain-text-positions entry all-data)))
                ;; Headline entry
                (org-with-point-at (org-brain-entry-marker entry)
                  (apply #'buffer-substring-no-properties
                         (org-brain-text-positions entry all-data))))))
    (if all-data
        (org-remove-indentation entry-text)
      (with-temp-buffer
        (insert (org-remove-indentation entry-text))
        (goto-char (org-brain-first-headline-position))
        (if (re-search-backward org-brain-resources-start-re nil t)
            (progn
              (end-of-line)
              (re-search-forward org-drawer-regexp nil t))
          (goto-char (point-min)))
        (buffer-substring (point) (point-max))))))

(defun org-brain-parents (entry)
  "Get parents of ENTRY.
Often you want the siblings too, then use `org-brain-siblings' instead."
  (delete-dups
   (append (org-brain--linked-property-entries entry org-brain-parents-property-name)
           (org-brain-local-parent entry))))

(defun org-brain-local-parent (entry)
  "Get file local parent of ENTRY, as a list."
  (if-let ((parent
            (unless (org-brain-filep entry)
              (org-with-point-at (org-brain-entry-marker entry)
                (if (and (org-up-heading-safe)
                         (org-entry-get nil "ID"))
                    (org-brain-entry-from-id (org-entry-get nil "ID"))
                  (when (and org-brain-include-file-entries
                             (not (member org-brain-exclude-local-parent-tag
                                          (org-brain-get-tags (car entry)))))
                    (car entry)))))))
      (list parent)))

(defun org-brain-children (entry)
  "Get children of ENTRY."
  (delete-dups
   (append (org-brain--linked-property-entries entry org-brain-children-property-name)
           (org-brain-local-children entry))))

(defun org-brain-local-children (entry)
  "Get file local children of ENTRY."
  (remove
   entry
   (if (org-brain-filep entry)
       ;; File entry
       (with-temp-buffer
         (ignore-errors (insert-file-contents (org-brain-entry-path entry)))
         (org-element-map (org-element-parse-buffer 'headline) 'headline
           (lambda (headline)
             (when-let ((id (org-element-property :ID headline)))
               (unless (org-brain-id-exclude-taggedp id)
                 (org-brain-entry-from-id id))))
           nil nil 'headline))
     ;; Headline entry
     (org-with-point-at (org-brain-entry-marker entry)
       (let (children)
         (deactivate-mark)
         (org-mark-subtree)
         (org-goto-first-child)
         (setq children
               (org-map-entries
                (lambda () (org-brain-entry-from-id (org-entry-get nil "ID")))
                t 'region-start-level
                (lambda ()
                  (let ((id (org-entry-get nil "ID")))
                    (when (or (not id)
                              (org-brain-id-exclude-taggedp id))
                      (save-excursion
                        (outline-next-heading)
                        (point)))))))
         (deactivate-mark)
         children)))))

(defun org-brain-descendants (entry)
  "Get all entries which descend from ENTRY.
In other words get all the children, grand children, grand-grand children, etc.
The ENTRY itself is also included in the returned list."
  (let ((checked nil))
    (cl-labels ((collect-descendants
                 (e)
                 (unless (member e checked)
                   (push e checked)
                   (mapc #'collect-descendants (org-brain-children e)))))
      (collect-descendants entry)
      checked)))

(defun org-brain-local-descendants (entry)
  "Return the local descendants of ENTRY (excluding ENTRY itself).
Similar to `org-brain-descendants' but only for local children."
  (remove
   entry
   (if (org-brain-filep entry)
       ;; File entry
       (with-temp-buffer
         (ignore-errors (insert-file-contents (org-brain-entry-path entry)))
         (org-element-map (org-element-parse-buffer 'headline) 'headline
           (lambda (headline)
             (when-let ((id (org-element-property :ID headline)))
               (unless (org-brain-id-exclude-taggedp id)
                 (org-brain-entry-from-id id))))))
     ;; Headline entry
     (org-with-point-at (org-brain-entry-marker entry)
       (org-map-entries
        (lambda () (org-brain-entry-from-id (org-entry-get nil "ID")))
        t 'tree
        (lambda ()
          (let ((id (org-entry-get nil "ID")))
            (when (or (not id)
                      (org-brain-id-exclude-taggedp id))
              (or (outline-next-heading)
                  (point))))))))))

(defun org-brain-siblings (entry)
  "Get siblings of ENTRY.
Return an alist where key = parent, value = siblings from that parent."
  (delete-dups
   (mapcar
    (lambda (parent)
      (cons parent (remove entry (org-brain-children parent))))
    (org-brain-parents entry))))

(defun org-brain-friends (entry)
  "Get friends of ENTRY."
  (delete-dups (org-brain--linked-property-entries entry org-brain-friends-property-name)))

(defun org-brain-resources (entry)
  "Get alist of links in ENTRY, excluding `org-brain-ignored-resource-links'.
A link can be either an org link or an org attachment.
The car is the raw-link and the cdr is the description."
  (let ((links
         (delete-dups
          (with-temp-buffer
            (insert (org-brain-text entry t))
            (org-element-map (org-brain-entry-data entry) 'link
              (lambda (link)
                (unless (member (org-element-property :type link)
                                org-brain-ignored-resource-links)
                  (cons (org-element-property :raw-link link)
                        (when-let ((beg (org-element-property :contents-begin link))
                                   (end (org-element-property :contents-end link)))
                          (replace-regexp-in-string
                           "[ \t\n\r]+" " " (buffer-substring beg end))))))
              nil nil t)))))
    (if (org-brain-filep entry)
        links
      ;; Headline entry
      (org-with-point-at (org-brain-entry-marker entry)
        (unless (member org-brain-exclude-resouces-tag (org-get-tags nil t))
          (append links
                  ;; Attachments
                  (when-let ((attach-dir (org-attach-dir)))
                    (mapcar (lambda (attachment)
                              (cons (format "file:%s"
                                            (org-link-escape
                                             (file-truename (expand-file-name attachment attach-dir))))
                                    attachment))
                            (org-attach-file-list attach-dir)))))))))

(defun org-brain--choose-resource (entries)
  "Use `completing-read' to get link to a resource from ENTRIES."
  (let ((resources (mapcan
                    (lambda (entry)
                      (mapcar (lambda (x)
                                (cons (or (cdr x) (car x)) (car x)))
                              (org-brain-resources entry)))
                    entries)))
    (if (equal (length resources) 1)
        (cdar resources)
      (cdr (assoc (org-brain-completing-read "Resource: " resources nil t) resources)))))

;;;###autoload
(defun org-brain-open-resource (entry)
  "Choose and open a resource from ENTRY.
If run with `\\[universal-argument]' then also choose from descendants of ENTRY.
Uses `org-brain-entry-at-pt' for ENTRY, or asks for it if none at point."
  (interactive (list (or (ignore-errors (org-brain-entry-at-pt t))
                         (org-brain-choose-entry "Resource from: " 'all))))
  (org-open-link-from-string
   (format "[[%s]]" (org-brain--choose-resource
                     (if current-prefix-arg
                         (org-brain-descendants entry)
                       (list entry))))))

(defun org-brain--linked-property-entries (entry property)
  "Get list of entries linked to in ENTRY by PROPERTY.
PROPERTY could for instance be `org-brain-children-property-name'."
  (let ((propertylist
         (if (org-brain-filep entry)
             ;; File entry
             (mapcar
              (lambda (x) (or (org-brain-entry-from-id x) x))
              (mapcar #'org-entry-restore-space
                      (when-let ((kw-values (cdr (assoc property
                                                        (org-brain-keywords entry)))))
                        (org-split-string kw-values "[ \t]+"))))
           ;; Headline entry
           (mapcar
            (lambda (x) (or (org-brain-entry-from-id x) x))
            (org-entry-get-multivalued-property (org-brain-entry-marker entry) property)))))
    (if (equal propertylist '("")) nil propertylist)))

(defun org-brain-add-relationship (parent child)
  "Add external relationship between PARENT and CHILD."
  (when (equal parent child)
    (error "An entry can't be a parent/child to itself"))
  (unless (member child (org-brain-children parent))
    (org-save-all-org-buffers)
    (if (org-brain-filep parent)
        ;; Parent = File
        (org-with-point-at (org-brain-entry-marker parent)
          (goto-char (point-min))
          (if (re-search-forward (concat "^#\\+" org-brain-children-property-name ":.*$") nil t)
              (insert (concat " " (org-brain-entry-identifier child)))
            (insert (concat "#+" org-brain-children-property-name ": "
                            (org-brain-entry-identifier child)
                            "\n\n"))))
      ;; Parent = Headline
      (org-entry-add-to-multivalued-property (org-brain-entry-marker parent)
                                             org-brain-children-property-name
                                             (org-brain-entry-identifier child)))
    (if (org-brain-filep child)
        ;; Child = File
        (org-with-point-at (org-brain-entry-marker child)
          (goto-char (point-min))
          (if (re-search-forward (concat "^#\\+" org-brain-parents-property-name ":.*$") nil t)
              (insert (concat " " (org-brain-entry-identifier parent)))
            (insert (concat "#+" org-brain-parents-property-name ": "
                            (org-brain-entry-identifier parent)
                            "\n\n"))))
      ;; Child = Headline
      (org-entry-add-to-multivalued-property (org-brain-entry-marker child)
                                             org-brain-parents-property-name
                                             (org-brain-entry-identifier parent)))
    (org-save-all-org-buffers)))

(defun org-brain-delete-current-line (&optional match-regex)
  "Delete whole line at `point', and the newline.
Optionally only delete if matching MATCH-REGEX."
  (when (or (not match-regex)
            (string-match match-regex (buffer-substring
                                       (line-beginning-position)
                                       (line-end-position))))
    (delete-region (line-beginning-position)
                   (progn (forward-line 1) (point)))))

(defun org-brain-remove-relationship (parent child)
  "Remove external relationship between PARENT and CHILD."
  (unless (member child (org-brain-children parent))
    (error "Relationship doesn't exist"))
  (org-save-all-org-buffers)
  (if (org-brain-filep parent)
      ;; Parent = File
      (org-with-point-at (org-brain-entry-marker parent)
        (goto-char (point-min))
        (re-search-forward (concat "^#\\+" org-brain-children-property-name ":.*$"))
        (beginning-of-line)
        (re-search-forward (concat " " (regexp-quote (org-brain-entry-identifier child))))
        (replace-match "")
        (org-brain-delete-current-line (concat "^#\\+" org-brain-children-property-name ":[[:space:]]*$"))
        (org-brain-delete-current-line "^[[:space:]]*$")
        (save-buffer))
    ;; Parent = Headline
    (org-entry-remove-from-multivalued-property (org-brain-entry-marker parent)
                                                org-brain-children-property-name
                                                (org-brain-entry-identifier child)))
  (if (org-brain-filep child)
      ;; Child = File
      (org-with-point-at (org-brain-entry-marker child)
        (goto-char (point-min))
        (re-search-forward (concat "^#\\+" org-brain-parents-property-name ":.*$"))
        (beginning-of-line)
        (re-search-forward (concat " " (regexp-quote (org-brain-entry-identifier parent))))
        (replace-match "")
        (org-brain-delete-current-line (concat "^#\\+" org-brain-parents-property-name ":[[:space:]]*$"))
        (org-brain-delete-current-line "^[[:space:]]*$")
        (save-buffer))
    ;; Child = Headline
    (org-entry-remove-from-multivalued-property (org-brain-entry-marker child)
                                                org-brain-parents-property-name
                                                (org-brain-entry-identifier parent)))
  (org-save-all-org-buffers))

;;;; Buffer commands

;;;###autoload
(defun org-brain-add-child (entry children &optional verbose)
  "Add external CHILDREN (a list of entries) to ENTRY.
If called interactively use `org-brain-entry-at-pt' and let user choose entry.
Using `\\[universal-argument]' will use `org-brain-button-at-point' as ENTRY.
If chosen CHILD entry doesn't exist, create it as a new file.
Several children can be added, by using `org-brain-entry-separator'.
If VERBOSE is non-nil then display a message."
  (interactive (list (if current-prefix-arg
                         (car (org-brain-button-at-point))
                       (org-brain-entry-at-pt t))
                     (org-brain-choose-entries "Add child: " 'all)
                     t))
  (dolist (child-entry children)
    (org-brain-add-relationship entry child-entry)
    (if verbose (message "Added '%s' as a child of '%s'."
                         (org-brain-entry-name child-entry)
                         (org-brain-entry-name entry))))
  (org-brain--revert-if-visualizing))

;;;###autoload
(defun org-brain-add-child-headline (entry child-names &optional verbose)
  "Create new internal child headline(s) to ENTRY named CHILD-NAMES.
Several children can be created, by using `org-brain-entry-separator'.
If called interactively use `org-brain-entry-at-pt' and prompt for children.
Using `\\[universal-argument]' will use `org-brain-button-at-point' as ENTRY.
If VERBOSE is non-nil then display a message."
  (interactive (list (if current-prefix-arg
                         (car (org-brain-button-at-point))
                       (org-brain-entry-at-pt t))
                     (read-string "Add child headline: ")
                     t))
  (dolist (child-name (split-string child-names org-brain-entry-separator))
    (when (equal (length child-name) 0)
      (error "Child name must be at least 1 character"))
    (if (org-brain-filep entry)
        ;; File entry
        (org-with-point-at (org-brain-entry-marker entry)
          (goto-char (org-brain-first-headline-position))
          (open-line 1)
          (insert (concat "* " child-name))
          (org-brain-get-id)
          (save-buffer))
      ;; Headline entry
      (org-with-point-at (org-brain-entry-marker entry)
        (if (org-goto-first-child)
            (open-line 1)
          (org-end-of-subtree t))
        (org-insert-heading nil t)
        (org-do-demote)
        (insert child-name)
        (org-brain-get-id)
        (save-buffer)))
    (if verbose (message "Added '%s' as a child of '%s'."
                         child-name
                         (org-brain-entry-name entry))))
  (org-brain--revert-if-visualizing))

(define-obsolete-function-alias 'org-brain-new-child 'org-brain-add-child-headline "0.5")

;;;###autoload
(defun org-brain-remove-child (entry child &optional verbose)
  "Remove CHILD from ENTRY.
If called interactively use `org-brain-entry-at-point' and prompt for CHILD.
Using `\\[universal-argument]' will use `org-brain-button-at-point' as ENTRY.
If VERBOSE is non-nil then display a message."
  (interactive (let ((e (if current-prefix-arg
                            (car (org-brain-button-at-point))
                          (org-brain-entry-at-pt))))
                 (list e (org-brain-choose-entry "Remove child: "
                                                 (org-brain-children e)
                                                 nil t)
                       t)))
  (if (member child (org-brain-local-children entry))
      (if (and (> (length (org-brain-parents child)) 1)
               (y-or-n-p
                (format "%s is %s's local parent. Would you like to change the local parent of %s? "
                        (org-brain-title entry) (org-brain-title child) (org-brain-title child))))
          (let* ((linked-parents (org-brain--linked-property-entries child org-brain-parents-property-name))
                 (new-parent (if (equal 1 (length linked-parents))
                                 (car-safe linked-parents)
                               (org-brain-choose-entry "Refile to parent: " linked-parents))))
            (org-brain-remove-relationship entry (org-brain-change-local-parent child new-parent)))
        (org-brain-delete-entry child))
    (org-brain-remove-relationship entry child))
  (if verbose (message "'%s' is no longer a child of '%s'."
                       (org-brain-entry-name child)
                       (org-brain-entry-name entry)))
  (org-brain--revert-if-visualizing))

;;;###autoload
(defun org-brain-add-parent (entry parents &optional verbose)
  "Add external PARENTS (a list of entries) to ENTRY.
If called interactively use `org-brain-entry-at-pt' and prompt for PARENT.
Using `\\[universal-argument]' will use `org-brain-button-at-point' as ENTRY.

If chosen parent entry doesn't exist, create it as a new file.
Several parents can be added, by using `org-brain-entry-separator'.
If VERBOSE is non-nil then display a message."
  (interactive (list (if current-prefix-arg
                         (car (org-brain-button-at-point))
                       (org-brain-entry-at-pt t))
                     (org-brain-choose-entries "Add parent: " 'all)
                     t))
  (dolist (parent parents)
    (org-brain-add-relationship parent entry)
    (if verbose (message "Added '%s' as a parent of '%s'."
                         (org-brain-entry-name parent)
                         (org-brain-entry-name entry))))
  (org-brain--revert-if-visualizing))

;;;###autoload
(defun org-brain-remove-parent (entry parent &optional verbose)
  "Remove PARENT from ENTRY.
If called interactively use `org-brain-entry-at-pt' and prompt for PARENT.
Using `\\[universal-argument]' will use `org-brain-button-at-point' as ENTRY."
  (interactive (let ((e (if current-prefix-arg
                            (car (org-brain-button-at-point))
                          (org-brain-entry-at-pt))))
                 (list e (org-brain-choose-entry "Remove parent: "
                                                 (org-brain-parents e)
                                                 nil t)
                       t)))
  (if (member entry (org-brain-local-children parent))
      (if-let* ((linked-parents (org-brain--linked-property-entries entry org-brain-parents-property-name))
                (new-parent (if (equal 1 (length linked-parents))
                                (car-safe linked-parents)
                              (org-brain-choose-entry (format "Removing %s's local parent. Refile to: "
                                                              (org-brain-title entry))
                                                      linked-parents))))
          (org-brain-remove-relationship parent (org-brain-change-local-parent entry new-parent))
        (if (and org-brain-default-file-parent
                 (y-or-n-p (format "%s has no more parents, move it to %s? "
                                   (org-brain-title entry) org-brain-default-file-parent)))
            (org-brain-remove-relationship
             parent (org-brain-change-local-parent entry org-brain-default-file-parent))
          (error "%s is %s's only parent, it can't be removed"
                 (org-brain-title parent) (org-brain-title entry))))
    (org-brain-remove-relationship parent entry))
  (if verbose (message "'%s' is no longer a parent of '%s'."
                       (org-brain-entry-name parent)
                       (org-brain-entry-name entry)))
  (org-brain--revert-if-visualizing))

(defun org-brain--internal-add-friendship (entry1 entry2 &optional oneway)
  "Add friendship between ENTRY1 and ENTRY2.
If ONEWAY is t, add ENTRY2 as friend of ENTRY1, but not the other way around."
  (when (equal entry1 entry2)
    (error "Can't have an entry as a friend to itself"))
  (unless (member entry2 (org-brain-friends entry1))
    (if (org-brain-filep entry1)
        ;; Entry1 = File
        (org-with-point-at (org-brain-entry-marker entry1)
          (goto-char (point-min))
          (if (re-search-forward (concat "^#\\+" org-brain-friends-property-name ":.*$") nil t)
              (insert (concat " " (org-brain-entry-identifier entry2)))
            (insert (concat "#+" org-brain-friends-property-name ": "
                            (org-brain-entry-identifier entry2)
                            "\n\n")))
          (save-buffer))
      ;; Entry1 = Headline
      (org-entry-add-to-multivalued-property (org-brain-entry-marker entry1)
                                             org-brain-friends-property-name
                                             (org-brain-entry-identifier entry2))))
  (unless oneway (org-brain--internal-add-friendship entry2 entry1 t))
  (org-save-all-org-buffers))

;;;###autoload
(defun org-brain-add-friendship (entry friends &optional verbose)
  "Add a new FRIENDS (a list of entries) to ENTRY.
If called interactively use `org-brain-entry-at-pt' and prompt for FRIENDS.
Using `\\[universal-argument]' will use `org-brain-button-at-point' as ENTRY.

If chosen friend entry doesn't exist, create it as a new file.
Several friends can be added, by using `org-brain-entry-separator'.
If VERBOSE is non-nil then display a message."
  (interactive (list (if current-prefix-arg
                         (car (org-brain-button-at-point))
                       (org-brain-entry-at-pt t))
                     (org-brain-choose-entries "Add friend: " 'all)
                     t))
  (dolist (friend-entry friends)
    (org-brain--internal-add-friendship entry friend-entry)
    (if verbose (message "'%s' and '%s' are now friends."
                         (org-brain-entry-name entry)
                         (org-brain-entry-name friend-entry))))
  (org-brain--revert-if-visualizing))

;;;###autoload
(defun org-brain-remove-friendship (entry1 entry2 &optional oneway verbose)
  "Remove friendship between ENTRY1 and ENTRY2.
If ONEWAY is t, then remove ENTRY2 as a friend of ENTRY1, but not vice versa.

If run interactively, use `org-brain-entry-at-pt' as ENTRY1 and prompt for ENTRY2.
Using `\\[universal-argument]' will use `org-brain-button-at-point' as ENTRY1.
If VERBOSE is non-nil then display a message."
  (interactive
   (let ((entry-at-pt (if current-prefix-arg
                          (car (org-brain-button-at-point))
                        (org-brain-entry-at-pt))))
     (list entry-at-pt
           (org-brain-choose-entry "Remove friend: " (org-brain-friends entry-at-pt) nil t)
           nil t)))
  (when (member entry2 (org-brain-friends entry1))
    (if (org-brain-filep entry1)
        ;; Entry1 = File
        (org-with-point-at (org-brain-entry-marker entry1)
          (goto-char (point-min))
          (re-search-forward (concat "^#\\+" org-brain-friends-property-name ":.*$"))
          (beginning-of-line)
          (re-search-forward (concat " " (regexp-quote (org-brain-entry-identifier entry2))))
          (replace-match "")
          (org-brain-delete-current-line (concat "^#\\+" org-brain-friends-property-name ":[[:space:]]*$"))
          (org-brain-delete-current-line "^[[:space:]]*$")
          (save-buffer))
      ;; Entry2 = Headline
      (org-entry-remove-from-multivalued-property (org-brain-entry-marker entry1)
                                                  org-brain-friends-property-name
                                                  (org-brain-entry-identifier entry2))))
  (if oneway
      (org-brain--revert-if-visualizing)
    (org-brain-remove-friendship entry2 entry1 t verbose))
  (org-save-all-org-buffers)
  (if (and (not oneway) verbose)
      (message "'%s' and '%s' are no longer friends."
               (org-brain-entry-name entry1)
               (org-brain-entry-name entry2))))

;;;###autoload
(defun org-brain-goto (&optional entry goto-file-func)
  "Goto buffer and position of org-brain ENTRY.
If ENTRY isn't specified, ask for the ENTRY.
Unless GOTO-FILE-FUNC is nil, use `pop-to-buffer-same-window' for opening the entry."
  (interactive)
  (org-brain-stop-wandering)
  (unless entry (setq entry (org-brain-choose-entry "Goto entry: " 'all)))
  (when (and org-brain-quit-after-goto (eq 'major-mode 'org-brain-visualize-mode))
    (org-brain-visualize-quit))
  (let ((marker (org-brain-entry-marker entry)))
    (apply (or goto-file-func #'pop-to-buffer-same-window)
           (list (marker-buffer marker)))
    (widen)
    (goto-char (marker-position marker))
    (when (org-at-heading-p)
      (org-show-entry)
      (org-show-subtree)))
  entry)

(define-obsolete-function-alias 'org-brain-open 'org-brain-goto "0.4")

;;;###autoload
(defun org-brain-goto-other-window (&optional entry)
  "Goto buffer and position of org-brain ENTRY in other window.
If ENTRY isn't specified, ask for the ENTRY."
  (interactive)
  (org-brain-goto entry #'pop-to-buffer))

;;;###autoload
(defun org-brain-goto-end (&optional entry same-window)
  "Like `org-brain-goto', but visits the end of ENTRY.
If SAME-WINDOW is t, use the current window.
If ENTRY isn't specified, ask for the ENTRY."
  (interactive)
  (if (org-brain-filep (org-brain-goto entry (if same-window nil #'pop-to-buffer)))
      (or (outline-next-heading)
          (goto-char (point-max)))
    (let ((tags (org-get-tags nil t)))
      (or (and (not (member org-brain-exclude-children-tag tags))
               (not (member org-brain-show-children-tag tags))
               (org-goto-first-child))
          (org-end-of-subtree t)))))

;;;###autoload
(defun org-brain-goto-current (&optional same-window)
  "Use `org-brain-goto' on `org-brain-entry-at-pt', in other window..
If run with `\\[universal-argument]', or SAME-WINDOW as t, use current window."
  (interactive "P")
  (if same-window
      (org-brain-goto (org-brain-entry-at-pt))
    (org-brain-goto (org-brain-entry-at-pt) #'pop-to-buffer)))

;;;###autoload
(defun org-brain-goto-child (entry &optional all)
  "Goto a child of ENTRY.
If run interactively, get ENTRY from context.
If ALL is nil, choose only between externally linked children."
  (interactive (list (org-brain-entry-at-pt)))
  (let* ((entries (if all (org-brain-children entry)
                    (org-brain--linked-property-entries
                     entry org-brain-children-property-name)))
         (child (cond
                 ((equal 1 (length entries)) (car-safe entries))
                 ((not entries) (error (concat entry " has no children")))
                 (t (org-brain-choose-entry "Goto child: " entries nil t)))))
    (org-brain-goto child)))

;;;###autoload
(defun org-brain-goto-parent (entry &optional all)
  "Goto a parent of ENTRY.
If run interactively, get ENTRY from context.
If ALL is nil, choose only between externally linked parents."
  (interactive (list (org-brain-entry-at-pt)))
  (let* ((entries (if all (org-brain-parents entry)
                    (org-brain--linked-property-entries
                     entry org-brain-parents-property-name)))
         (parent (cond
                  ((equal 1 (length entries)) (car-safe entries))
                  ((not entries) (error (concat entry " has no parent")))
                  (t (org-brain-choose-entry "Goto parent: " entries nil t)))))
    (org-brain-goto parent)))

;;;###autoload
(defun org-brain-visualize-parent (entry)
  "Visualize a parent of ENTRY, preferring local parents.
This allows the user to quickly jump up the hierarchy."
  (interactive (list (org-brain-entry-at-pt)))
  (if-let ((parent (car (or (org-brain-local-parent entry)
                            (org-brain-parents entry)))))
      (org-brain-visualize parent)
    (error "This entry has no parent")))

;;;###autoload
(defun org-brain-goto-friend (entry)
  "Goto a friend of ENTRY.
If run interactively, get ENTRY from context."
  (interactive (list (org-brain-entry-at-pt)))
  (let* ((entries (org-brain--linked-property-entries
                   entry org-brain-friends-property-name))
         (friend (cond
                  ((equal 1 (length entries)) (car-safe entries))
                  ((not entries) (error (concat entry " has no friends")))
                  (t (org-brain-choose-entry "Goto friend: " entries nil t)))))
    (org-brain-goto friend)))

;;;###autoload
(defun org-brain-refile (max-level)
  "Run `org-refile' to a heading in `org-brain-files', with set MAX-LEVEL.
When in `org-brain-visualize-mode' the current entry will be refiled.
If MAX-LEVEL isn't given, use `org-brain-refile-max-level'.
After refiling, all headlines will be given an id."
  (interactive "p")
  (unless current-prefix-arg
    (setq max-level org-brain-refile-max-level))
  (let ((org-refile-targets `((org-brain-files . (:maxlevel . ,max-level))))
        (org-after-refile-insert-hook org-after-refile-insert-hook))
    (add-hook 'org-after-refile-insert-hook
              (lambda () (org-map-tree 'org-brain-get-id)))
    (if (eq major-mode 'org-brain-visualize-mode)
        (if (org-brain-filep org-brain--vis-entry)
            (user-error "Only headline entries can be refiled")
          (org-with-point-at (org-brain-entry-marker org-brain--vis-entry)
            (org-refile))
          (org-brain--revert-if-visualizing))
      (org-refile))))

(defun org-brain-refile-to (entry parent)
  "Refile ENTRY to be a local child of PARENT, returning the new refiled entry.

If ENTRY is linked to PARENT before the refile, this relationship is removed.
Pins, history, and selected lists are updated
to account for the change in ENTRY's local parent."
  (when (member parent (org-brain-local-descendants entry))
    (error "Cannot refile. New parent %s is a local descendant of %s"
           (org-brain-title parent) (org-brain-title entry)))
  (when (org-brain-filep entry)
    (error "Cannot refile a file entry"))
  (let ((entry-marker (org-brain-entry-marker entry))
        (parent-title (org-brain-title parent)))
    (if (org-brain-filep parent)
        ;; Parent is a file entry
        (let ((parent-path (org-brain-entry-path parent)))
          (with-current-buffer (find-file-noselect parent-path)
            (goto-char (point-max))
            (insert "\n* temp headline")
            (let ((newpoint (point)))
              (org-with-point-at entry-marker
                (org-refile nil nil (list parent-title parent-path "" newpoint))))
            (outline-next-heading)
            (org-promote-subtree)
            (outline-previous-heading)
            (org-cut-subtree)
            (pop kill-ring)
            (forward-line -1)
            (org-brain-delete-current-line "^[[:space:]]*$")))
      ;; Parent is a headline entry
      (let ((id (org-brain-entry-identifier parent)))
        (pcase (org-id-find id)
          (`(,file-name . ,pos)
           (org-with-point-at entry-marker
             (org-refile nil nil (list parent-title file-name "" pos))))
          (_ (error "Parent headline with ID %s not found" id)))))
    (let ((new-entry (org-brain-entry-from-id (org-brain-entry-identifier entry))))
      (cl-flet ((replace-entry (e) (if (equal e entry) new-entry e)))
        (setq org-brain-pins (mapcar #'replace-entry org-brain-pins))
        (setq org-brain--vis-history (mapcar #'replace-entry org-brain--vis-history))
        (setq org-brain-selected (mapcar #'replace-entry org-brain-selected)))
      (when (member parent
                    (org-brain--linked-property-entries new-entry org-brain-parents-property-name))
        (org-brain-remove-relationship parent new-entry))
      (org-save-all-org-buffers)
      (when (eq entry org-brain--vis-entry)
        (setq org-brain--vis-entry new-entry))
      new-entry)))

;;;###autoload
(defun org-brain-change-local-parent (&optional entry parent)
  "Refile ENTRY to be a local child of PARENT.
Entries are relinked so existing parent-child relationships are unaffected.

If ENTRY is not supplied, the entry at point is used.
If PARENT is not supplied, it is prompted for
among the list of ENTRY's linked parents.
Returns the new refiled entry."
  (interactive)
  (unless entry (setq entry (org-brain-entry-at-pt t)))
  (unless parent (let ((linked-parents (org-brain--linked-property-entries entry org-brain-parents-property-name)))
                   (cl-case (length linked-parents)
                     (0 (error "Entry \"%s\" has only one parent" (org-brain-title entry)))
                     (1 (setq parent (car linked-parents)))
                     (otherwise (setq parent (org-brain-choose-entry
                                              (format "Refile \"%s\" to parent: " (org-brain-title entry)) linked-parents))))))
  (let ((old-parent (car (org-brain-local-parent entry)))
        (new-entry (org-brain-refile-to entry parent)))
    (org-brain-add-relationship old-parent new-entry)
    (org-brain--revert-if-visualizing)
    new-entry))

(defun org-brain--remove-relationships (entry &optional recursive)
  "Remove all external relationships from ENTRY.
Also unpin and unselect the entry.

If RECURSIVE is t, remove local children's relationships."
  (dolist (child (org-brain--linked-property-entries
                  entry org-brain-children-property-name))
    (org-brain-remove-relationship entry child))
  (dolist (parent (org-brain--linked-property-entries
                   entry org-brain-parents-property-name))
    (org-brain-remove-relationship parent entry))
  (dolist (friend (org-brain-friends entry))
    (org-brain-remove-friendship entry friend))
  (ignore-errors (org-brain-pin entry -1)
                 (org-brain-select entry -1))
  (when recursive
    (dolist (child (org-brain-local-children entry))
      (org-brain--remove-relationships child t))))

;;;###autoload
(defun org-brain-rename-file (file-entry new-name)
  "Rename FILE-ENTRY to NEW-NAME.
Both arguments should be relative to `org-brain-path' and should
not contain `org-brain-files-extension'."
  (interactive (let ((entry (org-brain-choose-entry
                             "Rename file: " (org-brain-files t) nil t)))
                 (list entry (read-string "New filename: " entry))))
  (let ((newpath (org-brain-entry-path new-name))
        (oldpath (org-brain-entry-path file-entry)))
    (when (file-exists-p newpath)
      (error "There's already a file %s" newpath))
    (when (member newpath (mapcar #'buffer-file-name (buffer-list)))
      (error "There's an active buffer associated with file %s" newpath))
    (let ((children (org-brain--linked-property-entries file-entry org-brain-children-property-name))
          (parents (org-brain--linked-property-entries file-entry org-brain-parents-property-name))
          (friends (org-brain-friends file-entry))
          (is-pinned (member file-entry org-brain-pins))
          (is-selected (member file-entry org-brain-selected)))
      (org-brain--remove-relationships file-entry)
      (org-save-all-org-buffers)
      (make-directory (file-name-directory newpath) t)
      (if (vc-backend oldpath)
          (vc-rename-file oldpath newpath)
        (rename-file oldpath newpath))
      (org-brain-update-id-locations)
      (when is-pinned (org-brain-pin new-name 1))
      (when is-selected (org-brain-select new-name 1))
      (cl-flet ((replace-entry (e) (if (org-brain-filep e)
                                       (if (equal e file-entry) new-name e)
                                     (when (equal (car e) file-entry)
                                       (cons new-name (cdr e)) e))))
        (setq org-brain-pins (mapcar #'replace-entry org-brain-pins))
        (setq org-brain-selected (mapcar #'replace-entry org-brain-selected))
        (setq org-brain--vis-history (mapcar #'replace-entry org-brain--vis-history))
        (setq org-brain--vis-entry (replace-entry org-brain--vis-entry)))
      (dolist (child children)
        (org-brain-add-relationship new-name child))
      (dolist (parent parents)
        (org-brain-add-relationship parent new-name))
      (dolist (friend friends)
        (org-brain--internal-add-friendship new-name friend))
      (when (equal file-entry org-brain--vis-entry)
        (setq org-brain--vis-entry new-name))
      ;; Change edges
      (let ((edge-property (org-brain-edge-prop-name file-entry)))
        (dolist (file (org-brain-files))
          (with-temp-file file
            (insert-file-contents file)
            (goto-char (point-min))
            (replace-regexp (concat edge-property ":")
                            (concat org-brain-edge-property-prefix-name "_" (org-brain-entry-identifier new-name) ":")))))
      (org-brain--revert-if-visualizing)
      (message "Renamed %s to %s" file-entry new-name))))

;;;###autoload
(defun org-brain-delete-entry (entry &optional noconfirm)
  "Delete ENTRY and all of its local children.
If run interactively, ask for the ENTRY.
If NOCONFIRM is nil, ask if we really want to delete."
  (interactive
   (list (org-brain-choose-entry "Delete entry: " 'all nil t)
         nil))
  (let ((local-children (org-brain-local-children entry)))
    (when (or noconfirm
              (yes-or-no-p
               (format "%s and its %d local children will be deleted. Are you sure? "
                       (org-brain-entry-name entry)
                       (length local-children))))
      (ignore-errors (org-brain-select entry -1))
      (dolist (child local-children)
        (org-brain-delete-entry child t))
      (org-brain--remove-relationships entry)
      (if (org-brain-filep entry)
          (let ((filename (org-brain-entry-path entry)))
            (if (vc-backend filename)
                (vc-delete-file filename)
              (delete-file filename delete-by-moving-to-trash)
              (kill-buffer (get-file-buffer filename))))
        (org-with-point-at (org-brain-entry-marker entry)
          (org-mark-subtree)
          (delete-region (region-beginning) (region-end))))))
  (setq org-brain--vis-history (delete entry org-brain--vis-history))
  (org-save-all-org-buffers)
  (if (equal entry org-brain--vis-entry)
      (when-let ((brain-buffer (get-buffer "*org-brain*")))
        (if (ignore-errors (org-brain-visualize-back))
            (message "Deleted visualized entry, going back in history.")
          (kill-buffer brain-buffer)
          (message "Deleted visualized entry. No history, hence killing org-brain buffer.")))
    (org-brain--revert-if-visualizing t)))

;;;###autoload
(defun org-brain-insert-relationships (entry &optional recursive)
  "Insert an `org-mode' list of relationships to ENTRY.
Local children are not included in the list.
If run interactively, get ENTRY from context.

Normally the list is inserted at point, but if RECURSIVE is t
insert at end of ENTRY.  Then recurse in the local (grand)children
of ENTRY and insert there too."
  (interactive (list (org-brain-entry-at-pt t)))
  (cl-flet ((list-to-items
             (list)
             (when list
               `(unordered
                 ,@(mapcar (lambda (x)
                             (list (org-make-link-string
                                    (format "brain:%s" (org-brain-entry-identifier x))
                                    (org-brain-title x))))
                           list)))))
    (save-excursion
      (when recursive
        (org-brain-goto-end entry)
        (newline 2))
      (insert
       ":RELATIONSHIPS:\n"
       (org-list-to-org `(unordered
                          ,(remq nil `("Parents"
                                       ,(list-to-items (org-brain-parents entry))))
                          ,(remq nil `("Children"
                                       ,(list-to-items (org-brain--linked-property-entries
                                                        entry org-brain-children-property-name))))
                          ,(remq nil `("Friends"
                                       ,(list-to-items (org-brain-friends entry))))))
       "\n:END:\n")))
  (when recursive
    (dolist (child (org-brain-local-children entry))
      (org-brain-insert-relationships child t))))

;;;###autoload
(defun org-brain-archive (entry)
  "Use `org-archive-subtree-default' on ENTRY.
If run interactively, get ENTRY from context.
Before archiving, recursively run `org-brain-insert-relationships' on ENTRY.
Remove external relationships from ENTRY, in order to clean up the brain."
  (interactive (list (org-brain-entry-at-pt t)))
  (when (org-brain-filep entry)
    (user-error "Only headline entries can be archived"))
  (org-brain-insert-relationships entry t)
  (org-brain--remove-relationships entry t)
  (org-with-point-at (org-brain-entry-marker entry)
    (org-archive-subtree-default))
  (setq org-brain--vis-history (delete entry org-brain--vis-history))
  (org-save-all-org-buffers)
  (org-brain--revert-if-visualizing))

;;;###autoload
(defun org-brain-pin (entry &optional status)
  "Change if ENTRY is pinned or not.
If run interactively, get ENTRY from context.
Using `\\[universal-argument]' will use `org-brain-button-at-point' as ENTRY.

If STATUS is positive, pin the entry.  If negative, remove the pin.
If STATUS is omitted, toggle between pinned / not pinned."
  (interactive (list (if current-prefix-arg
                         (car (org-brain-button-at-point))
                       (org-brain-entry-at-pt t))))
  (cond ((eq status nil)
         (if (member entry org-brain-pins)
             (org-brain-pin entry -1)
           (org-brain-pin entry 1)))
        ((>= status 1)
         (if (member entry org-brain-pins)
             (error "Entry is already pinned")
           (push entry org-brain-pins)
           (org-brain-save-data)
           (message "Pinned '%s'." (org-brain-entry-name entry))))
        ((< status 1)
         (if (member entry org-brain-pins)
             (progn
               (setq org-brain-pins (delete entry org-brain-pins))
               (org-brain-save-data)
               (message "Unpinned '%s'." (org-brain-entry-name entry)))
           (error "Entry isn't pinned"))))
  (org-brain--revert-if-visualizing))

;;;###autoload
(defun org-brain-select (entry &optional status)
  "Toggle selection of ENTRY.
If run interactively, get ENTRY from context.

If STATUS is positive, select ENTRY.  If negative, unselect it.
If STATUS is omitted, toggle between selected / not selected."
  (interactive (list (org-brain-entry-at-pt)))
  (when (null entry) (error "Cannot select null entry"))
  (cond ((eq status nil)
         (if (member entry org-brain-selected)
             (org-brain-select entry -1)
           (org-brain-select entry 1)))
        ((>= status 1)
         (if (member entry org-brain-selected)
             (error "Entry is already selected")
           (push entry org-brain-selected)
           (org-brain-save-data)
           (message "Entry selected.")))
        ((< status 1)
         (if (member entry org-brain-selected)
             (progn
               (setq org-brain-selected (delete entry org-brain-selected))
               (org-brain-save-data)
               (message "Entry unselected."))
           (error "Entry isn't selected"))))
  (org-brain--revert-if-visualizing))

;;;###autoload
(defun org-brain-clear-selected ()
  "Clear the selected list."
  (interactive)
  (setq org-brain-selected nil)
  (org-brain--revert-if-visualizing))

(defun org-brain-add-selected-children (entry)
  "Add selected entries as children of ENTRY.
If run interactively, get ENTRY from context.

When ENTRY is in the selected list, it is ignored."
  (interactive (list (org-brain-entry-at-pt)))
  ;; org-brain-add-child takes a list of children,
  ;; but we call it one at a time
  ;; so that errors don't interrupt the bulk operation.
  (dolist (child org-brain-selected)
    (ignore-errors (org-brain-add-child entry (list child)))))

(defun org-brain-remove-selected-children (entry)
  "Remove selected entries from the list of ENTRY's children.
If run interactively, get ENTRY from context.

Ignores selected entries that are not children of ENTRY."
  (interactive (list (org-brain-entry-at-pt)))
  (dolist (child org-brain-selected)
    (ignore-errors (org-brain-remove-child entry child))))

(defun org-brain-add-selected-parents (entry)
  "Add selected entries as parents of ENTRY.
If run interactively, get ENTRY from context.

When ENTRY is in the selected list, it is ignored."
  (interactive (list (org-brain-entry-at-pt)))
  ;; org-brain-add-parent takes a list of parents,
  ;; but we call it one at a time
  ;; so that errors don't interrupt the bulk operation.
  (dolist (parent org-brain-selected)
    (ignore-errors (org-brain-add-parent entry (list parent)))))

(defun org-brain-remove-selected-parents (entry)
  "Remove selected entries from the list of ENTRY's parents.
If run interactively, get ENTRY from context.

Ignores selected entries that are not parents of ENTRY."
  (interactive (list (org-brain-entry-at-pt)))
  (dolist (parent org-brain-selected)
    (ignore-errors (org-brain-remove-parent entry parent))))

(defun org-brain-add-selected-friendships (entry)
  "Add selected entries as friends of ENTRY.
If run interactively, get ENTRY from context.

When ENTRY is in the selected list, it is ignored."
  (interactive (list (org-brain-entry-at-pt)))
  ;; org-brain-add-friendship takes a list of friends,
  ;; but we call it one at a time
  ;; so that errors don't interrupt the bulk operation.
  (dolist (friend org-brain-selected)
    (ignore-errors (org-brain-add-friendship entry (list friend)))))

(defun org-brain-remove-selected-friendships (entry)
  "Remove selected entries from the list of ENTRY's friends.
If run interactively, get ENTRY from context.

Ignores selected entries that are not friends of ENTRY."
  (interactive (list (org-brain-entry-at-pt)))
  (dolist (selected org-brain-selected)
    (ignore-errors (org-brain-remove-friendship entry selected))))

(defun org-brain-delete-selected-entries ()
  "Delete all of the selected entries."
  (interactive)
  (dolist (selected org-brain-selected)
    (org-brain-delete-entry selected)))

(defun org-brain-change-selected-local-parents ()
  "Change the local parent of all the selected entries."
  (interactive)
  (dolist (selected org-brain-selected)
    (org-brain-change-local-parent selected)))

;;;###autoload
(defun org-brain-set-title (entry title)
  "Set the name of ENTRY to TITLE.
If run interactively, get ENTRY from context and prompt for TITLE."
  (interactive
   (let* ((entry-at-pt (org-brain-entry-at-pt t))
          (new-title (org-brain-title entry-at-pt)))
     (when (equal (length new-title) 0)
       (error "Title must be at least 1 character"))
     (list entry-at-pt (read-string "Title: " new-title))))
  (if (org-brain-filep entry)
      ;; File entry
      (org-with-point-at (org-brain-entry-marker entry)
        (goto-char (point-min))
        (when (assoc "TITLE" (org-brain-keywords entry))
          (re-search-forward "^#\\+TITLE:")
          (org-brain-delete-current-line))
        (insert (format "#+TITLE: %s\n" title))
        (save-buffer))
    ;; Headline entry
    (org-with-point-at (org-brain-entry-marker entry)
      (org-edit-headline title)
      (save-buffer)
      (setf (nth 1 org-brain--vis-entry) title)))
  (org-brain--revert-if-visualizing))

;;;###autoload
(defun org-brain-set-tags (entry)
  "Modify the ENTRY tags.
Use `org-set-tags-command' on headline ENTRY.
Instead sets #+FILETAGS on file ENTRY.
If run interactively, get ENTRY from context."
  (interactive (list (org-brain-entry-at-pt t)))
  (if (org-brain-filep entry)
      (org-with-point-at (org-brain-entry-marker entry)
        (let ((tag-str (read-string "FILETAGS: "
                                    (mapconcat #'identity org-file-tags ":"))))
          (goto-char (point-min))
          (when (assoc "FILETAGS" (org-brain-keywords entry))
            (re-search-forward "^#\\+FILETAGS:")
            (org-brain-delete-current-line))
          (insert (format "#+FILETAGS: %s\n" tag-str)))
        ;; From org.el
        (let ((org-inhibit-startup-visibility-stuff t)
              (org-startup-align-all-tables nil))
          (when (boundp 'org-table-coordinate-overlays)
            (mapc #'delete-overlay org-table-coordinate-overlays)
            (setq org-table-coordinate-overlays nil))
          (org-save-outline-visibility 'use-markers (org-mode-restart)))
        (save-buffer))
    (org-with-point-at (org-brain-entry-marker entry)
      (org-set-tags-command)
      (save-buffer)))
  (org-brain--revert-if-visualizing))

;;;###autoload
(defun org-brain-add-nickname (entry nickname)
  "ENTRY gets a new NICKNAME.
If run interactively use `org-brain-entry-at-pt' and prompt for NICKNAME."
  (interactive (list (org-brain-entry-at-pt)
                     (read-string "Nickname: ")))
  (if (org-brain-filep entry)
      (let ((nickname (org-entry-protect-space nickname)))
        (org-with-point-at (org-brain-entry-marker entry)
          (goto-char (point-min))
          (if (re-search-forward "^#\\+NICKNAMES:.*$" nil t)
              (insert (concat " " nickname))
            (insert (format "#+NICKNAMES: %s\n" nickname)))
          (save-buffer)))
    (org-entry-add-to-multivalued-property
     (org-brain-entry-marker entry) "NICKNAMES" nickname)
    (org-save-all-org-buffers)))

;;;###autoload
(defun org-brain-headline-to-file (entry)
  "Convert headline ENTRY to a file entry.
Prompt for name of the new file.
If interactive, also prompt for ENTRY."
  (interactive (list (org-brain-choose-entry "Convert entry: "
                                             (org-brain-headline-entries)
                                             nil t)))
  (let* (level
         (title (org-brain-title entry))
         (new-entry (read-string "New file entry: " title))
         (path (org-brain-entry-path new-entry)))
    (when (file-exists-p path)
      (error "That file already exists"))
    (let ((parents (org-brain-parents entry))
          (external-parents (org-brain--linked-property-entries entry org-brain-parents-property-name))
          (children (org-brain--linked-property-entries entry org-brain-children-property-name))
          (friends (org-brain-friends entry))
          (hl-text (org-with-point-at (org-brain-entry-marker entry)
                     (setq level (org-outline-level))
                     (org-get-entry))))
      (dolist (parent external-parents)
        (org-brain-remove-relationship parent entry))
      (dolist (child children)
        (org-brain-remove-relationship entry child))
      (dolist (friend friends)
        (org-brain-remove-friendship entry friend))
      (org-with-point-at (org-brain-entry-marker entry)
        (org-cut-subtree)
        (pop kill-ring)
        (save-buffer))
      (make-directory (file-name-directory path) t)
      (with-temp-file path
        (insert (format "#+TITLE:%s\n\n%s" title hl-text))
        (delay-mode-hooks
          (org-mode)
          (goto-char (point-min))
          (re-search-forward org-property-drawer-re)
          (replace-match "")
          (goto-char (point-max))
          (let ((level-regex "^"))
            (dotimes (_i (1+ level))
              (setq level-regex (concat level-regex "\\*")))
            (setq level-regex (concat level-regex " "))
            (while (re-search-backward level-regex nil t)
              (dotimes (_i level) (org-promote-subtree))))))
      (dolist (parent parents)
        (org-brain-add-relationship parent new-entry))
      (dolist (child children)
        (org-brain-add-relationship new-entry child))
      (dolist (friend friends)
        (org-brain--internal-add-friendship new-entry friend))
      (when (equal entry org-brain--vis-entry)
        (setq org-brain--vis-entry new-entry))
      (when (member entry org-brain-pins)
        (org-brain-pin entry -1)
        (org-brain-pin new-entry 1)))))

;;;###autoload
(defun org-brain-ensure-ids-in-buffer ()
  "Run `org-brain-get-id' on all headlines in current buffer
taking into account the ignore tags such as :childess:
Only works if in an `org-mode' buffer inside `org-brain-path'.
Suitable for use with `before-save-hook'."
  (interactive)
  (and (eq major-mode 'org-mode)
       (string-prefix-p (file-truename org-brain-path)
                        (file-truename (buffer-file-name)))
       (let ((match (format "-%s-%s|-%s+TAGS={%s}"  ; "-nobrain-childless|-nobrain+TAGS={childless}"
                            org-brain-exclude-tree-tag org-brain-exclude-children-tag
                            org-brain-exclude-tree-tag org-brain-exclude-children-tag)))
         (org-map-entries #'org-brain-get-id match 'file))))

;;;###autoload
(defun org-brain-agenda ()
  "Like `org-agenda', but only for `org-brain-files'."
  (interactive)
  (let ((org-agenda-files (org-brain-files)))
    (org-agenda)))

;;;###autoload
(defun org-brain-create-relationships-from-links ()
  "Add relationships for brain: links in `org-brain-path'.
Only create relationships to other files, not to headline entries.

This function is meant to be used in order to convert old
org-brain setups to the system introduced in version 0.4. Please
make a backup of your `org-brain-path' before running this
function."
  (interactive)
  (when (y-or-n-p "This function is meant for old configurations.  Are you sure you want to scan for links? ")
    (dolist (file (org-brain-files))
      (with-temp-buffer
        (insert-file-contents file)
        (org-element-map (org-element-parse-buffer) 'link
          (lambda (link)
            (when (string-equal (org-element-property :type link) "brain")
              (org-brain-add-relationship
               (org-brain-path-entry-name file)
               (car (split-string (org-element-property :path link) "::"))))))))))

;;;; Sorting

(defun org-brain-title< (entry1 entry2)
  "Return non-nil if title of ENTRY1 is less than ENTRY2 in lexicographic order.
Case is significant."
  (string< (org-brain-title entry1) (org-brain-title entry2)))

(defvar org-brain-visualize-sort-function 'org-brain-title<
  "How to sort lists of relationships when visualizing.
Should be a function which accepts two entries as arguments.
The function returns t if the first entry is smaller than the second.

If you don't want to sort the relationships, set this to `ignore'.")

;;;; Visualize

(defvar org-brain--visualize-follow nil "Used by `org-brain-visualize-follow'.")

;;;###autoload
(defun org-brain-visualize-follow (should-follow)
  "Set if `org-brain-visualize' SHOULD-FOLLOW the current entry or not.
When following, the visualized entry will be shown in a separate
buffer when changing the visualized entry.
If run interactively, toggle following on/off."
  (interactive (list (not org-brain--visualize-follow)))
  (setq org-brain--visualize-follow should-follow)
  (message (if should-follow
               "Enabled following visualized entry."
             "Disabled following visualized entry.")))

(defvar-local org-brain--visualize-header-end-pos 0
  "Buffer position at end of headers (history etc) in `org-brain-visualize'.")

;;;###autoload
(defun org-brain-visualize (entry &optional nofocus nohistory wander)
  "View a concept map with ENTRY at the center.

When run interactively, prompt for ENTRY and suggest
`org-brain-entry-at-pt'.  By default, the choices presented is
determined by `org-brain-visualize-default-choices': 'all will
show all entries, 'files will only show file entries and 'root
will only show files in the root of `org-brain-path'.

You can override `org-brain-visualize-default-choices':
  `\\[universal-argument]' will use 'all.
  `\\[universal-argument] \\[universal-argument]' will use 'files.
  `\\[universal-argument] \\[universal-argument] \\[universal-argument]' will use 'root.

Unless NOFOCUS is non-nil, the `org-brain-visualize' buffer will gain focus.
Unless NOHISTORY is non-nil, add the entry to `org-brain--vis-history'.
Setting NOFOCUS to t implies also having NOHISTORY as t.
Unless WANDER is t, `org-brain-stop-wandering' will be run."
  (interactive
   (progn
     (org-brain-maybe-switch-brain)
     (let ((choices (cond ((equal current-prefix-arg '(4)) 'all)
                          ((equal current-prefix-arg '(16)) 'files)
                          ((equal current-prefix-arg '(64)) 'root)
                          (t org-brain-visualize-default-choices)))
           (def-choice (unless (eq major-mode 'org-brain-visualize-mode)
                         (ignore-errors (org-brain-entry-name (org-brain-entry-at-pt))))))
       (org-brain-stop-wandering)
       (list
        (org-brain-choose-entry
         "Entry: "
         (cond ((equal choices 'all)
                'all)
               ((equal choices 'files)
                (org-brain-files t))
               ((equal choices 'root)
                (make-directory org-brain-path t)
                (mapcar #'org-brain-path-entry-name
                        (directory-files org-brain-path t (format "\\.%s$" org-brain-files-extension)))))
         nil nil def-choice)))))
  (unless wander (org-brain-stop-wandering))
  (with-current-buffer (get-buffer-create "*org-brain*")
    (setq-local indent-tabs-mode nil)
    (read-only-mode 1)
    (setq-local default-directory (file-name-directory (org-brain-entry-path entry)))
    (setq list-buffers-directory (org-brain-vis-title entry))
    (org-brain-maybe-switch-brain)
    (unless (eq org-brain--vis-entry entry)
      (setq org-brain--vis-entry entry)
      (setq org-brain-mind-map-parent-level (default-value 'org-brain-mind-map-parent-level))
      (setq org-brain-mind-map-child-level (default-value 'org-brain-mind-map-child-level)))
    (setq org-brain--vis-entry-keywords (when (org-brain-filep entry)
                                          (org-brain-keywords entry)))
    (let ((inhibit-read-only t)
          (entry-pos))
      (delete-region (point-min) (point-max))
      (org-brain--vis-pinned)
      (org-brain--vis-selected)
      (when (not nohistory)
        (setq org-brain--vis-history
              (seq-filter (lambda (elt) (not (equal elt entry))) org-brain--vis-history))
        (setq org-brain--vis-history (seq-take org-brain--vis-history 15))
        (push entry org-brain--vis-history))
      (when org-brain-show-history (org-brain--vis-history))
      (if org-brain-visualizing-mind-map
          (setq entry-pos (org-brain-mind-map org-brain--vis-entry org-brain-mind-map-parent-level org-brain-mind-map-child-level))
        (setq-local org-brain--visualize-header-end-pos (point))
        (insert "\n\n")
        (org-brain--vis-parents-siblings entry)
        ;; Insert entry title
        (let ((title (org-brain-vis-title entry)))
          (let ((half-title-length (/ (string-width title) 2)))
            (if (>= half-title-length (current-column))
                (delete-char (- (current-column)))
              (ignore-errors (delete-char (- half-title-length)))))
          (setq entry-pos (point))
          (insert (propertize title
                              'face (org-brain-display-face entry 'org-brain-title)
                              'aa2u-text t))
          (org-brain--vis-friends entry)
          (org-brain--vis-children entry)))
      (when (and org-brain-show-resources)
        (org-brain--vis-resources (org-brain-resources entry)))
      (if org-brain-show-text
          (org-brain--vis-text entry)
        (run-hooks 'org-brain-after-visualize-hook))
      (unless (eq major-mode 'org-brain-visualize-mode)
        (org-brain-visualize-mode))
      (goto-char entry-pos)
      (set-buffer-modified-p nil))
    (unless nofocus
      (when org-brain--visualize-follow
        (org-brain-goto-current)
        (run-hooks 'org-brain-visualize-follow-hook))
      (if (or org-brain--visualize-follow org-brain-open-same-window)
          (pop-to-buffer "*org-brain*")
        (pop-to-buffer-same-window "*org-brain*")))))

;;;###autoload
(defun org-brain-visualize-dwim ()
  "Switch to the *org-brain* buffer.
If there's no such buffer, or if already there, run `org-brain-visualize'."
  (interactive)
  (if (and (not (org-brain-maybe-switch-brain))
           (not (eq major-mode 'org-brain-visualize-mode))
           (get-buffer "*org-brain*"))
      (if org-brain-open-same-window
          (pop-to-buffer "*org-brain*")
        (pop-to-buffer-same-window "*org-brain*"))
    (call-interactively #'org-brain-visualize)))

;;;###autoload
(defun org-brain-visualize-entry-at-pt ()
  "Use `org-brain-visualize' on the `org-brain-entry-at-pt'.
Useful if wanting to visualize the current `org-mode' entry."
  (interactive)
  (org-brain-visualize (org-brain-entry-at-pt)))

;;;###autoload
(defun org-brain-visualize-random (&optional restrict-to)
  "Run `org-brain-visualize' on a random org-brain entry.
If RESTRICT-TO is given, then only choose among those entries.

If called interactively with `\\[universal-argument]' then
restrict to descendants of the visualized entry."
  (interactive (when (equal current-prefix-arg '(4))
                 (list (org-brain-descendants org-brain--vis-entry))))
  (let ((entries (or restrict-to
                     (append (org-brain-files t)
                             (org-brain-headline-entries)))))
    (org-brain-visualize (nth (random (length entries)) entries) nil nil t)))

(defvar org-brain-wander-timer nil
  "A timer running `org-brain-visualize-random' at a set interval.

Can be (de)activated by `org-brain-visualize-wander'.")

(defun org-brain-stop-wandering ()
  "Cancels `org-brain-wander-timer', if it is active."
  (when (member org-brain-wander-timer timer-list)
    (cancel-timer org-brain-wander-timer)
    t))

(defun org-brain-visualize-wander (&optional restrict-to)
  "Run `org-brain-visualize-random' every `org-brain-wander-interval'.
If RESTRICT-TO is given, then only wander among those entries.

If called interactively with `\\[universal-argument]' then
restrict to descendants of the visualized entry starting the wandering session.

Wandering is cancelled by many org-brain commands, but can also be
cancelled manually with `org-brain-stop-wandering'."
  (interactive (when (equal current-prefix-arg '(4))
                 (list (org-brain-descendants org-brain--vis-entry))))
  (if (org-brain-stop-wandering)
      (message "Wandering stopped.")
    (setq org-brain-wander-timer (run-at-time nil org-brain-wander-interval #'org-brain-visualize-random restrict-to))
    (message "Wandering started.")))

(defun org-brain-visualize-quit ()
  "Like `quit-window', but also stops `org-brain-visualize-wander'."
  (interactive)
  (org-brain-stop-wandering)
  (quit-window))

(defun org-brain-entry-icon (entry)
  "Get a string representing the icon of ENTRY.
Checks for the org mode category of ENTRY, then search for the
category icon in `org-agenda-category-icon-alist'."
  (when (and org-brain-show-icons
             org-agenda-category-icon-alist)
    (org-with-point-at (org-brain-entry-marker entry)
      (when-let* ((category (org-get-category))
                  (icon (org-agenda-get-category-icon category)))
        (propertize (make-string org-brain-category-icon-width ? ) 'display icon)))))

(defun org-brain-vis-title (entry)
  "The title of ENTRY when shown in `org-brain-visualize-mode'."
  (string-join (remove
                ""
                (list
                 ;; Prepend stuff to the title
                 (mapconcat (lambda (func) (funcall func entry))
                            org-brain-vis-title-prepend-functions
                            " ")
                 (if (eq org-brain--vis-entry entry)
                     (mapconcat (lambda (func) (funcall func entry))
                                org-brain-vis-current-title-prepend-functions
                                " ")
                   "")
                 ;; The title itself
                 (org-brain-title entry (or (not org-brain-visualizing-mind-map)
                                            org-brain-cap-mind-map-titles))
                 ;; Append stuff to the title
                 (mapconcat (lambda (func) (funcall func entry))
                            org-brain-vis-title-append-functions
                            " ")
                 (if (eq org-brain--vis-entry entry)
                     (mapconcat (lambda (func) (funcall func entry))
                                org-brain-vis-current-title-append-functions
                                " ")
                   "")))
               " "))

(defun org-brain-insert-visualize-button (entry &optional face category)
  "Insert a button, running `org-brain-visualize' on ENTRY when clicked.
FACE is sent to `org-brain-display-face' and sets the face of the button.
CATEGORY is used to set the `brain-category` text property."
  (let ((annotation (org-brain-get-edge-annotation org-brain--vis-entry
                                                   entry
                                                   org-brain--vis-entry-keywords)))
    (insert-text-button
     (org-brain-vis-title entry)
     'action (lambda (_x) (org-brain-visualize entry))
     'id (org-brain-entry-identifier entry)
     'follow-link t
     'brain-category (or category 'default)
     'help-echo annotation
     'aa2u-text t
     'face (org-brain-display-face entry face annotation))))

(defun org-brain-jump-to-visualize-button (entry)
  "If ENTRY has a visualize button in the current buffer, jump to its position."
  (when (eq major-mode 'org-brain-visualize-mode)
    (let ((start-pos (point))
          (entry-id (org-brain-entry-identifier entry)))
      (goto-char org-brain--visualize-header-end-pos)
      (while (and (or (ignore-errors (forward-button 1))
                      (and (goto-char start-pos) nil))
                  (not (equal (button-get (button-at (point)) 'id)
                              entry-id)))))))

(defun org-brain-insert-resource-button (resource &optional indent)
  "Insert a new line with a RESOURCE button, indented by INDENT spaces."
  (insert (make-string (or indent 0) ?\ ) "\n- ")
  (run-hook-with-args 'org-brain-after-resource-button-functions (car resource))
  (insert-text-button
   (or (cdr resource) (car resource))
   'action (lambda (_x)
             (org-open-link-from-string (format "[[%s]]" (car resource))))
   'follow-link t
   'aa2u-text t))

(defun org-brain-button-at-point ()
  "If there's an entry link button at `point' return (entry . button)."
  (if-let* ((button (button-at (point)))
            (id (button-get button 'id))
            (entry (or (org-brain-entry-from-id id)
                       (org-entry-restore-space id))))
      (cons entry button)
    (user-error "No entry button at point")))

(defun org-brain-add-resource (&optional link description prompt entry)
  "Insert LINK with DESCRIPTION in ENTRY.
If ENTRY is nil, try to get it from context or prompt for it.
If LINK is nil then use `org-insert-link-global'. Otherwise:
If PROMPT is non nil, let user edit the resource even if run non-interactively."
  (interactive)
  (unless entry
    (setq entry (or (ignore-errors (org-brain-entry-at-pt))
                    (org-brain-choose-entry "Insert link in entry: " 'all))))
  (let ((link-text
         (if link
             (progn
               (when prompt
                 (setq link (read-string "Insert link: " link))
                 (when (string-match org-bracket-link-regexp link)
                   (let ((linkdesc (match-string 3 link)))
                     (when (and (not description) linkdesc)
                       (setq description linkdesc))
                     (setq link (match-string 1 link))))
                 (setq description (read-string "Link description: " description)))
               (concat "- " (org-make-link-string link description)))
           (let ((bfn (buffer-file-name)))
             (when-let ((l (with-temp-buffer
                             (let ((buffer-file-name bfn))
                               (org-insert-link-global)
                               (buffer-string)))))
               (concat "- " l))))))
    (if (org-brain-filep entry)
        ;; File entry
        (org-with-point-at (org-brain-entry-marker entry)
          (goto-char (org-brain-first-headline-position))
          (if (re-search-backward org-brain-resources-start-re nil t)
              (end-of-line)
            (if (re-search-backward org-brain-keyword-regex nil t)
                (progn
                  (end-of-line)
                  (newline-and-indent))
              (goto-char (point-min)))
            (insert (concat ":" org-brain-resources-drawer-name ":\n:END:\n"))
            (re-search-backward org-brain-resources-start-re nil t)
            (end-of-line))
          (newline-and-indent)
          (insert link-text)
          (save-buffer))
      ;; Headline entry
      (org-with-point-at (org-brain-entry-marker entry)
        (goto-char (cdr (org-get-property-block)))
        (forward-line 1)
        (if (looking-at org-brain-resources-start-re)
            (end-of-line)
          (open-line 1)
          (indent-for-tab-command)
          (insert (concat ":" org-brain-resources-drawer-name ":"))
          (save-excursion
            (insert "\n")
            (indent-for-tab-command)
            (insert ":END:")))
        (newline-and-indent)
        (insert link-text)
        (save-buffer))))
  (org-brain--revert-if-visualizing))

(defalias 'org-brain-visualize-add-resource #'org-brain-add-resource)

(defun org-brain-add-file-line-as-resource (file line &optional entry)
  "Add a link to a FILE LINE as a resource in ENTRY.
If called interactively use current FILE and LINE
and prompt for ENTRY, unless called with `\\[universal-argument]'
in which case use the current/last visualized entry."
  (interactive (list (buffer-file-name)
                     (number-to-string (line-number-at-pos))))
  (org-brain-add-resource (concat "file:" file "::" line)
                          nil nil
                          (or entry (when current-prefix-arg
                                      org-brain--vis-entry)))
  (ignore-errors
    (with-current-buffer "*org-brain*"
      (org-brain--revert-if-visualizing)))
  (message "A new resource has been added."))

(defun org-brain-add-file-as-resource (file &optional entry)
  "Add a link to a FILE as a resource in ENTRY.
If called interactively use current FILE
and prompt for ENTRY, unless called with `\\[universal-argument]'
in which case use the current/last visualized entry."
  (interactive (list (buffer-file-name)))
  (org-brain-add-resource (concat "file:" file)
                          nil nil
                          (or entry (when current-prefix-arg
                                      org-brain--vis-entry)))
  (ignore-errors
    (with-current-buffer "*org-brain*"
      (org-brain--revert-if-visualizing)))
  (message "A new resource has been added."))

(defun org-brain-visualize-attach ()
  "Use `org-attach' on `org-brain--vis-entry'."
  (interactive)
  (unless (eq major-mode 'org-brain-visualize-mode)
    (error "Not in org-brain-visualize-mode"))
  (when (org-brain-filep org-brain--vis-entry)
    (error "Can only attach to headline entries"))
  (org-with-point-at (org-brain-entry-marker org-brain--vis-entry)
    (goto-char (cdr (org-id-find (nth 2 org-brain--vis-entry))))
    (call-interactively #'org-attach)
    (save-buffer))
  (org-brain--revert-if-visualizing))

(defun org-brain-paste-resource ()
  "Add `current-kill' as a resource link.
See `org-brain-add-resource'."
  (interactive)
  (org-brain-add-resource (current-kill 0) nil t))

(defalias 'org-brain-visualize-paste-resource #'org-brain-paste-resource)

;;;###autoload
(defun org-brain-select-button ()
  "Toggle selection of the entry linked to by the button at point."
  (interactive)
  (org-brain-select (car (org-brain-button-at-point)))
  t)

;;;###autoload
(defun org-brain-select-dwim (arg)
  "Use `org-brain-select-button' or `org-brain-select' depending on context.
If run with `\\[universal-argument\\]' (ARG is non nil)
then always use `org-brain-select'."
  (interactive "P")
  (when (or arg (not (ignore-errors (org-brain-select-button))))
    (org-brain-select (org-brain-entry-at-pt))))

(defun org-brain-edge-prop-name (entry)
  "Retrun edge annotation property name of ENTRY."
  (concat org-brain-edge-property-prefix-name "_" (org-brain-entry-identifier entry)))

(defun org-brain-get-edge-annotation (from to &optional keywords)
  "Get edge annotation FROM an entry TO another entry.
If KEYWORDS is given, use it instead of `org-brain-keywords' (optimization)."
  (if (org-brain-filep from)
      (cdr (assoc (upcase (org-brain-edge-prop-name to))
                  (or keywords (org-brain-keywords from))))
    (org-entry-get (org-brain-entry-marker from) (org-brain-edge-prop-name to))))

(defun org-brain-annotate-edge (entry target annotation two-way)
  "When visualizing ENTRY, links to TARGET will have an ANNOTATION.
You can think of it as edges with comments in a graph.
If TWO-WAY is non-nil, then also add the ANNOTATION from TARGET to ENTRY.

When called interactively use the visualized ENTRY,
`org-brain-button-at-point' as TARGET, and prompt for ANNOTATION.
TWO-WAY will be t unless called with `\\[universal-argument\\]'."
  (interactive
   (let ((target (car (org-brain-button-at-point))))
     (list org-brain--vis-entry
           target
           (read-string (concat (org-brain-title target) " edge: "))
           (not current-prefix-arg))))
  (if (org-brain-filep entry)
      ;; File entry
      (let ((edge-regex (format "^#\\+%s:"
                                (org-brain-edge-prop-name target))))
        (org-with-point-at (org-brain-entry-marker entry)
          (if (re-search-forward edge-regex nil t)
              (org-brain-delete-current-line edge-regex)
            (goto-char (point-min)))
          (when (> (length annotation) 0)
            (insert "#+" (org-brain-edge-prop-name target) ": " annotation "\n"))
          (save-buffer)))
    ;; Headline entry
    (org-with-point-at (org-brain-entry-marker entry)
      (if (> (length annotation) 0)
          (org-set-property (org-brain-edge-prop-name target) annotation)
        (org-delete-property (org-brain-edge-prop-name target)))
      (save-buffer)))
  (when two-way
    (run-with-idle-timer 0.2 nil 'org-brain-annotate-edge
                         target entry annotation nil))
  (org-brain--revert-if-visualizing))

(defun org-brain-visualize-back ()
  "Go back to the previously visualized entry."
  (interactive)
  (if (cadr org-brain--vis-history)
      (progn (pop org-brain--vis-history)
             (org-brain-visualize (car org-brain--vis-history) nil t))
    (error "No further history")))

(defun org-brain-visualize-revert (_ignore-auto _noconfirm)
  "Revert function for `org-brain-visualize-mode'."
  (org-brain-visualize org-brain--vis-entry t))

(defun org-brain--revert-if-visualizing (&optional ignore-button-at-pt)
  "Revert buffer if in `org-brain-visualize-mode'.
Unless IGNORE-BUTTON-AT-PT is non nil, jump to the button at
point before the buffer was reverted."
  (when (eq major-mode 'org-brain-visualize-mode)
    (let ((button-entry
           (unless ignore-button-at-pt
             (car (ignore-errors (org-brain-button-at-point))))))
      (org-brain-stop-wandering)
      (revert-buffer)
      (when button-entry (org-brain-jump-to-visualize-button button-entry)))))

(defun org-brain--bookmark-handler (bookmark)
  "Visualize the entry stored in BOOKMARK."
  (org-brain-visualize (cdr (assoc 'brain-entry bookmark)) nil)
  (switch-to-buffer "*org-brain*"))

(defun org-brain-make-bookmark-record ()
  "Make a bookmark out of `org-brain--vis-entry'.
Used as `bookmark-make-record-function' in `org-brain-visualize-mode'."
  (if-let ((entry org-brain--vis-entry))
      (cons (org-brain-title org-brain--vis-entry)
            `((handler . org-brain--bookmark-handler)
              (brain-entry . ,org-brain--vis-entry)))
    (user-error "For some reason `org-brain--vis-entry' is nil")))

(define-derived-mode org-brain-visualize-mode
  special-mode  "Org-brain Visualize"
  "Major mode for `org-brain-visualize'.
\\{org-brain-visualize-mode-map}"
  (setq-local revert-buffer-function #'org-brain-visualize-revert)
  (setq-local bookmark-make-record-function #'org-brain-make-bookmark-record))

;;;;; Keybindings

(define-key org-brain-visualize-mode-map "p" 'org-brain-add-parent)
(define-key org-brain-visualize-mode-map "P" 'org-brain-remove-parent)
(define-key org-brain-visualize-mode-map "c" 'org-brain-add-child)
(define-key org-brain-visualize-mode-map "C" 'org-brain-remove-child)
(define-key org-brain-visualize-mode-map "*" 'org-brain-add-child-headline)
(define-key org-brain-visualize-mode-map "h" 'org-brain-add-child-headline)
(define-key org-brain-visualize-mode-map "n" 'org-brain-pin)
(define-key org-brain-visualize-mode-map "N" 'org-brain-add-nickname)
(define-key org-brain-visualize-mode-map "t" 'org-brain-set-title)
(define-key org-brain-visualize-mode-map "j" 'forward-button)
(define-key org-brain-visualize-mode-map "k" 'backward-button)
(define-key org-brain-visualize-mode-map "u" 'org-brain-visualize-parent)
(define-key org-brain-visualize-mode-map [?\t] 'forward-button)
(define-key org-brain-visualize-mode-map [backtab] 'backward-button)
(define-key org-brain-visualize-mode-map "o" 'org-brain-goto-current)
(define-key org-brain-visualize-mode-map "O" 'org-brain-goto)
(define-key org-brain-visualize-mode-map "v" 'org-brain-visualize)
(define-key org-brain-visualize-mode-map "V" 'org-brain-visualize-follow)
(define-key org-brain-visualize-mode-map "f" 'org-brain-add-friendship)
(define-key org-brain-visualize-mode-map "F" 'org-brain-remove-friendship)
(define-key org-brain-visualize-mode-map "d" 'org-brain-delete-entry)
(define-key org-brain-visualize-mode-map "l" 'org-brain-add-resource)
(define-key org-brain-visualize-mode-map "r" 'org-brain-open-resource)
(define-key org-brain-visualize-mode-map "a" 'org-brain-visualize-attach)
(define-key org-brain-visualize-mode-map "A" 'org-brain-archive)
(define-key org-brain-visualize-mode-map "b" 'org-brain-visualize-back)
(define-key org-brain-visualize-mode-map "\C-y" 'org-brain-visualize-paste-resource)
(define-key org-brain-visualize-mode-map "T" 'org-brain-set-tags)
(define-key org-brain-visualize-mode-map "q" 'org-brain-visualize-quit)
(define-key org-brain-visualize-mode-map "w" 'org-brain-visualize-random)
(define-key org-brain-visualize-mode-map "W" 'org-brain-visualize-wander)
(define-key org-brain-visualize-mode-map "m" 'org-brain-visualize-mind-map)
(define-key org-brain-visualize-mode-map "+" 'org-brain-show-descendant-level)
(define-key org-brain-visualize-mode-map "-" 'org-brain-hide-descendant-level)
(define-key org-brain-visualize-mode-map "z" 'org-brain-show-ancestor-level)
(define-key org-brain-visualize-mode-map "Z" 'org-brain-hide-ancestor-level)
(define-key org-brain-visualize-mode-map "e" 'org-brain-annotate-edge)
(define-key org-brain-visualize-mode-map "\C-c\C-w" 'org-brain-refile)
(define-key org-brain-visualize-mode-map "\C-c\C-x\C-v" 'org-toggle-inline-images)

(define-prefix-command 'org-brain-select-map)
(define-key org-brain-select-map "s" 'org-brain-clear-selected)
(define-key org-brain-select-map "c" 'org-brain-add-selected-children)
(define-key org-brain-select-map "C" 'org-brain-remove-selected-children)
(define-key org-brain-select-map "p" 'org-brain-add-selected-parents)
(define-key org-brain-select-map "P" 'org-brain-remove-selected-parents)
(define-key org-brain-select-map "f" 'org-brain-add-selected-friendships)
(define-key org-brain-select-map "F" 'org-brain-remove-selected-friendships)
(define-key org-brain-select-map "s" 'org-brain-clear-selected)
(define-key org-brain-select-map "S" 'org-brain-clear-selected)
(define-key org-brain-select-map "d" 'org-brain-delete-selected-entries)
(define-key org-brain-select-map "l" 'org-brain-change-selected-local-parents)

(define-key org-brain-visualize-mode-map "s" 'org-brain-select-dwim)
(define-key org-brain-visualize-mode-map "S" 'org-brain-select-map)

(define-prefix-command 'org-brain-move-map)
(define-key org-brain-move-map "r" 'org-brain-refile)
(define-key org-brain-move-map "p" 'org-brain-change-local-parent)

(define-key org-brain-visualize-mode-map "M" 'org-brain-move-map)

(let ((map (define-prefix-command 'org-brain-prefix-map)))
  (set-keymap-parent map org-brain-visualize-mode-map)
  (mapc (lambda (x) (define-key map x nil))
        '("j" "k" "g" [?\t] [backtab] "o" "b" "u" "V" "T" "q"
          "m" "+" "-" "z" "Z" "e" "?" "\C-c\C-w" "\C-c\C-x\C-v"
          "" " " "<" ">" "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"
          [33554464])))

;;;;; Drawing helpers

(defun org-brain--visually-sort (lst)
  "Sort LST destructively according to org-brain-visualize-sort-function."
  (sort lst org-brain-visualize-sort-function))

(defun org-brain--visually-sorted (lst)
  "Sorted LST according to org-brain-visualize-sort-function."
  (org-brain--visually-sort (copy-sequence lst)))

(defun org-brain--maybe-visually-sort (entry lst)
  "Sorted LST unless ENTRY has a :nosort: tag."
  (if (member org-brain-no-sort-children-tag (org-brain-get-tags entry))
      lst
    (org-brain--visually-sort lst)))

(defun org-brain--visually-sorted-parents (entry)
  "List of parents, sorted unless ENTRY has a :nosort: tag."
  (org-brain--maybe-visually-sort entry (org-brain-parents entry)))

(defun org-brain--visually-sorted-children (entry)
  "List of children, sorted unless ENTRY has a :nosort: tag."
  (org-brain--maybe-visually-sort entry (org-brain-children entry)))

(defun org-brain--visually-sorted-friends (entry)
  "List of friends, sorted unless ENTRY has a :nosort: tag."
  (org-brain--maybe-visually-sort entry (org-brain-friends entry)))

(defun org-brain--visually-sorted-siblings (entry)
  "List of siblings, sorted unless ENTRY has a :nosort: tag."
  (let ((siblings (org-brain-siblings entry)))
    (if (member org-brain-no-sort-children-tag (org-brain-get-tags entry))
	siblings
      (sort siblings (lambda (x y)
		       (funcall org-brain-visualize-sort-function
				(car x) (car y)))))))

(defun org-brain--visually-sorted-siblings-from (pair)
  "List of siblings for a parent, sorted unless the parent in PAIR has a :nosort: tag, or empty list if the parent has a :nosiblings: tag."
  (let ((parent (car pair)))
    (unless (member org-brain-exclude-siblings-tag (org-brain-get-tags parent))
      (org-brain--maybe-visually-sort parent (cdr pair)))))

(defun org-brain--visually-sorted-pins ()
  "List of pins visually sorted."
  (org-brain--visually-sorted org-brain-pins))

(defun org-brain--visually-sorted-selected ()
  "Visually sorted selection list."
  (org-brain--visually-sorted org-brain-selected))

(defun org-brain--vis-pinned ()
  "Insert pinned entries.
Helper function for `org-brain-visualize'."
  (insert "PINNED:")
  (dolist (pin (org-brain--visually-sorted-pins))
    (insert "  ")
    (org-brain-insert-visualize-button pin 'org-brain-pinned 'pinned))
  (insert "\n"))

(defun org-brain--vis-selected ()
  "Insert selected entries.
Helper function for `org-brain-visualize'."
  (unless (null org-brain-selected)
    (insert "SELECTED:")
    (dolist (selection (org-brain--visually-sorted-selected))
      (insert "  ")
      (org-brain-insert-visualize-button selection 'org-brain-selected-list))
    (insert "\n")))

(defun org-brain--hist-entries-to-draw (max-width hist width to-draw)
  "Determines the entries in HIST that can fit on a line of MAX-WIDTH.
Returns those entries in reversed order.
WIDTH and TO-DRAW are state parameters.
WIDTH represents the width of the line comprising the elements in TO-DRAW.
Assumes elements will be drawn with a two-character padding between them.
Helper function for `org-brain--vis-history'."
  (if (null hist)
      to-draw
    (let* ((entry-title-width (string-width (org-brain-vis-title (car hist))))
           (new-line-width (+ width 2 entry-title-width)))
      (if (and (<= max-width new-line-width)
               (not (null to-draw)))  ; Always display at least one entry
          to-draw
        (org-brain--hist-entries-to-draw max-width (cdr hist) new-line-width (cons (car hist) to-draw))))))

(defun org-brain--vis-history ()
  "Show as many of the most recently visited entries as fit on one line.
Helper function for `org-brain-visualize'."
  (insert "HISTORY:")
  (dolist (entry (org-brain--hist-entries-to-draw (window-width) org-brain--vis-history (string-width "HISTORY:") nil))
    (insert "  ")
    (org-brain-insert-visualize-button entry 'org-brain-history-list 'history))
  (insert "\n"))

(defun org-brain--insert-wire (&rest strings)
  "Helper function for drawing fontified wires in the org-brain visualization buffer."
  (insert (propertize (apply 'concat strings) 'face 'org-brain-wires)))

(defun org-brain--vis-parents-siblings (entry)
  "Insert parents and siblings of ENTRY.
Helper function for `org-brain-visualize'."
  (when-let ((siblings (org-brain--visually-sorted-siblings entry)))
    (let ((parent-positions nil)
          (max-width 0))
      (dolist (parent siblings)
        (let* ((children-links (org-brain--visually-sorted-siblings-from parent))
               (sibling-middle (ceiling (/ (length children-links) 2.0)))
               (base-line (if org-brain-show-history 5 4))
               (col-start (+ 3 max-width))
               (parent-width (string-width (org-brain-vis-title (car parent)))))
          (org-goto-line base-line)
          (mapc
           (lambda (child)
             (picture-forward-column col-start)
             (org-brain--insert-wire (make-string (1+ parent-width) ?\ ) "+-")
             (org-brain-insert-visualize-button
              child
              (if (and (member (car parent) (org-brain-local-parent child))
                       (member (car parent) (org-brain-local-parent entry)))
                  'org-brain-local-sibling
                'org-brain-sibling) 'sibling)
             (setq max-width (max max-width (current-column)))
             (newline (forward-line 1)))
	   children-links)
          (org-goto-line base-line)
          (forward-line (1- sibling-middle))
          (picture-forward-column col-start)
          (push (cons (picture-current-line)
                      (+ (current-column) (/ parent-width 2)))
                parent-positions)
          (org-brain-insert-visualize-button
           (car parent)
           (if (member (car parent) (org-brain-local-parent entry))
               'org-brain-local-parent
             'org-brain-parent) 'parent)
          (setq max-width (max max-width (current-column)))
          (when children-links
            (org-brain--insert-wire "-")
            (delete-char (+ 1 parent-width)))))
      ;; Draw lines
      (when parent-positions
        (let ((maxline (line-number-at-pos (point-max))))
          ;; Bottom line
          (org-goto-line maxline)
          (picture-forward-column (cdar (last parent-positions)))
          (picture-move-down 1)
          (org-brain--insert-wire (make-string (1+ (- (cdar parent-positions)
                                                      (cdar (last parent-positions))))
                                               ?-))
          ;; Lines from parents to bottom
          (dolist (pos parent-positions)
            (org-goto-line (car pos))
            (picture-forward-column (cdr pos))
            (while (< (line-number-at-pos (point))
                      maxline)
              (picture-move-down 1)
              (org-brain--insert-wire "|")
              (unless (looking-at-p "\n") (delete-char 1)))
            (picture-move-down 1)
            (ignore-errors
              (delete-char 1))
            (org-brain--insert-wire "+"))
          ;; Line to main entry
          (move-to-column (/ (+ (cdar (last parent-positions))
                                (cdar parent-positions))
                             2)
			  t)
          (delete-char 1)
          (when (> (length parent-positions) 1)
            (org-brain--insert-wire "+")
            (backward-char 1)
            (picture-move-down 1)
            (org-brain--insert-wire "|")
            (picture-move-down 1))
          (org-brain--insert-wire "V"))))
    (picture-move-down 1)))

(defun org-brain--vis-children (entry)
  "Insert children of ENTRY.
Helper function for `org-brain-visualize'."
  (let ((tags (org-brain-get-tags entry t)))
    (when-let ((children (org-brain--visually-sorted-children entry))
               (fill-col (if (member org-brain-each-child-on-own-line-tag
                                     (org-brain-get-tags entry))
                             0
                           (eval org-brain-child-linebreak-sexp))))
      (insert "\n\n")
      (dolist (child children)
        (let ((child-title (org-brain-title child))
              (face (if (member entry (org-brain-local-parent child))
                        'org-brain-local-child
                      'org-brain-child)))
          (when (> (+ (current-column) (length child-title)) fill-col)
            (insert "\n"))
          (org-brain-insert-visualize-button child face 'child)
          (insert "  "))))))

(defun org-brain--vis-friends (entry)
  "Insert friends of ENTRY.
Helper function for `org-brain-visualize'."
  (when-let ((friends (org-brain--visually-sorted-friends entry)))
    (org-brain--insert-wire " <-> ")
    (dolist (friend friends)
      (let ((column (current-column)))
        (org-brain-insert-visualize-button friend 'org-brain-friend 'friend)
        (picture-move-down 1)
        (move-to-column column t)))
    (org-brain-delete-current-line)
    (backward-char 1)))

(defun org-brain--vis-resources (resources)
  "Insert links to RESOURCES.
Helper function for `org-brain-visualize'."
  (when resources
    (insert "\n\n--- Resources ---------------------------------\n")
    (mapc #'org-brain-insert-resource-button resources)))

(defvar org-brain--vis-entry-text-marker 0
  "Marker to where `org-brain-text' begins in `org-brain-visualize-mode'.")

(defun org-brain--vis-text (entry)
  "Insert text of ENTRY.
Helper function for `org-brain-visualize'."
  (if-let ((text (org-brain-text entry)))
      (progn
        (setq text (string-trim text))
        (if (or (boundp 'org-brain-polymode)
                org-brain-show-full-entry
                (> (length text) 0))
            (progn
              (insert "\n\n")
              (setq org-brain--vis-entry-text-marker (point-marker))
              (insert "--- Entry -------------------------------------\n\n")
              (run-hooks 'org-brain-after-visualize-hook)
              (insert (with-temp-buffer
                        (insert text)
                        (delay-mode-hooks
                          (org-mode)
                          (setq-local org-pretty-entities t)
                          (font-lock-ensure (point-min) (point-max))
                          (buffer-string))))
              (run-hooks 'org-brain-visualize-text-hook))
          (run-hooks 'org-brain-after-visualize-hook)))
    (run-hooks 'org-brain-after-visualize-hook)))

;;;;; Mind-map

(defun org-brain-map-create-indentation (level)
  "Return a string of spaces, length determined by indentation LEVEL."
  (make-string (* level 2) ? ))

(defun org-brain-insert-recursive-child-buttons (entry max-level indent)
  "Use `org-brain-insert-visualize-button' on ENTRY and its children.
Also insert buttons for grand-children, up to MAX-LEVEL.
Each button is indented, starting at level determined by INDENT."
  (insert (org-brain-map-create-indentation indent))
  (org-brain-insert-visualize-button entry 'org-brain-child (if (> max-level 0) 'grandchild 'child))
  (insert "\n")
  (dolist (child (and (> max-level 0)
		      (org-brain--visually-sorted-children entry)))
    (org-brain-insert-recursive-child-buttons child (1- max-level) (1+ indent))))

(defun org-brain-tree-depth (tree)
  "Return depth of nested TREE."
  (if (atom tree)
      0
    (1+ (cl-reduce #'max (mapcar #'org-brain-tree-depth tree)))))

(defun org-brain-recursive-parents (entry max-level &optional func)
  "Return a tree of ENTRY and its (grand)parents, up to MAX-LEVEL.
Apply FUNC to each tree member. FUNC is a function which takes an
entry as the only argument. If FUNC is nil or omitted, get the
raw entry data."
  (cons (funcall (or func #'identity) entry)
        (when (> max-level 0)
          (mapcar (lambda (x) (org-brain-recursive-parents x (1- max-level) func))
                  (org-brain-parents entry)))))

(defun org-brain-recursive-children (entry max-level &optional func)
  "Return a tree of ENTRY and its (grand)children up to MAX-LEVEL.
Apply FUNC to each tree member. FUNC is a function which takes an
entry as the only argument. If FUNC is nil or omitted, get the
raw entry data."
  (cons (funcall (or func #'identity) entry)
        (when (> max-level 0)
          (mapcar (lambda (x) (org-brain-recursive-children x (1- max-level) func))
                  (org-brain-children entry)))))

(defun org-brain-insert-recursive-parent-buttons (entry max-level indent)
  "Use `org-brain-insert-visualize-button' on ENTRY and its parents.
Also insert buttons for grand-parents, up to MAX-LEVEL.
Each button is indented, starting at level determined by INDENT."
  (dolist (parent (and (> max-level 0)
                       (org-brain--visually-sorted-parents entry)))
    (org-brain-insert-recursive-parent-buttons parent (1- max-level) (1- indent)))
  (insert (org-brain-map-create-indentation indent))
  (org-brain-insert-visualize-button entry 'org-brain-parent (if (> max-level 0) 'grandparent 'parent))
  (insert "\n"))

(defun org-brain-mind-map (entry parent-max-level children-max-level)
  "Insert a tree of buttons for the parents and children of ENTRY.
Insert friends to ENTRY in a row above the tree.
Will also insert grand-parents up to PARENT-MAX-LEVEL, and
children up to CHILDREN-MAX-LEVEL.
Return the position of ENTRY in the buffer."
  (insert "FRIENDS:")
  (dolist (friend (org-brain--visually-sorted-friends entry))
    (insert "  ")
    (org-brain-insert-visualize-button friend 'org-brain-friend 'friend))
  (setq-local org-brain--visualize-header-end-pos (point))
  (insert "\n\n")
  (let ((indent (1- (org-brain-tree-depth (org-brain-recursive-parents entry parent-max-level))))
        (entry-pos))
    (dolist (parent (org-brain--visually-sorted-siblings entry))
      (org-brain-insert-recursive-parent-buttons (car parent) (1- parent-max-level) (1- indent))
      (dolist (sibling (org-brain--visually-sorted-siblings-from parent))
        (insert (org-brain-map-create-indentation indent))
        (org-brain-insert-visualize-button sibling 'org-brain-sibling 'sibling)
        (insert "\n")))
    (insert (org-brain-map-create-indentation indent))
    (setq entry-pos (point))
    (insert (propertize (org-brain-title entry)
                        'face 'org-brain-title
                        'aa2u-text t) "\n")
    (dolist (child (org-brain--visually-sorted-children entry))
      (org-brain-insert-recursive-child-buttons child (1- children-max-level) (1+ indent)))
    entry-pos))

(defvar org-brain-visualizing-mind-map nil)
(defvar-local org-brain-mind-map-child-level 1)
(defvar-local org-brain-mind-map-parent-level 1)

(defun org-brain-visualize-mind-map ()
  "Toggle mind-map view of `org-brain-visualize'."
  (interactive)
  (when (eq major-mode 'org-brain-visualize-mode)
    (setq org-brain-visualizing-mind-map (not org-brain-visualizing-mind-map))
    (org-brain-visualize org-brain--vis-entry)))

;;;;; Show/hide nested levels
(defun org-brain-show-descendant-level ()
  "Show one more level of descendant entries to the right in the mind-map visualization buffer."
  (interactive)
  (setq org-brain-visualizing-mind-map t)
  (cl-incf org-brain-mind-map-child-level)
  (org-brain--revert-if-visualizing))

(defun org-brain-hide-descendant-level ()
  "Hide the rightmost level of descendant entries in the mind-map visualization buffer."
  (interactive)
  (setq org-brain-visualizing-mind-map t)
  (when (> org-brain-mind-map-child-level 1)
    (cl-decf org-brain-mind-map-child-level))
  (org-brain--revert-if-visualizing))

(defun org-brain-show-ancestor-level ()
  "Show one more level of ancestor entries to the left in the mind-map visualization buffer."
  (interactive)
  (setq org-brain-visualizing-mind-map t)
  (cl-incf org-brain-mind-map-parent-level)
  (org-brain--revert-if-visualizing))

(defun org-brain-hide-ancestor-level ()
  "Hide the leftmost level of ancestor entries in the mind-map visualization buffer."
  (interactive)
  (setq org-brain-visualizing-mind-map t)
  (when (> org-brain-mind-map-parent-level 1)
    (cl-decf org-brain-mind-map-parent-level))
  (org-brain--revert-if-visualizing))

(define-obsolete-function-alias
  'org-brain-visualize-add-grandchild 'org-brain-show-descendant-level "0.5")
(define-obsolete-function-alias
  'org-brain-visualize-remove-grandchild 'org-brain-hide-descendant-level "0.5")
(define-obsolete-function-alias
  'org-brain-visualize-add-grandparent 'org-brain-show-ancestor-level "0.5")
(define-obsolete-function-alias
  'org-brain-visualize-remove-grandparent 'org-brain-hide-ancestor-level "0.5")

;;;;; Polymode

;; This code has been adapted from Dustin Lacewell's project polybrain
;; Have a look at: https://github.com/dustinlacewell/polybrain.el/

(with-eval-after-load 'polymode
  (define-hostmode org-brain-poly-hostmode
    :mode 'org-brain-visualize-mode)

  (define-innermode org-brain-poly-innermode
    :mode 'org-mode
    :head-matcher "^[─-]\\{3\\} Entry [─-]+\n"
    :tail-matcher "\\'"
    :head-mode 'host
    :tail-mode 'host)

  (define-polymode org-brain-polymode
    :hostmode 'org-brain-poly-hostmode
    :innermodes '(org-brain-poly-innermode)
    (setq-local polymode-move-these-vars-from-old-buffer
                (delq 'buffer-read-only polymode-move-these-vars-from-old-buffer)))

  (defun org-brain-polymode-save ()
    "Save entry text to the entry's file."
    (interactive)
    (when (buffer-modified-p)
      (let ((text (save-excursion
                    (goto-char org-brain--vis-entry-text-marker)
                    (end-of-line)
                    (buffer-substring (point) (point-max)))))
        (find-file (org-brain-entry-path org-brain--vis-entry))
        (seq-let (entry-min entry-max) (org-brain-text-positions org-brain--vis-entry)
          (goto-char entry-min)
          (delete-region entry-min entry-max)
          (insert text)
          (unless (looking-at-p "\n")
            (insert "\n\n"))
          (save-buffer)
          (switch-to-buffer (other-buffer (current-buffer) 1))
          (set-buffer-modified-p nil)))))

  (define-key org-brain-polymode-map "\C-x\C-s" 'org-brain-polymode-save))

;;;; Brain link

(defun org-brain-link-complete (&optional link-type)
  "Create an org-link target string to a file in `org-brain-path'.
LINK-TYPE will be \"brain\" by default."
  (setq link-type (or link-type "brain"))
  (let* ((entry (ignore-errors (org-brain-entry-at-pt t)))
         (choice (if (and (not entry)
                          (member link-type
                                  (list org-brain-child-link-name
                                        org-brain-parent-link-name
                                        org-brain-friend-link-name)))
                     (error "No entry  at point")
                   (org-brain-choose-entry "Entry: " 'all))))
    (cond ((string-equal link-type org-brain-child-link-name)
           (org-brain-add-relationship entry choice))
          ((string-equal link-type org-brain-parent-link-name)
           (org-brain-add-relationship choice entry))
          ((string-equal link-type org-brain-friend-link-name)
           (org-brain--internal-add-friendship entry choice))
          ((and org-brain-backlink (string-equal link-type "brain"))
           (if entry
               (org-brain-add-resource
                (concat "brain:" (org-brain-entry-identifier entry))
                (concat (and (stringp org-brain-backlink) org-brain-backlink)
                        (org-brain-title entry))
                nil choice)
             (org-brain-add-resource
              (cl-concatenate 'string
                              "file:"
                              (file-relative-name
                               (buffer-file-name)
                               (file-name-directory (org-brain-entry-path choice)))
                              (if-let ((outline-path
                                        (and org-brain-backlink-heading
                                             (ignore-errors (org-get-outline-path t)))))
                                  (concat "::* " (nth 0 (last outline-path)))))
              (concat (and (stringp org-brain-backlink) org-brain-backlink)
		      (if (and org-brain-backlink-heading
                               (ignore-errors (org-get-outline-path t)))
                          (string-join (org-get-outline-path t) " > ")
                        (file-name-base)))
              nil choice))))
    (let ((link (concat link-type ":"
                        (if (org-brain-filep choice) choice (nth 2 choice)))))
      (if (version< (org-release) "9.3")
          (push link org-insert-link-history)
        (push link org-link--insert-history))
      (push `(,link ,(org-brain-title choice)) org-stored-links)
      link)))

(defun org-brain-link-store ()
  "Store a brain: type link from an `org-brain-visualize-mode' buffer."
  (when (eq major-mode 'org-brain-visualize-mode)
    (org-store-link-props
     :type "brain"
     :link (concat "brain:" (org-brain-entry-identifier org-brain--vis-entry))
     :description (org-brain-title org-brain--vis-entry))))

(org-link-set-parameters "brain"
                         :complete 'org-brain-link-complete
                         :follow 'org-brain-goto
                         :store 'org-brain-link-store)

(org-link-set-parameters org-brain-child-link-name
                         :complete (lambda () (org-brain-link-complete org-brain-child-link-name))
                         :follow 'org-brain-goto)

(org-link-set-parameters org-brain-parent-link-name
                         :complete (lambda () (org-brain-link-complete org-brain-parent-link-name))
                         :follow 'org-brain-goto)

(org-link-set-parameters org-brain-friend-link-name
                         :complete (lambda () (org-brain-link-complete org-brain-friend-link-name))
                         :follow 'org-brain-goto)

;;;; Brain switch link

(defun org-brain--switch-link-complete ()
  "Create an org-link target string to an org-brain and one of its entries."
  (let* ((org-brain-path (read-directory-name "Brain dir: " org-brain-path))
         (entry (org-brain-choose-entry
                 "Entry: " (append (when org-brain-include-file-entries (org-brain-files t))
                                   (org-brain-headline-entries)))))
    (concat "brainswitch:" org-brain-path
            "::"
            (if (org-brain-filep entry)
                entry
              (nth 2 entry)))))

(defun org-brain--switch-and-visualize (directory entry)
  "Switch brain to DIRECTORY and visualize ENTRY.
ENTRY should be a string; an id in the case of an headline entry."
  (org-brain-switch-brain directory)
  (org-brain-visualize (or (org-brain-entry-from-id entry) entry)))

(defun org-brain--switch-link-follow (link)
  "Follow function for brainswitch links."
  (let ((link-parts (split-string link "::")))
    (org-brain--switch-and-visualize (car link-parts)
                                     (cadr link-parts))))

(org-link-set-parameters "brainswitch"
                         :complete 'org-brain--switch-link-complete
                         :follow 'org-brain--switch-link-follow)

;;;; Helm integration

(with-eval-after-load 'helm
  (defun helm-brain--add-children (_c)
    (dolist (candidate (helm-marked-candidates))
      (org-brain-add-relationship
       (org-brain-entry-at-pt) (or (org-brain-entry-from-id candidate) candidate)))
    (org-brain--revert-if-visualizing))

  (defun helm-brain--add-parents (_c)
    (dolist (candidate (helm-marked-candidates))
      (org-brain-add-relationship
       (or (org-brain-entry-from-id candidate) candidate) (org-brain-entry-at-pt)))
    (org-brain--revert-if-visualizing))

  (defun helm-brain--add-friends (_c)
    (dolist (candidate (helm-marked-candidates))
      (org-brain--internal-add-friendship
       (org-brain-entry-at-pt) (or (org-brain-entry-from-id candidate) candidate)))
    (org-brain--revert-if-visualizing))

  (defun helm-brain--delete-entries (_c)
    (dolist (candidate (helm-marked-candidates))
      (org-brain-delete-entry (or (org-brain-entry-from-id candidate) candidate))))

  (defun helm-brain--archive (_c)
    (dolist (candidate (helm-marked-candidates))
      (org-brain-archive (or (org-brain-entry-from-id candidate) candidate))))

  (defun helm-brain--select (_c)
    (dolist (candidate (helm-marked-candidates))
      (org-brain-select (or (org-brain-entry-from-id candidate) candidate) 1)))

  (defun helm-brain--unselect (_c)
    (dolist (candidate (helm-marked-candidates))
      (org-brain-select (or (org-brain-entry-from-id candidate) candidate) -1)))

  (defvar helm-brain--actions
    (helm-make-actions
     "Visualize" (lambda (x)
                   (org-brain-visualize (or (org-brain-entry-from-id x) x)))
     "Add children" 'helm-brain--add-children
     "Add parents" 'helm-brain--add-parents
     "Add friends" 'helm-brain--add-friends
     "Delete" 'helm-brain--delete-entries
     "Archive" 'helm-brain--archive
     "Select" 'helm-brain--select
     "Unselect" 'helm-brain--unselect))

  (defvar helm-brain--source
    (helm-make-source "Brain" 'helm-source-sync
      :candidates #'org-brain--all-targets
      :action 'helm-brain--actions))

  (defvar helm-brain--fallback-source
    (helm-make-source "New entry" 'helm-source-dummy
      :action (helm-make-actions
               "Visualize" (lambda (x)
                             (org-brain-visualize (org-brain-get-entry-from-title x)))
               "Add children" 'helm-brain--add-children
               "Add parents" 'helm-brain--add-parents
               "Add friends" 'helm-brain--add-friends)))

  (defun helm-brain ()
    "Use `helm' to choose among your org-brain entries.
Provides actions for visualizing, adding/removing relations, etc.
Supports selecting multiple entries at once."
    (interactive)
    (helm :sources '(helm-brain--source helm-brain--fallback-source))))

;;;; Ivy integration

(with-eval-after-load 'ivy
  (defun counsel-brain ()
    "Use Ivy to choose among your org-brain entries.
Provides actions for visualizing, adding/removing relations, etc."
    (interactive)
    (let ((targets (org-brain--all-targets)))
      (ivy-read "Org-brain: "
                targets
                :action (lambda (x)
                          (org-brain-visualize
                           (if (stringp x)
                               (org-brain-get-entry-from-title x)
                             (or (org-brain-entry-from-id (cdr x))
                                 (cdr x)))))
                :preselect (ignore-errors
                             (org-brain-entry-name
                              (org-brain-entry-at-pt)))
                :caller 'counsel-brain)))

  (defun counsel-brain--add-child (child)
    (org-brain-add-relationship (org-brain-entry-at-pt)
                                (or (org-brain-entry-from-id (cdr child))
                                    (cdr child)))
    (org-brain--revert-if-visualizing))

  (defun counsel-brain--add-parent (parent)
    (org-brain-add-relationship (or (org-brain-entry-from-id (cdr parent))
                                    (cdr parent))
                                (org-brain-entry-at-pt))
    (org-brain--revert-if-visualizing))

  (defun counsel-brain--add-friend (friend)
    (org-brain--internal-add-friendship (org-brain-entry-at-pt)
                                        (or (org-brain-entry-from-id (cdr friend))
                                            (cdr friend)))
    (org-brain--revert-if-visualizing))

  (defun counsel-brain--delete (x)
    (org-brain-delete-entry (or (org-brain-entry-from-id (cdr x)) (cdr x))))

  (defun counsel-brain--archive (x)
    (org-brain-archive (or (org-brain-entry-from-id (cdr x)) (cdr x))))

  (defun counsel-brain--select (x)
    (org-brain-select (or (org-brain-entry-from-id (cdr x)) (cdr x)) 1))

  (defun counsel-brain--unselect (x)
    (org-brain-select (or (org-brain-entry-from-id (cdr x)) (cdr x)) -1))

  (ivy-set-actions
   'counsel-brain
   '(("c" counsel-brain--add-child "add as child")
     ("p" counsel-brain--add-parent "add as parent")
     ("f" counsel-brain--add-friend "add as friend")
     ("d" counsel-brain--delete "delete")
     ("a" counsel-brain--archive "archive")
     ("s" counsel-brain--select "select")
     ("S" counsel-brain--unselect "unselect"))))

(provide 'org-brain)
;;; org-brain.el ends here
