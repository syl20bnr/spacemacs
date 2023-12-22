;;; magit-repos.el --- Listing repositories  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2024 The Magit Project Contributors

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Magit is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Magit is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements support for listing repositories.  This
;; includes getting a Lisp list of known repositories as well as a
;; mode for listing repositories in a buffer.

;;; Code:

(require 'magit-core)

(declare-function magit-status-setup-buffer "magit-status" (&optional directory))

(defvar x-stretch-cursor)

;;; Options

(defcustom magit-repository-directories nil
  "List of directories that are or contain Git repositories.

Each element has the form (DIRECTORY . DEPTH).  DIRECTORY has
to be a directory or a directory file-name, a string.  DEPTH,
an integer, specifies the maximum depth to look for Git
repositories.  If it is 0, then only add DIRECTORY itself.

This option controls which repositories are being listed by
`magit-list-repositories'.  It also affects `magit-status'
\(which see) in potentially surprising ways."
  :package-version '(magit . "3.0.0")
  :group 'magit-essentials
  :type '(repeat (cons directory (integer :tag "Depth"))))

(defgroup magit-repolist nil
  "List repositories in a buffer."
  :link '(info-link "(magit)Repository List")
  :group 'magit-modes)

(defcustom magit-repolist-mode-hook '(hl-line-mode)
  "Hook run after entering Magit-Repolist mode."
  :package-version '(magit . "2.9.0")
  :group 'magit-repolist
  :type 'hook
  :get #'magit-hook-custom-get
  :options '(hl-line-mode))

(defcustom magit-repolist-columns
  '(("Name"    25 magit-repolist-column-ident nil)
    ("Version" 25 magit-repolist-column-version
     ((:sort magit-repolist-version<)))
    ("B<U"      3 magit-repolist-column-unpulled-from-upstream
     (;; (:help-echo "Upstream changes not in branch")
      (:right-align t)
      (:sort <)))
    ("B>U"      3 magit-repolist-column-unpushed-to-upstream
     (;; (:help-echo "Local changes not in upstream")
      (:right-align t)
      (:sort <)))
    ("Path"    99 magit-repolist-column-path nil))
  "List of columns displayed by `magit-list-repositories'.

Each element has the form (HEADER WIDTH FORMAT PROPS).

HEADER is the string displayed in the header.  WIDTH is the width
of the column.  FORMAT is a function that is called with one
argument, the repository identification (usually its basename),
and with `default-directory' bound to the toplevel of its working
tree.  It has to return a string to be inserted or nil.  PROPS is
an alist that supports the keys `:right-align', `:pad-right' and
`:sort'.

The `:sort' function has a weird interface described in the
docstring of `tabulated-list--get-sort'.  Alternatively `<' and
`magit-repolist-version<' can be used as those functions are
automatically replaced with functions that satisfy the interface.
Set `:sort' to nil to inhibit sorting; if unspecified, then the
column is sortable using the default sorter.

You may wish to display a range of numeric columns using just one
character per column and without any padding between columns, in
which case you should use an appropriate HEADER, set WIDTH to 1,
and set `:pad-right' to 0.  \"+\" is substituted for numbers higher
than 9."
  :package-version '(magit . "2.12.0")
  :group 'magit-repolist
  :type '(repeat (list :tag "Column"
                       (string   :tag "Header Label")
                       (integer  :tag "Column Width")
                       (function :tag "Inserter Function")
                       (repeat   :tag "Properties"
                                 (list (choice :tag "Property"
                                               (const :right-align)
                                               (const :pad-right)
                                               (const :sort)
                                               (symbol))
                                       (sexp   :tag "Value"))))))

(defcustom magit-repolist-column-flag-alist
  '((magit-untracked-files . "N")
    (magit-unstaged-files . "U")
    (magit-staged-files . "S"))
  "Association list of predicates and flags for `magit-repolist-column-flag'.

Each element is of the form (FUNCTION . FLAG).  Each FUNCTION is
called with no arguments, with `default-directory' bound to the
top level of a repository working tree, until one of them returns
a non-nil value.  FLAG corresponding to that function is returned
as the value of `magit-repolist-column-flag'."
  :package-version '(magit . "3.0.0")
  :group 'magit-repolist
  :type '(alist :key-type (function :tag "Predicate Function")
                :value-type (string :tag "Flag")))

(defcustom magit-repolist-sort-key '("Path" . nil)
  "Initial sort key for buffer created by `magit-list-repositories'.
If nil, no additional sorting is performed.  Otherwise, this
should be a cons cell (NAME . FLIP).  NAME is a string matching
one of the column names in `magit-repolist-columns'.  FLIP, if
non-nil, means to invert the resulting sort."
  :package-version '(magit . "3.2.0")
  :group 'magit-repolist
  :type '(choice (const nil)
                 (cons (string :tag "Column name")
                       (boolean :tag "Flip order"))))

;;; List Repositories
;;;; List Commands
;;;###autoload
(defun magit-list-repositories ()
  "Display a list of repositories.

Use the option `magit-repository-directories' to control which
repositories are displayed."
  (interactive)
  (magit-repolist-setup (default-value 'magit-repolist-columns)))

;;;; Mode Commands

(defun magit-repolist-status (&optional _button)
  "Show the status for the repository at point."
  (interactive)
  (if-let ((id (tabulated-list-get-id)))
      (magit-status-setup-buffer (expand-file-name id))
    (user-error "There is no repository at point")))

(defun magit-repolist-mark ()
  "Mark a repository and move to the next line."
  (interactive)
  (magit-repolist--ensure-padding)
  (tabulated-list-put-tag "*" t))

(defun magit-repolist-unmark ()
  "Unmark a repository and move to the next line."
  (interactive)
  (tabulated-list-put-tag " " t))

(defun magit-repolist-fetch (repos)
  "Fetch all marked or listed repositories."
  (interactive (list (magit-repolist--get-repos ?*)))
  (run-hooks 'magit-credential-hook)
  (magit-repolist--mapc (apply-partially #'magit-run-git "remote" "update")
                        repos "Fetching in %s..."))

(defun magit-repolist-find-file-other-frame (repos file)
  "Find a file in all marked or listed repositories."
  (interactive (list (magit-repolist--get-repos ?*)
                     (read-string "Find file in repositories: ")))
  (magit-repolist--mapc (apply-partially #'find-file-other-frame file) repos))

(defun magit-repolist--ensure-padding ()
  "Set `tabulated-list-padding' to 2, unless that is already non-zero."
  (when (zerop tabulated-list-padding)
    (setq tabulated-list-padding 2)
    (tabulated-list-init-header)
    (tabulated-list-print t)))

(defun magit-repolist--get-repos (&optional char)
  "Return marked repositories or `all' if none are marked.
If optional CHAR is non-nil, then only return repositories
marked with that character.  If no repositories are marked
then ask whether to act on all repositories instead."
  (or (magit-repolist--marked-repos char)
      (if (magit-confirm 'repolist-all
            "Nothing selected.  Act on ALL displayed repositories")
          'all
        (user-error "Abort"))))

(defun magit-repolist--marked-repos (&optional char)
  "Return marked repositories.
If optional CHAR is non-nil, then only return repositories
marked with that character."
  (let (c list)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (setq c (char-after))
        (unless (eq c ?\s)
          (if char
              (when (eq c char)
                (push (tabulated-list-get-id) list))
            (push (cons c (tabulated-list-get-id)) list)))
        (forward-line)))
    list))

(defun magit-repolist--mapc (fn repos &optional msg)
  "Apply FN to each directory in REPOS for side effects only.
If REPOS is the symbol `all', then call FN for all displayed
repositories.  When FN is called, `default-directory' is bound to
the top-level directory of the current repository.  If optional
MSG is non-nil then that is displayed around each call to FN.
If it contains \"%s\" then the directory is substituted for that."
  (when (eq repos 'all)
    (setq repos nil)
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (push (tabulated-list-get-id) repos)
        (forward-line)))
    (setq repos (nreverse repos)))
  (let ((base default-directory)
        (len (length repos))
        (i 0))
    (mapc (lambda (repo)
            (let ((default-directory
                   (file-name-as-directory (expand-file-name repo base))))
              (if msg
                  (let ((msg (concat (format "(%s/%s) " (cl-incf i) len)
                                     (format msg default-directory))))
                    (message msg)
                    (funcall fn)
                    (message (concat msg "done")))
                (funcall fn))))
          repos)))

;;;; Mode

(defvar-keymap magit-repolist-mode-map
  :doc "Local keymap for Magit-Repolist mode buffers."
  :parent tabulated-list-mode-map
  "C-m" #'magit-repolist-status
  "m"   #'magit-repolist-mark
  "u"   #'magit-repolist-unmark
  "f"   #'magit-repolist-fetch
  "5"   #'magit-repolist-find-file-other-frame)

(define-derived-mode magit-repolist-mode tabulated-list-mode "Repos"
  "Major mode for browsing a list of Git repositories."
  (setq-local x-stretch-cursor nil)
  (setq tabulated-list-padding 0)
  (add-hook 'tabulated-list-revert-hook #'magit-repolist-refresh nil t)
  (setq imenu-prev-index-position-function
        #'magit-repolist--imenu-prev-index-position)
  (setq imenu-extract-index-name-function #'tabulated-list-get-id))

(defun magit-repolist-setup (columns)
  (unless magit-repository-directories
    (user-error "You need to customize `magit-repository-directories' %s"
                "before you can list repositories"))
  (with-current-buffer (get-buffer-create "*Magit Repositories*")
    (magit-repolist-mode)
    (setq-local magit-repolist-columns columns)
    (magit-repolist-setup-1)
    (magit-repolist-refresh)
    (switch-to-buffer (current-buffer))))

(defun magit-repolist-setup-1 ()
  (unless tabulated-list-sort-key
    (setq tabulated-list-sort-key
          (pcase-let ((`(,column . ,flip) magit-repolist-sort-key))
            (cons (or (car (assoc column magit-repolist-columns))
                      (caar magit-repolist-columns))
                  flip))))
  (setq tabulated-list-format
        (vconcat (seq-map-indexed
                  (lambda (column idx)
                    (pcase-let* ((`(,title ,width ,_fn ,props) column)
                                 (sort-set (assoc :sort props))
                                 (sort-fn (cadr sort-set)))
                      (nconc (list title width
                                   (cond ((eq sort-fn '<)
                                          (magit-repolist-make-sorter
                                           sort-fn #'string-to-number idx))
                                         ((eq sort-fn 'magit-repolist-version<)
                                          (magit-repolist-make-sorter
                                           sort-fn #'identity idx))
                                         (sort-fn sort-fn)
                                         (sort-set nil)
                                         (t t)))
                             (flatten-tree props))))
                  magit-repolist-columns))))

(defun magit-repolist-refresh ()
  (setq tabulated-list-entries
        (mapcar (pcase-lambda (`(,id . ,path))
                  (let ((default-directory path))
                    (list path
                          (vconcat
                           (mapcar (pcase-lambda (`(,title ,width ,fn ,props))
                                     (or (funcall fn `((:id ,id)
                                                       (:title ,title)
                                                       (:width ,width)
                                                       ,@props))
                                         ""))
                                   magit-repolist-columns)))))
                (magit-list-repos-uniquify
                 (--map (cons (file-name-nondirectory (directory-file-name it))
                              it)
                        (magit-list-repos)))))
  (message "Listing repositories...")
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (message "Listing repositories...done"))

(defun magit-repolist--imenu-prev-index-position ()
  (and (not (bobp))
       (forward-line -1)))

;;;; Columns

(defun magit-repolist-make-sorter (sort-predicate convert-cell column-idx)
  "Return a function suitable as a sorter for tabulated lists.
See `tabulated-list--get-sorter'.  Given a more reasonable API
this would not be necessary and one could just use SORT-PREDICATE
directly.  CONVERT-CELL can be used to turn the cell value, which
is always a string back into, e.g., a number.  COLUMN-IDX has to
be the index of the column that uses the returned sorter function."
  (lambda (a b)
    (funcall sort-predicate
             (funcall convert-cell (aref (cadr a) column-idx))
             (funcall convert-cell (aref (cadr b) column-idx)))))

(defun magit-repolist-column-ident (spec)
  "Insert the identification of the repository.
Usually this is just its basename."
  (cadr (assq :id spec)))

(defun magit-repolist-column-path (_)
  "Insert the absolute path of the repository."
  (abbreviate-file-name default-directory))

(defvar magit-repolist-column-version-regexp "\
\\(?1:-\\(?2:[0-9]*\\)\
\\(?3:-g[a-z0-9]*\\)\\)?\
\\(?:-\\(?4:dirty\\)\\)\
?\\'")

(defvar magit-repolist-column-version-resume-regexp
  "\\`Resume development\\'")

(defun magit-repolist-column-version (_)
  "Insert a description of the repository's `HEAD' revision."
  (and-let* ((v (or (magit-git-string "describe" "--tags" "--dirty")
                    ;; If there are no tags, use the date in MELPA format.
                    (magit-rev-format "%cd-g%h" nil
                                      "--date=format:%Y%m%d.%H%M"))))
    (save-match-data
      (when (string-match magit-repolist-column-version-regexp v)
        (magit--put-face (match-beginning 0) (match-end 0) 'shadow v)
        (when (match-end 2)
          (magit--put-face (match-beginning 2) (match-end 2) 'bold v))
        (when (match-end 4)
          (magit--put-face (match-beginning 4) (match-end 4) 'error v))
        (when (and (equal (match-string 2 v) "1")
                   (string-match-p magit-repolist-column-version-resume-regexp
                                   (magit-rev-format "%s")))
          (setq v (replace-match (propertize "+" 'face 'shadow) t t v 1))))
      (if (and v (string-match "\\`[0-9]" v))
          (concat " " v)
        (when (and v (string-match "\\`[^0-9]+" v))
          (magit--put-face 0 (match-end 0) 'shadow v))
        v))))

(defun magit-repolist-version< (a b)
  (save-match-data
    (let ((re "[0-9]+\\(\\.[0-9]*\\)*"))
      (setq a (and (string-match re a) (match-string 0 a)))
      (setq b (and (string-match re b) (match-string 0 b)))
      (cond ((and a b) (version< a b))
            (b nil)
            (t t)))))

(defun magit-repolist-column-branch (_)
  "Insert the current branch."
  (let ((branch (magit-get-current-branch)))
    (if (member branch magit-main-branch-names)
        (magit--propertize-face branch 'shadow)
      branch)))

(defun magit-repolist-column-upstream (_)
  "Insert the upstream branch of the current branch."
  (magit-get-upstream-branch))

(defun magit-repolist-column-flag (_)
  "Insert a flag as specified by `magit-repolist-column-flag-alist'.

By default this indicates whether there are uncommitted changes.
- N if there is at least one untracked file.
- U if there is at least one unstaged file.
- S if there is at least one staged file.
Only one letter is shown, the first that applies."
  (seq-some (pcase-lambda (`(,fun . ,flag))
              (and (funcall fun) flag))
            magit-repolist-column-flag-alist))

(defun magit-repolist-column-flags (_)
  "Insert all flags as specified by `magit-repolist-column-flag-alist'.
This is an alternative to function `magit-repolist-column-flag',
which only lists the first one found."
  (mapconcat (pcase-lambda (`(,fun . ,flag))
               (if (funcall fun) flag " "))
             magit-repolist-column-flag-alist
             ""))

(defun magit-repolist-column-unpulled-from-upstream (spec)
  "Insert number of upstream commits not in the current branch."
  (and-let* ((br (magit-get-upstream-branch)))
    (magit-repolist-insert-count (cadr (magit-rev-diff-count "HEAD" br)) spec)))

(defun magit-repolist-column-unpulled-from-pushremote (spec)
  "Insert number of commits in the push branch but not the current branch."
  (and-let* ((br (magit-get-push-branch nil t)))
    (magit-repolist-insert-count (cadr (magit-rev-diff-count "HEAD" br)) spec)))

(defun magit-repolist-column-unpushed-to-upstream (spec)
  "Insert number of commits in the current branch but not its upstream."
  (and-let* ((br (magit-get-upstream-branch)))
    (magit-repolist-insert-count (car (magit-rev-diff-count "HEAD" br)) spec)))

(defun magit-repolist-column-unpushed-to-pushremote (spec)
  "Insert number of commits in the current branch but not its push branch."
  (and-let* ((br (magit-get-push-branch nil t)))
    (magit-repolist-insert-count (car (magit-rev-diff-count "HEAD" br)) spec)))

(defun magit-repolist-column-branches (spec)
  "Insert number of branches."
  (magit-repolist-insert-count (length (magit-list-local-branches))
                               `((:normal-count 1) ,@spec)))

(defun magit-repolist-column-stashes (spec)
  "Insert number of stashes."
  (magit-repolist-insert-count (length (magit-list-stashes)) spec))

(defun magit-repolist-insert-count (n spec)
  (magit--propertize-face
   (if (and  (> n 9) (= (cadr (assq :width spec)) 1))
       "+"
     (number-to-string n))
   (if (> n (or (cadr (assq :normal-count spec)) 0)) 'bold 'shadow)))

;;; Read Repository

(defun magit-read-repository (&optional read-directory-name)
  "Read a Git repository in the minibuffer, with completion.

The completion choices are the basenames of top-levels of
repositories found in the directories specified by option
`magit-repository-directories'.  In case of name conflicts
the basenames are prefixed with the name of the respective
parent directories.  The returned value is the actual path
to the selected repository.

If READ-DIRECTORY-NAME is non-nil or no repositories can be
found based on the value of `magit-repository-directories',
then read an arbitrary directory using `read-directory-name'
instead."
  (if-let ((repos (and (not read-directory-name)
                       magit-repository-directories
                       (magit-repos-alist))))
      (let ((reply (magit-completing-read "Git repository" repos)))
        (file-name-as-directory
         (or (cdr (assoc reply repos))
             (if (file-directory-p reply)
                 (expand-file-name reply)
               (user-error "Not a repository or a directory: %s" reply)))))
    (file-name-as-directory
     (read-directory-name "Git repository: "
                          (or (magit-toplevel) default-directory)))))

(defun magit-list-repos ()
  (cl-mapcan (pcase-lambda (`(,dir . ,depth))
               (magit-list-repos-1 dir depth))
             magit-repository-directories))

(defun magit-list-repos-1 (directory depth)
  (cond ((file-readable-p (expand-file-name ".git" directory))
         (list (file-name-as-directory directory)))
        ((and (> depth 0) (magit-file-accessible-directory-p directory))
         (--mapcat (and (file-directory-p it)
                        (magit-list-repos-1 it (1- depth)))
                   (directory-files directory t
                                    directory-files-no-dot-files-regexp t)))))

(defun magit-list-repos-uniquify (alist)
  (let (result (dict (make-hash-table :test #'equal)))
    (dolist (a (delete-dups alist))
      (puthash (car a) (cons (cdr a) (gethash (car a) dict)) dict))
    (maphash
     (lambda (key value)
       (if (length= value 1)
           (push (cons key (car value)) result)
         (setq result
               (append result
                       (magit-list-repos-uniquify
                        (--map (cons (concat
                                      key "\\"
                                      (file-name-nondirectory
                                       (directory-file-name
                                        (substring it 0 (- (1+ (length key)))))))
                                     it)
                               value))))))
     dict)
    result))

(defun magit-repos-alist ()
  (magit-list-repos-uniquify
   (--map (cons (file-name-nondirectory (directory-file-name it)) it)
          (magit-list-repos))))

;;; _
(provide 'magit-repos)
;;; magit-repos.el ends here
