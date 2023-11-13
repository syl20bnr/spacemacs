;;; magit-tag.el --- Tag functionality  -*- lexical-binding:t -*-

;; Copyright (C) 2008-2023 The Magit Project Contributors

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

;; This library implements tag commands.

;;; Code:

(require 'magit)

;; For `magit-tag-delete'.
(defvar helm-comp-read-use-marked)

;;; Commands

;;;###autoload (autoload 'magit-tag "magit" nil t)
(transient-define-prefix magit-tag ()
  "Create or delete a tag."
  :man-page "git-tag"
  ["Arguments"
   ("-f" "Force"    ("-f" "--force"))
   ("-a" "Annotate" ("-a" "--annotate"))
   ("-s" "Sign"     ("-s" "--sign"))
   (magit-tag:--local-user)]
  [["Create"
    ("t"  "tag"     magit-tag-create)
    ("r"  "release" magit-tag-release)]
   ["Do"
    ("k"  "delete"  magit-tag-delete)
    ("p"  "prune"   magit-tag-prune)]])

(defun magit-tag-arguments ()
  (transient-args 'magit-tag))

(transient-define-argument magit-tag:--local-user ()
  :description "Sign as"
  :class 'transient-option
  :shortarg "-u"
  :argument "--local-user="
  :reader #'magit-read-gpg-signing-key
  :history-key 'magit:--gpg-sign)

;;;###autoload
(defun magit-tag-create (name rev &optional args)
  "Create a new tag with the given NAME at REV.
With a prefix argument annotate the tag.
\n(git tag [--annotate] NAME REV)"
  (interactive (list (magit-read-tag "Tag name")
                     (magit-read-branch-or-commit "Place tag on")
                     (let ((args (magit-tag-arguments)))
                       (when current-prefix-arg
                         (cl-pushnew "--annotate" args :test #'equal))
                       args)))
  (magit-run-git-with-editor "tag" args name rev))

;;;###autoload
(defun magit-tag-delete (tags)
  "Delete one or more tags.
If the region marks multiple tags (and nothing else), then offer
to delete those, otherwise prompt for a single tag to be deleted,
defaulting to the tag at point.
\n(git tag -d TAGS)"
  (interactive (list (if-let ((tags (magit-region-values 'tag)))
                         (magit-confirm t nil "Delete %d tags" nil tags)
                       (let ((helm-comp-read-use-marked t))
                         (magit-read-tag "Delete tag" t)))))
  (magit-run-git "tag" "-d" tags))

;;;###autoload
(defun magit-tag-prune (tags remote-tags remote)
  "Offer to delete tags missing locally from REMOTE, and vice versa."
  (interactive
   (let* ((remote (magit-read-remote "Prune tags using remote"))
          (tags   (magit-list-tags))
          (rtags  (prog2 (message "Determining remote tags...")
                      (magit-remote-list-tags remote)
                    (message "Determining remote tags...done")))
          (ltags  (cl-set-difference tags rtags :test #'equal))
          (rtags  (cl-set-difference rtags tags :test #'equal)))
     (unless (or ltags rtags)
       (message "Same tags exist locally and remotely"))
     (unless (magit-confirm t
               "Delete %s locally"
               "Delete %d tags locally"
               'noabort ltags)
       (setq ltags nil))
     (unless (magit-confirm t
               "Delete %s from remote"
               "Delete %d tags from remote"
               'noabort rtags)
       (setq rtags nil))
     (list ltags rtags remote)))
  (when tags
    (magit-call-git "tag" "-d" tags))
  (when remote-tags
    (magit-run-git-async "push" remote (--map (concat ":" it) remote-tags))))

(defvar magit-tag-version-regexp-alist
  '(("^[-._+ ]?snapshot\\.?$" . -4)
    ("^[-._+]$" . -4)
    ("^[-._+ ]?\\(cvs\\|git\\|bzr\\|svn\\|hg\\|darcs\\)\\.?$" . -4)
    ("^[-._+ ]?unknown\\.?$" . -4)
    ("^[-._+ ]?alpha\\.?$" . -3)
    ("^[-._+ ]?beta\\.?$" . -2)
    ("^[-._+ ]?\\(pre\\|rc\\)\\.?$" . -1))
  "Overrides `version-regexp-alist' for `magit-tag-release'.
See also `magit-release-tag-regexp'.")

(defvar magit-release-tag-regexp "\\`\
\\(?1:\\(?:v\\(?:ersion\\)?\\|r\\(?:elease\\)?\\)[-_]?\\)?\
\\(?2:[0-9]+\\(?:\\.[0-9]+\\)*\
\\(?:-[a-zA-Z0-9-]+\\(?:\\.[a-zA-Z0-9-]+\\)*\\)?\\)\\'"
  "Regexp used by `magit-tag-release' to parse release tags.

The first submatch must match the prefix, if any.  The second
submatch must match the version string.

If this matches versions that are not dot separated numbers,
then `magit-tag-version-regexp-alist' has to contain entries
for the separators allowed here.")

(defvar magit-release-commit-regexp "\\`Release version \\(.+\\)\\'"
  "Regexp used by `magit-tag-release' to parse release commit messages.
The first submatch must match the version string.")

;;;###autoload
(defun magit-tag-release (tag msg &optional args)
  "Create a release tag for `HEAD'.

Assume that release tags match `magit-release-tag-regexp'.

If `HEAD's message matches `magit-release-commit-regexp', then
base the tag on the version string specified by that.  Otherwise
prompt for the name of the new tag using the highest existing
tag as initial input and leaving it to the user to increment the
desired part of the version string.

If `--annotate' is enabled, then prompt for the message of the
new tag.  Base the proposed tag message on the message of the
highest tag, provided that that contains the corresponding
version string and substituting the new version string for that.
Otherwise propose something like \"Foo-Bar 1.2.3\", given, for
example, a TAG \"v1.2.3\" and a repository located at something
like \"/path/to/foo-bar\"."
  (interactive
   (save-match-data
     (pcase-let*
         ((`(,pver ,ptag ,pmsg) (car (magit--list-releases)))
          (msg (magit-rev-format "%s"))
          (ver (and (string-match magit-release-commit-regexp msg)
                    (match-string 1 msg)))
          (_   (and (not ver)
                    (require (quote sisyphus) nil t)
                    (string-match magit-release-commit-regexp
                                  (magit-rev-format "%s" ptag))
                    (user-error "Use `sisyphus-create-release' first")))
          (tag (cond
                ((not ptag)
                 (read-string "Create first release tag: "
                              (if (and ver (string-match-p "\\`[0-9]" ver))
                                  (concat "v" ver)
                                ver)))
                (ver
                 (concat (and (string-match magit-release-tag-regexp ptag)
                              (match-string 1 ptag))
                         ver))
                (t
                 (read-string
                  (format "Create release tag (previous was %s): " ptag)
                  ptag))))
          (ver (and (string-match magit-release-tag-regexp tag)
                    (match-string 2 tag)))
          (args (magit-tag-arguments)))
       (list tag
             (and (member "--annotate" args)
                  (read-string
                   (format "Message for %S: " tag)
                   (cond ((and pver (string-match (regexp-quote pver) pmsg))
                          (replace-match ver t t pmsg))
                         ((and ptag (string-match (regexp-quote ptag) pmsg))
                          (replace-match tag t t pmsg))
                         (t (format "%s %s"
                                    (capitalize
                                     (file-name-nondirectory
                                      (directory-file-name (magit-toplevel))))
                                    ver)))))
             args))))
  (magit-run-git-async "tag" args (and msg (list "-m" msg)) tag)
  (set-process-sentinel
   magit-this-process
   (lambda (process event)
     (when (memq (process-status process) '(exit signal))
       (magit-process-sentinel process event)
       (magit-refs-setup-buffer "HEAD" (magit-show-refs-arguments))))))

(defun magit--list-releases ()
  "Return a list of releases.
The list is ordered, beginning with the highest release.
Each release element has the form (VERSION TAG MESSAGE).
`magit-release-tag-regexp' is used to determine whether
a tag qualifies as a release tag."
  (save-match-data
    (mapcar
     #'cdr
     (nreverse
      (cl-sort (cl-mapcan
                (lambda (line)
                  (and (string-match " +" line)
                       (let ((tag (substring line 0 (match-beginning 0)))
                             (msg (substring line (match-end 0))))
                         (and (string-match magit-release-tag-regexp tag)
                              (let ((ver (match-string 2 tag))
                                    (version-regexp-alist
                                     magit-tag-version-regexp-alist))
                                (list (list (version-to-list ver)
                                            ver tag msg)))))))
                ;; Cannot rely on "--sort=-version:refname" because
                ;; that gets confused if the version prefix has changed.
                (magit-git-lines "tag" "-n"))
               ;; The inverse of this function does not exist.
               #'version-list-< :key #'car)))))

;;; _
(provide 'magit-tag)
;;; magit-tag.el ends here
