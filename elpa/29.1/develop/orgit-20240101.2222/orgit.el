;;; orgit.el --- Support for Org links to Magit buffers  -*- lexical-binding:t -*-

;; Copyright (C) 2014-2024 The Magit Project Contributors

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/magit/orgit
;; Keywords: hypermedia vc

;; Package-Version: 1.9.0
;; Package-Requires: (
;;     (emacs "25.1")
;;     (compat "29.1.4.1")
;;     (magit "3.3.0")
;;     (org "9.6.5"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package defines the Org link types `orgit', `orgit-rev', and
;; `orgit-log', which can be used to link to Magit status, revision,
;; and log buffers.

;; Use the command `org-store-link' in such a buffer to store a link.
;; Later you can insert that into an Org buffer using the command
;; `org-insert-link'.

;; Alternatively you can use `org-insert-link' to insert a link
;; without first storing it.  When prompted, first enter just the
;; link type followed by a colon and press RET.  Then you are
;; prompted again and can provide the repository with completion.
;; The `orgit-rev' and `orgit-log' types additionally read a revision,
;; again with completion.

;; Format
;; ------

;; The three link types defined here take these forms:
;;
;;    orgit:/path/to/repo/            links to a `magit-status' buffer
;;    orgit-rev:/path/to/repo/::REV   links to a `magit-revision' buffer
;;    orgit-log:/path/to/repo/::ARGS  links to a `magit-log' buffer

;; Before v1.3.0 only the first revision was stored in `orgit-log'
;; links, and all other revisions were discarded.  All other arguments
;; were also discarded and Magit's usual mechanism for determining the
;; switches and options was used instead.

;; For backward compatibility, and because it is the common case and
;; looks best, ARGS by default has the form REV as before.  However if
;; linking to a log buffer that shows the log for multiple revisions,
;; then ("REV"...) is used instead.  If `orgit-log-save-arguments' is
;; non-nil, then (("REV"...) ("ARG"...) [("FILE"...)]) is always used,
;; which allows restoring the buffer most faithfully.

;; Export
;; ------

;; When an Org file containing such links is exported, then the url of
;; the remote configured with `orgit-remote' is used to generate a web
;; url according to `orgit-export-alist'.  That webpage should present
;; approximately the same information as the Magit buffer would.

;; Both the remote to be considered the public remote, as well as the
;; actual web urls can be defined in individual repositories using Git
;; variables.

;; To use a remote different from `orgit-remote' but still use
;; `orgit-export-alist' to generate the web urls, use:
;;
;;    git config orgit.remote REMOTE-NAME

;; To explicitly define the web urls, use something like:
;;
;;    git config orgit.status http://example.com/repo/overview
;;    git config orgit.rev http://example.com/repo/revision/%r
;;    git config orgit.log http://example.com/repo/history/%r

;;; Code:

(require 'cl-lib)
(require 'compat)
(require 'format-spec)
(require 'magit)
(require 'org)

(unless (fboundp 'org-link-store-props)
  (defalias 'org-link-store-props 'org-store-link-props))

(eval-when-compile (require 'subr-x))

;;; Options

(defgroup orgit nil
  "Org links to Magit buffers."
  :group 'magit-extensions
  :group 'org-link)

(defcustom orgit-export-alist
  `(("github.com[:/]\\(.+?\\)\\(?:\\.git\\)?$"
     "https://github.com/%n"
     "https://github.com/%n/commits/%r"
     "https://github.com/%n/commit/%r")
    ("gitlab.com[:/]\\(.+?\\)\\(?:\\.git\\)?$"
     "https://gitlab.com/%n"
     "https://gitlab.com/%n/commits/%r"
     "https://gitlab.com/%n/commit/%r")
    ("git.sr.ht[:/]\\(.+?\\)\\(?:\\.git\\)?$"
     "https://git.sr.ht/%n"
     "https://git.sr.ht/%n/log/%r"
     "https://git.sr.ht/%n/commit/%r")
    ("bitbucket.org[:/]\\(.+?\\)\\(?:\\.git\\)?$"
     "https://bitbucket.org/%n"
     "https://bitbucket.org/%n/commits/branch/%r"
     "https://bitbucket.org/%n/commits/%r")
    ("code.orgmode.org[:/]\\(.+\\)$"
     "https://code.orgmode.org/cgit.cgi/%n"
     "https://code.orgmode.org/cgit.cgi/%n/commits/%r"
     "https://code.orgmode.org/cgit.cgi/%n/commit/%r")
    ("git.kernel.org/pub/scm[:/]\\(.+\\)$"
     "https://git.kernel.org/cgit/%n"
     "https://git.kernel.org/cgit/%n/log/?h=%r"
     "https://git.kernel.org/cgit/%n/commit/?id=%r"))
  "Alist used to translate Git urls to web urls when exporting links.

Each entry has the form (REMOTE-REGEXP STATUS LOG REVISION).  If
a REMOTE-REGEXP matches the url of the chosen remote then one of
the corresponding format strings STATUS, LOG or REVISION is used
according to the major mode of the buffer being linked to.

The first submatch of REMOTE-REGEXP has to match the repository
identifier (which usually consists of the username and repository
name).  The %n in the format string is replaced with that match.
LOG and REVISION additionally have to contain %r which is
replaced with the appropriate revision.

This can be overwritten in individual repositories using the Git
variables `orgit.status', `orgit.log' and `orgit.commit'. The
values of these variables must not contain %n, but in case of the
latter two variables they must contain %r.  When these variables
are defined then `orgit-remote' and `orgit.remote' have no effect."
  :group 'orgit
  :type '(repeat (list :tag "Remote template"
                       (regexp :tag "Remote regexp")
                       (string :tag "Status format")
                       (string :tag "Log format" :format "%{%t%}:    %v")
                       (string :tag "Revision format"))))

(defcustom orgit-remote "origin"
  "Default remote used when exporting links.

If there exists but one remote, then that is used unconditionally.
Otherwise if the Git variable `orgit.remote' is defined and that
remote exists, then that is used.  Finally the value of this
variable is used, provided it does exist in the given repository.
If all of the above fails then `orgit-export' raises an error."
  :group 'orgit
  :type 'string)

(defcustom orgit-log-save-arguments nil
  "Whether `orgit-log' links store arguments beside the revisions."
  :group 'orgit
  :type 'boolean)

(defcustom orgit-store-repository-id nil
  "Whether to store only name of repository instead of path.

If nil, then store the full path to the repository in the link.

If t, then attempt to store only the name of the repository.
This works by looking up the repository's path in the list of
repositories defined by `magit-repository-directories'.  If the
repository cannot be found there, then the path is used instead.
If the repository is checked out multiple times, then the names
of the clones are made unique by adding additional parts of the
path.

Storing just the name can be useful if you want to share links
with others, but be aware that doing so does not guarantee that
others will be able to open these links.  The repository has to
be checked out under the same name that you use and it has to be
configured in `magit-repository-directory'."
  :package-version '(orgit . "1.6.0")
  :group 'orgit
  :type 'boolean)

(defcustom orgit-store-reference nil
  "Whether `orgit-rev-store' attempts to store link to a reference.

If nil, then store a link to the commit itself, using its full
hash.

If t, then attempt to store a link to a tag or branch.  If that
is not possible because no such reference points at the commit,
then store a link to the commit itself.

The prefix argument also affects how the revision is stored,
see `orgit-rev-store'."
  :package-version '(orgit . "1.6.0")
  :group 'orgit
  :type 'boolean)

(defcustom orgit-rev-description-format "%%N (magit-rev %%R)"
  "Format used for `orgit-rev' links.

The format is used in two passes.  The first pass consumes all
specs of the form `%C'; to preserve a spec for the second pass
it has to be quoted like `%%C'.

The first pass accepts the \"pretty format\" specs documented
in the git-show(1) manpage.

The second pass accepts these specs:
`%%N' The name or id of the repository.
`%%R' Either a reference, abbreviated revision or revision of
      the form \":/TEXT\".  See `orgit-ref-store'."
  :package-version '(orgit . "1.8.0")
  :group 'orgit
  :type 'string)

;;; Command

;;;###autoload
(with-eval-after-load 'magit
  (keymap-set magit-mode-map "<remap> <org-store-link>" #'orgit-store-link))

;;;###autoload
(defun orgit-store-link (_arg)
  "Like `org-store-link' but store links to all selected commits, if any."
  (interactive "P")
  (if-let ((sections (magit-region-sections 'commit)))
      (save-excursion
        (dolist (section sections)
          (goto-char (oref section start))
          (set-mark (point))
          (activate-mark)
          (call-interactively #'org-store-link))
        (deactivate-mark))
    (call-interactively #'org-store-link)))

;;; Status

;;;###autoload
(with-eval-after-load 'org
  (with-eval-after-load 'magit
    (org-link-set-parameters "orgit"
                             :store    #'orgit-status-store
                             :follow   #'orgit-status-open
                             :export   #'orgit-status-export
                             :complete #'orgit-status-complete-link)))

;;;###autoload
(defun orgit-status-store ()
  "Store a link to a Magit-Status mode buffer.
When the region selects one or more commits, then do nothing.
In that case `orgit-rev-store' stores one or more links instead."
  (when (and (eq major-mode 'magit-status-mode)
             (not (magit-region-sections '(commit issue pullreq))))
    (let ((repo (orgit--current-repository)))
      (org-link-store-props
       :type        "orgit"
       :link        (format "orgit:%s" repo)
       :description (format "%s (magit-status)" repo)))))

;;;###autoload
(defun orgit-status-open (repo)
  (magit-status-setup-buffer (orgit--repository-directory repo)))

;;;###autoload
(defun orgit-status-export (path desc format)
  (orgit-export path desc format "status" 1))

;;;###autoload
(defun orgit-status-complete-link (&optional arg)
  (let ((default-directory (magit-read-repository arg)))
    (concat "orgit:" (orgit--current-repository))))

;;; Log

;;;###autoload
(with-eval-after-load 'org
  (with-eval-after-load 'magit
    (org-link-set-parameters "orgit-log"
                             :store    #'orgit-log-store
                             :follow   #'orgit-log-open
                             :export   #'orgit-log-export
                             :complete #'orgit-log-complete-link)))

;;;###autoload
(defun orgit-log-store ()
  "Store a link to a Magit-Log mode buffer.
When the region selects one or more commits, then do nothing.
In that case `orgit-rev-store' stores one or more links instead."
  (when (and (eq major-mode 'magit-log-mode)
             (not (magit-region-sections 'commit)))
    (let ((repo (orgit--current-repository))
          (args (if orgit-log-save-arguments
                    (if magit-buffer-log-files
                        (list magit-buffer-revisions
                              magit-buffer-log-args
                              magit-buffer-log-files)
                      (list magit-buffer-revisions
                            magit-buffer-log-args))
                  magit-buffer-revisions)))
      (org-link-store-props
       :type        "orgit-log"
       :link        (format "orgit-log:%s::%S" repo args)
       :description (format "%s %S" repo (cons 'magit-log args))))))

;;;###autoload
(defun orgit-log-open (path)
  (pcase-let* ((`(,repo ,args) (split-string path "::"))
               (default-directory (orgit--repository-directory repo))
               (`(,revs ,args ,files)
                (cond ((string-prefix-p "((" args)
                       (read args))
                      ((string-prefix-p "(" args)
                       (list (read args) (car (magit-log-arguments))))
                      (t
                       (list (list args) (car (magit-log-arguments)))))))
    (magit-log-setup-buffer revs args files)))

;;;###autoload
(defun orgit-log-export (path desc format)
  (pcase-let* ((`(,repo ,args) (split-string path "::"))
               (first-branch (cond ((string-prefix-p "((" args)
                                    (caar (read args)))
                                   ((string-prefix-p "(" args)
                                    (car (read args)))
                                   (t args))))
    (when (string-prefix-p "--" first-branch)
      (setq first-branch nil))
    (orgit-export (concat repo "::" first-branch)
                  desc format "log" 2)))

;;;###autoload
(defun orgit-log-complete-link (&optional arg)
  (let ((default-directory (magit-read-repository arg)))
    (format "orgit-log:%s::%s"
            (orgit--current-repository)
            (magit-read-branch-or-commit "Revision"))))

;;; Revision

;;;###autoload
(with-eval-after-load 'org
  (with-eval-after-load 'magit
    (org-link-set-parameters "orgit-rev"
                             :store    #'orgit-rev-store
                             :follow   #'orgit-rev-open
                             :export   #'orgit-rev-export
                             :complete #'orgit-rev-complete-link)))

;;;###autoload
(defun orgit-rev-store ()
  "Store a link to a Magit-Revision mode buffer.

By default store an abbreviated revision hash.

\\<global-map>With a single \\[universal-argument] \
prefix argument instead store the name of a tag
or branch that points at the revision, if any.  The meaning of this
prefix argument is reversed if `orgit-store-reference' is non-nil.

With a single \\[negative-argument] \
negative prefix argument store revision using the
form \":/TEXT\", which is described in the gitrevisions(7) manpage.

When more than one prefix argument is used, then `org-store-link'
stores a link itself, without calling this function.

When the region selects one or more commits, e.g., in a log, then
store links to the Magit-Revision mode buffers for these commits."
  (cond ((eq major-mode 'magit-revision-mode)
         (orgit-rev-store-1 magit-buffer-revision))
        ((derived-mode-p 'magit-mode)
         (when-let* ((revs (magit-region-values 'commit)))
           ;; Cannot use and-let* because of debbugs#31840.
           (mapc #'orgit-rev-store-1 revs)
           t))))

(defun orgit-rev-store-1 (rev)
  (pcase-let* ((repo (orgit--current-repository))
               (`(,rev ,desc)
                (pcase (list current-prefix-arg orgit-store-reference)
                  ((or '((4) nil) '(nil t))
                   (if-let ((ref (or (and (magit-ref-p rev) rev)
                                     (magit-name-tag rev)
                                     (magit-name-branch rev))))
                       (list ref ref)
                     (list (magit-rev-parse rev)
                           (magit-rev-abbrev rev))))
                  (`(- ,_)
                   (let ((txt (concat ":/" (magit-rev-format "%s" rev))))
                     (list txt txt)))
                  (_
                   (list (magit-rev-parse rev)
                         (magit-rev-abbrev rev))))))
    (org-link-store-props
     :type        "orgit-rev"
     :link        (format "orgit-rev:%s::%s" repo rev)
     :description (format-spec
                   (magit-rev-format orgit-rev-description-format rev)
                   `((?N . ,repo)
                     (?R . ,desc))))))

;;;###autoload
(defun orgit-rev-open (path)
  (pcase-let* ((`(,repo ,rev) (split-string path "::"))
               (default-directory (orgit--repository-directory repo)))
    (magit-revision-setup-buffer
     rev (car (magit-diff-arguments 'magit-revision-mode)) nil)))

;;;###autoload
(defun orgit-rev-export (path desc format)
  (orgit-export path desc format "rev" 3))

;;;###autoload
(defun orgit-rev-complete-link (&optional arg)
  (let ((default-directory (magit-read-repository arg)))
    (format "orgit-rev:%s::%s"
            (orgit--current-repository)
            (magit-read-branch-or-commit "Revision"))))

;;; Export

(defun orgit-export (path desc format gitvar idx)
  (pcase-let* ((`(,dir ,rev) (split-string path "::"))
               (dir (orgit--repository-directory dir)))
    (if (file-exists-p dir)
        (let* ((default-directory dir)
               (remotes (magit-git-lines "remote"))
               (remote  (magit-get "orgit.remote"))
               (remote  (cond ((length= remotes 1) (car remotes))
                              ((member remote remotes) remote)
                              ((member orgit-remote remotes) orgit-remote))))
          (if remote
              (if-let ((link
                        (or (and-let* ((url (magit-get "orgit" gitvar)))
                              (format-spec url `((?r . ,rev))))
                            (and-let* ((url (magit-get "remote" remote "url"))
                                       (format (cl-find-if
                                                (lambda (elt)
                                                  (string-match (car elt) url))
                                                orgit-export-alist)))
                              (format-spec (nth idx format)
                                           `((?n . ,(match-string 1 url))
                                             (?r . ,rev)))))))
                  (orgit--format-export link desc format)
                (signal 'org-link-broken
                        (list (format "Cannot determine public url for %s"
                                      path))))
            (signal 'org-link-broken
                    (list (format "Cannot determine public remote for %s"
                                  default-directory)))))
      (signal 'org-link-broken
              (list (format "Cannot determine public url for %s %s"
                            path "(which itself does not exist)"))))))

(defun orgit--format-export (link desc format)
  (pcase format
    ('html  (format "<a href=\"%s\">%s</a>" link desc))
    ('latex (format "\\href{%s}{%s}" link desc))
    ('ascii link)
    (_      link)))

;;; Utilities

(defun orgit--current-repository ()
  (or (and orgit-store-repository-id
           (car (rassoc default-directory (magit-repos-alist))))
      (abbreviate-file-name default-directory)))

(defun orgit--repository-directory (repo)
  (let ((dir (or (cdr (assoc repo (magit-repos-alist)))
                 (file-name-as-directory (expand-file-name repo)))))
    (cond ((file-exists-p dir) dir)
          ((string-match-p "\\`[./]" repo)
           (error "Cannot open link; %S does not exist" dir))
          (t
           (error "Cannot open link; no entry for %S in `%s'"
                  repo 'magit-repository-directories)))))

;;; _
(provide 'orgit)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; orgit.el ends here
