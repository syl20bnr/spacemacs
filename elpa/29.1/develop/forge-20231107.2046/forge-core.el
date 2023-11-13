;;; forge-core.el --- Core functionality  -*- lexical-binding:t -*-

;; Copyright (C) 2018-2023 Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

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

;;; Code:

(require 'magit)

(require 'cl-lib)
(require 'compat)
(require 'dash)
(require 'eieio)
(require 'seq)
(require 'subr-x)

(require 'transient)

(require 'forge-db)

(eval-when-compile
  (cl-pushnew 'id     eieio--known-slot-names)
  (cl-pushnew 'name   eieio--known-slot-names)
  (cl-pushnew 'owner  eieio--known-slot-names)
  (cl-pushnew 'number eieio--known-slot-names))

;;; Options

(defgroup forge nil
  "Options concerning Git forges."
  :group 'magit)

(defgroup forge-faces nil
  "Faces concerning Git forges."
  :group 'forge
  :group 'magit-faces)

(defcustom forge-alist
  '(;; Forges
    ("github.com" "api.github.com"
     "github.com" forge-github-repository)
    ("gitlab.com" "gitlab.com/api/v4"
     "gitlab.com" forge-gitlab-repository)
    ("salsa.debian.org" "salsa.debian.org/api/v4"
     "salsa.debian.org" forge-gitlab-repository)
    ("framagit.org" "framagit.org/api/v4"
     "framagit.org" forge-gitlab-repository)
    ("gitlab.gnome.org" "gitlab.gnome.org/api/v4"
     "gitlab.gnome.org" forge-gitlab-repository)
    ;; Forges (API unsupported)
    ("codeberg.org" "codeberg.org/api/v1"
     "codeberg.org" forge-gitea-repository)
    ("code.orgmode.org" "code.orgmode.org/api/v1"
     "code.orgmode.org" forge-gogs-repository)
    ("bitbucket.org" "api.bitbucket.org/2.0"
     "bitbucket.org" forge-bitbucket-repository)
    ;; Semi-Forges
    ("git.savannah.gnu.org" nil
     "git.savannah.gnu.org" forge-cgit**-repository)
    ("git.kernel.org" nil
     "git.kernel.org" forge-cgit-repository)
    ("repo.or.cz" nil
     "repo.or.cz" forge-repoorcz-repository)
    ("git.suckless.org" nil
     "git.suckless.org" forge-stagit-repository)
    ("git.sr.ht" nil
     "git.sr.ht" forge-srht-repository))
  "List of Git forges.

Each entry has the form (GITHOST APIHOST ID CLASS).

GITHOST is matched against the host part of Git remote urls
  using `forge--url-regexp' to identify the forge.
APIHOST is the api endpoint of the forge's api.
ID is used to identify the forge in the local database.
CLASS is the class to be used for repository from the forge.

GITHOST and APIHOST can be changed, but ID and CLASS are final.
If you change ID, then the identity of every repository from
that forge changes.  If you change CLASS, then things start
falling apart.

There can be multiple elements that only differ in GITHOST.
Among those, the canonical element should come first.  Any
elements that have the same APIHOST must also have the same
ID, and vice-versa.

Complications:

* When connecting to a Github Enterprise edition whose REST
  API's end point is \"<host>/v3\" and whose GraphQL API's
  end point is \"<host>/graphql\", then use \"<host>/v3\" as
  APIHOST.  This is a historic accident.  See issue #174.

* This variable does not account for the possibility that the
  git repository does not use the same address as the web
  interface.  That is another historic accident and if many
  users are affected by it, we might have to fix it properly.
  If you would like that, comment on issue #532.

  For now a workaround has to be used.  Assuming the web
  interface is available at \"example.com\" and the repository
  is at \"ssh.example.com\", use \"example.com\" as GITHOST and
  add something like this to \".gitconfig\":

    [url \"git@ssh.example.com\"]
        insteadOf = git@example.com"
  :package-version '(forge . "0.1.0")
  :group 'forge
  :type '(repeat (list (string :tag "Git host")
                       (choice (string :tag "API endpoint")
                               (const  :tag "No API" nil))
                       (string :tag "ID")
                       (symbol :tag "Repository class"))))

(defcustom forge-pull-notifications nil
  "Whether `forge-pull' also fetches notifications.
If this is nil, then `forge-pull-notifications' has to be used."
  :package-version '(forge . "0.2.0")
  :group 'forge
  :type 'boolean)

;;; Class

(defclass forge-object (closql-object) () :abstract t)

(defmacro forge--childp (obj type)
  "Somewhat similar to `cl-typep' but only for (possibly unknown) classes.
TYPE is evaluated at macro-expansion time but unlike with
`cl-typep' the respective class does not have to be defined
at that time."
  (let ((fn (intern (concat (symbol-name (eval type)) "--eieio-childp"))))
    `(and (fboundp ',fn) (,fn ,obj))))

;;; Query

(cl-defgeneric forge-get-parent (object)
  "Return the parent object of OBJECT.
The hierarchy is repository > topic > post.
For other objects return nil.")

(cl-defgeneric forge-get-repository (demand)
  "Return a forge repository object or nil, or signal an error.

The DEMAND argument controls what to do when the object isn't
stored in the database yet, or if it is marked as sparse.  The
valid values are:

* `nil' If the repository is stored in the database then return
  it, even if it is sparse.  Otherwise return nil.

* `t' If the repository isn't stored in the database or if the
  object is sparse, then signal an error, informing the user
  that `this-command' cannot be run until the repository has
  been pulled.

* `full' If the repository is stored in the database and the
  object isn't sparse, then return it.  Otherwise return nil.

* `stub' If the repository is stored in the database, then return
  it, regardless of whether it is sparse or not.  Otherwise create
  a new object and return it, but do not store it in the database.
  In the latter case it is assumed that the caller does not need
  the `id' and `forge-id' slots whose value differ from what they
  would be if the object were retrieved from the database.

* `maybe' Like `stub', but when that would signal an error, just
  return nil.

* `create' This value is only intended to be used by commands
  that fetch data from the API.  If the repository is stored in
  the database, then return that, regardless of whether the
  object is sparse or not.  If the repository is not stored in the
  database, then make an API request to determine the ID used on
  the forge, derive our own ID from that, and store a new sparse
  object in the database and return it.

If DEMAND is t, `stub' or `create', then also signal an error if
if the Forge repository cannot be determined.  This happens if
`default-directory' is not inside a Git repository, if there is
no matching entry in `forge-alist', of if it is unclear which
remote to use.

Also update the object's `apihost', `githost' and `remote' slots
according to the respective entry in `forge-alist' and the REMOTE
argument.

Also see info node `(forge) Repository Detection'.")

(cl-defgeneric forge-get-topic ()
  "Return a forge issue or pullreq object.")

(cl-defgeneric forge-get-issue ()
  "Return a forge issue object.")

(cl-defgeneric forge-get-pullreq ()
  "Return a forge pullreq object.")

;;; Identity

(cl-defgeneric forge--object-id (class &rest args)
  "Return the database id for the CLASS object specified by ARGS.")

(cl-defgeneric forge--repository-ids (class host owner name &optional stub)
  "Return the database and forge ids for the specified CLASS object.")

(cl-defmethod magit-section-ident-value ((obj forge-object))
  "Return the value ob OBJ's `id' slot.
Using OBJ itself would not be appropriate because multiple
non-equal objects may exist, representing the same thing."
  (oref obj id))

(defun forge--set-id-slot (repo object slot rows)
  "Set the value in OBJECT for SLOT to VALUE, actually storing foreign keys."
  ;; TODO Should CloSQL advice `oset' to make this unnecessary?
  (let ((repo-id (oref repo id)))
    (closql-oset
     object slot
     (mapcar (lambda (val)
               (forge--object-id repo-id
                                 (if (atom val) val (alist-get 'id val))))
             rows))))

;;; Format

(cl-defgeneric forge--format (object slot &optional spec)
  "Return a string based on SPEC and the format-string in OBJECT's SLOT.
The available `format'-like specs depend on the type of OBJECT.
SPEC can be used to add additional specs, as for `format-spec'.
The latter override the former.  SLOT is expected to be class-
allocated.  Some methods also accept a format string in place
of SLOT.")

(cl-defmethod forge--format-resource ((object forge-object) resource)
  "Return an API resource based on RESOURCE and slots of OBJECT.
For use in `forge--FORGE-METHOD' such as `forge--ghub-get'.
RESOURCE is a string separated by slashes.  Each part that begins
with a colon is replaced with a value from OBJECT.  `:repo' is a
synonym for `:name'.  `:project' is a like `:owner/:name', but the
slash is quoted on Gitlab.  `:topic' is a synonym for `:number'
but only if OBJECT is a topic.  Any other `:SLOT' means to use
the value of that slot in OBJECT, or if that doesn't exist in its
parent object (determined using `forge-get-parent')."
  (save-match-data
    (setq resource
          (replace-regexp-in-string
           ":\\([^/]+\\)"
           (lambda (str)
             (let ((slot (intern (substring str 1))))
               (or (and-let*
                       ((v (ignore-errors
                             (pcase slot
                               ('repo    (oref object name))
                               ('project (concat (string-replace
                                                  "/" "%2F" (oref object owner))
                                                 "%2F"
                                                 (oref object name)))
                               ('topic   (and (forge--childp object 'forge-topic)
                                              (oref object number)))
                               (_        (eieio-oref object slot))))))
                     (format "%s" v))
                   str)))
           resource t t))
    (if (string-match ":[^/]*" resource)
        (if-let ((parent (ignore-errors (forge-get-parent object))))
            (forge--format-resource parent resource)
          (error "Cannot resolve %s for a %s"
                 (match-string 0 resource)
                 (eieio-object-class object)))
      resource)))

;;; URLs

(defun forge--url-regexp ()
  (concat "\\`\\(?:git://\\|"
          "[^/@]+@\\|"
          "\\(?:ssh\\|ssh\\+git\\|git\\+ssh\\)://\\(?:[^/@]+@\\)?\\|"
          "https?://\\(?:[^/@]+@\\)?\\)?"
          (regexp-opt (mapcar #'car forge-alist) t)
          "\\(?::[0-9]+\\)?"
          "\\(?:/\\|:/?\\)"
          "\\(.+?\\)"
          "\\(?:\\.git\\|/\\)?\\'"))

(defun forge--split-url (url)
  (and (string-match (forge--url-regexp) url)
       (and-let* ((host (match-string 1 url))
                  (path (match-string 2 url))
                  (path (forge--split-url-path
                         (nth 3 (assoc host forge-alist))
                         path)))
         (cons host path))))

(cl-defmethod forge--split-url-path
  ((_class (subclass forge-repository)) path)
  (and (string-match "\\`\\([^/]+\\)/\\([^/]+?\\)\\'" path)
       (list (match-string 1 path)
             (match-string 2 path))))

(cl-defmethod forge--split-url-path
  ((_class (subclass forge-noapi-repository)) path)
  (and (string-match "\\`\\(?:~?\\(.+\\)/\\)?\\([^/]+?\\)\\'" path)
       (list (match-string 1 path)
             (match-string 2 path))))

(defun forge--url-p (url)
  (save-match-data
    (and (string-match (forge--url-regexp) url)
         (nth 2 (assoc (match-string 1 url) forge-alist)))))

(defun forge--forge-remote-p (remote)
  (and-let* ((url (magit-git-string "remote" "get-url" remote)))
    (forge--url-p url)))

(defun forge--url-equal (urlA urlB)
  (or (equal urlA urlB)
      (save-match-data
        (let ((re (forge--url-regexp))
              hostA repoA hostB repoB)
          (and (when (string-match re urlA)
                 (setq hostA (match-string 1 urlA))
                 (setq repoA (match-string 2 urlA)))
               (when (string-match re urlB)
                 (setq hostB (match-string 1 urlB))
                 (setq repoB (match-string 2 urlB)))
               (equal repoA repoB)
               (equal (cl-caddr (assoc hostA forge-alist))
                      (cl-caddr (assoc hostB forge-alist))))))))

;;; Miscellaneous

(defun forge--sanitize-string (string)
  ;; For Gitlab this may also be nil.
  (if string (string-replace "\r\n" "\n" string) ""))

(defun forge--uuid ()
  "Return string with random (version 4) UUID."
  ;; This is a copy of `org-id-uuid'.
  ;; Only used in `forge-create-mark'.
  (let ((rnd (md5 (format "%s%s%s%s%s%s%s"
                          (random)
                          (current-time)
                          (user-uid)
                          (emacs-pid)
                          (user-full-name)
                          user-mail-address
                          (recent-keys)))))
    (format "%s-%s-4%s-%s%s-%s"
            (substring rnd 0 8)
            (substring rnd 8 12)
            (substring rnd 13 16)
            (format "%x"
                    (logior
                     #b10000000
                     (logand
                      #b10111111
                      (string-to-number
                       (substring rnd 16 18) 16))))
            (substring rnd 18 20)
            (substring rnd 20 32))))

;;; _
(provide 'forge-core)
;;; forge-core.el ends here
