;;; package-recipe.el --- Package recipes as EIEIO objects  -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2018-2024 Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/melpa/package-build
;; Keywords: maint tools

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

;; Package recipes as EIEIO objects.

;;; Code:

(require 'compat nil t)
(require 'eieio)
(require 'subr-x)
(require 'url-parse)

(defvar package-build-use-git-remote-hg)
(defvar package-build-recipes-dir)
(defvar package-build-working-dir)

;;; Classes

(defclass package-recipe ()
  ((url-format      :allocation :class       :initform nil)
   (repopage-format :allocation :class       :initform nil)
   (stable-p        :allocation :class       :initform nil)
   (name            :initarg :name           :initform nil)
   (url             :initarg :url            :initform nil)
   (repo            :initarg :repo           :initform nil)
   (repopage        :initarg :repopage       :initform nil)
   (files           :initarg :files          :initform nil)
   (branch          :initarg :branch         :initform nil)
   (commit          :initarg :commit         :initform nil)
   (time                                     :initform nil)
   (version                                  :initform nil)
   (version-regexp  :initarg :version-regexp :initform nil)
   (old-names       :initarg :old-names      :initform nil))
  :abstract t)

;;;; Git

(defclass package-git-recipe (package-recipe) ())

(defclass package-github-recipe (package-git-recipe)
  ((url-format      :initform "https://github.com/%s.git")
   (repopage-format :initform "https://github.com/%s")))

(defclass package-gitlab-recipe (package-git-recipe)
  ((url-format      :initform "https://gitlab.com/%s.git")
   (repopage-format :initform "https://gitlab.com/%s")))

(defclass package-codeberg-recipe (package-git-recipe)
  ((url-format      :initform "https://codeberg.org/%s.git")
   (repopage-format :initform "https://codeberg.org/%s")))

(defclass package-sourcehut-recipe (package-git-recipe)
  ((url-format      :initform "https://git.sr.ht/~%s")
   (repopage-format :initform "https://git.sr.ht/~%s")))

;;;; Mercurial

(defclass package-hg-recipe (package-recipe) ())

(defclass package-git-remote-hg-recipe (package-git-recipe) ())

;;; Methods

(cl-defmethod package-recipe--working-tree ((rcp package-recipe))
  (file-name-as-directory
   (expand-file-name (oref rcp name) package-build-working-dir)))

(cl-defmethod package-recipe--upstream-url ((rcp package-recipe))
  (or (oref rcp url)
      (format (oref rcp url-format)
              (oref rcp repo))))

(cl-defmethod package-recipe--upstream-url ((rcp package-git-remote-hg-recipe))
  (concat "hg::" (oref rcp url)))

(cl-defmethod package-recipe--upstream-protocol ((rcp package-recipe))
  (let ((url (package-recipe--upstream-url rcp)))
    (cond ((string-match "\\`\\([a-z]+\\)://" url)
           (match-string 1 url))
          ((string-match "\\`[^:/ ]+:" url) "ssh")
          (t "file"))))

(cl-defmethod package-recipe--fetcher ((rcp package-recipe))
  (substring (symbol-name (eieio-object-class rcp)) 8 -7))

;;; Constants

(defconst package-recipe--forge-fetchers
  '(github gitlab codeberg sourcehut))

(defconst package-recipe--fetchers
  (append '(git hg) package-recipe--forge-fetchers))

;;; Interface

(defun package-recipe-recipes ()
  "Return a list of the names of packages with available recipes."
  (directory-files package-build-recipes-dir nil "^[^.]"))

(defun package-recipe-read-name ()
  "Read the name of a package for which a recipe is available."
  (completing-read "Package: " (package-recipe-recipes)))

(defun package-recipe-lookup (name)
  "Return a recipe object for the package named NAME.
If no such recipe file exists or if the contents of the recipe
file is invalid, then raise an error."
  (let ((file (expand-file-name name package-build-recipes-dir)))
    (if (file-exists-p file)
        (let* ((recipe (with-temp-buffer
                         (insert-file-contents file)
                         (read (current-buffer))))
               (plist (cdr recipe))
               (fetcher (plist-get plist :fetcher))
               key val args)
          (package-recipe--validate recipe name)
          (while (and (setq key (pop plist))
                      (setq val (pop plist)))
            (unless (eq key :fetcher)
              (push val args)
              (push key args)))
          (when (and package-build-use-git-remote-hg (eq fetcher 'hg))
            (setq fetcher 'git-remote-hg))
          (apply (intern (format "package-%s-recipe" fetcher))
                 name :name name args))
      (error "No such recipe: %s" name))))

;;; Validation

;;;###autoload
(defun package-recipe-validate-all ()
  "Validate all recipes."
  (interactive)
  (dolist-with-progress-reporter (name (package-recipe-recipes))
      "Validating recipes..."
    (condition-case err
        (package-recipe-lookup name)
      (error (message "Invalid recipe for %s: %S" name (cdr err))))))

(defun package-recipe--validate (recipe name)
  "Perform some basic checks on the raw RECIPE for the package named NAME."
  (pcase-let ((`(,ident . ,plist) recipe))
    (cl-assert ident)
    (cl-assert (symbolp ident))
    (cl-assert (string= (symbol-name ident) name)
               nil "Recipe '%s' contains mismatched package name '%s'"
               name ident)
    (cl-assert plist)
    (let* ((symbol-keys '(:fetcher))
           (string-keys '(:url :repo :commit :branch :version-regexp))
           (list-keys '(:files :old-names))
           (all-keys (append symbol-keys string-keys list-keys)))
      (dolist (thing plist)
        (when (keywordp thing)
          (cl-assert (memq thing all-keys) nil "Unknown keyword %S" thing)))
      (let ((fetcher (plist-get plist :fetcher)))
        (cl-assert fetcher nil ":fetcher is missing")
        (if (memq fetcher package-recipe--forge-fetchers)
            (progn
              (cl-assert (plist-get plist :repo) ":repo is missing")
              (cl-assert (not (plist-get plist :url)) ":url is redundant"))
          (cl-assert (plist-get plist :url) ":url is missing")))
      (dolist (key symbol-keys)
        (when-let ((val (plist-get plist key)))
          (cl-assert (symbolp val) nil "%s must be a symbol but is %S" key val)))
      (dolist (key list-keys)
        (when-let ((val (plist-get plist key)))
          (cl-assert (listp val) nil "%s must be a list but is %S" key val)))
      (dolist (key string-keys)
        (when-let ((val (plist-get plist key)))
          (cl-assert (stringp val) nil "%s must be a string but is %S" key val)))
      (when-let ((spec (plist-get plist :files)))
        ;; `:defaults' is only allowed as the first element.
        ;; If we find it in that position, skip over it.
        (when (eq (car spec) :defaults)
          (setq spec (cdr spec)))
        ;; All other elements have to be strings or lists of strings.
        ;; A list whose first element is `:exclude' is also valid.
        (dolist (entry spec)
          (unless (cond ((stringp entry)
                         (not (equal entry "*")))
                        ((listp entry)
                         (and-let* ((globs (cdr entry)))
                           (and (or (eq (car entry) :exclude)
                                    (stringp (car entry)))
                                (seq-every-p (lambda (glob)
                                               (and (stringp glob)
                                                    (not (equal glob "*"))))
                                             globs)))))
            (error "Invalid files spec entry %S" entry))))
      ;; Silence byte compiler of Emacs 28.  It appears that uses
      ;; inside cl-assert sometimes, but not always, do not count.
      (list name ident all-keys))
    recipe))

(provide 'package-recipe)
;;; package-recipe.el ends here
