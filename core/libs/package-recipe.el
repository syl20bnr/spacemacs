;;; package-recipe.el --- Package recipes as EIEIO objects  -*- lexical-binding: t -*-

;; Copyright (C) 2018-2022  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Package recipes as EIEIO objects.

;;; Code:

(require 'eieio)
(require 'url-parse)

(defvar package-build-recipes-dir)
(defvar package-build-working-dir)

;;; Classes

(defclass package-recipe ()
  ((url-format      :allocation :class       :initform nil)
   (repopage-format :allocation :class       :initform nil)
   (time-regexp     :allocation :class       :initform nil)
   (stable-p        :allocation :class       :initform nil)
   (name            :initarg :name           :initform nil)
   (url             :initarg :url            :initform nil)
   (repo            :initarg :repo           :initform nil)
   (repopage        :initarg :repopage       :initform nil)
   (files           :initarg :files          :initform nil)
   (branch          :initarg :branch         :initform nil)
   (commit          :initarg :commit         :initform nil)
   (version-regexp  :initarg :version-regexp :initform nil)
   (old-names       :initarg :old-names      :initform nil))
  :abstract t)

(cl-defmethod package-recipe--working-tree ((rcp package-recipe))
  (file-name-as-directory
   (expand-file-name (oref rcp name) package-build-working-dir)))

(cl-defmethod package-recipe--upstream-url ((rcp package-recipe))
  (or (oref rcp url)
      (format (oref rcp url-format)
              (oref rcp repo))))

(cl-defmethod package-recipe--upstream-protocol ((rcp package-recipe))
  (let ((url (package-recipe--upstream-url rcp)))
    (cond ((string-match "\\`\\([a-z]+\\)://" url)
           (match-string 1 url))
          ((string-match "\\`[^:/ ]+:" url) "ssh")
          (t "file"))))

(cl-defmethod package-recipe--fetcher ((rcp package-recipe))
  (substring (symbol-name (eieio-object-class rcp)) 8 -7))

;;;; Git

(defclass package-git-recipe (package-recipe)
  ((time-regexp     :initform "\
\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \
[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\( [+-][0-9]\\{4\\}\\)?\\)")))

(defclass package-github-recipe (package-git-recipe)
  ((url-format      :initform "https://github.com/%s.git")
   (repopage-format :initform "https://github.com/%s")))

(defclass package-gitlab-recipe (package-git-recipe)
  ((url-format      :initform "https://gitlab.com/%s.git")
   (repopage-format :initform "https://gitlab.com/%s")))

;;;; Mercurial

(defclass package-hg-recipe (package-recipe)
  ((time-regexp     :initform "\
\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \
[0-9]\\{2\\}:[0-9]\\{2\\}\\( [+-][0-9]\\{4\\}\\)?\\)")))

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
          (apply (intern (format "package-%s-recipe" fetcher))
                 name :name name args))
      (error "No such recipe: %s" name))))

;;; Validation

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
        (if (memq fetcher '(github gitlab))
            (progn
              (cl-assert (plist-get plist :repo) ":repo is missing")
              (cl-assert (not (plist-get plist :url)) ":url is redundant"))
          (cl-assert (plist-get plist :url) ":url is missing")))
      (dolist (key symbol-keys)
        (let ((val (plist-get plist key)))
          (when val
            (cl-assert (symbolp val) nil "%s must be a symbol but is %S" key val))))
      (dolist (key list-keys)
        (let ((val (plist-get plist key)))
          (when val
            (cl-assert (listp val) nil "%s must be a list but is %S" key val))))
      (dolist (key string-keys)
        (let ((val (plist-get plist key)))
          (when val
            (cl-assert (stringp val) nil "%s must be a string but is %S" key val))))
      ;; Silence byte compiler of Emacs 28.  It appears that uses
      ;; inside cl-assert sometimes, but not always, do not count.
      (list name ident all-keys))
    recipe))

;;; _
(provide 'package-recipe)
;; Local Variables:
;; coding: utf-8
;; checkdoc-minor-mode: 1
;; indent-tabs-mode: nil
;; End:
;;; package-recipe.el ends here
