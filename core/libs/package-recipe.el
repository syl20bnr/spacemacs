;;; package-recipe.el --- Package recipes as EIEIO objects  -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2018-2026 Jonas Bernoulli

;; Author: Jonas Bernoulli <emacs.package-build@jonas.bernoulli.dev>
;; Maintainer: Jonas Bernoulli <emacs.package-build@jonas.bernoulli.dev>
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
   (name            :initarg :name           :initform nil)
   (url             :initarg :url            :initform nil)
   (repo            :initarg :repo           :initform nil)
   (repopage        :initarg :repopage       :initform nil)
   (files           :initarg :files          :initform nil)
   (branch          :initarg :branch         :initform nil)
   (tag             :initarg :tag            :initform nil)
   (commit          :initarg :commit         :initform nil)
   (version-regexp  :initarg :version-regexp :initform nil)
   (shell-command   :initarg :shell-command  :initform nil)
   (make-targets    :initarg :make-targets   :initform nil)
   (org-exports     :initarg :org-exports    :initform nil)
   (old-names       :initarg :old-names      :initform nil)
   (version                                  :initform nil)
   (revdesc                                  :initform nil)
   (time                                     :initform nil)
   (summary                                  :initform nil)
   (dependencies                             :initform nil)
   (webpage                                  :initform nil)
   (keywords                                 :initform nil)
   (authors                                  :initform nil)
   (maintainers                              :initform nil)
   (tarballp                                 :initform t))
  :abstract t)

;;;; Git

(defclass package-git-recipe (package-recipe) ())

(defclass package-github-recipe (package-git-recipe)
  ((url-format      :initform "https://github.com/%s")
   (repopage-format :initform "https://github.com/%s")))

(defclass package-gitlab-recipe (package-git-recipe)
  ((url-format      :initform "https://gitlab.com/%s")
   (repopage-format :initform "https://gitlab.com/%s")))

(defclass package-codeberg-recipe (package-git-recipe)
  ((url-format      :initform "https://codeberg.org/%s")
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

(cl-defmethod package-recipe--upstream-protocol ((rcp package-recipe))
  (let ((url (oref rcp url)))
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

(defun package-recipe-read-name (&optional prompt)
  "Read the name of a package for which a recipe is available."
  (completing-read (or prompt "Package: ") (package-recipe-recipes)))

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
               key val args rcp)
          (package-recipe--validate recipe name)
          (while (setq key (pop plist))
            (setq val (pop plist))
            (unless (eq key :fetcher)
              (push val args)
              (push key args)))
          (when (and package-build-use-git-remote-hg (eq fetcher 'hg))
            (setq fetcher 'git-remote-hg)
            (setq args (plist-put args :url (concat "hg::" (oref rcp url)))))
          (setq rcp (apply (intern (format "package-%s-recipe" fetcher))
                           :name name args))
          (unless (oref rcp url)
            (oset rcp url (format (oref rcp url-format) (oref rcp repo))))
          rcp)
      (error "No such recipe: %s" name))))

;;; Validation

(define-error 'package-recipe-invalid "Invalid package recipe"
              'package-build-error)

;;;###autoload
(defun package-recipe-validate-all ()
  "Validate all package recipes.
Return a boolean indicating whether all recipes are valid and show
a message for each invalid recipe."
  (interactive)
  (let ((invalid 0)
        (errors 0))
    (dolist-with-progress-reporter (name (package-recipe-recipes))
        "Validating recipes..."
      (condition-case err
          (package-recipe-lookup name)
        (package-recipe-invalid
         (message "%s" (error-message-string err))
         (cl-incf invalid))
        (error
         (message "Error validating recipe: %s, %s" name
                  (error-message-string err))
         (cl-incf invalid)
         (cl-incf errors))))
    (cond ((= invalid 0)
           (message "All recipes are valid"))
          ((= errors 0)
           (message "%s recipe%s invalid"
                    invalid (if (= invalid 1) " is" "s are")))
          ((message "%s recipe%s invalid (%s error%s)"
                    invalid (if (= invalid 1) " is" "s are")
                    errors (if (= errors 1) "" "s"))))
    (= invalid 0)))

(defmacro package-recipe--assert (name form format-string &rest args)
  (declare (indent 1))
  `(unless ,form
     (signal 'package-recipe-invalid
             (list (let ((name ,name))
                     (if (stringp name) (intern-soft name) name))
                   (format-message ,format-string ,@args)))))

(defun package-recipe--validate (recipe name)
  "Perform some basic checks on the raw RECIPE for the package named NAME."
  (pcase-let ((`(,ident . ,plist) recipe))
    (package-recipe--assert name
      (and ident
           (symbolp ident)
           (not (keywordp ident)))
      "must begin with symbol, naming the package; not %S" ident)
    (package-recipe--assert name
      (string= (symbol-name ident) name)
      "mismatched package name %s vs. %s" name ident)
    (package-recipe--assert name plist "Recipe cannot be empty")
    (let* ((symbol-keys '(:fetcher))
           (string-keys '( :url :repo :branch :tag :commit
                           :version-regexp :shell-command))
           (list-keys '(:files :make-targets :org-exports :old-names))
           (all-keys (append symbol-keys string-keys list-keys)))
      (dolist (thing plist)
        (when (keywordp thing)
          (package-recipe--assert name
            (memq thing all-keys)
            "unknown keyword %S" thing)))
      (let ((fetcher (plist-get plist :fetcher)))
        (package-recipe--assert name fetcher ":fetcher is missing")
        (if (memq fetcher package-recipe--forge-fetchers)
            (progn
              (package-recipe--assert name
                (plist-get plist :repo)
                ":repo is missing")
              (package-recipe--assert name
                (not (plist-get plist :url))
                ":url is redundant"))
          (package-recipe--assert name
            (plist-get plist :url)
            ":url is missing")))
      (dolist (key symbol-keys)
        (when-let* ((val (plist-get plist key)))
          (package-recipe--assert name
            (symbolp val)
            "%s must be a symbol but is %S" key val)))
      (dolist (key list-keys)
        (when-let* ((val (plist-get plist key)))
          (package-recipe--assert name
            (listp val)
            "%s must be a list but is %S" key val)))
      (dolist (key string-keys)
        (when-let* ((val (plist-get plist key)))
          (package-recipe--assert name
            (stringp val)
            "%s must be a string but is %S" key val)))
      (when-let* ((spec (plist-get plist :files)))
        ;; `:defaults' is only allowed as the first element.
        ;; If we find it in that position, skip over it.
        (when (eq (car spec) :defaults)
          (setq spec (cdr spec)))
        ;; All other elements have to be strings or lists of strings.
        ;; Lists whose first element is `:exclude', `:inputs' or
        ;; `:rename' are also valid.
        (dolist (entry spec)
          (package-recipe--assert name
            (cond ((stringp entry)
                   (not (equal entry "*")))
                  ((listp entry)
                   (and-let* ((globs (cdr entry)))
                     (and (or (memq (car entry)
                                    '(:exclude :inputs :rename))
                              (stringp (car entry)))
                          (seq-every-p (lambda (glob)
                                         (and (stringp glob)
                                              (not (equal glob "*"))))
                                       globs)))))
            "invalid files spec entry %S" entry))))
    recipe))

(provide 'package-recipe)
;;; package-recipe.el ends here
