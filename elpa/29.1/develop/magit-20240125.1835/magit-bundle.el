;;; magit-bundle.el --- Bundle support for Magit  -*- lexical-binding:t -*-

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

;;; Code:

(require 'magit)

;;; Commands

;;;###autoload (autoload 'magit-bundle "magit-bundle" nil t)
(transient-define-prefix magit-bundle ()
  "Create or verify Git bundles."
  :man-page "git-bundle"
  ["Actions"
   ("c" "create"     magit-bundle-create)
   ("v" "verify"     magit-bundle-verify)
   ("l" "list-heads" magit-bundle-list-heads)])

;;;###autoload (autoload 'magit-bundle-import "magit-bundle" nil t)
(transient-define-prefix magit-bundle-create (&optional file refs args)
  "Create a bundle."
  :man-page "git-bundle"
  ["Arguments"
   ("-a" "Include all refs" "--all")
   ("-b" "Include branches" "--branches=" :allow-empty t)
   ("-t" "Include tags"     "--tags="     :allow-empty t)
   ("-r" "Include remotes"  "--remotes="  :allow-empty t)
   ("-g" "Include refs"     "--glob=")
   ("-e" "Exclude refs"     "--exclude=")
   (magit-log:-n)
   (magit-log:--since)
   (magit-log:--until)]
  ["Actions"
   ("c" "create regular bundle" magit-bundle-create)
   ("t" "create tracked bundle" magit-bundle-create-tracked)
   ("u" "update tracked bundle" magit-bundle-update-tracked)]
  (interactive
   (and (eq transient-current-command 'magit-bundle-create)
        (list (read-file-name "Create bundle: " nil nil nil
                              (concat (file-name-nondirectory
                                       (directory-file-name (magit-toplevel)))
                                      ".bundle"))
              (magit-completing-read-multiple "Refnames (zero or more): "
                                              (magit-list-refnames))
              (transient-args 'magit-bundle-create))))
  (if file
      (magit-git-bundle "create" file refs args)
    (transient-setup 'magit-bundle-create)))

;;;###autoload
(defun magit-bundle-create-tracked (file tag branch refs args)
  "Create and track a new bundle."
  (interactive
   (let ((tag    (magit-read-tag "Track bundle using tag"))
         (branch (magit-read-branch "Bundle branch"))
         (refs   (magit-completing-read-multiple
                  "Additional refnames (zero or more): "
                  (magit-list-refnames))))
     (list (read-file-name "File: " nil nil nil (concat tag ".bundle"))
           tag branch
           (if (equal branch (magit-get-current-branch))
               (cons "HEAD" refs)
             refs)
           (transient-args 'magit-bundle-create))))
  (magit-git-bundle "create" file (cons branch refs) args)
  (magit-git "tag" "--force" tag branch
             "-m" (concat ";; git-bundle tracking\n"
                          (pp-to-string `((file   . ,file)
                                          (branch . ,branch)
                                          (refs   . ,refs)
                                          (args   . ,args))))))

;;;###autoload
(defun magit-bundle-update-tracked (tag)
  "Update a bundle that is being tracked using TAG."
  (interactive (list (magit-read-tag "Update bundle tracked by tag" t)))
  (let (msg)
    (let-alist (magit--with-temp-process-buffer
                 (save-excursion
                   (magit-git-insert "for-each-ref" "--format=%(contents)"
                                     (concat "refs/tags/" tag)))
                 (setq msg (buffer-string))
                 (ignore-errors (read (current-buffer))))
      (unless (and .file .branch)
        (error "Tag %s does not appear to track a bundle" tag))
      (magit-git-bundle "create" .file
                        (cons (concat tag ".." .branch) .refs)
                        .args)
      (magit-git "tag" "--force" tag .branch "-m" msg))))

;;;###autoload
(defun magit-bundle-verify (file)
  "Check whether FILE is valid and applies to the current repository."
  (interactive (list (magit-bundle--read-file-name "Verify bundle: ")))
  (magit-process-buffer)
  (magit-git-bundle "verify" file))

;;;###autoload
(defun magit-bundle-list-heads (file)
  "List the refs in FILE."
  (interactive (list (magit-bundle--read-file-name "List heads of bundle: ")))
  (magit-process-buffer)
  (magit-git-bundle "list-heads" file))

(defun magit-bundle--read-file-name (prompt)
  (read-file-name prompt nil nil t (magit-file-at-point) #'file-regular-p))

(defun magit-git-bundle (command file &optional refs args)
  (magit-git "bundle" command (magit-convert-filename-for-git file) refs args))

;;; _
(provide 'magit-bundle)
;;; magit-bundle.el ends here
