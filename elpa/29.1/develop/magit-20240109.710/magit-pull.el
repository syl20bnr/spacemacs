;;; magit-pull.el --- Update local objects and refs  -*- lexical-binding:t -*-

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

;; This library implements pull commands.

;;; Code:

(require 'magit)

;;; Options

(defcustom magit-pull-or-fetch nil
  "Whether `magit-pull' also offers some fetch suffixes."
  :package-version '(magit . "3.0.0")
  :group 'magit-commands
  :type 'boolean)

;;; Commands

;;;###autoload (autoload 'magit-pull "magit-pull" nil t)
(transient-define-prefix magit-pull ()
  "Pull from another repository."
  :man-page "git-pull"
  :incompatible '(("--ff-only" "--rebase"))
  [:description
   (lambda () (if magit-pull-or-fetch "Pull arguments" "Arguments"))
   ("-f" "Fast-forward only" "--ff-only")
   ("-r" "Rebase local commits" ("-r" "--rebase"))
   ("-A" "Autostash" "--autostash" :level 7)]
  [:description
   (lambda ()
     (if-let ((branch (magit-get-current-branch)))
         (concat
          (propertize "Pull into " 'face 'transient-heading)
          (propertize branch       'face 'magit-branch-local)
          (propertize " from"      'face 'transient-heading))
       (propertize "Pull from" 'face 'transient-heading)))
   ("p" magit-pull-from-pushremote)
   ("u" magit-pull-from-upstream)
   ("e" "elsewhere"         magit-pull-branch)]
  ["Fetch from"
   :if-non-nil magit-pull-or-fetch
   ("f" "remotes"           magit-fetch-all-no-prune)
   ("F" "remotes and prune" magit-fetch-all-prune)]
  ["Fetch"
   :if-non-nil magit-pull-or-fetch
   ("o" "another branch"    magit-fetch-branch)
   ("s" "explicit refspec"  magit-fetch-refspec)
   ("m" "submodules"        magit-fetch-modules)]
  ["Configure"
   ("r" magit-branch.<branch>.rebase :if magit-get-current-branch)
   ("C" "variables..." magit-branch-configure)]
  (interactive)
  (transient-setup 'magit-pull nil nil :scope (magit-get-current-branch)))

(defun magit-pull-arguments ()
  (transient-args 'magit-pull))

;;;###autoload (autoload 'magit-pull-from-pushremote "magit-pull" nil t)
(transient-define-suffix magit-pull-from-pushremote (args)
  "Pull from the push-remote of the current branch.

With a prefix argument or when the push-remote is either not
configured or unusable, then let the user first configure the
push-remote."
  :if #'magit-get-current-branch
  :description #'magit-pull--pushbranch-description
  (interactive (list (magit-pull-arguments)))
  (pcase-let ((`(,branch ,remote)
               (magit--select-push-remote "pull from there")))
    (run-hooks 'magit-credential-hook)
    (magit-run-git-with-editor "pull" args remote branch)))

(defun magit-pull--pushbranch-description ()
  ;; Also used by `magit-rebase-onto-pushremote'.
  (let* ((branch (magit-get-current-branch))
         (target (magit-get-push-branch branch t))
         (remote (magit-get-push-remote branch))
         (v (magit--push-remote-variable branch t)))
    (cond
     (target)
     ((member remote (magit-list-remotes))
      (format "%s, replacing non-existent" v))
     (remote
      (format "%s, replacing invalid" v))
     (t
      (format "%s, setting that" v)))))

;;;###autoload (autoload 'magit-pull-from-upstream "magit-pull" nil t)
(transient-define-suffix magit-pull-from-upstream (args)
  "Pull from the upstream of the current branch.

With a prefix argument or when the upstream is either not
configured or unusable, then let the user first configure
the upstream."
  :if #'magit-get-current-branch
  :description #'magit-pull--upstream-description
  (interactive (list (magit-pull-arguments)))
  (let* ((branch (or (magit-get-current-branch)
                     (user-error "No branch is checked out")))
         (remote (magit-get "branch" branch "remote"))
         (merge  (magit-get "branch" branch "merge")))
    (when (or current-prefix-arg
              (not (or (magit-get-upstream-branch branch)
                       (magit--unnamed-upstream-p remote merge))))
      (magit-set-upstream-branch
       branch (magit-read-upstream-branch
               branch (format "Set upstream of %s and pull from there" branch)))
      (setq remote (magit-get "branch" branch "remote"))
      (setq merge  (magit-get "branch" branch "merge")))
    (run-hooks 'magit-credential-hook)
    (magit-run-git-with-editor "pull" args remote merge)))

(defun magit-pull--upstream-description ()
  (and-let* ((branch (magit-get-current-branch)))
    (or (magit-get-upstream-branch branch)
        (let ((remote (magit-get "branch" branch "remote"))
              (merge  (magit-get "branch" branch "merge"))
              (u (magit--propertize-face "@{upstream}" 'bold)))
          (cond
           ((magit--unnamed-upstream-p remote merge)
            (format "%s of %s"
                    (magit--propertize-face merge 'magit-branch-remote)
                    (magit--propertize-face remote 'bold)))
           ((magit--valid-upstream-p remote merge)
            (concat u ", replacing non-existent"))
           ((or remote merge)
            (concat u ", replacing invalid"))
           (t
            (concat u ", setting that")))))))

;;;###autoload
(defun magit-pull-branch (source args)
  "Pull from a branch read in the minibuffer."
  (interactive (list (magit-read-remote-branch "Pull" nil nil nil t)
                     (magit-pull-arguments)))
  (run-hooks 'magit-credential-hook)
  (pcase-let ((`(,remote . ,branch)
               (magit-get-tracked source)))
    (magit-run-git-with-editor "pull" args remote branch)))

;;; _
(provide 'magit-pull)
;;; magit-pull.el ends here
