;;; magit-push.el --- Update remote objects and refs  -*- lexical-binding:t -*-

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

;; This library implements push commands.

;;; Code:

(require 'magit)

;;; Commands

;;;###autoload (autoload 'magit-push "magit-push" nil t)
(transient-define-prefix magit-push ()
  "Push to another repository."
  :man-page "git-push"
  ["Arguments"
   ("-f" "Force with lease" (nil "--force-with-lease"))
   ("-F" "Force"            ("-f" "--force"))
   ("-h" "Disable hooks"    "--no-verify")
   ("-n" "Dry run"          ("-n" "--dry-run"))
   (5 "-u" "Set upstream"   "--set-upstream")
   (7 "-t" "Follow tags"    "--follow-tags")]
  [:if magit-get-current-branch
   :description (lambda ()
                  (format (propertize "Push %s to" 'face 'transient-heading)
                          (propertize (magit-get-current-branch)
                                      'face 'magit-branch-local)))
   ("p" magit-push-current-to-pushremote)
   ("u" magit-push-current-to-upstream)
   ("e" "elsewhere" magit-push-current)]
  ["Push"
   [("o" "another branch"    magit-push-other)
    ("r" "explicit refspecs" magit-push-refspecs)
    ("m" "matching branches" magit-push-matching)]
   [("T" "a tag"             magit-push-tag)
    ("t" "all tags"          magit-push-tags)
    (6 "n" "a note ref"      magit-push-notes-ref)]]
  ["Configure"
   ("C" "Set variables..."  magit-branch-configure)])

(defun magit-push-arguments ()
  (transient-args 'magit-push))

(defun magit-git-push (branch target args)
  (run-hooks 'magit-credential-hook)
  ;; If the remote branch already exists, then we do not have to
  ;; qualify the target, which we prefer to avoid doing because
  ;; using the default namespace is wrong in obscure cases.
  (pcase-let ((namespace (if (magit-get-tracked target) "" "refs/heads/"))
              (`(,remote . ,target)
               (magit-split-branch-name target)))
    (magit-run-git-async "push" "-v" args remote
                         (format "%s:%s%s" branch namespace target))))

;;;###autoload (autoload 'magit-push-current-to-pushremote "magit-push" nil t)
(transient-define-suffix magit-push-current-to-pushremote (args)
  "Push the current branch to its push-remote.

When the push-remote is not configured, then read the push-remote
from the user, set it, and then push to it.  With a prefix
argument the push-remote can be changed before pushed to it."
  :if #'magit-get-current-branch
  :description #'magit-push--pushbranch-description
  (interactive (list (magit-push-arguments)))
  (pcase-let ((`(,branch ,remote ,changed)
               (magit--select-push-remote "push there")))
    (when changed
      (magit-confirm 'set-and-push
        (string-replace
         "%" "%%"
         (format "Really use \"%s\" as push-remote and push \"%s\" there"
                 remote branch))))
    (run-hooks 'magit-credential-hook)
    (magit-run-git-async "push" "-v" args remote
                         (format "refs/heads/%s:refs/heads/%s"
                                 branch branch)))) ; see #3847 and #3872

(defun magit-push--pushbranch-description ()
  (let* ((branch (magit-get-current-branch))
         (target (magit-get-push-branch branch t))
         (remote (magit-get-push-remote branch))
         (v (magit--push-remote-variable branch t)))
    (cond
     (target)
     ((member remote (magit-list-remotes))
      (format "%s, creating it"
              (magit--propertize-face (concat remote "/" branch)
                                      'magit-branch-remote)))
     (remote
      (format "%s, replacing invalid" v))
     (t
      (format "%s, setting that" v)))))

;;;###autoload (autoload 'magit-push-current-to-upstream "magit-push" nil t)
(transient-define-suffix magit-push-current-to-upstream (args)
  "Push the current branch to its upstream branch.

With a prefix argument or when the upstream is either not
configured or unusable, then let the user first configure
the upstream."
  :if #'magit-get-current-branch
  :description #'magit-push--upstream-description
  (interactive (list (magit-push-arguments)))
  (let* ((branch (or (magit-get-current-branch)
                     (user-error "No branch is checked out")))
         (remote (magit-get "branch" branch "remote"))
         (merge  (magit-get "branch" branch "merge")))
    (when (or current-prefix-arg
              (not (or (magit-get-upstream-branch branch)
                       (magit--unnamed-upstream-p remote merge)
                       (magit--valid-upstream-p remote merge))))
      (let* ((branches (cl-union (--map (concat it "/" branch)
                                        (magit-list-remotes))
                                 (magit-list-remote-branch-names)
                                 :test #'equal))
             (upstream (magit-completing-read
                        (format "Set upstream of %s and push there" branch)
                        branches nil nil nil 'magit-revision-history
                        (or (car (member (magit-remote-branch-at-point) branches))
                            (car (member "origin/master" branches)))))
             (upstream* (or (magit-get-tracked upstream)
                            (magit-split-branch-name upstream))))
        (setq remote (car upstream*))
        (setq merge  (cdr upstream*))
        (unless (string-prefix-p "refs/" merge)
          ;; User selected a non-existent remote-tracking branch.
          ;; It is very likely, but not certain, that this is the
          ;; correct thing to do.  It is even more likely that it
          ;; is what the user wants to happen.
          (setq merge (concat "refs/heads/" merge)))
        (magit-confirm 'set-and-push
          (string-replace
           "%" "%%"
           (format "Really use \"%s\" as upstream and push \"%s\" there"
                   upstream branch))))
      (cl-pushnew "--set-upstream" args :test #'equal))
    (run-hooks 'magit-credential-hook)
    (magit-run-git-async "push" "-v" args remote (concat branch ":" merge))))

(defun magit-push--upstream-description ()
  (and-let* ((branch (magit-get-current-branch)))
    (or (magit-get-upstream-branch branch)
        (let ((remote (magit-get "branch" branch "remote"))
              (merge  (magit-get "branch" branch "merge"))
              (u (magit--propertize-face "@{upstream}" 'bold)))
          (cond
           ((magit--unnamed-upstream-p remote merge)
            (format "%s as %s"
                    (magit--propertize-face remote 'bold)
                    (magit--propertize-face merge 'magit-branch-remote)))
           ((magit--valid-upstream-p remote merge)
            (format "%s creating %s"
                    (magit--propertize-face remote 'magit-branch-remote)
                    (magit--propertize-face merge 'magit-branch-remote)))
           ((or remote merge)
            (concat u ", creating it and replacing invalid"))
           (t
            (concat u ", creating it")))))))

;;;###autoload
(defun magit-push-current (target args)
  "Push the current branch to a branch read in the minibuffer."
  (interactive
   (if-let ((current (magit-get-current-branch)))
       (list (magit-read-remote-branch (format "Push %s to" current)
                                       nil nil current 'confirm)
             (magit-push-arguments))
     (user-error "No branch is checked out")))
  (magit-git-push (magit-get-current-branch) target args))

;;;###autoload
(defun magit-push-other (source target args)
  "Push an arbitrary branch or commit somewhere.
Both the source and the target are read in the minibuffer."
  (interactive
   (let ((source (magit-read-local-branch-or-commit "Push")))
     (list source
           (magit-read-remote-branch
            (format "Push %s to" source) nil
            (if (magit-local-branch-p source)
                (or (magit-get-push-branch source)
                    (magit-get-upstream-branch source))
              (and (magit-rev-ancestor-p source "HEAD")
                   (or (magit-get-push-branch)
                       (magit-get-upstream-branch))))
            source 'confirm)
           (magit-push-arguments))))
  (magit-git-push source target args))

(defvar magit-push-refspecs-history nil)

;;;###autoload
(defun magit-push-refspecs (remote refspecs args)
  "Push one or multiple REFSPECS to a REMOTE.
Both the REMOTE and the REFSPECS are read in the minibuffer.  To
use multiple REFSPECS, separate them with commas.  Completion is
only available for the part before the colon, or when no colon
is used."
  (interactive
   (list (magit-read-remote "Push to remote")
         (magit-completing-read-multiple
          "Push refspec,s: "
          (cons "HEAD" (magit-list-local-branch-names))
          nil nil nil 'magit-push-refspecs-history)
         (magit-push-arguments)))
  (run-hooks 'magit-credential-hook)
  (magit-run-git-async "push" "-v" args remote refspecs))

;;;###autoload
(defun magit-push-matching (remote &optional args)
  "Push all matching branches to another repository.
If multiple remotes exist, then read one from the user.
If just one exists, use that without requiring confirmation."
  (interactive (list (magit-read-remote "Push matching branches to" nil t)
                     (magit-push-arguments)))
  (run-hooks 'magit-credential-hook)
  (magit-run-git-async "push" "-v" args remote ":"))

;;;###autoload
(defun magit-push-tags (remote &optional args)
  "Push all tags to another repository.
If only one remote exists, then push to that.  Otherwise prompt
for a remote, offering the remote configured for the current
branch as default."
  (interactive (list (magit-read-remote "Push tags to remote" nil t)
                     (magit-push-arguments)))
  (run-hooks 'magit-credential-hook)
  (magit-run-git-async "push" remote "--tags" args))

;;;###autoload
(defun magit-push-tag (tag remote &optional args)
  "Push a tag to another repository."
  (interactive
   (let  ((tag (magit-read-tag "Push tag")))
     (list tag (magit-read-remote (format "Push %s to remote" tag) nil t)
           (magit-push-arguments))))
  (run-hooks 'magit-credential-hook)
  (magit-run-git-async "push" remote tag args))

;;;###autoload
(defun magit-push-notes-ref (ref remote &optional args)
  "Push a notes ref to another repository."
  (interactive
   (let ((note (magit-notes-read-ref "Push notes" nil nil)))
     (list note
           (magit-read-remote (format "Push %s to remote" note) nil t)
           (magit-push-arguments))))
  (run-hooks 'magit-credential-hook)
  (magit-run-git-async "push" remote ref args))

;;;###autoload (autoload 'magit-push-implicitly "magit-push" nil t)
(transient-define-suffix magit-push-implicitly (args)
  "Push somewhere without using an explicit refspec.

This command simply runs \"git push -v [ARGS]\".  ARGS are the
arguments specified in the popup buffer.  No explicit refspec
arguments are used.  Instead the behavior depends on at least
these Git variables: `push.default', `remote.pushDefault',
`branch.<branch>.pushRemote', `branch.<branch>.remote',
`branch.<branch>.merge', and `remote.<remote>.push'.

If you add this suffix to a transient prefix without explicitly
specifying the description, then an attempt is made to predict
what this command will do.  To add it use something like:

  (transient-insert-suffix \\='magit-push \"o\"
    \\='(\"i\" magit-push-implicitly))"
  :description #'magit-push-implicitly--desc
  (interactive (list (magit-push-arguments)))
  (run-hooks 'magit-credential-hook)
  (magit-run-git-async "push" "-v" args))

(defun magit-push-implicitly--desc ()
  ;; This implements the logic for git push as documented.
  ;; First, we resolve a remote to use based on various remote and
  ;; pushRemote options.
  ;; Then, we resolve the refspec to use for the remote based on push
  ;; and pushDefault options.
  ;; Note that the remote and refspec to push are handled separately,
  ;; so it doesn't make sense to talk about "pushing to upstream".
  ;; Depending on the options, you could end up pushing to the
  ;; "upstream" remote but not the "upstream" branch, and vice versa.
  (let* ((branch (magit-get-current-branch))
         (remote (or (magit-get-push-remote branch)
                     ;; Note: Avoid `magit-get-remote' because it
                     ;; filters out the local repo case (".").
                     (magit-get "branch" branch "remote")
                     (let ((remotes (magit-list-remotes)))
                       (cond
                        ((and (magit-git-version>= "2.27")
                              (= (length remotes) 1))
                         (car remotes))
                        ((member "origin" remotes) "origin"))))))
    (if (null remote)
        "nothing (no remote)"
      (let ((refspec (magit-get "remote" remote "push")))
        (if refspec
            (format "to %s with refspecs %s"
                    (magit--propertize-face remote 'bold)
                    (magit--propertize-face refspec 'bold))
          (pcase (or (magit-get "push.default") "simple")
            ("nothing" "nothing (due to push.default)")
            ((or "current" "simple")
             (format "%s to %s"
                     (magit--propertize-face branch 'magit-branch-current)
                     (magit--propertize-face (format "%s/%s" remote branch)
                                             'magit-branch-remote)))
            ((or "upstream" "tracking")
             (let ((ref (magit-get "branch" branch "merge")))
               (if ref
                   (format "%s to %s"
                           (magit--propertize-face branch 'magit-branch-current)
                           (cond
                            ((string-prefix-p "refs/heads/" ref)
                             (magit--propertize-face
                              (format "%s/%s" remote
                                      (substring ref (length "refs/heads/")))
                              'magit-branch-remote))
                            ((not (string-match "/" ref))
                             (magit--propertize-face (format "%s/%s" remote ref)
                                                     'magit-branch-remote))
                            (t (format "%s as %s"
                                       (magit--propertize-face remote 'bold)
                                       (magit--propertize-face ref 'bold)))))
                 "nothing (no upstream)")))
            ("matching" (format "all matching to %s"
                                (magit--propertize-face remote 'bold)))))))))

;;;###autoload (autoload 'magit-push-to-remote "magit-push" nil t)
(transient-define-suffix magit-push-to-remote (remote args)
  "Push to REMOTE without using an explicit refspec.
The REMOTE is read in the minibuffer.

This command simply runs \"git push -v [ARGS] REMOTE\".  ARGS
are the arguments specified in the popup buffer.  No refspec
arguments are used.  Instead the behavior depends on at least
these Git variables: `push.default', `remote.pushDefault',
`branch.<branch>.pushRemote', `branch.<branch>.remote',
`branch.<branch>.merge', and `remote.<remote>.push'.

You can add this command as a suffix using something like:

  (transient-insert-suffix \\='magit-push \"o\"
    \\='(\"x\" magit-push-to-remote))"
  :description #'magit-push-to-remote--desc
  (interactive (list (magit-read-remote "Push to remote")
                     (magit-push-arguments)))
  (run-hooks 'magit-credential-hook)
  (magit-run-git-async "push" "-v" args remote))

(defun magit-push-to-remote--desc ()
  (format "using %s" (magit--propertize-face "git push <remote>" 'bold)))

;;; _
(provide 'magit-push)
;;; magit-push.el ends here
