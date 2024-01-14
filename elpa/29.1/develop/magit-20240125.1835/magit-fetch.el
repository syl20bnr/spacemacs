;;; magit-fetch.el --- Download objects and refs  -*- lexical-binding:t -*-

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

;; This library implements fetch commands.

;;; Code:

(require 'magit)

;;; Commands

;;;###autoload (autoload 'magit-fetch "magit-fetch" nil t)
(transient-define-prefix magit-fetch ()
  "Fetch from another repository."
  :man-page "git-fetch"
  ["Arguments"
   ("-p" "Prune deleted branches" ("-p" "--prune"))
   ("-t" "Fetch all tags" ("-t" "--tags"))
   (7 "-u" "Fetch full history" "--unshallow")]
  ["Fetch from"
   ("p" magit-fetch-from-pushremote)
   ("u" magit-fetch-from-upstream)
   ("e" "elsewhere"        magit-fetch-other)
   ("a" "all remotes"      magit-fetch-all)]
  ["Fetch"
   ("o" "another branch"   magit-fetch-branch)
   ("r" "explicit refspec" magit-fetch-refspec)
   ("m" "submodules"       magit-fetch-modules)]
  ["Configure"
   ("C" "variables..." magit-branch-configure)])

(defun magit-fetch-arguments ()
  (transient-args 'magit-fetch))

(defun magit-git-fetch (remote args)
  (run-hooks 'magit-credential-hook)
  (magit-run-git-async "fetch" remote args))

;;;###autoload (autoload 'magit-fetch-from-pushremote "magit-fetch" nil t)
(transient-define-suffix magit-fetch-from-pushremote (args)
  "Fetch from the current push-remote.

With a prefix argument or when the push-remote is either not
configured or unusable, then let the user first configure the
push-remote."
  :description #'magit-fetch--pushremote-description
  (interactive (list (magit-fetch-arguments)))
  (let ((remote (magit-get-push-remote)))
    (when (or current-prefix-arg
              (not (member remote (magit-list-remotes))))
      (let ((var (magit--push-remote-variable)))
        (setq remote
              (magit-read-remote (format "Set %s and fetch from there" var)))
        (magit-set remote var)))
    (magit-git-fetch remote args)))

(defun magit-fetch--pushremote-description ()
  (let* ((branch (magit-get-current-branch))
         (remote (magit-get-push-remote branch))
         (v (magit--push-remote-variable branch t)))
    (cond
     ((member remote (magit-list-remotes)) remote)
     (remote
      (format "%s, replacing invalid" v))
     (t
      (format "%s, setting that" v)))))

;;;###autoload (autoload 'magit-fetch-from-upstream "magit-fetch" nil t)
(transient-define-suffix magit-fetch-from-upstream (remote args)
  "Fetch from the \"current\" remote, usually the upstream.

If the upstream is configured for the current branch and names
an existing remote, then use that.  Otherwise try to use another
remote: If only a single remote is configured, then use that.
Otherwise if a remote named \"origin\" exists, then use that.

If no remote can be determined, then this command is not available
from the `magit-fetch' transient prefix and invoking it directly
results in an error."
  :if          (lambda () (magit-get-current-remote t))
  :description (lambda () (magit-get-current-remote t))
  (interactive (list (magit-get-current-remote t)
                     (magit-fetch-arguments)))
  (unless remote
    (error "The \"current\" remote could not be determined"))
  (magit-git-fetch remote args))

;;;###autoload
(defun magit-fetch-other (remote args)
  "Fetch from another repository."
  (interactive (list (magit-read-remote "Fetch remote")
                     (magit-fetch-arguments)))
  (magit-git-fetch remote args))

;;;###autoload
(defun magit-fetch-branch (remote branch args)
  "Fetch a BRANCH from a REMOTE."
  (interactive
   (let ((remote (magit-read-remote-or-url "Fetch from remote or url")))
     (list remote
           (magit-read-remote-branch "Fetch branch" remote)
           (magit-fetch-arguments))))
  (magit-git-fetch remote (cons branch args)))

;;;###autoload
(defun magit-fetch-refspec (remote refspec args)
  "Fetch a REFSPEC from a REMOTE."
  (interactive
   (let ((remote (magit-read-remote-or-url "Fetch from remote or url")))
     (list remote
           (magit-read-refspec "Fetch using refspec" remote)
           (magit-fetch-arguments))))
  (magit-git-fetch remote (cons refspec args)))

;;;###autoload
(defun magit-fetch-all (args)
  "Fetch from all remotes."
  (interactive (list (magit-fetch-arguments)))
  (magit-git-fetch nil (cons "--all" args)))

;;;###autoload
(defun magit-fetch-all-prune ()
  "Fetch from all remotes, and prune.
Prune remote tracking branches for branches that have been
removed on the respective remote."
  (interactive)
  (run-hooks 'magit-credential-hook)
  (magit-run-git-async "remote" "update" "--prune"))

;;;###autoload
(defun magit-fetch-all-no-prune ()
  "Fetch from all remotes."
  (interactive)
  (run-hooks 'magit-credential-hook)
  (magit-run-git-async "remote" "update"))

;;;###autoload (autoload 'magit-fetch-modules "magit-fetch" nil t)
(transient-define-prefix magit-fetch-modules (&optional transient args)
  "Fetch all populated submodules.

Fetching is done using \"git fetch --recurse-submodules\", which
means that the super-repository and recursively all submodules
are also fetched.

To set and potentially save other arguments invoke this command
with a prefix argument."
  :man-page "git-fetch"
  :value (list "--verbose" "--jobs=4")
  ["Arguments"
   ("-v" "verbose"        "--verbose")
   ("-j" "number of jobs" "--jobs=" :reader transient-read-number-N+)]
  ["Action"
   ("m" "fetch modules" magit-fetch-modules)]
  (interactive (if current-prefix-arg
                   (list t)
                 (list nil (transient-args 'magit-fetch-modules))))
  (if transient
      (transient-setup 'magit-fetch-modules)
    (when (magit-git-version< "2.8.0")
      (when-let ((value (transient-arg-value "--jobs=" args)))
        (message "Dropping --jobs; not supported by Git v%s"
                 (magit-git-version))
        (setq args (remove (format "--jobs=%s" value) args))))
    (magit-with-toplevel
      (magit-run-git-async "fetch" "--recurse-submodules" args))))

;;; _
(provide 'magit-fetch)
;;; magit-fetch.el ends here
