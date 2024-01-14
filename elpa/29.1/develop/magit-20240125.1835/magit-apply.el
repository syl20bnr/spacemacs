;;; magit-apply.el --- Apply Git diffs  -*- lexical-binding:t -*-

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

;; This library implements commands for applying Git diffs or parts
;; of such a diff.  The supported "apply variants" are apply, stage,
;; unstage, discard, and reverse - more than Git itself knows about,
;; at least at the porcelain level.

;;; Code:

(require 'magit-core)
(require 'magit-diff)
(require 'magit-wip)

(require 'transient) ; See #3732.

;; For `magit-apply'
(declare-function magit-am "magit-sequence" () t)
(declare-function magit-patch-apply "magit-patch" () t)
;; For `magit-discard-files'
(declare-function magit-checkout-stage "magit-merge" (file arg))
(declare-function magit-checkout-read-stage "magit-merge" (file))
(defvar auto-revert-verbose)
;; For `magit-stage-untracked'
(declare-function magit-submodule-add-1 "magit-submodule"
                  (url &optional path name args))
(declare-function magit-submodule-read-name-for-path "magit-submodule"
                  (path &optional prefer-short))
(defvar borg-user-emacs-directory)

(cl-eval-when (compile load)
  (when (< emacs-major-version 26)
    (defalias 'smerge-keep-upper 'smerge-keep-mine)
    (defalias 'smerge-keep-lower 'smerge-keep-other)))

;;; Options

(defcustom magit-delete-by-moving-to-trash t
  "Whether Magit uses the system's trash can.

You should absolutely not disable this and also remove `discard'
from `magit-no-confirm'.  You shouldn't do that even if you have
all of the Magit-Wip modes enabled, because those modes do not
track any files that are not tracked in the proper branch."
  :package-version '(magit . "2.1.0")
  :group 'magit-essentials
  :type 'boolean)

(defcustom magit-unstage-committed t
  "Whether unstaging a committed change reverts it instead.

A committed change cannot be unstaged, because staging and
unstaging are actions that are concerned with the differences
between the index and the working tree, not with committed
changes.

If this option is non-nil (the default), then typing \"u\"
\(`magit-unstage') on a committed change, causes it to be
reversed in the index but not the working tree.  For more
information see command `magit-reverse-in-index'."
  :package-version '(magit . "2.4.1")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-reverse-atomically nil
  "Whether to reverse changes atomically.

If some changes can be reversed while others cannot, then nothing
is reversed if the value of this option is non-nil.  But when it
is nil, then the changes that can be reversed are reversed and
for the other changes diff files are created that contain the
rejected reversals."
  :package-version '(magit . "2.7.0")
  :group 'magit-commands
  :type 'boolean)

(defcustom magit-post-stage-hook nil
  "Hook run after staging changes.
This hook is run by `magit-refresh' if `this-command'
is a member of `magit-post-stage-hook-commands'."
  :package-version '(magit . "2.90.0")
  :group 'magit-commands
  :type 'hook)

(defcustom magit-post-unstage-hook nil
  "Hook run after unstaging changes.
This hook is run by `magit-refresh' if `this-command'
is a member of `magit-post-unstage-hook-commands'."
  :package-version '(magit . "2.90.0")
  :group 'magit-commands
  :type 'hook)

;;; Commands
;;;; Apply

(defun magit-apply (&rest args)
  "Apply the change at point to the working tree.
With a prefix argument fallback to a 3-way merge.  Doing
so causes the change to be applied to the index as well."
  (interactive (and current-prefix-arg (list "--3way")))
  (when-let ((s (magit-apply--get-selection)))
    (pcase (list (magit-diff-type) (magit-diff-scope))
      (`(,(or 'unstaged 'staged) ,_)
       (user-error "Change is already in the working tree"))
      (`(untracked ,(or 'file 'files))
       (call-interactively #'magit-am))
      (`(,_ region) (magit-apply-region s args))
      (`(,_   hunk) (magit-apply-hunk   s args))
      (`(,_  hunks) (magit-apply-hunks  s args))
      (`(rebase-sequence file)
       (call-interactively #'magit-patch-apply))
      (`(,_   file) (magit-apply-diff   s args))
      (`(,_  files) (magit-apply-diffs  s args)))))

(defun magit-apply--section-content (section)
  (buffer-substring-no-properties (if (magit-hunk-section-p section)
                                      (oref section start)
                                    (oref section content))
                                  (oref section end)))

(defun magit-apply-diffs (sections &rest args)
  (setq sections (magit-apply--get-diffs sections))
  (magit-apply-patch sections args
                     (mapconcat
                      (lambda (s)
                        (concat (magit-diff-file-header s)
                                (magit-apply--section-content s)))
                      sections "")))

(defun magit-apply-diff (section &rest args)
  (setq section (car (magit-apply--get-diffs (list section))))
  (magit-apply-patch section args
                     (concat (magit-diff-file-header section)
                             (magit-apply--section-content section))))

(defun magit-apply--adjust-hunk-new-starts (hunks)
  "Adjust new line numbers in headers of HUNKS for partial application.
HUNKS should be a list of ordered, contiguous hunks to be applied
from a file.  For example, if there is a sequence of hunks with
the headers

  @@ -2,6 +2,7 @@
  @@ -10,6 +11,7 @@
  @@ -18,6 +20,7 @@

and only the second and third are to be applied, they would be
adjusted as \"@@ -10,6 +10,7 @@\" and \"@@ -18,6 +19,7 @@\"."
  (let* ((first-hunk (car hunks))
         (offset (if (string-match diff-hunk-header-re-unified first-hunk)
                     (- (string-to-number (match-string 3 first-hunk))
                        (string-to-number (match-string 1 first-hunk)))
                   (error "Header hunks have to be applied individually"))))
    (if (= offset 0)
        hunks
      (mapcar (lambda (hunk)
                (if (string-match diff-hunk-header-re-unified hunk)
                    (replace-match (number-to-string
                                    (- (string-to-number (match-string 3 hunk))
                                       offset))
                                   t t hunk 3)
                  (error "Hunk does not have expected header")))
              hunks))))

(defun magit-apply--adjust-hunk-new-start (hunk)
  (car (magit-apply--adjust-hunk-new-starts (list hunk))))

(defun magit-apply-hunks (hunks &rest args)
  (let ((file (oref (car hunks) parent)))
    (when (magit-diff--combined-p file)
      (user-error "Cannot un-/stage resolution hunks.  Stage the whole file"))
    (magit-apply-patch
     file args
     (concat (oref file header)
             (mapconcat #'identity
                        (magit-apply--adjust-hunk-new-starts
                         (mapcar #'magit-apply--section-content hunks))
                        "")))))

(defun magit-apply-hunk (hunk &rest args)
  (let ((file (oref hunk parent)))
    (when (magit-diff--combined-p file)
      (user-error "Cannot un-/stage resolution hunks.  Stage the whole file"))
    (let* ((header (car (oref hunk value)))
           (header (and (symbolp header) header))
           (content (magit-apply--section-content hunk)))
      (magit-apply-patch
       file args
       (concat (magit-diff-file-header hunk (not (eq header 'rename)))
               (if header
                   content
                 (magit-apply--adjust-hunk-new-start content)))))))

(defun magit-apply-region (hunk &rest args)
  (let ((file (oref hunk parent)))
    (when (magit-diff--combined-p file)
      (user-error "Cannot un-/stage resolution hunks.  Stage the whole file"))
    (magit-apply-patch
     file args
     (concat (magit-diff-file-header hunk)
             (magit-apply--adjust-hunk-new-start
              (magit-diff-hunk-region-patch hunk args))))))

(defun magit-apply-patch (section:s args patch)
  (let* ((files (if (atom section:s)
                    (list (oref section:s value))
                  (--map (oref it value) section:s)))
         (command (symbol-name this-command))
         (command (if (and command (string-match "^magit-\\([^-]+\\)" command))
                      (match-string 1 command)
                    "apply"))
         (ignore-context (magit-diff-ignore-any-space-p)))
    (unless (magit-diff-context-p)
      (user-error "Not enough context to apply patch.  Increase the context"))
    (when (and magit-wip-before-change-mode (not magit-inhibit-refresh))
      (magit-wip-commit-before-change files (concat " before " command)))
    (with-temp-buffer
      (insert patch)
      (magit-run-git-with-input
       "apply" args "-p0"
       (and ignore-context "-C0")
       "--ignore-space-change" "-"))
    (unless magit-inhibit-refresh
      (when magit-wip-after-apply-mode
        (magit-wip-commit-after-apply files (concat " after " command)))
      (magit-refresh))))

(defun magit-apply--get-selection ()
  (or (magit-region-sections '(hunk file module) t)
      (let ((section (magit-current-section)))
        (pcase (oref section type)
          ((or 'hunk 'file 'module) section)
          ((or 'staged 'unstaged 'untracked
               'stashed-index 'stashed-worktree 'stashed-untracked)
           (oref section children))
          (_ (user-error "Cannot apply this, it's not a change"))))))

(defun magit-apply--get-diffs (sections)
  (magit-section-case
    ([file diffstat]
     (--map (or (magit-get-section
                 (append `((file . ,(oref it value)))
                         (magit-section-ident magit-root-section)))
                (error "Cannot get required diff headers"))
            sections))
    (t sections)))

(defun magit-apply--ignore-whitespace-p (selection type scope)
  "Return t if it is necessary and possible to ignore whitespace.
It is necessary to do so when the diff ignores whitespace changes
and whole files are being applied.  It is possible when no binary
files are involved.  If it is both necessary and impossible, then
return nil, possibly causing whitespace changes to be applied."
  (and (memq type  '(unstaged staged))
       (memq scope '(file files list))
       (cl-find-if (lambda (arg)
                     (member arg '("--ignore-space-at-eol"
                                   "--ignore-space-change"
                                   "--ignore-all-space"
                                   "--ignore-blank-lines")))
                   magit-buffer-diff-args)
       (not (cl-find-if (lambda (section)
                          (oref section binary))
                        (ensure-list selection)))))

;;;; Stage

(defun magit-stage (&optional intent)
  "Add the change at point to the staging area.
With a prefix argument, INTENT, and an untracked file (or files)
at point, stage the file but not its content."
  (interactive "P")
  (if-let ((s (and (derived-mode-p 'magit-mode)
                   (magit-apply--get-selection)))
           (type (magit-diff-type))
           (scope (magit-diff-scope)))
      (pcase (list type scope
                   (magit-apply--ignore-whitespace-p s type scope))
        (`(untracked     ,_  ,_) (magit-stage-untracked intent))
        (`(unstaged  region  ,_) (magit-apply-region s "--cached"))
        (`(unstaged    hunk  ,_) (magit-apply-hunk   s "--cached"))
        (`(unstaged   hunks  ,_) (magit-apply-hunks  s "--cached"))
        ('(unstaged    file   t) (magit-apply-diff   s "--cached"))
        ('(unstaged   files   t) (magit-apply-diffs  s "--cached"))
        ('(unstaged    list   t) (magit-apply-diffs  s "--cached"))
        ('(unstaged    file nil) (magit-stage-1 "-u" (list (oref s value))))
        ('(unstaged   files nil) (magit-stage-1 "-u" (magit-region-values nil t)))
        ('(unstaged    list nil) (magit-stage-modified))
        (`(staged        ,_  ,_) (user-error "Already staged"))
        (`(committed     ,_  ,_) (user-error "Cannot stage committed changes"))
        (`(undefined     ,_  ,_) (user-error "Cannot stage this change")))
    (call-interactively #'magit-stage-file)))

;;;###autoload
(defun magit-stage-buffer-file ()
  "Stage all changes to the file being visited in the current buffer."
  (interactive)
  (unless buffer-file-name
    (user-error "Not visiting a file"))
  (magit-with-toplevel
    (magit-stage-1 (and (magit-file-ignored-p buffer-file-name)
                        (if (y-or-n-p "Visited file is ignored; stage anyway?")
                            "--force"
                          (user-error "Abort")))
                   (list (magit-file-relative-name)))))

;;;###autoload
(defun magit-stage-file (files &optional force)
  "Read one or more files and stage all changes in those files.
With prefix argument FORCE, offer ignored files for completion."
  (interactive
   (let* ((choices (if current-prefix-arg
                       (magit-ignored-files)
                     (nconc (magit-unstaged-files)
                            (magit-untracked-files))))
          (default (or (magit-section-value-if 'file)
                       (magit-file-relative-name)))
          (default (car (member default choices))))
     (list (magit-completing-read-multiple
            (if current-prefix-arg "Stage ignored file,s: " "Stage file,s: ")
            choices nil t nil nil default)
           current-prefix-arg)))
  (magit-with-toplevel
    ;; For backward compatibility, and because of
    ;; the function's name, don't require a list.
    (magit-stage-1 (and force "--force")
                   (if (listp files) files (list files)))))

;;;###autoload
(defun magit-stage-modified (&optional all)
  "Stage all changes to files modified in the worktree.
Stage all new content of tracked files and remove tracked files
that no longer exist in the working tree from the index also.
With a prefix argument also stage previously untracked (but not
ignored) files."
  (interactive "P")
  (when (magit-anything-staged-p)
    (magit-confirm 'stage-all-changes))
  (magit-with-toplevel
    (magit-stage-1 (if all "--all" "-u") magit-buffer-diff-files)))

(defun magit-stage-1 (arg &optional files)
  (magit-wip-commit-before-change files " before stage")
  (magit-run-git "add" arg (if files (cons "--" files) "."))
  (when magit-auto-revert-mode
    (mapc #'magit-turn-on-auto-revert-mode-if-desired files))
  (magit-wip-commit-after-apply files " after stage"))

(defun magit-stage-untracked (&optional intent)
  (let* ((section (magit-current-section))
         (files (pcase (magit-diff-scope)
                  ('file  (list (oref section value)))
                  ('files (magit-region-values nil t))
                  ('list  (magit-untracked-files))))
         plain repos)
    (dolist (file files)
      (if (and (not (file-symlink-p file))
               (magit-git-repo-p file t))
          (push file repos)
        (push file plain)))
    (magit-wip-commit-before-change files " before stage")
    (when plain
      (magit-run-git "add" (and intent "--intent-to-add")
                     "--" plain)
      (when magit-auto-revert-mode
        (mapc #'magit-turn-on-auto-revert-mode-if-desired plain)))
    (dolist (repo repos)
      (save-excursion
        (goto-char (oref (magit-get-section
                          `((file . ,repo) (untracked) (status)))
                         start))
        (when (and (fboundp 'borg-assimilate)
                   (fboundp 'borg--maybe-absorb-gitdir)
                   (fboundp 'borg--sort-submodule-sections))
          (let* ((topdir (magit-toplevel))
                 (url (let ((default-directory
                             (file-name-as-directory (expand-file-name repo))))
                        (or (magit-get "remote" (magit-get-some-remote) "url")
                            (concat (file-name-as-directory ".") repo))))
                 (package
                  (and (equal borg-user-emacs-directory topdir)
                       (file-name-nondirectory (directory-file-name repo)))))
            (if (and package
                     (y-or-n-p (format "Also assimilate `%s' drone?" package)))
                (borg-assimilate package url)
              (magit-submodule-add-1
               url repo (magit-submodule-read-name-for-path repo package))
              (when package
                (borg--sort-submodule-sections
                 (expand-file-name ".gitmodules" topdir))
                (let ((default-directory borg-user-emacs-directory))
                  (borg--maybe-absorb-gitdir package))))))))
    (magit-wip-commit-after-apply files " after stage")))

(defvar magit-post-stage-hook-commands
  '(magit-stage
    magit-stage-buffer-file
    magit-stage-file
    magit-stage-modified))

(defun magit-run-post-stage-hook ()
  (when (memq this-command magit-post-stage-hook-commands)
    (magit-run-hook-with-benchmark 'magit-post-stage-hook)))

;;;; Unstage

(defun magit-unstage ()
  "Remove the change at point from the staging area."
  (interactive)
  (when-let ((s (magit-apply--get-selection))
             (type (magit-diff-type))
             (scope (magit-diff-scope)))
    (pcase (list type scope
                 (magit-apply--ignore-whitespace-p s type scope))
      (`(untracked     ,_  ,_) (user-error "Cannot unstage untracked changes"))
      (`(unstaged    file  ,_) (magit-unstage-intent (list (oref s value))))
      (`(unstaged   files  ,_) (magit-unstage-intent (magit-region-values nil t)))
      (`(unstaged      ,_  ,_) (user-error "Already unstaged"))
      (`(staged    region  ,_) (magit-apply-region s "--reverse" "--cached"))
      (`(staged      hunk  ,_) (magit-apply-hunk   s "--reverse" "--cached"))
      (`(staged     hunks  ,_) (magit-apply-hunks  s "--reverse" "--cached"))
      ('(staged      file   t) (magit-apply-diff   s "--reverse" "--cached"))
      ('(staged     files   t) (magit-apply-diffs  s "--reverse" "--cached"))
      ('(staged      list   t) (magit-apply-diffs  s "--reverse" "--cached"))
      ('(staged      file nil) (magit-unstage-1 (list (oref s value))))
      ('(staged     files nil) (magit-unstage-1 (magit-region-values nil t)))
      ('(staged      list nil) (magit-unstage-all))
      (`(committed     ,_  ,_) (if magit-unstage-committed
                                   (magit-reverse-in-index)
                                 (user-error "Cannot unstage committed changes")))
      (`(undefined     ,_  ,_) (user-error "Cannot unstage this change")))))

;;;###autoload
(defun magit-unstage-buffer-file ()
  "Unstage all changes to the file being visited in the current buffer."
  (interactive)
  (unless buffer-file-name
    (user-error "Not visiting a file"))
  (magit-with-toplevel
    (magit-unstage-1 (list (magit-file-relative-name)))))

;;;###autoload
(defun magit-unstage-file (files)
  "Read one or more files and unstage all changes to those files."
  (interactive
   (let* ((choices (magit-staged-files))
          (default (or (magit-section-value-if 'file)
                       (magit-file-relative-name)))
          (default (car (member default choices))))
     (list (magit-completing-read-multiple "Unstage file,s: " choices
                                           nil t nil nil default))))
  (magit-with-toplevel
    ;; For backward compatibility, and because of
    ;; the function's name, don't require a list.
    (magit-unstage-1 (if (listp files) files (list files)))))

(defun magit-unstage-1 (files)
  (magit-wip-commit-before-change files " before unstage")
  (if (magit-no-commit-p)
      (magit-run-git "rm" "--cached" "--" files)
    (magit-run-git "reset" "HEAD" "--" files))
  (magit-wip-commit-after-apply files " after unstage"))

(defun magit-unstage-intent (files)
  (if-let ((staged (magit-staged-files))
           (intent (--filter (member it staged) files)))
      (magit-unstage-1 intent)
    (user-error "Already unstaged")))

;;;###autoload
(defun magit-unstage-all ()
  "Remove all changes from the staging area."
  (interactive)
  (unless (magit-anything-staged-p)
    (user-error "Nothing to unstage"))
  (when (or (magit-anything-unstaged-p)
            (magit-untracked-files))
    (magit-confirm 'unstage-all-changes))
  (magit-wip-commit-before-change nil " before unstage")
  (magit-run-git "reset" "HEAD" "--" magit-buffer-diff-files)
  (magit-wip-commit-after-apply nil " after unstage"))

(defvar magit-post-unstage-hook-commands
  '(magit-unstage
    magit-unstage-buffer-file
    magit-unstage-file
    magit-unstage-all))

(defun magit-run-post-unstage-hook ()
  (when (memq this-command magit-post-unstage-hook-commands)
    (magit-run-hook-with-benchmark 'magit-post-unstage-hook)))

;;;; Discard

(defun magit-discard ()
  "Remove the change at point.

On a hunk or file with unresolved conflicts prompt which side to
keep (while discarding the other).  If point is within the text
of a side, then keep that side without prompting."
  (interactive)
  (when-let ((s (magit-apply--get-selection)))
    (pcase (list (magit-diff-type) (magit-diff-scope))
      (`(committed ,_) (user-error "Cannot discard committed changes"))
      (`(undefined ,_) (user-error "Cannot discard this change"))
      (`(,_    region) (magit-discard-region s))
      (`(,_      hunk) (magit-discard-hunk   s))
      (`(,_     hunks) (magit-discard-hunks  s))
      (`(,_      file) (magit-discard-file   s))
      (`(,_     files) (magit-discard-files  s))
      (`(,_      list) (magit-discard-files  s)))))

(defun magit-discard-region (section)
  (magit-confirm 'discard "Discard region")
  (magit-discard-apply section 'magit-apply-region))

(defun magit-discard-hunk (section)
  (magit-confirm 'discard "Discard hunk")
  (let ((file (magit-section-parent-value section)))
    (pcase (cddr (car (magit-file-status file)))
      ('(?U ?U) (magit-smerge-keep-current))
      (_ (magit-discard-apply section #'magit-apply-hunk)))))

(defun magit-discard-apply (section apply)
  (if (eq (magit-diff-type section) 'unstaged)
      (funcall apply section "--reverse")
    (if (magit-anything-unstaged-p
         nil (if (magit-file-section-p section)
                 (oref section value)
               (magit-section-parent-value section)))
        (progn (let ((magit-inhibit-refresh t))
                 (funcall apply section "--reverse" "--cached")
                 (funcall apply section "--reverse" "--reject"))
               (magit-refresh))
      (funcall apply section "--reverse" "--index"))))

(defun magit-discard-hunks (sections)
  (magit-confirm 'discard (format "Discard %s hunks from %s"
                                  (length sections)
                                  (magit-section-parent-value (car sections))))
  (magit-discard-apply-n sections #'magit-apply-hunks))

(defun magit-discard-apply-n (sections apply)
  (let ((section (car sections)))
    (if (eq (magit-diff-type section) 'unstaged)
        (funcall apply sections "--reverse")
      (if (magit-anything-unstaged-p
           nil (if (magit-file-section-p section)
                   (oref section value)
                 (magit-section-parent-value section)))
          (progn (let ((magit-inhibit-refresh t))
                   (funcall apply sections "--reverse" "--cached")
                   (funcall apply sections "--reverse" "--reject"))
                 (magit-refresh))
        (funcall apply sections "--reverse" "--index")))))

(defun magit-discard-file (section)
  (magit-discard-files (list section)))

(defun magit-discard-files (sections)
  (let ((auto-revert-verbose nil)
        (type (magit-diff-type (car sections)))
        (status (magit-file-status))
        files delete resurrect rename discard discard-new resolve)
    (dolist (section sections)
      (let ((file (oref section value)))
        (push file files)
        (pcase (cons (pcase type
                       (`staged ?X)
                       (`unstaged ?Y)
                       (`untracked ?Z))
                     (cddr (assoc file status)))
          ('(?Z) (dolist (f (magit-untracked-files nil file))
                   (push f delete)))
          ((or '(?Z ?? ??) '(?Z ?! ?!)) (push file delete))
          ('(?Z ?D ? )                  (push file delete))
          (`(,_ ?D ?D)                  (push file resolve))
          ((or `(,_ ?U ,_) `(,_ ,_ ?U)) (push file resolve))
          (`(,_ ?A ?A)                  (push file resolve))
          (`(?X ?M ,(or ?  ?M ?D)) (push section discard))
          (`(?Y ,_         ?M    ) (push section discard))
          ('(?X ?A         ?M    ) (push file discard-new))
          ('(?X ?C         ?M    ) (push file discard-new))
          (`(?X ?A ,(or ?     ?D)) (push file delete))
          (`(?X ?C ,(or ?     ?D)) (push file delete))
          (`(?X ?D ,(or ?  ?M   )) (push file resurrect))
          (`(?Y ,_            ?D ) (push file resurrect))
          (`(?X ?R ,(or ?  ?M ?D)) (push file rename)))))
    (unwind-protect
        (let ((magit-inhibit-refresh t))
          (magit-wip-commit-before-change files " before discard")
          (when resolve
            (magit-discard-files--resolve (nreverse resolve)))
          (when resurrect
            (magit-discard-files--resurrect (nreverse resurrect)))
          (when delete
            (magit-discard-files--delete (nreverse delete) status))
          (when rename
            (magit-discard-files--rename (nreverse rename) status))
          (when (or discard discard-new)
            (magit-discard-files--discard (nreverse discard)
                                          (nreverse discard-new)))
          (magit-wip-commit-after-apply files " after discard"))
      (magit-refresh))))

(defun magit-discard-files--resolve (files)
  (if-let ((arg (and (cdr files)
                     (magit-read-char-case
                         (format "For these %d files\n%s\ncheckout:\n"
                                 (length files)
                                 (mapconcat (lambda (file)
                                              (concat "  " file))
                                            files "\n"))
                         t
                       (?o "[o]ur stage"   "--ours")
                       (?t "[t]heir stage" "--theirs")
                       (?c "[c]onflict"    "--merge")
                       (?i "decide [i]ndividually" nil)))))
      (dolist (file files)
        (magit-checkout-stage file arg))
    (dolist (file files)
      (magit-checkout-stage file (magit-checkout-read-stage file)))))

(defun magit-discard-files--resurrect (files)
  (magit-confirm-files 'resurrect files)
  (if (eq (magit-diff-type) 'staged)
      (magit-call-git "reset"  "--" files)
    (magit-call-git "checkout" "--" files)))

(defun magit-discard-files--delete (files status)
  (magit-confirm-files (if magit-delete-by-moving-to-trash 'trash 'delete)
                       files)
  (let ((delete-by-moving-to-trash magit-delete-by-moving-to-trash))
    (dolist (file files)
      (when (string-match-p "\\`\\\\?~" file)
        (error "Refusing to delete %S, too dangerous" file))
      (pcase (nth 3 (assoc file status))
        ((guard (memq (magit-diff-type) '(unstaged untracked)))
         (dired-delete-file file dired-recursive-deletes
                            magit-delete-by-moving-to-trash)
         (dired-clean-up-after-deletion file))
        (?\s (delete-file file t)
             (magit-call-git "rm" "--cached" "--" file))
        (?M  (let ((temp (magit-git-string "checkout-index" "--temp" file)))
               (string-match
                (format "\\(.+?\\)\t%s" (regexp-quote file)) temp)
               (rename-file (match-string 1 temp)
                            (setq temp (concat file ".~{index}~")))
               (delete-file temp t))
             (magit-call-git "rm" "--cached" "--force" "--" file))
        (?D  (magit-call-git "checkout" "--" file)
             (delete-file file t)
             (magit-call-git "rm" "--cached" "--force" "--" file))))))

(defun magit-discard-files--rename (files status)
  (magit-confirm 'rename "Undo rename %s" "Undo %d renames" nil
    (mapcar (lambda (file)
              (setq file (assoc file status))
              (format "%s -> %s" (cadr file) (car file)))
            files))
  (dolist (file files)
    (let ((orig (cadr (assoc file status))))
      (if (file-exists-p file)
          (progn
            (when-let ((path (file-name-directory orig)))
              (make-directory path t))
            (magit-call-git "mv" file orig))
        (magit-call-git "rm" "--cached" "--" file)
        (magit-call-git "reset" "--" orig)))))

(defun magit-discard-files--discard (sections new-files)
  (let ((files (--map (oref it value) sections)))
    (magit-confirm-files 'discard (append files new-files)
                         (format "Discard %s changes in" (magit-diff-type)))
    (if (eq (magit-diff-type (car sections)) 'unstaged)
        (magit-call-git "checkout" "--" files)
      (when new-files
        (magit-call-git "add"   "--" new-files)
        (magit-call-git "reset" "--" new-files))
      (let ((binaries (magit-binary-files "--cached")))
        (when binaries
          (setq sections
                (--remove (member (oref it value) binaries)
                          sections)))
        (cond ((length= sections 1)
               (magit-discard-apply (car sections) 'magit-apply-diff))
              (sections
               (magit-discard-apply-n sections #'magit-apply-diffs)))
        (when binaries
          (let ((modified (magit-unstaged-files t)))
            (setq binaries (--separate (member it modified) binaries)))
          (when (cadr binaries)
            (magit-call-git "reset" "--" (cadr binaries)))
          (when (car binaries)
            (user-error
             (concat
              "Cannot discard staged changes to binary files, "
              "which also have unstaged changes.  Unstage instead."))))))))

;;;; Reverse

(defun magit-reverse (&rest args)
  "Reverse the change at point in the working tree.
With a prefix argument fallback to a 3-way merge.  Doing
so causes the change to be applied to the index as well."
  (interactive (and current-prefix-arg (list "--3way")))
  (when-let ((s (magit-apply--get-selection)))
    (pcase (list (magit-diff-type) (magit-diff-scope))
      (`(untracked ,_) (user-error "Cannot reverse untracked changes"))
      (`(unstaged  ,_) (user-error "Cannot reverse unstaged changes"))
      (`(,_    region) (magit-reverse-region s args))
      (`(,_      hunk) (magit-reverse-hunk   s args))
      (`(,_     hunks) (magit-reverse-hunks  s args))
      (`(,_      file) (magit-reverse-file   s args))
      (`(,_     files) (magit-reverse-files  s args))
      (`(,_      list) (magit-reverse-files  s args)))))

(defun magit-reverse-region (section args)
  (magit-confirm 'reverse "Reverse region")
  (magit-reverse-apply section #'magit-apply-region args))

(defun magit-reverse-hunk (section args)
  (magit-confirm 'reverse "Reverse hunk")
  (magit-reverse-apply section #'magit-apply-hunk args))

(defun magit-reverse-hunks (sections args)
  (magit-confirm 'reverse
    (format "Reverse %s hunks from %s"
            (length sections)
            (magit-section-parent-value (car sections))))
  (magit-reverse-apply sections #'magit-apply-hunks args))

(defun magit-reverse-file (section args)
  (magit-reverse-files (list section) args))

(defun magit-reverse-files (sections args)
  (pcase-let ((`(,binaries ,sections)
               (let ((bs (magit-binary-files
                          (cond ((derived-mode-p 'magit-revision-mode)
                                 magit-buffer-range)
                                ((derived-mode-p 'magit-diff-mode)
                                 magit-buffer-range)
                                (t
                                 "--cached")))))
                 (--separate (member (oref it value) bs)
                             sections))))
    (magit-confirm-files 'reverse (--map (oref it value) sections))
    (cond ((length= sections 1)
           (magit-reverse-apply (car sections) #'magit-apply-diff args))
          (sections
           (magit-reverse-apply sections #'magit-apply-diffs args)))
    (when binaries
      (user-error "Cannot reverse binary files"))))

(defun magit-reverse-apply (section:s apply args)
  (funcall apply section:s "--reverse" args
           (and (not magit-reverse-atomically)
                (not (member "--3way" args))
                "--reject")))

(defun magit-reverse-in-index (&rest args)
  "Reverse the change at point in the index but not the working tree.

Use this command to extract a change from `HEAD', while leaving
it in the working tree, so that it can later be committed using
a separate commit.  A typical workflow would be:

0. Optionally make sure that there are no uncommitted changes.
1. Visit the `HEAD' commit and navigate to the change that should
   not have been included in that commit.
2. Type \"u\" (`magit-unstage') to reverse it in the index.
   This assumes that `magit-unstage-committed-changes' is non-nil.
3. Type \"c e\" to extend `HEAD' with the staged changes,
   including those that were already staged before.
4. Optionally stage the remaining changes using \"s\" or \"S\"
   and then type \"c c\" to create a new commit."
  (interactive)
  (magit-reverse (cons "--cached" args)))

;;; Smerge Support

(defun magit-smerge-keep-current ()
  "Keep the current version of the conflict at point."
  (interactive)
  (magit-call-smerge #'smerge-keep-current))

(defun magit-smerge-keep-upper ()
  "Keep the upper/our version of the conflict at point."
  (interactive)
  (magit-call-smerge #'smerge-keep-upper))

(defun magit-smerge-keep-base ()
  "Keep the base version of the conflict at point."
  (interactive)
  (magit-call-smerge #'smerge-keep-base))

(defun magit-smerge-keep-lower ()
  "Keep the lower/their version of the conflict at point."
  (interactive)
  (magit-call-smerge #'smerge-keep-lower))

(defun magit-smerge-keep-all ()
  "Keep all versions of the conflict at point."
  (interactive)
  (magit-call-smerge #'smerge-keep-all))

(defun magit-call-smerge (fn)
  (pcase-let* ((file (magit-file-at-point t t))
               (keep (get-file-buffer file))
               (`(,buf ,pos)
                (let ((magit-diff-visit-jump-to-change nil))
                  (magit-diff-visit-file--noselect file))))
    (with-current-buffer buf
      (save-excursion
        (save-restriction
          (unless (<= (point-min) pos (point-max))
            (widen))
          (goto-char pos)
          (condition-case nil
              (smerge-match-conflict)
            (error
             (if (eq fn #'smerge-keep-current)
                 (when (eq this-command #'magit-discard)
                   (re-search-forward smerge-begin-re nil t)
                   (setq fn
                         (magit-read-char-case "Keep side: " t
                           (?o "[o]urs/upper"   #'smerge-keep-upper)
                           (?b "[b]ase"         #'smerge-keep-base)
                           (?t "[t]heirs/lower" #'smerge-keep-lower))))
               (re-search-forward smerge-begin-re nil t))))
          (funcall fn)))
      (when (and keep (magit-anything-unmerged-p file))
        (smerge-start-session))
      (save-buffer))
    (unless keep
      (kill-buffer buf))
    (magit-refresh)))

;;; _
(provide 'magit-apply)
;;; magit-apply.el ends here
