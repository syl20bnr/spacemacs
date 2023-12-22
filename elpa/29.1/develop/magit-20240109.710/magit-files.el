;;; magit-files.el --- Finding files  -*- lexical-binding:t -*-

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

;; This library implements support for finding blobs, staged files,
;; and Git configuration files.  It also implements modes useful in
;; buffers visiting files and blobs, and the commands used by those
;; modes.

;;; Code:

(require 'magit)

;;; Find Blob

(defvar magit-find-file-hook nil)
(add-hook 'magit-find-file-hook #'magit-blob-mode)

;;;###autoload
(defun magit-find-file (rev file)
  "View FILE from REV.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go
to the line and column corresponding to that location."
  (interactive (magit-find-file-read-args "Find file"))
  (magit-find-file--internal rev file #'pop-to-buffer-same-window))

;;;###autoload
(defun magit-find-file-other-window (rev file)
  "View FILE from REV, in another window.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go to
the line and column corresponding to that location."
  (interactive (magit-find-file-read-args "Find file in other window"))
  (magit-find-file--internal rev file #'switch-to-buffer-other-window))

;;;###autoload
(defun magit-find-file-other-frame (rev file)
  "View FILE from REV, in another frame.
Switch to a buffer visiting blob REV:FILE, creating one if none
already exists.  If prior to calling this command the current
buffer and/or cursor position is about the same file, then go to
the line and column corresponding to that location."
  (interactive (magit-find-file-read-args "Find file in other frame"))
  (magit-find-file--internal rev file #'switch-to-buffer-other-frame))

(defun magit-find-file-read-args (prompt)
  (let ((pseudo-revs '("{worktree}" "{index}")))
    (if-let ((rev (magit-completing-read "Find file from revision"
                                         (append pseudo-revs
                                                 (magit-list-refnames nil t))
                                         nil nil nil 'magit-revision-history
                                         (or (magit-branch-or-commit-at-point)
                                             (magit-get-current-branch)))))
        (list rev (magit-read-file-from-rev (if (member rev pseudo-revs)
                                                "HEAD"
                                              rev)
                                            prompt))
      (user-error "Nothing selected"))))

(defun magit-find-file--internal (rev file fn)
  (let ((buf (magit-find-file-noselect rev file))
        line col)
    (when-let ((visited-file (magit-file-relative-name)))
      (setq line (line-number-at-pos))
      (setq col (current-column))
      (cond
       ((not (equal visited-file file)))
       ((equal magit-buffer-revision rev))
       ((equal rev "{worktree}")
        (setq line (magit-diff-visit--offset file magit-buffer-revision line)))
       ((equal rev "{index}")
        (setq line (magit-diff-visit--offset file nil line)))
       (magit-buffer-revision
        (setq line (magit-diff-visit--offset
                    file (concat magit-buffer-revision ".." rev) line)))
       (t
        (setq line (magit-diff-visit--offset file (list "-R" rev) line)))))
    (funcall fn buf)
    (when line
      (with-current-buffer buf
        (widen)
        (goto-char (point-min))
        (forward-line (1- line))
        (move-to-column col)))
    buf))

(defun magit-find-file-noselect (rev file)
  "Read FILE from REV into a buffer and return the buffer.
REV is a revision or one of \"{worktree}\" or \"{index}\".
FILE must be relative to the top directory of the repository."
  (magit-find-file-noselect-1 rev file))

(defun magit-find-file-noselect-1 (rev file &optional revert)
  "Read FILE from REV into a buffer and return the buffer.
REV is a revision or one of \"{worktree}\" or \"{index}\".
FILE must be relative to the top directory of the repository.
Non-nil REVERT means to revert the buffer.  If `ask-revert',
then only after asking.  A non-nil value for REVERT is ignored if REV is
\"{worktree}\"."
  (if (equal rev "{worktree}")
      (find-file-noselect (expand-file-name file (magit-toplevel)))
    (let ((topdir (magit-toplevel)))
      (when (file-name-absolute-p file)
        (setq file (file-relative-name file topdir)))
      (with-current-buffer (magit-get-revision-buffer-create rev file)
        (when (or (not magit-buffer-file-name)
                  (if (eq revert 'ask-revert)
                      (y-or-n-p (format "%s already exists; revert it? "
                                        (buffer-name))))
                  revert)
          (setq magit-buffer-revision
                (if (equal rev "{index}")
                    "{index}"
                  (magit-rev-format "%H" rev)))
          (setq magit-buffer-refname rev)
          (setq magit-buffer-file-name (expand-file-name file topdir))
          (setq default-directory
                (let ((dir (file-name-directory magit-buffer-file-name)))
                  (if (file-exists-p dir) dir topdir)))
          (setq-local revert-buffer-function #'magit-revert-rev-file-buffer)
          (revert-buffer t t)
          (run-hooks (if (equal rev "{index}")
                         'magit-find-index-hook
                       'magit-find-file-hook)))
        (current-buffer)))))

(defun magit-get-revision-buffer-create (rev file)
  (magit-get-revision-buffer rev file t))

(defun magit-get-revision-buffer (rev file &optional create)
  (funcall (if create #'get-buffer-create #'get-buffer)
           (format "%s.~%s~" file (subst-char-in-string ?/ ?_ rev))))

(defun magit-revert-rev-file-buffer (_ignore-auto noconfirm)
  (when (or noconfirm
            (and (not (buffer-modified-p))
                 (catch 'found
                   (dolist (regexp revert-without-query)
                     (when (string-match regexp magit-buffer-file-name)
                       (throw 'found t)))))
            (yes-or-no-p (format "Revert buffer from Git %s? "
                                 (if (equal magit-buffer-refname "{index}")
                                     "index"
                                   (concat "revision " magit-buffer-refname)))))
    (let* ((inhibit-read-only t)
           (default-directory (magit-toplevel))
           (file (file-relative-name magit-buffer-file-name))
           (coding-system-for-read (or coding-system-for-read 'undecided)))
      (erase-buffer)
      (magit-git-insert "cat-file" "-p"
                        (if (equal magit-buffer-refname "{index}")
                            (concat ":" file)
                          (concat magit-buffer-refname ":" file)))
      (setq buffer-file-coding-system last-coding-system-used))
    (let ((buffer-file-name magit-buffer-file-name)
          (after-change-major-mode-hook
           (remq 'global-diff-hl-mode-enable-in-buffers
                 after-change-major-mode-hook)))
      (normal-mode t))
    (setq buffer-read-only t)
    (set-buffer-modified-p nil)
    (goto-char (point-min))))

(defun magit--lsp--disable-when-visiting-blob (fn &rest args)
  "Do nothing when visiting blob using `magit-find-file' and similar.
See also https://github.com/doomemacs/doomemacs/pull/6309."
  (unless magit-buffer-revision
    (apply fn args)))

(advice-add 'lsp :around #'magit--lsp--disable-when-visiting-blob)

;;; Find Index

(defvar magit-find-index-hook nil)

(defun magit-find-file-index-noselect (file &optional revert)
  "Read FILE from the index into a buffer and return the buffer.
FILE must to be relative to the top directory of the repository."
  (magit-find-file-noselect-1 "{index}" file (or revert 'ask-revert)))

(defun magit-update-index ()
  "Update the index with the contents of the current buffer.
The current buffer has to be visiting a file in the index, which
is done using `magit-find-index-noselect'."
  (interactive)
  (let ((file (magit-file-relative-name)))
    (unless (equal magit-buffer-refname "{index}")
      (user-error "%s isn't visiting the index" file))
    (if (y-or-n-p (format "Update index with contents of %s" (buffer-name)))
        (let ((index (make-temp-name
                      (expand-file-name "magit-update-index-" (magit-gitdir))))
              (buffer (current-buffer)))
          (when magit-wip-before-change-mode
            (magit-wip-commit-before-change (list file) " before un-/stage"))
          (unwind-protect
              (progn
                (let ((coding-system-for-write buffer-file-coding-system))
                  (with-temp-file index
                    (insert-buffer-substring buffer)))
                (magit-with-toplevel
                  (magit-call-git
                   "update-index" "--cacheinfo"
                   (substring (magit-git-string "ls-files" "-s" file)
                              0 6)
                   (magit-git-string "hash-object" "-t" "blob" "-w"
                                     (concat "--path=" file)
                                     "--" (magit-convert-filename-for-git index))
                   file)))
            (ignore-errors (delete-file index)))
          (set-buffer-modified-p nil)
          (when magit-wip-after-apply-mode
            (magit-wip-commit-after-apply (list file) " after un-/stage")))
      (message "Abort")))
  (when-let ((buffer (magit-get-mode-buffer 'magit-status-mode)))
    (with-current-buffer buffer
      (magit-refresh)))
  t)

;;; Find Config File

(defun magit-find-git-config-file (filename &optional wildcards)
  "Edit a file located in the current repository's git directory.

When \".git\", located at the root of the working tree, is a
regular file, then that makes it cumbersome to open a file
located in the actual git directory.

This command is like `find-file', except that it temporarily
binds `default-directory' to the actual git directory, while
reading the FILENAME."
  (interactive
   (let ((default-directory (magit-gitdir)))
     (find-file-read-args "Find file: "
                          (confirm-nonexistent-file-or-buffer))))
  (find-file filename wildcards))

(defun magit-find-git-config-file-other-window (filename &optional wildcards)
  "Edit a file located in the current repo's git directory, in another window.

When \".git\", located at the root of the working tree, is a
regular file, then that makes it cumbersome to open a file
located in the actual git directory.

This command is like `find-file-other-window', except that it
temporarily binds `default-directory' to the actual git
directory, while reading the FILENAME."
  (interactive
   (let ((default-directory (magit-gitdir)))
     (find-file-read-args "Find file in other window: "
                          (confirm-nonexistent-file-or-buffer))))
  (find-file-other-window filename wildcards))

(defun magit-find-git-config-file-other-frame (filename &optional wildcards)
  "Edit a file located in the current repo's git directory, in another frame.

When \".git\", located at the root of the working tree, is a
regular file, then that makes it cumbersome to open a file
located in the actual git directory.

This command is like `find-file-other-frame', except that it
temporarily binds `default-directory' to the actual git
directory, while reading the FILENAME."
  (interactive
   (let ((default-directory (magit-gitdir)))
     (find-file-read-args "Find file in other frame: "
                          (confirm-nonexistent-file-or-buffer))))
  (find-file-other-frame filename wildcards))

;;; File Dispatch

;;;###autoload (autoload 'magit-file-dispatch "magit" nil t)
(transient-define-prefix magit-file-dispatch ()
  "Invoke a Magit command that acts on the visited file.
When invoked outside a file-visiting buffer, then fall back
to `magit-dispatch'."
  :info-manual "(magit) Minor Mode for Buffers Visiting Files"
  [:if magit-file-relative-name
   ["File actions"
    ("  s" "Stage"    magit-stage-buffer-file)
    ("  u" "Unstage"  magit-unstage-buffer-file)
    (", x" "Untrack"  magit-file-untrack)
    (", r" "Rename"   magit-file-rename)
    (", k" "Delete"   magit-file-delete)
    (", c" "Checkout" magit-file-checkout)]
   ["Inspect"
    ("D" "Diff..."    magit-diff)
    ("d" "Diff"       magit-diff-buffer-file)]
   [""
    ("L" "Log..."     magit-log)
    ("l" "Log"        magit-log-buffer-file)
    ("t" "Trace"      magit-log-trace-definition)
    (7 "M" "Merged"   magit-log-merged)]
   [""
    ("B" "Blame..."   magit-blame)
    ("b" "Blame"      magit-blame-addition)
    ("r" "...removal" magit-blame-removal)
    ("f" "...reverse" magit-blame-reverse)
    ("m" "Blame echo" magit-blame-echo)
    ("q" "Quit blame" magit-blame-quit)]
   ["Navigate"
    ("p" "Prev blob"   magit-blob-previous)
    ("n" "Next blob"   magit-blob-next)
    ("v" "Goto blob"   magit-find-file)
    ("V" "Goto file"   magit-blob-visit-file)
    ("g" "Goto status" magit-status-here)
    ("G" "Goto magit"  magit-display-repository-buffer)]
   ["More actions"
    ("c" "Commit"     magit-commit)
    ("e" "Edit line"  magit-edit-line-commit)]]
  [:if-not magit-file-relative-name
   ["File actions"
    ("s" "Stage"    magit-stage-file)
    ("u" "Unstage"  magit-unstage-file)
    ("x" "Untrack"  magit-file-untrack)
    ("r" "Rename"   magit-file-rename)
    ("k" "Delete"   magit-file-delete)
    ("c" "Checkout" magit-file-checkout)]
   ["Navigate"
    ("g" "Goto status" magit-status-here :if-not-mode magit-status-mode)
    ("G" "Goto magit"  magit-display-repository-buffer)]])

;;; Blob Mode

(defvar-keymap magit-blob-mode-map
  :doc "Keymap for `magit-blob-mode'."
  "p" #'magit-blob-previous
  "n" #'magit-blob-next
  "b" #'magit-blame-addition
  "r" #'magit-blame-removal
  "f" #'magit-blame-reverse
  "q" #'magit-kill-this-buffer)

(define-minor-mode magit-blob-mode
  "Enable some Magit features in blob-visiting buffers.

Currently this only adds the following key bindings.
\n\\{magit-blob-mode-map}"
  :package-version '(magit . "2.3.0"))

(defun magit-blob-next ()
  "Visit the next blob which modified the current file."
  (interactive)
  (if magit-buffer-file-name
      (magit-blob-visit (or (magit-blob-successor magit-buffer-revision
                                                  magit-buffer-file-name)
                            magit-buffer-file-name))
    (if (buffer-file-name (buffer-base-buffer))
        (user-error "You have reached the end of time")
      (user-error "Buffer isn't visiting a file or blob"))))

(defun magit-blob-previous ()
  "Visit the previous blob which modified the current file."
  (interactive)
  (if-let ((file (or magit-buffer-file-name
                     (buffer-file-name (buffer-base-buffer)))))
      (if-let ((ancestor (magit-blob-ancestor magit-buffer-revision file)))
          (magit-blob-visit ancestor)
        (user-error "You have reached the beginning of time"))
    (user-error "Buffer isn't visiting a file or blob")))

;;;###autoload
(defun magit-blob-visit-file ()
  "View the file from the worktree corresponding to the current blob.
When visiting a blob or the version from the index, then go to
the same location in the respective file in the working tree."
  (interactive)
  (if-let ((file (magit-file-relative-name)))
      (magit-find-file--internal "{worktree}" file #'pop-to-buffer-same-window)
    (user-error "Not visiting a blob")))

(defun magit-blob-visit (blob-or-file)
  (if (stringp blob-or-file)
      (find-file blob-or-file)
    (pcase-let ((`(,rev ,file) blob-or-file))
      (magit-find-file rev file)
      (apply #'message "%s (%s %s ago)"
             (magit-rev-format "%s" rev)
             (magit--age (magit-rev-format "%ct" rev))))))

(defun magit-blob-ancestor (rev file)
  (let ((lines (magit-with-toplevel
                 (magit-git-lines "log" "-2" "--format=%H" "--name-only"
                                  "--follow" (or rev "HEAD") "--" file))))
    (if rev (cddr lines) (butlast lines 2))))

(defun magit-blob-successor (rev file)
  (let ((lines (magit-with-toplevel
                 (magit-git-lines "log" "--format=%H" "--name-only" "--follow"
                                  "HEAD" "--" file))))
    (catch 'found
      (while lines
        (if (equal (nth 2 lines) rev)
            (throw 'found (list (nth 0 lines) (nth 1 lines)))
          (setq lines (nthcdr 2 lines)))))))

;;; File Commands

(defun magit-file-rename (file newname)
  "Rename or move FILE to NEWNAME.
NEWNAME may be a file or directory name.  If FILE isn't tracked in
Git, fallback to using `rename-file'."
  (interactive
   (let* ((file (magit-read-file "Rename file"))
          (path (expand-file-name file (magit-toplevel))))
     (list path (expand-file-name
                 (read-file-name (format "Move %s to destination: " file)
                                 (file-name-directory path))))))
  (let ((oldbuf (get-file-buffer file))
        (dstdir (file-name-directory newname))
        (dstfile (if (directory-name-p newname)
                     (concat newname (file-name-nondirectory file))
                   newname)))
    (when (and oldbuf (buffer-modified-p oldbuf))
      (user-error "Save %s before moving it" file))
    (when (file-exists-p dstfile)
      (user-error "%s already exists" dstfile))
    (unless (file-exists-p dstdir)
      (user-error "Destination directory %s does not exist" dstdir))
    (if (magit-file-tracked-p file)
        (magit-call-git "mv"
                        (magit-convert-filename-for-git file)
                        (magit-convert-filename-for-git newname))
      (rename-file file newname current-prefix-arg))
    (when oldbuf
      (with-current-buffer oldbuf
        (let ((buffer-read-only buffer-read-only))
          (set-visited-file-name dstfile nil t))
        (if (fboundp 'vc-refresh-state)
            (vc-refresh-state)
          (with-no-warnings
            (vc-find-file-hook))))))
  (magit-refresh))

(defun magit-file-untrack (files &optional force)
  "Untrack the selected FILES or one file read in the minibuffer.

With a prefix argument FORCE do so even when the files have
staged as well as unstaged changes."
  (interactive (list (or (if-let ((files (magit-region-values 'file t)))
                             (if (magit-file-tracked-p (car files))
                                 (magit-confirm-files 'untrack files "Untrack")
                               (user-error "Already untracked"))
                           (list (magit-read-tracked-file "Untrack file"))))
                     current-prefix-arg))
  (magit-with-toplevel
    (magit-run-git "rm" "--cached" (and force "--force") "--" files)))

(defun magit-file-delete (files &optional force)
  "Delete the selected FILES or one file read in the minibuffer.

With a prefix argument FORCE do so even when the files have
uncommitted changes.  When the files aren't being tracked in
Git, then fallback to using `delete-file'."
  (interactive (list (if-let ((files (magit-region-values 'file t)))
                         (magit-confirm-files 'delete files "Delete")
                       (list (magit-read-file "Delete file")))
                     current-prefix-arg))
  (if (magit-file-tracked-p (car files))
      (magit-call-git "rm" (and force "--force") "--" files)
    (let ((topdir (magit-toplevel)))
      (dolist (file files)
        (delete-file (expand-file-name file topdir) t))))
  (magit-refresh))

;;;###autoload
(defun magit-file-checkout (rev file)
  "Checkout FILE from REV."
  (interactive
   (let ((rev (magit-read-branch-or-commit
               "Checkout from revision" magit-buffer-revision)))
     (list rev (magit-read-file-from-rev rev "Checkout file"))))
  (magit-with-toplevel
    (magit-run-git "checkout" rev "--" file)))

;;; Read File

(defvar magit-read-file-hist nil)

(defun magit-read-file-from-rev (rev prompt &optional default)
  (let ((files (magit-revision-files rev)))
    (magit-completing-read
     prompt files nil t nil 'magit-read-file-hist
     (car (member (or default (magit-current-file)) files)))))

(defun magit-read-file (prompt &optional tracked-only)
  (magit-with-toplevel
    (let ((choices (nconc (magit-list-files)
                          (and (not tracked-only)
                               (magit-untracked-files)))))
      (magit-completing-read
       prompt choices nil t nil nil
       (car (member (or (magit-section-value-if '(file submodule))
                        (magit-file-relative-name nil tracked-only))
                    choices))))))

(defun magit-read-tracked-file (prompt)
  (magit-read-file prompt t))

(defun magit-read-unmerged-file (&optional prompt)
  (let ((current  (magit-current-file))
        (unmerged (magit-unmerged-files)))
    (unless unmerged
      (user-error "There are no unresolved conflicts"))
    (magit-completing-read (or prompt "Resolve file")
                           unmerged nil t nil nil
                           (car (member current unmerged)))))

(defun magit-read-file-choice (prompt files &optional error default)
  "Read file from FILES.

If FILES has only one member, return that instead of prompting.
If FILES has no members, give a user error.  ERROR can be given
to provide a more informative error.

If DEFAULT is non-nil, use this as the default value instead of
`magit-current-file'."
  (pcase (length files)
    (0 (user-error (or error "No file choices")))
    (1 (car files))
    (_ (magit-completing-read
        prompt files nil t nil 'magit-read-file-hist
        (car (member (or default (magit-current-file)) files))))))

(defun magit-read-changed-file (rev-or-range prompt &optional default)
  (magit-read-file-choice
   prompt
   (magit-changed-files rev-or-range)
   default
   (concat "No file changed in " rev-or-range)))

;;; _
(provide 'magit-files)
;;; magit-files.el ends here
