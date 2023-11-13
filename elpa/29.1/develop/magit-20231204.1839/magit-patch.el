;;; magit-patch.el --- Creating and applying patches  -*- lexical-binding:t -*-

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

;; This library implements patch commands.

;;; Code:

(require 'magit)

;;; Options

(defcustom magit-patch-save-arguments '(exclude "--stat")
  "Control arguments used by the command `magit-patch-save'.

`magit-patch-save' (which see) saves a diff for the changes
shown in the current buffer in a patch file.  It may use the
same arguments as used in the buffer or a subset thereof, or
a constant list of arguments, depending on this option and
the prefix argument."
  :package-version '(magit . "2.12.0")
  :group 'magit-diff
  :type '(choice (const :tag "use buffer arguments" buffer)
                 (cons :tag "use buffer arguments except"
                       (const :format "" exclude)
                       (repeat :format "%v%i\n"
                               (string :tag "Argument")))
                 (repeat :tag "use constant arguments"
                         (string :tag "Argument"))))

;;; Commands

;;;###autoload (autoload 'magit-patch "magit-patch" nil t)
(transient-define-prefix magit-patch ()
  "Create or apply patches."
  ["Actions"
   [("c"  "Create patches"     magit-patch-create)
    ("w"  "Apply patches"      magit-am)]
   [("a"  "Apply plain patch"  magit-patch-apply)
    ("s"  "Save diff as patch" magit-patch-save)]
   [("r"  "Request pull"       magit-request-pull)]])

;;;###autoload (autoload 'magit-patch-create "magit-patch" nil t)
(transient-define-prefix magit-patch-create (range args files)
  "Create patches for the commits in RANGE.
When a single commit is given for RANGE, create a patch for the
changes introduced by that commit (unlike 'git format-patch'
which creates patches for all commits that are reachable from
`HEAD' but not from the specified commit)."
  :man-page "git-format-patch"
  :incompatible '(("--subject-prefix=" "--rfc"))
  ["Mail arguments"
   (6 magit-format-patch:--in-reply-to)
   (6 magit-format-patch:--thread)
   (6 magit-format-patch:--from)
   (6 magit-format-patch:--to)
   (6 magit-format-patch:--cc)]
  ["Patch arguments"
   (magit-format-patch:--base)
   (magit-format-patch:--reroll-count)
   (5 magit-format-patch:--interdiff)
   (magit-format-patch:--range-diff)
   (magit-format-patch:--subject-prefix)
   ("C-m r  " "RFC subject prefix" "--rfc")
   ("C-m l  " "Add cover letter" "--cover-letter")
   (5 magit-format-patch:--cover-from-description)
   (5 magit-format-patch:--notes)
   (magit-format-patch:--output-directory)]
  ["Diff arguments"
   (magit-diff:-U)
   (magit-diff:-M)
   (magit-diff:-C)
   (magit-diff:--diff-algorithm)
   (magit:--)
   (7 "-b" "Ignore whitespace changes" ("-b" "--ignore-space-change"))
   (7 "-w" "Ignore all whitespace"     ("-w" "--ignore-all-space"))]
  ["Actions"
   ("c" "Create patches" magit-patch-create)]
  (interactive
   (if (not (eq transient-current-command 'magit-patch-create))
       (list nil nil nil)
     (cons (if-let ((revs (magit-region-values 'commit t)))
               (concat (car (last revs)) "^.." (car revs))
             (let ((range (magit-read-range-or-commit
                           "Create patches for range or commit")))
               (if (string-search ".." range)
                   range
                 (format "%s~..%s" range range))))
           (let ((args (transient-args 'magit-patch-create)))
             (list (seq-filter #'stringp args)
                   (cdr (assoc "--" args)))))))
  (if (not range)
      (transient-setup 'magit-patch-create)
    (magit-run-git "format-patch" range args "--" files)
    (when (member "--cover-letter" args)
      (save-match-data
        (find-file
         (expand-file-name
          (concat (and-let* ((v (transient-arg-value "--reroll-count=" args)))
                    (format "v%s-" v))
                  "0000-cover-letter.patch")
          (let ((topdir (magit-toplevel)))
            (if-let ((dir (transient-arg-value "--output-directory=" args)))
                (expand-file-name dir topdir)
              topdir))))))))

(transient-define-argument magit-format-patch:--in-reply-to ()
  :description "In reply to"
  :class 'transient-option
  :key "C-m C-r"
  :argument "--in-reply-to=")

(transient-define-argument magit-format-patch:--thread ()
  :description "Thread style"
  :class 'transient-option
  :key "C-m s  "
  :argument "--thread="
  :reader #'magit-format-patch-select-thread-style)

(defun magit-format-patch-select-thread-style (&rest _ignore)
  (magit-read-char-case "Thread style " t
    (?d "[d]eep" "deep")
    (?s "[s]hallow" "shallow")))

(transient-define-argument magit-format-patch:--base ()
  :description "Insert base commit"
  :class 'transient-option
  :key "C-m b  "
  :argument "--base="
  :reader #'magit-format-patch-select-base)

(defun magit-format-patch-select-base (prompt initial-input history)
  (or (magit-completing-read prompt (cons "auto" (magit-list-refnames))
                             nil nil initial-input history "auto")
      (user-error "Nothing selected")))

(transient-define-argument magit-format-patch:--reroll-count ()
  :description "Reroll count"
  :class 'transient-option
  :key "C-m v  "
  :shortarg "-v"
  :argument "--reroll-count="
  :reader #'transient-read-number-N+)

(transient-define-argument magit-format-patch:--interdiff ()
  :description "Insert interdiff"
  :class 'transient-option
  :key "C-m d i"
  :argument "--interdiff="
  :reader #'magit-transient-read-revision)

(transient-define-argument magit-format-patch:--range-diff ()
  :description "Insert range-diff"
  :class 'transient-option
  :key "C-m d r"
  :argument "--range-diff="
  :reader #'magit-format-patch-select-range-diff)

(defun magit-format-patch-select-range-diff (prompt _initial-input _history)
  (magit-read-range-or-commit prompt))

(transient-define-argument magit-format-patch:--subject-prefix ()
  :description "Subject Prefix"
  :class 'transient-option
  :key "C-m p  "
  :argument "--subject-prefix=")

(transient-define-argument magit-format-patch:--cover-from-description ()
  :description "Use branch description"
  :class 'transient-option
  :key "C-m D  "
  :argument "--cover-from-description="
  :reader #'magit-format-patch-select-description-mode)

(defun magit-format-patch-select-description-mode (&rest _ignore)
  (magit-read-char-case "Use description as " t
    (?m "[m]essage" "message")
    (?s "[s]ubject" "subject")
    (?a "[a]uto"    "auto")
    (?n "[n]othing" "none")))

(transient-define-argument magit-format-patch:--notes ()
  :description "Insert commentary from notes"
  :class 'transient-option
  :key "C-m n  "
  :argument "--notes="
  :reader #'magit-notes-read-ref)

(transient-define-argument magit-format-patch:--from ()
  :description "From"
  :class 'transient-option
  :key "C-m C-f"
  :argument "--from="
  :reader #'magit-transient-read-person)

(transient-define-argument magit-format-patch:--to ()
  :description "To"
  :class 'transient-option
  :key "C-m C-t"
  :argument "--to="
  :reader #'magit-transient-read-person)

(transient-define-argument magit-format-patch:--cc ()
  :description "CC"
  :class 'transient-option
  :key "C-m C-c"
  :argument "--cc="
  :reader #'magit-transient-read-person)

(transient-define-argument magit-format-patch:--output-directory ()
  :description "Output directory"
  :class 'transient-option
  :key "C-m o  "
  :shortarg "-o"
  :argument "--output-directory="
  :reader #'transient-read-existing-directory)

;;;###autoload (autoload 'magit-patch-apply "magit-patch" nil t)
(transient-define-prefix magit-patch-apply (file &rest args)
  "Apply the patch file FILE."
  :man-page "git-apply"
  ["Arguments"
   ("-i" "Also apply to index" "--index")
   ("-c" "Only apply to index" "--cached")
   ("-3" "Fall back on 3way merge" ("-3" "--3way"))]
  ["Actions"
   ("a"  "Apply patch" magit-patch-apply)]
  (interactive
   (if (not (eq transient-current-command 'magit-patch-apply))
       (list nil)
     (list (expand-file-name
            (read-file-name "Apply patch: "
                            default-directory nil nil
                            (and-let* ((file (magit-file-at-point)))
                              (file-relative-name file))))
           (transient-args 'magit-patch-apply))))
  (if (not file)
      (transient-setup 'magit-patch-apply)
    (magit-run-git "apply" args "--" (magit-convert-filename-for-git file))))

;;;###autoload
(defun magit-patch-save (file &optional arg)
  "Write current diff into patch FILE.

What arguments are used to create the patch depends on the value
of `magit-patch-save-arguments' and whether a prefix argument is
used.

If the value is the symbol `buffer', then use the same arguments
as the buffer.  With a prefix argument use no arguments.

If the value is a list beginning with the symbol `exclude', then
use the same arguments as the buffer except for those matched by
entries in the cdr of the list.  The comparison is done using
`string-prefix-p'.  With a prefix argument use the same arguments
as the buffer.

If the value is a list of strings (including the empty list),
then use those arguments.  With a prefix argument use the same
arguments as the buffer.

Of course the arguments that are required to actually show the
same differences as those shown in the buffer are always used."
  (interactive (list (read-file-name "Write patch file: " default-directory)
                     current-prefix-arg))
  (unless (derived-mode-p 'magit-diff-mode)
    (user-error "Only diff buffers can be saved as patches"))
  (let ((rev     magit-buffer-range)
        (typearg magit-buffer-typearg)
        (args    magit-buffer-diff-args)
        (files   magit-buffer-diff-files))
    (cond ((eq magit-patch-save-arguments 'buffer)
           (when arg
             (setq args nil)))
          ((eq (car-safe magit-patch-save-arguments) 'exclude)
           (unless arg
             (setq args
                   (cl-set-difference args (cdr magit-patch-save-arguments)
                                      :test #'equal))))
          ((not arg)
           (setq args magit-patch-save-arguments)))
    (with-temp-file file
      (magit-git-insert "diff" rev "-p" typearg args "--" files)))
  (magit-refresh))

;;;###autoload
(defun magit-request-pull (url start end)
  "Request upstream to pull from your public repository.

URL is the url of your publicly accessible repository.
START is a commit that already is in the upstream repository.
END is the last commit, usually a branch name, which upstream
is asked to pull.  START has to be reachable from that commit."
  (interactive
   (list (magit-get "remote" (magit-read-remote "Remote") "url")
         (magit-read-branch-or-commit "Start" (magit-get-upstream-branch))
         (magit-read-branch-or-commit "End")))
  (let ((dir default-directory))
    ;; mu4e changes default-directory
    (compose-mail)
    (setq default-directory dir))
  (message-goto-body)
  (magit-git-insert "request-pull" start url end)
  (set-buffer-modified-p nil))

;;; _
(provide 'magit-patch)
;;; magit-patch.el ends here
