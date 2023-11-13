;;; magit-bisect.el --- Bisect support for Magit  -*- lexical-binding:t -*-

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

;; Use a binary search to find the commit that introduced a bug.

;;; Code:

(require 'magit)

;;; Options

(defcustom magit-bisect-show-graph t
  "Whether to use `--graph' in the log showing commits yet to be bisected."
  :package-version '(magit . "2.8.0")
  :group 'magit-status
  :type 'boolean)

(defface magit-bisect-good
  '((t :foreground "DarkOliveGreen"))
  "Face for good bisect revisions."
  :group 'magit-faces)

(defface magit-bisect-skip
  '((t :foreground "DarkGoldenrod"))
  "Face for skipped bisect revisions."
  :group 'magit-faces)

(defface magit-bisect-bad
  '((t :foreground "IndianRed4"))
  "Face for bad bisect revisions."
  :group 'magit-faces)

;;; Commands

;;;###autoload (autoload 'magit-bisect "magit-bisect" nil t)
(transient-define-prefix magit-bisect ()
  "Narrow in on the commit that introduced a bug."
  :man-page "git-bisect"
  [:class transient-subgroups
   :if-not magit-bisect-in-progress-p
   ["Arguments"
    ("-n" "Don't checkout commits"              "--no-checkout")
    ("-p" "Follow only first parent of a merge" "--first-parent"
     :if (lambda () (magit-git-version>= "2.29")))
    (6 magit-bisect:--term-old
       :if (lambda () (magit-git-version>= "2.7")))
    (6 magit-bisect:--term-new
       :if (lambda () (magit-git-version>= "2.7")))]
   ["Actions"
    ("B" "Start"        magit-bisect-start)
    ("s" "Start script" magit-bisect-run)]]
  ["Actions"
   :if magit-bisect-in-progress-p
   ("B" "Bad"          magit-bisect-bad)
   ("g" "Good"         magit-bisect-good)
   (6 "m" "Mark"       magit-bisect-mark
      :if (lambda () (magit-git-version>= "2.7")))
   ("k" "Skip"         magit-bisect-skip)
   ("r" "Reset"        magit-bisect-reset)
   ("s" "Run script"   magit-bisect-run)])

(transient-define-argument magit-bisect:--term-old ()
  :description "Old/good term"
  :class 'transient-option
  :key "=o"
  :argument "--term-old=")

(transient-define-argument magit-bisect:--term-new ()
  :description "New/bad term"
  :class 'transient-option
  :key "=n"
  :argument "--term-new=")

;;;###autoload
(defun magit-bisect-start (bad good args)
  "Start a bisect session.

Bisecting a bug means to find the commit that introduced it.
This command starts such a bisect session by asking for a known
good and a known bad commit.  To move the session forward use the
other actions from the bisect transient command (\
\\<magit-status-mode-map>\\[magit-bisect])."
  (interactive (if (magit-bisect-in-progress-p)
                   (user-error "Already bisecting")
                 (magit-bisect-start-read-args)))
  (unless (magit-rev-ancestor-p good bad)
    (user-error
     "The %s revision (%s) has to be an ancestor of the %s one (%s)"
     (or (transient-arg-value "--term-old=" args) "good")
     good
     (or (transient-arg-value "--term-new=" args) "bad")
     bad))
  (when (magit-anything-modified-p)
    (user-error "Cannot bisect with uncommitted changes"))
  (magit-repository-local-set 'bisect--first-parent
                              (transient-arg-value "--first-parent" args))
  (magit-git-bisect "start" (list args bad good) t))

(defun magit-bisect-start-read-args ()
  (let* ((args (transient-args 'magit-bisect))
         (bad (magit-read-branch-or-commit
               (format "Start bisect with %s revision"
                       (or (transient-arg-value "--term-new=" args)
                           "bad")))))
    (list bad
          (magit-read-other-branch-or-commit
           (format "%s revision" (or (transient-arg-value "--term-old=" args)
                                     "Good"))
           bad)
          args)))

;;;###autoload
(defun magit-bisect-reset ()
  "After bisecting, cleanup bisection state and return to original `HEAD'."
  (interactive)
  (magit-confirm 'reset-bisect)
  (magit-run-git "bisect" "reset")
  (magit-repository-local-delete 'bisect--first-parent)
  (ignore-errors
    (delete-file (expand-file-name "BISECT_CMD_OUTPUT" (magit-gitdir)))))

;;;###autoload
(defun magit-bisect-good ()
  "While bisecting, mark the current commit as good.
Use this after you have asserted that the commit does not contain
the bug in question."
  (interactive)
  (magit-git-bisect (or (cadr (magit-bisect-terms))
                        (user-error "Not bisecting"))))

;;;###autoload
(defun magit-bisect-bad ()
  "While bisecting, mark the current commit as bad.
Use this after you have asserted that the commit does contain the
bug in question."
  (interactive)
  (magit-git-bisect (or (car (magit-bisect-terms))
                        (user-error "Not bisecting"))))

;;;###autoload
(defun magit-bisect-mark ()
  "While bisecting, mark the current commit with a bisect term.
During a bisect using alternate terms, commits can still be
marked with `magit-bisect-good' and `magit-bisect-bad', as those
commands map to the correct term (\"good\" to --term-old's value
and \"bad\" to --term-new's).  However, in some cases, it can be
difficult to keep that mapping straight in your head; this
command provides an interface that exposes the underlying terms."
  (interactive)
  (magit-git-bisect
   (pcase-let ((`(,term-new ,term-old) (or (magit-bisect-terms)
                                           (user-error "Not bisecting"))))
     (pcase (read-char-choice
             (format "Mark HEAD as %s ([n]ew) or %s ([o]ld)"
                     term-new term-old)
             (list ?n ?o))
       (?n term-new)
       (?o term-old)))))

;;;###autoload
(defun magit-bisect-skip ()
  "While bisecting, skip the current commit.
Use this if for some reason the current commit is not a good one
to test.  This command lets Git choose a different one."
  (interactive)
  (magit-git-bisect "skip"))

;;;###autoload
(defun magit-bisect-run (cmdline &optional bad good args)
  "Bisect automatically by running commands after each step.

Unlike `git bisect run' this can be used before bisecting has
begun.  In that case it behaves like `git bisect start; git
bisect run'."
  (interactive (let ((args (and (not (magit-bisect-in-progress-p))
                                (magit-bisect-start-read-args))))
                 (cons (read-shell-command "Bisect shell command: ") args)))
  (when (and bad good)
    ;; Avoid `magit-git-bisect' because it's asynchronous, but the
    ;; next `git bisect run' call requires the bisect to be started.
    (magit-with-toplevel
      (magit-process-git
       (list :file (expand-file-name "BISECT_CMD_OUTPUT" (magit-gitdir)))
       (magit-process-git-arguments
        (list "bisect" "start" bad good args)))
      (magit-refresh)))
  (magit--with-connection-local-variables
   (magit-git-bisect "run" (list shell-file-name
                                 shell-command-switch cmdline))))

(defun magit-git-bisect (subcommand &optional args no-assert)
  (unless (or no-assert (magit-bisect-in-progress-p))
    (user-error "Not bisecting"))
  (message "Bisecting...")
  (magit-with-toplevel
    (magit-run-git-async "bisect" subcommand args))
  (set-process-sentinel
   magit-this-process
   (lambda (process event)
     (when (memq (process-status process) '(exit signal))
       (if (> (process-exit-status process) 0)
           (magit-process-sentinel process event)
         (process-put process 'inhibit-refresh t)
         (magit-process-sentinel process event)
         (when (buffer-live-p (process-buffer process))
           (with-current-buffer (process-buffer process)
             (when-let* ((section (magit-section-at))
                         (output (buffer-substring-no-properties
                                  (oref section content)
                                  (oref section end))))
               (with-temp-file
                   (expand-file-name "BISECT_CMD_OUTPUT" (magit-gitdir))
                 (insert output)))))
         (magit-refresh))
       (message "Bisecting...done")))))

;;; Sections

(defun magit-bisect-in-progress-p ()
  (file-exists-p (expand-file-name "BISECT_LOG" (magit-gitdir))))

(defun magit-bisect-terms ()
  (magit-file-lines (expand-file-name "BISECT_TERMS" (magit-gitdir))))

(defun magit-insert-bisect-output ()
  "While bisecting, insert section with output from `git bisect'."
  (when (magit-bisect-in-progress-p)
    (let* ((lines
            (or (magit-file-lines
                 (expand-file-name "BISECT_CMD_OUTPUT" (magit-gitdir)))
                (list "Bisecting: (no saved bisect output)"
                      "It appears you have invoked `git bisect' from a shell."
                      "There is nothing wrong with that, we just cannot display"
                      "anything useful here.  Consult the shell output instead.")))
           (done-re "^\\([a-z0-9]\\{40,\\}\\) is the first bad commit$")
           (bad-line (or (and (string-match done-re (car lines))
                              (pop lines))
                         (--first (string-match done-re it) lines))))
      (magit-insert-section ((eval (if bad-line 'commit 'bisect-output))
                             (and bad-line (match-string 1 bad-line)))
        (magit-insert-heading
          (propertize (or bad-line (pop lines))
                      'font-lock-face 'magit-section-heading))
        (dolist (line lines)
          (insert line "\n"))))
    (insert "\n")))

(defun magit-insert-bisect-rest ()
  "While bisecting, insert section visualizing the bisect state."
  (when (magit-bisect-in-progress-p)
    (magit-insert-section (bisect-view)
      (magit-insert-heading "Bisect Rest:")
      (magit-git-wash (apply-partially #'magit-log-wash-log 'bisect-vis)
        "bisect" "visualize" "git" "log"
        "--format=%h%x00%D%x00%s" "--decorate=full"
        (and magit-bisect-show-graph "--graph")
        (and (magit-repository-local-get 'bisect--first-parent)
             "--first-parent")))))

(defun magit-insert-bisect-log ()
  "While bisecting, insert section logging bisect progress."
  (when (magit-bisect-in-progress-p)
    (magit-insert-section (bisect-log)
      (magit-insert-heading "Bisect Log:")
      (magit-git-wash #'magit-wash-bisect-log "bisect" "log")
      (insert ?\n))))

(defun magit-wash-bisect-log (_args)
  (let (beg)
    (while (progn (setq beg (point-marker))
                  (re-search-forward
                   "^\\(\\(?:git bisect\\|# status:\\) [^\n]+\n\\)" nil t))
      (if (string-prefix-p "# status:" (match-string 1))
          (magit-delete-match)
        (magit-bind-match-strings (heading) nil
          (magit-delete-match)
          (save-restriction
            (narrow-to-region beg (point))
            (goto-char (point-min))
            (magit-insert-section (bisect-item heading t)
              (insert (propertize heading 'font-lock-face
                                  'magit-section-secondary-heading))
              (magit-insert-heading)
              (magit-wash-sequence
               (apply-partially #'magit-log-wash-rev 'bisect-log
                                (magit-abbrev-length)))
              (insert ?\n))))))
    (when (re-search-forward
           "# first bad commit: \\[\\([a-z0-9]\\{40,\\}\\)\\] [^\n]+\n" nil t)
      (magit-bind-match-strings (hash) nil
        (magit-delete-match)
        (magit-insert-section (bisect-item)
          (insert hash " is the first bad commit\n"))))))

;;; _
(provide 'magit-bisect)
;;; magit-bisect.el ends here
