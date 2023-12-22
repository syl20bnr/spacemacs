;;; git-commit.el --- Edit Git commit messages  -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2008-2024 The Magit Project Contributors

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;;     Sebastian Wiesner <lunaryorn@gmail.com>
;;     Florian Ragwitz <rafl@debian.org>
;;     Marius Vollmer <marius.vollmer@gmail.com>
;; Maintainer: Jonas Bernoulli <jonas@bernoul.li>

;; Homepage: https://github.com/magit/magit
;; Keywords: git tools vc

;; Package-Version: 3.3.0.50-git
;; Package-Requires: (
;;     (emacs "25.1")
;;     (compat "29.1.4.4")
;;     (seq "2.24")
;;     (transient "0.5.0")
;;     (with-editor "3.3.2"))

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Magit is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; Magit is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Magit.  If not, see <https://www.gnu.org/licenses/>.

;; You should have received a copy of the AUTHORS.md file, which
;; lists all contributors.  If not, see https://magit.vc/authors.

;;; Commentary:

;; This package assists the user in writing good Git commit messages.

;; While Git allows for the message to be provided on the command
;; line, it is preferable to tell Git to create the commit without
;; actually passing it a message.  Git then invokes the `$GIT_EDITOR'
;; (or if that is undefined `$EDITOR') asking the user to provide the
;; message by editing the file ".git/COMMIT_EDITMSG" (or another file
;; in that directory, e.g., ".git/MERGE_MSG" for merge commits).

;; When `global-git-commit-mode' is enabled, which it is by default,
;; then opening such a file causes the features described below, to
;; be enabled in that buffer.  Normally this would be done using a
;; major-mode but to allow the use of any major-mode, as the user sees
;; fit, it is done here by running a setup function, which among other
;; things turns on the preferred major-mode, by default `text-mode'.

;; Git waits for the `$EDITOR' to finish and then either creates the
;; commit using the contents of the file as commit message, or, if the
;; editor process exited with a non-zero exit status, aborts without
;; creating a commit.  Unfortunately Emacsclient (which is what Emacs
;; users should be using as `$EDITOR' or at least as `$GIT_EDITOR')
;; does not differentiate between "successfully" editing a file and
;; aborting; not out of the box that is.

;; By making use of the `with-editor' package this package provides
;; both ways of finish an editing session.  In either case the file
;; is saved, but Emacseditor's exit code differs.
;;
;;   C-c C-c  Finish the editing session successfully by returning
;;            with exit code 0.  Git then creates the commit using
;;            the message it finds in the file.
;;
;;   C-c C-k  Aborts the edit editing session by returning with exit
;;            code 1.  Git then aborts the commit.

;; Aborting the commit does not cause the message to be lost, but
;; relying solely on the file not being tampered with is risky.  This
;; package additionally stores all aborted messages for the duration
;; of the current session (i.e., until you close Emacs).  To get back
;; an aborted message use M-p and M-n while editing a message.
;;
;;   M-p      Replace the buffer contents with the previous message
;;            from the message ring.  Of course only after storing
;;            the current content there too.
;;
;;   M-n      Replace the buffer contents with the next message from
;;            the message ring, after storing the current content.

;; Support for inserting Git trailers (as described in the manpage
;; git-interpret-trailers(1)) is available.
;;
;;   C-c C-i  Insert a trailer selected from a transient menu.

;; When Git requests a commit message from the user, it does so by
;; having her edit a file which initially contains some comments,
;; instructing her what to do, and providing useful information, such
;; as which files were modified.  These comments, even when left
;; intact by the user, do not become part of the commit message.  This
;; package ensures these comments are propertizes as such and further
;; prettifies them by using different faces for various parts, such as
;; files.

;; Finally this package highlights style errors, like lines that are
;; too long, or when the second line is not empty.  It may even nag
;; you when you attempt to finish the commit without having fixed
;; these issues.  The style checks and many other settings can easily
;; be configured:
;;
;;   M-x customize-group RET git-commit RET

;;; Code:

(require 'compat)
(require 'subr-x)

(when (and (featurep' seq)
           (not (fboundp 'seq-keep)))
  (unload-feature 'seq 'force))
(require 'seq)

(require 'log-edit)
(require 'ring)
(require 'server)
(require 'transient)
(require 'with-editor)

;; For historic reasons Magit isn't a hard dependency.
(require 'magit-base nil t)
(require 'magit-git nil t)
(declare-function magit-completing-read "magit-base"
                  ( prompt collection &optional predicate require-match
                    initial-input hist def fallback))
(declare-function magit-expand-git-file-name "magit-git" (filename))
(declare-function magit-git-lines "magit-git" (&rest args))
(declare-function magit-hook-custom-get "magit-base" (symbol))
(declare-function magit-list-local-branch-names "magit-git" ())

(defvar diff-default-read-only)
(defvar flyspell-generic-check-word-predicate)
(defvar font-lock-beg)
(defvar font-lock-end)
(defvar recentf-exclude)

;;; Options
;;;; Variables

(defgroup git-commit nil
  "Edit Git commit messages."
  :prefix "git-commit-"
  :link '(info-link "(magit)Editing Commit Messages")
  :group 'tools)

(define-minor-mode global-git-commit-mode
  "Edit Git commit messages.

This global mode arranges for `git-commit-setup' to be called
when a Git commit message file is opened.  That usually happens
when Git uses the Emacsclient as $GIT_EDITOR to have the user
provide such a commit message.

Loading the library `git-commit' by default enables this mode,
but the library is not automatically loaded because doing that
would pull in many dependencies and increase startup time too
much.  You can either rely on `magit' loading this library or
you can load it explicitly.  Autoloading is not an alternative
because in this case autoloading would immediately trigger
full loading."
  :group 'git-commit
  :type 'boolean
  :global t
  :init-value t
  :initialize (lambda (symbol exp)
                (custom-initialize-default symbol exp)
                (when global-git-commit-mode
                  (add-hook 'find-file-hook #'git-commit-setup-check-buffer)))
  (if global-git-commit-mode
      (add-hook  'find-file-hook #'git-commit-setup-check-buffer)
    (remove-hook 'find-file-hook #'git-commit-setup-check-buffer)))

(defcustom git-commit-major-mode #'text-mode
  "Major mode used to edit Git commit messages.

The major mode configured here is turned on by the minor mode
`git-commit-mode'."
  :group 'git-commit
  :type '(choice (function-item text-mode)
                 (function-item markdown-mode)
                 (function-item org-mode)
                 (function-item fundamental-mode)
                 (function-item git-commit-elisp-text-mode)
                 (function :tag "Another mode")
                 (const :tag "No major mode")))
;;;###autoload(put 'git-commit-major-mode 'safe-local-variable
;;;###autoload     (lambda (val)
;;;###autoload       (memq val '(text-mode
;;;###autoload                   markdown-mode
;;;###autoload                   org-mode
;;;###autoload                   fundamental-mode
;;;###autoload                   git-commit-elisp-text-mode))))

(defcustom git-commit-setup-hook
  '(git-commit-save-message
    git-commit-setup-changelog-support
    git-commit-turn-on-auto-fill
    git-commit-propertize-diff
    bug-reference-mode)
  "Hook run at the end of `git-commit-setup'."
  :group 'git-commit
  :type 'hook
  :get (and (featurep 'magit-base) #'magit-hook-custom-get)
  :options '(git-commit-save-message
             git-commit-setup-changelog-support
             magit-generate-changelog
             git-commit-turn-on-auto-fill
             git-commit-turn-on-orglink
             git-commit-turn-on-flyspell
             git-commit-propertize-diff
             bug-reference-mode))

(defcustom git-commit-post-finish-hook nil
  "Hook run after the user finished writing a commit message.

\\<with-editor-mode-map>\
This hook is only run after pressing \\[with-editor-finish] in a buffer used
to edit a commit message.  If a commit is created without the
user typing a message into a buffer, then this hook is not run.

This hook is not run until the new commit has been created.  If
that takes Git longer than `git-commit-post-finish-hook-timeout'
seconds, then this hook isn't run at all.  For certain commands
such as `magit-rebase-continue' this hook is never run because
doing so would lead to a race condition.

This hook is only run if `magit' is available.

Also see `magit-post-commit-hook'."
  :group 'git-commit
  :type 'hook
  :get (and (featurep 'magit-base) #'magit-hook-custom-get))

(defcustom git-commit-post-finish-hook-timeout 1
  "Time in seconds to wait for git to create a commit.

The hook `git-commit-post-finish-hook' (which see) is run only
after git is done creating a commit.  If it takes longer than
`git-commit-post-finish-hook-timeout' seconds to create the
commit, then the hook is not run at all."
  :group 'git-commit
  :safe 'numberp
  :type 'number)

(defcustom git-commit-finish-query-functions
  '(git-commit-check-style-conventions)
  "List of functions called to query before performing commit.

The commit message buffer is current while the functions are
called.  If any of them returns nil, then the commit is not
performed and the buffer is not killed.  The user should then
fix the issue and try again.

The functions are called with one argument.  If it is non-nil,
then that indicates that the user used a prefix argument to
force finishing the session despite issues.  Functions should
usually honor this wish and return non-nil."
  :options '(git-commit-check-style-conventions)
  :type 'hook
  :group 'git-commit)

(defcustom git-commit-style-convention-checks '(non-empty-second-line)
  "List of checks performed by `git-commit-check-style-conventions'.

Valid members are `non-empty-second-line' and `overlong-summary-line'.
That function is a member of `git-commit-finish-query-functions'."
  :options '(non-empty-second-line overlong-summary-line)
  :type '(list :convert-widget custom-hook-convert-widget)
  :group 'git-commit)

(defcustom git-commit-summary-max-length 68
  "Column beyond which characters in the summary lines are highlighted.

The highlighting indicates that the summary is getting too long
by some standards.  It does in no way imply that going over the
limit a few characters or in some cases even many characters is
anything that deserves shaming.  It's just a friendly reminder
that if you can make the summary shorter, then you might want
to consider doing so."
  :group 'git-commit
  :safe 'numberp
  :type 'number)

(defcustom git-commit-trailers
  '("Acked-by"
    "Modified-by"
    "Reviewed-by"
    "Signed-off-by"
    "Tested-by"
    "Cc"
    "Reported-by"
    "Suggested-by"
    "Co-authored-by"
    "Co-developed-by")
  "A list of Git trailers to be highlighted.

See also manpage git-interpret-trailer(1).  This package does
not use that Git command, but the initial description still
serves as a good introduction."
  :group 'git-commit
  :safe (lambda (val) (and (listp val) (seq-every-p #'stringp val)))
  :type '(repeat string))

(defcustom git-commit-use-local-message-ring nil
  "Whether to use a local message ring instead of the global one.

This can be set globally, in which case every repository gets its
own commit message ring, or locally for a single repository.  If
Magit isn't available, then setting this to a non-nil value has
no effect."
  :group 'git-commit
  :safe 'booleanp
  :type 'boolean)

;;;; Faces

(defgroup git-commit-faces nil
  "Faces used for highlighting Git commit messages."
  :prefix "git-commit-"
  :group 'git-commit
  :group 'faces)

(defface git-commit-summary
  '((t :inherit font-lock-type-face))
  "Face used for the summary in commit messages."
  :group 'git-commit-faces)

(defface git-commit-overlong-summary
  '((t :inherit font-lock-warning-face))
  "Face used for the tail of overlong commit message summaries."
  :group 'git-commit-faces)

(defface git-commit-nonempty-second-line
  '((t :inherit font-lock-warning-face))
  "Face used for non-whitespace on the second line of commit messages."
  :group 'git-commit-faces)

(defface git-commit-keyword
  '((t :inherit font-lock-string-face))
  "Face used for keywords in commit messages.
In this context a \"keyword\" is text surrounded by brackets."
  :group 'git-commit-faces)

(defface git-commit-trailer-token
  '((t :inherit font-lock-keyword-face))
  "Face used for Git trailer tokens in commit messages."
  :group 'git-commit-faces)

(defface git-commit-trailer-value
  '((t :inherit font-lock-string-face))
  "Face used for Git trailer values in commit messages."
  :group 'git-commit-faces)

(defface git-commit-comment-branch-local
  (if (featurep 'magit)
      '((t :inherit magit-branch-local))
    '((t :inherit font-lock-variable-name-face)))
  "Face used for names of local branches in commit message comments."
  :group 'git-commit-faces)

(defface git-commit-comment-branch-remote
  (if (featurep 'magit)
      '((t :inherit magit-branch-remote))
    '((t :inherit font-lock-variable-name-face)))
  "Face used for names of remote branches in commit message comments.
This is only used if Magit is available."
  :group 'git-commit-faces)

(defface git-commit-comment-detached
  '((t :inherit git-commit-comment-branch-local))
  "Face used for detached `HEAD' in commit message comments."
  :group 'git-commit-faces)

(defface git-commit-comment-heading
  '((t :inherit git-commit-trailer-token))
  "Face used for headings in commit message comments."
  :group 'git-commit-faces)

(defface git-commit-comment-file
  '((t :inherit git-commit-trailer-value))
  "Face used for file names in commit message comments."
  :group 'git-commit-faces)

(defface git-commit-comment-action
  '((t :inherit bold))
  "Face used for actions in commit message comments."
  :group 'git-commit-faces)

;;; Keymap

(defvar-keymap git-commit-redundant-bindings
  :doc "Bindings made redundant by `git-commit-insert-trailer'.
This keymap is used as the parent of `git-commit-mode-map',
to avoid upsetting muscle-memory.  If you would rather avoid
the redundant bindings, then set this to nil, before loading
`git-commit'."
  "C-c C-a" #'git-commit-ack
  "C-c M-i" #'git-commit-suggested
  "C-c C-m" #'git-commit-modified
  "C-c C-o" #'git-commit-cc
  "C-c C-p" #'git-commit-reported
  "C-c C-r" #'git-commit-review
  "C-c C-s" #'git-commit-signoff
  "C-c C-t" #'git-commit-test)

(defvar-keymap git-commit-mode-map
  :doc "Keymap used by `git-commit-mode'."
  :parent git-commit-redundant-bindings
  "M-p"     #'git-commit-prev-message
  "M-n"     #'git-commit-next-message
  "C-c M-p" #'git-commit-search-message-backward
  "C-c M-n" #'git-commit-search-message-forward
  "C-c C-i" #'git-commit-insert-trailer
  "C-c M-s" #'git-commit-save-message)

;;; Menu

(require 'easymenu)
(easy-menu-define git-commit-mode-menu git-commit-mode-map
  "Git Commit Mode Menu"
  '("Commit"
    ["Previous" git-commit-prev-message t]
    ["Next" git-commit-next-message t]
    "-"
    ["Ack" git-commit-ack t
     :help "Insert an 'Acked-by' trailer"]
    ["Modified-by" git-commit-modified t
     :help "Insert a 'Modified-by' trailer"]
    ["Reviewed-by" git-commit-review t
     :help "Insert a 'Reviewed-by' trailer"]
    ["Sign-Off" git-commit-signoff t
     :help "Insert a 'Signed-off-by' trailer"]
    ["Tested-by" git-commit-test t
     :help "Insert a 'Tested-by' trailer"]
    "-"
    ["CC" git-commit-cc t
     :help "Insert a 'Cc' trailer"]
    ["Reported" git-commit-reported t
     :help "Insert a 'Reported-by' trailer"]
    ["Suggested" git-commit-suggested t
     :help "Insert a 'Suggested-by' trailer"]
    ["Co-authored-by" git-commit-co-authored t
     :help "Insert a 'Co-authored-by' trailer"]
    ["Co-developed-by" git-commit-co-developed t
     :help "Insert a 'Co-developed-by' trailer"]
    "-"
    ["Save" git-commit-save-message t]
    ["Cancel" with-editor-cancel t]
    ["Commit" with-editor-finish t]))

;;; Hooks

(defconst git-commit-filename-regexp "/\\(\
\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\
\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'")

(with-eval-after-load 'recentf
  (add-to-list 'recentf-exclude git-commit-filename-regexp))

(add-to-list 'with-editor-file-name-history-exclude git-commit-filename-regexp)

(defun git-commit-setup-font-lock-in-buffer ()
  (when (and buffer-file-name
             (string-match-p git-commit-filename-regexp buffer-file-name))
    (git-commit-setup-font-lock)))

(add-hook 'after-change-major-mode-hook #'git-commit-setup-font-lock-in-buffer)

(defun git-commit-setup-check-buffer ()
  (when (and buffer-file-name
             (string-match-p git-commit-filename-regexp buffer-file-name))
    (git-commit-setup)))

(defvar git-commit-mode)

(defun git-commit-file-not-found ()
  ;; cygwin git will pass a cygwin path (/cygdrive/c/foo/.git/...),
  ;; try to handle this in window-nt Emacs.
  (when-let
      ((file (and (or (string-match-p git-commit-filename-regexp
                                      buffer-file-name)
                      (and (boundp 'git-rebase-filename-regexp)
                           (string-match-p git-rebase-filename-regexp
                                           buffer-file-name)))
                  (not (file-accessible-directory-p
                        (file-name-directory buffer-file-name)))
                  (if (require 'magit-git nil t)
                      ;; Emacs prepends a "c:".
                      (magit-expand-git-file-name
                       (substring buffer-file-name 2))
                    ;; Fallback if we can't load `magit-git'.
                    (and (string-match
                          "\\`[a-z]:/\\(cygdrive/\\)?\\([a-z]\\)/\\(.*\\)"
                          buffer-file-name)
                         (concat (match-string 2 buffer-file-name) ":/"
                                 (match-string 3 buffer-file-name)))))))
    (when (file-accessible-directory-p (file-name-directory file))
      (let ((inhibit-read-only t))
        (insert-file-contents file t)
        t))))

(when (eq system-type 'windows-nt)
  (add-hook 'find-file-not-found-functions #'git-commit-file-not-found))

(defconst git-commit-default-usage-message "\
Type \\[with-editor-finish] to finish, \
\\[with-editor-cancel] to cancel, and \
\\[git-commit-prev-message] and \\[git-commit-next-message] \
to recover older messages")

(defvar git-commit-usage-message git-commit-default-usage-message
  "Message displayed when editing a commit message.
When this is nil, then `with-editor-usage-message' is displayed
instead.  One of these messages has to be displayed; otherwise
the user gets to see the message displayed by `server-execute'.
That message is misleading and because we cannot prevent it from
being displayed, we have to immediately show another message to
prevent the user from seeing it.")

(defvar git-commit-header-line-format nil
  "If non-nil, header line format used by `git-commit-mode'.
Used as the local value of `header-line-format', in buffer using
`git-commit-mode'.  If it is a string, then it is passed through
`substitute-command-keys' first.  A useful setting may be:
  (setq git-commit-header-line-format git-commit-default-usage-message)
  (setq git-commit-usage-message nil) ; show a shorter message")

(defun git-commit-setup ()
  (when (fboundp 'magit-toplevel)
    ;; `magit-toplevel' is autoloaded and defined in magit-git.el,
    ;; That library declares this functions without loading
    ;; magit-process.el, which defines it.
    (require 'magit-process nil t))
  ;; Pretend that git-commit-mode is a major-mode,
  ;; so that directory-local settings can be used.
  (let ((default-directory
         (or (and (not (file-exists-p ".dir-locals.el"))
                  ;; When $GIT_DIR/.dir-locals.el doesn't exist,
                  ;; fallback to $GIT_WORK_TREE/.dir-locals.el,
                  ;; because the maintainer can use the latter
                  ;; to enforce conventions, while s/he has no
                  ;; control over the former.
                  (fboundp 'magit-toplevel)  ; silence byte-compiler
                  (magit-toplevel))
             default-directory)))
    (let ((buffer-file-name nil)         ; trick hack-dir-local-variables
          (major-mode 'git-commit-mode)) ; trick dir-locals-collect-variables
      (hack-dir-local-variables)
      (hack-local-variables-apply)))
  (when git-commit-major-mode
    (let ((auto-mode-alist
           ;; `set-auto-mode--apply-alist' removes the remote part from
           ;; the file-name before looking it up in `auto-mode-alist'.
           ;; For our temporary entry to be found, we have to modify the
           ;; file-name the same way.
           (list (cons (concat "\\`"
                               (regexp-quote
                                (or (file-remote-p buffer-file-name 'localname)
                                    buffer-file-name))
                               "\\'")
                       git-commit-major-mode)))
          ;; The major-mode hook might want to consult these minor
          ;; modes, while the minor-mode hooks might want to consider
          ;; the major mode.
          (git-commit-mode t)
          (with-editor-mode t))
      (normal-mode t)))
  ;; Below we instead explicitly show a message.
  (setq with-editor-show-usage nil)
  (unless with-editor-mode
    ;; Maybe already enabled when using `shell-command' or an Emacs shell.
    (with-editor-mode 1))
  (add-hook 'with-editor-finish-query-functions
            #'git-commit-finish-query-functions nil t)
  (add-hook 'with-editor-pre-finish-hook
            #'git-commit-save-message nil t)
  (add-hook 'with-editor-pre-cancel-hook
            #'git-commit-save-message nil t)
  (when (fboundp 'magit-commit--reset-command)
    (add-hook 'with-editor-post-finish-hook #'magit-commit--reset-command)
    (add-hook 'with-editor-post-cancel-hook #'magit-commit--reset-command))
  (when (and (fboundp 'magit-rev-parse)
             (not (memq last-command
                        '(magit-sequencer-continue
                          magit-sequencer-skip
                          magit-am-continue
                          magit-am-skip
                          magit-rebase-continue
                          magit-rebase-skip))))
    (add-hook 'with-editor-post-finish-hook
              (apply-partially #'git-commit-run-post-finish-hook
                               (magit-rev-parse "HEAD"))
              nil t)
    (when (fboundp 'magit-wip-maybe-add-commit-hook)
      (magit-wip-maybe-add-commit-hook)))
  (setq with-editor-cancel-message
        #'git-commit-cancel-message)
  (git-commit-mode 1)
  (git-commit-setup-font-lock)
  (git-commit-prepare-message-ring)
  (when (boundp 'save-place)
    (setq save-place nil))
  (save-excursion
    (goto-char (point-min))
    (when (looking-at "\\`\\(\\'\\|\n[^\n]\\)")
      (open-line 1)))
  (with-demoted-errors "Error running git-commit-setup-hook: %S"
    (run-hooks 'git-commit-setup-hook))
  (when git-commit-usage-message
    (setq with-editor-usage-message git-commit-usage-message))
  (with-editor-usage-message)
  (when-let ((format git-commit-header-line-format))
    (setq header-line-format
          (if (stringp format) (substitute-command-keys format) format)))
  (set-buffer-modified-p nil))

(defun git-commit-run-post-finish-hook (previous)
  (when (and git-commit-post-finish-hook
             (require 'magit nil t)
             (fboundp 'magit-rev-parse))
    (cl-block nil
      (let ((break (time-add (current-time)
                             (seconds-to-time
                              git-commit-post-finish-hook-timeout))))
        (while (equal (magit-rev-parse "HEAD") previous)
          (if (time-less-p (current-time) break)
              (sit-for 0.01)
            (message "No commit created after 1 second.  Not running %s."
                     'git-commit-post-finish-hook)
            (cl-return))))
      (run-hooks 'git-commit-post-finish-hook))))

(define-minor-mode git-commit-mode
  "Auxiliary minor mode used when editing Git commit messages.
This mode is only responsible for setting up some key bindings.
Don't use it directly, instead enable `global-git-commit-mode'."
  :lighter "")

(put 'git-commit-mode 'permanent-local t)

(defun git-commit-setup-changelog-support ()
  "Treat ChangeLog entries as unindented paragraphs."
  (when (fboundp 'log-indent-fill-entry) ; New in Emacs 27.
    (setq-local fill-paragraph-function #'log-indent-fill-entry))
  (setq-local fill-indent-according-to-mode t)
  (setq-local paragraph-start (concat paragraph-start "\\|\\*\\|(")))

(defun git-commit-turn-on-auto-fill ()
  "Unconditionally turn on Auto Fill mode."
  (setq-local comment-auto-fill-only-comments nil)
  (turn-on-auto-fill))

(defun git-commit-turn-on-orglink ()
  "Turn on Orglink mode if it is available.
If `git-commit-major-mode' is `org-mode', then silently forgo
turning on `orglink-mode'."
  (when (and (not (derived-mode-p 'org-mode))
             (boundp 'orglink-match-anywhere)
             (fboundp 'orglink-mode))
    (setq-local orglink-match-anywhere t)
    (orglink-mode 1)))

(defun git-commit-turn-on-flyspell ()
  "Unconditionally turn on Flyspell mode.
Also prevent comments from being checked and
finally check current non-comment text."
  (require 'flyspell)
  (turn-on-flyspell)
  (setq flyspell-generic-check-word-predicate
        #'git-commit-flyspell-verify)
  (let ((end)
        (comment-start-regex (format "^\\(%s\\|$\\)" comment-start)))
    (save-excursion
      (goto-char (point-max))
      (while (and (not (bobp)) (looking-at comment-start-regex))
        (forward-line -1))
      (unless (looking-at comment-start-regex)
        (forward-line))
      (setq end (point)))
    (flyspell-region (point-min) end)))

(defun git-commit-flyspell-verify ()
  (not (= (char-after (line-beginning-position))
          (aref comment-start 0))))

(defun git-commit-finish-query-functions (force)
  (run-hook-with-args-until-failure
   'git-commit-finish-query-functions force))

(defun git-commit-check-style-conventions (force)
  "Check for violations of certain basic style conventions.

For each violation ask the user if she wants to proceed anyway.
Option `git-commit-style-convention-checks' controls which
conventions are checked."
  (or force
      (save-excursion
        (goto-char (point-min))
        (re-search-forward (git-commit-summary-regexp) nil t)
        (if (equal (match-string 1) "")
            t ; Just try; we don't know whether --allow-empty-message was used.
          (and (or (not (memq 'overlong-summary-line
                              git-commit-style-convention-checks))
                   (equal (match-string 2) "")
                   (y-or-n-p "Summary line is too long.  Commit anyway? "))
               (or (not (memq 'non-empty-second-line
                              git-commit-style-convention-checks))
                   (not (match-string 3))
                   (y-or-n-p "Second line is not empty.  Commit anyway? ")))))))

(defun git-commit-cancel-message ()
  (message
   (concat "Commit canceled"
           (and (memq 'git-commit-save-message with-editor-pre-cancel-hook)
                ".  Message saved to `log-edit-comment-ring'"))))

;;; History

(defun git-commit-prev-message (arg)
  "Cycle backward through message history, after saving current message.
With a numeric prefix ARG, go back ARG comments."
  (interactive "*p")
  (let ((len (ring-length log-edit-comment-ring)))
    (if (<= len 0)
        (progn (message "Empty comment ring") (ding))
      ;; Unlike `log-edit-previous-comment' we save the current
      ;; non-empty and newly written comment, because otherwise
      ;; it would be irreversibly lost.
      (when-let ((message (git-commit-buffer-message)))
        (unless (ring-member log-edit-comment-ring message)
          (ring-insert log-edit-comment-ring message)
          (cl-incf arg)
          (setq len (ring-length log-edit-comment-ring))))
      ;; Delete the message but not the instructions at the end.
      (save-restriction
        (goto-char (point-min))
        (narrow-to-region
         (point)
         (if (re-search-forward (concat "^" comment-start) nil t)
             (max 1 (- (point) 2))
           (point-max)))
        (delete-region (point-min) (point)))
      (setq log-edit-comment-ring-index (log-edit-new-comment-index arg len))
      (message "Comment %d" (1+ log-edit-comment-ring-index))
      (insert (ring-ref log-edit-comment-ring log-edit-comment-ring-index)))))

(defun git-commit-next-message (arg)
  "Cycle forward through message history, after saving current message.
With a numeric prefix ARG, go forward ARG comments."
  (interactive "*p")
  (git-commit-prev-message (- arg)))

(defun git-commit-search-message-backward (string)
  "Search backward through message history for a match for STRING.
Save current message first."
  (interactive
   ;; Avoid `format-prompt' because it isn't available until Emacs 28.
   (list (read-string (format "Comment substring (default %s): "
                              log-edit-last-comment-match)
                      nil nil log-edit-last-comment-match)))
  (cl-letf (((symbol-function #'log-edit-previous-comment)
             (symbol-function #'git-commit-prev-message)))
    (log-edit-comment-search-backward string)))

(defun git-commit-search-message-forward (string)
  "Search forward through message history for a match for STRING.
Save current message first."
  (interactive
   ;; Avoid `format-prompt' because it isn't available until Emacs 28.
   (list (read-string (format "Comment substring (default %s): "
                              log-edit-last-comment-match)
                      nil nil log-edit-last-comment-match)))
  (cl-letf (((symbol-function #'log-edit-previous-comment)
             (symbol-function #'git-commit-prev-message)))
    (log-edit-comment-search-forward string)))

(defun git-commit-save-message ()
  "Save current message to `log-edit-comment-ring'."
  (interactive)
  (if-let ((message (git-commit-buffer-message)))
      (progn
        (when-let ((index (ring-member log-edit-comment-ring message)))
          (ring-remove log-edit-comment-ring index))
        (ring-insert log-edit-comment-ring message)
        (when (and git-commit-use-local-message-ring
                   (fboundp 'magit-repository-local-set))
          (magit-repository-local-set 'log-edit-comment-ring
                                      log-edit-comment-ring))
        (message "Message saved"))
    (message "Only whitespace and/or comments; message not saved")))

(defun git-commit-prepare-message-ring ()
  (make-local-variable 'log-edit-comment-ring-index)
  (when (and git-commit-use-local-message-ring
             (fboundp 'magit-repository-local-get))
    (setq-local log-edit-comment-ring
                (magit-repository-local-get
                 'log-edit-comment-ring
                 (make-ring log-edit-maximum-comment-ring-size)))))

(defun git-commit-buffer-message ()
  (let ((flush (concat "^" comment-start))
        (str (buffer-substring-no-properties (point-min) (point-max))))
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (when (re-search-forward (concat flush " -+ >8 -+$") nil t)
        (delete-region (line-beginning-position) (point-max)))
      (goto-char (point-min))
      (flush-lines flush)
      (goto-char (point-max))
      (unless (eq (char-before) ?\n)
        (insert ?\n))
      (setq str (buffer-string)))
    (and (not (string-match "\\`[ \t\n\r]*\\'" str))
         (progn
           (when (string-match "\\`\n\\{2,\\}" str)
             (setq str (replace-match "\n" t t str)))
           (when (string-match "\n\\{2,\\}\\'" str)
             (setq str (replace-match "\n" t t str)))
           str))))

;;; Utilities

(defsubst git-commit-executable ()
  (if (fboundp 'magit-git-executable)
      (magit-git-executable)
    "git"))

;;; Trailers

(transient-define-prefix git-commit-insert-trailer ()
  "Insert a commit message trailer.

See also manpage git-interpret-trailer(1).  This command does
not use that Git command, but the initial description still
serves as a good introduction."
  [[:description (lambda ()
                   (cond (prefix-arg
                          "Insert ... by someone ")
                         ("Insert ... by yourself")))
    ("a"   "Ack"          git-commit-ack)
    ("m"   "Modified"     git-commit-modified)
    ("r"   "Reviewed"     git-commit-review)
    ("s"   "Signed-off"   git-commit-signoff)
    ("t"   "Tested"       git-commit-test)]
   ["Insert ... by someone"
    ("C-c" "Cc"           git-commit-cc)
    ("C-r" "Reported"     git-commit-reported)
    ("C-i" "Suggested"    git-commit-suggested)
    ("C-a" "Co-authored"  git-commit-co-authored)
    ("C-d" "Co-developed" git-commit-co-developed)]])

(defun git-commit-ack (name mail)
  "Insert a trailer acknowledging that you have looked at the commit."
  (interactive (git-commit-get-ident "Acked-by"))
  (git-commit--insert-ident-trailer "Acked-by" name mail))

(defun git-commit-modified (name mail)
  "Insert a trailer to signal that you have modified the commit."
  (interactive (git-commit-get-ident "Modified-by"))
  (git-commit--insert-ident-trailer "Modified-by" name mail))

(defun git-commit-review (name mail)
  "Insert a trailer acknowledging that you have reviewed the commit.
With a prefix argument, prompt for another person who performed a
review."
  (interactive (git-commit-get-ident "Reviewed-by"))
  (git-commit--insert-ident-trailer "Reviewed-by" name mail))

(defun git-commit-signoff (name mail)
  "Insert a trailer to sign off the commit.
With a prefix argument, prompt for another person who signed off."
  (interactive (git-commit-get-ident "Signed-off-by"))
  (git-commit--insert-ident-trailer "Signed-off-by" name mail))

(defun git-commit-test (name mail)
  "Insert a trailer acknowledging that you have tested the commit.
With a prefix argument, prompt for another person who tested."
  (interactive (git-commit-get-ident "Tested-by"))
  (git-commit--insert-ident-trailer "Tested-by" name mail))

(defun git-commit-cc (name mail)
  "Insert a trailer mentioning someone who might be interested."
  (interactive (git-commit-read-ident "Cc"))
  (git-commit--insert-ident-trailer "Cc" name mail))

(defun git-commit-reported (name mail)
  "Insert a trailer mentioning the person who reported the issue."
  (interactive (git-commit-read-ident "Reported-by"))
  (git-commit--insert-ident-trailer "Reported-by" name mail))

(defun git-commit-suggested (name mail)
  "Insert a trailer mentioning the person who suggested the change."
  (interactive (git-commit-read-ident "Suggested-by"))
  (git-commit--insert-ident-trailer "Suggested-by" name mail))

(defun git-commit-co-authored (name mail)
  "Insert a trailer mentioning the person who co-authored the commit."
  (interactive (git-commit-read-ident "Co-authored-by"))
  (git-commit--insert-ident-trailer "Co-authored-by" name mail))

(defun git-commit-co-developed (name mail)
  "Insert a trailer mentioning the person who co-developed the commit."
  (interactive (git-commit-read-ident "Co-developed-by"))
  (git-commit--insert-ident-trailer "Co-developed-by" name mail))

(defun git-commit-get-ident (&optional prompt)
  "Return name and email of the user or read another name and email.
If PROMPT and `current-prefix-arg' are both non-nil, read name
and email using `git-commit-read-ident' (which see), otherwise
return name and email of the current user (you)."
  (if (and prompt current-prefix-arg)
      (git-commit-read-ident prompt)
    (list (or (getenv "GIT_AUTHOR_NAME")
              (getenv "GIT_COMMITTER_NAME")
              (with-demoted-errors "Error running 'git config user.name': %S"
                (car (process-lines
                      (git-commit-executable) "config" "user.name")))
              user-full-name
              (read-string "Name: "))
          (or (getenv "GIT_AUTHOR_EMAIL")
              (getenv "GIT_COMMITTER_EMAIL")
              (getenv "EMAIL")
              (with-demoted-errors "Error running 'git config user.email': %S"
                (car (process-lines
                      (git-commit-executable) "config" "user.email")))
              (read-string "Email: ")))))

(defalias 'git-commit-self-ident #'git-commit-get-ident)

(defvar git-commit-read-ident-history nil)

(defun git-commit-read-ident (prompt)
  "Read a name and email, prompting with PROMPT, and return them.
If Magit is available, read them using a single prompt, offering
past commit authors as completion candidates.  The input must
have the form \"NAME <EMAIL>\"."
  (if (require 'magit-git nil t)
      (let ((str (magit-completing-read
                  prompt
                  (sort (delete-dups
                         (magit-git-lines "log" "-n9999" "--format=%aN <%ae>"))
                        #'string<)
                  nil nil nil 'git-commit-read-ident-history)))
        (save-match-data
          (if (string-match "\\`\\([^<]+\\) *<\\([^>]+\\)>\\'" str)
              (list (save-match-data (string-trim (match-string 1 str)))
                    (string-trim (match-string 2 str)))
            (user-error "Invalid input"))))
    (list (read-string "Name: ")
          (read-string "Email: "))))

(defun git-commit--insert-ident-trailer (trailer name email)
  (git-commit--insert-trailer trailer (format "%s <%s>" name email)))

(defun git-commit--insert-trailer (trailer value)
  (save-excursion
    (let ((string (format "%s: %s" trailer value))
          (leading-comment-end nil))
      ;; Make sure we skip forward past any leading comments.
      (goto-char (point-min))
      (while (looking-at comment-start)
        (forward-line))
      (setq leading-comment-end (point))
      (goto-char (point-max))
      (cond
       ;; Look backwards for existing trailers.
       ((re-search-backward (git-commit--trailer-regexp) nil t)
        (end-of-line)
        (insert ?\n string)
        (unless (= (char-after) ?\n)
          (insert ?\n)))
       ;; Or place the new trailer right before the first non-leading
       ;; comments.
       (t
        (while (re-search-backward (concat "^" comment-start)
                                   leading-comment-end t))
        (unless (looking-back "\n\n" nil)
          (insert ?\n))
        (insert string ?\n))))
    (unless (or (eobp) (= (char-after) ?\n))
      (insert ?\n))))

;;; Font-Lock

(defvar-local git-commit-need-summary-line t
  "Whether the text should have a heading that is separated from the body.

For commit messages that is a convention that should not
be violated.  For notes it is up to the user.  If you do
not want to insist on an empty second line here, then use
something like:

  (add-hook \\='git-commit-setup-hook
            (lambda ()
              (when (equal (file-name-nondirectory (buffer-file-name))
                           \"NOTES_EDITMSG\")
                (setq git-commit-need-summary-line nil))))")

(defun git-commit--trailer-regexp ()
  (format
   "^\\(?:\\(%s:\\)\\( .*\\)\\|\\([-a-zA-Z]+\\): \\([^<\n]+? <[^>\n]+>\\)\\)"
   (regexp-opt git-commit-trailers)))

(defun git-commit-summary-regexp ()
  (if git-commit-need-summary-line
      (concat
       ;; Leading empty lines and comments
       (format "\\`\\(?:^\\(?:\\s-*\\|%s.*\\)\n\\)*" comment-start)
       ;; Summary line
       (format "\\(.\\{0,%d\\}\\)\\(.*\\)" git-commit-summary-max-length)
       ;; Non-empty non-comment second line
       (format "\\(?:\n%s\\|\n\\(.+\\)\\)?" comment-start))
    "\\(EASTER\\) \\(EGG\\)"))

(defun git-commit-extend-region-summary-line ()
  "Identify the multiline summary-regexp construct.
Added to `font-lock-extend-region-functions'."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (when (looking-at (git-commit-summary-regexp))
        (let ((summary-beg (match-beginning 0))
              (summary-end (match-end 0)))
          (when (or (< summary-beg font-lock-beg summary-end)
                    (< summary-beg font-lock-end summary-end))
            (setq font-lock-beg (min font-lock-beg summary-beg))
            (setq font-lock-end (max font-lock-end summary-end))))))))

(defvar-local git-commit--branch-name-regexp nil)

(defconst git-commit-comment-headings
  '("Changes to be committed:"
    "Untracked files:"
    "Changed but not updated:"
    "Changes not staged for commit:"
    "Unmerged paths:"
    "Author:"
    "Date:")
  "Also fontified outside of comments in `git-commit-font-lock-keywords-2'.")

(defconst git-commit-font-lock-keywords-1
  '(;; Trailers
    (eval . `(,(git-commit--trailer-regexp)
              (1 'git-commit-trailer-token)
              (2 'git-commit-trailer-value)
              (3 'git-commit-trailer-token)
              (4 'git-commit-trailer-value)))
    ;; Summary
    (eval . `(,(git-commit-summary-regexp)
              (1 'git-commit-summary)))
    ;; - Keyword [aka "text in brackets"] (overrides summary)
    ("\\[[^][]+?\\]"
     (0 'git-commit-keyword t))
    ;; - Non-empty second line (overrides summary and note)
    (eval . `(,(git-commit-summary-regexp)
              (2 'git-commit-overlong-summary t t)
              (3 'git-commit-nonempty-second-line t t)))))

(defconst git-commit-font-lock-keywords-2
  `(,@git-commit-font-lock-keywords-1
    ;; Comments
    (eval . `(,(format "^%s.*" comment-start)
              (0 'font-lock-comment-face append)))
    (eval . `(,(format "^%s On branch \\(.*\\)" comment-start)
              (1 'git-commit-comment-branch-local t)))
    (eval . `(,(format "^%s \\(HEAD\\) detached at" comment-start)
              (1 'git-commit-comment-detached t)))
    (eval . `(,(format "^%s %s" comment-start
                       (regexp-opt git-commit-comment-headings t))
              (1 'git-commit-comment-heading t)))
    (eval . `(,(format "^%s\t\\(?:\\([^:\n]+\\):\\s-+\\)?\\(.*\\)" comment-start)
              (1 'git-commit-comment-action t t)
              (2 'git-commit-comment-file t)))
    ;; "commit HASH"
    (eval . '("^commit [[:alnum:]]+$"
              (0 'git-commit-trailer-value)))
    ;; `git-commit-comment-headings' (but not in commented lines)
    (eval . `(,(format "\\(?:^%s[[:blank:]]+.+$\\)"
                       (regexp-opt git-commit-comment-headings))
              (0 'git-commit-trailer-value)))))

(defconst git-commit-font-lock-keywords-3
  `(,@git-commit-font-lock-keywords-2
    ;; More comments
    (eval
     ;; Your branch is ahead of 'master' by 3 commits.
     ;; Your branch is behind 'master' by 2 commits, and can be fast-forwarded.
     . `(,(format
           "^%s Your branch is \\(?:ahead\\|behind\\) of '%s' by \\([0-9]*\\)"
           comment-start git-commit--branch-name-regexp)
         (1 'git-commit-comment-branch-local t)
         (2 'git-commit-comment-branch-remote t)
         (3 'bold t)))
    (eval
     ;; Your branch is up to date with 'master'.
     ;; Your branch and 'master' have diverged,
     . `(,(format
           "^%s Your branch \\(?:is up[- ]to[- ]date with\\|and\\) '%s'"
           comment-start git-commit--branch-name-regexp)
         (1 'git-commit-comment-branch-local t)
         (2 'git-commit-comment-branch-remote t)))
    (eval
     ;; and have 1 and 2 different commits each, respectively.
     . `(,(format
           "^%s and have \\([0-9]*\\) and \\([0-9]*\\) commits each"
           comment-start)
         (1 'bold t)
         (2 'bold t)))))

(defvar git-commit-font-lock-keywords git-commit-font-lock-keywords-3
  "Font-Lock keywords for Git-Commit mode.")

(defun git-commit-setup-font-lock ()
  (with-demoted-errors "Error running git-commit-setup-font-lock: %S"
    (let ((table (make-syntax-table (syntax-table))))
      (when comment-start
        (modify-syntax-entry (string-to-char comment-start) "." table))
      (modify-syntax-entry ?#  "." table)
      (modify-syntax-entry ?\" "." table)
      (modify-syntax-entry ?\' "." table)
      (modify-syntax-entry ?`  "." table)
      (set-syntax-table table))
    (setq-local comment-start
                (or (with-temp-buffer
                      (and (zerop
                            (call-process
                             (git-commit-executable) nil (list t nil) nil
                             "config" "core.commentchar"))
                           (not (bobp))
                           (progn
                             (goto-char (point-min))
                             (buffer-substring (point) (line-end-position)))))
                    "#"))
    (setq-local comment-start-skip (format "^%s+[\s\t]*" comment-start))
    (setq-local comment-end "")
    (setq-local comment-end-skip "\n")
    (setq-local comment-use-syntax nil)
    (when (and (derived-mode-p 'markdown-mode)
               (fboundp 'markdown-fill-paragraph))
      (setq-local fill-paragraph-function
                  (lambda (&optional justify)
                    (and (not (= (char-after (line-beginning-position))
                                 (aref comment-start 0)))
                         (markdown-fill-paragraph justify)))))
    (setq-local git-commit--branch-name-regexp
                (if (and (featurep 'magit-git)
                         ;; When using cygwin git, we may end up in a
                         ;; non-existing directory, which would cause
                         ;; any git calls to signal an error.
                         (file-accessible-directory-p default-directory))
                    (progn
                      ;; Make sure the below functions are available.
                      (require 'magit)
                      ;; Font-Lock wants every submatch to succeed, so
                      ;; also match the empty string.  Avoid listing
                      ;; remote branches and using `regexp-quote',
                      ;; because in repositories have thousands of
                      ;; branches that would be very slow.  See #4353.
                      (format "\\(\\(?:%s\\)\\|\\)\\([^']+\\)"
                              (mapconcat #'identity
                                         (magit-list-local-branch-names)
                                         "\\|")))
                  "\\([^']*\\)"))
    (setq-local font-lock-multiline t)
    (add-hook 'font-lock-extend-region-functions
              #'git-commit-extend-region-summary-line
              t t)
    (font-lock-add-keywords nil git-commit-font-lock-keywords)))

(defun git-commit-propertize-diff ()
  (require 'diff-mode)
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward "^diff --git" nil t)
      (beginning-of-line)
      (let ((buffer (current-buffer)))
        (insert
         (with-temp-buffer
           (insert
            (with-current-buffer buffer
              (prog1 (buffer-substring-no-properties (point) (point-max))
                (delete-region (point) (point-max)))))
           (let ((diff-default-read-only nil))
             (diff-mode))
           (let (font-lock-verbose font-lock-support-mode)
             (if (fboundp 'font-lock-ensure)
                 (font-lock-ensure)
               (with-no-warnings
                 (font-lock-fontify-buffer))))
           (let (next (pos (point-min)))
             (while (setq next (next-single-property-change pos 'face))
               (put-text-property pos next 'font-lock-face
                                  (get-text-property pos 'face))
               (setq pos next))
             (put-text-property pos (point-max) 'font-lock-face
                                (get-text-property pos 'face)))
           (buffer-string)))))))

;;; Elisp Text Mode

(define-derived-mode git-commit-elisp-text-mode text-mode "ElText"
  "Major mode for editing commit messages of elisp projects.
This is intended for use as `git-commit-major-mode' for projects
that expect `symbols' to look like this.  I.e., like they look in
Elisp doc-strings, including this one.  Unlike in doc-strings,
\"strings\" also look different than the other text."
  (setq font-lock-defaults '(git-commit-elisp-text-mode-keywords)))

(defvar git-commit-elisp-text-mode-keywords
  `((,(concat "[`‘]\\(" lisp-mode-symbol-regexp "\\)['’]")
     (1 font-lock-constant-face prepend))
    ("\"[^\"]*\"" (0 font-lock-string-face prepend))))

;;; _

(define-obsolete-function-alias
  'git-commit-insert-pseudo-header
  'git-commit-insert-trailer
  "git-commit 4.0.0")
(define-obsolete-function-alias
  'git-commit-insert-header
  'git-commit--insert-ident-trailer
  "git-commit 4.0.0")
(define-obsolete-variable-alias
  'git-commit-known-pseudo-headers
  'git-commit-trailer
  "git-commit 4.0.0")
(define-obsolete-face-alias
 'git-commit-pseudo-header
 'git-commit-trailer-value
 "git-commit 4.0.0")
(define-obsolete-face-alias
 'git-commit-known-pseudo-header
 'git-commit-trailer-token
 "git-commit 4.0.0")

(provide 'git-commit)
;;; git-commit.el ends here
