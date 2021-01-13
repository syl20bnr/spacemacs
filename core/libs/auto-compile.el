;;; auto-compile.el --- automatically compile Emacs Lisp libraries  -*- lexical-binding: t -*-

;; Copyright (C) 2008-2020  Jonas Bernoulli

;; Author: Jonas Bernoulli <jonas@bernoul.li>
;; Homepage: https://github.com/emacscollective/auto-compile
;; Keywords: compile, convenience, lisp

;; Package-Requires: ((emacs "25.1") (packed "3.0.1"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides two minor modes which automatically recompile
;; Emacs Lisp source files.  Together these modes guarantee that Emacs
;; never loads outdated byte code files.

;; `auto-compile-on-save-mode' re-compiles source files when they are
;; being saved and `auto-compile-on-load-mode' does so before they are
;; being loaded (by advising `load' and `require').  Both modes only
;; ever _re-compile_ a source file when the respective byte code file
;; already exists but is outdated.  Otherwise they do _not_ compile
;; the source file.

;; Even when using `auto-compile-on-save-mode' it can happen that some
;; source file is newer than the respective byte code file, which is a
;; problem because by default Emacs loads the byte code file even when
;; the respective source file has been modified more recently.

;; Starting with Emacs version 24.4, setting `load-prefer-newer' to t
;; prevents outdated byte code files from being loaded.  However this
;; does not cause re-compilation of the source file, to actually do
;; that `auto-compile-on-load-mode' is still required.

;; Setup
;; -----

;; To reduce the risk of loading outdated byte code files, you should
;; set `load-prefer-newer' and enable `auto-compile-on-load-mode' as
;; early as possible.  Then also enable `auto-compile-on-save-mode'.
;; You should also consider not byte-compiling your personal init
;; file, or setting `load-prefer-newer' in a system-wide init file.

;; If you use `package.el' then use something like this:
;;
;;     ;;; init.el --- user init file
;;     (setq load-prefer-newer t)
;;     (package-initialize)
;;     (require 'auto-compile)
;;     (auto-compile-on-load-mode)
;;     (auto-compile-on-save-mode)

;; otherwise:
;;
;;     ;;; init.el --- user init file
;;     (setq load-prefer-newer t)
;;     (add-to-list 'load-path "/path/to/packed")
;;     (add-to-list 'load-path "/path/to/auto-compile")
;;     (require 'auto-compile)
;;     (auto-compile-on-load-mode)
;;     (auto-compile-on-save-mode)

;; You might want to set the file-local value of `no-byte-compile' to
;; t, e.g. by adding "-*- no-byte-compile: t -*-" (without the quotes)
;; at the end of the very first line.  That way all user files benefit
;; from the protection offered by `load-prefer-newer' and the modes
;; that are defined here, otherwise `~/.emacs.d/init.el' is the only
;; exception.

;; If you are using Emacs 27 or later, then these settings should be
;; placed in `early-init.el', which should never be compiled:

;;     ;;; early-init.el --- early bird  -*- no-byte-compile: t -*-
;;     (setq load-prefer-newer t)
;;     (add-to-list 'load-path "/path/to/packed")
;;     (add-to-list 'load-path "/path/to/auto-compile")
;;     (require 'auto-compile)
;;     (auto-compile-on-load-mode)
;;     (auto-compile-on-save-mode)
;;     ;;; early-init.el ends here

;; Usage
;; -----

;; Take note of the compile warnings and fix them.

;; To permanently or temporarily toggle automatic compilation of some
;; source file use the command `toggle-auto-compile'.  Since the modes
;; only ever _update_ byte code files, toggling automatic compilation
;; is done simply by either creating the byte code file or by removing
;; it.  `toggle-auto-compile' can also toggle automatic compilation of
;; multiple files at once; see its doc-string for more information.

;; Customization
;; -------------

;; Constantly having the *Compile-Log* buffer pop up when a file is
;; being saved can quickly become annoying.  Obviously the first thing
;; you should do to about that is to actually fix outstanding issues.

;; Once you have done that you might also want to keep that buffer
;; from being automatically displayed and instead only show the number
;; of compile warnings for the current file in the mode-line.

;;     (setq auto-compile-display-buffer nil)
;;     (setq auto-compile-mode-line-counter t)

;; To display the buffer use `M-x auto-compile-display-log' or click
;; on the counter in the mode-line.

;; Using `auto-compile-inhibit-compile-hook' it is possible to inhibit
;; automatic compilation under certain circumstances; e.g. when HEAD
;; is detached inside a Git repository (useful during rebase sessions).

;;; Code:

(require 'bytecomp)
(require 'cl-lib)
(require 'packed)

(declare-function autoload-rubric "autoload")
(declare-function autoload-find-destination "autoload")
(declare-function autoload-file-load-name "autoload")
(declare-function autoload-generate-file-autoloads "autoload")

(defvar autoload-modified-buffers)
(defvar warning-minimum-level)

(defvar auto-compile-use-mode-line)

(defgroup auto-compile nil
  "Automatically compile Emacs Lisp source libraries."
  :group 'convenience
  :prefix 'auto-compile
  :link '(function-link toggle-auto-compile)
  :link '(function-link auto-compile-mode))

;;; Auto-Compile-On-Save Mode

;;;###autoload
(define-minor-mode auto-compile-mode
  "Compile Emacs Lisp source files after the visiting buffers are saved.

After a buffer containing Emacs Lisp code is saved to its source
file update the respective byte code file.  If the latter does
not exist do nothing.  Therefore to disable automatic compilation
remove the byte code file.  See command `toggle-auto-compile' for
a convenient way to do so.

This mode should be enabled globally, using it's globalized
variant `auto-compile-on-save-mode'.  Also see the related
`auto-compile-on-load-mode'."
  :lighter auto-compile-mode-lighter
  :group 'auto-compile
  (unless (derived-mode-p 'emacs-lisp-mode)
    (user-error "This mode only makes sense with emacs-lisp-mode"))
  (if auto-compile-mode
      (add-hook  'after-save-hook 'auto-compile-byte-compile nil t)
    (remove-hook 'after-save-hook 'auto-compile-byte-compile t))
  (auto-compile-modify-mode-line auto-compile-use-mode-line))

;;;###autoload
(define-globalized-minor-mode auto-compile-on-save-mode
  auto-compile-mode turn-on-auto-compile-mode)

(defun turn-on-auto-compile-mode ()
  (when (eq major-mode 'emacs-lisp-mode)
    (auto-compile-mode 1)))

(defvar auto-compile-mode-lighter ""
  "Mode lighter for Auto-Compile Mode.")

;;; Options

(defcustom auto-compile-visit-failed t
  "Whether to visit source files which failed to compile.

If this is non-nil visit but don't select a source file if it
isn't being visited in a buffer already.  Also set the buffer
local value of variable `auto-compile-pretend-byte-compiled'
\(which see) to t and mark the buffer as modified if the value
of variable `auto-compile-mark-failed-modified' is non-nil."
  :group 'auto-compile
  :type 'boolean)

(defcustom auto-compile-mark-failed-modified nil
  "Whether to mark buffers which failed to compile as modified.

This serves as a reminder to fix fatal errors.  While useful this
can get annoying so this variable can be quickly toggled with the
command `auto-compile-toggle-mark-failed-modified'."
  :group 'auto-compile
  :type 'boolean)

(defcustom auto-compile-ding t
  "Whether to beep (or flash the screen) when an error occurs.

Expected errors (such as compile error, unmatched parens, or
failure to remove a file) are caught and execution continues so
that failure to process one file does not prevent other files
from being processed.

To inform users of such errors Auto-Compile instead beeps or
flashes the screen; set this variable to nil to disable even
that."
  :group 'auto-compile
  :type 'boolean)

(defcustom auto-compile-check-parens t
  "Whether to check for unbalanced parentheses before compiling.

This only has as an effect on files which are currently being
visited in a buffer.  Other files are compiled without performing
this check first.  If unbalanced parentheses are found no attempt
is made to compile the file as that would obviously fail also."
  :group 'auto-compile
  :type 'boolean)

(defcustom auto-compile-update-autoloads nil
  "Whether to update autoloads after compiling.

If no autoload file as specified by `packed-loaddefs-filename' can be
found quietly skip this step."
  :group 'auto-compile
  :type 'boolean)

(defcustom auto-compile-inhibit-compile-hook nil
  "Hook used to inhibit automatic compilation.

This hook is run before automatic compilation takes place, if
any of the hook functions returns non-nil, then do not compile."
  :group 'auto-compile
  :options '(auto-compile-inhibit-compile-detached-git-head)
  :type 'hook)

(defcustom auto-compile-verbose nil
  "Whether to print messages describing progress of byte-compiler.

This overrides `byte-compile-verbose' but unlike that does not
default to t, and thus avoids unnecessary echo-area messages."
  :group 'auto-compile
  :type 'boolean)

(defcustom auto-compile-display-buffer t
  "Whether to automatically display the *Compile-Log* buffer.

When there are errors then the buffer is always displayed,
when there are no warnings or errors it is never displayed."
  :group 'auto-compile
  :type 'boolean)

(defcustom auto-compile-mode-line-counter nil
  "Whether to display the number of warnings in the mode line.

This assumes that `auto-compile-use-mode-line' (which see) is
non-nil."
  :group 'auto-compile
  :type 'boolean)

(defun auto-compile-modify-mode-line (after)
  (let ((format (delete 'mode-line-auto-compile
                        (default-value 'mode-line-format)))
        cell)
    (when (and after auto-compile-mode
               (setq cell (member after format)))
      (push 'mode-line-auto-compile (cdr cell)))
    (set-default 'mode-line-format format)))

(defcustom auto-compile-use-mode-line
  (car (memq 'mode-line-modified (default-value 'mode-line-format)))
  "Whether to show information about the byte code file in the mode line.

This works by inserting `mode-line-auto-compile' into the default
value of `mode-line-format' after the construct (usually a symbol)
specified here.  This happens every time local Auto-Compile mode
is turned on so the specified construct does not have to a member
of `mode-line-format' when this is set (this allows loading that
package after `auto-compile-on-load-mode' has been activated, so
that it can ensures the respective byte code file is up-to-date).

If you want to add `mode-line-auto-compile' as a member of a
variable that is itself a member of `mode-line-format' then you
have to set this option to nil and manually modify that variable
to include `mode-line-auto-compile'."
  :group 'auto-compile
  :set (lambda (symbol value)
         (set-default symbol value)
         (auto-compile-modify-mode-line value))
  :type '(choice (const :tag "don't insert" nil)
                 (const :tag "after mode-line-modified" mode-line-modified)
                 (const :tag "after mode-line-remote" mode-line-remote)
                 (sexp  :tag "after construct")))

(defcustom auto-compile-toggle-recompiles t
  "Whether to recompile all source files when turning on compilation.

When turning on auto compilation for multiple files at once
recompile source files even if their byte code file already
exist and are up-to-date.  It's advisable to keep this enabled
to ensure changes to macros are picked up."
  :group 'auto-compile
  :type 'boolean)

(defcustom auto-compile-delete-stray-dest t
  "Whether to remove stray byte code files.

If this is non-nil byte code files without a corresponding source
file are removed as they are encountered.  This happens in the
functions `auto-compile-on-save' and `toggle-auto-compile'.  The
main purpose of this functionality is to prevent leftover byte
code files from shadowing a source or byte code file in a
directory that comes later in the `load-path'."
  :group 'auto-compile
  :type 'boolean)

(defcustom auto-compile-toggle-deletes-nonlib-dest nil
  "Whether to delete non-library byte code files when toggling compilation."
  :group 'auto-compile
  :type 'boolean)

(defcustom auto-compile-source-recreate-deletes-dest nil
  "Whether to delete leftover byte code file when creating source file.

When this is non-nil and saving a source buffer causes the file
to be created (as opposed to being overwritten) while its byte
code file already exists (because the source already existed and
was compiled in the past), then remove the latter (instead of
updating it by recompiling the source).  This can e.g. happen
when switching git branches."
  :group 'auto-compile
  :type 'boolean)

;;; Toggle and Perform Compilation

;;;###autoload
(defun toggle-auto-compile (file action)
  "Toggle automatic compilation of an Emacs Lisp source file or files.

Read a file or directory name from the minibuffer defaulting to
the visited Emacs Lisp source file or `default-directory' if no
such file is being visited in the current buffer.

If the user selects a file then automatic compilation of only
that file is toggled.  Since both `auto-compile-on-save' and
`auto-compile-on-save' only ever _recompile_ byte code files,
toggling automatic compilation is done simply by creating or
removing the respective byte code file.

If the user selects a directory then automatic compilation for
multiple files is toggled as follows:

* With a positive prefix argument always compile source files;
  with a negative prefix argument always remove byte code files.

* Otherwise the existence or absence of the byte code file of
  the source file that was current when this command was invoked
  determines whether byte code files should be created or removed.

* If no Emacs Lisp source file is being visited in the buffer
  that was current when the command was invoked ask the user what
  to do.

* When _removing_ byte code files then all byte code files are
  removed.  If `auto-compile-deletes-stray-dest' is non-nil this
  even includes byte code files for which no source file exists.

* When _creating_ byte code files only do so for source files
  that are actual libraries.  Source files that provide the
  correct feature are considered to be libraries; see
  `packed-library-p'.

* Note that non-libraries can still be automatically compiled,
  you just cannot _recursively_ turn on automatic compilation
  using this command.

* When `auto-compile-toggle-recompiles' is non-nil recompile all
  affected source files even when the respective source files are
  up-to-date.  Do so even for non-library source files.

* Only enter subdirectories for which `packed-ignore-directory-p'
  returns nil; i.e. don't enter hidden directories or directories
  containing a file named \".nosearch\"."
  (interactive
   (let* ((file (and (eq major-mode 'emacs-lisp-mode)
                     (buffer-file-name)))
          (action
           (cond
            (current-prefix-arg
             (if (> (prefix-numeric-value current-prefix-arg) 0)
                 'start
               'quit))
            (file
             (if (file-exists-p (byte-compile-dest-file file))
                 'quit
               'start))
            (t
             (pcase (read-char-choice
                     "Toggle automatic compilation (s=tart, q=uit, C-g)? "
                     '(?s ?q))
               (?s 'start)
               (?q 'quit))))))
     (list (read-file-name (concat (capitalize (symbol-name action))
                                   " auto-compiling: ")
                           (and file (file-name-directory file))
                           nil t
                           (and file (file-name-nondirectory file)))
           action)))
  (if (file-regular-p file)
      (pcase action
        (`start (auto-compile-byte-compile file t))
        (`quit  (auto-compile-delete-dest (byte-compile-dest-file file))))
    (when (called-interactively-p 'any)
      (let ((buffer (get-buffer byte-compile-log-buffer)))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))
    (dolist (f (directory-files file t))
      (cond
       ((file-directory-p f)
        (unless (packed-ignore-directory-p f)
          (toggle-auto-compile f action)))
       ((packed-library-p f)
        (let ((dest (byte-compile-dest-file f)))
          (if (eq action 'start)
              (and (file-exists-p f)
                   (or auto-compile-toggle-recompiles
                       (file-newer-than-file-p f dest))
                   (or (not (string-match "^\\.?#" (file-name-nondirectory f)))
                       (file-exists-p dest))
                   (auto-compile-byte-compile f t))
            (auto-compile-delete-dest dest))))
       ((and auto-compile-toggle-deletes-nonlib-dest
             (eq action 'quit)
             (string-match (packed-el-regexp) f))
        (auto-compile-delete-dest (byte-compile-dest-file f)))
       ((and auto-compile-delete-stray-dest
             (string-match "\\.elc$" f)
             (not (file-exists-p (packed-el-file f))))
        (auto-compile-delete-dest f))))))

(defalias 'auto-compile-toggle 'toggle-auto-compile)

(defun auto-compile-toggle-mark-failed-modified ()
  "Toggle whether buffers which failed to compile are marked as modified."
  (interactive)
  (message (concat (if (setq auto-compile-mark-failed-modified
                             (not auto-compile-mark-failed-modified))
                       "Mark "
                     "Don't mark ")
                   "files that failed to compile as modified")))

(defvar-local auto-compile-pretend-byte-compiled nil
  "Whether to try again to compile this file after a failed attempt.

Command `auto-compile-byte-compile' sets this buffer local
variable to t after failing to compile a source file being
visited in a buffer (or when variable `auto-compile-visit-failed'
is non-nil for all files being compiled) causing it to try again
when being called again. Command `toggle-auto-compile' will also
pretend the byte code file exists.")

(defvar auto-compile-file-buffer nil)
(defvar-local auto-compile-warnings 0)

(define-advice byte-compile-log-warning
    (:before (_string &optional _fill _level) auto-compile)
  "Increment local value of `auto-compile-warnings'."
  (when auto-compile-file-buffer
    (with-current-buffer auto-compile-file-buffer
      (cl-incf auto-compile-warnings))))

(cl-defun auto-compile-byte-compile (&optional file start)
  "Perform byte compilation for Auto-Compile mode."
  (when (run-hook-with-args-until-success 'auto-compile-inhibit-compile-hook)
    (cl-return-from auto-compile-byte-compile))
  (let ((default-directory default-directory)
        dest buf auto-compile-file-buffer success loaddefs)
    (when (and file
               (setq buf (get-file-buffer file))
               (buffer-modified-p buf)
               (y-or-n-p (format "Save buffer %s first? " (buffer-name buf))))
      (with-current-buffer buf (save-buffer)))
    (unless file
      (setq file (buffer-file-name))
      (setq buf  (get-file-buffer file)))
    (setq default-directory (file-name-directory file))
    (setq auto-compile-file-buffer buf)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq auto-compile-warnings 0)))
    (catch 'auto-compile
      (when (and auto-compile-check-parens buf)
        (condition-case check-parens
            (save-restriction
              (widen)
              (check-parens))
          (error
           (message (error-message-string check-parens))
           (auto-compile-handle-compile-error file buf start)
           (throw 'auto-compile nil))))
      (setq dest (byte-compile-dest-file file))
      (when (or start
                (and (file-exists-p dest)
                     (or (file-exists-p file)
                         (not auto-compile-source-recreate-deletes-dest)
                         (prog1 nil
                           (auto-compile-delete-dest dest))))
                (and (buffer-live-p buf)
                     (buffer-local-value auto-compile-pretend-byte-compiled
                                         buf)))
        (condition-case nil
            (let ((byte-compile-verbose auto-compile-verbose)
                  (warning-minimum-level
                   (if auto-compile-display-buffer :warning :error)))
              (setq success (packed-byte-compile-file file))
              (when (buffer-live-p buf)
                (with-current-buffer buf
                  (kill-local-variable auto-compile-pretend-byte-compiled))))
          (file-error
           (message "Byte-compiling %s failed" file)
           (auto-compile-handle-compile-error file buf start)
           (setq success nil)))
        (when (and auto-compile-update-autoloads
                   (setq loaddefs (packed-loaddefs-file)))
          (require 'autoload)
          (condition-case nil
              (packed-with-loaddefs loaddefs
                (let ((autoload-modified-buffers nil))
                  (autoload-generate-file-autoloads
                   file nil generated-autoload-file)))
            (error
             (message "Generating loaddefs for %s failed" file)
             (setq loaddefs nil))))
        (pcase success
          (`no-byte-compile)
          (`t (message "Wrote %s.{%s,%s}%s"
                        (file-name-sans-extension
                         (file-name-sans-extension file))
                        (progn (string-match "\\(\\.[^./]+\\)+$" file)
                               (substring (match-string 0 file) 1))
                        (file-name-extension dest)
                        (if loaddefs " (+)" "")))
          (_  (message "Wrote %s (byte-compiling failed)" file))))
      success)))

(defun auto-compile-delete-dest (dest &optional failurep)
  (unless failurep
    (let ((buffer (get-file-buffer (packed-el-file dest))))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (kill-local-variable 'auto-compile-pretend-byte-compiled)))))
  (condition-case nil
      (when (file-exists-p dest)
        (message "Deleting %s..." dest)
        (delete-file dest)
        (message "Deleting %s...done" dest))
    (file-error
     (auto-compile-ding)
     (message "Deleting %s...failed" dest))))

(defun auto-compile-handle-compile-error (file buf &optional start)
  (auto-compile-ding)
  (let (update)
    (let ((dest (byte-compile-dest-file file)))
      (when (file-exists-p dest)
        (setq update t)
        (auto-compile-delete-dest dest t)))
    (when (or buf
              (and auto-compile-visit-failed
                   (setq buf (find-file-noselect file))))
      (with-current-buffer buf
        (when (or update start)
          (setq auto-compile-pretend-byte-compiled t))
        (when auto-compile-mark-failed-modified
          (set-buffer-modified-p t))))))

(defun auto-compile-handle-autoloads-error (dest)
  (auto-compile-ding)
  (packed-remove-autoloads dest nil))

(defun auto-compile-ding ()
  (when auto-compile-ding
    (ding)))

(define-advice save-buffers-kill-emacs
    (:around (fn &optional arg) auto-compile)
  "Bind `auto-compile-mark-failed-modified' to nil when killing Emacs.
If the regular value of this variable is non-nil the user might
still be asked whether she wants to save modified buffers, which
she actually did already safe.  This advice ensures she at least
is only asked once about each such file."
  (let ((auto-compile-mark-failed-modified nil))
    (funcall fn arg)))

(define-advice save-buffers-kill-terminal
    (:around (fn &optional arg) auto-compile)
  "Bind `auto-compile-mark-failed-modified' to nil when killing Emacs.
If the regular value of this variable is non-nil the user might
still be asked whether she wants to save modified buffers, which
she actually did already safe.  This advice ensures she at least
is only asked once about each such file."
  (let ((auto-compile-mark-failed-modified nil))
    (funcall fn arg)))

(defun auto-compile-inhibit-compile-detached-git-head ()
  "Inhibit compiling in Git repositories when `HEAD' is detached.
This is especially useful during rebase sessions."
  (with-temp-buffer
    (call-process "git" nil t nil "symbolic-ref" "HEAD")
    (equal (buffer-string) "fatal: ref HEAD is not a symbolic ref\n")))

;;; Mode-Line

(defvar-local mode-line-auto-compile
  '(auto-compile-mode (:eval (mode-line-auto-compile-control))))
(put 'mode-line-auto-compile 'risky-local-variable t)

(defun mode-line-auto-compile-control ()
  (let ((src (buffer-file-name))
        dst)
    (when (and src (setq dst (byte-compile-dest-file src)))
      (list
       (when (and auto-compile-mode-line-counter
                  (> auto-compile-warnings 0))
         (propertize
          (format "%s" auto-compile-warnings)
          'help-echo (format "%s compile warnings\nmouse-1 display compile log"
                             auto-compile-warnings)
          'face 'error
          'mouse-face 'mode-line-highlight
          'local-map (purecopy (make-mode-line-mouse-map
                                'mouse-1
                                #'auto-compile-display-log))))
       (cond
        ((file-writable-p dst)
         (propertize
          "-"
          'help-echo "Byte-compile destination is writable"
          'mouse-face 'mode-line))
        (t
         (propertize
          "%%"
          'help-echo "Byte-compile destination is read-only"
          'mouse-face 'mode-line)))
       (cond
        ((and auto-compile-pretend-byte-compiled
              (not (file-exists-p dst)))
         (propertize
          "!"
          'help-echo "Failed to byte-compile updating\nmouse-1 retry"
          'mouse-face 'mode-line-highlight
          'local-map (purecopy (make-mode-line-mouse-map
                                'mouse-1
                                #'auto-compile-mode-line-byte-compile))))
        ((not (file-exists-p dst))
         (propertize
          "%%"
          'help-echo "Byte-compiled file doesn't exist\nmouse-1 create"
          'mouse-face 'mode-line-highlight
          'local-map (purecopy (make-mode-line-mouse-map
                                'mouse-1
                                #'mode-line-toggle-auto-compile))))
        ((file-newer-than-file-p src dst)
         (propertize
          "*"
          'help-echo "Byte-compiled file needs updating\nmouse-1 update"
          'mouse-face 'mode-line-highlight
          'local-map (purecopy (make-mode-line-mouse-map
                                'mouse-1
                                #'auto-compile-mode-line-byte-compile))))
        (t
         (propertize
          "-"
          'help-echo "Byte-compiled file is up-to-date\nmouse-1 remove"
          'mouse-face 'mode-line-highlight
          'local-map (purecopy (make-mode-line-mouse-map
                                'mouse-1
                                #'mode-line-toggle-auto-compile)))))))))

(defun auto-compile-display-log ()
  "Display the *Compile-Log* buffer."
  (interactive)
  (let ((buffer (get-buffer byte-compile-log-buffer)))
    (if (buffer-live-p buffer)
        (pop-to-buffer buffer)
      (user-error "Buffer %s doesn't exist" byte-compile-log-buffer))))

(defun mode-line-toggle-auto-compile (event)
  "Toggle automatic compilation from the mode-line."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (toggle-auto-compile
     (buffer-file-name)
     (if (file-exists-p (byte-compile-dest-file (buffer-file-name)))
         'quit
       'start))
    (force-mode-line-update)))

(defun auto-compile-mode-line-byte-compile (event)
  "Recompile visited file from the mode-line."
  (interactive "e")
  (save-selected-window
    (select-window (posn-window (event-start event)))
    (auto-compile-byte-compile (buffer-file-name) t)
    (force-mode-line-update)))

;;; Auto-Compile-On-Load Mode

;;;###autoload
(define-minor-mode auto-compile-on-load-mode
  "Before loading a library recompile it if it needs recompilation.

A library needs to be recompiled if the source file is newer than
it's byte-compile destination.  Without this advice the outdated
byte code file would be loaded instead.

Also see the related `auto-compile-on-save-mode'."
  :lighter auto-compile-on-load-mode-lighter
  :group 'auto-compile
  :global t)

(defvar auto-compile-on-load-mode-lighter ""
  "Mode lighter for Auto-Compile-On-Load Mode.")

(define-advice load
    (:before (file &optional _noerror _nomessage nosuffix _must-suffix)
             auto-compile)
  "Before loading the library recompile it if it needs recompilation.
If `auto-compile-on-load-mode' isn't enabled, then do nothing.
It needs recompilation if it is newer than the byte-code file.
Without this advice the outdated source file would get loaded."
  (when auto-compile-on-load-mode
    (auto-compile-on-load file nosuffix)))

(define-advice require
    (:before (feature &optional filename _noerror) auto-compile)
  "Before loading the library recompile it if it needs recompilation.
If `auto-compile-on-load-mode' isn't enabled, then do nothing.
It needs recompilation if it is newer than the byte-code file.
Without this advice the outdated source file would get loaded."
  (when auto-compile-on-load-mode
    (unless (featurep feature)
      (auto-compile-on-load (or filename (symbol-name feature))))))

(defvar auto-compile--loading nil)

(defun auto-compile-on-load (file &optional nosuffix)
  (unless (member file auto-compile--loading)
    (let ((auto-compile--loading (cons file auto-compile--loading))
          byte-compile-verbose el elc el*)
      (condition-case nil
          (when (setq el (packed-locate-library file nosuffix))
            (setq elc (byte-compile-dest-file el))
            (when (and (file-exists-p elc)
                       (file-writable-p elc)
                       (file-newer-than-file-p el elc))
              (message "Recompiling %s..." el)
              (packed-byte-compile-file el)
              (message "Recompiling %s...done" el))
            (when auto-compile-delete-stray-dest
              (setq el* (locate-library file))
              (unless (equal (file-name-directory el)
                             (file-name-directory el*))
                (auto-compile-delete-dest el* t))))
        (error
         (message "Recompiling %s...failed" el)
         (when elc
           (auto-compile-delete-dest elc t)))))))

;;; _
(provide 'auto-compile)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; auto-compile.el ends here
