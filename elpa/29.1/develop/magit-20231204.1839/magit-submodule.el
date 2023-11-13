;;; magit-submodule.el --- Submodule support for Magit  -*- lexical-binding:t -*-

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

;;; Code:

(require 'magit)

(defvar x-stretch-cursor)

;;; Options

(defcustom magit-module-sections-hook
  '(magit-insert-modules-overview
    magit-insert-modules-unpulled-from-upstream
    magit-insert-modules-unpulled-from-pushremote
    magit-insert-modules-unpushed-to-upstream
    magit-insert-modules-unpushed-to-pushremote)
  "Hook run by `magit-insert-modules'.

That function isn't part of `magit-status-sections-hook's default
value, so you have to add it yourself for this hook to have any
effect."
  :package-version '(magit . "2.11.0")
  :group 'magit-status
  :type 'hook)

(defcustom magit-module-sections-nested t
  "Whether `magit-insert-modules' wraps inserted sections.

If this is non-nil, then only a single top-level section
is inserted.  If it is nil, then all sections listed in
`magit-module-sections-hook' become top-level sections."
  :package-version '(magit . "2.11.0")
  :group 'magit-status
  :type 'boolean)

(defcustom magit-submodule-list-mode-hook '(hl-line-mode)
  "Hook run after entering Magit-Submodule-List mode."
  :package-version '(magit . "2.9.0")
  :group 'magit-repolist
  :type 'hook
  :get 'magit-hook-custom-get
  :options '(hl-line-mode))

(defcustom magit-submodule-list-columns
  '(("Path"     25 magit-modulelist-column-path   nil)
    ("Version"  25 magit-repolist-column-version
     ((:sort magit-repolist-version<)))
    ("Branch"   20 magit-repolist-column-branch   nil)
    ("B<U" 3 magit-repolist-column-unpulled-from-upstream
     ((:right-align t)
      (:sort <)))
    ("B>U" 3 magit-repolist-column-unpushed-to-upstream
     ((:right-align t)
      (:sort <)))
    ("B<P" 3 magit-repolist-column-unpulled-from-pushremote
     ((:right-align t)
      (:sort <)))
    ("B>P" 3 magit-repolist-column-unpushed-to-pushremote
     ((:right-align t)
      (:sort <)))
    ("B"   3 magit-repolist-column-branches
     ((:right-align t)
      (:sort <)))
    ("S"   3 magit-repolist-column-stashes
     ((:right-align t)
      (:sort <))))
  "List of columns displayed by `magit-list-submodules'.

Each element has the form (HEADER WIDTH FORMAT PROPS).

HEADER is the string displayed in the header.  WIDTH is the width
of the column.  FORMAT is a function that is called with one
argument, the repository identification (usually its basename),
and with `default-directory' bound to the toplevel of its working
tree.  It has to return a string to be inserted or nil.  PROPS is
an alist that supports the keys `:right-align', `:pad-right' and
`:sort'.

The `:sort' function has a weird interface described in the
docstring of `tabulated-list--get-sort'.  Alternatively `<' and
`magit-repolist-version<' can be used as those functions are
automatically replaced with functions that satisfy the interface.
Set `:sort' to nil to inhibit sorting; if unspecified, then the
column is sortable using the default sorter.

You may wish to display a range of numeric columns using just one
character per column and without any padding between columns, in
which case you should use an appropriate HEADER, set WIDTH to 1,
and set `:pad-right' to 0.  \"+\" is substituted for numbers higher
than 9."
  :package-version '(magit . "2.8.0")
  :group 'magit-repolist
  :type `(repeat (list :tag "Column"
                       (string   :tag "Header Label")
                       (integer  :tag "Column Width")
                       (function :tag "Inserter Function")
                       (repeat   :tag "Properties"
                                 (list (choice :tag "Property"
                                               (const :right-align)
                                               (const :pad-right)
                                               (const :sort)
                                               (symbol))
                                       (sexp   :tag "Value"))))))

(defcustom magit-submodule-list-sort-key '("Path" . nil)
  "Initial sort key for buffer created by `magit-list-submodules'.
If nil, no additional sorting is performed.  Otherwise, this
should be a cons cell (NAME . FLIP).  NAME is a string matching
one of the column names in `magit-submodule-list-columns'.  FLIP,
if non-nil, means to invert the resulting sort."
  :package-version '(magit . "3.2.0")
  :group 'magit-repolist
  :type '(choice (const nil)
                 (cons (string :tag "Column name")
                       (boolean :tag "Flip order"))))

(defvar magit-submodule-list-format-path-functions nil)

(defcustom magit-submodule-remove-trash-gitdirs nil
  "Whether `magit-submodule-remove' offers to trash module gitdirs.

If this is nil, then that command does not offer to do so unless
a prefix argument is used.  When this is t, then it does offer to
do so even without a prefix argument.

In both cases the action still has to be confirmed unless that is
disabled using the option `magit-no-confirm'.  Doing the latter
and also setting this variable to t will lead to tears."
  :package-version '(magit . "2.90.0")
  :group 'magit-commands
  :type 'boolean)

;;; Popup

;;;###autoload (autoload 'magit-submodule "magit-submodule" nil t)
(transient-define-prefix magit-submodule ()
  "Act on a submodule."
  :man-page "git-submodule"
  ["Arguments"
   ("-f" "Force"            ("-f" "--force"))
   ("-r" "Recursive"        "--recursive")
   ("-N" "Do not fetch"     ("-N" "--no-fetch"))
   ("-C" "Checkout tip"     "--checkout")
   ("-R" "Rebase onto tip"  "--rebase")
   ("-M" "Merge tip"        "--merge")
   ("-U" "Use upstream tip" "--remote")]
  ["One module actions"
   ("a" magit-submodule-add)
   ("r" magit-submodule-register)
   ("p" magit-submodule-populate)
   ("u" magit-submodule-update)
   ("s" magit-submodule-synchronize)
   ("d" magit-submodule-unpopulate)
   ("k" "Remove" magit-submodule-remove)]
  ["Populated modules actions"
   ("l" "List modules"  magit-list-submodules)
   ("f" "Fetch modules" magit-fetch-modules)])

(defun magit-submodule-arguments (&rest filters)
  (--filter (and (member it filters) it)
            (transient-args 'magit-submodule)))

(defclass magit--git-submodule-suffix (transient-suffix)
  ())

(cl-defmethod transient-format-description ((obj magit--git-submodule-suffix))
  (let ((value (delq nil (mapcar #'transient-infix-value transient--suffixes))))
    (replace-regexp-in-string
     "\\[--[^]]+\\]"
     (lambda (match)
       (format (propertize "[%s]" 'face 'transient-inactive-argument)
               (mapconcat (lambda (arg)
                            (propertize arg 'face
                                        (if (member arg value)
                                            'transient-argument
                                          'transient-inactive-argument)))
                          (save-match-data
                            (split-string (substring match 1 -1) "|"))
                          (propertize "|" 'face 'transient-inactive-argument))))
     (cl-call-next-method obj))))

;;;###autoload (autoload 'magit-submodule-add "magit-submodule" nil t)
(transient-define-suffix magit-submodule-add (url &optional path name args)
  "Add the repository at URL as a module.

Optional PATH is the path to the module relative to the root of
the superproject.  If it is nil, then the path is determined
based on the URL.  Optional NAME is the name of the module.  If
it is nil, then PATH also becomes the name."
  :class 'magit--git-submodule-suffix
  :description "Add            git submodule add [--force]"
  (interactive
   (magit-with-toplevel
     (let* ((url (magit-read-string-ns "Add submodule (remote url)"))
            (path (let ((read-file-name-function
                         (if (or (eq read-file-name-function 'ido-read-file-name)
                                 (advice-function-member-p
                                  'ido-read-file-name
                                  read-file-name-function))
                             ;; The Ido variant doesn't work properly here.
                             #'read-file-name-default
                           read-file-name-function)))
                    (directory-file-name
                     (file-relative-name
                      (read-directory-name
                       "Add submodules at path: " nil nil nil
                       (and (string-match "\\([^./]+\\)\\(\\.git\\)?$" url)
                            (match-string 1 url))))))))
       (list url
             (directory-file-name path)
             (magit-submodule-read-name-for-path path)
             (magit-submodule-arguments "--force")))))
  (magit-submodule-add-1 url path name args))

(defun magit-submodule-add-1 (url &optional path name args)
  (magit-with-toplevel
    (magit-submodule--maybe-reuse-gitdir name path)
    (magit-run-git-async "submodule" "add"
                         (and name (list "--name" name))
                         args "--" url path)
    (set-process-sentinel
     magit-this-process
     (lambda (process event)
       (when (memq (process-status process) '(exit signal))
         (if (> (process-exit-status process) 0)
             (magit-process-sentinel process event)
           (process-put process 'inhibit-refresh t)
           (magit-process-sentinel process event)
           (when (magit-git-version>= "2.12.0")
             (magit-call-git "submodule" "absorbgitdirs" path))
           (magit-refresh)))))))

;;;###autoload
(defun magit-submodule-read-name-for-path (path &optional prefer-short)
  (let* ((path (directory-file-name (file-relative-name path)))
         (name (file-name-nondirectory path)))
    (push (if prefer-short path name) minibuffer-history)
    (magit-read-string-ns
     "Submodule name" nil (cons 'minibuffer-history 2)
     (or (--keep (pcase-let ((`(,var ,val) (split-string it "=")))
                   (and (equal val path)
                        (cadr (split-string var "\\."))))
                 (magit-git-lines "config" "--list" "-f" ".gitmodules"))
         (if prefer-short name path)))))

;;;###autoload (autoload 'magit-submodule-register "magit-submodule" nil t)
(transient-define-suffix magit-submodule-register (modules)
  "Register MODULES.

With a prefix argument act on all suitable modules.  Otherwise,
if the region selects modules, then act on those.  Otherwise, if
there is a module at point, then act on that.  Otherwise read a
single module from the user."
  ;; This command and the underlying "git submodule init" do NOT
  ;; "initialize" modules.  They merely "register" modules in the
  ;; super-projects $GIT_DIR/config file, the purpose of which is to
  ;; allow users to change such values before actually initializing
  ;; the modules.
  :description "Register       git submodule init"
  (interactive
   (list (magit-module-confirm "Register" 'magit-module-no-worktree-p)))
  (magit-with-toplevel
    (magit-run-git-async "submodule" "init" "--" modules)))

;;;###autoload (autoload 'magit-submodule-populate "magit-submodule" nil t)
(transient-define-suffix magit-submodule-populate (modules)
  "Create MODULES working directories, checking out the recorded commits.

With a prefix argument act on all suitable modules.  Otherwise,
if the region selects modules, then act on those.  Otherwise, if
there is a module at point, then act on that.  Otherwise read a
single module from the user."
  ;; This is the command that actually "initializes" modules.
  ;; A module is initialized when it has a working directory,
  ;; a gitlink, and a .gitmodules entry.
  :description "Populate       git submodule update --init"
  (interactive
   (list (magit-module-confirm "Populate" 'magit-module-no-worktree-p)))
  (magit-with-toplevel
    (magit-run-git-async "submodule" "update" "--init" "--" modules)))

;;;###autoload (autoload 'magit-submodule-update "magit-submodule" nil t)
(transient-define-suffix magit-submodule-update (modules args)
  "Update MODULES by checking out the recorded commits.

With a prefix argument act on all suitable modules.  Otherwise,
if the region selects modules, then act on those.  Otherwise, if
there is a module at point, then act on that.  Otherwise read a
single module from the user."
  ;; Unlike `git-submodule's `update' command ours can only update
  ;; "initialized" modules by checking out other commits but not
  ;; "initialize" modules by creating the working directories.
  ;; To do the latter we provide the "setup" command.
  :class 'magit--git-submodule-suffix
  :description "Update         git submodule update [--force] [--no-fetch]
                     [--remote] [--recursive] [--checkout|--rebase|--merge]"
  (interactive
   (list (magit-module-confirm "Update" 'magit-module-worktree-p)
         (magit-submodule-arguments
          "--force" "--remote" "--recursive" "--checkout" "--rebase" "--merge"
          "--no-fetch")))
  (magit-with-toplevel
    (magit-run-git-async "submodule" "update" args "--" modules)))

;;;###autoload (autoload 'magit-submodule-synchronize "magit-submodule" nil t)
(transient-define-suffix magit-submodule-synchronize (modules args)
  "Synchronize url configuration of MODULES.

With a prefix argument act on all suitable modules.  Otherwise,
if the region selects modules, then act on those.  Otherwise, if
there is a module at point, then act on that.  Otherwise read a
single module from the user."
  :class 'magit--git-submodule-suffix
  :description "Synchronize    git submodule sync [--recursive]"
  (interactive
   (list (magit-module-confirm "Synchronize" 'magit-module-worktree-p)
         (magit-submodule-arguments "--recursive")))
  (magit-with-toplevel
    (magit-run-git-async "submodule" "sync" args "--" modules)))

;;;###autoload (autoload 'magit-submodule-unpopulate "magit-submodule" nil t)
(transient-define-suffix magit-submodule-unpopulate (modules args)
  "Remove working directories of MODULES.

With a prefix argument act on all suitable modules.  Otherwise,
if the region selects modules, then act on those.  Otherwise, if
there is a module at point, then act on that.  Otherwise read a
single module from the user."
  ;; Even though a package is "uninitialized" (it has no worktree)
  ;; the super-projects $GIT_DIR/config may never-the-less set the
  ;; module's url.  This may happen if you `deinit' and then `init'
  ;; to register (NOT initialize).  Because the purpose of `deinit'
  ;; is to remove the working directory AND to remove the url, this
  ;; command does not limit itself to modules that have no working
  ;; directory.
  :class 'magit--git-submodule-suffix
  :description "Unpopulate     git submodule deinit [--force]"
  (interactive
   (list (magit-module-confirm "Unpopulate")
         (magit-submodule-arguments "--force")))
  (magit-with-toplevel
    (magit-run-git-async "submodule" "deinit" args "--" modules)))

;;;###autoload
(defun magit-submodule-remove (modules args trash-gitdirs)
  "Unregister MODULES and remove their working directories.

For safety reasons, do not remove the gitdirs and if a module has
uncommitted changes, then do not remove it at all.  If a module's
gitdir is located inside the working directory, then move it into
the gitdir of the superproject first.

With the \"--force\" argument offer to remove dirty working
directories and with a prefix argument offer to delete gitdirs.
Both actions are very dangerous and have to be confirmed.  There
are additional safety precautions in place, so you might be able
to recover from making a mistake here, but don't count on it."
  (interactive
   (list (if-let ((modules (magit-region-values 'magit-module-section t)))
             (magit-confirm 'remove-modules nil "Remove %d modules" nil modules)
           (list (magit-read-module-path "Remove module")))
         (magit-submodule-arguments "--force")
         current-prefix-arg))
  (when (magit-git-version< "2.12.0")
    (error "This command requires Git v2.12.0"))
  (when magit-submodule-remove-trash-gitdirs
    (setq trash-gitdirs t))
  (magit-with-toplevel
    (when-let
        ((modified
          (seq-filter (lambda (module)
                        (let ((default-directory (file-name-as-directory
                                                  (expand-file-name module))))
                          (and (cddr (directory-files default-directory))
                               (magit-anything-modified-p))))
                      modules)))
      (if (member "--force" args)
          (if (magit-confirm 'remove-dirty-modules
                "Remove dirty module %s"
                "Remove %d dirty modules"
                t modified)
              (dolist (module modified)
                (let ((default-directory (file-name-as-directory
                                          (expand-file-name module))))
                  (magit-git "stash" "push"
                             "-m" "backup before removal of this module")))
            (setq modules (cl-set-difference modules modified :test #'equal)))
        (if (cdr modified)
            (message "Omitting %s modules with uncommitted changes: %s"
                     (length modified)
                     (mapconcat #'identity modified ", "))
          (message "Omitting module %s, it has uncommitted changes"
                   (car modified)))
        (setq modules (cl-set-difference modules modified :test #'equal))))
    (when modules
      (let ((alist
             (and trash-gitdirs
                  (--map (split-string it "\0")
                         (magit-git-lines "submodule" "foreach" "-q"
                                          "printf \"$sm_path\\0$name\n\"")))))
        (magit-git "submodule" "absorbgitdirs" "--" modules)
        (magit-git "submodule" "deinit" args "--" modules)
        (magit-git "rm" args "--" modules)
        (when (and trash-gitdirs
                   (magit-confirm 'trash-module-gitdirs
                     "Trash gitdir of module %s"
                     "Trash gitdirs of %d modules"
                     t modules))
          (dolist (module modules)
            (if-let ((name (cadr (assoc module alist))))
                ;; Disregard if `magit-delete-by-moving-to-trash'
                ;; is nil.  Not doing so would be too dangerous.
                (delete-directory (convert-standard-filename
                                   (expand-file-name
                                    (concat "modules/" name)
                                    (magit-gitdir)))
                                  t t)
              (error "BUG: Weird module name and/or path for %s" module)))))
      (magit-refresh))))

;;; Sections

;;;###autoload
(defun magit-insert-modules ()
  "Insert submodule sections.
Hook `magit-module-sections-hook' controls which module sections
are inserted, and option `magit-module-sections-nested' controls
whether they are wrapped in an additional section."
  (when-let ((modules (magit-list-module-paths)))
    (if magit-module-sections-nested
        (magit-insert-section (modules nil t)
          (magit-insert-heading
            (format "%s (%s)"
                    (propertize "Modules"
                                'font-lock-face 'magit-section-heading)
                    (length modules)))
          (magit-insert-section-body
            (magit--insert-modules)))
      (magit--insert-modules))))

(defun magit--insert-modules (&optional _section)
  (magit-run-section-hook 'magit-module-sections-hook))

;;;###autoload
(defun magit-insert-modules-overview ()
  "Insert sections for all modules.
For each section insert the path and the output of `git describe --tags',
or, failing that, the abbreviated HEAD commit hash."
  (when-let ((modules (magit-list-module-paths)))
    (magit-insert-section (modules nil t)
      (magit-insert-heading
        (format "%s (%s)"
                (propertize "Modules overview"
                            'font-lock-face 'magit-section-heading)
                (length modules)))
      (magit-insert-section-body
        (magit--insert-modules-overview)))))

(defvar magit-modules-overview-align-numbers t)

(defun magit--insert-modules-overview (&optional _section)
  (magit-with-toplevel
    (let* ((modules (magit-list-module-paths))
           (path-format (format "%%-%ds "
                                (min (apply #'max (mapcar #'length modules))
                                     (/ (window-width) 2))))
           (branch-format (format "%%-%ds " (min 25 (/ (window-width) 3)))))
      (dolist (module modules)
        (let ((default-directory
               (expand-file-name (file-name-as-directory module))))
          (magit-insert-section (magit-module-section module t)
            (insert (propertize (format path-format module)
                                'font-lock-face 'magit-diff-file-heading))
            (if (not (file-exists-p ".git"))
                (insert "(unpopulated)")
              (insert
               (format
                branch-format
                (if-let ((branch (magit-get-current-branch)))
                    (propertize branch 'font-lock-face 'magit-branch-local)
                  (propertize "(detached)" 'font-lock-face 'warning))))
              (if-let ((desc (magit-git-string "describe" "--tags")))
                  (progn (when (and magit-modules-overview-align-numbers
                                    (string-match-p "\\`[0-9]" desc))
                           (insert ?\s))
                         (insert (propertize desc 'font-lock-face 'magit-tag)))
                (when-let ((abbrev (magit-rev-format "%h")))
                  (insert (propertize abbrev 'font-lock-face 'magit-hash)))))
            (insert ?\n))))))
  (insert ?\n))

(defvar-keymap magit-modules-section-map
  :doc "Keymap for `modules' sections."
  "<remap> <magit-visit-thing>" #'magit-list-submodules
  "<1>" (magit-menu-item "List %t" #'magit-list-submodules))

(defvar-keymap magit-module-section-map
  :doc "Keymap for `module' sections."
  "C-j"        #'magit-submodule-visit
  "C-<return>" #'magit-submodule-visit
  "<remap> <magit-unstage-file>" #'magit-unstage
  "<remap> <magit-stage-file>"   #'magit-stage
  "<remap> <magit-visit-thing>"  #'magit-submodule-visit
  "<5>" (magit-menu-item "Module commands..." #'magit-submodule)
  "<4>" '(menu-item "--")
  "<3>" (magit-menu-item "Unstage %T" #'magit-unstage
                         '(:visible (eq (magit-diff-type) 'staged)))
  "<2>" (magit-menu-item "Stage %T" #'magit-stage
                         '(:visible (eq (magit-diff-type) 'unstaged)))
  "<1>" (magit-menu-item "Visit %s" #'magit-submodule-visit))

(defun magit-submodule-visit (module &optional other-window)
  "Visit MODULE by calling `magit-status' on it.
Offer to initialize MODULE if it's not checked out yet.
With a prefix argument, visit in another window."
  (interactive (list (or (magit-section-value-if 'module)
                         (magit-read-module-path "Visit module"))
                     current-prefix-arg))
  (magit-with-toplevel
    (let ((path (expand-file-name module)))
      (cond
       ((file-exists-p (expand-file-name ".git" module))
        (magit-diff-visit-directory path other-window))
       ((y-or-n-p (format "Initialize submodule '%s' first?" module))
        (magit-run-git-async "submodule" "update" "--init" "--" module)
        (set-process-sentinel
         magit-this-process
         (lambda (process event)
           (let ((magit-process-raise-error t))
             (magit-process-sentinel process event))
           (when (and (eq (process-status      process) 'exit)
                      (=  (process-exit-status process) 0))
             (magit-diff-visit-directory path other-window)))))
       ((file-exists-p path)
        (dired-jump other-window (concat path "/.")))))))

;;;###autoload
(defun magit-insert-modules-unpulled-from-upstream ()
  "Insert sections for modules that haven't been pulled from the upstream.
These sections can be expanded to show the respective commits."
  (magit--insert-modules-logs "Modules unpulled from @{upstream}"
                              'modules-unpulled-from-upstream
                              "HEAD..@{upstream}"))

;;;###autoload
(defun magit-insert-modules-unpulled-from-pushremote ()
  "Insert sections for modules that haven't been pulled from the push-remote.
These sections can be expanded to show the respective commits."
  (magit--insert-modules-logs "Modules unpulled from @{push}"
                              'modules-unpulled-from-pushremote
                              "HEAD..@{push}"))

;;;###autoload
(defun magit-insert-modules-unpushed-to-upstream ()
  "Insert sections for modules that haven't been pushed to the upstream.
These sections can be expanded to show the respective commits."
  (magit--insert-modules-logs "Modules unmerged into @{upstream}"
                              'modules-unpushed-to-upstream
                              "@{upstream}..HEAD"))

;;;###autoload
(defun magit-insert-modules-unpushed-to-pushremote ()
  "Insert sections for modules that haven't been pushed to the push-remote.
These sections can be expanded to show the respective commits."
  (magit--insert-modules-logs "Modules unpushed to @{push}"
                              'modules-unpushed-to-pushremote
                              "@{push}..HEAD"))

(defun magit--insert-modules-logs (heading type range)
  "For internal use, don't add to a hook."
  (unless (magit-ignore-submodules-p)
    (when-let ((modules (magit-list-module-paths)))
      (magit-insert-section section ((eval type) nil t)
        (string-match "\\`\\(.+\\) \\([^ ]+\\)\\'" heading)
        (magit-insert-heading
          (propertize (match-string 1 heading)
                      'font-lock-face 'magit-section-heading)
          " "
          (propertize (match-string 2 heading)
                      'font-lock-face 'magit-branch-remote)
          ":")
        (magit-with-toplevel
          (dolist (module modules)
            (when (magit-module-worktree-p module)
              (let ((default-directory
                     (expand-file-name (file-name-as-directory module))))
                (when (magit-file-accessible-directory-p default-directory)
                  (magit-insert-section sec (magit-module-section module t)
                    (magit-insert-heading
                      (propertize module
                                  'font-lock-face 'magit-diff-file-heading)
                      ":")
                    (oset sec range range)
                    (magit-git-wash
                        (apply-partially #'magit-log-wash-log 'module)
                      "-c" "push.default=current" "log" "--oneline" range)
                    (when (> (point)
                             (oref sec content))
                      (delete-char -1))))))))
        (if (> (point)
               (oref section content))
            (insert ?\n)
          (magit-cancel-section))))))

;;; List

;;;###autoload
(defun magit-list-submodules ()
  "Display a list of the current repository's populated submodules."
  (interactive)
  (magit-submodule-list-setup magit-submodule-list-columns))

(defvar-keymap magit-submodule-list-mode-map
  :doc "Local keymap for Magit-Submodule-List mode buffers."
  :parent magit-repolist-mode-map)

(define-derived-mode magit-submodule-list-mode tabulated-list-mode "Modules"
  "Major mode for browsing a list of Git submodules."
  :group 'magit-repolist-mode
  (setq-local x-stretch-cursor nil)
  (setq tabulated-list-padding 0)
  (add-hook 'tabulated-list-revert-hook #'magit-submodule-list-refresh nil t)
  (setq imenu-prev-index-position-function
        #'magit-repolist--imenu-prev-index-position)
  (setq imenu-extract-index-name-function #'tabulated-list-get-id))

(defvar-local magit-submodule-list-predicate nil)

(defun magit-submodule-list-setup (columns &optional predicate)
  (magit-display-buffer
   (or (magit-get-mode-buffer 'magit-submodule-list-mode)
       (magit-generate-new-buffer 'magit-submodule-list-mode)))
  (magit-submodule-list-mode)
  (setq-local magit-repolist-columns columns)
  (setq-local magit-repolist-sort-key magit-submodule-list-sort-key)
  (setq-local magit-submodule-list-predicate predicate)
  (magit-repolist-setup-1)
  (magit-submodule-list-refresh))

(defun magit-submodule-list-refresh ()
  (setq tabulated-list-entries
        (seq-keep
         (lambda (module)
           (let ((default-directory
                  (expand-file-name (file-name-as-directory module))))
             (and (file-exists-p ".git")
                  (or (not magit-submodule-list-predicate)
                      (funcall magit-submodule-list-predicate module))
                  (list module
                        (vconcat
                         (mapcar (pcase-lambda (`(,title ,width ,fn ,props))
                                   (or (funcall fn `((:path  ,module)
                                                     (:title ,title)
                                                     (:width ,width)
                                                     ,@props))
                                       ""))
                                 magit-repolist-columns))))))
         (magit-list-module-paths)))
  (message "Listing submodules...")
  (tabulated-list-init-header)
  (tabulated-list-print t)
  (message "Listing submodules...done"))

(defun magit-modulelist-column-path (spec)
  "Insert the relative path of the submodule."
  (let ((path (cadr (assq :path spec))))
    (or (run-hook-with-args-until-success
         'magit-submodule-list-format-path-functions path)
        path)))

;;; Utilities

(defun magit-submodule--maybe-reuse-gitdir (name path)
  (let ((gitdir (convert-standard-filename
                 (expand-file-name (concat "modules/" name)
                                   (magit-gitdir)))))
    (when (and (file-exists-p gitdir)
               (not (file-exists-p path)))
      (pcase (read-char-choice
              (concat
               gitdir " already exists.\n"
               "Type [u] to use the existing gitdir and create the working tree\n"
               "     [r] to rename the existing gitdir and clone again\n"
               "     [t] to trash the existing gitdir and clone again\n"
               "   [C-g] to abort ")
              '(?u ?r ?t))
        (?u (magit-submodule--restore-worktree (expand-file-name path) gitdir))
        (?r (rename-file gitdir (concat gitdir "-"
                                        (format-time-string "%F-%T"))))
        (?t (delete-directory gitdir t t))))))

(defun magit-submodule--restore-worktree (worktree gitdir)
  (make-directory worktree t)
  (with-temp-file (expand-file-name ".git" worktree)
    (insert "gitdir: " (file-relative-name gitdir worktree) "\n"))
  (let ((default-directory worktree))
    (magit-call-git "reset" "--hard" "HEAD" "--")))

;;; _
(provide 'magit-submodule)
;;; magit-submodule.el ends here
