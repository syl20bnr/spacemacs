;;; helm-projectile.el --- Helm integration for Projectile         -*- lexical-binding: t; -*-

;; Copyright (C) 2011-2020 Bozhidar Batsov

;; Author: Bozhidar Batsov
;; URL: https://github.com/bbatsov/helm-projectile
;; Created: 2011-31-07
;; Keywords: project, convenience
;; Version: 1.1.0-snapshot
;; Package-Requires: ((helm "1.9.9") (projectile "2.2.0") (cl-lib "0.3"))

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provides easy project management and navigation.  The
;; concept of a project is pretty basic - just a folder containing
;; special file.  Currently git, mercurial and bazaar repos are
;; considered projects by default.  If you want to mark a folder
;; manually as a project just create an empty .projectile file in
;; it.  See the README for more details.
;;
;;; Code:

(require 'subr-x)
(require 'projectile)
(require 'cl-lib)
(require 'grep)
(require 'helm)
(require 'helm-types)
(require 'helm-locate)
(require 'helm-buffers)
(require 'helm-files)

(declare-function eshell "eshell")
(declare-function helm-do-ag "ext:helm-ag")
(declare-function dired-get-filename "dired")
(defvar helm-ag-base-command)

(defvar grep-find-ignored-directories)
(defvar grep-find-ignored-files)

(defgroup helm-projectile nil
  "Helm support for projectile."
  :prefix "helm-projectile-"
  :group 'projectile
  :group 'helm
  :link `(url-link :tag "GitHub" "https://github.com/bbatsov/helm-projectile"))

(defvar helm-projectile-current-project-root)

(defcustom helm-projectile-truncate-lines nil
  "Truncate lines in helm projectile commands when non--nil.

Some `helm-projectile' commands have similar behavior with existing
Helms.  In these cases their respective custom var for truncation
of lines will be honored.  E.g. `helm-buffers-truncate-lines'
dictates the truncation in `helm-projectile-switch-to-buffer'."
  :group 'helm-projectile
  :type 'boolean)

;;;###autoload
(defcustom helm-projectile-fuzzy-match t
  "Enable fuzzy matching for Helm Projectile commands.
This needs to be set before loading helm-projectile.el."
  :group 'helm-projectile
  :type 'boolean)

(defmacro helm-projectile-define-key (keymap key def &rest bindings)
  "In KEYMAP, define KEY - DEF sequence KEY1 as DEF1, KEY2 as DEF2 ..."
  (declare (indent defun))
  (let ((ret '(progn)))
    (while key
      (push
       `(define-key ,keymap ,key
          (lambda ()
            (interactive)
            (helm-exit-and-execute-action ,def)))
       ret)
      (setq key (pop bindings)
            def (pop bindings)))
    (reverse ret)))

(defun helm-projectile-hack-actions (actions &rest prescription)
  "Given a Helm action list and a prescription, return a hacked Helm action list.
Optionally applies the PRESCRIPTION beforehand.

The Helm action list ACTIONS is of the form:

\(\(DESCRIPTION1 . FUNCTION1\)
 \(DESCRIPTION2 . FUNCTION2\)
 ...
 \(DESCRIPTIONn . FUNCTIONn\)\)

PRESCRIPTION is in the form:

\(INSTRUCTION1 INSTRUCTION2 ... INSTRUCTIONn\)

If an INSTRUCTION is a symbol, the action with function name
INSTRUCTION is deleted.

If an INSTRUCTION is of the form \(FUNCTION1 . FUNCTION2\), the
action with function name FUNCTION1 will change it's function to
FUNCTION2.

If an INSTRUCTION is of the form \(FUNCTION . DESCRIPTION\), and
if an action with function name FUNCTION exists in the original
Helm action list, the action in the Helm action list, with
function name FUNCTION will change it's description to
DESCRIPTION.  Otherwise, (FUNCTION . DESCRIPTION) will be added to
the action list.

Please check out how `helm-projectile-file-actions' is defined
for an example of how this function is being used."
  (let* ((to-delete (cl-remove-if (lambda (entry) (listp entry)) prescription))
         (actions (cl-delete-if (lambda (action) (memq (cdr action) to-delete))
                                (copy-alist actions)))
         new)
    (cl-dolist (action actions)
      (when (setq new (cdr (assq (cdr action) prescription)))
        (if (stringp new)
            (setcar action new)
          (setcdr action new))))
    ;; Add new actions from PRESCRIPTION
    (setq new nil)
    (cl-dolist (instruction prescription)
      (when (and (listp instruction)
                 (null (rassq (car instruction) actions))
                 (symbolp (car instruction)) (stringp (cdr instruction)))
        (push (cons (cdr instruction) (car instruction)) new)))
    (append actions (nreverse new))))

(defun helm-projectile-vc (dir)
  "A Helm action for jumping to project root using `vc-dir' or Magit.
DIR is a directory to be switched"
  (let ((projectile-require-project-root nil))
    (projectile-vc dir)))

(defun helm-projectile-compile-project (dir)
  "A Helm action for compile a project.
DIR is the project root."
  (let ((helm--reading-passwd-or-string t)
        (default-directory dir))
    (projectile-compile-project helm-current-prefix-arg)))

(defun helm-projectile-test-project (dir)
  "A Helm action for test a project.
DIR is the project root."
  (let ((helm--reading-passwd-or-string t)
        (default-directory dir))
    (projectile-test-project helm-current-prefix-arg)))

(defun helm-projectile-run-project (dir)
  "A Helm action for run a project.
DIR is the project root."
  (let ((helm--reading-passwd-or-string t)
        (default-directory dir))
    (projectile-run-project helm-current-prefix-arg)))

(defun helm-projectile-remove-known-project (_ignore)
  "Remove selected projects from projectile project list.
_IGNORE means the argument does not matter.
It is there because Helm requires it."
  (let* ((projects (helm-marked-candidates :with-wildcard t))
         (len (length projects)))
    (with-helm-display-marked-candidates
      helm-marked-buffer-name
      projects
      (if (not (y-or-n-p (format "Remove *%s projects(s)? " len)))
          (message "(No removal performed)")
        (progn
          (mapc (lambda (p)
                  (setq projectile-known-projects (delete p projectile-known-projects)))
                projects)
          (projectile-save-known-projects))
        (message "%s projects(s) removed" len)))))

(defvar helm-projectile-projects-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (helm-projectile-define-key map
      (kbd "C-d") #'dired
      (kbd "M-g") #'helm-projectile-vc
      (kbd "M-e") #'helm-projectile-switch-to-shell
      (kbd "C-s") #'helm-projectile-grep
      (kbd "M-c") #'helm-projectile-compile-project
      (kbd "M-t") #'helm-projectile-test-project
      (kbd "M-r") #'helm-projectile-run-project
      (kbd "M-D") #'helm-projectile-remove-known-project)
    map)
  "Mapping for known projectile projects.")

(defcustom helm-source-projectile-projects-actions
  (helm-make-actions
   "Switch to project" (lambda (project)
                         (let ((projectile-completion-system 'helm))
                           (projectile-switch-project-by-name project)))
   "Open Dired in project's directory `C-d'" #'dired
   "Open project root in vc-dir or magit `M-g'" #'helm-projectile-vc
   "Switch to Eshell `M-e'" #'helm-projectile-switch-to-shell
   "Grep in projects `C-s'" #'helm-projectile-grep
   "Compile project `M-c'. With C-u, new compile command" #'helm-projectile-compile-project
   "Remove project(s) from project list `M-D'" #'helm-projectile-remove-known-project)
  "Actions for `helm-source-projectile-projects'."
  :group 'helm-projectile
  :type '(alist :key-type string :value-type function))

(defvar helm-source-projectile-projects
  (helm-build-sync-source "Projectile projects"
    :candidates (lambda () (with-helm-current-buffer projectile-known-projects))
    :fuzzy-match helm-projectile-fuzzy-match
    :keymap helm-projectile-projects-map
    :mode-line helm-read-file-name-mode-line-string
    :action 'helm-source-projectile-projects-actions)
  "Helm source for known projectile projects.")

(defvar helm-projectile-dirty-projects-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (helm-projectile-define-key map
      (kbd "C-d") #'dired
      (kbd "M-o") #'(lambda (project)
                      (let ((projectile-completion-system 'helm))
                        (projectile-switch-project-by-name project)))
      (kbd "M-e") #'helm-projectile-switch-to-shell
      (kbd "C-s") #'helm-projectile-grep
      (kbd "M-c") #'helm-projectile-compile-project
      (kbd "M-t") #'helm-projectile-test-project
      (kbd "M-r") #'helm-projectile-run-project
      (kbd "M-D") #'helm-projectile-remove-known-project)
    map)
  "Mapping for dirty projectile projects.")

(defvar helm-source-projectile-dirty-projects
  (helm-build-sync-source "Projectile dirty projects"
    :candidates (lambda () (with-helm-current-buffer (helm-projectile-get-dirty-projects)))
    :fuzzy-match helm-projectile-fuzzy-match
    :keymap helm-projectile-dirty-projects-map
    :mode-line helm-read-file-name-mode-line-string
    :action '(("Open project root in vc-dir or magit" . helm-projectile-vc)
              ("Switch to project `M-o'" .
               (lambda (project)
                 (let ((projectile-completion-system 'helm))
                   (projectile-switch-project-by-name project))))
              ("Open Dired in project's directory `C-d'" . dired)
              ("Switch to Eshell `M-e'" . helm-projectile-switch-to-shell)
              ("Grep in projects `C-s'" . helm-projectile-grep)
              ("Compile project `M-c'. With C-u, new compile command"
               . helm-projectile-compile-project)))
    "Helm source for dirty version controlled projectile projects.")

(defun helm-projectile-get-dirty-projects ()
  "Return dirty version controlled known projects as an alist to
have a nice display in Helm."
  (message "Checking for dirty known projects...")
  (let* ((status (projectile-check-vcs-status-of-known-projects))
         (proj-dir (cl-loop for stat in status
                            collect (car stat)))
         (status-display (cl-loop for stat in status collect
                                  (propertize (format "[%s]" (mapconcat 'identity (car (cdr stat)) ", ")) 'face 'helm-match)))
         (max-status-display-length (cl-loop for sd in status-display
                                             maximize (length sd)))
         (status-display (cl-loop for sd in status-display collect
                                  (format "%s%s    " sd (make-string (- max-status-display-length (length sd)) ? ))))
         (full-display (cl-mapcar 'concat status-display proj-dir))
         (helm-list (cl-pairlis full-display proj-dir)))
    helm-list))

(define-key helm-etags-map (kbd "C-c p f")
  (lambda ()
    (interactive)
    (helm-run-after-exit 'helm-projectile-find-file nil)))

(defun helm-projectile-file-persistent-action (candidate)
  "Persistent action for file-related functionality.

Previews the contents of a file in a temporary buffer."
  (let ((buf (get-buffer-create " *helm-projectile persistent*")))
    (cl-flet ((preview (candidate)
                       (switch-to-buffer buf)
                       (setq inhibit-read-only t)
                       (erase-buffer)
                       (insert-file-contents candidate)
                       (let ((buffer-file-name candidate))
                         (set-auto-mode))
                       (font-lock-ensure)
                       (setq inhibit-read-only nil)))
      (if (and (helm-get-attr 'previewp)
               (string= candidate (helm-get-attr 'current-candidate)))
          (progn
            (kill-buffer buf)
            (helm-set-attr 'previewp nil))
        (preview candidate)
        (helm-set-attr 'previewp t)))
    (helm-set-attr 'current-candidate candidate)))

(defun helm-projectile-find-files-eshell-command-on-file-action (candidate)
  (interactive)
  (let* ((helm-ff-default-directory (file-name-directory candidate)))
    (helm-find-files-eshell-command-on-file candidate)))

(defun helm-projectile-ff-etags-select-action (candidate)
  (interactive)
  (let* ((helm-ff-default-directory (file-name-directory candidate)))
    (helm-ff-etags-select candidate)))

(defun helm-projectile-switch-to-shell (dir)
  "Within DIR, switch to a shell corresponding to `helm-ff-preferred-shell-mode'."
  (interactive)
  (let* ((projectile-require-project-root nil)
         (helm-ff-default-directory (file-name-directory (projectile-expand-root dir))))
    (helm-ff-switch-to-shell dir)))

(defun helm-projectile-files-in-current-dired-buffer ()
  "Return a list of files (only) in the current dired buffer."
  (let (flist)
    (cl-flet ((fpush (fname) (push fname flist)))
      (save-excursion
        (let (file buffer-read-only)
          (goto-char (point-min))
          (while (not (eobp))
            (save-excursion
              (and (not (eolp))
                   (setq file (dired-get-filename t t)) ; nil on non-file
                   (progn (end-of-line)
                          (funcall #'fpush file))))
            (forward-line 1)))))
    (mapcar 'file-truename (nreverse flist))))

(defun helm-projectile-all-dired-buffers ()
  "Get all current Dired buffers."
  (mapcar (lambda (b)
            (with-current-buffer b (buffer-name)))
          (cl-remove-if-not
           (lambda (b)
             (with-current-buffer b
               (and (eq major-mode 'dired-mode)
                    (buffer-name))))
           (buffer-list))))

(defvar helm-projectile-virtual-dired-remote-enable nil
  "Enable virtual Dired manager on remote host.
Disabled by default.")

(defun helm-projectile-dired-files-new-action (candidate)
  "Create a Dired buffer from chosen files.
CANDIDATE is the selected file, but choose the marked files if available."
  (if (and (file-remote-p (projectile-project-root))
           (not helm-projectile-virtual-dired-remote-enable))
      (message "Virtual Dired manager is disabled in remote host. Enable with %s."
               (propertize "helm-projectile-virtual-dired-remote-enable" 'face 'font-lock-keyword-face))
    (let ((files (cl-remove-if-not
                  (lambda (f)
                    (not (string= f "")))
                  (mapcar (lambda (file)
                            (replace-regexp-in-string (projectile-project-root) "" file))
                          (helm-marked-candidates :with-wildcard t))))
          (new-name (completing-read "Select or enter a new buffer name: "
                                     (helm-projectile-all-dired-buffers)))
          (helm--reading-passwd-or-string t)
          (default-directory (projectile-project-root)))
      ;; create a unique buffer that is unique to any directory in default-directory
      ;; or opened buffer; when Dired is passed with a non-existence directory name,
      ;; it only creates a buffer and insert everything. If a new name user supplied
      ;; exists as default-directory, Dired throws error when insert anything that
      ;; does not exist in current directory.
      (with-current-buffer (dired (cons (make-temp-name new-name)
                                        (if files
                                            files
                                          (list candidate))))
        (when (get-buffer new-name)
          (kill-buffer new-name))
        (rename-buffer new-name)))))

(defun helm-projectile-dired-files-add-action (candidate)
  "Add files to a Dired buffer.
CANDIDATE is the selected file.  Used when no file is explicitly marked."
  (if (and (file-remote-p (projectile-project-root))
           (not helm-projectile-virtual-dired-remote-enable))
      (message "Virtual Dired manager is disabled in remote host. Enable with %s."
               (propertize "helm-projectile-virtual-dired-remote-enable" 'face 'font-lock-keyword-face))
    (if (eq (with-helm-current-buffer major-mode) 'dired-mode)
        (let* ((marked-files (helm-marked-candidates :with-wildcard t))
               (helm--reading-passwd-or-string t)
               (root (projectile-project-root)) ; store root for later use
               (dired-buffer-name (or (and (eq major-mode 'dired-mode) (buffer-name))
                                      (completing-read "Select a Dired buffer:"
                                                       (helm-projectile-all-dired-buffers))))
               (dired-files (with-current-buffer dired-buffer-name
                              (helm-projectile-files-in-current-dired-buffer)))
               (files (sort (mapcar (lambda (file)
                                      (replace-regexp-in-string (projectile-project-root) "" file))
                                    (cl-nunion (if marked-files
                                                   marked-files
                                                 (list candidate))
                                               dired-files
                                               :test #'string-equal))
                            'string-lessp)))
          (kill-buffer dired-buffer-name)
          ;; Rebind default-directory because after killing a buffer, we
          ;; could be in any buffer and default-directory is set to that
          ;; random buffer
          ;;
          ;; Also use saved root directory, because after killing a buffer,
          ;; we could be outside of current project
          (let ((default-directory root))
            (with-current-buffer (dired (cons (make-temp-name dired-buffer-name)
                                              (if files
                                                  (mapcar (lambda (file)
                                                            (replace-regexp-in-string root "" file))
                                                          files)
                                                (list candidate))))
              (rename-buffer dired-buffer-name))))
      (error "You're not in a Dired buffer to add"))))

(defun helm-projectile-dired-files-delete-action (candidate)
  "Delete selected entries from a Dired buffer.
CANDIDATE is the selected file.  Used when no file is explicitly marked."
  (if (and (file-remote-p (projectile-project-root))
           (not helm-projectile-virtual-dired-remote-enable))
      (message "Virtual Dired manager is disabled in remote host. Enable with %s."
               (propertize "helm-projectile-virtual-dired-remote-enable" 'face 'font-lock-keyword-face))
    (let* ((helm--reading-passwd-or-string t)
           (root (projectile-project-root))
           (dired-buffer-name (with-helm-current-buffer (buffer-name)))
           (dired-files (with-current-buffer dired-buffer-name
                          (helm-projectile-files-in-current-dired-buffer)))
           (files (sort (cl-set-exclusive-or (helm-marked-candidates :with-wildcard t)
                                             dired-files
                                             :test #'string-equal) #'string-lessp)))
      (kill-buffer dired-buffer-name)
      ;; similar reason to `helm-projectile-dired-files-add-action'
      (let ((default-directory root))
        (with-current-buffer (dired (cons (make-temp-name dired-buffer-name)
                                          (if files
                                              (mapcar (lambda (file)
                                                        (replace-regexp-in-string root "" file))
                                                      files)
                                            (list candidate))))
          (rename-buffer dired-buffer-name))))))

(defun helm-projectile-run-projectile-hooks-after-find-file (_orig-fun &rest _args)
  "Run `projectile-find-file-hook' if using projectile."
  (when (and projectile-mode (projectile-project-p))
    (run-hooks 'projectile-find-file-hook)))

(advice-add 'helm-find-file-or-marked
            :after #'helm-projectile-run-projectile-hooks-after-find-file)

(defvar helm-projectile-find-file-map
  (let ((map (copy-keymap helm-find-files-map)))
    (helm-projectile-define-key map
      (kbd "C-c f") #'helm-projectile-dired-files-new-action
      (kbd "C-c a") #'helm-projectile-dired-files-add-action
      (kbd "M-e") #'helm-projectile-switch-to-shell
      (kbd "M-.") #'helm-projectile-ff-etags-select-action
      (kbd "M-!") #'helm-projectile-find-files-eshell-command-on-file-action)
    (define-key map (kbd "<left>") #'helm-previous-source)
    (define-key map (kbd "<right>") #'helm-next-source)
    (dolist (cmd '(helm-find-files-up-one-level
                   helm-find-files-down-last-level))
      (substitute-key-definition cmd nil map))
    map)
  "Mapping for file commands in Helm Projectile.")

(defvar helm-projectile-file-actions
  (helm-projectile-hack-actions
   helm-find-files-actions
   ;; Delete these actions
   'helm-ff-browse-project
   'helm-insert-file-name-completion-at-point
   'helm-ff-find-sh-command
   'helm-ff-cache-add-file
   ;; Substitute these actions
   '(helm-ff-switch-to-shell . helm-projectile-switch-to-shell)
   '(helm-ff-etags-select     . helm-projectile-ff-etags-select-action)
   '(helm-find-files-eshell-command-on-file
     . helm-projectile-find-files-eshell-command-on-file-action)
   ;; Change action descriptions
   '(helm-find-file-as-root . "Find file as root `C-c r'")
   ;; New actions
   '(helm-projectile-dired-files-new-action
     . "Create Dired buffer from files `C-c f'")
   '(helm-projectile-dired-files-add-action
     . "Add files to Dired buffer `C-c a'"))
  "Action for files.")

(defun helm-projectile--move-selection-p (selection)
  "Return non-nil if should move Helm selector after SELECTION.

SELECTION should be moved unless it's one of:

- Non-string
- Existing file
- Non-remote file that matches `helm-tramp-file-name-regexp'"
  (not (or (not (stringp selection))
         (file-exists-p selection)
         (and (string-match helm-tramp-file-name-regexp selection)
              (not (file-remote-p selection nil t))))))

(defun helm-projectile--move-to-real ()
  "Move to first real candidate.

Similar to `helm-ff--move-to-first-real-candidate', but without
unnecessary complexity."
  (while (let* ((src (helm-get-current-source))
                (selection (and (not (helm-empty-source-p))
                                (helm-get-selection nil nil src))))
           (and (not (helm-end-of-source-p))
                (helm-projectile--move-selection-p selection)))
    (helm-next-line)))

(defun helm-projectile--remove-move-to-real ()
  "Hook function to remove `helm-projectile--move-to-real'.

Meant to be added to `helm-cleanup-hook', from which it removes
 itself at the end."
  (remove-hook 'helm-after-update-hook #'helm-projectile--move-to-real)
  (remove-hook 'helm-cleanup-hook #'helm-projectile--remove-move-to-real))

(defvar helm-source-projectile-files-list-before-init-hook
  (lambda ()
    (add-hook 'helm-after-update-hook #'helm-projectile--move-to-real)
    (add-hook 'helm-cleanup-hook #'helm-projectile--remove-move-to-real)))

(defvar helm-source-projectile-files-list
  (helm-build-sync-source "Projectile files"
    :before-init-hook 'helm-source-projectile-files-list-before-init-hook
    :candidates (lambda ()
                  (when (projectile-project-p)
                    (with-helm-current-buffer
                      (cl-loop with root = (projectile-project-root)
                               for display in (projectile-current-project-files)
                               collect (cons display (expand-file-name display root))))))
    :filtered-candidate-transformer
    (lambda (files _source)
      (with-helm-current-buffer
        (let* ((root (projectile-project-root))
               (file-at-root (file-relative-name (expand-file-name helm-pattern root))))
          (if (or (string-empty-p helm-pattern)
                  (assoc helm-pattern files))
              files
            (if (equal helm-pattern file-at-root)
                (cl-acons (helm-ff-prefix-filename helm-pattern nil t)
                          (expand-file-name helm-pattern)
                          files)
              (cl-pairlis (list (helm-ff-prefix-filename helm-pattern nil t)
                                (helm-ff-prefix-filename file-at-root nil t))
                          (list (expand-file-name helm-pattern)
                                (expand-file-name helm-pattern root))
                          files))))))
    :fuzzy-match helm-projectile-fuzzy-match
    :keymap helm-projectile-find-file-map
    :help-message 'helm-ff-help-message
    :mode-line helm-read-file-name-mode-line-string
    :action helm-projectile-file-actions
    :persistent-action #'helm-projectile-file-persistent-action
    :persistent-help "Preview file")
  "Helm source definition for Projectile files.")

(defvar helm-source-projectile-files-in-all-projects-list
  (helm-build-sync-source "Projectile files in all Projects"
    :candidates (lambda ()
                  (with-helm-current-buffer
                    (let ((projectile-require-project-root nil))
                      (projectile-all-project-files))))
    :keymap helm-projectile-find-file-map
    :help-message 'helm-ff-help-message
    :mode-line helm-read-file-name-mode-line-string
    :action helm-projectile-file-actions
    :persistent-action #'helm-projectile-file-persistent-action
    :persistent-help "Preview file")
  "Helm source definition for all Projectile files in all projects.")

(defvar helm-projectile-dired-file-actions
  (helm-projectile-hack-actions
   helm-projectile-file-actions
   ;; New actions
   '(helm-projectile-dired-files-delete-action . "Remove entry(s) from Dired buffer `C-c d'")))

(defvar helm-source-projectile-dired-files-list
  (helm-build-in-buffer-source "Projectile files in current Dired buffer"
    :data (lambda ()
            (if (and (file-remote-p (projectile-project-root))
                     (not helm-projectile-virtual-dired-remote-enable))
                nil
              (when (eq major-mode 'dired-mode)
                (helm-projectile-files-in-current-dired-buffer))))
    :filter-one-by-one (lambda (file)
                         (let ((helm-ff-transformer-show-only-basename t))
                           (helm-ff-filter-candidate-one-by-one file)))
    :action-transformer 'helm-find-files-action-transformer
    :keymap (let ((map (copy-keymap helm-projectile-find-file-map)))
              (helm-projectile-define-key map
                (kbd "C-c d") 'helm-projectile-dired-files-delete-action)
              map)
    :help-message 'helm-ff-help-message
    :mode-line helm-read-file-name-mode-line-string
    :action helm-projectile-dired-file-actions)
  "Helm source definition for Projectile delete files.")

(defun helm-projectile-dired-find-dir (dir)
  "Jump to a selected directory DIR from `helm-projectile'."
  (dired (expand-file-name dir (projectile-project-root)))
  (run-hooks 'projectile-find-dir-hook))

(defun helm-projectile-dired-find-dir-other-window (dir)
  "Jump to a selected directory DIR from `helm-projectile'."
  (dired-other-window (expand-file-name dir (projectile-project-root)))
  (run-hooks 'projectile-find-dir-hook))

(defvar helm-source-projectile-directories-list
  (helm-build-sync-source "Projectile directories"
    :candidates (lambda ()
                  (when (projectile-project-p)
                    (with-helm-current-buffer
                      (let ((dirs (if projectile-find-dir-includes-top-level
                                      (append '("./") (projectile-current-project-dirs))
                                    (projectile-current-project-dirs))))
                        (helm-projectile--files-display-real dirs (projectile-project-root))))))
    :fuzzy-match helm-projectile-fuzzy-match
    :action-transformer 'helm-find-files-action-transformer
    :keymap (let ((map (make-sparse-keymap)))
              (set-keymap-parent map helm-map)
              (helm-projectile-define-key map
                (kbd "<left>") #'helm-previous-source
                (kbd "<right>") #'helm-next-source
                (kbd "C-c o") #'helm-projectile-dired-find-dir-other-window
                (kbd "M-e")   #'helm-projectile-switch-to-shell
                (kbd "C-c f") #'helm-projectile-dired-files-new-action
                (kbd "C-c a") #'helm-projectile-dired-files-add-action
                (kbd "C-s")   #'helm-projectile-grep)
              map)
    :help-message 'helm-ff-help-message
    :mode-line helm-read-file-name-mode-line-string
    :action '(("Open Dired" . helm-projectile-dired-find-dir)
              ("Open Dired in other window `C-c o'" . helm-projectile-dired-find-dir)
              ("Switch to Eshell `M-e'" . helm-projectile-switch-to-shell)
              ("Grep in projects `C-s'" . helm-projectile-grep)
              ("Create Dired buffer from files `C-c f'" . helm-projectile-dired-files-new-action)
              ("Add files to Dired buffer `C-c a'" . helm-projectile-dired-files-add-action)))
  "Helm source for listing project directories.")

(defvar helm-projectile-buffers-list-cache nil)

(defclass helm-source-projectile-buffer (helm-source-sync helm-type-buffer)
  ((init :initform (lambda ()
                     ;; Issue #51 Create the list before `helm-buffer' creation.
                     (setq helm-projectile-buffers-list-cache
                           (ignore-errors (remove (buffer-name) (projectile-project-buffer-names))))
                     (let ((result (cl-loop for b in helm-projectile-buffers-list-cache
                                            maximize (length b) into len-buf
                                            maximize (length (with-current-buffer b
                                                               (symbol-name major-mode)))
                                            into len-mode
                                            finally return (cons len-buf len-mode))))
                       (unless helm-buffer-max-length
                         (setq helm-buffer-max-length (car result)))
                       (unless helm-buffer-max-len-mode
                         ;; If a new buffer is longer that this value
                         ;; this value will be updated
                         (setq helm-buffer-max-len-mode (cdr result))))))
   (candidates :initform 'helm-projectile-buffers-list-cache)
   (matchplugin :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform 'helm-buffer-map)
   (volatile :initform t)
   (persistent-help
    :initform
    "Show this buffer / C-u \\[helm-execute-persistent-action]: Kill this buffer")))

(defvar helm-source-projectile-buffers-list (helm-make-source "Project buffers" 'helm-source-projectile-buffer))

(defvar helm-source-projectile-recentf-list
  (helm-build-sync-source "Projectile recent files"
    :candidates (lambda ()
                  (when (projectile-project-p)
                   (with-helm-current-buffer
                     (helm-projectile--files-display-real (projectile-recentf-files)
                                                          (projectile-project-root)))))
    :fuzzy-match helm-projectile-fuzzy-match
    :keymap helm-projectile-find-file-map
    :help-message 'helm-ff-help-message
    :mode-line helm-read-file-name-mode-line-string
    :action helm-projectile-file-actions
    :persistent-action #'helm-projectile-file-persistent-action
    :persistent-help "Preview file")
  "Helm source definition for recent files in current project.")

(defvar helm-source-projectile-files-and-dired-list
  '(helm-source-projectile-dired-files-list
    helm-source-projectile-files-list))

(defvar helm-source-projectile-directories-and-dired-list
  '(helm-source-projectile-dired-files-list
    helm-source-projectile-directories-list))

(defcustom helm-projectile-git-grep-command
  "git --no-pager grep --no-color -n%c -e %p -- %f"
  "Command to execute when performing `helm-grep' inside a projectile git project.
See documentation of `helm-grep-default-command' for the format."
  :type 'string
  :group 'helm-projectile
  )

(defcustom helm-projectile-grep-command
  "grep -a -r %e -n%cH -e %p %f ."
  "Command to execute when performing `helm-grep' outside a projectile git project.
See documentation of `helm-grep-default-command' for the format."
  :type 'string
  :group 'helm-projectile
  )


(defcustom helm-projectile-sources-list
  '(helm-source-projectile-buffers-list
    helm-source-projectile-files-list
    helm-source-projectile-projects)
  "Default sources for `helm-projectile'."
  :type '(repeat symbol)
  :group 'helm-projectile)

(defmacro helm-projectile-command (command source prompt &optional not-require-root truncate-lines-var)
  "Template for generic `helm-projectile' commands.
COMMAND is a command name to be appended with \"helm-projectile\" prefix.
SOURCE is a Helm source that should be Projectile specific.
PROMPT is a string for displaying as a prompt.
NOT-REQUIRE-ROOT specifies the command doesn't need to be used in a
project root.
TRUNCATE-LINES-VAR is the symbol used dictate truncation of lines.
Defaults is `helm-projectile-truncate-lines'."
  (unless truncate-lines-var (setq truncate-lines-var 'helm-projectile-truncate-lines))
  `(defun ,(intern (concat "helm-projectile-" command)) (&optional arg)
     "Use projectile with Helm for finding files in project

With a prefix ARG invalidates the cache first."
     (interactive "P")
     (if (projectile-project-p)
         (projectile-maybe-invalidate-cache arg)
       (unless ,not-require-root
         (error "You're not in a project")))
     (let ((helm-ff-transformer-show-only-basename nil)
           ;; for consistency, we should just let Projectile take care of ignored files
           (helm-boring-file-regexp-list nil))
       (helm :sources ,source
             :buffer (concat "*helm projectile: " (projectile-project-name) "*")
             :truncate-lines ,truncate-lines-var
             :prompt (projectile-prepend-project-name ,prompt)))))

(helm-projectile-command "switch-project" 'helm-source-projectile-projects "Switch to project: " t)
(helm-projectile-command "find-file" helm-source-projectile-files-and-dired-list "Find file: ")
(helm-projectile-command "find-file-in-known-projects" 'helm-source-projectile-files-in-all-projects-list "Find file in projects: " t)
(helm-projectile-command "find-dir" helm-source-projectile-directories-and-dired-list "Find dir: ")
(helm-projectile-command "recentf" 'helm-source-projectile-recentf-list "Recently visited file: ")
(helm-projectile-command "switch-to-buffer" 'helm-source-projectile-buffers-list "Switch to buffer: " nil helm-buffers-truncate-lines)
(helm-projectile-command "browse-dirty-projects" 'helm-source-projectile-dirty-projects "Select a project: " t)

(defun helm-projectile--files-display-real (files root)
  "Create (DISPLAY . REAL) pairs with FILES and ROOT.

  DISPLAY is the short file name.  REAL is the full path."
  (cl-loop for display in files
           collect (cons display (expand-file-name display root))))

;;;###autoload
(defun helm-projectile-find-file-dwim ()
  "Find file at point based on context."
  (interactive)
  (let* ((project-root (projectile-project-root))
         (project-files (projectile-current-project-files))
         (files (projectile-select-files project-files)))
    (if (= (length files) 1)
        (find-file (expand-file-name (car files) (projectile-project-root)))
      (helm :sources (helm-build-sync-source "Projectile files"
                       :candidates (if (> (length files) 1)
                                       (helm-projectile--files-display-real files project-root)
                                     (helm-projectile--files-display-real project-files project-root))
                       :fuzzy-match helm-projectile-fuzzy-match
                       :action-transformer 'helm-find-files-action-transformer
                       :keymap helm-projectile-find-file-map
                       :help-message helm-ff-help-message
                       :mode-line helm-read-file-name-mode-line-string
                       :action helm-projectile-file-actions
                       :persistent-action #'helm-projectile-file-persistent-action
                       :persistent-help "Preview file")
            :buffer "*helm projectile*"
            :truncate-lines helm-projectile-truncate-lines
            :prompt (projectile-prepend-project-name "Find file: ")))))

;;;###autoload
(defun helm-projectile-find-other-file (&optional flex-matching)
  "Switch between files with the same name but different extensions using Helm.
With FLEX-MATCHING, match any file that contains the base name of
current file.  Other file extensions can be customized with the
variable `projectile-other-file-alist'."
  (interactive "P")
  (let* ((project-root (projectile-project-root))
         (other-files (projectile-get-other-files (buffer-file-name)
                                                  flex-matching)))
    (if other-files
        (if (= (length other-files) 1)
            (find-file (expand-file-name (car other-files) project-root))
          (progn
            (let* ((helm-ff-transformer-show-only-basename nil))
              (helm :sources (helm-build-sync-source "Projectile other files"
                               :candidates (helm-projectile--files-display-real other-files project-root)
                               :keymap helm-projectile-find-file-map
                               :help-message helm-ff-help-message
                               :mode-line helm-read-file-name-mode-line-string
                               :action helm-projectile-file-actions
                               :persistent-action #'helm-projectile-file-persistent-action
                               :persistent-help "Preview file")
                    :buffer "*helm projectile*"
                    :truncate-lines helm-projectile-truncate-lines
                    :prompt (projectile-prepend-project-name "Find other file: ")))))
      (error "No other file found"))))

(defcustom helm-projectile-grep-or-ack-actions
  '("Find file" helm-grep-action
    "Find file other frame" helm-grep-other-frame
    (lambda () (and (locate-library "elscreen")
               "Find file in Elscreen"))
    helm-grep-jump-elscreen
    "Save results in grep buffer" helm-grep-save-results
    "Find file other window" helm-grep-other-window)
  "Available actions for `helm-projectile-grep-or-ack'.
The contents of this list are passed as the arguments to `helm-make-actions'."
  :type 'symbol
  :group 'helm-projectile)

(defcustom helm-projectile-set-input-automatically t
  "If non-nil, attempt to set search input automatically.
Automatic input selection uses the region (if there is an active
region), otherwise it uses the current symbol at point (if there
is one).  Applies to `helm-projectile-grep' and
`helm-projectile-ack' only.  If the `helm-ag' package is
installed, then automatic input behavior for `helm-projectile-ag'
can be customized using `helm-ag-insert-at-point'."
  :group 'helm-projectile
  :type 'boolean)

(defun helm-projectile-grep-or-ack (&optional dir use-ack-p ack-ignored-pattern ack-executable)
  "Perform helm-grep at project root.
DIR directory where to search
USE-ACK-P indicates whether to use ack or not.
ACK-IGNORED-PATTERN is a file regex to exclude from searching.
ACK-EXECUTABLE is the actual ack binary name.
It is usually \"ack\" or \"ack-grep\".
If it is nil, or ack/ack-grep not found then use default grep command."
  (let* ((default-directory (or dir (projectile-project-root)))
         (helm-ff-default-directory default-directory)
         (helm-grep-in-recurse t)
         (helm-grep-ignored-files (cl-union (projectile-ignored-files-rel)  grep-find-ignored-files))
         (helm-grep-ignored-directories
          (cl-union (mapcar 'directory-file-name (projectile-ignored-directories-rel))
                    grep-find-ignored-directories))
         (helm-grep-default-command (if use-ack-p
                                        (concat ack-executable " -H --no-group --no-color " ack-ignored-pattern " %p %f")
                                      (if (and projectile-use-git-grep (eq (projectile-project-vcs) 'git))
                                          helm-projectile-git-grep-command
                                        helm-projectile-grep-command)))
         (helm-grep-default-recurse-command helm-grep-default-command))

    (setq helm-source-grep
          (helm-build-async-source
              (capitalize (helm-grep-command t))
            :header-name (lambda (_name)
                           (let ((name (if use-ack-p
                                           "Helm Projectile Ack"
                                         "Helm Projectile Grep")))
                             (concat name " " "(C-c ? Help)")))
            :candidates-process 'helm-grep-collect-candidates
            :filter-one-by-one 'helm-grep-filter-one-by-one
            :candidate-number-limit 9999
            :nohighlight t
            ;; We need to specify keymap here and as :keymap arg [1]
            ;; to make it available in further resuming.
            :keymap helm-grep-map
            :history 'helm-grep-history
            :action (apply #'helm-make-actions helm-projectile-grep-or-ack-actions)
            :persistent-action 'helm-grep-persistent-action
            :persistent-help "Jump to line (`C-u' Record in mark ring)"
            :requires-pattern 2))
    (helm
     :sources 'helm-source-grep
     :input (when helm-projectile-set-input-automatically
              (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (thing-at-point 'symbol)))
     :buffer (format "*helm %s*" (if use-ack-p
                                     "ack"
                                   "grep"))
     :default-directory default-directory
     :keymap helm-grep-map
     :history 'helm-grep-history
     :truncate-lines helm-grep-truncate-lines)))

;;;###autoload
(defun helm-projectile-on ()
  "Turn on `helm-projectile' key bindings."
  (interactive)
  (message "Turn on helm-projectile key bindings")
  (helm-projectile-toggle 1))

;;;###autoload
(defun helm-projectile-off ()
  "Turn off `helm-projectile' key bindings."
  (interactive)
  (message "Turn off helm-projectile key bindings")
  (helm-projectile-toggle -1))

;;;###autoload
(defun helm-projectile-grep (&optional dir)
  "Helm version of `projectile-grep'.
DIR is the project root, if not set then current directory is used"
  (interactive)
  (let ((project-root (or dir (projectile-project-root) (error "You're not in a project"))))
    (funcall 'run-with-timer 0.01 nil
             #'helm-projectile-grep-or-ack project-root nil)))

;;;###autoload
(defun helm-projectile-ack (&optional dir)
  "Helm version of projectile-ack."
  (interactive)
  (let ((project-root (or dir (projectile-project-root) (error "You're not in a project"))))
    (let ((ack-ignored (mapconcat
                        'identity
                        (cl-union (mapcar (lambda (path)
                                            (concat "--ignore-dir=" (file-name-nondirectory (directory-file-name path))))
                                          (projectile-ignored-directories))
                                  (mapcar (lambda (path)
                                            (concat "--ignore-file=match:" (shell-quote-argument path)))
                                          (append (projectile-ignored-files) (projectile-patterns-to-ignore)))) " "))
          (helm-ack-grep-executable (cond
                                     ((executable-find "ack") "ack")
                                     ((executable-find "ack-grep") "ack-grep")
                                     (t (error "ack or ack-grep is not available")))))
      (funcall 'run-with-timer 0.01 nil
               #'helm-projectile-grep-or-ack project-root t ack-ignored helm-ack-grep-executable))))

;;;###autoload
(defun helm-projectile-ag (&optional options)
  "Helm version of `projectile-ag'."
  (interactive (if current-prefix-arg (list (helm-read-string "option: " "" 'helm-ag--extra-options-history))))
  (if (require 'helm-ag nil t)
      (if (projectile-project-p)
          (let* ((grep-find-ignored-files (cl-union (projectile-ignored-files-rel) grep-find-ignored-files))
                 (grep-find-ignored-directories (cl-union (projectile-ignored-directories-rel) grep-find-ignored-directories))
                 (ignored (mapconcat (lambda (i)
                                       (concat "--ignore " i))
                                     (append grep-find-ignored-files grep-find-ignored-directories (cadr (projectile-parse-dirconfig-file)))
                                     " "))
                 (helm-ag-base-command (concat helm-ag-base-command " " ignored " " options))
                 (current-prefix-arg nil))
            (helm-do-ag (projectile-project-root) (car (projectile-parse-dirconfig-file))))
        (error "You're not in a project"))
    (when (yes-or-no-p "`helm-ag' is not installed. Install? ")
      (condition-case nil
          (progn
            (package-install 'helm-ag)
            (helm-projectile-ag options))
        (error (error "`helm-ag' is not available.  Is MELPA in your `package-archives'?"))))))

;; Declare/define these to satisfy the byte compiler
(defvar helm-rg-prepend-file-name-line-at-top-of-matches)
(defvar helm-rg-include-file-on-every-match-line)
(declare-function helm-rg "ext:helm-rg")
(declare-function helm-rg--get-thing-at-pt "ext:helm-rg")

(defun helm-projectile-rg--region-selection ()
  (when helm-projectile-set-input-automatically
    (if (region-active-p)
        (buffer-substring-no-properties (region-beginning) (region-end))
      (helm-rg--get-thing-at-pt))))

(defun glob-quote (string)
  "Quote the special glob characters: *, ?, [, and ].
STRING the string in which to escape special characters."
  (replace-regexp-in-string "[]*?[]" "\\\\\\&" string))

;;;###autoload
(defun helm-projectile-rg ()
  "Projectile version of `helm-rg'."
  (interactive)
  (if (require 'helm-rg nil t)
      (if (projectile-project-p)
          (let* ((helm-rg-prepend-file-name-line-at-top-of-matches nil)
                 (helm-rg-include-file-on-every-match-line t)
                 (ignored-files (mapcan (lambda (path)
                                          (list "--glob" (concat "!" (glob-quote path))))
                                        (cl-union (projectile-ignored-files-rel)  grep-find-ignored-files)))
                 (ignored-directories (mapcan (lambda (path)
                                                   (list "--glob" (concat "!" (glob-quote path) "/**")))
                                              (cl-union (mapcar 'directory-file-name (projectile-ignored-directories-rel))
                                                        grep-find-ignored-directories)))
                 (helm-rg--extra-args `(,@ignored-files ,@ignored-directories)))
            (let ((default-directory (projectile-project-root)))
              (helm-rg (helm-projectile-rg--region-selection)
                       nil)))
        (error "You're not in a project"))
    (when (yes-or-no-p "`helm-rg' is not installed. Install? ")
      (condition-case nil
          (progn
            (package-install 'helm-rg)
            (helm-projectile-rg))
        (error "`helm-rg' is not available.  Is MELPA in your `package-archives'?")))))

(defun helm-projectile-commander-bindings ()
  (def-projectile-commander-method ?a
    "Run ack on project."
    (call-interactively 'helm-projectile-ack))

  (def-projectile-commander-method ?A
    "Find ag on project."
    (call-interactively 'helm-projectile-ag))

  (def-projectile-commander-method ?f
    "Find file in project."
    (helm-projectile-find-file))

  (def-projectile-commander-method ?b
    "Switch to project buffer."
    (helm-projectile-switch-to-buffer))

  (def-projectile-commander-method ?d
    "Find directory in project."
    (helm-projectile-find-dir))

  (def-projectile-commander-method ?g
    "Run grep on project."
    (helm-projectile-grep))

  (def-projectile-commander-method ?s
    "Switch project."
    (helm-projectile-switch-project))

  (def-projectile-commander-method ?e
    "Find recently visited file in project."
    (helm-projectile-recentf))

  (def-projectile-commander-method ?V
    "Find dirty projects."
    (helm-projectile-browse-dirty-projects)))

;;;###autoload
(defun helm-projectile-toggle (toggle)
  "Toggle Helm version of Projectile commands."
  (if (> toggle 0)
      (progn
        (when (eq projectile-switch-project-action #'projectile-find-file)
          (setq projectile-switch-project-action #'helm-projectile-find-file))
        (define-key projectile-mode-map [remap projectile-find-other-file] #'helm-projectile-find-other-file)
        (define-key projectile-mode-map [remap projectile-find-file] #'helm-projectile-find-file)
        (define-key projectile-mode-map [remap projectile-find-file-in-known-projects] #'helm-projectile-find-file-in-known-projects)
        (define-key projectile-mode-map [remap projectile-find-file-dwim] #'helm-projectile-find-file-dwim)
        (define-key projectile-mode-map [remap projectile-find-dir] #'helm-projectile-find-dir)
        (define-key projectile-mode-map [remap projectile-switch-project] #'helm-projectile-switch-project)
        (define-key projectile-mode-map [remap projectile-recentf] #'helm-projectile-recentf)
        (define-key projectile-mode-map [remap projectile-switch-to-buffer] #'helm-projectile-switch-to-buffer)
        (define-key projectile-mode-map [remap projectile-grep] #'helm-projectile-grep)
        (define-key projectile-mode-map [remap projectile-ack] #'helm-projectile-ack)
        (define-key projectile-mode-map [remap projectile-ag] #'helm-projectile-ag)
        (define-key projectile-mode-map [remap projectile-ripgrep] #'helm-projectile-rg)
        (define-key projectile-mode-map [remap projectile-browse-dirty-projects] #'helm-projectile-browse-dirty-projects)
        (helm-projectile-commander-bindings))
    (progn
      (when (eq projectile-switch-project-action #'helm-projectile-find-file)
        (setq projectile-switch-project-action #'projectile-find-file))
      (define-key projectile-mode-map [remap projectile-find-other-file] nil)
      (define-key projectile-mode-map [remap projectile-find-file] nil)
      (define-key projectile-mode-map [remap projectile-find-file-in-known-projects] nil)
      (define-key projectile-mode-map [remap projectile-find-file-dwim] nil)
      (define-key projectile-mode-map [remap projectile-find-dir] nil)
      (define-key projectile-mode-map [remap projectile-switch-project] nil)
      (define-key projectile-mode-map [remap projectile-recentf] nil)
      (define-key projectile-mode-map [remap projectile-switch-to-buffer] nil)
      (define-key projectile-mode-map [remap projectile-grep] nil)
      (define-key projectile-mode-map [remap projectile-ag] nil)
      (define-key projectile-mode-map [remap projectile-ripgrep] nil)
      (define-key projectile-mode-map [remap projectile-browse-dirty-projects] nil)
      (projectile-commander-bindings))))

;;;###autoload
(defun helm-projectile (&optional arg)
  "Use projectile with Helm instead of ido.

With a prefix ARG invalidates the cache first.
If invoked outside of a project, displays a list of known projects to jump."
  (interactive "P")
  (if (not (projectile-project-p))
      (helm-projectile-switch-project arg)
    (projectile-maybe-invalidate-cache arg)
    (let ((helm-ff-transformer-show-only-basename nil))
      (helm :sources helm-projectile-sources-list
            :buffer "*helm projectile*"
            :truncate-lines helm-projectile-truncate-lines
            :prompt (projectile-prepend-project-name (if (projectile-project-p)
                                                         "pattern: "
                                                       "Switch to project: "))))))

;;;###autoload
(eval-after-load 'projectile
  '(progn
     (define-key projectile-command-map (kbd "h") #'helm-projectile)))

(provide 'helm-projectile)

;;; helm-projectile.el ends here
