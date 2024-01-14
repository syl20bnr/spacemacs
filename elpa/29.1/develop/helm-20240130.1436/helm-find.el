;;; helm-find.el --- helm interface for find command. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2023 Thierry Volpiatto 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'helm-files)
(require 'helm-external)

(defcustom helm-findutils-skip-boring-files t
  "Ignore boring files in find command results."
  :group 'helm-files
  :type  'boolean)

(defcustom helm-findutils-search-full-path nil
  "Search in full path with shell command find when non-nil.
I.e. use the -path/ipath arguments of find instead of
-name/iname."
  :group 'helm-files
  :type 'boolean)

(defcustom helm-find-noerrors nil
  "Prevent showing error messages in helm buffer when non nil."
  :group 'helm-files
  :type 'boolean)

(defvar helm-find-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-generic-files-map)
    (define-key map (kbd "DEL") 'helm-delete-backward-no-update)
    map))

(defvar helm-source-findutils
  (helm-build-async-source "Find"
    :header-name (lambda (name)
                   (concat name " in [" (helm-default-directory) "]"))
    :candidates-process 'helm-find-shell-command-fn
    :filtered-candidate-transformer 'helm-findutils-transformer
    :action-transformer 'helm-transform-file-load-el
    :persistent-action 'helm-ff-kill-or-find-buffer-fname
    :action 'helm-type-file-actions
    :help-message 'helm-generic-file-help-message
    :keymap helm-find-map
    :candidate-number-limit 9999
    :requires-pattern 3))

(defun helm-findutils-transformer (candidates _source)
  (let (non-essential
        (default-directory (helm-default-directory)))
    (cl-loop for i in candidates
             for abs = (expand-file-name
                        (helm-aif (file-remote-p default-directory)
                            (concat it i) i))
             for type = (car (file-attributes abs))
             for disp = (if (and helm-ff-transformer-show-only-basename
                                 (not (string-match "[.]\\{1,2\\}$" i)))
                            (helm-basename abs) abs)
             collect (cond ((eq t type)
                            (cons (propertize disp 'face 'helm-ff-directory)
                                  abs))
                           ((stringp type)
                            (cons (propertize disp 'face 'helm-ff-symlink)
                                  abs))
                           (t (cons (propertize disp 'face 'helm-ff-file)
                                    abs))))))

(defun helm-find--build-cmd-line ()
  (require 'find-cmd)
  (let* ((default-directory (or (file-remote-p default-directory 'localname)
                                default-directory))
         (patterns+options (split-string helm-pattern "\\(\\`\\| +\\)\\* +"))
         (fold-case (helm-set-case-fold-search (car patterns+options)))
         (patterns (split-string (car patterns+options)))
         (additional-options (and (cdr patterns+options)
                                  (list (concat (cadr patterns+options) " "))))
         (ignored-dirs ())
         (ignored-files (when helm-findutils-skip-boring-files
                          (cl-loop for f in completion-ignored-extensions
                                   if (string-match "/$" f)
                                   do (push (replace-match "" nil t f)
                                            ignored-dirs)
                                   else collect (concat "*" f))))
         (path-or-name (if helm-findutils-search-full-path
                           '(ipath path) '(iname name)))
         (name-or-iname (if fold-case
                            (car path-or-name) (cadr path-or-name))))
    (find-cmd (and ignored-dirs
                   `(prune (name ,@ignored-dirs)))
              (and ignored-files
                   `(not (name ,@ignored-files)))
              `(and ,@(mapcar
                       (lambda (pattern)
                         `(,name-or-iname ,(concat "*" pattern "*")))
                       patterns)
                    ,@additional-options))))

(defun helm-find-shell-command-fn ()
  "Asynchronously fetch candidates for `helm-find'.
Additional find options can be specified after a \"*\"
separator."
  (let* (process-connection-type
         non-essential
         (cmd (concat (helm-find--build-cmd-line)
                      (if helm-find-noerrors "2> /dev/null" "")))
         (proc (start-file-process-shell-command "hfind" helm-buffer cmd)))
    (helm-log "helm-find-shell-command-fn" "Find command:\n%s" cmd)
    (prog1 proc
      (set-process-sentinel
       proc
       (lambda (process event)
           (helm-process-deferred-sentinel-hook
            process event (helm-default-directory))
           (if (string= event "finished\n")
               (helm-locate-update-mode-line "Find")
             (helm-log "helm-find-shell-command-fn sentinel" "Error: Find %s"
                       (replace-regexp-in-string "\n" "" event))))))))

(defun helm-find-1 (dir)
  (let ((default-directory (file-name-as-directory dir)))
    (helm :sources 'helm-source-findutils
          :buffer "*helm find*"
          :ff-transformer-show-only-basename nil
          :case-fold-search helm-file-name-case-fold-search)))


;;; Preconfigured commands
;;
;;
;;;###autoload
(defun helm-find (arg)
  "Preconfigured `helm' for the find shell command.

Recursively find files whose names are matched by all specified
globbing PATTERNs under the current directory using the external
program specified in `find-program' (usually \"find\").  Every
input PATTERN is silently wrapped into two stars: *PATTERN*.

With prefix argument, prompt for a directory to search.

When user option `helm-findutils-search-full-path' is non-nil,
match against complete paths, otherwise, against file names
without directory part.

The (possibly empty) list of globbing PATTERNs can be followed by
the separator \"*\" plus any number of additional arguments that
are passed to \"find\" literally."
  (interactive "P")
  (let ((directory
         (if arg
             (file-name-as-directory
              (read-directory-name "DefaultDirectory: "))
           default-directory)))
    (helm-find-1 directory)))

(provide 'helm-find)

;;; helm-find.el ends here
