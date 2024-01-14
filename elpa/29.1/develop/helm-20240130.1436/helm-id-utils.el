;;; helm-id-utils.el --- Helm interface for id-utils. -*- lexical-binding: t -*-

;; Copyright (C) 2015 ~ 2020 Thierry Volpiatto 

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

(require 'helm-grep)
(require 'helm-help)

(defgroup helm-id-utils nil
  "ID-Utils related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-gid-program "gid"
  "Name of gid command (usually `gid').
For Mac OS X users, if you install GNU coreutils, the name `gid'
might be occupied by `id' from GNU coreutils, and you should set
it to correct name (or absolute path).  For example, if using
MacPorts to install id-utils, it should be `gid32'."
  :group 'helm-id-utils
  :type 'file)

(defcustom helm-gid-db-file-name "ID"
  "Name of a database file created by `mkid' command from `ID-utils'."
  :group 'helm-id-utils
  :type 'string)

(defun helm-gid-candidates-process ()
  (let* ((patterns (helm-mm-split-pattern helm-pattern))
         (default-com (format "%s -r %s" helm-gid-program
                              (shell-quote-argument (car patterns))))
         (cmd (helm-aif (cdr patterns)
                  (concat default-com
                          (cl-loop for p in it
                                   concat (format " | grep --color=always %s"
                                                  (shell-quote-argument p))))
                default-com))
         (proc (start-process-shell-command
                "gid" helm-buffer cmd)))
    (set (make-local-variable 'helm-grep-last-cmd-line) cmd)
    (prog1 proc
      (set-process-sentinel
       proc (lambda (_process event)
              (when (string= event "finished\n")
                (helm-maybe-show-help-echo)
                (with-helm-window
                  (setq mode-line-format
                        '(" " mode-line-buffer-identification " "
                          (:eval (format "L%s" (helm-candidate-number-at-point))) " "
                          (:eval (propertize
                                  (format "[Helm Gid process finished - (%s results)]"
                                          (max (1- (count-lines
                                                    (point-min) (point-max)))
                                               0))
                                  'face 'helm-locate-finish))))
                  (force-mode-line-update))
                (helm-log "helm-gid-candidates-process" "Error: Gid %s"
                          (replace-regexp-in-string "\n" "" event))))))))

(defun helm-gid-filtered-candidate-transformer (candidates _source)
  ;; "gid -r" may add dups in some rare cases.
  (cl-loop for c in (helm-fast-remove-dups candidates :test 'equal)
           collect (helm-grep--filter-candidate-1 c)))

(defclass helm-gid-source (helm-source-async)
  ((header-name
    :initform
    (lambda (name)
      (concat name " [" (helm-get-attr 'db-dir) "]")))
   (db-dir :initarg :db-dir
           :initform nil
           :custom string
           :documentation " Location of ID file.")
   (candidates-process :initform #'helm-gid-candidates-process)
   (filtered-candidate-transformer
    :initform #'helm-gid-filtered-candidate-transformer)
   (candidate-number-limit :initform 99999)
   (action :initform (helm-make-actions
                      "Find File" 'helm-grep-action
                      "Find file other frame" 'helm-grep-other-frame
                      "Save results in grep buffer" 'helm-grep-save-results
                      "Find file other window" 'helm-grep-other-window))
   (persistent-action :initform 'helm-grep-persistent-action)
   (history :initform 'helm-grep-history)
   (nohighlight :initform t)
   (help-message :initform 'helm-grep-help-message)
   (requires-pattern :initform 2)))

;;;###autoload
(defun helm-gid ()
  "Preconfigured `helm' for `gid' command line of `ID-Utils'.
Need A database created with the command `mkid' above
`default-directory'.
Need id-utils as dependency which provide `mkid', `gid' etc..
See <https://www.gnu.org/software/idutils/>."
  (interactive)
  (let* ((db (locate-dominating-file
              default-directory
              helm-gid-db-file-name))
         (helm-grep-default-directory-fn
          (lambda () default-directory))
         (helm-maybe-use-default-as-input t))
    (cl-assert db nil "No DataBase found, create one with `mkid'")
    (helm :sources (helm-make-source "Gid" 'helm-gid-source
                     :db-dir db)
          :buffer "*helm gid*"
          :keymap helm-grep-map
          :truncate-lines helm-grep-truncate-lines)))

(provide 'helm-id-utils)

;;; helm-id-utils ends here
