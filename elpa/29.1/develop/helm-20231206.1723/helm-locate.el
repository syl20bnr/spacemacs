;;; helm-locate.el --- helm interface for locate. -*- lexical-binding: t -*-

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

;; NOTE for WINDOZE users:
;; You have to install Everything with his command line interface here:
;; http://www.voidtools.com/download.php

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-types)
(require 'helm-help)

(defvar helm-ff-default-directory)
(declare-function helm-read-file-name "helm-mode")


(defgroup helm-locate nil
  "Locate related Applications and libraries for Helm."
  :group 'helm)

(defcustom helm-locate-db-file-regexp "m?locate\\.db$"
  "Default regexp to match locate database.
If nil Search in all files."
  :type  'string)

(defcustom helm-ff-locate-db-filename "locate.db"
  "The basename of the locatedb file you use locally in your directories.
When this is set and Helm finds such a file in the directory from
where you launch locate, it will use this file and will not
prompt you for a db file.
Note that this happen only when locate is launched with a prefix
arg."
  :type 'string)

(defcustom helm-locate-command nil
  "A list of arguments for locate program.

Helm will calculate a default value for your system on startup
unless `helm-locate-command' is non-nil.

Here are the default values it will use according to your system:

Gnu/linux:     \"locate %s -e -A -N --regex %s\"
berkeley-unix: \"locate %s %s\"
windows-nt:    \"es %s %s\"
Others:        \"locate %s %s\"

This string will be passed to format so it should end with `%s'.
The first format spec is used for the \"-i\" value of locate/es,
so don't set it directly but use `helm-locate-case-fold-search'
for this.

The last option must be the one preceding pattern i.e \"-r\" or
\"--regex\".

The option \"-N\" may not be available on old locate versions, it is needed on
latest systems as locate send quoted filenames, it is BTW enabled by default, if
this option is not recognized on your system, remove it.
 
You will be able to pass other options such as \"-b\" or \"l\"
during Helm invocation after entering pattern only when multi
matching, not when fuzzy matching.

Note that the \"-b\" option is added automatically by Helm when
var `helm-locate-fuzzy-match' is non-nil and switching back from
multimatch to fuzzy matching (this is done automatically when a
space is detected in pattern)."
  :type 'string)

(defcustom helm-locate-create-db-command
  "updatedb -l 0 -o '%s' -U '%s'"
  "Command used to create a locale locate db file."
  :type 'string)

(defcustom helm-locate-case-fold-search helm-case-fold-search
  "It have the same meaning as `helm-case-fold-search'.
The -i option of locate will be used depending of value of
`helm-pattern' when this is set to \\='smart.
When nil \"-i\" will not be used at all and when non-nil it will
always be used.
NOTE: the -i option of the \"es\" command used on windows does
the opposite of \"locate\" command."
  :type 'symbol)

(defcustom helm-locate-fuzzy-match nil
  "Enable fuzzy matching in `helm-locate'.
Note that when this is enabled searching is done on basename."
  :type 'boolean)

(defcustom helm-locate-fuzzy-sort-fn
  #'helm-locate-default-fuzzy-sort-fn
  "Default fuzzy matching sort function for locate."
  :type 'boolean)

(defcustom helm-locate-project-list nil
  "A list of directories, your projects.
When set, allow browsing recursively files in all directories of
this list with `helm-projects-find-files'."
  :type '(repeat string))

(defcustom helm-locate-recursive-dirs-command "locate -i -e -A --regex '^%s' '%s.*$'"
  "Command used for recursive directories completion in `helm-find-files'.

For Windows and `es' use something like \"es -r ^%s.*%s.*$\"

The two format specs are mandatory.

If for some reasons you can't use locate because your filesystem
doesn't have a database, you can use find command from findutils
but be aware that it will be much slower.  See `helm-find-files'
embedded help for more infos."
  :type 'string
  :group 'helm-files)


(defvar helm-locate-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-generic-files-map)
    (define-key map (kbd "DEL") 'helm-delete-backward-no-update)
    map))

(defface helm-locate-finish
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :foreground "Green"))
  "Face used in mode line when locate process is finish."
  :group 'helm-locate)


(defun helm-ff-find-locatedb (&optional from-ff)
  "Try to find if a local locatedb file is available.
The search is done in `helm-ff-default-directory' or falls back to
`default-directory' if FROM-FF is nil."
  (helm-aif (and helm-ff-locate-db-filename
                 (locate-dominating-file
                  (or (and from-ff
                           helm-ff-default-directory)
                      default-directory)
                  helm-ff-locate-db-filename))
      (expand-file-name helm-ff-locate-db-filename it)))

(defun helm-locate-create-db-default-function (db-name directory)
  "Default function used to create a locale locate db file.
Argument DB-NAME name of the db file.
Argument DIRECTORY root of file system subtree to scan."
  (format helm-locate-create-db-command
          db-name (expand-file-name directory)))

(defvar helm-locate-create-db-function
  #'helm-locate-create-db-default-function
  "Function used to create a locale locate db file.
It should receive the same arguments as
`helm-locate-create-db-default-function'.")

(defun helm-locate-1 (&optional localdb init from-ff default)
  "Generic function to run Locate.
Prefix arg LOCALDB when (4) search and use a local locate db file
when it exists or create it, when (16) force update of existing
db file even if exists.
It has no effect when locate command is \\='es'.  INIT is a string
to use as initial input in prompt.
See `helm-locate-with-db' and `helm-locate'."
  (require 'helm-mode)
  (helm-locate-set-command)
  (let ((pfn (lambda (candidate)
                 (if (file-directory-p candidate)
                     (message "Error: The locate Db should be a file")
                   (if (= (shell-command
                           (funcall helm-locate-create-db-function
                                    candidate
                                    helm-ff-default-directory))
                          0)
                       (message "New locatedb file `%s' created" candidate)
                     (error "Failed to create locatedb file `%s'" candidate)))))
        (locdb (and localdb
                    (not (string-match "^es" helm-locate-command))
                    (or (and (equal '(4) localdb)
                             (helm-ff-find-locatedb from-ff))
                        (helm-read-file-name
                         "Create Locate Db file: "
                         :initial-input (expand-file-name "locate.db"
                                                          (or helm-ff-default-directory
                                                              default-directory))
                         :preselect helm-locate-db-file-regexp
                         :test (lambda (x)
                                   (if helm-locate-db-file-regexp
                                       ;; Select only locate db files and directories
                                       ;; to allow navigation.
                                       (or (string-match
                                            helm-locate-db-file-regexp x)
                                           (file-directory-p x))
                                     x)))))))
    (when (and locdb (or (equal localdb '(16))
                         (not (file-exists-p locdb))))
      (funcall pfn locdb))
    (helm-locate-with-db (and localdb locdb) init default)))

(defun helm-locate-set-command ()
  "Setup `helm-locate-command' if not already defined."
  (unless helm-locate-command
    (setq helm-locate-command
          (cl-case system-type
            ;; Use -N option by default (bug#2625)
            (gnu/linux "locate %s -e -A -N --regex %s")
            (berkeley-unix "locate %s %s")
            (windows-nt "es %s %s")
            (t "locate %s %s")))))

(defun helm-locate-initial-setup ()
  (require 'helm-for-files)
  (helm-locate-set-command))

(defvar helm-file-name-history nil)
(defun helm-locate-with-db (&optional db initial-input default)
  "Run locate -d DB.
If DB is not given or nil use locate without -d option.
Argument DB can be given as a string or list of db files.
Argument INITIAL-INPUT is a string to use as initial-input.
See also `helm-locate'."
  (require 'helm-files)
  (when (and db (stringp db)) (setq db (list db)))
  (helm-locate-set-command)
  (let ((helm-locate-command
         (if db
             (replace-regexp-in-string
              "locate"
              (format (if helm-locate-fuzzy-match
                          "locate -b -d '%s'" "locate -d '%s'")
                      (mapconcat 'identity
                                 ;; Remove eventually
                                 ;; marked directories by error.
                                 (cl-loop for i in db
                                       unless (file-directory-p i)
                                       ;; expand-file-name to resolve
                                       ;; abbreviated fnames not
                                       ;; expanding inside single
                                       ;; quotes i.e. '%s'.
                                       collect (expand-file-name i))
                                 ":"))
              helm-locate-command)
           (if (and helm-locate-fuzzy-match
                    (not (string-match-p "\\`locate -b" helm-locate-command)))
               (replace-regexp-in-string
                "\\`locate" "locate -b" helm-locate-command)
               helm-locate-command))))
    (setq helm-file-name-history (mapcar 'helm-basename file-name-history))
    (helm :sources 'helm-source-locate
          :buffer "*helm locate*"
          :ff-transformer-show-only-basename nil
          :input initial-input
          :default default
          :history 'helm-file-name-history)))

(defun helm-locate-update-mode-line (process-name)
  "Update mode-line with PROCESS-NAME status information."
  (with-helm-window
    (setq mode-line-format
          `(" " mode-line-buffer-identification " "
            (:eval (format "L%s" (helm-candidate-number-at-point))) " "
            (:eval (propertize
                    (format "[%s process finished - (%s results)]"
                            (max (1- (count-lines
                                      (point-min) (point-max)))
                                 0)
                            ,process-name)
                    'face 'helm-locate-finish))))
    (force-mode-line-update)))

(defun helm-locate--default-process-coding-system ()
  "Fix `default-process-coding-system' in locate for Windows systems."
  ;; This is an attempt to fix issue #1322.
  (if (and (eq system-type 'windows-nt)
           (boundp 'w32-ansi-code-page))
      (let ((code-page-eol
             (intern (format "cp%s-%s" w32-ansi-code-page "dos"))))
        (if (ignore-errors (check-coding-system code-page-eol))
            (cons code-page-eol code-page-eol)
          default-process-coding-system))
    default-process-coding-system))

(defun helm-locate-init ()
  "Initialize async locate process for `helm-source-locate'."
  (let* ((default-process-coding-system
          (helm-locate--default-process-coding-system))
         (locate-is-es (string-match "\\`es" helm-locate-command))
         (real-locate (string-match "\\`locate" helm-locate-command))
         (case-sensitive-flag (if locate-is-es "-i" ""))
         (ignore-case-flag (if (or locate-is-es
                                   (not real-locate)) "" "-i"))
         (args (helm-mm-split-pattern helm-pattern))
         (cmd (format helm-locate-command
                      (cl-case helm-locate-case-fold-search
                        (smart (let ((case-fold-search nil))
                                 (if (string-match "[[:upper:]]" helm-pattern)
                                     case-sensitive-flag
                                     ignore-case-flag)))
                        (t (if helm-locate-case-fold-search
                               ignore-case-flag
                               case-sensitive-flag)))
                      (helm-aif (cdr args)
                          (concat
                           ;; The pattern itself.
                           (shell-quote-argument (car args)) " "
                           ;; Possible locate args added
                           ;; after pattern, don't quote them.
                           (mapconcat 'identity it " "))
                        (shell-quote-argument (car args)))))
         (default-directory (if (file-directory-p default-directory)
                                default-directory "/")))
    (helm-log "helm-locat-init" "Starting helm-locate process")
    (helm-log "helm-locat-init" "Command line used was:\n\n%s"
              (concat ">>> " (propertize cmd 'face 'font-lock-comment-face) "\n\n"))
    (prog1
        (start-process-shell-command
         "locate-process" helm-buffer
         cmd)
      (set-process-sentinel
       (get-buffer-process helm-buffer)
       (lambda (process event)
         (let* ((err (process-exit-status process))
                (noresult (= err 1)))
           (cond (noresult
                  (with-helm-buffer
                    (unless (cdr helm-sources)
                      (insert (concat "* Exit with code 1, no result found,"
                                      " command line was:\n\n "
                                      cmd)))))
                 ((string= event "finished\n")
                  (when (and helm-locate-fuzzy-match
                             (not (string-match-p "\\s-" helm-pattern)))
                    (helm-redisplay-buffer))
                  (helm-locate-update-mode-line "Locate"))
                 (t
                  (helm-log "helm-locat-init" "Error: Locate %s"
                            (replace-regexp-in-string "\n" "" event))))))))))

(defun helm-locate-default-fuzzy-sort-fn (candidates)
  "Default sort function for files in fuzzy matching.
Sort is done on basename of CANDIDATES."
  (helm-fuzzy-matching-default-sort-fn-1 candidates nil t))

(defclass helm-locate-override-inheritor (helm-type-file) ())

(defclass helm-locate-source (helm-source-async helm-locate-override-inheritor)
  ((init :initform 'helm-locate-initial-setup)
   (candidates-process :initform 'helm-locate-init)
   (requires-pattern :initform 3)
   (history :initform 'helm-file-name-history)
   (persistent-action :initform 'helm-ff-kill-or-find-buffer-fname)
   (candidate-number-limit :initform 9999)
   (redisplay :initform (progn helm-locate-fuzzy-sort-fn))))

;; Override helm-type-file class keymap.
(cl-defmethod helm--setup-source :after ((source helm-locate-override-inheritor))
  (setf (slot-value source 'keymap) helm-locate-map)
  (setf (slot-value source 'group) 'helm-locate))

(defvar helm-source-locate
  (helm-make-source "Locate" 'helm-locate-source
    :pattern-transformer 'helm-locate-pattern-transformer
    ;; :match-part is only used here to tell helm which part
    ;; of candidate to highlight.
    :match-part (lambda (candidate)
                  (if (or (string-match-p " -b\\'" helm-pattern)
                          (and helm-locate-fuzzy-match
                               (not (string-match "\\s-" helm-pattern))))
                      (helm-basename candidate)
                      candidate))))

(defun helm-locate-pattern-transformer (pattern)
  (if helm-locate-fuzzy-match
      ;; When fuzzy is enabled helm add "-b" option on startup.
      (cond ((string-match-p " " pattern)
             (when (string-match "\\`locate -b" helm-locate-command)
               (setq helm-locate-command
                     (replace-match "locate" t t helm-locate-command)))
             pattern)
            (t
             (unless (string-match-p "\\`locate -b" helm-locate-command)
               (setq helm-locate-command
                     (replace-regexp-in-string
                      "\\`locate" "locate -b" helm-locate-command)))
             (helm--mapconcat-pattern pattern)))
      pattern))

(defun helm-locate-find-dbs-in-projects (&optional update)
  (let* ((pfn (lambda (candidate directory)
                (unless (= (shell-command
                            (funcall helm-locate-create-db-function
                                     candidate
                                     directory))
                           0)
                  (error "Failed to create locatedb file `%s'" candidate)))))
    (cl-loop for p in helm-locate-project-list
             for db = (expand-file-name
                       helm-ff-locate-db-filename
                       (file-name-as-directory p))
             if (and (null update) (file-exists-p db))
             collect db
             else do (funcall pfn db p)
             and collect db)))

;;; Directory completion for hff.
;;
(defclass helm-locate-subdirs-source (helm-source-in-buffer)
  ((basedir :initarg :basedir
            :initform nil
            :custom string)
   (subdir :initarg :subdir
           :initform nil
           :custom 'string)
   (data :initform #'helm-locate-init-subdirs)
   (group :initform 'helm-locate)))

(defun helm-locate-init-subdirs ()
  (with-temp-buffer
    (call-process-shell-command
     (if (string-match-p "\\`fd" helm-locate-recursive-dirs-command)
         (format helm-locate-recursive-dirs-command
                 ;; fd pass path at end.
                 (helm-get-attr 'subdir) (helm-get-attr 'basedir))
       (format helm-locate-recursive-dirs-command
	       (if (string-match-p "\\`es" helm-locate-recursive-dirs-command)
                   ;; Fix W32 paths.
		   (replace-regexp-in-string
                    "/" "\\\\\\\\" (helm-get-attr 'basedir))
                 (helm-get-attr 'basedir))
	       (helm-get-attr 'subdir)))
     nil t nil)
    (buffer-string)))

;;;###autoload
(defun helm-projects-find-files (update)
  "Find files with locate in `helm-locate-project-list'.
With a prefix arg refresh the database in each project."
  (interactive "P")
  (helm-locate-set-command)
  (cl-assert (and (string-match-p "\\`locate" helm-locate-command)
                  (executable-find "updatedb"))
             nil "Unsupported locate version")
  (let ((dbs (helm-locate-find-dbs-in-projects update)))
    (if dbs
        (helm-locate-with-db dbs)
        (user-error "No projects found, please setup `helm-locate-project-list'"))))

;;;###autoload
(defun helm-locate (arg)
  "Preconfigured `helm' for Locate.
Note: you can add locate options after entering pattern.
See \\='man locate' for valid options and also `helm-locate-command'.

You can specify a local database with prefix argument ARG.
With two prefix arg, refresh the current local db or create it if
it doesn't exists.

To create a user specific db, use
\"updatedb -l 0 -o db_path -U directory\".
Where db_path is a filename matched by
`helm-locate-db-file-regexp'."
  (interactive "P")
  (helm-set-local-variable 'helm-async-outer-limit-hook
                           (list (lambda ()
                                   (when (and helm-locate-fuzzy-match
                                              (not (string-match-p
                                                    "\\s-" helm-pattern)))
                                     (helm-redisplay-buffer)))))
  (setq helm-ff-default-directory default-directory)
  (helm-locate-1 arg nil nil (thing-at-point 'filename)))

(provide 'helm-locate)

;;; helm-locate.el ends here
