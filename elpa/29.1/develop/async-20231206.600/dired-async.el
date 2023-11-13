;;; dired-async.el --- Asynchronous dired actions -*- lexical-binding: t -*-

;; Copyright (C) 2012-2019 Free Software Foundation, Inc.

;; Authors: John Wiegley <jwiegley@gmail.com>
;;          Thierry Volpiatto <thievol@posteo.net>

;; Keywords: dired async network
;; X-URL: https://github.com/jwiegley/emacs-async

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provide a redefinition of `dired-create-file' function,
;; performs copies, moves and all what is handled by `dired-create-file'
;; in the background using a slave Emacs process,
;; by means of the async.el module.
;; To use it, put this in your .emacs:

;;     (dired-async-mode 1)

;; This will enable async copy/rename etc...
;; in dired and helm.

;;; Code:

(require 'cl-lib)
(require 'dired-aux)
(require 'async)

(eval-when-compile
  (defvar async-callback))

(defgroup dired-async nil
  "Copy rename files asynchronously from dired."
  :group 'dired)

(defcustom dired-async-env-variables-regexp
  "\\`\\(tramp-\\(default\\|connection\\|remote\\)\\|ange-ftp\\)-.*"
  "Variables matching this regexp will be loaded on Child Emacs."
  :type  'regexp)

(defcustom dired-async-message-function 'dired-async-mode-line-message
  "Function to use to notify result when operation finish.
Should take same args as `message'."
  :type  'function)

(defcustom dired-async-log-file "/tmp/dired-async.log"
  "File use to communicate errors from Child Emacs to host Emacs."
  :type 'string)

(defcustom dired-async-mode-lighter '(:eval
                                      (when (eq major-mode 'dired-mode)
                                        " Async"))
  "Mode line lighter used for `dired-async-mode'."
  :risky t
  :type 'sexp)

(defcustom dired-async-skip-fast nil
  "If non-nil, skip async for fast operations.
Same device renames and copying and renaming files smaller than
`dired-async-small-file-max' are considered fast."
  :risky t
  :type 'boolean)

(defcustom dired-async-small-file-max 5000000
  "Files smaller than this in bytes are considered fast to copy
or rename for `dired-async-skip-fast'."
  :risky t
  :type 'integer)

(defface dired-async-message
    '((t (:foreground "yellow")))
  "Face used for mode-line message.")

(defface dired-async-failures
    '((t (:foreground "red")))
  "Face used for mode-line message.")

(defface dired-async-mode-message
    '((t (:foreground "Gold")))
  "Face used for `dired-async--modeline-mode' lighter.")

(define-minor-mode dired-async--modeline-mode
    "Notify mode-line that an async process run."
  :global t
  :lighter (:eval (propertize (format " [%s Async job(s) running]"
                                      (length (dired-async-processes)))
                              'face 'dired-async-mode-message))
  (unless dired-async--modeline-mode
    (let ((visible-bell t)) (ding))))

(defun dired-async-mode-line-message (text face &rest args)
  "Notify end of operation in `mode-line'."
  (message nil)
  (let ((mode-line-format (concat
                           " " (propertize
                                (if args
                                    (apply #'format text args)
                                  text)
                                'face face))))
    (force-mode-line-update)
    (sit-for 3)
    (force-mode-line-update)))

(defun dired-async-processes ()
  (cl-loop for p in (process-list)
           when (process-get p 'dired-async-process)
           collect p))

(defun dired-async-kill-process ()
  (interactive)
  (let* ((processes (dired-async-processes))
         (proc (car (last processes))))
    (and proc (delete-process proc))
    (unless (> (length processes) 1)
      (dired-async--modeline-mode -1))))

(defun dired-async-after-file-create (total operation failures skipped)
  "Callback function used for operation handled by `dired-create-file'."
  (unless (dired-async-processes)
    ;; Turn off mode-line notification
    ;; only when last process end.
    (dired-async--modeline-mode -1))
  (when operation
    (if (file-exists-p dired-async-log-file)
        (progn
          (pop-to-buffer (get-buffer-create dired-log-buffer))
          (goto-char (point-max))
          (setq inhibit-read-only t)
          (insert "Error: ")
          (insert-file-contents dired-async-log-file)
          (special-mode)
          (shrink-window-if-larger-than-buffer)
          (delete-file dired-async-log-file))
      (run-with-timer
       0.1 nil
       (lambda ()
         ;; First send error messages.
         (cond (failures
                (funcall dired-async-message-function
                         "%s failed for %d of %d file%s -- See *Dired log* buffer"
                         'dired-async-failures
                         (car operation) (length failures)
                         total (dired-plural-s total)))
               (skipped
                (funcall dired-async-message-function
                         "%s: %d of %d file%s skipped -- See *Dired log* buffer"
                         'dired-async-failures
                         (car operation) (length skipped) total
                         (dired-plural-s total))))
         (when dired-buffers
           (cl-loop for (_f . b) in dired-buffers
                    when (buffer-live-p b)
                    do (with-current-buffer b
                         (when (and (not (file-remote-p default-directory nil t))
                                    (file-exists-p default-directory))
                           (revert-buffer nil t)))))
         ;; Finally send the success message.
         (funcall dired-async-message-function
                  "Asynchronous %s of %s on %s file%s done"
                  'dired-async-message
                  (car operation) (cadr operation)
                  total (dired-plural-s total)))))))

(defun dired-async-maybe-kill-ftp ()
  "Return a form to kill ftp process in child emacs."
  (quote
   (progn
     (require 'cl-lib)
     (let ((buf (cl-loop for b in (buffer-list)
                         thereis (and (string-match
                                       "\\`\\*ftp.*"
                                       (buffer-name b)) b))))
       (when buf (kill-buffer buf))))))

(defsubst dired-async--directory-p (attributes)
  "Return non-nil if ATTRIBUTES is for a directory.
See `file-attributes'."
  ;; Can also be a string for symlinks, so check for t explicitly.
  (eq (file-attribute-type attributes) t))

(defsubst dired-async--same-device-p (f1 f2)
  "Return non-nil if F1 and F2 have the same device number."
  ;; file-attribute-device-number may be a cons cell, so use equal for
  ;; testing (See Emacs bug/58446).
  (equal (file-attribute-device-number (file-attributes f1))
         (file-attribute-device-number (file-attributes f2))))

(defun dired-async--small-file-p (file)
  "Return non-nil if FILE is considered small.

File is considered small if it size is smaller than
`dired-async-small-file-max'."
  (let ((a (file-attributes file)))
    ;; Directories are always large since we can't easily figure out
    ;; their total size.
    (and (not (dired-async--directory-p a))
         (< (file-attribute-size a) dired-async-small-file-max))))

(defun dired-async--skip-async-p (file-creator file name-constructor)
  "Return non-nil if we should skip async for FILE.
See `dired-create-files' for FILE-CREATOR and NAME-CONSTRUCTOR."
  ;; Skip async for small files.
  (or (dired-async--small-file-p file)
      ;; Also skip async for same device renames.
      (and (eq file-creator 'dired-rename-file)
           (let ((new (funcall name-constructor file)))
             (dired-async--same-device-p file (file-name-directory new))))))

(defun dired-async--smart-create-files (old-func file-creator
                                        operation fn-list name-constructor
                                        &optional marker-char)
  "Around advice for `dired-create-files'.
Uses async like `dired-async-create-files' but skips certain fast
cases if `dired-async-skip-fast' is non-nil."
  (let (async-list quick-list)
    (if (or (eq file-creator 'backup-file)
            (null dired-async-skip-fast))
        (setq async-list fn-list)
      (dolist (old fn-list)
        (if (dired-async--skip-async-p file-creator old name-constructor)
            (push old quick-list)
          (push old async-list))))
    (when async-list
      (dired-async-create-files
       file-creator operation (nreverse async-list)
       name-constructor marker-char))
    (when quick-list
      (funcall old-func file-creator operation
               (nreverse quick-list) name-constructor marker-char))))

(defun dired-async--abort-if-file-too-large (size op-type filename)
  "If file SIZE larger than `large-file-warning-threshold', allow user to abort.
Same as `abort-if-file-too-large' but without user-error."
  (when (and large-file-warning-threshold size
	     (> size large-file-warning-threshold))
    (files--ask-user-about-large-file
     size op-type filename nil)))

(defvar overwrite-query)
(defun dired-async-create-files (file-creator operation fn-list name-constructor
                                              &optional _marker-char)
  "Same as `dired-create-files' but asynchronous.

See `dired-create-files' for the behavior of arguments."
  (setq overwrite-query nil)
  (let ((total (length fn-list))
        failures async-fn-list skipped callback
        async-quiet-switch create-dir)
    (let (to)
      (dolist (from fn-list)
        (setq to (funcall name-constructor from))
        (if (and (equal to from)
                 (null (eq file-creator 'backup-file)))
            (progn
              (setq to nil)
              (dired-log "Cannot %s to same file: %s\n"
                         (downcase operation) from)))
        (if (not to)
            (setq skipped (cons (dired-make-relative from) skipped))
          (let* ((overwrite (and (null (eq file-creator 'backup-file))
                                 (file-exists-p to)))
                 (dired-overwrite-confirmed ; for dired-handle-overwrite
                  (and overwrite
                       (let ((help-form `(format "\
Type SPC or `y' to overwrite file `%s',
DEL or `n' to skip to next,
ESC or `q' to not overwrite any of the remaining files,
`!' to overwrite all remaining files with no more questions." ,to)))
                         (dired-query 'overwrite-query "Overwrite `%s'?" to)))))
            ;; Handle the `dired-copy-file' file-creator specially
            ;; When copying a directory to another directory or
            ;; possibly to itself or one of its subdirectories.
            ;; e.g "~/foo/" => "~/test/"
            ;; or "~/foo/" =>"~/foo/"
            ;; or "~/foo/ => ~/foo/bar/")
            ;; In this case the 'name-constructor' have set the destination
            ;; TO to "~/test/foo" because the old emacs23 behavior
            ;; of `copy-directory' was to not create the subdirectory
            ;; and instead copy the contents.
            ;; With the new behavior of `copy-directory'
            ;; (similar to the `cp' shell command) we don't
            ;; need such a construction of the target directory,
            ;; so modify the destination TO to "~/test/" instead of "~/test/foo/".
            (let ((destname (file-name-directory to)))
              (when (and (file-directory-p from)
                         (file-directory-p to)
                         (eq file-creator 'dired-copy-file))
                (setq to destname))
              ;; If DESTNAME is a subdirectory of FROM, not a symlink,
              ;; and the method in use is copying, signal an error.
              (and (eq t (car (file-attributes destname)))
                   (eq file-creator 'dired-copy-file)
                   (file-in-directory-p destname from)
                   (error "Cannot copy `%s' into its subdirectory `%s'"
                          from to)))
            ;; Skip file if it is too large.
            (if (and (member operation '("Copy" "Rename"))
                     (eq (dired-async--abort-if-file-too-large
                          (file-attribute-size
                           (file-attributes (file-truename from)))
                          (downcase operation) from)
                         'abort))
                (push from skipped)
              (if overwrite
                  (or (and dired-overwrite-confirmed
                           (push (cons from to) async-fn-list))
                      (progn
                        (push (dired-make-relative from) failures)
                        (dired-log "%s `%s' to `%s' failed\n"
                                   operation from to)))
                (push (cons from to) async-fn-list))))))
      ;; Fix tramp issue #80 with emacs-26, use "-q" only when needed.
      (setq async-quiet-switch
            (if (and (boundp 'tramp-cache-read-persistent-data)
                     async-fn-list
                     (cl-loop for (_from . to) in async-fn-list
                              thereis (file-remote-p to)))
                "-q" "-Q"))
      ;; When failures have been printed to dired log add the date at bob.
      (when (or failures skipped) (dired-log t))
      ;; When async-fn-list is empty that's mean only one file
      ;; had to be copied and user finally answer NO.
      ;; In this case async process will never start and callback
      ;; will have no chance to run, so notify failures here.
      (unless async-fn-list
        (cond (failures
               (funcall dired-async-message-function
                        "%s failed for %d of %d file%s -- See *Dired log* buffer"
                        'dired-async-failures
                        operation (length failures)
                        total (dired-plural-s total)))
              (skipped
               (funcall dired-async-message-function
                        "%s: %d of %d file%s skipped -- See *Dired log* buffer"
                        'dired-async-failures
                        operation (length skipped) total
                        (dired-plural-s total)))))
      ;; Setup callback.
      (setq callback
            (lambda (&optional _ignore)
              (dired-async-after-file-create
               total (list operation (length async-fn-list)) failures skipped)
              (when (string= (downcase operation) "rename")
                (cl-loop for (file . to) in async-fn-list
                         for bf = (get-file-buffer file)
                         for destp = (file-exists-p to)
                         do (and bf destp
                                 (with-current-buffer bf
                                   (set-visited-file-name to t t)))))))
      (let ((dirp (file-directory-p to))
            (dest (file-name-directory to)))
        (when (boundp 'dired-create-destination-dirs)
          (setq create-dir
                (cl-case dired-create-destination-dirs
                  (always 'always)
                  (ask (and (null dirp)
                            (null (file-directory-p dest))
                            (y-or-n-p (format "Create directory `%s'? " dest)))
                       'always))))))
    ;; Start async process.
    (when async-fn-list
      (process-put
       (async-start `(lambda ()
                       (require 'cl-lib) (require 'dired-aux) (require 'dired-x)
                       ,(async-inject-variables dired-async-env-variables-regexp)
                       (advice-add #'files--ask-user-about-large-file
                                   :override (lambda (&rest args) nil))
                       (let ((dired-recursive-copies (quote always))
                             (dired-copy-preserve-time
                              ,dired-copy-preserve-time)
                             (dired-create-destination-dirs ',create-dir)
                             auth-source-save-behavior)
                         (setq overwrite-backup-query nil)
                         ;; Inline `backup-file' as long as it is not
                         ;; available in emacs.
                         (defalias 'backup-file
                           ;; Same feature as "cp -f --backup=numbered from to"
                           ;; Symlinks are copied as file from source unlike
                           ;; `dired-copy-file' which is same as cp -d.
                           ;; Directories are omitted.
                           (lambda (from to ok)
                             (cond ((file-directory-p from) (ignore))
                                   (t (let ((count 0))
                                        (while (let ((attrs (file-attributes to)))
                                                 (and attrs (null (nth 0 attrs))))
                                          (cl-incf count)
                                          (setq to (concat (file-name-sans-versions to)
                                                           (format ".~%s~" count)))))
                                      (condition-case err
                                          (copy-file from to ok dired-copy-preserve-time)
                                        (file-date-error
                                         (dired-log "Can't set date on %s:\n%s\n" from err)))))))
                         ;; Now run the FILE-CREATOR function on files.
                         (cl-loop with fn = (quote ,file-creator)
                                  for (from . dest) in (quote ,async-fn-list)
                                  do (condition-case err
                                         (funcall fn from dest t)
                                       (file-error
                                        (dired-log "%s: %s\n" (car err) (cdr err))
                                        nil)))
                         (when (get-buffer dired-log-buffer)
                           (dired-log t)
                           (with-current-buffer dired-log-buffer
                             (write-region (point-min) (point-max)
                                           ,dired-async-log-file))))
                       ,(dired-async-maybe-kill-ftp))
                    callback)
       'dired-async-process t)
      ;; Run mode-line notifications while process running.
      (dired-async--modeline-mode 1)
      (message "%s proceeding asynchronously..." operation))))

(defvar wdired-use-interactive-rename)
(defun dired-async-wdired-do-renames (old-fn &rest args)
  ;; Perhaps a better fix would be to ask for renaming BEFORE starting
  ;; OLD-FN when `wdired-use-interactive-rename' is non-nil.  For now
  ;; just bind it to nil to ensure no questions will be asked between
  ;; each rename.
  (let (wdired-use-interactive-rename)
    (apply old-fn args)))

;;;###autoload
(define-minor-mode dired-async-mode
  "Do dired actions asynchronously."
  :lighter dired-async-mode-lighter
  :global t
  (if dired-async-mode
      (progn
        (advice-add 'dired-create-files :around #'dired-async--smart-create-files)
        (advice-add 'wdired-do-renames :around #'dired-async-wdired-do-renames))
    (progn
      (advice-remove 'dired-create-files #'dired-async--smart-create-files)
      (advice-remove 'wdired-do-renames #'dired-async-wdired-do-renames))))

(defmacro dired-async--with-async-create-files (&rest body)
  "Evaluate BODY with ‘dired-create-files’ set to ‘dired-async-create-files’."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'dired-create-files) #'dired-async-create-files))
     ,@body))

;;;###autoload
(defun dired-async-do-copy (&optional arg)
  "Run ‘dired-do-copy’ asynchronously."
  (interactive "P")
  (dired-async--with-async-create-files
   (dired-do-copy arg)))

;;;###autoload
(defun dired-async-do-symlink (&optional arg)
  "Run ‘dired-do-symlink’ asynchronously."
  (interactive "P")
  (dired-async--with-async-create-files
   (dired-do-symlink arg)))

;;;###autoload
(defun dired-async-do-hardlink (&optional arg)
  "Run ‘dired-do-hardlink’ asynchronously."
  (interactive "P")
  (dired-async--with-async-create-files
   (dired-do-hardlink arg)))

;;;###autoload
(defun dired-async-do-rename (&optional arg)
  "Run ‘dired-do-rename’ asynchronously."
  (interactive "P")
  (dired-async--with-async-create-files
   (dired-do-rename arg)))

(provide 'dired-async)

;;; dired-async.el ends here
