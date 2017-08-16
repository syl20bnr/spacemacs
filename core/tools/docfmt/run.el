#!/usr/bin/emacs --script
;;
;;; run.el -- Spacemacs docs formatter runner -*- lexical-binding: t -*-
;;
;; Copyright (C) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;; This file is not part of GNU Emacs.
;;
;; Note: see `spacemacs-docfmt-help-text' for usage.
;;
;;; License: GPLv3

(defconst spacemacs-docfmt-help-text
 (concat
  "Spacemacs documentation formatting tool\n"
  "=======================================\n"
  "Arguments:\n"
  " - Number of Emacs processes to use.\n"
  "6 if the script is run without arguments.\n"
  "The value will be clamped up to the number of files. 0 is error.\n"
  "If actual value (after clamping) is 1 then the current Emacs process\n"
  "will be used for formatting.\n"
  " - Rest arguments are org file paths.\n"
  "They can be absolute or relative to the Spacemacs root directory.\n"
  "Format all Spacemacs org files if omitted.")
 "Help text for the script.")

(defconst spacemacs-docfmt-this-file-name
  (or load-file-name
      buffer-file-name)
  "`load-file-name' or `buffer-file-name'/")

(defconst spacemacs-docfmt-this-file-dir
  (file-name-directory spacemacs-docfmt-this-file-name)
  "`file-name-directory' of `spacemacs-docfmt-this-file-name'")

(load
 (expand-file-name
  "../lib/shared.el"
  spacemacs-docfmt-this-file-dir)
 nil t)

(byte-compile-file
 (expand-file-name
  "../lib/toc-org.el"
  spacemacs-docfmt-this-file-dir))

;; NOTE: We load it this way instead directly from `byte-compile-file'
;; because we don't wan to see "Loading.." message :)
(load
 (expand-file-name
  "../lib/toc-org.elc"
  spacemacs-docfmt-this-file-dir)
 nil t)


(defvar spacemacs--docfmt-workers-count nil
  "Number of Emacs instances that will be used for formatting.")
(defvar spacemacs--docfmt-workers-fin 0
  "Number of workers finished")
(defvar spacemacs--docfmt-stop-waiting nil
  "Used for blocking until all formatters have exited.")
(defvar spacemacs--docfmt-worker-path
  (let* ((default-directory spacemacs-docfmt-this-file-dir)
         (worker-path (file-truename "_worker.elc")))
    (byte-compile-file "_worker.el")
    worker-path)
  "Path to worker script")

(load spacemacs--docfmt-worker-path nil t)
(declare-function spacemacs/docfmt-apply-all "_worker.el")

(defun spacemacs/docfmt-format ()
  "Format current `org-mode' buffer."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "org-mode major mode should be enabled in the file."))
  (spacemacs/docfmt-apply-all))

(defun spacemacs//docfmt-concurrently-sentinel (p e)
  (condition-case err
      (let ((buff (process-buffer p)))
        (if (not (eq (process-status p) 'exit))
            (error "Process %s doesn't have status: exit" p)
          (dolist (line (split-string (with-current-buffer buff (buffer-string))
                                      "\n"))
            (unless (or (string= line "")
                        (string-match-p "^Loading.*\\.\\.\\.$" line))
             (message "Process \"%s\" says:" p)
             (message line)))
          (kill-buffer buff))
        (if (string-match-p "finished" e)
            (progn
              (message "Process \"%s\" has finished\n" p)
              (when
                  (= (setq spacemacs--docfmt-workers-fin
                           (1+ spacemacs--docfmt-workers-fin))
                     spacemacs--docfmt-workers-count)
                (setq spacemacs--docfmt-stop-waiting t)))
          (error "Process %s was %s"
                 p e)
          (setq spacemacs--docfmt-stop-waiting t)))
    (error (setq spacemacs--docfmt-stop-waiting t)
           (error "%s" err))))

(defun spacemacs//process-files-arg (files)
  "Make sure that we have a valid file list or
use default value if it's empty.
See `spacemacs-docfmt-help-text' for more info."
  (let (res)
    (push
     (mapcar
      (lambda (file-path)
        (let* ((default-directory spacemacs--spacetools-root-dir)
               (fp (file-truename file-path)))
          (unless (and (file-exists-p fp)
                       (file-writable-p fp)
                       (not (file-directory-p fp)))
            (error "File: \"%s\" isn't writable/readable or a dir."
                   fp))
          fp))
      (or files
          (directory-files-recursively
           spacemacs--spacetools-root-dir
           "\\.org$")))
     res)
    (car res)))

(defun spacemacs/docfmt-run (arg-list)
  "Main function for running as a script. ARG-LIST is an argument list
where the fist element is the number of emacs process that will be used and
the rest elements are file paths (absolute or relative to Spacemacs root dir)."
  (setq spacemacs--docfmt-workers-fin 0
        spacemacs--docfmt-stop-waiting nil)
  (let* ((default-directory spacemacs-docfmt-this-file-dir)
         (first-arg (pop arg-list))
         (files (spacemacs//process-files-arg arg-list))
         (f-length (length files))
         (w-count (min
                   f-length
                   (if first-arg
                       (let ((maybe-w-count
                              (string-to-number first-arg)))
                         (if (> maybe-w-count 0)
                             maybe-w-count
                           (error spacemacs-docfmt-help-text)))
                     6))))
    (unless (and w-count
                 files)
      (error spacemacs-docfmt-help-text))
    (if (= w-count 1)
        (spacemacs/docfmt-apply-all-batch files)
      (spacemacs//spacetools-do-concurrently
       files
       (setq spacemacs--docfmt-workers-count w-count)
       spacemacs--docfmt-worker-path
       (lambda (fps) (format "%S" `(spacemacs/docfmt-apply-all-batch ',fps)))
       'spacemacs//docfmt-concurrently-sentinel)
      (while (not spacemacs--docfmt-stop-waiting)
        (accept-process-output)))))

;; Script entry point.
(when (and load-file-name
           noninteractive)
  (spacemacs/docfmt-run argv))
