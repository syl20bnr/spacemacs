#!/usr/bin/emacs --script
;;
;;; run.el -- Spacemacs documentation export runner -*- lexical-binding: t -*-
;;
;; Copyright (C) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;; This file is not part of GNU Emacs.
;;
;; Note: see `spacemacs-export-docs-help-text' for usage.
;;
;;; License: GPLv3

(require 'json)
(require 'cl-lib)
(require 'subr-x)

(defconst spacemacs-export-docs-this-file-name
  (or load-file-name
      buffer-file-name)
  "`load-file-name' or `buffer-file-name'/")

(defconst spacemacs-export-docs-this-file-dir
  (file-name-directory spacemacs-export-docs-this-file-name)
  "`file-name-directory' of `spacemacs-export-docs-this-file-name'")

(load
 (expand-file-name
  "../lib/shared.el"
  spacemacs-export-docs-this-file-dir)
 nil t)

(defvar spacemacs--export-docs-default-exclude-re
  (regexp-opt
   (append
    (mapcar (lambda (el)
              (file-truename
               (concat
                "../../../"
                el)))
            `("export/"
              "private/"
              "tests/"
              "elpa/"))
    (file-expand-wildcards
     (file-truename (concat spacemacs--spacetools-root-dir
                            ".*")))
    '("LAYERS.org")))
  "Default regexp for ignoring ORG and static files.
It will be matched against full path of each exported file.")

(defconst spacemacs-export-docs-help-text
  `((:general . ("Spacemacs documentation exporter"
                 "================================"
                 "First argument should be either \"test\" or \"export\""
                 "Use \"test -h\" or \"export -h\" for more info"))
    (:test . ("Spacemacs documentation exporter (test mode)"
              "==============================================="
              "Arguments:"
              " - <test> - You already know this :)"
              " - list of files for testing"
              " Example: ./run.el test \"doc/FAQ.org\" \"CONTRIBUTING.org\""
              "NOTE: In this mode warnings are counted as errors"))
    (:export . ("Spacemacs documentation exporter (export mode)"
                "================================================="
                "Arguments:"
                " - <export> - You already know this :)"
                " - Target directory for export"
                "   (default: \"<TMP>/spacemacs--export/\")"
                " - Count of Emacs instances that will be used for exporting"
                "   (default \"6\", minimal vale is 1 and maximal"
                "    is total count of ORG files)"
                " - Regexp for ignoring ORG and static files,"
                "   exclude nothing if empty string"
                "   (It will be matched against full path of each exported file"
                ,(format "   Current default: %S)"
                         spacemacs--export-docs-default-exclude-re))))
  "Help text for `spacemacs//export-docs-maybe-show-help' .")

(defvar spacemacs--export-docs-configs-plist nil
  "Script configurations created with `spacemacs//export-docs-parse-args'.
The shape:(
:mode <export or test>
:target-directory <directory to output stuff>
:workers-count <how many workers will be used>
:exclude-re <file exclusion regular expression>
:files <filtered files list>
)
See `spacemacs-export-docs-help-text' for details.")

(defvar spacemacs--export-docs-workers-fin 0
 "Count of Emacs instances that finished exporting.")
(defvar spacemacs--export-docs-mode nil
  "Current mode")
(defvar spacemacs--export-docs-test-failed? nil
 "Will be set to t if exporting fails in the test mode.")
(defvar spacemacs--export-docs-stop-waiting nil
    "Used for blocking until all exporters have exited.")
(defvar spacemacs--export-docs-copy-queue '()
  "Queue of static dependencies to be copied to
the export dir.")

(defun spacemacs//export-docs-maybe-show-help (arg-list)
  "Show help message if arguments are invalid.
Returns t if help was shown."
  (message
   (cond
    ((or (not arg-list)
         (not (member (car arg-list)
                      '("test" "export"))))
     (mapconcat
      'identity
      (alist-get :general spacemacs-export-docs-help-text)
      "\n"))

    ;; test
    ((and (string= (car arg-list) "test")
          (or (< (length arg-list) 2)
              (string-match "-h" (or (cadr arg-list) ""))))
     (mapconcat
      'identity
      (alist-get :test spacemacs-export-docs-help-text)
      "\n"))

    ;; export
    ((and (string= (car arg-list) "export")
          (or (> (length arg-list) 3)
              (string-match "-h" (or (cadr arg-list) ""))))
     (mapconcat
      'identity
      (alist-get :export spacemacs-export-docs-help-text)
      "\n")))))

(defun spacemacs//export-docs-parse-args (arg-list)
  "Initialization. ARG-LIST is argument list.
The function will return a plist of configs or print
a help message if the list of arguments is invalid.
See `spacemacs-export-docs-help-text' for description."
  (when (and (spacemacs//export-docs-maybe-show-help arg-list)
             noninteractive)
   (kill-emacs 1))
  (let ((script-mode (pop arg-list)))
   (append
    `(:mode ,script-mode)
    (cond
     ;; Export mode.
     ((string= script-mode "export")
      (append
       `(:target-directory
         ,(or (pop arg-list)
              (concat temporary-file-directory
                      "spacemacs-export/"))
         :workers-count
         ,(max (or (pop arg-list) 6) 1))
       (let ((exclude-re
              (or (pop arg-list)
                  spacemacs--export-docs-default-exclude-re))
             (all-org-files (directory-files-recursively
                             spacemacs--spacetools-root-dir
                             "\\.org$")))
         `(:exclude-re
           ,exclude-re
           :files
           ,(if (or (not exclude-re) (string-empty-p exclude-re))
                all-org-files
              (cl-remove-if
               (lambda(e)(string-match-p exclude-re e))
               all-org-files))))))
     ;; Test mode.
     ((string= script-mode "test")
      `(:target-directory
        ,(file-name-as-directory (make-temp-file "spacemacs-test-export" t))
        :workers-count
        1
        :exclude-re
        ""
        :files
        ,(mapcar
          (lambda (path)
            (let ((abs-path
                   (file-truename
                    (if (file-name-absolute-p path)
                        path
                      (expand-file-name
                       path
                       spacemacs--spacetools-root-dir)))))
              (if (and (file-readable-p abs-path)
                       (not (file-directory-p abs-path))
                       (string-prefix-p
                        spacemacs--spacetools-root-dir
                        abs-path))
                  abs-path
                (error "%S is invalid file or outside %S"
                       path
                       spacemacs--spacetools-root-dir))))
          arg-list)))
     (t (error "Unknown script mode: %s"
               spacemacs--export-docs-mode))))))

(defun spacemacs//export-docs-copy-file-to-export-dir (path export-dir)
  "Copy file at PATH into corresponding PATH in
TO EXPORT-DIR unless PATH
matches :exclude-re from `spacemacs--export-docs-configs-plist'.
NOTE: PATH mast be absolute path."
  (let ((exclude-re (plist-get
                     spacemacs--export-docs-configs-plist
                     :exclude-re)))
   (if (and
        (not (string= exclude-re ""))
        (string-match-p exclude-re
                        path))
       (message "File %S was ignored (matched by the exclusion regexp)"
                path)
     (let ((new-path
            (concat
             export-dir
             (substring
              path
              (length spacemacs--spacetools-root-dir)))))
       (make-directory (file-name-directory new-path) t)
       (message "Copying file %S into %S"
                path
                new-path)
       (copy-file path new-path t)))))

(defun spacemacs//export-docs-concurrently-sentinel (p e)
  (condition-case err
      (let ((buff (process-buffer p)))
        (if (not (eq (process-status p) 'exit))
            (error "Process %s doesn't have status: exit" p)
          (spacemacs//export-docs-interpret-proc-output p buff)
          (kill-buffer buff))
        (if (string-match-p "finished" e)
            (progn
              (message "Process %s has finished\n" p)
              (when
                  (= (setq spacemacs--export-docs-workers-fin
                           (1+ spacemacs--export-docs-workers-fin))
                     (plist-get
                      spacemacs--export-docs-configs-plist
                      :workers-count))
                (setq spacemacs--export-docs-stop-waiting t)))
          (error "Process %s was %s"
                 p e)
          (setq spacemacs--export-docs-stop-waiting t)))
    (error (setq spacemacs--export-docs-stop-waiting t)
           (error "%s" err))))

(defun spacemacs//export-docs-interpret-proc-output (proc buff)
  "Parses process PROC BUFFER. Process P should be finished."
  (message "PROCESS: %S\n" proc)
  (dolist (line (split-string (with-current-buffer buff (buffer-string)) "\n"))
    (unless (or (string= line "")
                (string-match-p "^Loading.*\\.\\.\\.$" line))
      (let ((resp (ignore-errors (json-read-from-string line))))
        (unless resp
          (error "Malformed response:%s" line))
        (let ((type (alist-get 'type resp))
              (text (replace-regexp-in-string
                     "\r"
                     "\n"
                     (alist-get 'text resp)))
              (mode (plist-get
                     spacemacs--export-docs-configs-plist
                     :mode)))
          (cond
           ;; Export mode.
           ((string= mode "export")
            (message
             "%s"
             (cond
              ((string= type "message")
               text)
              ((string= type "warning")
               (concat "\n=============== WARNING ===============\n"
                       text
                       "\n=======================================\n"))
              ((string= type "error")
               (concat "\n!!!!!!!!!!!!!!!! ERROR !!!!!!!!!!!!!!!!\n"
                       text
                       "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"))
              ((string= type "export")
               (format
                (concat "File %S has static dependency %S\n"
                        "=> it will be copied into the export directory")
                (alist-get 'source resp)
                (progn
                  (push text spacemacs--export-docs-copy-queue)
                  text)))
              (t
               (error
                "%s"
                (concat "\n?????????? UNKNOWN EVENT TYPE ????????????\n"
                        (format "TYPE:\"%s\" TEXT: \"%s\"" type text)
                        "\n?????????????????????????????????????????\n"))))))
           ;; Test mode.
           ((string= mode "test")
            (when-let
                ((err
                  (cond
                   ((or (string= type "message")
                        (string= type "export"))
                    nil)
                   ((or (string= type "warning")
                        (string= type "error"))
                    (concat "\n!!!!!!!!!!!!!! TEST FAILED !!!!!!!!!!!!!!\n"
                            text
                            "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n"))
                   (t
                    (concat "\n!!!!!!! ERROR: UNKNOWN EVENT TYPE !!!!!!!\n"
                            (format "TYPE:\"%s\" TEXT: \"%s\"" type text)
                            "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")))))
              (message "%s" (setq spacemacs--export-docs-test-failed? err))))
           (t (error "Unknown script mode: %s" mode)))))))
  (while spacemacs--export-docs-copy-queue
    (spacemacs//export-docs-copy-file-to-export-dir
     (pop spacemacs--export-docs-copy-queue)
     (plist-get spacemacs--export-docs-configs-plist :target-directory))))

(defun spacemacs//export-docs-run (arg-list)
  "Main function for running as a script. ARG-LIST is an argument list.
See `spacemacs-export-docs-help-text' for description."
  (setq spacemacs--export-docs-test-failed? nil
        spacemacs--export-docs-workers-fin 0
        spacemacs--export-docs-stop-waiting nil)
  (let* ((mode (car arg-list))
         (default-directory spacemacs-export-docs-this-file-dir)
         (w-path
          (let ((worker-path (file-truename "_worker.elc")))
            (byte-compile-file "_worker.el")
            worker-path)))
    (let ((conf (setq spacemacs--export-docs-configs-plist
                      (spacemacs//export-docs-parse-args arg-list))))
      (unwind-protect
          (spacemacs//spacetools-do-concurrently
           (plist-get conf :files)
           (plist-get conf :workers-count)
           w-path
           (lambda (files)
             (format
              "%S"
              `(spacemacs/export-docs-to-edn
                ,(let ((dir (plist-get conf :target-directory)))
                   (make-directory dir t)
                   dir)
                ',files)))
           'spacemacs//export-docs-concurrently-sentinel)
        (when (string= mode "test")
          (delete-directory (plist-get conf :target-directory) t)))))
  (while (not spacemacs--export-docs-stop-waiting)
    (accept-process-output)))

(defun spacemacs/export-docs-do-test (&rest files)
  "Try exporting Spacemacs documentation .org FILES.
if error occurs return nil - otherwise t.
NOTE: See `spacemacs-export-docs-help-text' for more details."
  (spacemacs//export-docs-run (append '("test") files))
  (not spacemacs--export-docs-test-failed?))

(defun spacemacs/export-docs-do-export
    (&optional target-dir workers-count exclude-re files)
  "Export Spacemacs documentation files
into TARGET-DIR using WORKERS-COUNT worker threads and
filtering FILE list with EXCLUDE-RE matching
against full paths of the files.
NOTE: See `spacemacs-export-docs-help-text' for more details."
  (spacemacs//export-docs-run
   (list "export" target-dir workers-count exclude-re files)))

;; Script entry point.
(when (and load-file-name
           noninteractive)
  (spacemacs//export-docs-run argv)
  (when spacemacs--export-docs-test-failed?
   (error "Test failed.")))
