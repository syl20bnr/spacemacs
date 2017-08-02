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

(defvar spacemacs-export-docs-this-file-name
  (or load-file-name
      buffer-file-name)
  "`load-file-name' or `buffer-file-name'/")

(defvar spacemacs-export-docs-spacemacs-root-dir
  (file-truename
   (concat
    (file-name-directory
     spacemacs-export-docs-this-file-name)
    "../../../"))
  "Root directory of Spacemacs")

(defvar spacemacs-export-docs-default-exclude-re
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
     (file-truename (concat spacemacs-export-docs-spacemacs-root-dir
                            ".*")))
    '("LAYERS.org")))
  "Default regexp for ignoring ORG and static files.
It will be matched against full path of each exported file.")

(defvar spacemacs-export-docs-mode nil
  "Current mode")

(defvar spacemacs-export-docs-directory nil
  "Target directory for export.")

(defvar spacemacs-export-docs-workers-num nil
  "Number of Emacs instances that will be used for exporting.")

(defvar spacemacs-export-docs-exclude-re nil
  "Regexp for ignoring ORG and static files.
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
                "   (default: \"<TMP>/spacemacs-export/\")"
                " - Number of Emacs instances that will be used for exporting"
                "   (default \"6\", minimal vale is 1 and maximal"
                "    is total number of ORG files)"
                " - Regexp for ignoring ORG and static files,"
                "   exclude nothing if empty string"
                "   (It will be matched against full path of each exported file"
                ,(format "   Current default: %S)"
                         spacemacs-export-docs-default-exclude-re))))
  "Help text for `spacemacs//export-docs-maybe-show-help' .")

(defvar spacemacs--export-docs-copy-queue '())
(defvar spacemacs--export-docs-worker-fin 0)
(defvar spacemacs--export-docs-part-in nil)
(defvar spacemacs--export-docs-file-size-path-alist '())
(defvar spacemacs--export-docs-test-failed? '())

(defun spacemacs//export-docs-maybe-show-help (arg-list)
  "Show help message if arguments are invalid.
Returns t if help was shown."
  (cond
   ((or (not arg-list)
        (not (member (car arg-list)
                     '("test" "export"))))
    (message
     (mapconcat
      'identity
      (alist-get :general spacemacs-export-docs-help-text)
      "\n")))

   ;; test
   ((and (string= (car arg-list) "test")
         (or (< (length arg-list) 2)
             (string-match "-h" (or (cadr arg-list) ""))))
    (message
     (mapconcat
      'identity
      (alist-get :test spacemacs-export-docs-help-text)
      "\n")))

   ;; export
   ((and (string= (car arg-list) "export")
         (or (> (length arg-list) 3)
             (string-match "-h" (or (cadr arg-list) ""))))
    (message
     (mapconcat
      'identity
      (alist-get :export spacemacs-export-docs-help-text)
      "\n")))))

(defun spacemacs//export-docs-make-file-size-path-alist ()
  "Finds org files in `spacemacs-export-docs-spacemacs-root-dir',
filers this with  `spacemacs-export-docs-exclude-re' and
returns (file-size . file-path) alist."
  (let (res)
   (dolist (org-file-fp
            (directory-files-recursively
             spacemacs-export-docs-spacemacs-root-dir
             "\\.org$"))
     (unless (and
              (not (string=
                     spacemacs-export-docs-exclude-re
                     ""))
              (string-match-p spacemacs-export-docs-exclude-re
                              org-file-fp))
       (push `(,(float
                 (nth 7 (file-attributes org-file-fp))) . ,org-file-fp)
             res)))
   res))

(defun spacemacs/export-docs-parse-cmd-args (arg-list)
  "Initialization. ARG-LIST is argument list.
  see `spacemacs-export-docs-help-text' for description."
  (when (and (spacemacs//export-docs-maybe-show-help arg-list)
             noninteractive)
   (kill-emacs 1))
  (setq spacemacs-export-docs-mode (pop arg-list))
  (cond
   ;; Export mode.
   ((string= spacemacs-export-docs-mode "export")
    (setq
     spacemacs-export-docs-directory
     (or (pop arg-list)
         (concat temporary-file-directory
                 "spacemacs-export/"))
     spacemacs-export-docs-workers-num
     (max (or (pop arg-list) 6) 1)
     spacemacs-export-docs-exclude-re
     (or (pop arg-list)
         spacemacs-export-docs-default-exclude-re)
     spacemacs--export-docs-file-size-path-alist
     (spacemacs//export-docs-make-file-size-path-alist)))
   ;; Test mode.
   ((string= spacemacs-export-docs-mode "test")
    (setq
     spacemacs-export-docs-directory
     (file-name-as-directory (make-temp-file "spacemacs-test-export" t))
     spacemacs-export-docs-workers-num 1
     spacemacs-export-docs-exclude-re ""
     spacemacs--export-docs-file-size-path-alist
     (mapcar
      (lambda (path)
        (let ((abs-path
               (file-truename
                (if (file-name-absolute-p path)
                    path
                  (expand-file-name
                   path
                   spacemacs-export-docs-spacemacs-root-dir)))))
          (if (and (file-readable-p abs-path)
                   (not (file-directory-p abs-path))
                   (string-prefix-p
                    spacemacs-export-docs-spacemacs-root-dir
                    abs-path))
              `(,(float
                  (nth 7 (file-attributes abs-path))) . ,abs-path)
            (error "%S is invalid file or outside %S"
                   path
                   spacemacs-export-docs-spacemacs-root-dir))))
      arg-list)))
   (t (error "Unknown script mode: %s"
             spacemacs-export-docs-mode))))

(defun spacemacs//export-docs-copy-file-to-export-dir (path export-dir)
  "Copy file at PATH into corresponding PATH in
TO EXPORT-DIR unless PATH
matches `spacemacs-export-docs-exclude-re'.
NOTE: PATH mast be absolute path."
  (if (and
       (not (string= spacemacs-export-docs-exclude-re ""))
       (string-match-p spacemacs-export-docs-exclude-re
                       path))
      (message "File %S was ignored (matched by the exclusion regexp)"
               path)
    (let ((new-path
           (concat
            export-dir
            (substring
             path
             (length spacemacs-export-docs-spacemacs-root-dir)))))
      (make-directory (file-name-directory new-path) t)
      (message "Copying file %S into %S"
               path
               new-path)
      (copy-file path new-path t))))

(defun spacemacs//org-edn-f-alist-to-buckets (f-sp-alist n)
  "Split F-SP-ALIST - alist of (<file size> . <file path>) into
N  alists balancing by file sizes.
NOTE: N should be less than the alist's length."
  (let ((fps-alist (sort
                    f-sp-alist
                    (lambda (e1 e2) (> (car e1) (car e2)))))
        ;; Try it :)
        ;; (buckets (make-list n '(0 . nil))))
        (buckets '()))
    ;;  ^^^^^^^^^^^^^^
    (dotimes (_ n) (push (cl-copy-list '(0)) buckets))
    (dolist (fps fps-alist)
      (setf buckets (sort
                     buckets
                     (lambda (e1 e2) (< (car e1) (car e2))))
            (car buckets)
            (cons (+ (caar buckets) (car fps))
                  (push (cdr fps) (cdar buckets)))))
    (mapcar 'cdr buckets)))
(byte-compile 'spacemacs//org-edn-f-alist-to-buckets)

(defun spacemacs//export-docs-concurrently-sentinel (p e)
  (let ((buff (process-buffer p)))
    (when (eq (process-status p) 'exit)
      (spacemacs//export-docs-interpret-proc-output p buff)
      (kill-buffer buff)))
  (if (string-match-p "finished" e)
      (progn
        (message "Process %s has finished\n" p)
        (setq spacemacs--export-docs-worker-fin
              (1+ spacemacs--export-docs-worker-fin)))
    (error "Process %s was %s"
           p e)
    ;; stop waiting
    (setq spacemacs--export-docs-part-in
          -1)))

(defun spacemacs//export-docs-interpret-proc-output (proc buff)
  "Parses process PROC BUFFER. Process P should be finished."
  (unless (eq (process-status proc) 'exit)
    (error "Process %s doesn't have status: exit" proc))
  (message "PROCESS: %S\n" proc)
  (dolist (line (split-string (with-current-buffer buff (buffer-string)) "\n"))
    (unless (or (string= line "")
                (string-match-p "^Loading.*\\.\\.\\.$" line))
      (message
       "%s"
       (or
        (let ((resp (ignore-errors (json-read-from-string line))))
         (when resp
          (let ((type (alist-get 'type resp))
                (text (replace-regexp-in-string
                       "\r"
                       "\n"
                       (alist-get 'text resp))))
            (cond
             ;; Export mode.
             ((string= spacemacs-export-docs-mode "export")
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
                (concat "\n?????????? UNKNOWN EVENT TYPE ????????????\n"
                        (format "TYPE:\"%s\" TEXT: \"%s\"" type text)
                        "\n?????????????????????????????????????????\n"))))
             ;; Test mode.
             ((string= spacemacs-export-docs-mode "test")
              (cond
               ((string= type "message")
                text)
               ((or (string= type "warning")
                    (string= type "error"))
                (error "%s"
                 (concat "\n!!!!!!!!!!!!!! TEST FAILED !!!!!!!!!!!!!!\n"
                         text
                         "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")))
               ((string= type "export")
                (format
                 "File %S has static dependency %S"
                 (alist-get 'source resp)
                 text))
               (t
                (error "%s"
                 (concat "\n!!!!!!! ERROR: UNKNOWN EVENT TYPE !!!!!!!\n"
                         (format "TYPE:\"%s\" TEXT: \"%s\"" type text)
                         "\n!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!\n")))))
             (t (error "Unknown script mode: %s"
                     spacemacs-export-docs-mode))))))
        (error "Malformed response:%s" line)))))
  (while spacemacs--export-docs-copy-queue
    (spacemacs//export-docs-copy-file-to-export-dir
     (pop spacemacs--export-docs-copy-queue)
     spacemacs-export-docs-directory)))

(defun spacemacs//export-docs-concurrently ()
  "Export Spacemacs documentation files and using `spacemacs-edn' backend."
  (setq spacemacs--export-docs-part-in spacemacs-export-docs-workers-num
        spacemacs--export-docs-worker-fin 0)
  (let ((exp-dir spacemacs-export-docs-directory)
        (org-file-buckets '())
        (emacs-fp (executable-find "emacs"))
        (worker-fp (let ((load-file-dir
                          (file-name-directory
                           spacemacs-export-docs-this-file-name)))
                     (byte-compile-file
                      (concat
                       load-file-dir
                       "_worker.el"))
                     (concat
                      load-file-dir
                      "_worker.elc")))
        (toc-org-fp (concat
                     (file-name-directory
                      spacemacs-export-docs-this-file-name)
                     "../lib/toc-org.el")))
    (unless emacs-fp
      (error "Can't find emacs executable"))
    (setq org-file-buckets
          (spacemacs//org-edn-f-alist-to-buckets
           spacemacs--export-docs-file-size-path-alist
           (min spacemacs--export-docs-part-in
                (length spacemacs--export-docs-file-size-path-alist))))
    (if toc-org-fp
        (byte-compile-file toc-org-fp)
      (error "toc-org.el should be present at:%S"
             toc-org-fp))
    (make-directory exp-dir t)
    (dolist (file-path-group org-file-buckets)
      (make-process
       :name
       "worker"
       :sentinel
       'spacemacs//export-docs-concurrently-sentinel
       :buffer (generate-new-buffer "worker-buffer")
       :command
       (list
        emacs-fp
        "-l"
        worker-fp
        "--batch"
        "-eval"
        (format
         "%S"
         `(spacemacs//export-docs-to-edn
           ,exp-dir
           ',file-path-group))))))
  (while (> spacemacs-export-docs-workers-num
            spacemacs--export-docs-worker-fin)
    (accept-process-output)))

(defun spacemacs//export-docs-run (arg-list)
  "Main function for running as a script. ARG-LIST is an argument list.
See `spacemacs-export-docs-help-text' for description."
  (let ((mode (car arg-list)))
    (unwind-protect
        (progn
          (spacemacs/export-docs-parse-cmd-args arg-list)
          (spacemacs//export-docs-concurrently))
      (when (string= mode "test")
        (delete-directory spacemacs-export-docs-directory t)))))

(defun spacemacs/export-docs-do-test (&rest files)
  "Try exporting Spacemacs documentation .org FILES.
If error occurs return nil - otherwise t.
NOTE: If files nil - return t."
  (not
   (let (test-feiled?)
     (condition-case _
         (spacemacs//export-docs-run (append '("test") files))
       (error (setq test-feiled? t))))))

(defun spacemacs/export-docs-do-export
    (&optional target-dir num-workers exclude-re)
  "Export Spacemacs documentation files
into TARGET-DIR using NUM-WORKERS threads and
filtering the file list with EXCLUDE-RE matching
against full paths of the files.
NOTE: See `spacemacs-export-docs-help-text' for more details."
  (setq
   spacemacs-export-docs-mode
   "export"
   spacemacs-export-docs-directory
   (or target-dir
       (concat temporary-file-directory
               "spacemacs-export/"))
   spacemacs-export-docs-workers-num
   (max (or num-workers 6) 1)
   spacemacs-export-docs-exclude-re
   (or exclude-re
       spacemacs-export-docs-default-exclude-re)
   spacemacs--export-docs-file-size-path-alist
   (spacemacs//export-docs-make-file-size-path-alist))
  (spacemacs//export-docs-concurrently))

;; Script entry point.
(when (and load-file-name
           noninteractive)
  (spacemacs//export-docs-run argv))
