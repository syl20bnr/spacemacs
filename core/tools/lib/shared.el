;;; shader.el -- shader code of Spacemacs tools -*- lexical-binding: t -*-
;;
;; Copyright (C) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst spacemacs--spacetools-root-dir
  (file-truename
   (concat
    (file-name-directory
     (or load-file-name (buffer-file-name)))
    "../../../"))
  "Root directory of Spacemacs")

(defun spacemacs//spacetools-make-file-size-path-alist (files)
  "Return (<file size> . <abs file path>) alist of FILES.
File paths can be absolute or relative to `spacemacs--spacetools-root-dir'."
  (let ((res nil)
        (default-directory spacemacs--spacetools-root-dir))
    (dolist (file files)
      (unless (and (file-writable-p file)
                   (not (file-directory-p file)))
        (error "File \"%s\" unwritable or directory"
               file))
      (push (cons (float (nth 7 (file-attributes file)))
                  (expand-file-name
                   file
                   spacemacs--spacetools-root-dir))
            res))
    res))

(defun spacemacs//spacetools-files-to-buckets (files n)
  "Split FILES into N lists(task buckets) balancing by file sizes."
  (let ((fps-alist (sort
                    (spacemacs//spacetools-make-file-size-path-alist
                     files)
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
(byte-compile 'spacemacs//spacetools-filse-to-buckets)

(defun spacemacs//spacetools-do-concurrently
    (files w-count w-path make-task sentinel)
  "Process FILES using W-COUNT workers(child emacs processes) loaded from W-PATH
MAKE-TASK is a function that takes single argument (file list) and returns
string representation of a task that each worker will perform - it will be
called as \"emacs -l W-PATH --batch -eval <value returned from MAKE-TASK>\"/.
SENTINEL is a worker process sentinel."
  (let ((file-buckets '())
        (emacs-fp (executable-find "emacs"))
        (toc-org-fp (concat
                     spacemacs--spacetools-root-dir
                     "core/tools/lib/toc-org.el")))
    (unless emacs-fp
      (error "Can't find emacs executable"))
    (setq file-buckets
          (spacemacs//spacetools-files-to-buckets
           files
           (min w-count
                (length files))))
    (byte-compile-file toc-org-fp)
    (dolist (file-path-bucket file-buckets)
      (make-process
       :name
       "worker"
       :sentinel
       sentinel
       :buffer
       (generate-new-buffer "workers")
       :command
       (list emacs-fp
             "-Q"
             "-l" w-path
             "--batch"
             "-eval" (funcall
                       make-task
                       file-path-bucket))))))
