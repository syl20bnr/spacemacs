;;; org-export-ftest.el --- Space-macs Org Export Functional Test File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: smile13241324 <smile13241324@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3
(require 'mocker)
(require 'org)
(require 'core-documentation)

;; Replacement for directory-files-recursively for e-macs < 25.1.1
(defun directory-files-recursive_ForOlde-macs (directory match maxdepth ignore)
  "List files in DIRECTORY and in its sub-directories.
   Return files that match the regular expression MATCH but ignore
   files and directories that match IGNORE (IGNORE is tested before MATCH. Recurse only
   to depth MAXDEPTH. If zero or negative, then do not recurse"
  (let* ((files-list '())
         (current-directory-list
          (directory-files directory t)))
    ;; while we are in the current directory
    (while current-directory-list
      (let ((f (car current-directory-list)))
        (cond
         ((and
           ignore ;; make sure it is not nil
           (string-match ignore f))
                                        ; ignore
          nil
          )
         ((and
           (file-regular-p f)
           (file-readable-p f)
           (not(file-directory-p f))
           (string-match match f))
          (setq files-list (cons f files-list))
          )
         ((and
           (file-directory-p f)
           (file-readable-p f)
           (not (string-equal ".." (substring f -2)))
           (not (string-equal "." (substring f -1)))
           (> maxdepth 0))
          ;; recurse only if necessary
          (setq files-list (append files-list (directory-files-recursive_ForOlde-macs f match (- maxdepth -1) ignore)))
          )
         (t)
         )
        )
      (setq current-directory-list (cdr current-directory-list)))
    files-list))

;; -----------------------------------------------------------------------------
;; Space-macs Documentation HTML Export Test
;; Currently checks whether all org documentation files can be converted to html
;; -----------------------------------------------------------------------------
(ert-deftest test-space-macs-html-export ()
  (unwind-protect (space-macs/publish-doc)
    (delete-directory (concat space-macs-start-directory
                              "export/")
                      t)))


