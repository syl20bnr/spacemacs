;;; org-export-ftest.el --- Spacemacs Org Export Functional Test File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: smile13241324 <smile13241324@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'mocker)
(require 'org)
(require 'core-documentation)

;; Replacement for directory-files-recursively for emacs < 25.1.1
(defun directory-files-recursive_ForOldEmacs (directory match maxdepth ignore)
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
          (setq files-list (append files-list (directory-files-recursive_ForOldEmacs f match (- maxdepth -1) ignore)))
          )
         (t)
         )
        )
      (setq current-directory-list (cdr current-directory-list)))
    files-list))

;; -----------------------------------------------------------------------------
;; Spacemacs Documentation HTML Export Test
;; Currently checks whether all org documentation files can be converted to html
;; -----------------------------------------------------------------------------
(ert-deftest test-spacemacs-html-export ()
  (unwind-protect (spacemacs/publish-doc)
    (delete-directory (concat spacemacs-start-directory
                              "export/")
                      t)))
