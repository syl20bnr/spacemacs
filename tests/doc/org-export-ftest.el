;;; org-export-ftest.el --- Spacemacs Org Export Functional Test File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
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

;; ---------------------------------------------------------------------------
;; Spacemacs Documentation HTML Export Test
;; Currently checks whether all org documentation files can be converted to html
;; ---------------------------------------------------------------------------
(ert-deftest test-spacemacs-html-export ()
  (unwind-protect (spacemacs/publish-doc)
    (delete-directory (concat spacemacs-start-directory
                              "export/")
                      t))
  ;; Activate this to check all external links, beware don't try this on travis bandwith limits will prevent this from finishing
  ;;
  ;; (let ((allFiles (directory-files-recursive_ForOldEmacs (concat spacemacs-start-directory "export/") "\\.html" 9999 "NeverIgnore")))
  ;;   (dolist (file allFiles)
  ;;     (when (file-readable-p file)
  ;;        (with-temp-buffer
  ;;          (message "%s" file)
  ;;          (insert-file-contents file)
  ;;          (while (re-search-forward "\\(?:href\\|src\\)[ \n]*=[ \n]*\"\\([^\"]+?\\)\"" nil t)

  ;;              ;;Start url validation
  ;;              (let ((url (match-string 1))
  ;;                    (p-current (point))
  ;;                    (p-mb (match-beginning 0)))
  ;;                (save-excursion
  ;;                  (search-backward "<" nil t)
  ;;                  (setq p1 (point))
  ;;                  (search-forward ">" nil t)
  ;;                  (setq p2 (point)))

  ;;                (when (and (< p1 p-mb) (< p-current p2) ) ; the “href="…"” is inside <…>
  ;;                  (if (string-match "^http://\\|^https://" url)
  ;;                      (progn
  ;;                        ;; (message "%s%s" "Checking " url)
  ;;                        (url-retrieve-synchronously url t t)
  ;;                        )
  ;;                        ;; Checking for internal links not done yet
  ;;                        ;; (progn
  ;;                        ;;   (message "%s%s" "Checking " (concat "file://" url))
  ;;                        ;;   (url-retrieve-synchronously (concat "file://" url) t))
  ;;                        ))))))))
  )
