;;; fmt.el --- .org file formatter.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(load (expand-file-name "./core/tools/spacefmt/toc-org.el") nil t)

(require 'cl)
(require 'files)
(require 'org)
(require 'thingatpt)

(defconst empty-line-regexp "^[ \t]*$")
(defconst tree-trunk-regexp "^[ 	]*|_")

(defconst toc-heading-head "* Table of Contents")
(defconst toc-heading-tail ":TOC_4_gh:noexport:")
(defconst toc-headline (format "%-41s%s"
                               toc-heading-head
                               toc-heading-tail))

(defun apply-all ()
  (message "Processing %s file.." (buffer-file-name))
  "Apply all filters."
  (remove-empty-lines-at-the-beginning)
  (insert-title)
  (insert-toc)
  (apply-toc)
  (remove-empty-lines-after-headlines)
  ;; Multiply empty lines are handled by
  ;; the bash script.
  (insert-empty-line-before-tables)
  (insert-empty-line-after-tables)
  (insert-empty-line-after-sections)
  (insert-empty-line-at-the-end)
  (align-tables)
  (save-buffer 0))

(defun remove-empty-lines-at-the-beginning ()
  "Remove empty lines at the begging of the buffer."
  (goto-char (point-min))
  (while (looking-at-p empty-line-regexp)
    (delete-blank-lines)))

(defun insert-empty-line-at-the-end ()
  "Insert an empty line at the end of the buffer."
  (goto-char (point-max))
  (unless (looking-at-p empty-line-regexp)
    (open-line 1)))

(defun insert-title()
  "Insert #TITLE:{DIR_NAME} if the buffer doesn't have one."
  (goto-char (point-min))
  (unless (looking-at-p "^#\\+TITLE:.*$")
    (insert (format "#+TITLE:%s\n"
                    (clj/->> (buffer-file-name)
                             file-name-directory
                             directory-file-name
                             file-name-base)))))

(defun insert-toc ()
  "Insert toc if the buffer doesn't have one."
  (goto-char (point-min))
  (unless (re-search-forward toc-org-toc-org-regexp nil t)
    (goto-char (point-max))
    ;; Skip from the end of the buffer to the first headling.
    (while (re-search-backward org-heading-regexp nil t))
    (open-line 3)
    (forward-line 1)
    (insert toc-headline)))

(defun remove-empty-lines-after-headlines()
  "Remove empty liners after each headline."
  (goto-char (point-min))
  (while (re-search-forward org-heading-regexp nil t)
    (unless (= (forward-line) 0)
      (while (looking-at-p empty-line-regexp)
        (delete-blank-lines)))))

(defun insert-empty-line-before-tables ()
  "Insert an empty line before each org table."
  (goto-char (point-min))
  (while (goto-next-table)
    (forward-line -1)
    (unless (looking-at-p empty-line-regexp)
      (end-of-line)
      (open-line 1))
    (forward-line 1)))

(defun insert-empty-line-after-sections ()
  "Insert an empty line after each section."
  (goto-char (point-min))
  (while (re-search-forward org-heading-regexp nil t)
    (forward-line -1)
    (unless (or (looking-at-p empty-line-regexp)
                (looking-at-p org-heading-regexp))
      (end-of-line)
      (open-line 1))
    (forward-line 2)))

(defun insert-empty-line-after-tables ()
  "Insert an empty line after each table."
  (goto-char (point-min))
  (while (goto-next-table)
    ;; Skip current table.
    (while (looking-at-p org-table-any-line-regexp)
      (forward-line))
    (unless (looking-at-p empty-line-regexp)
      (goto-char (point-at-bol))
      (open-line 1)
      (forward-line))))

(defun align-tables ()
  "Align all tables"
  (goto-char (point-min))
  (while (goto-next-table)
    (org-table-align)))

(defun apply-toc ()
  "Apply current toc-org TAG to TOC."
  (goto-char (point-min))
  (toc-org-insert-toc))

(defun goto-next-table ()
  "Goto next org table.
Returns nil if no more tables left."
  ;; Skip current table.
  (goto-char (point-at-bol))
  (while (looking-at-p org-table-any-line-regexp)
    (goto-char (point-at-bol))
    (forward-line))
  ;; Skip to the next table.
  (re-search-forward org-table-any-line-regexp nil t)
  (goto-char (point-at-bol))
  (when (looking-at-p tree-trunk-regexp)
    (goto-next-table))
  (looking-at-p org-table-any-line-regexp))

(defun move-packages-to-config ()
  "Move xxx-packages list to config.el."
  (let ((config-file (concat default-directory "config.el")))
    (when (or (re-search-forward "(setq.*-packages" nil t)
              (re-search-forward "(defcustom.*-packages" nil t))
      (re-search-backward "(")
      (kill-sexp)
      (with-current-buffer (find-file-noselect config-file)
        (when (file-exists-p config-file)
          (re-search-forward ";;; License: GPLv3")
          (newline)
          (forward-line 2))
        (yank)
        ;; config.el
        (save-buffer 0))
      ;; packages.el
      (save-buffer 0))))

(defmacro clj/->> (o &rest forms)
  "Threads the expr through the forms.
Inserts o as the  last item in the first form,
making a list of it if it is not a  list already.
If there are more forms, inserts the first form
as the  last item in second form, etc."
  (cond ((not forms) o)
        ((= 1 (length forms))
         (let ((f (first forms)))
           (append (if (symbolp f)
                       (list f) f)
                   (list o))))
        (:else `(clj/->> (clj/->> ,o ,(first forms)) ,@(rest forms)))))
