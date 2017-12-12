;;; _worker.el --- Spacemacs doc formatter worker -*- lexical-binding: t -*-
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;;         Eugene "JAremko" Yaremenko <w3techplayground@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(when (and load-file-name
           noninteractive)
  (setq gc-cons-threshold 10000000000))

(load
 (concat
  (file-name-directory
   (or load-file-name
       buffer-file-name))
  "../lib/toc-org.elc")
 nil t)

(declare-function toc-org-hrefify-gh "../lib/toc-org.el" (str &optional hash))

(eval-when-compile
  (require 'cl))

(defconst spacemacs--docfmt-empty-line-regexp "^[ \t]*$")
(defconst spacemacs--docfmt-tree-trunk-regexp "^[ 	]*|_")
(defconst spacemacs--docfmt-toc-heading-head "* Table of Contents")
(defconst spacemacs--docfmt-toc-heading-tail ":TOC_4_gh:noexport:")
(defconst spacemacs--docfmt-toc-headline (format
                                          "%-41s%s"
                                          spacemacs--docfmt-toc-heading-head
                                          spacemacs--docfmt-toc-heading-tail))

(defsubst spacemacs//docfmt-rm-empty-lines-at-beg ()
  "Remove newlines at the beginning of the buffer."
  (goto-char (point-min))
  (while (looking-at-p spacemacs--docfmt-empty-line-regexp)
    (delete-blank-lines)))

(defsubst spacemacs//docfmt-rm-empty-lines-at-end ()
  "Remove newlines at the ending of the buffer."
   (goto-char (point-max))
   (delete-blank-lines)
   (delete-blank-lines))

(defsubst spacemacs//docfmt-rm-trail-delim-in-hl ()
  "Remove trailing delimiters in headlines."
  (goto-char (point-min))
  (while (re-search-forward "^*+[[:space:]]+.*\\([;,\\.[:space:]]+\\)$" nil t)
   (replace-match "" nil nil nil 1)
   (forward-line -1)))

(defsubst spacemacs//docfmt-remove-readtheorg-meta ()
  "Remove '#+HTML_HEAD_EXTRA: ... readtheorg.css\" />'."
  (goto-char (point-min))
  (while (re-search-forward "#\\+HTML_HEAD_EXTRA.*readtheorg\\\.css.*" nil t)
   (replace-match "")))

(defsubst spacemacs//docfmt-multy-nl-with-single ()
  "Replace multiply empty lines with a single empty line."
  (goto-char (point-min))
  (while (re-search-forward "\\(^[[:space:]]*$\\)\n" nil t)
    (replace-match "\n")
    (forward-char)))

(defsubst spacemacs//docfmt-replace-org-toc ()
  "Replace \":TOC_X_org:\" with \":TOC_4_gh:\"."
  (goto-char (point-min))
  (while (re-search-forward toc-org-toc-org-regexp nil t)
   (replace-match "_4_gh" nil nil nil 2)))

(defsubst spacemacs//docfmt-goto-next-table ()
  "Goto next org table.
Returns nil if no more tables left."
  (loop
   ;; Skip current table.
   (goto-char (point-at-bol))
   (while (and (looking-at-p org-table-any-line-regexp)
               (not (= (point) (point-max))))
     (goto-char (point-at-bol))
     (forward-line))
   ;; Skip to the next table.
   (re-search-forward org-table-any-line-regexp nil t)
   (goto-char (point-at-bol))
   (unless (looking-at-p spacemacs--docfmt-tree-trunk-regexp)
     (return)))
  (looking-at-p org-table-any-line-regexp))

(defsubst spacemacs//docfmt-remote-empty-lines-at-the-beginning ()
  "Remove empty lines at the begging of the buffer."
  (goto-char (point-min))
  (while (looking-at-p spacemacs--docfmt-empty-line-regexp)
    (delete-blank-lines)))

(defsubst spacemacs//docfmt-insert-empoty-line-at-the-end ()
  "Insert an empty line at the end of the buffer."
  (goto-char (point-max))
  (unless (looking-at-p spacemacs--docfmt-empty-line-regexp)
    (open-line 1)))

(defsubst spacemacs//docfmt-insert-title ()
  "Insert #TITLE:{DIR_NAME} if the buffer doesn't have one."
  (goto-char (point-min))
  (unless (looking-at-p "^#\\+TITLE:.*$")
    (insert (format "#+TITLE:%s\n"
                    (file-name-base
                     (directory-file-name
                      (file-name-directory
                       (buffer-file-name))))))))

(defsubst spacemacs//docfmt-insert-toc ()
  "Insert toc if the buffer doesn't have one."
  (goto-char (point-min))
  (unless (re-search-forward toc-org-toc-org-regexp nil t)
    (goto-char (point-max))
    ;; Skip from the end of the buffer to the first headling.
    (while (re-search-backward org-heading-regexp nil t))
    (open-line 3)
    (forward-line 1)
    (insert spacemacs--docfmt-toc-headline)))

(defsubst spacemacs//docfmt-remove-empty-lines-after-headlines()
  "Remove empty liners after each headline."
  (goto-char (point-min))
  (while (re-search-forward org-heading-regexp nil t)
    (unless (= (forward-line) 0)
      (while (looking-at-p spacemacs--docfmt-empty-line-regexp)
        (delete-blank-lines)))))

(defsubst spacemacs//docfmt-insert-empoty-line-before-tables ()
  "Insert an empty line before each org table."
  (goto-char (point-min))
  (while (spacemacs//docfmt-goto-next-table)
    (forward-line -1)
    (unless (looking-at-p spacemacs--docfmt-empty-line-regexp)
      (end-of-line)
      (open-line 1))
    (forward-line 1)))

(defsubst spacemacs//docfmt-insert-empoty-line-after-sections ()
  "Insert an empty line after each section."
  (goto-char (point-min))
  (while (re-search-forward org-heading-regexp nil t)
    (forward-line -1)
    (unless (or (looking-at-p spacemacs--docfmt-empty-line-regexp)
                (looking-at-p org-heading-regexp))
      (end-of-line)
      (open-line 1))
    (forward-line 2)))

(defsubst spacemacs//docfmt-insert-empoty-line-after-tables ()
  "Insert an empty line after each table."
  (goto-char (point-min))
  (while (spacemacs//docfmt-goto-next-table)
    ;; Skip current table.
    (while (looking-at-p org-table-any-line-regexp)
      (forward-line))
    (unless (looking-at-p spacemacs--docfmt-empty-line-regexp)
      (goto-char (point-at-bol))
      (open-line 1)
      (forward-line))))

(defsubst spacemacs//docfmt-align-tables ()
  "Align all tables"
  (goto-char (point-min))
  (while (spacemacs//docfmt-goto-next-table)
    (ignore-errors
      (org-table-align))))

(defsubst spacemacs//docfmt-apply-toc ()
  "Apply current toc-org TAG to TOC."
  (toc-org-enable)
  (goto-char (point-min))
  (toc-org-insert-toc))

(defun spacemacs/docfmt-apply-all ()
  "Format current `org-mode' buffer."
  (let ((old-buff-str (buffer-string))
        (new-buff-str ""))
    (loop
     (spacemacs//docfmt-rm-empty-lines-at-beg)
     (spacemacs//docfmt-rm-empty-lines-at-end)
     (spacemacs//docfmt-remove-readtheorg-meta)
     (spacemacs//docfmt-replace-org-toc)
     (spacemacs//docfmt-rm-trail-delim-in-hl)
     (spacemacs//docfmt-multy-nl-with-single)
     (spacemacs//docfmt-remote-empty-lines-at-the-beginning)
     (spacemacs//docfmt-insert-title)
     (spacemacs//docfmt-insert-toc)
     (spacemacs//docfmt-apply-toc)
     (spacemacs//docfmt-remove-empty-lines-after-headlines)
     (spacemacs//docfmt-insert-empoty-line-before-tables)
     (spacemacs//docfmt-insert-empoty-line-after-tables)
     (spacemacs//docfmt-insert-empoty-line-after-sections)
     (spacemacs//docfmt-insert-empoty-line-at-the-end)
     (spacemacs//docfmt-align-tables)
     (setq new-buff-str (buffer-string))
     (if (string= old-buff-str new-buff-str)
         (return)
       (setq old-buff-str new-buff-str)))))

(defun spacemacs/docfmt-apply-all-batch (files)
  "Function for batch processing FILES in `noninteractive' mode."
  (message "Files to be formatted: %S" files)
  (dolist (file files)
    (with-temp-file file
      (insert-file-contents file)
      (set-visited-file-name file t t)
      (spacemacs/docfmt-apply-all)))
  (message "Done!"))
