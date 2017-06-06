;;; funcs.el --- Org Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(defun spacemacs/org-githubify-orgbuf ()
  "Transforms current README.org file buffer to be viewed via GitHub."
  (interactive)
  (let ((toc-line-rexp "^.*:TOC.*:.*$")
        (toc-item-rexp  "^[\s]*\-\s\\[\\[.*\\]\\[.*\\]\\].*$")
        (org-heading-rexp "^[\\*]+\s.*$")
        (github-link-rm-chars (string-to-list
                               (concat "?~!@#$%^&*()+|[>）"
                                       "]\"`'},.;:№=/^{<"))))
    (save-excursion
      (save-match-data

        ;; Find TOC
        (let ((last-pos 0)
              (toc-line-count 0))
          (progn (beginning-of-buffer)
                 (while (search-forward-regexp toc-line-rexp nil t)
                   (incf toc-line-count))
                 (unless (= toc-line-count 1)
                   (error (format (concat "Found %d lines annotated with TOC. "
                                          "Should be exactly 1.") toc-line-count)))))

        (let ((line-after-toc-head-beg-pos (progn
                                             (beginning-of-buffer)
                                             (search-forward-regexp toc-line-rexp)
                                             (line-beginning-position 2)))
              headings
              toc-items-and-levels)

          (progn  (goto-char line-after-toc-head-beg-pos)
                  ;; Remove TOC items
                  (while (looking-at-p toc-item-rexp) (kill-whole-line))
                  ;; Find all headings
                  (while (search-forward-regexp org-heading-rexp nil t)
                    (push (buffer-substring-no-properties
                           (line-beginning-position)
                           (line-end-position))
                          headings))
                  ;; Calculate headings levels
                  (mapcar (lambda (heading)
                            (let ((heading-lvl (- (length heading)
                                                  (length (replace-regexp-in-string
                                                           "^[\*]+"
                                                           ""
                                                           heading))))
                                  ;; List of filters to be applied to the headings
                                  (filters `((lambda (h-ing)
                                               (progn (mapcar (lambda (rm-char)
                                                                (setq h-ing (remove rm-char h-ing)))
                                                              github-link-rm-chars)
                                                      h-ing))
                                             ,(apply-partially 'replace-regexp-in-string "^[\s]*" "")
                                             ,(apply-partially 'replace-regexp-in-string "[\s]*$" "")
                                             ,(apply-partially 'replace-regexp-in-string " " "-")
                                             downcase)))

                              (progn
                                ;; Apply filters
                                (mapcar (lambda (filter) (setq heading (funcall filter heading))) filters)
                                (push (cons heading heading-lvl) toc-items-and-levels))))
                          (reverse headings))

                  ;; Populate TOC items
                  (mapcar*  (lambda (item-and-lvl)
                              (progn (setq item (format " - [[#%s][%s]]\n"
                                                        ;; Link
                                                        (car item-and-lvl)
                                                        ;; Remove *'s for titles.
                                                        (replace-regexp-in-string "^[\*]+\s"
                                                                                  ""
                                                                                  ;; Title
                                                                                  (pop headings))))
                                     ;; Create indentations
                                     (dotimes , (1- (cdr item-and-lvl)) (setq item (concat "  " item)))
                                     ;; Inset the items after the TOC header
                                     (goto-char line-after-toc-head-beg-pos)
                                     (insert item)))

                            toc-items-and-levels)))))))
