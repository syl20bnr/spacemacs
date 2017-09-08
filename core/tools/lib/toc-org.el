;;; toc-org.el --- add table of contents to org-mode files (formerly, org-toc)

;; Copyright (C) 2014 Sergei Nosov

;; Author: Sergei Nosov <sergei.nosov [at] gmail.com>
;; Version: 1.0
;; Keywords: org-mode org-toc toc-org org toc table of contents
;; URL: https://github.com/snosov1/toc-org

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; toc-org helps you to have an up-to-date table of contents in org files
;; without exporting (useful primarily for readme files on GitHub).

;; NOTE: Previous name of the package is org-toc. It was changed because of a
;; name conflict with one of the org contrib modules.

;; After installation put into your .emacs file something like

;; (if (require 'toc-org nil t)
;;     (add-hook 'org-mode-hook 'toc-org-enable)
;;   (warn "toc-org not found"))

;; And every time you'll be saving an org file, the first headline with a :TOC:
;; tag will be updated with the current table of contents.

;; For details, see https://github.com/snosov1/toc-org

;;; Code:

(require 'org)

(defgroup toc-org nil
  "toc-org is a utility to have an up-to-date table of contents
in the org files without exporting (useful primarily for readme
files on GitHub)"
  :group 'org)

;; just in case, simple regexp "^*.*:toc:\\($\\|[^ ]*:$\\)"
(defconst toc-org-toc-org-regexp "^*.*:toc\\([@_][0-9]\\|\\([@_][0-9][@_][a-zA-Z]+\\)\\)?:\\($\\|[^ ]*?:$\\)"
  "Regexp to find the heading with the :toc: tag")
(defconst toc-org-noexport-regexp "\\(^*+\\)\s+.*:noexport\\([@_][0-9]\\)?:\\($\\|[^ ]*?:$\\)"
  "Regexp to find the extended version of :noexport: tag")
(defconst toc-org-tags-regexp "\s*:[[:word:]:@_]*:\s*$"
  "Regexp to find tags on the line")
(defconst toc-org-states-regexp "^*+\s+\\(TODO\s+\\|DONE\s+\\)"
  "Regexp to find states on the line")
(defconst toc-org-COMMENT-regexp "\\(^*+\\)\s+\\(COMMENT\s+\\)"
  "Regexp to find COMMENT headlines")
(defconst toc-org-priorities-regexp "^*+\s+\\(\\[#.\\]\s+\\)"
  "Regexp to find states on the line")
(defconst toc-org-links-regexp "\\[\\[\\(.*?\\)\\]\\[\\(.*?\\)\\]\\]"
  "Regexp to find states on the line")
(defconst toc-org-special-chars-regexp "[^[:alnum:]_-]"
  "Regexp with the special characters (which are omitted in hrefs
  by GitHub)")
(defconst toc-org-statistics-cookies-regexp "\s*\\[[0-9]*\\(%\\|/[0-9]*\\)\\]\s*"
  "Regexp to find statistics cookies on the line")
(defconst toc-org-leave-todo-regexp "^#\\+OPTIONS:.*\stodo:t[\s\n]"
  "Regexp to find the todo export setting")
(defconst toc-org-drawer-regexp "^[ 	]*:\\(\\(?:\\w\\|[-_]\\)+\\):[ 	]*$"
  "Regexp to match org drawers. Note: generally, it should be
equal to `org-drawer-regexp'. However, some older versions of
org (notably, 8.2.10) restrict the values that can be placed
between the colons. So, the value here is set explicitly.")

(defcustom toc-org-max-depth 2
  "Maximum depth of the headings to use in the table of
contents. The default of 2 uses only the highest level headings
and their subheadings (one and two stars)."
  :group 'toc-org)

(defcustom toc-org-hrefify-default "gh"
  "Default hrefify function to use."
  :group 'toc-org)

(defcustom toc-org-enable-links-opening t
  "With this option, org-open-at-point (C-c C-o) should work on
the TOC links (even if the style is different from org)."
  :group 'toc-org)

(defvar-local toc-org-hrefify-hash nil
  "Buffer local hash-table that is used to enable links
opening. The keys are hrefified headings, the values are original
headings.")

(defun toc-org-raw-toc ()
  "Return the \"raw\" table of contents of the current file,
i.e. simply flush everything that's not a heading and strip
auxiliary text."
  (let ((content (buffer-substring-no-properties
                  (point-min) (point-max)))
        (leave-states-p nil))
    (with-temp-buffer
      (insert content)

      ;; set leave-states-p variable
      (goto-char (point-min))
      (when (re-search-forward toc-org-leave-todo-regexp nil t)
        (setq leave-states-p t))

      ;; keep only lines starting with *s
      (goto-char (point-min))
      (keep-lines "^\*+[ ]")

      ;; don't include the TOC itself
      (goto-char (point-min))
      (re-search-forward toc-org-toc-org-regexp nil t)
      (beginning-of-line)
      (delete-region (point) (progn (forward-line 1) (point)))

      ;; strip states
      (unless leave-states-p
        (goto-char (point-min))
        (while (re-search-forward toc-org-states-regexp nil t)
          (replace-match "" nil nil nil 1)))

      ;; strip COMMENT headlines
      (goto-char (point-min))
      (while (re-search-forward toc-org-COMMENT-regexp nil t)
        (let ((skip-depth (concat (match-string 1) "*")))
          (while (progn
                   (beginning-of-line)
                   (delete-region (point) (min (1+ (line-end-position)) (point-max)))
                   (string-prefix-p skip-depth (or (current-word) ""))))))

      ;; strip headings with :noexport: tag
      (goto-char (point-min))
      (while (re-search-forward toc-org-noexport-regexp nil t)
        (save-excursion
          (let* ((tag  (match-string 2))
                 (depth (if tag (string-to-number (substring tag 1)) 0))
                 (subheading-depth (concat (match-string 1) "*"))
                 (skip-depth (concat subheading-depth (make-string (max (1- depth) 0) ?*))))
            (if (> depth 0)
                (forward-line)
              (beginning-of-line)
              (delete-region (point) (min (1+ (line-end-position)) (point-max))))
            (while (string-prefix-p subheading-depth (or (current-word) ""))
              (if (string-prefix-p skip-depth (or (current-word) ""))
                  (progn
                    (beginning-of-line)
                    (delete-region (point) (min (1+ (line-end-position)) (point-max))))
                (forward-line))))))

      ;; strip priorities
      (goto-char (point-min))
      (while (re-search-forward toc-org-priorities-regexp nil t)
        (replace-match "" nil nil nil 1))

      ;; strip tags
      (goto-char (point-min))
      (while (re-search-forward toc-org-tags-regexp nil t)
        (replace-match "" nil nil))

      ;; flatten links
      (goto-char (point-min))
      (while (re-search-forward toc-org-links-regexp nil t)
        (replace-match "\\2" nil nil))

      (buffer-substring-no-properties
       (point-min) (point-max)))))

(defun toc-org-hrefify-gh (str &optional hash)
  "Given a heading, transform it into a href using the GitHub
rules."
  (let* ((spc-fix (replace-regexp-in-string " " "-" str))
         (upcase-fix (replace-regexp-in-string "[A-Z]" 'downcase spc-fix t))
         (special-chars-fix (replace-regexp-in-string toc-org-special-chars-regexp "" upcase-fix t))
         (hrefified-base (concat "#" special-chars-fix))
         (hrefified hrefified-base)
         (idx 0))
    ;; try appending -1, -2, -3, etc. until unique href is found
    (when hash
      (while (gethash hrefified hash)
        (setq hrefified
              (concat hrefified-base "-" (number-to-string (setq idx (1+ idx)))))))
    hrefified))

(defun toc-org-format-visible-link (str)
  "Formats the visible text of the link."
  (with-temp-buffer
    (insert str)

    ;; strip statistics cookies
    (goto-char (point-min))
    (while (re-search-forward toc-org-statistics-cookies-regexp nil t)
      (replace-match "" nil nil))
    (buffer-substring-no-properties
     (point-min) (point-max))))

(defun toc-org-hrefify-org (str &optional hash)
  "Given a heading, transform it into a href using the org-mode
rules."
  (toc-org-format-visible-link str))

(defun toc-org-unhrefify (type path)
  "Looks for a value in toc-org-hrefify-hash using path as a key."
  (let ((ret-type type)
        (ret-path path)
        (original-path (and (not (eq toc-org-hrefify-hash nil))
                            (gethash
                             (concat
                              ;; Org 8.3 and above provides type as "custom-id"
                              ;; and strips the leading hash symbol
                              (if (equal type "custom-id") "#" "")
                              (substring-no-properties path))
                             toc-org-hrefify-hash
                             nil))))
    (when toc-org-enable-links-opening
      (when original-path
        ;; Org 8.2 and below provides type as "thisfile"
        (when (equal type "thisfile")
          (setq ret-path original-path))
        (when (equal type "custom-id")
          (setq ret-type "fuzzy")
          (setq ret-path original-path))))
    (cons ret-type ret-path)))

(defun toc-org-hrefify-toc (toc hrefify &optional hash)
  "Format the raw `toc' using the `hrefify' function to transform
each heading into a link."
  (with-temp-buffer
    (insert toc)
    (goto-char (point-min))

    (while
        (progn
          (when (looking-at "\\*")
            (delete-char 1)

            (while (looking-at "\\*")
              (delete-char 1)
              (insert (make-string
                       (+ 2 (or (bound-and-true-p org-list-indent-offset) 0))
                       ?\s)))

            (insert "-")
            (skip-chars-forward " ")

	    (save-excursion
	      (delete-trailing-whitespace (point) (line-end-position)))

            (let* ((beg (point))
                   (end (line-end-position))
                   (heading (buffer-substring-no-properties
                             beg end))
                   (hrefified (funcall hrefify heading hash)))
              (insert "[[")
              (insert hrefified)
              (insert "][")
              (insert
               (toc-org-format-visible-link
                (buffer-substring-no-properties
                 (point) (line-end-position))))
              (delete-region (point) (line-end-position))
              (insert "]]")

              ;; maintain the hash table, if provided
              (when hash
                (puthash hrefified heading hash)))
            (= 0 (forward-line 1)))))

    (buffer-substring-no-properties
     (point-min) (point-max))))

(defun toc-org-flush-subheadings (toc max-depth)
  "Flush subheadings of the raw `toc' deeper than `max-depth'."
  (with-temp-buffer
    (insert toc)
    (goto-char (point-min))

    (let ((re "^"))
      (dotimes (i (1+ max-depth))
        (setq re (concat re "\\*")))
      (flush-lines re))

    (buffer-substring-no-properties
     (point-min) (point-max))))

(defun toc-org-insert-toc (&optional dry-run)
  "Update table of contents in heading tagged :TOC:.

When DRY-RUN is non-nil, the buffer is not modified, only the
internal hash-table is updated to enable `org-open-at-point' for
TOC links.

The table of contents heading may also be set with these tags:

- :TOC_#: Sets the maximum depth of the headlines in the table of
          contents to the number given, e.g. :TOC_3: for
          3 (default for plain :TOC: tag is 2).

- :TOC_#_gh: Sets the maximum depth as above and also uses
             GitHub-style anchors in the table of contents (the
             default).  The other supported style is :TOC_#_org:,
             which is the default org style.

Headings may be excluded from the TOC with these tags:

- :noexport: Exclude this heading.

- :noexport_#: Exclude this heading's children with relative
               level greater than number given (e.g. :noexport_1:
               causes all child headings to be excluded).

Note that :noexport: is also used by Org-mode's exporter, but
not :noexport_#:."

  (interactive)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
        ;; find the first heading with the :TOC: tag
        (when (re-search-forward toc-org-toc-org-regexp (point-max) t)
          (let* ((tag (match-string 1))
                 (depth (if tag
                            (- (aref tag 1) ?0) ;; is there a better way to convert char to number?
                          toc-org-max-depth))
                 (hrefify-tag (if (and tag (>= (length tag) 4))
                                  (downcase (substring tag 3))
                                toc-org-hrefify-default))
                 (hrefify-string (concat "toc-org-hrefify-" hrefify-tag))
                 (hrefify (intern-soft hrefify-string)))
            (if hrefify
                (let ((new-toc
                       (toc-org-hrefify-toc
                        (toc-org-flush-subheadings (toc-org-raw-toc) depth)
                        hrefify
                        (when toc-org-hrefify-hash
                          (clrhash toc-org-hrefify-hash)))))
                  (unless dry-run
                    (newline (forward-line 1))

                    ;; skip drawers
                    (let ((end
                           (save-excursion ;; limit to next heading
                             (search-forward-regexp "^\\*" (point-max) 'skip))))
                      (while (re-search-forward toc-org-drawer-regexp end t)
                        (skip-chars-forward "[:space:]")))
                    (beginning-of-line)

                    ;; insert newline if TOC is currently empty
                    (when (looking-at "^\\*")
                      (open-line 1))

                    ;; find TOC boundaries
                    (let ((beg (point))
                          (end
                           (save-excursion
                             (when (search-forward-regexp "^\\*" (point-max) 'skip)
                               (forward-line -1))
                             (end-of-line)
                             (point))))
                      ;; update the TOC, but only if it's actually different
                      ;; from the current one
                      (unless (equal
                               (buffer-substring-no-properties beg end)
                               new-toc)
                        (delete-region beg end)
                        (insert new-toc)))))
              (message (concat "Hrefify function " hrefify-string " is not found")))))))))

;;;###autoload
(defun toc-org-enable ()
  "Enable toc-org in this buffer."
  (add-hook 'before-save-hook 'toc-org-insert-toc nil t)

  ;; conservatively set org-link-translation-function
  (when (and (equal toc-org-enable-links-opening t)
             (or
              (not (fboundp org-link-translation-function))
              (equal org-link-translation-function 'toc-org-unhrefify)))
    (setq toc-org-hrefify-hash (make-hash-table :test 'equal))
    (setq org-link-translation-function 'toc-org-unhrefify)
    (toc-org-insert-toc t)))

;; Local Variables:
;; compile-command: "emacs -batch -l ert -l toc-org.el -l toc-org-test.el -f ert-run-tests-batch-and-exit && emacs -batch -f batch-byte-compile toc-org.el 2>&1 | sed -n '/Warning\|Error/p' | xargs -r ls"
;; End:

(provide 'toc-org)
;;; toc-org.el ends here
