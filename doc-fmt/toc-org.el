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

(require 'ert)
(require 'org)

(defgroup toc-org nil
  "toc-org is a utility to have an up-to-date table of contents
in the org files without exporting (useful primarily for readme
files on GitHub)"
  :group 'org)

;; just in case, simple regexp "^*.*:toc:\\($\\|[^ ]*:$\\)"
(defconst toc-org-toc-org-regexp "^*.*:toc\\([@_][0-9]\\|\\([@_][0-9][@_][a-zA-Z]+\\)\\)?:\\($\\|[^ ]*:$\\)"
  "Regexp to find the heading with the :toc: tag")
(defconst toc-org-tags-regexp "\s*:[[:word:]:@]*:\s*$"
  "Regexp to find tags on the line")
(defconst toc-org-states-regexp "^*+\s+\\(TODO\s+\\|DONE\s+\\)"
  "Regexp to find states on the line")
(defconst toc-org-links-regexp "\\[\\[\\(.*?\\)\\]\\[\\(.*?\\)\\]\\]"
  "Regexp to find states on the line")
(defconst toc-org-special-chars-regexp "[^[:alnum:]_-]"
  "Regexp with the special characters (which are omitted in hrefs
  by GitHub)")

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
tags."
  (let ((content (buffer-substring-no-properties
                  (point-min) (point-max))))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (keep-lines "^\*+[ ]")

      ;; don't include the TOC itself
      (goto-char (point-min))
      (re-search-forward toc-org-toc-org-regexp nil t)
      (beginning-of-line)
      (delete-region (point) (progn (forward-line 1) (point)))

      ;; strip states
      (goto-char (point-min))
      (while (re-search-forward toc-org-states-regexp nil t)
        (replace-match "" nil nil nil 1))

      ;; strip tags
      ;; TODO :export: and :noexport: tags semantic should be probably
      ;; implemented
      (goto-char (point-min))
      (while (re-search-forward toc-org-tags-regexp nil t)
        (replace-match "" nil nil))

      ;; flatten links
      (goto-char (point-min))
      (while (re-search-forward toc-org-links-regexp nil t)
        (replace-match "\\2" nil nil))

      (buffer-substring-no-properties
       (point-min) (point-max)))))

(ert-deftest toc-org-test-raw-toc ()
  "Test the `toc-org-raw-toc' function"

  (defun toc-org-test-raw-toc-gold-test (content gold)
    (should (equal
             (with-temp-buffer
               (insert content)
               (toc-org-raw-toc))
             gold)))
  (declare-function toc-org-test-raw-toc-gold-test "toc-org") ;; suppress compiler warning

  (let ((beg "* TODO [[http://somewhere.com][About]]\n:TOC:\n drawer\n:END:\n\ntoc-org is a utility to have an up-to-date table of contents in the\norg files without exporting (useful primarily for readme files on\nGitHub).\n\nIt is similar to the [[https://github.com/ardumont/markdown-toc][markdown-toc]] package, but works for org files.\n:TOC:\n  drawer\n:END:\n\n* Table of Contents                                                     ")
        (gold "* About\n"))

    ;; different TOC styles
    (toc-org-test-raw-toc-gold-test (concat beg ":TOC:"         ) gold)
    (toc-org-test-raw-toc-gold-test (concat beg ":TOC_1:"       ) gold)
    (toc-org-test-raw-toc-gold-test (concat beg ":TOC_1_qqq:"   ) gold)
    (toc-org-test-raw-toc-gold-test (concat beg ":TOC@1:"       ) gold)
    (toc-org-test-raw-toc-gold-test (concat beg ":TOC@1@cxv:"   ) gold)
    (toc-org-test-raw-toc-gold-test (concat beg ":TOC@1_hello:" ) gold)

    ;; trailing symbols
    (toc-org-test-raw-toc-gold-test (concat beg ":TOC@1_hello:" "\n\n\n") gold)
    (toc-org-test-raw-toc-gold-test (concat beg ":TOC@1_hello:" "\n\n\nsdfd") gold))

  ;; more complex case
  (toc-org-test-raw-toc-gold-test
   "* About\n:TOC:\n drawer\n:END:\n\ntoc-org is a utility to have an up-to-date table of contents in the\norg files without exporting (useful primarily for readme files on\nGitHub).\n\nIt is similar to the [[https://github.com/ardumont/markdown-toc][markdown-toc]] package, but works for org files.\n:TOC:\n  drawer\n:END:\n\n* Table of Contents                                                     :TOC:\n - [[#about][About]]\n - [[#use][Use]]\n - [[#different-href-styles][Different href styles]]\n - [[#example][Example]]\n\n* Installation\n** via package.el\nThis is the simplest method if you have the package.el module\n(built-in since Emacs 24.1) you can simply use =M-x package-install=\nand then put the following snippet in your ~/.emacs file\n#+BEGIN_SRC elisp\n  (eval-after-load \"toc-org-autoloads\"\n    '(progn\n       (if (require 'toc-org nil t)\n           (add-hook 'org-mode-hook 'toc-org-enable)\n         (warn \"toc-org not found\"))))\n#+END_SRC\n** Manual                                                             :Hello:\n- Create folder ~/.emacs.d if you don't have it\n- Go to it and clone toc-org there\n  #+BEGIN_SRC sh\n    git clone https://github.com/snosov1/toc-org.git\n  #+END_SRC\n- Put this in your ~/.emacs file\n  #+BEGIN_SRC elisp\n    (add-to-list 'load-path \"~/.emacs.d/toc-org\")\n    (when (require 'toc-org nil t)\n      (add-hook 'org-mode-hook 'toc-org-enable))\n  #+END_SRC\n\n* Use\n\nAfter the installation, every time you'll be saving an org file, the\nfirst headline with a :TOC: tag will be updated with the current table\nof contents.\n\nTo add a TOC tag, you can use the command =org-set-tags-command=.\n\nIn addition to the simple :TOC: tag, you can also use the following\ntag formats:\n\n- :TOC@2: - sets the max depth of the headlines in the table of\n  contents to 2 (the default)\n\n- :TOC@2@gh: - sets the max depth as in above and also uses the\n  GitHub-style hrefs in the table of contents (the default). The other\n  supported href style is 'org', which is the default org style (you\n  can use C-c C-o to go to the headline at point).\n\nYou can also use =_= as separator, instead of =@=.\n\n* Different href styles\n\nCurrently, only 2 href styles are supported: =gh= and =org=. You can easily\ndefine your own styles. If you use the tag =:TOC@2@STYLE:= (=STYLE= being a\nstyle name), then the package will look for a function named\n=toc-org-hrefify-STYLE=, which accepts a heading string and returns a href\ncorresponding to that heading.\n\nE.g. for =org= style it simply returns input as is:\n\n#+BEGIN_SRC emacs-lisp\n  (defun toc-org-hrefify-org (str)\n    \"Given a heading, transform it into a href using the org-mode\n  rules.\"\n    str)\n#+END_SRC\n\n* Example\n\n#+BEGIN_SRC org\n  * About\n  * Table of Contents                                           :TOC:\n    - [[#about][About]]\n    - [[#installation][Installation]]\n        - [[#via-packageel][via package.el]]\n        - [[#manual][Manual]]\n    - [[#use][Use]]\n  * Installation\n  ** via package.el\n  ** Manual\n  * Use\n  * Example\n#+END_SRC\n"
   "* About\n* Installation\n** via package.el\n** Manual\n* Use\n* Different href styles\n* Example\n"))

(defun toc-org-hrefify-gh (str)
  "Given a heading, transform it into a href using the GitHub
rules."
  (let* ((spc-fix (replace-regexp-in-string " " "-" str))
         (upcase-fix (replace-regexp-in-string "[A-Z]" 'downcase spc-fix t))
         (special-chars-fix (replace-regexp-in-string toc-org-special-chars-regexp "" upcase-fix t)))
    (concat "#" special-chars-fix)))

(ert-deftest toc-org-test-hrefify-gh ()
  "Test the `toc-org-hrefify-gh' function"
  (should (equal (toc-org-hrefify-gh "About") "#about"))
  (should (equal (toc-org-hrefify-gh "!h@#$%^&*(){}|][:;\"'/?.>,<`~") "#h"))
  (should (equal (toc-org-hrefify-gh "!h@#$% ^&*(S){}|][:;\"'/?.>,<`~") "#h-s")))

(defun toc-org-hrefify-org (str)
  "Given a heading, transform it into a href using the org-mode
rules."
  str)

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

            (skip-chars-forward " ")
            (insert "- ")

            (let* ((beg (point))
                   (end (line-end-position))
                   (heading (buffer-substring-no-properties
                             beg end))
                   (hrefified (funcall hrefify heading)))
              (insert "[[")
              (insert hrefified)
              (insert "][")
              (end-of-line)
              (insert "]]")

              ;; maintain the hash table, if provided
              (when hash
                (puthash hrefified heading hash)))
            (= 0 (forward-line 1)))))

    (buffer-substring-no-properties
     (point-min) (point-max))))

(ert-deftest toc-org-test-hrefify-toc ()
  (let ((hash (make-hash-table :test 'equal)))
    (should (equal (toc-org-hrefify-toc "* About\n" 'upcase hash)
                   " - [[ABOUT][About]]\n"))
    (should (equal (gethash "ABOUT" hash) "About")))
  (let ((hash (make-hash-table :test 'equal)))
    (should (equal (toc-org-hrefify-toc "* About\n* Installation\n** via package.el\n** Manual\n* Use\n* Different href styles\n* Example\n" 'upcase hash)
                   " - [[ABOUT][About]]\n - [[INSTALLATION][Installation]]\n   - [[VIA PACKAGE.EL][via package.el]]\n   - [[MANUAL][Manual]]\n - [[USE][Use]]\n - [[DIFFERENT HREF STYLES][Different href styles]]\n - [[EXAMPLE][Example]]\n"))
    (should (equal (gethash "ABOUT" hash) "About"))
    (should (equal (gethash "INSTALLATION" hash) "Installation"))
    (should (equal (gethash "VIA PACKAGE.EL" hash) "via package.el"))
    (should (equal (gethash "MANUAL" hash) "Manual"))
    (should (equal (gethash "USE" hash) "Use"))
    (should (equal (gethash "DIFFERENT HREF STYLES" hash) "Different href styles"))
    (should (equal (gethash "EXAMPLE" hash) "Example"))))

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

(ert-deftest toc-org-test-flush-subheadings ()
  (should (equal (toc-org-flush-subheadings "* About\n" 0)
                 ""))
  (should (equal (toc-org-flush-subheadings "* About\n" 1)
                 "* About\n"))
  (should (equal (toc-org-flush-subheadings "* About\n" 2)
                 "* About\n"))

  (should (equal (toc-org-flush-subheadings "* About\n* Installation\n** via package.el\n** Manual\n* Use\n* Different href styles\n* Example\n" 0)
                 ""))
  (should (equal (toc-org-flush-subheadings "* About\n* Installation\n** via package.el\n** Manual\n* Use\n* Different href styles\n* Example\n" 1)
                 "* About\n* Installation\n* Use\n* Different href styles\n* Example\n"))
  (should (equal (toc-org-flush-subheadings "* About\n* Installation\n** via package.el\n** Manual\n* Use\n* Different href styles\n* Example\n" 2)
                 "* About\n* Installation\n** via package.el\n** Manual\n* Use\n* Different href styles\n* Example\n"))
  (should (equal (toc-org-flush-subheadings "* About\n* Installation\n** via package.el\n** Manual\n* Use\n* Different href styles\n* Example\n" 3)
                 "* About\n* Installation\n** via package.el\n** Manual\n* Use\n* Different href styles\n* Example\n")))

(defun toc-org-insert-toc (&optional dry-run)
  "Looks for a headline with the TOC tag and updates it with the
current table of contents.

If optional second argument DRY-RUN is provided, then the buffer
is not modified at all. Only the internal hash-table is updated
to enable `org-open-at-point' for TOC links.

To add a TOC tag, you can use the command
`org-set-tags-command' (C-c C-q).

In addition to the simple :TOC: tag, you can also use the
following tag formats:

- :TOC_2: - sets the max depth of the headlines in the table of
  contents to 2 (the default)

- :TOC_2_gh: - sets the max depth as in above and also uses the
  GitHub-style hrefs in the table of contents (this style is
  default). The other supported href style is 'org', which is the
  default org style."

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

                    ;; insert newline if TOC is currently empty
                    (when (looking-at "^\\*")
                      (open-line 1))

                    ;; find TOC boundaries
                    (let ((beg (point))
                          (end
                           (save-excursion
                             (when (search-forward-regexp "^\\*" (point-max) t)
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

(ert-deftest toc-org-test-insert-toc ()
  "Test the `toc-org-insert-toc' function"

  (defun toc-org-test-insert-toc-gold-test (content gold)
    (with-temp-buffer
      (org-mode)
      (insert content)
      (toc-org-raw-toc)
      (toc-org-insert-toc)
      (should (equal
               (buffer-substring-no-properties
                (point-min) (point-max))
               gold))))
  (declare-function toc-org-test-insert-toc-gold-test "toc-org") ;; suppress compiler warning

  (let ((beg "* About\n:TOC:\n drawer\n:END:\n\ntoc-org is a utility to have an up-to-date table of contents in the\norg files without exporting (useful primarily for readme files on\nGitHub).\n\nIt is similar to the [[https://github.com/ardumont/markdown-toc][markdown-toc]] package, but works for org files.\n:TOC:\n  drawer\n:END:\n* Hello\n** Good-bye\n*** Salut\n* Table of Contents                                                     "))
    (toc-org-test-insert-toc-gold-test
     (concat beg ":TOC:")
     "* About\n:TOC:\n drawer\n:END:\n\ntoc-org is a utility to have an up-to-date table of contents in the\norg files without exporting (useful primarily for readme files on\nGitHub).\n\nIt is similar to the [[https://github.com/ardumont/markdown-toc][markdown-toc]] package, but works for org files.\n:TOC:\n  drawer\n:END:\n* Hello\n** Good-bye\n*** Salut\n* Table of Contents                                                     :TOC:\n - [[#about][About]]\n - [[#hello][Hello]]\n   - [[#good-bye][Good-bye]]\n")

    (toc-org-test-insert-toc-gold-test
     (concat beg ":TOC_1:")
     "* About\n:TOC:\n drawer\n:END:\n\ntoc-org is a utility to have an up-to-date table of contents in the\norg files without exporting (useful primarily for readme files on\nGitHub).\n\nIt is similar to the [[https://github.com/ardumont/markdown-toc][markdown-toc]] package, but works for org files.\n:TOC:\n  drawer\n:END:\n* Hello\n** Good-bye\n*** Salut\n* Table of Contents                                                     :TOC_1:\n - [[#about][About]]\n - [[#hello][Hello]]\n")

    (toc-org-test-insert-toc-gold-test
     (concat beg ":TOC_3:")
     "* About\n:TOC:\n drawer\n:END:\n\ntoc-org is a utility to have an up-to-date table of contents in the\norg files without exporting (useful primarily for readme files on\nGitHub).\n\nIt is similar to the [[https://github.com/ardumont/markdown-toc][markdown-toc]] package, but works for org files.\n:TOC:\n  drawer\n:END:\n* Hello\n** Good-bye\n*** Salut\n* Table of Contents                                                     :TOC_3:\n - [[#about][About]]\n - [[#hello][Hello]]\n   - [[#good-bye][Good-bye]]\n     - [[#salut][Salut]]\n")

    (toc-org-test-insert-toc-gold-test
     (concat beg ":TOC_1_org:")
     "* About\n:TOC:\n drawer\n:END:\n\ntoc-org is a utility to have an up-to-date table of contents in the\norg files without exporting (useful primarily for readme files on\nGitHub).\n\nIt is similar to the [[https://github.com/ardumont/markdown-toc][markdown-toc]] package, but works for org files.\n:TOC:\n  drawer\n:END:\n* Hello\n** Good-bye\n*** Salut\n* Table of Contents                                                     :TOC_1_org:\n - [[About][About]]\n - [[Hello][Hello]]\n")

    (toc-org-test-insert-toc-gold-test
     (concat beg ":TOC_3_org:")
     "* About\n:TOC:\n drawer\n:END:\n\ntoc-org is a utility to have an up-to-date table of contents in the\norg files without exporting (useful primarily for readme files on\nGitHub).\n\nIt is similar to the [[https://github.com/ardumont/markdown-toc][markdown-toc]] package, but works for org files.\n:TOC:\n  drawer\n:END:\n* Hello\n** Good-bye\n*** Salut\n* Table of Contents                                                     :TOC_3_org:\n - [[About][About]]\n - [[Hello][Hello]]\n   - [[Good-bye][Good-bye]]\n     - [[Salut][Salut]]\n")))

;; Local Variables:
;; compile-command: "emacs -batch -l ert -l *.el -f ert-run-tests-batch-and-exit && emacs -batch -f batch-byte-compile *.el 2>&1 | sed -n '/Warning\|Error/p' | xargs -r ls"
;; End:

(provide 'toc-org)
;;; toc-org.el ends here
