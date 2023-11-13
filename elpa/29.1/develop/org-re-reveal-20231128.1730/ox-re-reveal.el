;;; ox-re-reveal.el --- Loader for org-re-reveal  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2019 Jens Lechtenbörger

;;; Commentary:
;; Org export back-ends have file names starting with "ox-".
;; However, such files typically define variables and functions
;; starting with "org-", which causes errors by package-lint.  To
;; define variables and functions with the usual prefix "org-" while
;; avoiding errors by package-lint, code is located in
;; org-re-reveal.el.
;; However, the prefix "ox-" is hard-coded in org.el and used to load
;; back-ends in `org-export-backends'.  With this file, you can
;; customize `org-export-backends' and add `re-reveal'.  Then, when
;; pressing `C-c C-e', this file will be loaded, which loads
;; org-re-reveal.el.

;;; Code:
(require 'org-re-reveal)
(provide 'ox-re-reveal)
;;; ox-re-reveal.el ends here
