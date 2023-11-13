;;; hydra-ox.el --- Org mode export widget implemented in Hydra

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Oleh Krehel

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This shows how a complex dispatch menu can be built with Hydra.

;;; Code:

(require 'hydra)
(require 'org)
(declare-function org-html-export-as-html 'ox-html)
(declare-function org-html-export-to-html 'ox-html)
(declare-function org-latex-export-as-latex 'ox-latex)
(declare-function org-latex-export-to-latex 'ox-latex)
(declare-function org-latex-export-to-pdf 'ox-latex)
(declare-function org-ascii-export-as-ascii 'ox-ascii)
(declare-function org-ascii-export-to-ascii 'ox-ascii)

(defhydradio hydra-ox ()
  (body-only "Export only the body.")
  (export-scope "Export scope." [buffer subtree])
  (async-export "When non-nil, export async.")
  (visible-only "When non-nil, export visible only")
  (force-publishing "Toggle force publishing"))

(defhydra hydra-ox-html (:color blue)
  "ox-html"
  ("H" (org-html-export-as-html
        hydra-ox/async-export
        (eq hydra-ox/export-scope 'subtree)
        hydra-ox/visible-only
        hydra-ox/body-only)
       "As HTML buffer")
  ("h" (org-html-export-to-html
        hydra-ox/async-export
        (eq hydra-ox/export-scope 'subtree)
        hydra-ox/visible-only
        hydra-ox/body-only) "As HTML file")
  ("o" (org-open-file
        (org-html-export-to-html
         hydra-ox/async-export
         (eq hydra-ox/export-scope 'subtree)
         hydra-ox/visible-only
         hydra-ox/body-only)) "As HTML file and open")
  ("b" hydra-ox/body "back")
  ("q" nil "quit"))

(defhydra hydra-ox-latex (:color blue)
  "ox-latex"
  ("L" org-latex-export-as-latex "As LaTeX buffer")
  ("l" org-latex-export-to-latex "As LaTeX file")
  ("p" org-latex-export-to-pdf "As PDF file")
  ("o" (org-open-file (org-latex-export-to-pdf)) "As PDF file and open")
  ("b" hydra-ox/body "back")
  ("q" nil "quit"))

(defhydra hydra-ox-text (:color blue)
  "ox-text"
  ("A" (org-ascii-export-as-ascii
        nil nil nil nil
        '(:ascii-charset ascii))
       "As ASCII buffer")

  ("a" (org-ascii-export-to-ascii
        nil nil nil nil
        '(:ascii-charset ascii))
       "As ASCII file")
  ("L" (org-ascii-export-as-ascii
        nil nil nil nil
        '(:ascii-charset latin1))
       "As Latin1 buffer")
  ("l" (org-ascii-export-to-ascii
        nil nil nil nil
        '(:ascii-charset latin1))
       "As Latin1 file")
  ("U" (org-ascii-export-as-ascii
        nil nil nil nil
        '(:ascii-charset utf-8))
       "As UTF-8 buffer")
  ("u" (org-ascii-export-to-ascii
        nil nil nil nil
        '(:ascii-charset utf-8))
       "As UTF-8 file")
  ("b" hydra-ox/body "back")
  ("q" nil "quit"))

(defhydra hydra-ox ()
  "
_C-b_ Body only:    % -15`hydra-ox/body-only^^^ _C-v_ Visible only:     %`hydra-ox/visible-only
_C-s_ Export scope: % -15`hydra-ox/export-scope _C-f_ Force publishing: %`hydra-ox/force-publishing
_C-a_ Async export: %`hydra-ox/async-export

"
  ("C-b" (hydra-ox/body-only) nil)
  ("C-v" (hydra-ox/visible-only) nil)
  ("C-s" (hydra-ox/export-scope) nil)
  ("C-f" (hydra-ox/force-publishing) nil)
  ("C-a" (hydra-ox/async-export) nil)
  ("h" hydra-ox-html/body "Export to HTML" :exit t)
  ("l" hydra-ox-latex/body "Export to LaTeX" :exit t)
  ("t" hydra-ox-text/body "Export to Plain Text" :exit t)
  ("q" nil "quit"))

(define-key org-mode-map (kbd "C-c C-,") 'hydra-ox/body)

(provide 'hydra-ox)

;;; hydra-ox.el ends here
