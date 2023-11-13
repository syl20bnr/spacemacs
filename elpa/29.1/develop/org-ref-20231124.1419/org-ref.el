;;; org-ref.el --- citations, cross-references and bibliographies in org-mode -*- lexical-binding: t; -*-

;; Copyright(C) 2014-2021 John Kitchin

;; Author: John Kitchin <jkitchin@andrew.cmu.edu>
;; URL: https://github.com/jkitchin/org-ref
;; Version: 3.0
;; Keywords: org-mode, cite, ref, label
;; Package-Requires: ((org "9.4") (dash "0") (s "0") (f "0") (htmlize "0") (hydra "0") (avy "0") (parsebib "0") (bibtex-completion "0") (citeproc "0") (ox-pandoc "0"))
;; This file is not currently part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program ; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Lisp code to setup bibliography, cite, ref and label org-mode links. The
;; links are clickable and do things that are useful.
;;
;; This uses vanilla completing-read in Emacs. You can customize by requiring
;; `org-ref-helm' or `org-ref-ivy' after this.

;; You should really read org-ref.org in this package for details.
;;

;;; Code:

(require 'org-ref-core)


(setq org-ref-insert-link-function 'org-ref-insert-link
      org-ref-insert-cite-function 'org-ref-insert-cite-link
      org-ref-insert-label-function 'org-ref-insert-label-link
      org-ref-insert-ref-function 'org-ref-insert-ref-link
      org-ref-cite-onclick-function (lambda (_) (org-ref-citation-hydra/body)))


;;* The end
(provide 'org-ref)

;;; org-ref.el ends here
