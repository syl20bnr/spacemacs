;;; devdocs.el --- Launch DevDocs search  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Chunyang Xu

;; Author: Chunyang Xu <xuchunyang.me@gmail.com>
;; URL: https://github.com/xuchunyang/DevDocs.el
;; Version: 0.02

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; DevDocs <http://devdocs.io/> is API Documentation Browser.  This
;; package allowing you to easily search the DevDocs Documentation.

;; To use, type M-x devdocs-search

;;; Code:

(defgroup devdocs nil
  "Searching in DevDocs."
  :group 'external)

(defcustom devdocs-url "https://devdocs.io"
  "DevDocs URL.

Don't change this unless you setup your own DevDocs locally."
  :type 'string
  :group 'devdocs)

(defcustom devdocs-alist
  '((c-mode           . "c")
    (c++-mode         . "c++")
    (clojure-mode     . "clojure")
    (coffee-mode      . "CoffeeScript")
    (common-lisp-mode . "lisp")
    (cperl-mode       . "perl")
    (css-mode         . "css")
    (elixir-mode      . "elixir")
    (enh-ruby-mode    . "ruby")
    (erlang-mode      . "erlang")
    (gfm-mode         . "markdown")
    (go-mode          . "go")
    (groovy-mode      . "groovy")
    (haskell-mode     . "haskell")
    (html-mode        . "html")
    (java-mode        . "java")
    (js2-mode         . "javascript")
    (js3-mode         . "javascript")
    (less-css-mode    . "less")
    (lua-mode         . "lua")
    (markdown-mode    . "markdown")
    (perl-mode        . "perl")
    (php-mode         . "php")
    (processing-mode  . "processing")
    (puppet-mode      . "puppet")
    (python-mode      . "python")
    (ruby-mode        . "ruby")
    (sass-mode        . "sass")
    (scala-mode       . "scala")
    (tcl-mode         . "tcl"))
  "Alist which maps major modes to names of DevDocs documentations."
  :type '(repeat (cons (symbol :tag "Major mode")
                       (string :tag "DevDocs documentation")))
  :group 'devdocs)

(defcustom devdocs-build-search-pattern-function
  'devdocs-build-search-pattern-function
  "A function to be called by `devdocs-search'.
It builds search pattern base on some context."
  :type 'function
  :group 'devdocs)


(defun devdocs-build-search-pattern-function ()
  "Build search pattern base on region/symbol-at-point and major-mode."
  (let ((documentation (cdr (assoc major-mode devdocs-alist)))
        (query (if (use-region-p)
                   (buffer-substring (region-beginning) (region-end))
                 (thing-at-point 'symbol))))
    (if documentation
        (concat documentation " " query)
      ;; Make sure we return a string
      (or query ""))))

(defun devdocs-do-search (pattern)
  (browse-url
   (format "%s/#q=%s" devdocs-url (url-hexify-string pattern))))

(defvar devdocs-search-history '() "Search History.")

;;;###autoload
(defun devdocs-search (&optional confirm)
  "Launch Devdocs search.
CONFIRM goes with asking for confirmation."
  (interactive "P")
  (let ((pattern (funcall devdocs-build-search-pattern-function)))
    (when confirm
      (setq pattern (read-string "Searching DevDocs: " pattern)))
    (devdocs-do-search pattern)
    (unless (string= "" pattern)
      (add-to-list 'devdocs-search-history pattern))))

(provide 'devdocs)
;;; devdocs.el ends here
