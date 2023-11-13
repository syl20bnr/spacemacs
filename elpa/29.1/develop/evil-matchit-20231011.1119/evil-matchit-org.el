;;; evil-matchit-org.el --- org-mode plugin of evil-matchit

;; Copyright (C) 2014-2021 Chen Bin

;; Author: Chen Bin

;; This file is not part of GNU Emacs.

;;; License:

;; This file is part of evil-matchit
;;
;; evil-matchit is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; evil-matchit is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.



;;; Commentary:
;; 

;;; Code:

;; OPTIONAL, you don't need SDK to write a plugin for evil-matchit
;; but SDK do make you write less code, isn't it?
;; All you need to do is just define the match-tags for SDK algorithm to lookup.
(require 'org)
(require 'evil-matchit-sdk)

(defvar evilmi-org-extract-keyword-howtos
  '(("^[ \t]*#\\+\\([a-zA-Z_]+\\).*$" 1)
    ("^[ \t]*\\:\\([a-zA-Z_]+\\)\\:$" 1))
  "How to extract keyword from current line.  Each howto is a pair.
First item of pair is regex to match current line.
Second is index of sub-match to extract keyword.
Sub-match is the match defined between '\\(' and '\\)' in regular expression.")

(defvar evilmi-org-match-tags
  '((("begin_src") () ("end_src") "MONOGAMY")
    (("begin_example") () ("end_example") "MONOGAMY")
    (("begin_html") () ("end_html") "MONOGAMY")
    (("begin_quote") () ("end_quote") "MONOGAMY")
    (("begin_export") () ("end_export") "MONOGAMY")
    (("results") () ("end") "MONOGAMY"))
  "Match tags in org file.")

(defun evilmi--element-property (property element)
  "Extract the value from the PROPERTY of an ELEMENT."
  (unless (stringp element)
    ;; `org-element-property' is not used because it's
    ;; available only in 24.4+
    (plist-get (nth 1 element) property)))

(defun evilmi--get-embedded-language-major-mode ()
  "Get major of embedded snippet."
  (let* ((lang (evilmi--element-property :language (org-element-at-point))))
    (when lang
      (cond
       ((string= lang "elisp")
        'emacs-lisp-mode)
       (t
        (intern (concat lang "-mode")))))))

;;;###autoload
(defun evilmi-org-get-tag ()
  "Get current tag in org file."
  (let* ((rlt (evilmi-sdk-get-tag evilmi-org-match-tags
                                  evilmi-org-extract-keyword-howtos)))
    ;; evilmi-org-jump knows what -1 means
    (unless rlt (setq rlt '(-1)))
    rlt))

(defvar evilmi-plugins)

;;;###autoload
(defun evilmi-org-jump (info num)
  "Jump to the matching tag using INFO and NUM."
  (cond
   ((< (car info) 0)
    (let* (ideal-dest
           jumped
           info
           (lang-f (evilmi--get-embedded-language-major-mode))
           (plugin (and lang-f (plist-get evilmi-plugins lang-f))))
      (when plugin
        (mapc
         (lambda (elem)
           (setq info (funcall (nth 0 elem)))
           (when (and info (not jumped))
             ;; before jump, we may need some operation
             (setq ideal-dest (funcall (nth 1 elem) info num))
             ;; jump only once if the jump is successful
             (setq jumped t)))
         plugin))))
   (t
    (evilmi-sdk-jump info
                     num
                     evilmi-org-match-tags
                     evilmi-org-extract-keyword-howtos))))

(provide 'evil-matchit-org)
;;; evil-matchit-org.el ends here
