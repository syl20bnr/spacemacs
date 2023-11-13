;;; ob-eukleides.el --- Org-babel functions for eukleides evaluation

;; Copyright (C) 2010-2021  Free Software Foundation, Inc.

;; Author: Luis Anaya
;; Keywords: literate programming, reproducible research
;; Homepage: https://git.sr.ht/~bzg/org-contrib

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for evaluating eukleides script.
;;
;; Inspired by Ian Yang's org-export-blocks-format-eukleides
;; https://www.emacswiki.org/emacs/org-export-blocks-format-eukleides.el

;;; Requirements:

;; eukleides     | http://eukleides.org
;; eukleides     | `org-eukleides-path' should point to the eukleides executablexs

;;; Code:
(require 'ob)
(require 'ob-eval)

(defvar org-babel-default-header-args:eukleides
  '((:results . "file") (:exports . "results"))
  "Default arguments for evaluating a eukleides source block.")

(defcustom org-eukleides-path nil
  "Path to the eukleides executable file."
  :group 'org-babel
  :type 'string)

(defcustom org-eukleides-eps-to-raster nil
  "Command used to convert EPS to raster. Nil for no conversion."
  :group 'org-babel
  :type '(choice
         (repeat :tag "Shell Command Sequence" (string :tag "Shell Command"))
         (const :tag "sam2p" "a=%s;b=%s;sam2p ${a} ${b}" )
         (const :tag "NetPNM"  "a=%s;b=%s;pstopnm -stdout ${a} | pnmtopng  > ${b}" )
         (const :tag "None" nil)))

(defun org-babel-execute:eukleides (body params)
  "Execute a block of eukleides code with org-babel.
This function is called by `org-babel-execute-src-block'."
  (let* ((result-params (split-string (or (cdr (assq :results params)) "")))
	 (out-file (or (cdr (assq :file params))
		       (error "Eukleides requires a \":file\" header argument")))
	 (cmdline (cdr (assq :cmdline params)))
	 (in-file (org-babel-temp-file "eukleides-"))
	 (java (or (cdr (assq :java params)) ""))
	 (cmd (if (not org-eukleides-path)
		  (error "`org-eukleides-path' is not set")
		(concat (expand-file-name org-eukleides-path)
                " -b --output="
                (org-babel-process-file-name
                 (concat
                  (file-name-sans-extension out-file) ".eps"))
                " "
                (org-babel-process-file-name in-file)))))
    (unless (file-exists-p org-eukleides-path)
      (error "Could not find eukleides at %s" org-eukleides-path))

    (if (string= (file-name-extension out-file) "png")
        (if org-eukleides-eps-to-raster
            (shell-command (format org-eukleides-eps-to-raster
                                    (concat (file-name-sans-extension out-file) ".eps")
                                    (concat (file-name-sans-extension out-file) ".png")))
          (error "Conversion to PNG not supported.  Use a file with an EPS name")))

    (with-temp-file in-file (insert body))
    (message "%s" cmd) (org-babel-eval cmd "")
    nil)) ;; signal that output has already been written to file

(defun org-babel-prep-session:eukleides (session params)
  "Return an error because eukleides does not support sessions."
  (error "Eukleides does not support sessions"))

(provide 'ob-eukleides)



;;; ob-eukleides.el ends here
