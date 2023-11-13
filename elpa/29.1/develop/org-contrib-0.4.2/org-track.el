;;; org-track.el --- Track the most recent Org-mode version available.
;;
;; Copyright (C) 2009-2021 Free Software Foundation, Inc.
;;
;; Author: Bastien Guerry <bzg@gnu.org>
;;         Eric S Fraga   <e.fraga at ucl.ac dot uk>
;;         Sebastian Rose <sebastian_rose at gmx dot de>
;;         The Worg people https://orgmode.org/worg/
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: https://git.sr.ht/~bzg/org-contrib
;; Version: 6.29a
;;
;; Released under the GNU General Public License version 3
;; see: https://www.gnu.org/licenses/gpl-3.0.html
;;
;; This file is not part of GNU Emacs.
;;
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
;;
;;; Commentary:
;;
;; WARNING: This library is obsolete, you should use the make targets
;; to keep track of Org latest developments.
;;
;; Download the latest development tarball, unpack and optionally compile it
;;
;; Usage:
;;
;;   (require 'org-track)
;;
;;   ;; ... somewhere in your setup (use customize):
;;
;;   (setq org-track-directory "~/test/")
;;   (setq org-track-compile-sources nil)
;;   (setq org-track-remove-package t)
;;
;;   M-x org-track-update RET

(require 'url-parse)
(require 'url-handlers)
(autoload 'url-file-local-copy "url-handlers")
(autoload 'url-generic-parse-url "url-parse")



;;; Variables:

(defgroup org-track nil
  "Track the most recent Org-mode version available.

To use org-track, adjust `org-track-directory'.
Org will download the archived latest git version for you,
unpack it into that directory (i.e. a subdirectory
`org-mode/' is added), create the autoloads file
`org-loaddefs.el' for you and, optionally, compile the
sources.
All you'll have to do is call `M-x org-track-update' from
time to time."
  :group 'org)

(defcustom org-track-directory (concat user-emacs-directory "org/lisp")
  "Directory where your org-mode/ directory lives.
If that directory does not exist, it will be created."
  :type 'directory)

(defcustom org-track-compile-sources t
  "If `nil', never compile org-sources.
Org will only create the autoloads file `org-loaddefs.el' for
you then. If `t', compile the sources, too.
Note, that emacs preferes compiled elisp files over
non-compiled ones."
  :type 'boolean)

(defcustom org-track-org-url "https://orgmode.org/"
  "The URL where the package to download can be found.
Please append a slash."
  :type 'string)

(defcustom org-track-org-package "org-latest.tar.gz"
  "The basename of the package you use.
Defaults to the development version of Org-mode.
This should be a *.tar.gz package, since emacs provides all
you need to unpack it."
  :type 'string)

(defcustom org-track-remove-package nil
  "Remove org-latest.tar.gz after updates?"
  :type 'boolean)



;;; Frontend

(defun org-track-update ()
  "Update to current Org-mode version.
Also, generate autoloads and evtl. compile the sources."
  (interactive)
  (let* ((base (file-truename org-track-directory))
         (org-exists (file-exists-p
                      (file-truename
                       (concat base "/org-mode/lisp/org.el"))))
         (nobase (not (file-directory-p
                       (file-truename org-track-directory)))))
    (if nobase
        (when (y-or-n-p
               (format "Directory %s does not exist. Create it?" base))
          (make-directory base t)
          (setq nobase nil)))
    (if nobase
        (message "Not creating %s - giving up." org-track-directory)
      (condition-case err
          (progn
            (org-track-fetch-package)
            (org-track-compile-org))
        (error (message "%s" (error-message-string err)))))))



;;; tar related functions

;; `url-retrieve-synchronously' fetches files synchronously. How can we ensure
;; that? If the maintainers of that package decide, that an assynchronous
;; download might be better??? (used by `url-file-local-copy')

;;;###autoload
(defun org-track-fetch-package (&optional directory)
  "Fetch Org package depending on `org-track-fetch-package-extension'.
If DIRECTORY is defined, unpack the package there, i.e. add the
subdirectory org-mode/ to DIRECTORY."
  (interactive "Dorg-track directory: ")
  (let* ((pack (concat
                (if (string-match "/$" org-track-org-url)
                    org-track-org-url
                  (concat org-track-org-url "/"))
                org-track-org-package))
         (base (file-truename
                (or directory org-track-directory)))
         (target (file-truename
                  (concat base "/" org-track-org-package)))
         url download tarbuff)
    (message "Fetching to %s - this might take some time..."  base)
    (setq url (url-generic-parse-url pack))
    (setq download (url-file-local-copy url)) ;; errors if fail
    (copy-file download target t)
    (delete-file download)
    ;; (tar-mode) leads to dubious errors. We use the auto-mode-alist to
    ;; ensure tar-mode is used:
    (add-to-list 'auto-mode-alist '("org-latest\\.tar\\.gz\\'" . tar-mode))
    (setq tarbuff (find-file target))
    (with-current-buffer tarbuff ;; with-temp-buffer does not work with tar-mode??
      (tar-untar-buffer))
    (kill-buffer tarbuff)
    (if org-track-remove-package
        (delete-file target))))



;;; Compile Org-mode sources


;;;###autoload
(defun org-track-compile-org (&optional directory)
  "Compile all *.el files that come with org-mode.
Generate the autoloads file `org-loaddefs.el'.

DIRECTORY is where the directory org-mode/ lives (i.e. the
          parent directory of your local repo."
  (interactive)
  ;; file-truename expands the filename and removes double slash, if exists:
  (setq directory (file-truename
                   (concat
                    (or directory
                        (file-truename (concat org-track-directory "/org-mode/lisp")))
                    "/")))
  (add-to-list 'load-path directory)
  (let ((list-of-org-files (file-expand-wildcards (concat directory "*.el"))))
    ;; create the org-loaddefs file
    (require 'autoload)
    (setq esf/org-install-file (concat directory "org-loaddefs.el"))
    (find-file esf/org-install-file)
    (erase-buffer)
    (mapc (lambda (x)
            (generate-file-autoloads x))
          list-of-org-files)
    (insert "\n(provide (quote org-loaddefs))\n")
    (save-buffer)
    (kill-buffer)
    (byte-compile-file esf/org-install-file t)

    (mapc (lambda (f)
            (if (file-exists-p (concat f "c"))
                (delete-file (concat f "c"))))
          list-of-org-files)
    (if org-track-compile-sources
        (mapc (lambda (f) (byte-compile-file f)) list-of-org-files))))

(provide 'org-track)

;;; org-track.el ends here
