;;; pdf-dev.el --- Mother's little development helper  -*- lexical-binding: t; -*-

;; Copyright (C) 2015  Andreas Politz

;; Author: Andreas Politz <politza@hochschule-trier.de>
;; Keywords:

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
;;
;; This file is only meant for developers.  The entry point is
;; pdf-dev-minor-mode, which see.

;;; Code:

(defvar pdf-dev-root-directory
  (file-name-directory
   (directory-file-name
    (file-name-directory load-file-name))))

(defun pdf-dev-reload ()
  "Reload Lisp files from source."
  (interactive)
  (let ((default-directory (expand-file-name
                            "lisp"
                            pdf-dev-root-directory))
        loaded)
    (dolist (file (directory-files default-directory nil "\\`pdf-\\w*\\.el\\'"))
      (push file loaded)
      (load-file file))
    (message "Loaded %s" (mapconcat 'identity loaded " "))))

(define-minor-mode pdf-dev-minor-mode
  "Make developing pdf-tools easier.

It does the following:

Quits the server and sets `pdf-info-epdfinfo-program' to
../server/epdfinfo.

Installs a `compilation-finish-functions' which will restart
epdfinfo after a successful recompilation.

Sets up `load-path' and reloads all PDF Tools Lisp files."
  :group 'pdf-dev
  (let ((lisp-dir (expand-file-name "lisp" pdf-dev-root-directory)))
    (setq load-path (remove lisp-dir load-path))
    (cond
     (pdf-dev-minor-mode
      (add-hook 'compilation-finish-functions 'pdf-dev-compilation-finished)
      (add-to-list 'load-path lisp-dir)
      (setq pdf-info-epdfinfo-program
            (expand-file-name
             "epdfinfo"
             (expand-file-name "server" pdf-dev-root-directory)))
      (pdf-info-quit)
      (pdf-dev-reload))
     (t
      (remove-hook 'compilation-finish-functions 'pdf-dev-compilation-finished)))))

(defun pdf-dev-compilation-finished (buffer status)
  "Restart the epdfinfo server.

BUFFER is the PDF buffer and STATUS is the compilation status of
building epdfinfo."
  (with-current-buffer buffer
    (when (and (equal status "finished\n")
               (file-equal-p
                (expand-file-name "server" pdf-dev-root-directory)
                default-directory))
      (message "Restarting epdfinfo server")
      (pdf-info-quit)
      (let ((pdf-info-restart-process-p t))
        (pdf-info-process-assert-running)))))

(provide 'pdf-dev)
;;; pdf-dev.el ends here
