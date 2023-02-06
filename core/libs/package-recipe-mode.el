;;; package-recipe-mode.el --- Major-mode for editing package recipes  -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2011-2023 Donald Ephraim Curtis
;; Copyright (C) 2012-2023 Steve Purcell
;; Copyright (C) 2016-2023 Jonas Bernoulli
;; Copyright (C) 2009 Phil Hagelberg

;; Author: Donald Ephraim Curtis <dcurtis@milkbox.net>
;; Homepage: https://github.com/melpa/package-build
;; Keywords: maint tools

;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This library defines the major-mode `package-recipe-mode', which is
;; used for Melpa package recipe files.

;;; Code:

(require 'package-build)

;;;###autoload
(defvar package-recipe-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'package-build-current-recipe)
    (define-key map (kbd "C-c C-n") 'package-build-create-recipe)
    map)
  "Keymap for `package-recipe-mode'.")

;;;###autoload
(if (fboundp 'lisp-data-mode) ; Since Emacs 28.1.
    (define-derived-mode package-recipe-mode lisp-data-mode "Melpa-Recipe"
      "Major mode for buffers holding Melpa package recipes."
      :group 'package-build
      (package-recipe-mode--enable))
  (define-derived-mode package-recipe-mode emacs-lisp-mode "Melpa-Recipe"
    "Major mode for buffers holding Melpa package recipes."
    :group 'package-build
    (package-recipe-mode--enable)))

(defun package-recipe-mode--enable ()
  (setq-local package-build-recipes-dir default-directory)
  (setq-local package-build-working-dir (expand-file-name "../working/"))
  (setq-local package-build-archive-dir (expand-file-name "../packages/"))
  (setq-local flycheck-checkers nil)
  (setq-local indent-tabs-mode nil)
  (setq-local require-final-newline t)
  (add-hook 'before-save-hook #'whitespace-cleanup)
  (message "%s" (substitute-command-keys "\
Use \\[package-build-current-recipe] to build this recipe, \
\\[package-build-create-recipe] to create a new recipe")))

;;;###autoload
(defun package-build-create-recipe (name fetcher)
  "Create a new recipe for the package named NAME using FETCHER."
  (interactive
   (list (read-string "Package name: ")
         (intern (completing-read "Fetcher: " package-recipe--fetchers
                                  nil t nil nil "github"))))
  (let ((recipe-file (expand-file-name name package-build-recipes-dir)))
    (when (file-exists-p recipe-file)
      (error "Recipe already exists"))
    (with-current-buffer (find-file recipe-file)
      (save-excursion
        (insert (format "(%s\n" name)
                (format " :fetcher %s\n" fetcher)
                (if (memq fetcher package-recipe--forge-fetchers)
                    " :repo \"USER/REPO\")\n"
                  " :url \"https://TODO\")\n"))))))

;;;###autoload
(defun package-build-current-recipe ()
  "Build archive for the recipe defined in the current buffer."
  (interactive)
  (unless (and (buffer-file-name)
               (file-equal-p (file-name-directory (buffer-file-name))
                             package-build-recipes-dir))
    (error "Buffer is not visiting a recipe"))
  (when (buffer-modified-p)
    (if (y-or-n-p (format "Save file %s? " buffer-file-name))
        (save-buffer)
      (error "Aborting")))
  (check-parens)
  (let ((name (file-name-nondirectory (buffer-file-name))))
    (package-build-archive name t)
    (let ((entry (assq (intern name) (package-build-archive-alist)))
          (output-buffer-name "*package-build-archive-entry*"))
      (with-output-to-temp-buffer output-buffer-name
        (princ ";; Please check the following package descriptor.\n")
        (princ ";; If the correct package description or dependencies are missing,\n")
        (princ ";; then the source .el file is likely malformed, and should be fixed.\n")
        (pp entry))
      (with-current-buffer output-buffer-name
        (if (fboundp 'lisp-data-mode) (lisp-data-mode) (emacs-lisp-mode))
        (view-mode))
      (when (y-or-n-p "Install new package? ")
        (package-install-file (package-build--artifact-file entry))
        (pop-to-buffer (get-buffer byte-compile-log-buffer))))))

(provide 'package-recipe-mode)
;;; package-recipe-mode.el ends here
