;;; core-load-paths.el --- Spacemacs Core File  -*- no-byte-compile: t; lexical-binding: t -*-
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;;    Define various PATH variables, and set up load path.

;;; Code:

;;;; PATH variables/constants

(defconst user-home-directory
  (expand-file-name "~/")
  "User home directory (default ~/).")

;; ~/.emacs.d
(defvar spacemacs-start-directory
  (concat (file-name-directory (or load-file-name buffer-file-name)) "../")
  "Spacemacs start directory.")

;; ~/.emacs.d/assets
(defconst spacemacs-assets-directory
  (concat spacemacs-start-directory "assets/")
  "Spacemacs assets directory.")

;; ~/.emacs.d/core
(defconst spacemacs-core-directory
  (concat spacemacs-start-directory "core/")
  "Spacemacs core directory.")

;; ~/.emacs.d/core/banners
(defconst spacemacs-banner-directory
  (concat spacemacs-core-directory "banners/")
  "Spacemacs banners directory.")

(defconst spacemacs-banner-official-png
  (concat spacemacs-banner-directory "img/spacemacs.png")
  "Spacemacs official banner image.")

(defconst spacemacs-badge-official-png
  (concat spacemacs-banner-directory "img/spacemacs-badge.png")
  "Spacemacs official badge image.")

(defconst spacemacs-gplv3-official-png
  (concat spacemacs-banner-directory "img/gplv3.png")
  "GPLv3 official badge image.")

;; ~/.emacs.d/core/info
(defconst spacemacs-info-directory
  (concat spacemacs-core-directory "info/")
  "Spacemacs info files directory.")

;; ~/.emacs.d/core/info/release-notes
(defconst spacemacs-release-notes-directory
  (concat spacemacs-info-directory "release-notes/")
  "Spacemacs release notes directory.")

;; ~/.emacs.d/doc
(defconst spacemacs-docs-directory
  (concat spacemacs-start-directory "doc/")
  "Spacemacs documentation directory.")

;; ~/.emacs.d/news
(defconst spacemacs-news-directory
  (concat spacemacs-start-directory "news/")
  "Spacemacs News directory.")

;; ~/.emacs.d/private
(defconst spacemacs-private-directory
  (concat spacemacs-start-directory "private/")
  "Spacemacs private directory.")

;; ~/.emacs.d/tests
(defconst spacemacs-test-directory
  (concat spacemacs-start-directory "tests/")
  "Spacemacs tests directory.")

;; ~/.emacs.d/.cache
(defconst spacemacs-cache-directory
  (concat user-emacs-directory ".cache/")
  "Spacemacs storage area for persistent files.")

;; ~/.emacs.d/.cache/auto-save
(defconst spacemacs-auto-save-directory
  (concat spacemacs-cache-directory "auto-save/")
  "Spacemacs auto-save directory.")


;;;; Setup cache directories

;; TODO: Should also catch any IO error such as permission error (Apr 25 2021 Lucius)
(unless (file-exists-p spacemacs-cache-directory)
  (make-directory spacemacs-cache-directory))

;;;; Load Paths
(dolist (subdirectory '(nil "libs/" "libs/spacemacs-theme/" "libs/forks/"))
  (let ((path (concat spacemacs-core-directory subdirectory)))
    (if (file-exists-p path)
       (add-to-list 'load-path path)
     (error "The directory %s does not exist and cannot be added to the `load-path'." path))))

;;;; Themes
(add-to-list 'custom-theme-load-path (concat spacemacs-core-directory
                                             "libs/spacemacs-theme/"))

(provide 'core-load-paths)
;;; core-load-paths.el ends here
