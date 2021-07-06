;;; core-load-paths.el --- Spacemacs Core File  -*- no-byte-compile: t; lexical-binding: t -*-
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
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
  (expand-file-name user-emacs-directory)
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

(defconst spacemacs-purple-heart-png
  (concat spacemacs-banner-directory "img/heart.png")
  "Purple heart emoji.")

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

(setq pcache-directory (concat spacemacs-cache-directory "pcache/"))

;;;; Load Paths
;; TODO: Since these functions are not called anywhere, consider to inline them (Apr 27 2021 Lucius)

(defun spacemacs//add-to-load-path (dir)
  "Prepend DIR to `load-path'."
  (add-to-list 'load-path dir))

;; FIXME: unused function (Apr 25 2021 Lucius)
(defun spacemacs//add-to-load-path-if-exists (dir)
  "If DIR exists in the file system, prepend it to `load-path'."
  (when (file-exists-p dir)
    (spacemacs//add-to-load-path dir)))

(dolist (suffix '(nil "libs/" "libs/spacemacs-theme/" "libs/forks"))
  (spacemacs//add-to-load-path (concat spacemacs-core-directory suffix)))

;;;; Themes
(add-to-list 'custom-theme-load-path (concat spacemacs-core-directory
                                             "libs/spacemacs-theme/"))

(provide 'core-load-paths)
;;; core-load-paths.el ends here
