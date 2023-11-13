;;; pdf-macs.el --- Macros for pdf-tools. -*- lexical-binding:t -*-

;; Copyright (C) 2013  Andreas Politz

;; Author: Andreas Politz <politza@fh-trier.de>
;; Keywords: files, doc-view, pdf

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
;;; Code:
;;

(defmacro pdf-view-current-page (&optional window)
  ;;TODO: write documentation!
  `(image-mode-window-get 'page ,window))

(defmacro pdf-view-current-overlay (&optional window)
  ;;TODO: write documentation!
  `(image-mode-window-get 'overlay ,window))

(defmacro pdf-view-current-image (&optional window)
  ;;TODO: write documentation!
  `(image-mode-window-get 'image ,window))

(defmacro pdf-view-current-slice (&optional window)
  ;;TODO: write documentation!
  `(image-mode-window-get 'slice ,window))

(defmacro pdf-view-current-window-size (&optional window)
  ;;TODO: write documentation!
  `(image-mode-window-get 'window-size ,window))

(defmacro pdf-view-window-needs-redisplay (&optional window)
  `(image-mode-window-get 'needs-redisplay ,window))

(provide 'pdf-macs)

;;; pdf-macs.el ends here
