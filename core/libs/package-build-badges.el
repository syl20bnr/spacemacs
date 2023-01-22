;;; package-build-badges.el --- Create batches for packages  -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2011-2023 Donald Ephraim Curtis
;; Copyright (C) 2012-2023 Steve Purcell
;; Copyright (C) 2018-2023 Jonas Bernoulli
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

;; In future we should provide a hook.  Note also that it would be
;; straightforward to generate the SVG ourselves, which would save
;; the network overhead.

;;; Code:

(defvar package-build-stable)

(defun package-build--write-melpa-badge-image (name version target-dir)
  (unless (zerop (call-process
                  "curl" nil nil nil "-f" "-o"
                  (expand-file-name (concat name "-badge.svg") target-dir)
                  (format "https://img.shields.io/badge/%s-%s-%s.svg"
                          (if package-build-stable "melpa stable" "melpa")
                          (url-hexify-string version)
                          (if package-build-stable "3e999f" "922793"))))
    (message "Failed to fetch badge")))

(provide 'package-build-badges)
;;; package-badges.el ends here
