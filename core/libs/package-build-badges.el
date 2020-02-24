;;; package-build-badges.el --- Create batches for packages

;; Copyright (C) 2011-2013 Donald Ephraim Curtis <dcurtis@milkbox.net>
;; Copyright (C) 2012-2014 Steve Purcell <steve@sanityinc.com>
;; Copyright (C) 2009 Phil Hagelberg <technomancy@gmail.com>

;; Author: Donald Ephraim Curtis <dcurtis@milkbox.net>
;; Keywords: tools

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; In future we should provide a hook.  Note also that it would be
;; straightforward to generate the SVG ourselves, which would save
;; the network overhead.

;;; Code:

(require 'package-build)

(defun package-build--write-melpa-badge-image (name version target-dir)
  (shell-command
   (mapconcat #'shell-quote-argument
              (list "curl" "-f" "-o"
                    (expand-file-name (concat name "-badge.svg") target-dir)
                    (format "https://img.shields.io/badge/%s-%s-%s.svg"
                            (if package-build-stable "melpa stable" "melpa")
                            (url-hexify-string version)
                            (if package-build-stable "3e999f" "922793")))
              " ")))

(provide 'package-build-badges)
;; End:
;;; package-badges.el ends here
