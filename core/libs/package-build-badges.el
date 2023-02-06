;;; package-build-badges.el --- Create batches for packages  -*- lexical-binding:t; coding:utf-8 -*-

;; Copyright (C) 2011-2023 Donald Ephraim Curtis
;; Copyright (C) 2012-2023 Steve Purcell
;; Copyright (C) 2018-2023 Jonas Bernoulli
;; Copyright (C) 2021-2023 Free Software Foundation, Inc
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

;; Create batches for packages.
;; The code in this file was lifted from `elpa-admin'.

;;; Code:

(defvar package-build-stable)

(defun package-build--write-melpa-badge-image ( name version target-dir
                                                &optional archive color)
  "Make badge svg file.
This is essentially a copy of `elpaa--make-badge'."
  (let* ((file (expand-file-name (concat name "-badge.svg") target-dir))
         (left (or archive (if package-build-stable "melpa stable" "melpa")))
         (right (url-hexify-string version))
         (color (or color (if package-build-stable "#3e999f" "#922793")))
         (lw (package-build-badge--string-width left))
         (rw (package-build-badge--string-width right))
         (pad (package-build-badge--string-width "x"))
         (width (/ (+ lw rw (* 4 pad)) 10))
         (offset -10) ;; Small alignment correction
         (ctx `((offset . ,offset)
                (left . ,left)
                (right . ,right)
                (lw . ,lw)
                (rw . ,rw)
                (width . ,width)
                (color . ,color)
                (pad . ,pad))))
    (with-temp-buffer
      (insert
       (replace-regexp-in-string
        "{\\([^}]+\\)}"
        (lambda (str)
          (url-insert-entities-in-string
           (format "%s" (eval (read (match-string 1 str)) ctx))))
        (eval-when-compile
          (replace-regexp-in-string
           "[ \t\n]+" " "
           (replace-regexp-in-string
            "'" "\""
            "<?xml version='1.0'?>
<svg xmlns='http://www.w3.org/2000/svg'
     xmlns:xlink='http://www.w3.org/1999/xlink'
     width='{width}'
     height='20'
     role='img'
     aria-label='{left}: {right}'>
  <title>{left}: {right}</title>
  <linearGradient id='s' x2='0' y2='100%'>
    <stop offset='0' stop-color='#bbb' stop-opacity='.1'/>
    <stop offset='1' stop-opacity='.1'/>
  </linearGradient>
  <clipPath id='r'>
    <rect width='{width}' height='20' rx='3' fill='#fff'/>
  </clipPath>
  <g clip-path='url(#r)'>
    <rect width='{(/ (+ lw (* 2 pad)) 10)}'
          height='20' fill='#555'/>
    <rect x='{(1- (/ (+ lw (* 2 pad)) 10))}'
          width='{width}' height='20' fill='{color}'/>
    <rect width='{width}' height='20' fill='url(#s)'/>
  </g>
  <g fill='#fff'
     text-anchor='middle'
     font-family='Verdana,Geneva,DejaVu Sans,sans-serif'
     font-size='110'
     text-rendering='geometricPrecision'>
    <text aria-hidden='true'
          x='{(+ (/ lw 2) pad offset)}'
          y='150'
          fill='#010101' fill-opacity='.3'
          transform='scale(.1)' textLength='{lw}'>{left}</text>
    <text x='{(+ (/ lw 2) pad offset)}'
          y='140' transform='scale(.1)'
          fill='#fff'
          textLength='{lw}'>{left}</text>
    <text aria-hidden='true'
          x='{(+ lw (/ rw 2) (* 3 pad) offset)}'
          y='150'
          fill='#010101'  fill-opacity='.3'
          transform='scale(.1)' textLength='{rw}'>{right}</text>
    <text x='{(+ lw (/ rw 2) (* 3 pad) offset)}'
          y='140'
          transform='scale(.1)'
          fill='#fff' textLength='{rw}'>{right}</text>
  </g>
</svg>")))))
      (write-region (point-min) (point-max) file))))

(defun package-build-badge--string-width (str)
  "Determine string width in pixels of STR."
  (with-temp-buffer
    ;; ImageMagick 7.1.0 or later requires using the "magick" driver,
    ;; rather than "convert" directly, but Debian doesn't provide it
    ;; yet (2021).
    (let ((args `(,@(if (executable-find "magick")
                        '("magick" "convert")
                      '("convert"))
                  "-debug" "annotate" "xc:" "-font" "DejaVu-Sans"
                  "-pointsize" "110" "-annotate" "0" ,str "null:")))
      (apply #'call-process (car args) nil t nil (delq nil (cdr args)))
      (goto-char (point-min))
      (if (not (re-search-forward "Metrics:.*?width: \\([0-9]+\\)"))
          (error "Could not determine string width")
        (let ((width (string-to-number (match-string 1))))
          ;; This test aims to catch the case where the font is missing,
          ;; but it seems it only works in some cases :-(
          (if (and (> (string-width str) 0) (not (> width 0)))
              (progn (message "convert:\n%s" (buffer-string))
                     (error "Could not determine string width"))
            width))))))

(provide 'package-build-badges)
;;; package-badges.el ends here
