;;; packages.el --- Translate Layer packages File for Spacemacs  -*- lexical-binding: nil; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: Ray Wang <rayw.public@gmail.com>
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


(defconst translate-packages
  '(translate-mode
    go-translate))

(defun translate/init-translate-mode ()
  "Initialize required packages."
  (use-package translate-mode
    :defer t
    :hook (translate-mode . translate//set-translate-mode-paragraph-functions)))

(defun translate/init-go-translate ()
  (use-package go-translate
    :commands (gt-start)
    :config
    (defun translate//reference-paragraph-texter ()
      (let ((text (translate-get-reference-paragraph-text-at-point)))
        (when (= 0 (length (if text (string-trim text) "")))
          (user-error "Make sure there is any word at point, or selection exists"))
        text))
    (defun translate//check-and-get-render (render)
      (if (equal render 'posframe)
          (if (featurep 'posframe)
              (gt-posframe-pop-render)
            (display-warning 'translate "Missing package `posframe', back to use default `gt-buffer-render'.")
            (gt-buffer-render))
        (gt-buffer-render)))
    (defconst translate//paragraph-translator
      (gt-translator
       :taker (gt-taker :text (lambda () (translate//reference-paragraph-texter)))
       :engines (list (gt-google-engine) (gt-bing-engine))
       :render (translate//check-and-get-render translate/paragraph-render))
      "Paragraph translator for `go-translate'.")
    (defconst translate//word-translator
      (gt-translator
       :taker (gt-taker :text 'word)
       :engines (list (gt-google-engine) (gt-bing-engine))
       :render (translate//check-and-get-render translate/word-render))
      "Word translator for `go-translate'.")))

(defun translate/pre-init-posframe ()
  (spacemacs|use-package-add-hook posframe
    :post-config (translate/init-go-translate)))
