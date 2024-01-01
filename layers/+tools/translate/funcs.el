;;; funcs.el --- Semantic Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
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

(when (and (configuration-layer/package-used-p 'translate-mode)
           (configuration-layer/package-used-p 'go-translate))

  (defun translate/translate-current-reference-paragraph ()
    "Show all available translations of the reference paragraph at point in a pop-up frame."
    (interactive)
    (gts-translate translate//paragraph-translator))

  (defun translate/translate-word-at-point ()
    "Pop-up translations of the word at point."
    (interactive)
    (gts-translate translate//word-translator))

  (defun translate//set-translate-mode-paragraph-functions ()
    (cond ((eq major-mode 'markdown-mode)
           (setq translate-forward-paragraph-function 'markdown-forward-paragraph
                 translate-backward-paragraph-function 'markdown-backward-paragraph))
          ((eq major-mode 'org-mode)
           (setq translate-forward-paragraph-function 'org-forward-paragraph
                 translate-backward-paragraph-function 'org-backward-paragraph)))))
