;;; funcs.el --- DjVu Layer functions File for Spacemacs
;;
;; Copyright (c) 2021 Sylvain Benner & Contributors
;;
;; Author: Daniel Nicolai <dalanicolai@gmail.com>
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


(defun spacemacs/djvu-advise-image-toggle (_file &rest args)
  (djvu-image-toggle))

;; djvu-continuous of djvu.el does not work with djvu3.el
(defun spacemacs/djvu-scroll-up-or-next-page ()
  (interactive)
  (if (not djvu-semi-continuous-scrolling)
      (if djvu-image-mode
          (djvu-image-scroll-up)
        (evil-next-visual-line))
    (scroll-up-line 5)
    (when (= (window-vscroll) 0)
      (djvu-next-page 1))))

(defun spacemacs/djvu-scroll-down-or-previous-page ()
  (interactive)
  (if (not djvu-semi-continuous-scrolling)
      (if djvu-image-mode
          (djvu-image-scroll-down)
        (evil-previous-visual-line))
    (if (/= (window-vscroll) 0)
        (scroll-down-line 5)
      (djvu-prev-page 1)
      (scroll-up-command))))

(defun spacemacs/djvu-fast-search (regexp)
  (interactive "sSearch (regexp): ")
  (when djvu-image-mode
    (djvu-image-toggle))
  (spacemacs/djvu-search-forward regexp))

(defun spacemacs/djvu-search-forward (query)
  "Search forward for match for REGEXP.
Search case-sensitivity is determined by the value of the variable
`case-fold-search', see Info node `(elisp)Searching and Case'.
Use the command `djvu-search-forward-continue' to continue the search."
  (interactive "sQuery: ")
  (setq djvu-last-search-re query)
  (unless (eq (djvu-ref page) (djvu-ref pagemax))
    (search-forward query nil t))
  (djvu-goto-page (let ((page (djvu-ref page))
                        (return 1))
                    (while (and (/= return 0) (< page (+ (djvu-ref pagemax) 1)))
                      (setq page (1+ page))
                      (setq return (call-process-shell-command
                                    (format "djvused %s -e 'select %s; print-pure-txt' | grep -i '%s'"
                                            (shell-quote-argument buffer-file-name)
                                            page
                                            query))))
                    page))
  (search-forward query nil t))

(defun spacemacs/djvu-re-search-forward-continue ()
  "Continue search forward for match for `djvu-last-search-re'."
  (interactive)
  (spacemacs/djvu-search-forward djvu-last-search-re))

(defun spacemacs//djvu-set-imenu-create-index-function ()
  (setq imenu-create-index-function #'djvu-imenu-create-index))

(defun spacemacs//djvu-set-imenu-goto-function ()
  (setq imenu-default-goto-function (lambda (title page)
                                      (djvu-goto-page page djvu-doc))))

(defun spacemacs/djvu-toggle-semi-continuous-scrolling ()
  (interactive)
  (setq djvu-semi-continuous-scrolling (not djvu-semi-continuous-scrolling))
  (message "Djvu alternative scrolling %s" (if djvu-semi-continuous-scrolling
                                               "enabled"
                                             "disabled")))

(defun spacemacs/djvu-occur-next-entry-and-follow ()
  (interactive)
  (evil-next-visual-line)
  (call-interactively 'djvu-occur-show-entry))

(defun spacemacs/djvu-occur-previous-entry-and-follow ()
  (interactive)
  (evil-previous-visual-line)
  (call-interactively 'djvu-occur-show-entry))
