;;; evil-collection-xwidget.el --- Evil bindings for Xwidget -*- lexical-binding: t -*-

;; Copyright (C) 2020, 2022 Ruslan Kamashev

;; Author: Ruslan Kamashev <rynffoll@gmail.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, xwidget, tools

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Evil bindings for Xwidget.

;;; Code:
(require 'xwidget)
(require 'evil-collection)

(defvar evil-collection-xwidget-maps '(xwidget-webkit-mode-map))

;; Silence compile errors
(declare-function xwidget-webkit-uri "xwidget")
(declare-function xwidget-window-inside-pixel-height "xwidget")

;; HACK: elisp byte-opt can't optimize 'format' out, so it's checked at runtime.
(defun evil-collection-xwidget-webkit--scroll-trampoline (f n)
  "Compatible wrapper for `xwidget-webkit-scroll-up' and so on.

F is the name of function, N is the pixel height."
  (let ((realf (intern (format "%s" f))))
    (if (>= emacs-major-version 28)
        (funcall realf n)
      (funcall realf))))

(defun evil-collection-xwidget-webkit-scroll-half-page-up ()
  "Compatible wrapper for `xwidget-webkit-scroll-up'."
  (interactive)
  (evil-collection-xwidget-webkit--scroll-trampoline
   "xwidget-webkit-scroll-up"
   (/ (xwidget-window-inside-pixel-height (selected-window)) 2)))

(defun evil-collection-xwidget-webkit-scroll-half-page-down ()
  "Compatible wrapper for `xwidget-webkit-scroll-down'."
  (interactive)
  (evil-collection-xwidget-webkit--scroll-trampoline
   "xwidget-webkit-scroll-down"
   (/ (xwidget-window-inside-pixel-height (selected-window)) 2)))

(defun evil-collection-xwidget-webkit-new-tab (url)
  "New tab (new buffer) of URL."
  (interactive "s(New Tab) xwidget-webkit URL: ")
  (xwidget-webkit-browse-url url :new-session))

(defvar evil-collection-xwidget-webkit-last-closed-tab-url nil)
(defun evil-collection-xwidget-webkit-close-tab ()
  "Close tab (kill buffer) without confirmation."
  (interactive)
  (setq evil-collection-xwidget-webkit-last-closed-tab-url
        (xwidget-webkit-uri (xwidget-webkit-current-session)))
  (let ((kill-buffer-query-functions nil))
    (kill-this-buffer)))

(defun evil-collection-xwidget-webkit-restore-last-closed-tab ()
  "Restore last closed tab."
  (interactive)
  (if (null evil-collection-xwidget-webkit-last-closed-tab-url)
      (message "No url to restore.")
    (message (format "Restoring last closed tab %s"
              evil-collection-xwidget-webkit-last-closed-tab-url))
    (xwidget-webkit-browse-url
     evil-collection-xwidget-webkit-last-closed-tab-url :new-session)
    (setq evil-collection-xwidget-webkit-last-closed-tab-url nil)))

(defun evil-collection-xwidget-webkit-search-tabs ()
  "Search tabs (buffers) with `buffer-name'."
  (interactive)
  (cl-flet ((is-xwidget-webkit-buffer?
             (b) (with-current-buffer b
                   (equal major-mode 'xwidget-webkit-mode))))
    (let ((bufs (cl-loop for b in (buffer-list)
                         when (is-xwidget-webkit-buffer? b)
                         collect (buffer-name b))))
      (switch-to-buffer (completing-read "Switch to Tab: " bufs nil t)))))

;;;###autoload
(defun evil-collection-xwidget-setup ()
  "Set up `evil' bindings for `xwidget'."
  (evil-collection-set-readonly-bindings 'xwidget-webkit-mode-map)

  (evil-collection-define-key 'visual 'xwidget-webkit-mode-map
    "y" 'xwidget-webkit-copy-selection-as-kill)

  (evil-collection-define-key 'normal 'xwidget-webkit-mode-map
    ;; Mimic vimium (a browser extension)
    ;;
    ;; Since xwidget has no tab concept, `open-url-new-tab' etc are not
    ;; supported.
    ;;
    ;; TODO: edit-mode, visual-mode
    ;;
    ;; Navigating the page
    "j" 'xwidget-webkit-scroll-up-line
    "k" 'xwidget-webkit-scroll-down-line
    "gg" 'xwidget-webkit-scroll-top
    "G" 'xwidget-webkit-scroll-bottom
    "d" 'evil-collection-xwidget-webkit-scroll-half-page-up
    "u" 'evil-collection-xwidget-webkit-scroll-half-page-down
    "h" 'xwidget-webkit-scroll-backward
    "l" 'xwidget-webkit-scroll-forward
    "r" 'xwidget-webkit-reload
    "yy" 'xwidget-webkit-current-url
    ;; TODO `first-text-input', `open-link-in-current-tab'
    "gi" 'undefined
    "f" 'undefined

    ;; Using the vomnibar
    ;;
    ;; For bookmark, use C-x r m, C-x r j.
    "o" 'xwidget-webkit-browse-url
    "T" 'evil-collection-xwidget-webkit-search-tabs

    ;; Using find
    ;;
    ;; TODO

    ;; Navigating history
    "H" 'xwidget-webkit-back
    "L" 'xwidget-webkit-forward

    ;; Manipulating tabs (actually buffers)
    ;;
    ;; Only `new-tab' and `close-tab' are supported.
    "t" 'evil-collection-xwidget-webkit-new-tab
    "x" 'evil-collection-xwidget-webkit-close-tab
    "X" 'evil-collection-xwidget-webkit-restore-last-closed-tab

    ;; Miscellaneous
    "?" 'describe-mode

    ;; Evil style

    "<" 'xwidget-webkit-back
    ">" 'xwidget-webkit-forward

    ;; Additional bindings to browse url.
    "gu" 'xwidget-webkit-browse-url
    "J" 'xwidget-webkit-browse-url

    ;; Additional binding to browse tabs.
    "gt" 'evil-collection-xwidget-webkit-search-tabs

    "gr" 'xwidget-webkit-reload
    (kbd "C-f") 'xwidget-webkit-scroll-up
    (kbd "C-b") 'xwidget-webkit-scroll-down
    (kbd "RET") 'xwidget-webkit-insert-string

    ;; zoom
    "+" 'xwidget-webkit-zoom-in
    "=" 'xwidget-webkit-zoom-in
    "-" 'xwidget-webkit-zoom-out)

  (when evil-want-C-d-scroll
    (evil-collection-define-key 'normal 'xwidget-webkit-mode-map
      (kbd "C-d") 'xwidget-webkit-scroll-up))
  (when evil-want-C-u-scroll
    (evil-collection-define-key 'normal 'xwidget-webkit-mode-map
      (kbd "C-u") 'xwidget-webkit-scroll-down)))

(provide 'evil-collection-xwidget)
;;; evil-collection-xwidget.el ends here
