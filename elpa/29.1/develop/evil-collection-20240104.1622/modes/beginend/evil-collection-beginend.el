;;; evil-collection-beginend.el --- Evil bindings for beginend -*- lexical-binding: t -*-

;; Copyright (C) 2021 Balaji Sivaraman

;; Author: Balaji Sivaraman <balaji@balajisivaraman.com>
;; Maintainer: Balaji Sivaraman <balaji@balajisivaraman.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, tools

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
;; Evil bindings for beginend.

;;; Code:

(require 'evil-collection)
(require 'beginend nil t)

(eval-and-compile
  (defconst evil-collection-beginend-modes '(bs-mode
                                             rg-mode
                                             org-mode
                                             deft-mode
                                             prog-mode
                                             LaTeX-mode
                                             dired-mode
                                             latex-mode
                                             nroam-mode
                                             occur-mode
                                             vc-dir-mode
                                             ibuffer-mode
                                             message-mode
                                             outline-mode
                                             prodigy-mode
                                             org-agenda-mode
                                             compilation-mode
                                             epa-key-list-mode
                                             magit-status-mode
                                             elfeed-search-mode
                                             elfeed-show-mode
                                             magit-revision-mode
                                             notmuch-search-mode
                                             recentf-dialog-mode)))


(defmacro evil-beginend--define-goto-beginning-motion (ec-mode-name)
  "Macro to define new Evil motion that will use the corresponding
beginend-goto-beginning function for EC-MODE-NAME when count is not
provided for the motion; otherwise behave like `evil-goto-first-line'
if count is provided.

This will also associate the regular Evil `gg' keybinding with the
newly defined motion."
  (let ((motion-name (intern (format "evil-beginend-%s-goto-beginning" ec-mode-name)))
        (beginend-beginning-fn-name (intern (format "beginend-%s-goto-beginning" ec-mode-name)))
        (beginend-map-name (intern (format "beginend-%s-map" ec-mode-name))))
    `(progn
       (declare-function ,motion-name "evil-collection")
       (declare-function ,beginend-beginning-fn-name "ext:beginend")
       (evil-define-motion ,motion-name (count)
         :jump t
         :type line
         (if count
             (evil-goto-first-line count)
           (,beginend-beginning-fn-name)))
       (evil-collection-define-key 'normal ',beginend-map-name
         "gg" ',motion-name))))

(defmacro evil-beginend--define-goto-end-motion (ec-mode-name)
  "Macro to define new Evil motion that will use the corresponding
beginend-goto-end function for EC-MODE-NAME when count is not
provided for the motion; otherwise behave like `evil-goto-line'
if count is provided.

This will also associate the regular Evil `G' keybinding with the
newly defined motion."
  (let ((motion-name (intern (format "evil-beginend-%s-goto-end" ec-mode-name)))
        (beginend-end-fn-name (intern (format "beginend-%s-goto-end" ec-mode-name)))
        (beginend-map-name (intern (format "beginend-%s-map" ec-mode-name))))
    `(progn
       (declare-function ,motion-name "evil-collection")
       (declare-function ,beginend-end-fn-name "ext:beginend")
       (evil-define-motion ,motion-name (count)
         :jump t
         :type line
         (if count
             (evil-goto-line count)
           (,beginend-end-fn-name)))
       (evil-collection-define-key 'normal ',beginend-map-name
         "G" ',motion-name))))

(defmacro evil-beginend--define-mode-motions ()
  "Define the goto-beginning and goto-end motions for all modes in
`evil-collection-beginend-modes'."
  `(progn
     ,@(mapcar
        (lambda (mode-symbol)
          (let ((mode-string (symbol-name mode-symbol)))
            `(progn
               (evil-beginend--define-goto-beginning-motion ,mode-string)
               (evil-beginend--define-goto-end-motion ,mode-string))))
        evil-collection-beginend-modes)))

;;;###autoload
(defun evil-collection-beginend-setup ()
  "Set up `evil' bindings for `beginend'."
  (evil-beginend--define-mode-motions))

(provide 'evil-collection-beginend)
;;; evil-collection-beginend.el ends here
