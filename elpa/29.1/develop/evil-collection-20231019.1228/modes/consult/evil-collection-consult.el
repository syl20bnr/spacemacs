;;; evil-collection-consult.el --- Evil bindings for consult -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, consult, tools

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
;; Evil bindings for `consult'.
;;
;; Since
;;
;; `consult-outline'
;; `consult-mark'
;; `consult-imenu'
;; `consult-org-heading'
;; `consult-line'
;;
;; are all autoloaded, no (require 'consult nil t) is needed.

;;; Code:
(require 'evil-collection)

(defvar consult-line-numbers-widen)
(declare-function consult--forbid-minibuffer "consult")
(declare-function consult--fontify-all "consult")
(declare-function consult--in-range-p "consult")
(declare-function consult--line-with-mark "consult")
(declare-function consult--location-candidate "consult")
(declare-function consult--remove-dups "consult")
(declare-function consult--mark-candidates "consult")
(declare-function consult-mark "consult")
(declare-function consult-global-mark "consult")

(defun evil-collection-consult-set-bindings ()
  "Set the bindings."
  (dolist (cmd '(consult-outline
                 consult-mark
                 consult-global-mark
                 consult-imenu
                 consult-org-heading
                 consult-line))
    (evil-declare-not-repeat cmd)
    (evil-set-command-property cmd :jump t)))

(defun evil-collection-consult--evil-mark-ring ()
  "Return alist of char & marker for evil markers in current buffer."
  (sort (cl-remove-if (lambda (elem)
                        (or (evil-global-marker-p (car elem))
                            (not (markerp (cdr-safe elem)))))
                      evil-markers-alist)
        #'car-less-than-car))

(defun evil-collection-consult--mark-candidates (&optional markers)
  "Return alist of lines containing markers from `evil-mark-alist'.
Opional MARKERS should be an alist containing (char . marker) pairs
as defined in `evil-collection-consult--evil-mark-ring'."
  (consult--forbid-minibuffer)
  (unless (evil-collection-consult--evil-mark-ring)
    (user-error "No marks"))
  (consult--fontify-all)
  (let* ((candidates)
         (current-buf (current-buffer)))
    (save-excursion
      (pcase-dolist (`(,char . ,marker) (or markers (evil-collection-consult--evil-mark-ring)))
        (let ((pos (marker-position marker))
              (buf (marker-buffer marker)))
          (when (and (eq buf current-buf)
                     (consult--in-range-p pos))
            (goto-char pos)
            (push (consult--location-candidate
                   (format "%s: %s" (char-to-string char) (consult--line-with-mark marker))
                   marker
                   (line-number-at-pos pos consult-line-numbers-widen)
                   marker)
                  candidates)))))
    (nreverse (delete-dups candidates))))

;;;###autoload
(defun evil-collection-consult-mark ()
  "Jump to an evil marker in the current buffer."
  (interactive)
  (cl-letf (((symbol-function 'consult--mark-candidates)
             #'evil-collection-consult--mark-candidates))
    (consult-mark (evil-collection-consult--evil-mark-ring))))

;;;###autoload
(defun evil-collection-consult-jump-list ()
  "Jump to a position in the evil jump list."
  (interactive)
  (consult-global-mark (delq nil (mapcar (lambda (jump)
                                           (let ((mark (car jump)))
                                             (when (markerp mark)
                                               mark)))
                                         (ring-elements (evil--jumps-get-window-jump-list))))))

;;;###autoload
(defun evil-collection-consult-setup ()
  "Set up `evil' bindings for `consult'."
  (evil-collection-consult-set-bindings))

(provide 'evil-collection-consult)
;;; evil-collection-consult.el ends here
