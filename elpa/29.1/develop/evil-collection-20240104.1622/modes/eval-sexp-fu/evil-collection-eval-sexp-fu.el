;;; evil-collection-eval-sexp-fu.el --- Bindings for `eval-sexp-fu' -*- lexical-binding: t -*-

;; Copyright (C) 2017 James Nguyen

;; Author: James Nguyen <james@jojojames.com>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, eval-sexp-fu, tools

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
;;; Bindings for `eval-sexp-fu'.

;;; Code:
(require 'evil-collection)
(require 'eval-sexp-fu nil t)

(defconst evil-collection-eval-sexp-fu-maps nil)

(defun evil-collection-eval-sexp-fu-bounds-of-thing-at-point-sexp (command &rest args)
  "In normal-state or motion-state, last sexp ends at point."
  (if (and (eq (nth 0 args) 'sexp)
           (and (not evil-move-beyond-eol)
                (or (evil-normal-state-p) (evil-motion-state-p))))
      (save-excursion
        (unless (or (eobp) (eolp)) (forward-char))
        (apply command args))
    (apply command args)))

(defun evil-collection-eval-sexp-fu-advise-bounds-of-thing-at-point (command &rest args)
  "Advise `bounds-of-thing-at-point' to handle `evil's `evil-move-beyond-eol'."
  (advice-add 'bounds-of-thing-at-point
              :around 'evil-collection-eval-sexp-fu-bounds-of-thing-at-point-sexp)
  (apply command args)
  (advice-remove 'bounds-of-thing-at-point
                 'evil-collection-eval-sexp-fu-bounds-of-thing-at-point-sexp))

;;;###autoload
(defun evil-collection-eval-sexp-fu-setup ()
  "Set up `evil' with `eval-sexp-fu'."
  (unless evil-move-beyond-eol
    (advice-add 'ad-Advice-eval-last-sexp
                :around 'evil-collection-eval-sexp-fu-advise-bounds-of-thing-at-point)))

(provide 'evil-collection-eval-sexp-fu)
;;; evil-collection-eval-sexp-fu.el ends here
