;;; elisp-slime-nav.el --- Make M-. and M-, work in elisp like they do in slime  -*- lexical-binding: t -*-

;; Copyright (C) 2016-2020  Steve Purcell

;; Author: Steve Purcell <steve@sanityinc.com>
;; Keywords: languages navigation slime elisp emacs-lisp
;; URL: https://github.com/purcell/elisp-slime-nav
;; Package-Version: 0
;; Package-Requires: ((emacs "24.1") (cl-lib "0.2"))

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
;;
;; This package provides Slime's convenient "M-." and "M-," navigation
;; in `emacs-lisp-mode', together with an elisp equivalent of
;; `slime-describe-symbol', bound by default to `C-c C-d d`.
;;
;; When the main functions are given a prefix argument, they will
;; prompt for the symbol upon which to operate.
;;
;; Usage:
;;
;; Enable the package in elisp and ielm modes as follows:
;;
;;   (require 'elisp-slime-nav) ;; optional if installed via package.el
;;   (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
;;     (add-hook hook 'turn-on-elisp-slime-nav-mode))
;;
;; Known issues:
;;
;;   When navigating into Emacs' C source, "M-," will not be bound to
;;   the same command, but "M-*" will typically do the trick.
;;
;;; Code:

(require 'etags)
(require 'help-mode)

(defvar elisp-slime-nav-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-.")         'elisp-slime-nav-find-elisp-thing-at-point)
    (define-key map (kbd "M-,")         'pop-tag-mark)
    (define-key map (kbd "C-c C-d d")   'elisp-slime-nav-describe-elisp-thing-at-point)
    (define-key map (kbd "C-c C-d C-d") 'elisp-slime-nav-describe-elisp-thing-at-point)
    map))

;;;###autoload
(define-minor-mode elisp-slime-nav-mode
  "Enable Slime-style navigation of elisp symbols using M-. and M-,"
  :lighter " SliNav" :keymap elisp-slime-nav-mode-map)

;;;###autoload
(define-obsolete-function-alias 'turn-on-elisp-slime-nav-mode 'elisp-slime-nav-mode
  "2020-01-30")

(defun elisp-slime-nav--all-navigable-symbol-names ()
  "Return a list of strings for the symbols to which navigation is possible."
  (let ((result '()))
    (mapatoms
     (lambda (x)
       (when (or (fboundp x) (boundp x) (symbol-plist x) (facep x))
         (push (symbol-name x) result))))
    result))

(defun elisp-slime-nav--read-symbol-at-point ()
  "Return the symbol at point as a string.
If `current-prefix-arg' is not nil, the user is prompted for the symbol."
  (let* ((sym-at-point (symbol-at-point))
         (at-point (and sym-at-point (symbol-name sym-at-point))))
    (if (or current-prefix-arg (null at-point))
        (completing-read "Symbol: "
                         (elisp-slime-nav--all-navigable-symbol-names)
                         nil t nil nil at-point)
      at-point)))

;;;###autoload
(defun elisp-slime-nav-find-elisp-thing-at-point (sym-name)
  "Find the elisp thing at point, be it a function, variable, library or face.

With a prefix arg, or if there is no thing at point, prompt for
the symbol to jump to.

Argument SYM-NAME is the thing to find."
  (interactive (list (elisp-slime-nav--read-symbol-at-point)))
  (when sym-name
    (let ((sym (intern sym-name)))
      (message "Searching for %s..." sym-name)
      (if (fboundp 'xref-push-marker-stack)
          (xref-push-marker-stack)
        (with-no-warnings
          (ring-insert find-tag-marker-ring (point-marker))))
      (cond
       ((fboundp sym)
        (find-function sym))
       ((boundp sym)
        (find-variable sym))
       ((or (featurep sym) (locate-library sym-name))
        (find-library sym-name))
       ((facep sym)
        (find-face-definition sym))
       (t
        (pop-tag-mark)
        (error "Don't know how to find '%s'" sym))))))

;;;###autoload
(defun elisp-slime-nav-describe-elisp-thing-at-point (sym-name)
  "Display the full documentation of the elisp thing at point.

The named subject may be a function, variable, library or face.

With a prefix arg, or if there is not \"thing\" at point, prompt
for the symbol to jump to.

Argument SYM-NAME is the thing to find."
  (interactive (list (elisp-slime-nav--read-symbol-at-point)))
  (if (fboundp 'describe-symbol)
      (describe-symbol (intern sym-name))
    (with-no-warnings
      (help-xref-interned (intern sym-name)))))


(provide 'elisp-slime-nav)
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
;;; elisp-slime-nav.el ends here
