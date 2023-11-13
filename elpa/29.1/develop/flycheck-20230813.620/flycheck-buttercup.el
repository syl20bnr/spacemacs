;;; flycheck-buttercup.el --- Flycheck: Extensions to Buttercup -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Flycheck contributors
;; Copyright (C) 2016 Sebastian Wiesner and Flycheck contributors

;; Author: Sebastian Wiesner <swiesner@lunaryorn.com>
;; Maintainer: Clément Pit-Claudel <clement.pitclaudel@live.com>
;;             fmdkdd <fmdkdd@gmail.com>
;; Keywords: lisp, tools

;; This file is not part of GNU Emacs.

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

;; Extensions to Buttercup to write BDD tests for Flycheck.
;;
;; Buttercup is a BDD testing framework for Emacs, see URL
;; `https://github.com/jorgenschaefer/emacs-buttercup/'.  Flycheck uses
;; Buttercup extensively for new tests.
;;
;; This library provides extensions to Buttercup to write Specs for Flycheck.
;;
;; * Custom matchers
;;
;; (expect 'foo :to-be-local) - Is `foo' a local variable in the current buffer?

;;; Code:

(require 'buttercup)
(require 'flycheck)
(require 'seq)


;;; Buttercup helpers

(defun flycheck-buttercup-format-error-list (errors)
  "Format ERRORS into a human-readable string."
  (mapconcat (lambda (e) (flycheck-error-format e 'with-file-name))
             errors "\n"))


;;; Data matchers

(buttercup-define-matcher :to-be-empty-string (s)
  (let ((s (funcall s)))
    (if (equal s "")
        (cons t (format "Expected %S not be an empty string" s))
      (cons nil (format "Expected %S to be an empty string" s)))))

(buttercup-define-matcher :to-match-with-group (re s index match)
  (let* ((re (funcall re))
         (s (funcall s))
         (index (funcall index))
         (match (funcall match))
         (matches? (string-match re s))
         (result (and matches? (match-string index s))))
    (if (and matches? (equal result match))
        (cons t (format "Expected %S not to match %S with %S in group %s"
                        re s match index))

      (cons nil (format "Expected %S to match %S with %S in group %s, %s"
                        re s match index
                        (if matches?
                            (format "but got %S" result)
                          "but did not match"))))))


;;; Emacs feature matchers

(buttercup-define-matcher :to-be-live (buffer)
  (let ((buffer (get-buffer (funcall buffer))))
    (if (buffer-live-p buffer)
        (cons t (format "Expected %S not to be a live buffer, but it is"
                        buffer))
      (cons nil (format "Expected %S to be a live buffer, but it is not"
                        buffer)))))

(buttercup-define-matcher :to-be-visible (buffer)
  (let ((buffer (get-buffer (funcall buffer))))
    (cond
     ((and buffer (get-buffer-window buffer))
      (cons t (format "Expected %S not to be a visible buffer, but it is"
                      buffer)))
     ((not (bufferp buffer))
      (cons nil
            (format "Expected %S to be a visible buffer, but it is not a buffer"
                    buffer)))
     (t (cons
         nil
         (format "Expected %S to be a visible buffer, but it is not visible"
                 buffer))))))

(buttercup-define-matcher :to-be-local (symbol)
  (let ((symbol (funcall symbol)))
    (if (local-variable-p symbol)
        (cons t (format "Expected %S not to be a local variable, but it is"
                        symbol))
      (cons nil (format "Expected %S to be a local variable, but it is not"
                        symbol)))))

(buttercup-define-matcher :to-contain-match (buffer re)
  (let ((buffer (funcall buffer))
        (re (funcall re)))
    (if (not (get-buffer buffer))
        (cons nil (format "Expected %S to contain a match of %s, \
but is not a buffer" buffer re))
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (if (re-search-forward re nil 'noerror)
              (cons t (format "Expected %S to contain a match \
for %s, but it did not" buffer re))
            (cons nil (format "Expected %S not to contain a match for \
%s but it did not." buffer re))))))))


;;; Flycheck matchers

(buttercup-define-matcher :to-be-equal-flycheck-errors (a b)
  (let* ((a (funcall a))
         (b (funcall b))
         (a-formatted (flycheck-buttercup-format-error-list a))
         (b-formatted (flycheck-buttercup-format-error-list b)))
    (if (equal a b)
        (cons t (format "Expected
%s
not to be equal to
%s" a-formatted b-formatted))
      (cons nil (format "Expected
%s
to be equal to
%s" a-formatted b-formatted)))))

(provide 'flycheck-buttercup)

;; Disable byte compilation for this library, to prevent package.el choking on a
;; missing `buttercup' library.  See
;; https://github.com/flycheck/flycheck/issues/860

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; flycheck-buttercup.el ends here
