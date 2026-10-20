;;; funcs.el --- harper layer funcs file for Spacemacs.  -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2025 Sylvain Benner & Contributors
;;
;; Author: sunlin7<at>hotmail.com
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


(declare-function 'flymake-goto-next-error "flymake")
(defun spacemacs/harper-next-error (n)
  "Jump to the Nth next harper error and maybe show the message."
  (interactive "p")
  (flymake-goto-next-error n))

(defun spacemacs/harper-prev-error (n)
  "Jump to the Nth previous harper error and maybe show the message."
  (interactive "p")
  (spacemacs/harper-next-error (- (or n 1))))

(defun spacemacs/harper-toggle (&optional off)
  "Perform grammar and spell checking on the current buffer using Harper."
  (interactive)
  (require 'eglot)
  (defvar harper-checker-parent)
  (if (or off (bound-and-true-p harper-checker-parent)
          (eq major-mode 'harper-mode))
      (progn
        (when-let* ((srv (eglot-current-server)))
          (eglot-shutdown srv))
        (funcall (or harper-checker-parent 'ignore)))
    ;; check the requirements
    (unless (fboundp 'eglot)
      (user-error "The eglot package is required."))
    (unless (executable-find harper-ls-bin)
      (user-error "The harper-ls is not found in PATH."))
    (when-let* ((langid (replace-regexp-in-string
                         "\\(?:-ts\\)?-mode$" ""
                         (symbol-name major-mode)))
                (harper-mode (intern (format "harper-checker-%s-mode" langid))))
      (unless (fboundp harper-mode)
        (eval                           ;; define a derived major mode to run eglot
         `(define-derived-mode ,harper-mode ,major-mode ,mode-name
            (set (make-local-variable 'harper-checker-parent) ',major-mode)
            (when-let* ((srv (eglot-current-server)))
              (eglot-shutdown srv))
            (eglot-ensure)))
        (spacemacs/set-leader-keys-for-major-mode harper-mode
          "," 'eglot-code-action-quickfix
          "'" 'eglot-code-actions
          "n" 'spacemacs/harper-next-error
          "p" 'spacemacs/harper-prev-error)
        (add-to-list 'eglot-server-programs
                     (cons (list harper-mode
                                 :language-id langid)
                           (append (list harper-ls-bin) harper-ls-args))))

      (funcall harper-mode))))

