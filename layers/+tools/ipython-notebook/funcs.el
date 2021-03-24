;;; funcs.el --- ipython-notebook Layer function File for Spacemacs
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Senghoo Kim <me@senghoo.com>
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


(when (configuration-layer/package-used-p 'company)

  (defun spacemacs/ein-company (command &optional arg &rest ignore)
    (interactive (list 'interactive))
    (pcase command
      (`interactive (company-begin-backend 'company-ein))
      (`prefix
       (and (--filter (and (boundp it) (symbol-value it) (eql it 'ein:notebook-mode))
                      minor-mode-list)
            (company-grab-symbol-cons "\\.\\|->\\|::" 2)))
      (`candidates
       (lexical-let ((kernel (ein:get-kernel-or-error))
                     (arg arg)
                     (col (current-column)))
         (cons :async
               (lambda (cb)
                 (ein:kernel-complete
                  kernel
                  (string-trim-right (thing-at-point 'line t))
                  col
                  (list :complete_reply
                        (list #'spacemacs//ein-company-callback cb))
                  nil)))))))

  (defun spacemacs//ein-company-callback (args content metadata)
    (let ((matches (append (plist-get content :matches) nil)))
      (condition-case err
          (progn
            (funcall (car args) matches))
        (error (error (format "Error %s running ein company completer." err))))))
  (defun spacemacs//ein-setup-company ()
    (if (eq ein-backend 'jupyter)
        (spacemacs|add-company-backends
          :backends spacemacs/ein-company
          :modes ein:notebook-mode
          :append-hooks nil
          :call-hooks t))))
