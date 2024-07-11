;;; funcs.el --- PureScript Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: nobv <6e6f6276@gmail.com>
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


(defun spacemacs//purescript-setup-backend ()
  "Conditionally setup purescript backend."
  (when (eq purescript-backend 'lsp)
    (lsp-deferred)))

(defun spacemacs//purescript-setup-company ()
  "Conditionally setup company based on backend."
  (pcase purescript-backend
    ('lsp
     (spacemacs|add-company-backends ;; Activate lsp company explicitly to activate
       :backends company-capf        ;; standard backends as well
       :modes purescript-mode))
    ('psc-ide
     (spacemacs|add-company-backends
       :backends company-psc-ide-backend
       :modes purescript-mode))))

(defun spacemacs/purescript-format ()
  "Call formatting tool specified in `purescript-fmt-tool'."
  (interactive)
  (call-interactively
   (pcase purescript-fmt-tool
     ('purs-tidy 'spacemacs/purescript-purs-tidy-format-buffer)
     (_ (user-error
         "%s isn't a valid purescript formatter. Possible values are 'purs-tidy"
         purescript-fmt-tool)))))

(defun spacemacs/purescript-purs-tidy-format-buffer ()
  "Format buffer with purs-tidy."
  (interactive)
  (if (executable-find "purs-tidy")
      (let*  ((extension (file-name-extension (or buffer-file-name "tmp.purs") t))
              (tmpfile (make-temp-file "~fmt-tmp" nil extension))
              (coding-system-for-read 'utf-8)
              (coding-system-for-write 'utf-8)
              (outputbuf (get-buffer-create "*~fmt-tmp.purs*")))
        (unwind-protect
            (progn
              (with-current-buffer outputbuf (erase-buffer))
              (write-region nil nil tmpfile)
              (if (zerop (apply #'call-process-region nil nil "purs-tidy" nil
                                `(,outputbuf ,tmpfile) nil
                                `("format")))
                  (let ((p (point)))
                    (save-excursion
                      (with-current-buffer (current-buffer)
                        (replace-buffer-contents outputbuf)))
                    (goto-char p)
                    (message "formatted.")
                    (kill-buffer outputbuf))
                (message "Formatting failed!")
                (display-buffer outputbuf)))
            (delete-file tmpfile)))
    (error "purs-tidy not found. Run \"npm install -g purs-tidy\"")))

(defun spacemacs/purescript-fmt-before-save-hook ()
  (add-hook 'before-save-hook 'spacemacs/purescript-format t t))
