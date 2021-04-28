;;; funcs.el --- languagetool layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Robbert van der Helm <mail@robbertvanderhelm.nl>
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


(defun spacemacs/languagetool-next-error (count)
  (interactive "p")
  (dotimes (_ count) (langtool-goto-next-error))
  (when languagetool-show-error-on-jump (langtool-show-message-at-point)))

(defun spacemacs/languagetool-previous-error (count)
  (interactive "p")
  (dotimes (_ count) (langtool-goto-previous-error))
  (when languagetool-show-error-on-jump (langtool-show-message-at-point)))

(defun spacemacs/languagetool-toggle ()
  "Performs grammar and spell checking on the current buffer
  using LanguageTool for grammar and flyspell for spell checking.
  Flyspell errors will be cleared if the 'spell-checking' layer
  is active as they add a lot of clutter."
  (interactive)
  (if (package-installed-p 'langtool)
      (let* ((has-ran (bound-and-true-p langtool-mode-line-message))
             (still-running
              (and has-ran (equal ":run" (cadr langtool-mode-line-message)))))
        ;; Don't do anything while LanguageTool is still running
        (unless still-running
          ;; Clear LanguageTool's errors if there is an active error overlay
          (if has-ran
              (langtool-check-done)
            (progn
              (langtool-check-buffer (spacemacs//languagetool-get-language))
              (when (package-installed-p 'flyspell)
                (flyspell-delete-all-overlays))))))
    (error "LanguageTool has not been set up yet")))

(defun spacemacs//languagetool-detect ()
  "Detects whether the LanguageTool binary exists."
  (cond ((boundp 'langtool-java-classpath) t)
        ((and (boundp 'langtool-http-server-host)
              (boundp 'langtool-http-server-port)) t)
        ((boundp 'langtool-language-tool-jar)
         (if (file-readable-p langtool-language-tool-jar)
             t
           (spacemacs-buffer/warning "LanguageTool binary not found")))
        (t (spacemacs-buffer/warning "LanguageTool binary not set"))))

(defun spacemacs//languagetool-get-language ()
  "Tries to parse the current spell checking language for a
  usable locale string. This won't do anything if the
  'spell-checking' layer is not active."
  (let ((language (and (package-installed-p 'flyspell)
                       (or ispell-local-dictionary ispell-dictionary))))
    (when language
      ;; We'll assume the language is either a locale or a named language (i.e.
      ;; "en_GB" or "english")
      (let* ((locale (or (cadr
                          (assoc language ispell-dicts-name2locale-equivs-alist))
                         language))
             ;; ispell uses underscores in its locales, but LanguageTool expects a
             ;; dash (e.g. "en_US" => "en-US")
             (langtool-code (replace-regexp-in-string "_" "-" locale)))
        langtool-code))))
