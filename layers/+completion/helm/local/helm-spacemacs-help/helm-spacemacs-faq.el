;;; helm-spacemacs-help.el --- Spacemacs layer exploration with `helm'.

;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: helm, spacemacs
;; Version: 0.1
;; Package-Requires: ((helm "1.5"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; This package adds a convenient way to discover Spacemacs FAQs in a
;; a helm buffer.
;; These sources are not part of `helm-spacemacs-help' feature because
;; of its `org' dependency which triggers lots of `require'.

;;; Code:

(require 'helm-spacemacs-help)
(require 'helm-org)

(defvar helm-spacemacs-help--faq-filename
  (concat spacemacs-docs-directory "FAQ.org")
  "Location of the FAQ file.")

;;;###autoload
(defun helm-spacemacs-help-faq ()
  "Helm session to search for the FAQ."
  (interactive)
  (helm-spacemacs-help-mode)
  (helm :buffer "*helm: spacemacs*"
        :sources `(,(helm-spacemacs-help//faq-source))))

(defun helm-spacemacs-help//faq-source ()
  "Construct the helm source for the FAQ."
  `((name . "FAQ")
    (candidates . ,(helm-spacemacs-help//faq-candidates))
    (candidate-number-limit)
    (action . (("Go to question" . helm-spacemacs-help//faq-goto-marker)))))

(defun helm-spacemacs-help//faq-candidate (cand)
  (let ((str (substring-no-properties (car cand))))
    (when (string-match "\\`.*/\\([^/]*\\)/\\(.*\\)\\'" str)
      (cons (concat (propertize
                     (match-string 1 str)
                     'face 'font-lock-type-face)
                    ": " (match-string 2 str))
            (cdr cand)))))

(defun helm-spacemacs-help//faq-candidates ()
  (let* ((helm-org-format-outline-path nil)
         (cands (helm-org-get-candidates (list helm-spacemacs-help--faq-filename)))
         section result)
    (dolist (c cands)
      (let ((str (substring-no-properties (car c))))
        (when (string-match "\\`\\* \\(.*\\)\\'" str)
          (setq section (match-string 1 str)))
        (when (string-match "\\`\\*\\* \\(.*\\)\\'" str)
          (push (cons (concat (propertize section 'face 'font-lock-type-face)
                              ": " (match-string 1 str))
                      (cdr c))
                result))))
    result))

(defun helm-spacemacs-help//faq-goto-marker (marker)
  (find-file helm-spacemacs-help--faq-filename)
  (goto-char marker)
  (org-show-context)
  (org-show-entry))

(provide 'helm-spacemacs-faq)

;;; helm-spacemacs-faq.el ends here
