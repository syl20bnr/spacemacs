;;; helm-space-macs-help.el --- Space-macs layer exploration with `helm'.

;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: helm, space-macs
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
;; along with GNU e-macs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; This package adds a convenient way to discover Space-macs FAQs in a
;; a helm buffer.
;; These sources are not part of `helm-space-macs-help' feature because
;; of its `org' dependency which triggers lots of `require'.

;;; Code:

(require 'helm-space-macs-help)
(require 'helm-org)

(defvar helm-space-macs-help--faq-filename
  (concat space-macs-docs-directory "FAQ.org")
  "Location of the FAQ file.")

;;;###autoload
(defun helm-space-macs-help-faq ()
  "Helm session to search for the FAQ."
  (interactive)
  (helm-space-macs-help-mode)
  (helm :buffer "*helm: space-macs*"
        :sources `(,(helm-space-macs-help//faq-source))))

(defun helm-space-macs-help//faq-source ()
  "Construct the helm source for the FAQ."
  `((name . "FAQ")
    (candidates . ,(helm-space-macs-help//faq-candidates))
    (candidate-number-limit)
    (action . (("Go to question" . helm-space-macs-help//faq-goto-marker)))))

(defun helm-space-macs-help//faq-candidate (cand)
  (let ((str (substring-no-properties (car cand))))
    (when (string-match "\\`.*/\\([^/]*\\)/\\(.*\\)\\'" str)
      (cons (concat (propertize
                     (match-string 1 str)
                     'face 'font-lock-type-face)
                    ": " (match-string 2 str))
            (cdr cand)))))

(defun helm-space-macs-help//get-faq-headings-list (sources)
  "Given the helm-org sources from FAQ.org.
Return a list of all it's headings."
    (aref (aref (cdadar sources) 2) 0))

(defun helm-space-macs-help//faq-candidates ()
  "Join the section headings to each of their questions.
Remove all text properties.
Color the section heading substring.
Add back the helm candidate text properties (to section substring):
`helm-real-display' with the joined \"section: question\"
`helm-realvalue' with the question marker.
The questions marker makes sure that the cursor
jumps to the selected question."
  (let* ((helm-org-format-outline-path nil)
         (cands (helm-org-build-sources
                 (list helm-space-macs-help--faq-filename)))
         section section-no-prop
         question question-marker question-no-prop
         section-and-question-with-marker
         result)
    (dolist (heading (helm-space-macs-help//get-faq-headings-list cands))
      (when (string-match "\\`\\* \\(.*\\)\\'" heading)
        (setq section (match-string 1 heading))
        (setq section-no-prop (substring-no-properties section)))
      (when (string-match "\\*\\* \\(.*\\)" heading)
        (setq question (match-string 1 heading))
        (setq question-marker (get-text-property 0 'helm-realvalue question))
        (setq question-no-prop (substring-no-properties question))
        (setq section-and-question
              (concat section-no-prop ": " question-no-prop))
        (setq section-and-question-with-marker
              (concat (propertize
                       section-no-prop
                       'face 'font-lock-type-face
                       'helm-real-display section-and-question
                       'helm-realvalue question-marker)
                      ": " question-no-prop))
        (push (cons section-and-question-with-marker heading) result)))
    ;; The result list is reversed, to show the questions in the same order as
    ;; they appear in the FAQ.org file, with the common questions first.
    (reverse result)))

(defun helm-space-macs-help//faq-goto-marker (marker)
  (find-file helm-space-macs-help--faq-filename)
  (goto-char marker)
  (org-show-context)
  (org-show-entry)
  (recenter-top-bottom))

(provide 'helm-space-macs-faq)

;;; helm-space-macs-faq.el ends here


