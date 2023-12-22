;;; helm-regexp.el --- In buffer regexp searching and replacement for helm. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2023 Thierry Volpiatto 

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

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-help)
(require 'helm-utils)

(declare-function helm-mm-split-pattern "helm-multi-match")


(defgroup helm-regexp nil
  "Regexp related Applications and libraries for Helm."
  :group 'helm)



;; History vars
(defvar helm-build-regexp-history nil)

(defun helm-query-replace-regexp (_candidate)
  "Query replace regexp from `helm-regexp'.
With a prefix arg replace only matches surrounded by word boundaries,
i.e. don't replace inside a word, regexp is surrounded with \\bregexp\\b."
  (let ((regexp helm-input))
    (apply 'query-replace-regexp
           (helm-query-replace-args regexp))))

(defun helm-kill-regexp-as-sexp (_candidate)
  "Kill regexp in a format usable in lisp code."
  (helm-regexp-kill-new
   (prin1-to-string helm-input)))

(defun helm-kill-regexp (_candidate)
  "Kill regexp as it is in `helm-pattern'."
  (helm-regexp-kill-new helm-input))

(defun helm-query-replace-args (regexp)
  "Create arguments of `query-replace-regexp' action in `helm-regexp'."
  (let ((region-only (helm-region-active-p)))
    (list
     regexp
     (query-replace-read-to regexp
                            (format "Query replace %sregexp %s"
                                    (if helm-current-prefix-arg "word " "")
                                    (if region-only "in region " ""))
                            t)
     helm-current-prefix-arg
     (when region-only (region-beginning))
     (when region-only (region-end)))))

(defvar helm-source-regexp
  (helm-build-in-buffer-source "Regexp Builder"
    :init (lambda ()
            (helm-init-candidates-in-buffer
                'global (with-temp-buffer
                          (insert-buffer-substring helm-current-buffer)
                          (buffer-string))))
    :get-line #'helm-regexp-get-line
    :persistent-action #'helm-regexp-persistent-action
    :persistent-help "Show this line"
    :multiline t
    :multimatch nil
    :requires-pattern 2
    :group 'helm-regexp
    :mode-line "Press TAB to select action."
    :action '(("Kill Regexp as sexp" . helm-kill-regexp-as-sexp)
              ("Query Replace Regexp (C-u Not inside word.)"
               . helm-query-replace-regexp)
              ("Kill Regexp" . helm-kill-regexp))))

(defun helm-regexp-get-line (s e)
  (let ((matches (match-data))
        (line    (buffer-substring s e)))
    (propertize
     (cl-loop with ln = (format "%5d: %s" (1- (line-number-at-pos s)) line)
           for i from 0 to (1- (/ (length matches) 2))
           if (match-string i)
           concat (format "\n%s%s'%s'"
                          (make-string 10 ? ) (format "Group %d: " i) it)
           into ln1
           finally return (concat ln ln1))
     'helm-realvalue s)))

(defun helm-regexp-persistent-action (pt)
  (helm-goto-char pt)
  (helm-highlight-current-line))

(defun helm-regexp-kill-new (input)
  (kill-new (substring-no-properties input))
  (message "Killed: %s" input))


;;; Predefined commands
;;
;;

;;;###autoload
(defun helm-regexp ()
  "Preconfigured helm to build regexps.
`query-replace-regexp' can be run from there against found regexp."
  (interactive)
  (save-restriction
    (when (and (helm-region-active-p)
               ;; Don't narrow to region if buffer is already narrowed.
               (not (helm-current-buffer-narrowed-p (current-buffer))))
      (narrow-to-region (region-beginning) (region-end)))
    (helm :sources helm-source-regexp
          :buffer "*helm regexp*"
          :prompt "Regexp: "
          :history 'helm-build-regexp-history)))


(provide 'helm-regexp)

;;; helm-regexp.el ends here
