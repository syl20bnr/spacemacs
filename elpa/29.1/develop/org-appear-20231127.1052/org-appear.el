;;; org-appear.el --- Auto-toggle Org elements -*- lexical-binding: t; -*-

;; Portions of code in this file are taken from org-fragtog https://github.com/io12/org-fragtog
;; org-fragtog Copyright (C) 2020 Benjamin Levy - MIT/X11 License
;; org-appear Copyright (C) 2021 Alice Istleyeva - MIT License
;; Author: Alice Istleyeva <awth13@gmail.com>
;; Version: 0.3.0
;; Description: Toggle Org mode element visibility upon entering and leaving
;; Homepage: https://github.com/awth13/org-appear
;; Package-Requires: ((emacs "25.1") (org "9.3"))

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This package enables automatic visibility toggling of various Org elements depending on cursor position.
;; It supports automatic toggling of emphasis markers, links, subscripts and
;; superscripts, entities, and keywords.  By default, toggling is instantaneous
;; and only affects emphasis markers.  If Org mode custom variables that control
;; visibility of elements are configured to show hidden parts, the respective
;; `org-appear' settings do not have an effect.

;;; Code:

(require 'org)
(require 'org-element)
(require 'subr-x)			; Compatibility

(defgroup org-appear nil
  "Auto-toggle Org elements."
  :group 'org)

(defcustom org-appear-trigger 'always
  "Method of triggering element toggling.
`always' means that elements are toggled every time they are under the cursor.
`on-change' means that elements are toggled only when the buffer is modified
or when the element under the cursor is clicked with a mouse.
`manual' means that toggling starts on call to `org-appear-manual-start'
and stops on call to `org-appear-manual-stop'."
  :type '(choice (const :tag "Always" always)
		 (const :tag "Only on change" on-change)
		 (const :tag "Manual" manual))
  :group 'org-appear)

(defcustom org-appear-autoemphasis t
  "Non-nil enables automatic toggling of emphasised and verbatim markers.
Does not have an effect if `org-hide-emphasis-markers' is nil."
  :type 'boolean
  :group 'org-appear)

(defcustom org-appear-autosubmarkers nil
  "Non-nil enables automatic toggling of subscript and superscript markers.
Does not have an effect if `org-pretty-entities' is nil."
  :type 'boolean
  :group 'org-appear)

(defcustom org-appear-autoentities nil
  "Non-nil enables automatic toggling of org entities.
Does not have an effect if `org-pretty-entities' is nil."
  :type 'boolean
  :group 'org-appear)

(defcustom org-appear-autolinks nil
  "Non-nil enables automatic toggling of links.
If set to the symbol `just-brackets', links will be shown with
brackets around them without showing the link target.  Does not
have an effect if `org-link-descriptive' is nil."
  :type '(choice (const :tag "Ignore links" nil)
		 (const :tag "Toggle full link" t)
		 (const :tag "Toggle brackets" just-brackets))
  :group 'org-appear)

(defcustom org-appear-autokeywords nil
  "Non-nil enables automatic toggling of keywords.
Does not have an effect if `org-hidden-keywords' is nil."
  :type 'boolean
  :group 'org-appear)

(defcustom org-appear-inside-latex nil
  "Also applies toggling inside Latex fragments and environments.
Sub- and superscript markers are toggled if `org-appear-autosubmarkers' is
non-nil.  Entities are toggled if `org-appear-autoentities' is non-nil."
  :type 'boolean
  :group 'org-appear)

(defcustom org-appear-delay 0.0
  "Seconds of delay before toggling an element."
  :type 'number
  :group 'org-appear)

(defcustom org-appear-manual-linger nil
  "Whether to hide the element on manual stop.
If true, hide upon navigating away."
  :type 'boolean
  :group 'org-appear)

(defvar-local org-appear--timer nil
  "Current active timer.")

;;;###autoload
(define-minor-mode org-appear-mode
  "A minor mode that automatically toggles elements in Org mode."
  :init-value nil
  :lighter nil
  :keymap nil

  (cond
   (org-appear-mode
    (org-appear--set-elements)
    (add-hook 'post-command-hook #'org-appear--post-cmd nil t)
    (add-hook 'pre-command-hook #'org-appear--pre-cmd nil t)
    (when (eq org-appear-trigger 'on-change)
      (add-hook 'mouse-leave-buffer-hook #'org-appear--after-change nil t)
      (add-hook 'after-change-functions #'org-appear--after-change nil t)))
   (t
    ;; Clean up current element when disabling the mode
    (when-let ((current-elem (org-appear--current-elem)))
      (org-appear--hide-invisible current-elem)
      (when org-appear--timer
	(cancel-timer org-appear--timer)
	(setq org-appear--timer nil)))
    (remove-hook 'post-command-hook #'org-appear--post-cmd t)
    (remove-hook 'pre-command-hook #'org-appear--pre-cmd t)
    (when (eq org-appear-trigger 'on-change)
      (remove-hook 'mouse-leave-buffer-hook #'org-appear--after-change t)
      (remove-hook 'after-change-functions #'org-appear--after-change t)))))

(defvar org-appear-elements nil
  "List of Org elements to toggle.")

(defvar-local org-appear--prev-elem nil
  "Previous element that surrounded the cursor.
nil if the cursor was not on an element.")

(defvar-local org-appear--do-buffer nil
  "Non-nil when `org-appear-mode' is notified to start toggling.")

(defvar-local org-appear--elem-toggled nil
  "Non-nil if the last encountered element has been toggled.")

(defun org-appear--set-elements ()
  "Add elements to toggle to `org-appear-elements'."
  (let ((emphasis-elements '(bold
			     italic
			     underline
			     strike-through
			     verbatim
			     code))
	(script-elements '(subscript
			   superscript))
	(entity-elements '(entity))
	(link-elements '(link))
	(keyword-elements '(keyword))
	(latex-elements '(latex-fragment latex-environment)))

    ;; HACK: is there a better way to do this?
    (setq-local org-appear--prev-elem nil)
    (setq org-appear-elements nil)	; reset
    (when (and org-hide-emphasis-markers org-appear-autoemphasis)
      (setq org-appear-elements (append org-appear-elements emphasis-elements)))
    (when (and org-pretty-entities org-appear-autosubmarkers)
      (setq org-appear-elements (append org-appear-elements script-elements)))
    (when (and org-pretty-entities org-appear-autoentities)
      (setq org-appear-elements (append org-appear-elements entity-elements)))
    (when (and org-link-descriptive org-appear-autolinks)
      (setq org-appear-elements (append org-appear-elements link-elements)))
    (when (and org-hidden-keywords org-appear-autokeywords)
      (setq org-appear-elements (append org-appear-elements keyword-elements)))
    (when org-appear-inside-latex
      (setq org-appear-elements (append org-appear-elements latex-elements)))))

(defun org-appear--post-cmd ()
  "This function is executed by `post-command-hook' in `org-appear-mode'.
It handles toggling elements depending on whether the cursor entered or exited them."
  (let* ((prev-elem org-appear--prev-elem)
	 (prev-elem-start (org-element-property :begin prev-elem))
	 (current-elem (org-appear--current-elem))
	 (current-elem-start (org-element-property :begin current-elem)))

    ;; After leaving an element
    (when (and prev-elem
	       org-appear--elem-toggled
	       (not (equal prev-elem-start current-elem-start)))

      ;; Forget element
      (setq org-appear--elem-toggled nil)

      ;; If timer for prev-elem fired and was expired
      (if (not org-appear--timer)
	  (save-excursion
	    (goto-char prev-elem-start)
	    ;; Reevaluate `org-element-context' in case the bounds
	    ;; of the previous element changed
	    (org-appear--hide-invisible (org-element-context)))
	(cancel-timer org-appear--timer)
	(setq org-appear--timer nil)))

    ;; Inside an element
    (when (and current-elem (or (eq org-appear-trigger 'always)
				org-appear--do-buffer
				org-appear--elem-toggled))

      ;; Mark element as toggled to continue toggling and ignore buffer state
      (setq org-appear--elem-toggled t)

      ;; New element, delay first unhiding
      (when (and (eq org-appear-trigger 'always)
		 (> org-appear-delay 0)
		 (not (eq prev-elem-start current-elem-start)))
	(setq org-appear--timer (run-with-idle-timer org-appear-delay
						     nil
						     #'org-appear--show-with-lock
						     current-elem
						     t)))

      ;; Not a new element
      (when (not org-appear--timer)
	(org-appear--show-with-lock current-elem)))

    (setq org-appear--prev-elem current-elem)
    (when (not (eq org-appear-trigger 'manual))
      (setq org-appear--do-buffer nil))))

(defun org-appear--after-change (&rest _args)
  "This function is executed by `after-change-functions' in `org-appear-mode'.
It signals that elements in the current buffer must be toggled."
  (setq org-appear--do-buffer 't))

(defun org-appear-manual-start ()
  "Signal that elements in the current buffer must be toggled."
  (setq org-appear--do-buffer 't))

(defun org-appear-manual-stop ()
  "Signal that elements in the current buffer must no longer be toggled."
  (when (not org-appear-manual-linger)
    (when-let ((current-elem (org-appear--current-elem)))
      (org-appear--hide-invisible current-elem))
    (setq org-appear--elem-toggled nil))
  (setq org-appear--do-buffer nil))

(defun org-appear--pre-cmd ()
  "This function is executed by `pre-command-hook' in `org-appear-mode'.
It hides elements before commands that modify the buffer based on column width."
  (when (memq this-command '(org-fill-paragraph
			     org-ctrl-c-ctrl-c))
    (when-let ((current-elem (org-appear--current-elem)))
      (org-appear--hide-invisible current-elem))))

(defun org-appear--current-elem ()
  "Return element at point.
Return nil if element is not supported by `org-appear-mode'."
  (when-let ((elem (org-element-context)))
    (let* ((elem-type (car elem))
	   (elem-end (- (org-element-property :end elem)
			(1- (org-element-property :post-blank elem))))
	   (link-ignore-p (and (eq elem-type 'link)
			       (or (string-match-p "[Cc]ite"
						   (org-element-property :type elem))
				   (memq (org-element-property :format elem)
					 '(plain angle)))))
	   (key-ignore-p (and (eq elem-type 'keyword)
			      (not (memq (intern (downcase
						  (org-element-property :key elem)))
					 org-hidden-keywords))))
	   (script-ignore-p (and (or (eq elem-type 'subscript)
				     (eq elem-type 'superscript))
				 (not (org-element-property :use-brackets-p elem))
				 (not (eq org-use-sub-superscripts t)))))
      (if (and (memq elem-type org-appear-elements)
	       (< (point) elem-end)     ; Ignore post-element whitespace
	       (not link-ignore-p)	; Ignore plain and org-ref links
	       (not key-ignore-p)	; Ignore unhidden keywords
	       (not script-ignore-p))	; Ignore sub/supercripts ignored by Org
	  elem
	nil))))

(defun org-appear--parse-elem (elem)
  "Return bounds of element ELEM.
Return nil if element cannot be parsed."
  (let* ((elem-type (car elem))
	 (elem-tag (cond ((memq elem-type '(bold
					    italic
					    underline
					    strike-through
					    verbatim
					    code))
			  'emph)
			 ((memq elem-type '(subscript
					    superscript))
			  'script)
			 ((eq elem-type 'entity)
			  'entity)
			 ((eq elem-type 'link)
			  'link)
			 ((eq elem-type 'keyword)
			  'keyword)
			 ((memq elem-type '(latex-fragment latex-environment))
			  'latex-fragment)
			 (t nil)))
	 (elem-start (org-element-property :begin elem))
	 (elem-end (org-element-property :end elem))
	 (elem-content-start (org-element-property :contents-begin elem))
	 (elem-content-end (org-element-property :contents-end elem))
	 ;; Some elements have extra spaces at the end
	 ;; The number of spaces is stored in the post-blank property
	 (post-elem-spaces (org-element-property :post-blank elem))
	 (elem-end-real (- elem-end post-elem-spaces)))
    ;; Only sub/superscript elements are guaranteed to have
    ;; contents-begin and contents-end properties
    (when elem-tag
      `(:start ,elem-start
	       :end ,elem-end-real
	       :visible-start ,(pcase elem-tag
				 ('emph (1+ elem-start))
				 ('script elem-content-start)
				 ('link (or elem-content-start (+ elem-start 2))))
	       :visible-end ,(pcase elem-tag
			       ('emph (1- elem-end-real))
			       ('script elem-content-end)
			       ('link (or elem-content-end (- elem-end-real 2))))))))

(defun org-appear--show-invisible (elem)
  "Silently remove invisible property from invisible parts of element ELEM."
  (let* ((elem-at-point (org-appear--parse-elem elem))
	 (elem-type (car elem))
	 (start (plist-get elem-at-point :start))
	 (end (plist-get elem-at-point :end))
	 (visible-start (plist-get elem-at-point :visible-start))
	 (visible-end (plist-get elem-at-point :visible-end)))
    (when (and (eq org-appear-autolinks 'just-brackets)
	       (eq elem-type 'link))
      (setq start (1- visible-start))
      (setq end (1+ visible-end)))
    (with-silent-modifications
      (cond ((eq elem-type 'entity)
	     (decompose-region start end))
	    ((memq elem-type '(latex-fragment latex-environment))
	     (when org-appear-autosubmarkers
	       (remove-text-properties start end '(invisible)))
	     (when org-appear-autoentities
	       (decompose-region start end)))
	    ((eq elem-type 'keyword)
	     (remove-text-properties start end '(invisible org-link)))
	    ((and (featurep 'org-fold)
		  (eq elem-type 'link))
	     (remove-text-properties start
				     visible-start
				     (list (org-fold-core--property-symbol-get-create 'org-link) nil))
	     (remove-text-properties visible-end
				     end
				     (list (org-fold-core--property-symbol-get-create 'org-link) nil)))
	    (t
	     (remove-text-properties start visible-start '(invisible org-link))
	     (remove-text-properties visible-end end '(invisible org-link)))))))

(defun org-appear--show-with-lock (elem &optional renew)
  "Show invisible parts of element ELEM.
When RENEW is non-nil, obtain element at point instead."
  ;; When called with timer, element might be different upon arrival
  (when renew
    (setq elem (org-appear--current-elem))
    (setq org-appear--prev-elem elem)
    (setq org-appear--timer nil))

  (when-let ((elem-start (org-element-property :begin elem))
	     (elem-end (org-element-property :end elem)))
    ;; Call `font-lock-ensure' before unhiding to prevent `jit-lock-mode'
    ;; from refontifying the element region after changes in buffer
    (font-lock-ensure elem-start elem-end)
    (org-appear--show-invisible elem)))

(defun org-appear--hide-invisible (elem)
  "Silently add invisible property to invisible parts of element ELEM."
  (let* ((elem-at-point (org-appear--parse-elem elem))
	 (elem-type (car elem))
	 (start (plist-get elem-at-point :start))
	 (end (plist-get elem-at-point :end))
	 (visible-start (plist-get elem-at-point :visible-start))
	 (visible-end (plist-get elem-at-point :visible-end)))
    (when elem-at-point
      (when (and (eq org-appear-autolinks 'just-brackets)
		 (eq elem-type 'link))
	(setq start (1- visible-start))
	(setq end (1+ visible-end)))
      (with-silent-modifications
	(cond ((eq elem-type 'entity)
	       (compose-region start end (org-element-property :utf-8 elem))
	       (font-lock-flush start end))
	      ((memq elem-type '(keyword latex-fragment latex-environment))
	       (font-lock-flush start end))
	      ((and (featurep 'org-fold)
		    (eq elem-type 'link))
	       (put-text-property start
				  visible-start
				  (org-fold-core--property-symbol-get-create 'org-link)
				  'org-link)
	       (put-text-property visible-end
				  end
				  (org-fold-core--property-symbol-get-create 'org-link)
				  'org-link))
	      (t
	       (put-text-property start visible-start 'invisible 'org-link)
	       (put-text-property visible-end end 'invisible 'org-link)
	       (font-lock-flush start end))))
      ;; Call `font-lock-ensure' after flushing to prevent `jit-lock-mode'
      ;; from refontifying the next element entered
      (font-lock-ensure start end))))

(provide 'org-appear)
;;; org-appear.el ends here
