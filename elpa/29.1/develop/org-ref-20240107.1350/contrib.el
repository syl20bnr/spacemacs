;;; contrib.el --- Code contributed by users

;;; Commentary:
;; 

;;; Code:

;; * Add messages in minibuffer
;; Contributed in https://github.com/jkitchin/org-ref/issues/938 by @DiogoFerrari


(defun org-ref-get-bibtex-key-under-cursor--display ()
  "Return key under the cursor in `org-mode'.
If not on a key, but on a cite, prompt for key."
  (if-let ((key (get-text-property (point) 'cite-key)))
      ;; Point is on a key, so we get it directly
      key
    ;; point is not on a key, but may still be on a cite link
    (let ((el (org-element-context))
	  data
	  keys)
      (cond
       ;; on a cite-link type
       ((and
	 (eq (org-element-type el) 'link)
	 (assoc (org-element-property :type el) org-ref-cite-types))

	(goto-char (org-element-property :begin el))
	(setq data (org-ref-parse-cite-path (org-element-property :path el))
	      keys (cl-loop for ref in (plist-get data :references)
			    collect (plist-get ref :key)))
	(setq text nil)
	(dolist (key keys)
	  (search-forward key)
	  (goto-char (match-beginning 0))
	  (get-text-property (point) 'cite-key)
	  ;; (message (bibtex-completion-apa-format-reference key))
	  (setq text (concat text "\n" (bibtex-completion-apa-format-reference key))))))))
  (message (string-trim-left text)))


(defvar org-ref-message-timer nil
  "Stores the idle timer for cite minibuffer messages.")


(defcustom org-ref-message-interval 0.5
  "Time in seconds to wait for the idle timer that displays the cite message."
  :group 'org-ref)


(defun org-ref-link-message ()
  "Display a message in the minibuffer when point is on a cite link."
  (when (and (eq major-mode 'org-mode) (eq (get-text-property (point) 'help-echo) 'org-ref-cite-tooltip))
    (save-excursion (org-ref-get-bibtex-key-under-cursor--display))))


(defun org-ref-messages-on ()
  "Turn cite messages to minibuffer on."
  (interactive)
  (setq org-ref-message-timer (run-with-idle-timer org-ref-message-interval 0 'org-ref-link-message)))


(defun org-ref-messages-off ()
  "Turn cite messages to minibuffer off."
  (interactive)
  (when org-ref-message-timer
    (cancel-timer org-ref-message-timer)
    (setq org-ref-message-timer nil)))


(provide 'contrib)

;;; contrib.el ends here
