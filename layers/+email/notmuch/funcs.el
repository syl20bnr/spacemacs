(defun spacemacs/notmuch-inbox-p (saved-search-property-item)
  (string-equal (plist-get saved-search-property-item :name) "inbox"))

(defun spacemacs/notmuch-inbox ()
  (interactive)
  (notmuch-search (plist-get (nth 0
                                  (-filter 'spacemacs/notmuch-inbox-p notmuch-saved-searches))
                             :query)))

(defun spacemacs/notmuch-search-archive-thread-down ()
  (interactive)
  (notmuch-search-archive-thread))

(defun spacemacs/notmuch-search-archive-thread-up ()
  (interactive)
  (notmuch-search-archive-thread)
  (notmuch-search-previous-thread)
  (notmuch-search-previous-thread))

(defun spacemacs/notmuch-message-delete (go-next)
  (interactive)
  (notmuch-search-tag '("+deleted" "-inbox" "-unread"))

  (if (equal go-next "up")
      (notmuch-search-previous-thread)
    (notmuch-search-next-thread)))
(defun spacemacs/notmuch-message-delete-down ()
  (interactive)
  (spacemacs/notmuch-message-delete "down"))

(defun spacemacs/notmuch-message-delete-up ()
  (interactive)
  (spacemacs/notmuch-message-delete "up"))

(defun spacemacs/notmuch-show-close-all ()
  (interactive)
  (goto-char (point-min))
  (setq current-prefix-arg '(4))
  (call-interactively 'notmuch-show-open-or-close-all))


;; Thanks to Kyle Meyer (@kyleam)
(defun spacemacs/notmuch-open-github-patch (buffer)
  "Find GitHub patch link in BUFFER and show it in a new buffer."
  (let ((url
         (with-current-buffer buffer
           (save-excursion
             (goto-char (point-min))
             (if (re-search-forward "https://github.com/.*\\.patch" nil t)
                 (match-string-no-properties 0)
               (user-error "No patch found"))))))
    (with-current-buffer (get-buffer-create
                          (generate-new-buffer-name "*mail-github-patch*"))

      (condition-case exception
          (url-insert-file-contents url)
        ('file-error
         ;; In case the link is private repository github will respond with a
         ;; temporary redirect 302 HTTP code and calculate the request-token
         ;; with javascript. In this case open diff in browser
         (browse-url url)))

      (diff-mode)
      (view-mode 1)
      (pop-to-buffer (current-buffer)))))

(defun spacemacs/notmuch-show-open-github-patch ()
  "Open patch from GitHub email."
  (interactive)
  (with-current-notmuch-show-message
   (spacemacs/notmuch-open-github-patch (current-buffer))))



;;;;;;;;;
;; git ;;
;;;;;;;;;

(defun spacemacs/notmuch-git-apply-patch (entire-thread)
  "Apply patch from a notmuch-show email buffer to a git repository

If ENTIRE-THREAD is non-nil it will apply patches from all open
messages in the current thread"
  (interactive "P")
  (notmuch-show-pipe-message entire-thread "git am"))

(defun spacemacs/notmuch-git-apply-patch-part ()
  "Apply patch attached to a message as MIME part to a git repository."
  (interactive)
  (let ((mime-type nil))
    (notmuch-show-apply-to-current-part-handle (lambda ()
                                                 (mm-pipe-part (notmuch-show-current-part-handle mime-type) "git am")))))


