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

(defun spacemacs/notmuch-trash (&optional beg end)
  "trash by removing inbox and adding trash"
  (interactive (notmuch-search-interactive-region))
  (notmuch-search-tag (list "-inbox" "+trash")
                      beg
                      end)
  (when (eq beg end)
    (notmuch-search-next-thread)))

(defun spacemacs/notmuch-trash-show ()
  "trash shown msg by removing inbox and adding trash"
  (interactive)
  (notmuch-show-add-tag (list "-inbox" "+trash"))
  (unless (notmuch-show-next-open-message)
    (defun spacemacs/compose-mail-other-frame ()
      "create a new frame for the mail composition"
      (compose-mail-other-frame))
    (notmuch-show-next-thread t)))


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


