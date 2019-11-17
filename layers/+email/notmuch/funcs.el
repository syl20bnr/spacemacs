;;; funcs.el --- Notmuch Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//notmuch-inbox-p (saved-search-property-item)
  "Returns non-nil if item is the inbox."
  (string-equal (plist-get saved-search-property-item :name) "inbox"))

(defun spacemacs/notmuch-inbox ()
  "Search inbox."
  (interactive)
  (notmuch-search
   (plist-get (nth 0 (-filter 'spacemacs//notmuch-inbox-p notmuch-saved-searches))
              :query)))

(defun spacemacs/notmuch-search-archive-thread-down ()
  "Search thread up."
  (interactive)
  (notmuch-search-archive-thread))

(defun spacemacs/notmuch-search-archive-thread-up ()
  "Search thread down."
  (interactive)
  (notmuch-search-archive-thread)
  (notmuch-search-previous-thread)
  (notmuch-search-previous-thread))

(defun spacemacs//notmuch-message-delete (go-next)
  "Delete message and select GO-NEXT message."
  (notmuch-search-tag notmuch-message-deleted-tags)
  (if (eq 'up go-next )
      (notmuch-search-previous-thread)
    (notmuch-search-next-thread)))

(defun spacemacs/notmuch-show-as-patch ()
  (interactive)
  (notmuch-show-choose-mime-of-part "text/x-patch"))

(defun spacemacs/notmuch-message-delete-down ()
  "Delete a message and select the next message."
  (interactive)
  (spacemacs//notmuch-message-delete 'down))

(defun spacemacs/notmuch-message-delete-up ()
  "Delete a message and select the previous message."
  (interactive)
  (spacemacs//notmuch-message-delete 'up))

(defun spacemacs/notmuch-show-close-all ()
  "Close all."
  (interactive)
  (goto-char (point-min))
  (let ((current-prefix-arg '(4)))
    (call-interactively 'notmuch-show-open-or-close-all)))


;; git

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
    (notmuch-show-apply-to-current-part-handle
     (lambda ()
       (mm-pipe-part (notmuch-show-current-part-handle mime-type) "git am")))))


;; GitHub

;; Thanks to Kyle Meyer (@kyleam)
(defun spacemacs//notmuch-open-github-patch (buffer)
  "Find GitHub patch link in BUFFER and show it in a new buffer."
  (let ((url
         (with-current-buffer buffer
           (save-excursion
             (goto-char (point-min))
             (if (re-search-forward "https://github.com/.*\\.patch" nil t)
                 (match-string-no-properties 0)
               (if (re-search-forward "https://github.com/[^/]+/[^/]+/pull/[0-9]+" nil t)
                   (concat (match-string-no-properties 0) ".patch")
                 (user-error "No patch found")))))))
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
      (pop-to-buffer-same-window (current-buffer)))))

(defun spacemacs/notmuch-show-open-github-patch ()
  "Open patch from GitHub email."
  (interactive)
  (with-current-notmuch-show-message
   (spacemacs//notmuch-open-github-patch (current-buffer))))


;; persp

(defun spacemacs//notmuch-persp-filter-save-buffers-function (buffer)
  "Filter for notmuch layout."
  (with-current-buffer buffer
    (memq major-mode notmuch-modes)))

(defun spacemacs//notmuch-buffer-to-persp ()
  "Add buffer to notmuch layout."
  (persp-add-buffer (current-buffer)
                    (persp-get-by-name notmuch-spacemacs-layout-name)))
