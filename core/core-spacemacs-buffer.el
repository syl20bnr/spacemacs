;;; core-spacemacs-buffer.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(defconst spacemacs-buffer-name "*spacemacs*"
  "The name of the spacemacs buffer.")

(defun spacemacs//insert-banner ()
  "Choose a banner and insert in spacemacs buffer.

Doge special banner can be reachable via `999', `doge' or `random*'.
`random' ignore special banners whereas `random*' does not."
  (let ((banner (cond
                 ((eq 'random dotspacemacs-startup-banner)
                  (spacemacs//choose-random-banner))
                 ((eq 'random* dotspacemacs-startup-banner)
                  (spacemacs//choose-random-banner t))
                 ((eq 'doge dotspacemacs-startup-banner)
                  (spacemacs//get-banner-path 999))
                 ((integerp dotspacemacs-startup-banner)
                  (spacemacs//get-banner-path dotspacemacs-startup-banner))))
        (buffer-read-only nil))
    (when banner
      (spacemacs/message (format "Banner: %s" banner))
      (insert-file-contents banner)
      (spacemacs//inject-version-in-buffer)
      (spacemacs/insert-buttons))))

(defun spacemacs//choose-random-banner (&optional all)
  "Return the full path of a banner chosen randomly.

If ALL is non-nil then truly all banners can be selected."
  (let* ((files (directory-files spacemacs-banner-directory t))
         (count (length files))
         ;; -2 then +2 to remove `.' and `..'
         (choice (+ 2 (random (- count (if all 2 3))))))
    (nth choice files)))

(defun spacemacs//get-banner-path (index)
  "Return the full path to banner with index INDEX."
  (concat spacemacs-banner-directory (format "%03d-banner.txt" index)))

(defun spacemacs//inject-version-in-buffer ()
  "Inject the current version of spacemacs in the first line of the
buffer, right justified."
  (save-excursion
    (beginning-of-buffer)
    (let* ((maxcol spacemacs-title-length)
           (injected (format "(%s)" spacemacs-version))
           (pos (- maxcol (length injected)))
           (buffer-read-only nil))
      ;; fill the first line with spaces if required
      (when (< (line-end-position) maxcol)
        (end-of-line)
        (insert-char ?\s (- maxcol (line-end-position))))
      (goto-char pos)
      (delete-char (length injected))
      (insert injected))))

(defun spacemacs/message (msg &rest args)
  "Display MSG in message prepended with '(Spacemacs)'."
  (message "(Spacemacs) %s" (apply 'format msg args)))

(defun spacemacs/append-to-buffer (msg &optional messagebuf)
  "Append MSG to spacemacs buffer. If MESSAGEBUF is not nil then MSG is
 also written in message buffer."
  (with-current-buffer (get-buffer-create "*spacemacs*")
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert msg)
      (if messagebuf (message "(Spacemacs) %s" msg)))))

(defun spacemacs/replace-last-line-of-buffer (msg &optional messagebuf)
  "Replace the last line of the spacemacs buffer with MSG. If MESSAGEBUF is
 not nil then MSG is also written in message buffer."
  (with-current-buffer (get-buffer-create "*spacemacs*")
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (delete-region (line-beginning-position) (point-max))
      (insert msg)
      (if messagebuf (message "(Spacemacs) %s" msg)))))

(defun spacemacs/loading-animation ()
  "Display LOADING-TITLE with trailing dots of max length
SPACEMACS-TITLE-LENGTH. New loading title is displayed by chunk
of size LOADING-DOTS-CHUNK-THRESHOLD."
  (setq spacemacs-loading-counter (1+ spacemacs-loading-counter))
  (when (>= spacemacs-loading-counter spacemacs-loading-dots-chunk-threshold)
    (setq spacemacs-loading-counter 0)
    (let ((i 0))
      (while (< i spacemacs-loading-dots-chunk-size)
        (setq spacemacs-loading-text (concat spacemacs-loading-text "."))
        (setq i (1+ i))))
    (spacemacs/replace-last-line-of-buffer spacemacs-loading-text)
    (redisplay)))

(defun spacemacs/insert-buttons ()
  (goto-char (point-max))
  (insert "      ")
  (insert-button "[Homepage]" 'action
                 (lambda (b) (browse-url "https://github.com/syl20bnr/spacemacs"))
                 'follow-link t)
  (insert " ")
  (insert-button "[Documentation]" 'action
                 (lambda (b) (browse-url "https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.md"))
                 'follow-link t)
  (insert " ")
  (insert-button "[Gitter Chat]" 'action
                 (lambda (b) (browse-url "https://gitter.im/syl20bnr/spacemacs"))
                 'follow-link t)
  (insert " ")
  (insert-button "[Update]" 'action
                 (lambda (b) (configuration-layer/update-packages)) 'follow-link t)
  (insert " ")
  (insert-button "[Rollback]" 'action
                 (lambda (b) (call-interactively 'configuration-layer/rollback)) 'follow-link t)
  (insert "\n\n")
  )

(defun spacemacs/goto-link-line ()
  "Move the point to the beginning of the link line."
  (interactive)
  (with-current-buffer spacemacs-buffer-name
    (goto-char (point-min))
    (re-search-forward "Homepage")
    (beginning-of-line)))

(provide 'core-spacemacs-buffer)
