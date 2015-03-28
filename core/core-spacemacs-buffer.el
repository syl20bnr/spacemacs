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

(defconst spacemacs--banner-length 75
  "Width of a banner.")

(defun spacemacs//insert-banner-and-buttons ()
  "Choose a banner accordingly to `dotspacemacs-startup-banner'and insert it
in spacemacs buffer along whith quick buttons underneath.

Easter egg:
Doge special text banner can be reachable via `999', `doge' or `random*'.
`random' ignore special banners whereas `random*' does not."
  (let ((banner (spacemacs//choose-banner))
        (buffer-read-only nil))
    (when banner
      (spacemacs/message (format "Banner: %s" banner))
      (if (string-match "\\.png\\'" banner)
          (spacemacs//insert-image-banner banner)
        (insert-file-contents banner))
      (spacemacs//inject-version)
      (spacemacs/insert-buttons)
      (spacemacs//redisplay))))

(defun spacemacs//choose-banner ()
  "Return the full path of a banner based on the dotfile value."
  (cond
   ((eq 'official dotspacemacs-startup-banner)
    (if (and (display-graphic-p) (image-type-available-p 'png))
        spacemacs-banner-official-png
      (spacemacs//get-banner-path 1)))
   ((eq 'random dotspacemacs-startup-banner)
    (spacemacs//choose-random-text-banner))
   ((eq 'random* dotspacemacs-startup-banner)
    (spacemacs//choose-random-text-banner t))
   ((eq 'doge dotspacemacs-startup-banner)
    (spacemacs//get-banner-path 999))
   ((integerp dotspacemacs-startup-banner)
    (spacemacs//get-banner-path dotspacemacs-startup-banner))
   ((and dotspacemacs-startup-banner
         (string-match "\\.png\\'" dotspacemacs-startup-banner))
    (if (and (display-graphic-p) (image-type-available-p 'png))
        (if (file-exists-p dotspacemacs-startup-banner)
            dotspacemacs-startup-banner
          (spacemacs/message (format "Warning: could not find banner %s"
                                     dotspacemacs-startup-banner))
          (spacemacs//get-banner-path 1))
      (spacemacs//get-banner-path 1)))))

(defun spacemacs//choose-random-text-banner (&optional all)
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

(defun spacemacs//insert-image-banner (banner)
  "Display an image banner."
  (when (file-exists-p banner)
    (let* ((spec (create-image banner))
           (size (image-size spec))
           (width (car size))
           (left-margin (floor (- spacemacs--banner-length width) 2)))
      (beginning-of-buffer)
      (insert "\n")
      (insert (make-string (- left-margin 1) ?\ ))
      (insert-image spec)
      (insert "\n\n")
      (insert "                           [S P A C E M A C S]\n\n"))))

(defun spacemacs//inject-version ()
  "Inject the current version of spacemacs in the first line of the
buffer, right justified."
  (save-excursion
    (beginning-of-buffer)
    (let* ((maxcol spacemacs--banner-length)
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

(defun spacemacs/set-mode-line (format)
  "Set mode-line format for spacemacs buffer."
  (with-current-buffer (get-buffer-create "*spacemacs*")
    (setq mode-line-format format)))

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
      (if messagebuf (message "(Spacemacs) %s" msg)))
    (spacemacs/set-mode-line "")))

(defun spacemacs/replace-last-line-of-buffer (msg &optional messagebuf)
  "Replace the last line of the spacemacs buffer with MSG. If MESSAGEBUF is
 not nil then MSG is also written in message buffer."
  (with-current-buffer (get-buffer-create "*spacemacs*")
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (delete-region (line-beginning-position) (point-max))
      (insert msg)
      (if messagebuf (message "(Spacemacs) %s" msg)))
    (spacemacs/set-mode-line "")))

(defun spacemacs/loading-animation ()
  "Display the progress bar by chunk of size `spacemacs--loading-dots-chunk-threshold'."
  (when dotspacemacs-loading-progress-bar
    (setq spacemacs-loading-counter (1+ spacemacs-loading-counter))
    (when (>= spacemacs-loading-counter spacemacs-loading-dots-chunk-threshold)
      (setq spacemacs-loading-counter 0)
      (setq spacemacs-loading-string
            (concat spacemacs-loading-string
                    (make-string spacemacs-loading-dots-chunk-size
                                 spacemacs-loading-char)))
      (setq mode-line-format spacemacs-loading-string)
      (spacemacs//redisplay))))

(defun spacemacs/insert-buttons ()
  (goto-char (point-max))
  (insert "      ")
  (insert-button "[Homepage]" 'action
                 (lambda (b) (browse-url "https://github.com/syl20bnr/spacemacs"))
                 'follow-link t 'help-echo "Open the Spacemacs Github page in your browser.")
  (insert " ")
  (insert-button "[Documentation]" 'action
                 (lambda (b) (browse-url "https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.md"))
                 'follow-link t 'help-echo "Open the Spacemacs documentation in your browser.")
  (insert " ")
  (insert-button "[Gitter Chat]" 'action
                 (lambda (b) (browse-url "https://gitter.im/syl20bnr/spacemacs"))
                 'follow-link t 'help-echo "Ask questions and chat with fellow users in our chat room.")
  (insert " ")
  (insert-button "[Update]" 'action
                 (lambda (b) (configuration-layer/update-packages))
                 'follow-link t 'help-echo "Update all ELPA packages to the latest versions.")
  (insert " ")
  (insert-button "[Rollback]" 'action
                 (lambda (b) (call-interactively 'configuration-layer/rollback))
                 'follow-link t 'help-echo "Rollback ELPA package upgrades if something got borked.")
  (insert "\n")
  (let ((button-title "[Search in Spacemacs]"))
    ; Compute the correct number of spaces to center the button.
    (dotimes (i (/ (- spacemacs--banner-length (string-width button-title) 1) 2)) (insert " "))
    (insert-button button-title 'action
                   (lambda (b) (call-interactively 'helm-spacemacs)) 'follow-link t
                   'help-echo "Find Spacemacs package and layer configs using helm-spacemacs."))
  (insert "\n\n")
  )

(defun spacemacs//insert-file-list (list-display-name list)
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (insert "\n    ")
            (insert-button el
                           'action `(lambda (b) (find-file-existing ,el))
                           'follow-link t))
          list)))

(defun spacemacs/insert-startupify-lists ()
  (interactive)
  (with-current-buffer (get-buffer-create "*spacemacs*")
    (let ((buffer-read-only nil)
          (list-separator "\n\n"))
      (goto-char (point-max))

      (mapc (lambda (el)
              (cond
               ((equal el 'recents)
                (recentf-mode)
                (when (spacemacs//insert-file-list "  Recent Files:" (recentf-elements 5))
                  (insert list-separator)))
               ((equal el 'bookmarks)
                (helm-mode)
                (when (spacemacs//insert-file-list "  Bookmarks:" (bookmark-all-names))
                  (insert list-separator)))
               ((equal el 'projects)
                (projectile-mode)
                (when (spacemacs//insert-file-list "  Projects:" (projectile-relevant-known-projects))
                  (insert list-separator))))) dotspacemacs-startup-lists))))

(defun spacemacs/goto-link-line ()
  "Move the point to the beginning of the link line."
  (interactive)
  (when (and dotspacemacs-startup-banner
             (not configuration-layer-error-count))
    (with-current-buffer spacemacs-buffer-name
      (goto-char (point-min))
      (re-search-forward "Homepage")
      (beginning-of-line))))

(provide 'core-spacemacs-buffer)
