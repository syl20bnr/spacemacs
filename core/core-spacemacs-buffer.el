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

(defconst spacemacs-buffer--banner-length 75
  "Width of a banner.")

(defconst spacemacs-buffer--cache-file
  (expand-file-name (concat spacemacs-cache-directory "spacemacs-buffer.el"))
  "Cache file for various persistent data for the spacemacs startup buffer")

(defvar spacemacs-buffer--release-note-version nil
  "If nil the release note is displayed. If non nil it contains
a version number, if the version number is lesser than the current
version the release note it displayed")

(defvar spacemacs-buffer--note-widgets nil
  "List of widgets used to display the release note.")

(defvar spacemacs-buffer--previous-insert-type nil
  "Previous type of note inserted.")

(defun spacemacs-buffer/insert-banner-and-buttons ()
  "Choose a banner accordingly to `dotspacemacs-startup-banner'and insert it
in spacemacs buffer along whith quick buttons underneath.

Easter egg:
Doge special text banner can be reachable via `999', `doge' or `random*'.
`random' ignore special banners whereas `random*' does not."
  (let ((banner (spacemacs-buffer//choose-banner))
        (buffer-read-only nil))
    (when banner
      (spacemacs-buffer/message (format "Banner: %s" banner))
      (if (image-type-available-p (intern (file-name-extension banner)))
          (spacemacs-buffer//insert-image-banner banner)
        (insert-file-contents banner))
      (spacemacs-buffer//inject-version)
      (spacemacs-buffer//insert-buttons)
      (if (file-exists-p spacemacs-buffer--cache-file)
          (load spacemacs-buffer--cache-file)
        (unless (file-exists-p dotspacemacs-filepath)
          ;; fresh install of spacemacs, the release notes are not displayed
          (setq spacemacs-buffer--release-note-version spacemacs-version)
          (spacemacs/dump-vars-to-file
           '(spacemacs-buffer--release-note-version) spacemacs-buffer--cache-file)))
      ;; if there is an installed dotfile we check the variable
      ;; spacemacs-buffer--release-note-version to decide whether
      ;; we show the release note
      (when (and (file-exists-p dotspacemacs-filepath)
                 (or (not spacemacs-buffer--release-note-version)
                     (version< spacemacs-buffer--release-note-version
                               spacemacs-version)))
        (spacemacs-buffer/toggle-note (concat spacemacs-release-notes-directory "0.103.txt")
                                      'release-note))
      (spacemacs//redisplay))))

(defun spacemacs-buffer//choose-banner ()
  "Return the full path of a banner based on the dotfile value."
  (cond
   ((eq 'official dotspacemacs-startup-banner)
    (if (and (display-graphic-p) (image-type-available-p 'png))
        spacemacs-banner-official-png
      (spacemacs-buffer//get-banner-path 1)))
   ((eq 'random dotspacemacs-startup-banner)
    (spacemacs-buffer//choose-random-text-banner))
   ((eq 'random* dotspacemacs-startup-banner)
    (spacemacs-buffer//choose-random-text-banner t))
   ((eq 'doge dotspacemacs-startup-banner)
    (spacemacs-buffer//get-banner-path 999))
   ((integerp dotspacemacs-startup-banner)
    (spacemacs-buffer//get-banner-path dotspacemacs-startup-banner))
   ((and dotspacemacs-startup-banner
         (image-type-available-p (intern (file-name-extension
                                          dotspacemacs-startup-banner))))
    (if (file-exists-p dotspacemacs-startup-banner)
        dotspacemacs-startup-banner
      (spacemacs-buffer/warning (format "could not find banner %s"
                                        dotspacemacs-startup-banner))
      (spacemacs-buffer//get-banner-path 1))
   (spacemacs-buffer//get-banner-path 1))))

(defun spacemacs-buffer//choose-random-text-banner (&optional all)
  "Return the full path of a banner chosen randomly.

If ALL is non-nil then truly all banners can be selected."
  (let* ((files (directory-files spacemacs-banner-directory t))
         (count (length files))
         ;; -2 then +2 to remove `.' and `..'
         (choice (+ 2 (random (- count (if all 2 3))))))
    (nth choice files)))

(defun spacemacs-buffer//get-banner-path (index)
  "Return the full path to banner with index INDEX."
  (concat spacemacs-banner-directory (format "%03d-banner.txt" index)))

(defun spacemacs-buffer//insert-image-banner (banner)
  "Display an image banner."
  (when (file-exists-p banner)
    (let* ((spec (create-image banner))
           (size (image-size spec))
           (width (car size))
           (left-margin (floor (- spacemacs-buffer--banner-length width) 2)))
      (beginning-of-buffer)
      (insert "\n")
      (insert (make-string (- left-margin 1) ?\ ))
      (insert-image spec)
      (insert "\n\n")
      (insert "                           [S P A C E M A C S]\n\n"))))

(defun spacemacs-buffer//inject-version ()
  "Inject the current version of spacemacs in the first line of the
buffer, right justified."
  (save-excursion
    (beginning-of-buffer)
    (let* ((maxcol spacemacs-buffer--banner-length)
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

(defun spacemacs-buffer//insert-note (file caption &optional additional-widgets)
  "Insert the release note just under the banner.

FILE is the file that contains the content to show.
CAPTION is the title of the note.
TAG-STRING is the label of the button for additional action.
HELP-STRING is the help message of the button for additional action."
  (save-excursion
    (beginning-of-buffer)
    (search-forward "Spacemacs\]")
    (next-line)
    (let* ((note (concat "\n" (spacemacs//render-framed-text file
                                                             spacemacs-buffer--banner-length
                                                             caption))))
      (add-to-list 'spacemacs-buffer--note-widgets (widget-create 'text note))
      (funcall additional-widgets))))

(defun spacemacs-buffer//insert-note-p (type)
  "Decicde if whether to insert note widget or not based on current note TYPE.

If note TYPE is `quickhelp' or `release-note' and is equal to
previous insert type in `spacemacs-buffer--previous-insert-type',
which means previous note widget of the same type already
inserted. In this case, we simply delete the widgets but don't insert.

Otherwise, delete and allow insert note of TYPE."
  (if (not (eq spacemacs-buffer--previous-insert-type type))
      type
    (setq spacemacs-buffer--previous-insert-type nil)))

(defun spacemacs-buffer/toggle-note (file type)
  "Toggle the note in FILE for the buffer based on TYPE.

If TYPE is nil, just remove widgets."
  (interactive)
  (spacemacs-buffer//remove-existing-widget-if-exist)
  (cond
   ((eq type 'quickhelp)
    (spacemacs-buffer//insert-quickhelp-widget file))
   ((eq type 'release-note)
    (spacemacs-buffer//insert-release-note-widget file))
   (t)))

(defun spacemacs-buffer//remove-existing-widget-if-exist ()
  "Remove existing note widgets if exists."
  (when spacemacs-buffer--note-widgets
    (spacemacs-buffer//remove-note-widgets)))

(defun spacemacs-buffer//insert-quickhelp-widget (file)
  "Insert quickhelp with content from FILE."
  (spacemacs-buffer//remove-existing-widget-if-exist)
  (let ((widget-func (lambda ()
                       (add-to-list 'spacemacs-buffer--note-widgets
                                    (widget-create 'push-button
                                                   :tag (propertize "Evil Tutorial" 'face 'font-lock-keyword-face)
                                                   :help-echo "Teach you how to use Vim basics."
                                                   :action (lambda (&rest ignore) (call-interactively #'evil-tutor-start))
                                                   :mouse-face 'highlight
                                                   :follow-link "\C-m"))
                       (widget-insert " ")
                       (add-to-list 'spacemacs-buffer--note-widgets
                                    (widget-create 'push-button
                                                   :tag (propertize "Emacs Tutorial" 'face 'font-lock-keyword-face)
                                                   :help-echo "Teach you how to use Emacs basics."
                                                   :action (lambda (&rest ignore) (call-interactively #'help-with-tutorial))
                                                   :mouse-face 'highlight
                                                   :follow-link "\C-m"))
                       (widget-insert " ")
                       (add-to-list 'spacemacs-buffer--note-widgets
                                    (widget-create 'push-button
                                                   :tag (propertize "Vim Migration Guide" 'face 'font-lock-keyword-face)
                                                   :help-echo "Documentation for former vim users."
                                                   :action (lambda (&rest ignore) (spacemacs/view-org-file (concat spacemacs-docs-directory "VIMUSERS.org") "^" 'all))
                                                   :mouse-face 'highlight
                                                   :follow-link "\C-m")))))
    (spacemacs-buffer//insert-note file "Quick Help" widget-func))
  (setq spacemacs-buffer--previous-insert-type 'quickhelp))

(defun spacemacs-buffer//insert-release-note-widget (file)
  "Insert release note with content from FILE."
  (spacemacs-buffer//remove-existing-widget-if-exist)
  (let ((widget-func (lambda ()
                       (add-to-list 'spacemacs-buffer--note-widgets
                                    (widget-create 'push-button
                                                   :tag (propertize "Click here for full change log" 'face 'font-lock-warning-face)
                                                   :help-echo "Open the full change log."
                                                   :action (lambda (&rest ignore)
                                                             (funcall 'spacemacs/view-org-file
                                                                      (concat user-emacs-directory "CHANGELOG.org")
                                                                      "Release 0.103.x"
                                                                      'subtree))
                                                   :mouse-face 'highlight
                                                   :follow-link "\C-m")))))
    (spacemacs-buffer//insert-note file
                                   " Important Notes (Release 0.103.x) "
                                   widget-func))

  (setq spacemacs-buffer--release-note-version nil)
  (spacemacs/dump-vars-to-file
   '(spacemacs-buffer--release-note-version) spacemacs-buffer--cache-file)
  (setq spacemacs-buffer--previous-insert-type 'release-note))

(defun spacemacs-buffer//remove-note-widgets ()
  (mapc 'widget-delete spacemacs-buffer--note-widgets)
  (setq spacemacs-buffer--note-widgets nil)
  (setq spacemacs-buffer--release-note-version spacemacs-version)
  (spacemacs/dump-vars-to-file
   '(spacemacs-buffer--release-note-version) spacemacs-buffer--cache-file))

(defun spacemacs-buffer/set-mode-line (format)
  "Set mode-line format for spacemacs buffer."
  (with-current-buffer (get-buffer-create "*spacemacs*")
    (setq mode-line-format format)))

(defun spacemacs-buffer/message (msg &rest args)
  "Display MSG in message prepended with '(Spacemacs)'.
The message is displayed only if `dotspacemacs-verbose-loading' is non nil."
  (when dotspacemacs-verbose-loading
    (message "(Spacemacs) %s" (apply 'format msg args))))

(defun spacemacs-buffer/warning (msg &rest args)
  "Display MSG as a warning message but in buffer `*Messages*'.
The message is always displayed. "
  (message "(Spacemacs) Warning: %s" (apply 'format msg args)))

(defun spacemacs-buffer/insert-page-break ()
  "Insert a page break line in spacemacs buffer."
  (spacemacs-buffer/append "\n\n\n"))

(defun spacemacs-buffer/append (msg &optional messagebuf)
  "Append MSG to spacemacs buffer. If MESSAGEBUF is not nil then MSG is
 also written in message buffer."
  (with-current-buffer (get-buffer-create "*spacemacs*")
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert msg)
      (if messagebuf (message "(Spacemacs) %s" msg)))
    (spacemacs-buffer/set-mode-line "")))

(defun spacemacs-buffer/replace-last-line (msg &optional messagebuf)
  "Replace the last line of the spacemacs buffer with MSG. If MESSAGEBUF is
 not nil then MSG is also written in message buffer."
  (with-current-buffer (get-buffer-create "*spacemacs*")
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (delete-region (line-beginning-position) (point-max))
      (insert msg)
      (if messagebuf (message "(Spacemacs) %s" msg)))
    (spacemacs-buffer/set-mode-line "")))

(defun spacemacs-buffer/insert-framed-text
    (msg &optional caption hpadding)
  "Insert MSG in spacemacs buffer within a frame of width FILL-COLUMN.

See `spacemacs//render-framed-text' for documentation of the other
parameters."
  (with-current-buffer (get-buffer-create "*spacemacs*")
    (let ((buffer-read-only nil))
      (insert (spacemacs//render-framed-text msg spacemacs-buffer--banner-length
                                             caption hpadding)))))

(defun spacemacs-buffer/insert-framed-text-from-file
    (filepath &optional caption hpadding)
  "Insert at point the content of FILENAME file in spacemacs buffer in a
frame.

If FILEPATH does not exists the function returns nil.

See `spacemacs//render-framed-text' for documentation of the other
parameters."
  (when (file-exists-p filepath)
    (with-current-buffer (get-buffer-create "*spacemacs*")
      (let ((buffer-read-only nil))
        (insert (spacemacs//render-framed-text filepath spacemacs-buffer--banner-length
                                               caption hpadding))))))

(defun spacemacs//render-framed-text (content &optional width caption hpadding)
  "Return a formated string framed with plained lines of width FILL-COLUMN.

CONTENT can be a text or a filepath.

WIDTH set the `fill-column' variable.

If CAPTION is non nil string then it is included in at the top of the frame.
If CAPTION length is greater than FILL-COLUMN minus 5 the function returns
nil.

HPADDING is the horizontal spacing between the text and the frame.
The vertical spacing is always one line."
  (with-temp-buffer
    (if (not (file-exists-p content))
        (insert content)
      (insert-file-contents content)
      ;; remove additional newline at eof
      (goto-char (point-max))
      (delete-char -1))
    (let* ((hpadding (or hpadding 1))
           (fill-column (if width
                            (- width hpadding)
                          fill-column))
           (sentence-end-double-space nil)
           (caption-len (length caption)))
      (fill-region (point-min) (point-max) 'justify 'nosqueeze)
      (concat
       ;; top
       "╭─"
       (if caption
           (concat caption
                   (make-string (+ (- fill-column caption-len 1)
                                   hpadding) ?─))
         (make-string fill-column ?─))
       (make-string hpadding ?─) "╮\n"
       ;; content
       (spacemacs//render-framed-line "" hpadding)
       (mapconcat (lambda (x)
                    (spacemacs//render-framed-line x hpadding))
                  (split-string (buffer-string) "\n" nil) "")
       (spacemacs//render-framed-line "" hpadding)
       ;; bottom
       "╰" (make-string hpadding ?─)
       (make-string fill-column ?─)
       (make-string hpadding ?─) "╯"))))

(defun spacemacs//render-framed-line (line hpadding)
  "Return a formated LINE with borders of a frame on each side and
with width FILL-COLUMN.

If length of LINE is bigger than FILL-COLUMN it returns nil.

HPADDING is the horizontal spacing betwee the content line and the frame border."
  (let* ((len (length line))
         (fill (- fill-column len)))
    (when (>= fill 0)
      (concat "│" (make-string hpadding ?\s)
              line (make-string fill ?\s)
              (make-string hpadding ?\s) "│\n"))))

(defun spacemacs-buffer/loading-animation ()
  "Display the progress bar by chunk of size `spacemacs--loading-dots-chunk-threshold'."
  (when dotspacemacs-loading-progress-bar
    (setq spacemacs-loading-counter (1+ spacemacs-loading-counter))
    (when (>= spacemacs-loading-counter spacemacs-loading-dots-chunk-threshold)
      (setq spacemacs-loading-counter 0)
      (setq spacemacs-loading-string
            (concat spacemacs-loading-string
                    (make-string spacemacs-loading-dots-chunk-size
                                 spacemacs-loading-char)))
      (spacemacs-buffer/set-mode-line spacemacs-loading-string)
      (spacemacs//redisplay))))

(defmacro spacemacs//insert--shortcut (shortcut-char search-label &optional no-next-line)
  `(define-key spacemacs-mode-map ,shortcut-char (lambda ()
                                                   (interactive)
                                                   (unless (search-forward ,search-label (point-max) t)
                                                     (search-backward ,search-label (point-min) t))
                                                   ,@(unless no-next-line
                                                       '((forward-line 1)))
                                                   (back-to-indentation))))

(defun spacemacs-buffer//insert-buttons ()
  (goto-char (point-max))
  (insert "    ")
  (spacemacs//insert--shortcut "m" "[?]" t)
  (widget-create 'url-link
                 :tag (propertize "?" 'face 'font-lock-doc-face)
                 :help-echo "Open the quickhelp."
                 :action (lambda (&rest ignore)
                           (spacemacs-buffer/toggle-note (concat spacemacs-info-directory "quickhelp.txt")
                                                         ;; if nil is returned, just delete the current note widgets
                                                         (spacemacs-buffer//insert-note-p 'quickhelp)))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 )
  (insert " ")
  (widget-create 'url-link
                 :tag (propertize "Homepage" 'face 'font-lock-keyword-face)
                 :help-echo "Open the Spacemacs Github page in your browser."
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 "https://github.com/syl20bnr/spacemacs")
  (insert " ")
  (widget-create 'url-link
                 :tag (propertize "Documentation" 'face 'font-lock-keyword-face)
                 :help-echo "Open the Spacemacs documentation in your browser."
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 "https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org")
  (insert " ")
  (widget-create 'url-link
                 :tag (propertize "Gitter Chat" 'face 'font-lock-keyword-face)
                 :help-echo "Ask questions and chat with fellow users in our chat room."
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 "https://gitter.im/syl20bnr/spacemacs")
  (insert " ")
  (widget-create 'push-button
                 :help-echo "Update all ELPA packages to the latest versions."
                 :action (lambda (&rest ignore) (configuration-layer/update-packages))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 (propertize "Update" 'face 'font-lock-keyword-face))
  (insert " ")
  (widget-create 'push-button
                 :help-echo "Rollback ELPA package upgrades if something got borked."
                 :action (lambda (&rest ignore) (call-interactively 'configuration-layer/rollback))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 (propertize "Rollback" 'face 'font-lock-keyword-face))
  (insert "\n")
  (insert "                 ")
  (widget-create 'push-button
                 :tag (propertize "Release Notes" 'face 'font-lock-preprocessor-face)
                 :help-echo "Hide or show the Changelog"
                 :action (lambda (&rest ignore)
                           (spacemacs-buffer/toggle-note (concat spacemacs-release-notes-directory "0.103.txt")
                                                         ;; if nil is returned, just delete the current note widgets
                                                         (spacemacs-buffer//insert-note-p 'release-note)))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 )
  (insert " ")
  (widget-create 'url-link
                 :tag (propertize "Search in Spacemacs" 'face 'font-lock-function-name-face)
                 :help-echo "Find Spacemacs package and layer configs using helm-spacemacs."
                 :action (lambda (&rest ignore) (call-interactively 'helm-spacemacs))
                 :mouse-face 'highlight
                 :follow-link "\C-m")
  (insert "\n\n"))

(defun spacemacs-buffer//insert-file-list (list-display-name list)
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (insert "\n    ")
            (widget-create 'push-button
                           :action `(lambda (&rest ignore) (find-file-existing ,el))
                           :mouse-face 'highlight
                           :follow-link "\C-m"
                           :button-prefix ""
                           :button-suffix ""
                           :format "%[%t%]"
                           (abbreviate-file-name el)))
          list)))

(defun spacemacs-buffer/insert-startupify-lists ()
  (interactive)
  (with-current-buffer (get-buffer-create "*spacemacs*")
    (let ((buffer-read-only nil)
          (list-separator "\n\n"))
      (goto-char (point-max))
      (page-break-lines-mode)
      (spacemacs-buffer/insert-page-break)
      (mapc (lambda (el)
              (cond
               ((eq el 'recents)
                (recentf-mode)
                (when (spacemacs-buffer//insert-file-list "Recent Files:" (recentf-elements 5))
                  (spacemacs//insert--shortcut "r" "Recent Files:")
                  (insert list-separator)))
               ((eq el 'bookmarks)
                (helm-mode)
                (when (spacemacs-buffer//insert-file-list "Bookmarks:" (mapcar 'bookmark-get-filename (bookmark-all-names)))
                  (spacemacs//insert--shortcut "m" "Bookmarks:")
                  (insert list-separator)))
               ((eq el 'projects)
                (projectile-mode)
                (when (spacemacs-buffer//insert-file-list "Projects:" (projectile-relevant-known-projects))
                  (spacemacs//insert--shortcut "p" "Projects:")
                  (insert list-separator))))) dotspacemacs-startup-lists))))

(defun spacemacs-buffer/goto-link-line ()
  "Move the point to the beginning of the link line."
  (interactive)
  (when (and dotspacemacs-startup-banner
             (not configuration-layer-error-count))
    (with-current-buffer spacemacs-buffer-name
      (goto-char (point-min))
      (re-search-forward "Homepage")
      (beginning-of-line)
      (widget-forward 1))))

(provide 'core-spacemacs-buffer)
