;;; core-spacemacs-buffer.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(defconst spacemacs-buffer-version-info "0.200"
  "Current version used to display addition release information.")

(defconst spacemacs-buffer-name "*spacemacs*"
  "The name of the spacemacs buffer.")

(defconst spacemacs-buffer-logo-title "[S P A C E M A C S]"
  "The title displayed beneath the logo.")

(defconst spacemacs-buffer-buttons-startup-lists-offset 25
  "Relative position in characters of the home buffer buttons and the home
 buffer startup lists.")

(defconst spacemacs-buffer--banner-length 75
  "Width of a banner.")

(defconst spacemacs-buffer--cache-file
  (expand-file-name (concat spacemacs-cache-directory "spacemacs-buffer.el"))
  "Cache file for various persistent data for the spacemacs startup buffer")

(defvar spacemacs-buffer-startup-lists-length 20
  "Length used for startup lists with otherwise unspecified bounds.
Set to nil for unbounded.")

(defvar spacemacs-buffer--release-note-version nil
  "If nil the release note is displayed. If non nil it contains
a version number, if the version number is lesser than the current
version the release note it displayed")

(defvar spacemacs-buffer--note-widgets nil
  "List of widgets used to display the release note.")

(defvar spacemacs-buffer--previous-insert-type nil
  "Previous type of note inserted.")

(defvar spacemacs-buffer--fresh-install
  (not (file-exists-p dotspacemacs-filepath))
  "Non-nil if this Emacs instance if a fresh install.")

(defvar spacemacs-buffer--buttons-position nil
  "Offset in characters between the edge of the screen and the beginning of the
 home buffer buttons. Do not set this variable.")

(defvar spacemacs-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] 'widget-button-click)
    (define-key map (kbd "RET") 'widget-button-press)

    (define-key map [tab] 'widget-forward)
    (define-key map (kbd "J") 'widget-forward)
    (define-key map (kbd "C-i") 'widget-forward)

    (define-key map [backtab] 'widget-backward)
    (define-key map (kbd "K") 'widget-backward)

    (define-key map (kbd "C-r") 'spacemacs-buffer/refresh)
    (define-key map "q" 'quit-window)
    map)
  "Keymap for spacemacs buffer mode.")

(with-eval-after-load 'evil
  (evil-make-overriding-map spacemacs-buffer-mode-map 'motion))

(define-derived-mode spacemacs-buffer-mode fundamental-mode "Spacemacs buffer"
  "Spacemacs major mode for startup screen.

\\<spacemacs-buffer-mode-map>
"
  :group 'spacemacs
  :syntax-table nil
  :abbrev-table nil
  (page-break-lines-mode)
  (setq buffer-read-only t
        truncate-lines t)
  ;; needed to make tab work correctly in terminal
  (evil-define-key 'motion spacemacs-buffer-mode-map
    (kbd "C-i") 'widget-forward)
  ;; motion state since this is a special mode
  (evil-set-initial-state 'spacemacs-buffer-mode 'motion))

(defun spacemacs-buffer/insert-ascii-banner-centered (file)
  (insert-string
   (with-temp-buffer
     (insert-file-contents file)
     (let ((banner-width 0))
       (while (not (eobp))
         (let ((line-length (- (line-end-position) (line-beginning-position))))
           (if (< banner-width line-length)
               (setq banner-width line-length)))
         (forward-line 1))
       (goto-char 0)
       (let ((margin (max 0 (floor (/ (- spacemacs-buffer--banner-length banner-width) 2)))))
         (while (not (eobp))
           (insert (make-string margin ?\ ))
           (forward-line 1))))
     (buffer-string))))

(defun spacemacs-buffer/insert-banner-and-buttons ()
  "Choose a banner according to `dotspacemacs-startup-banner'and insert it
in spacemacs buffer along with quick buttons underneath.

Easter egg:
Doge special text banner can be reachable via `999', `doge' or `random*'.
Cate special text banner can de reachable via `998', `cat' or `random*'.
`random' ignore special banners whereas `random*' does not."
  (let ((banner (spacemacs-buffer//choose-banner))
        (buffer-read-only nil))
    (progn
      (when banner
        (spacemacs-buffer/message (format "Banner: %s" banner))
        (if (image-type-available-p (intern (file-name-extension banner)))
            (spacemacs-buffer//insert-image-banner banner)
          (spacemacs-buffer/insert-ascii-banner-centered banner))
        (spacemacs-buffer//inject-version))
      (spacemacs-buffer//insert-buttons)
      (spacemacs//redisplay))))

(defun spacemacs-buffer/display-info-box ()
  "Display an info box."
  (when (file-exists-p spacemacs-buffer--cache-file)
    (load spacemacs-buffer--cache-file))
  (cond
   (spacemacs-buffer--fresh-install
    ;; we assume the user is  new to spacemacs and open the quickhelp
    (spacemacs-buffer/toggle-note
     (concat spacemacs-info-directory "quickhelp.txt")
     (spacemacs-buffer//insert-note-p 'quickhelp))
    (setq spacemacs-buffer--release-note-version spacemacs-version)
    (spacemacs/dump-vars-to-file '(spacemacs-buffer--release-note-version)
                                   spacemacs-buffer--cache-file))
   ((or (not spacemacs-buffer--release-note-version)
        (version< spacemacs-buffer--release-note-version
                  spacemacs-version))
    ;; check the variable ;; spacemacs-buffer--release-note-version
    ;; to decide whether ;; we show the release note
    (spacemacs-buffer/toggle-note
     (concat spacemacs-release-notes-directory
             spacemacs-buffer-version-info ".txt") 'release-note)))
  (spacemacs//redisplay))

(defun spacemacs-buffer//choose-banner ()
  "Return the full path of a banner based on the dotfile value."
  (when dotspacemacs-startup-banner
    (cond ((eq 'official dotspacemacs-startup-banner)
           (if (and (display-graphic-p) (image-type-available-p 'png))
               spacemacs-banner-official-png
             (spacemacs-buffer//get-banner-path 1)))
          ((eq 'random dotspacemacs-startup-banner)
           (spacemacs-buffer//choose-random-text-banner))
          ((eq 'random* dotspacemacs-startup-banner)
           (spacemacs-buffer//choose-random-text-banner t))
          ((eq 'doge dotspacemacs-startup-banner)
           (spacemacs-buffer//get-banner-path 999))
          ((eq 'cat dotspacemacs-startup-banner)
           (spacemacs-buffer//get-banner-path 998))
          ((integerp dotspacemacs-startup-banner)
           (spacemacs-buffer//get-banner-path dotspacemacs-startup-banner))
          ((and dotspacemacs-startup-banner
                (image-type-available-p (intern (file-name-extension
                                                 dotspacemacs-startup-banner)))
                (display-graphic-p))
           (if (file-exists-p dotspacemacs-startup-banner)
               dotspacemacs-startup-banner
             (spacemacs-buffer/warning (format "could not find banner %s"
                                               dotspacemacs-startup-banner))
             (spacemacs-buffer//get-banner-path 1)))
          (t (spacemacs-buffer//get-banner-path 1)))))

(defvar spacemacs-buffer--random-banner nil
  "The random banner chosen.")

(defun spacemacs-buffer//choose-random-text-banner (&optional all)
  "Return the full path of a banner chosen randomly.

If ALL is non-nil then truly all banners can be selected."
  (setq spacemacs-buffer--random-banner
        (or spacemacs-buffer--random-banner
            (let* ((files (directory-files spacemacs-banner-directory t ".*\.txt"))
                   (count (length files))
                   ;; -2 to remove the two last ones (easter eggs)
                   (choice (random (- count (if all 0 2)))))
              (nth choice files)))))

(defun spacemacs-buffer//get-banner-path (index)
  "Return the full path to banner with index INDEX."
  (concat spacemacs-banner-directory (format "%03d-banner.txt" index)))

(defun spacemacs-buffer//insert-image-banner (banner)
  "Display an image banner."
  (when (file-exists-p banner)
    (let* ((title spacemacs-buffer-logo-title)
           (spec (create-image banner))
           (size (image-size spec))
           (width (car size))
           (left-margin (max 0 (floor (- spacemacs-buffer--banner-length width) 2))))
      (goto-char (point-min))
      (insert "\n")
      (insert (make-string left-margin ?\ ))
      (insert-image spec)
      (insert "\n\n")
      (insert (make-string (max 0 (floor (/ (- spacemacs-buffer--banner-length
                                        (+ (length title) 1)) 2))) ?\ ))
      (insert (format "%s\n\n" title)))))

(defun spacemacs-buffer//inject-version ()
  "Inject the current version of spacemacs in the first line of the
buffer, right justified."
  (with-current-buffer (get-buffer-create spacemacs-buffer-name)
    (save-excursion
      (let ((maxcol spacemacs-buffer--banner-length)
            (version (format "%s@%s (%s)"
                             spacemacs-version
                             emacs-version
                             dotspacemacs-distribution))
            (buffer-read-only nil))
        (goto-char (point-min))
        (delete-region (point) (progn (end-of-line) (point)))
        (insert (format (format "%%%ds" maxcol) version))))))

(defun spacemacs-buffer//insert-footer ()
  (save-excursion
    (let* ((maxcol spacemacs-buffer--banner-length)
           (badge-path spacemacs-badge-official-png)
           (badge (when (and (display-graphic-p)
                             (image-type-available-p
                              (intern (file-name-extension badge-path))))
                    (create-image badge-path)))
           (badge-size (when badge (car (image-size badge))))
           (heart-path spacemacs-purple-heart-png)
           (heart (when (and (display-graphic-p)
                             (image-type-available-p
                              (intern (file-name-extension badge-path))))
                    (create-image heart-path)))
           (heart-size (when heart (car (image-size heart))))
           (build-lhs "Made with ")
           (build-rhs " by the community")
           (buffer-read-only nil))
      (when (or badge heart)
        (goto-char (point-max))
        (spacemacs-buffer/insert-page-break)
        (insert "\n")
        (when badge
          (insert (make-string (floor (/ (- maxcol badge-size) 2)) ?\ ))
          (insert-image badge))
        (when heart
          (when badge (insert "\n\n"))
          (insert (make-string (floor (/ (- maxcol
                                            (length build-lhs)
                                            heart-size
                                            (length build-rhs)) 2)) ?\ ))
          (insert build-lhs)
          (insert-image heart)
          (insert build-rhs)
          (insert "\n"))))))

(defun spacemacs-buffer//insert-note (file caption &optional additional-widgets)
  "Insert the release note just under the banner.

FILE is the file that contains the content to show.
CAPTION is the title of the note.
TAG-STRING is the label of the button for additional action.
HELP-STRING is the help message of the button for additional action."
  (save-excursion
    (goto-char (point-min))
    (search-forward "Search in Spacemacs\]")
    (forward-line)
    (let* ((note (concat "\n" (spacemacs//render-framed-text
                               file spacemacs-buffer--banner-length caption))))
      (add-to-list 'spacemacs-buffer--note-widgets (widget-create 'text note))
      (save-excursion
        (while (re-search-backward "\\[\\[\\(.*\\)\\]\\]" nil t)
          (let ((buffer-read-only nil))
            (make-text-button
             (match-beginning 1)
             (match-end 1)
             'type 'help-url
             'help-args (list (match-string 1))))))
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
  (let ((widget-func
         (lambda ()
           (add-to-list
            'spacemacs-buffer--note-widgets
            (widget-create 'push-button
                           :tag (propertize "Evil Tutorial"
                                            'face 'font-lock-keyword-face)
                           :help-echo
                           "Teach you how to use Vim basics."
                           :action (lambda (&rest ignore)
                                     (call-interactively #'evil-tutor-start))
                           :mouse-face 'highlight
                           :follow-link "\C-m"))
           (widget-insert " ")
           (add-to-list
            'spacemacs-buffer--note-widgets
            (widget-create 'push-button
                           :tag (propertize "Emacs Tutorial"
                                            'face 'font-lock-keyword-face)
                           :help-echo "Teach you how to use Emacs basics."
                           :action (lambda (&rest ignore)
                                     (call-interactively #'help-with-tutorial))
                           :mouse-face 'highlight
                           :follow-link "\C-m"))
           (widget-insert " ")
           (add-to-list
            'spacemacs-buffer--note-widgets
            (widget-create 'push-button
                           :tag (propertize "Vim Migration Guide"
                                            'face 'font-lock-keyword-face)
                           :help-echo "Documentation for former vim users."
                           :action (lambda (&rest ignore)
                                     (spacemacs/view-org-file
                                      (concat spacemacs-docs-directory
                                              "VIMUSERS.org") "^" 'all))
                           :mouse-face 'highlight
                           :follow-link "\C-m")))))
    (spacemacs-buffer//insert-note file "Quick Help" widget-func))
  (setq spacemacs-buffer--previous-insert-type 'quickhelp))

(defun spacemacs-buffer//insert-release-note-widget (file)
  "Insert release note with content from FILE."
  (spacemacs-buffer//remove-existing-widget-if-exist)
  (let ((widget-func
         (lambda ()
           (add-to-list
            'spacemacs-buffer--note-widgets
            (widget-create 'push-button
                           :tag (propertize "Click here for full change log"
                                            'face 'font-lock-warning-face)
                           :help-echo "Open the full change log."
                           :action
                           (lambda (&rest ignore)
                             (funcall 'spacemacs/view-org-file
                                      (concat spacemacs-start-directory
                                              "CHANGELOG.org")
                                      (format "Release %s.x"
                                              spacemacs-buffer-version-info)
                                      'subtree))
                           :mouse-face 'highlight
                           :follow-link "\C-m")))))
    (spacemacs-buffer//insert-note file
                                   (format " Important Notes (Release %s.x) "
                                           spacemacs-buffer-version-info)
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
  (with-current-buffer (get-buffer-create spacemacs-buffer-name)
    (setq mode-line-format format)))

(defun spacemacs-buffer/message (msg &rest args)
  "Display MSG in message prepended with '(Spacemacs)'.
The message is displayed only if `init-file-debug' is non nil."
  (when init-file-debug
    (message "(Spacemacs) %s" (apply 'format msg args))))

(defvar spacemacs-buffer--warnings nil
  "List of warnings during startup.")

(defun spacemacs-buffer/warning (msg &rest args)
  "Display MSG as a warning message but in buffer `*Messages*'.
The message is always displayed. "
  (let ((msg (apply 'format msg args)))
    (message "(Spacemacs) Warning: %s" msg)
    (add-to-list 'spacemacs-buffer--warnings msg 'append)))

(defun spacemacs-buffer/insert-page-break ()
  "Insert a page break line in spacemacs buffer."
  (spacemacs-buffer/append "\n\n"))

(defun spacemacs-buffer/append (msg &optional messagebuf)
  "Append MSG to spacemacs buffer. If MESSAGEBUF is not nil then MSG is
 also written in message buffer."
  (with-current-buffer (get-buffer-create spacemacs-buffer-name)
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert msg)
      (if messagebuf (message "(Spacemacs) %s" msg)))
    (spacemacs-buffer/set-mode-line "")))

(defun spacemacs-buffer/replace-last-line (msg &optional messagebuf)
  "Replace the last line of the spacemacs buffer with MSG. If MESSAGEBUF is
 not nil then MSG is also written in message buffer."
  (with-current-buffer (get-buffer-create spacemacs-buffer-name)
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
  (with-current-buffer (get-buffer-create spacemacs-buffer-name)
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
    (with-current-buffer (get-buffer-create spacemacs-buffer-name)
      (let ((buffer-read-only nil))
        (insert (spacemacs//render-framed-text
                 filepath spacemacs-buffer--banner-length caption hpadding))))))

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
                            (- width (+ 2 (* 2 hpadding)))
                          fill-column))
           (sentence-end-double-space nil)
           (caption-len (length caption)))
      (fill-region (point-min) (point-max) 'justify 'nosqueeze)
      (concat
       ;; top
       "╭─"
       (if caption
           (concat caption
                   (make-string (max 0 (+ (- fill-column caption-len 1)
                                          hpadding)) ?─))
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

HPADDING is the horizontal spacing betwee the content line and the frame
border."
  (let* ((len (length line))
         (fill (- fill-column len)))
    (when (>= fill 0)
      (concat "│" (make-string hpadding ?\s)
              line (make-string fill ?\s)
              (make-string hpadding ?\s) "│\n"))))

(defun spacemacs-buffer/loading-animation ()
  "Display the progress bar by chunk of size
`spacemacs--loading-dots-chunk-threshold'."
  (when (and (not noninteractive) dotspacemacs-loading-progress-bar)
    (setq spacemacs-loading-counter (1+ spacemacs-loading-counter))
    (setq spacemacs-loading-value (1+ spacemacs-loading-value))
    (when (>= spacemacs-loading-counter spacemacs-loading-dots-chunk-threshold)
      (let ((suffix (format "> %s/%s" spacemacs-loading-value
                            (length configuration-layer--used-packages))))
        (setq spacemacs-loading-counter 0)
        (setq spacemacs-loading-string
              (make-string
               (max 0
                    (- (* spacemacs-loading-dots-chunk-size
                          (floor (/ spacemacs-loading-value
                                    spacemacs-loading-dots-chunk-threshold)))
                       (length suffix)))
               spacemacs-loading-char))
        (spacemacs-buffer/set-mode-line (concat spacemacs-loading-string
                                                suffix)))
      (spacemacs//redisplay))))

(defmacro spacemacs//insert--shortcut (shortcut-char search-label
                                                     &optional no-next-line)
  `(define-key spacemacs-buffer-mode-map
     ,shortcut-char (lambda ()
                      (interactive)
                      (unless (search-forward ,search-label (point-max) t)
                        (search-backward ,search-label (point-min) t))
                      ,@(unless no-next-line
                          '((forward-line 1)))
                      (back-to-indentation))))

(defun spacemacs-buffer//center-line ()
  (let* ((width (current-column))
         (margin (max 0 (floor (/ (- spacemacs-buffer--banner-length width) 2)))))
    (beginning-of-line)
    (insert (make-string margin ?\ ))
    (end-of-line)))

(defun spacemacs-buffer//insert-buttons ()
    (goto-char (point-max))
    (spacemacs//insert--shortcut "m" "[?]" t)
    (widget-create 'url-link
                   :tag (propertize "?" 'face 'font-lock-doc-face)
                   :help-echo "Open the quickhelp."
                   :action (lambda (&rest ignore)
                             (spacemacs-buffer/toggle-note
                              (concat spacemacs-info-directory "quickhelp.txt")
                              ;; if nil is returned,
                              ;; just delete the current note widgets
                              (spacemacs-buffer//insert-note-p 'quickhelp)))
                   :mouse-face 'highlight
                   :follow-link "\C-m")
    (insert " ")
    (widget-create 'url-link
                   :tag (propertize "Homepage" 'face 'font-lock-keyword-face)
                   :help-echo "Open the Spacemacs Github page in your browser."
                   :mouse-face 'highlight
                   :follow-link "\C-m"
                   "http://spacemacs.org")
    (insert " ")
    (widget-create 'url-link
                   :tag (propertize "Documentation" 'face 'font-lock-keyword-face)
                   :help-echo "Open the Spacemacs documentation in your browser."
                   :mouse-face 'highlight
                   :follow-link "\C-m"
                   "http://spacemacs.org/doc/DOCUMENTATION.html")
    (insert " ")
    (widget-create 'url-link
                   :tag (propertize "Gitter Chat" 'face 'font-lock-keyword-face)
                   :help-echo
                   "Ask questions and chat with fellow users in our chat room."
                   :mouse-face 'highlight
                   :follow-link "\C-m"
                   "https://gitter.im/syl20bnr/spacemacs")
    (insert " ")
    (widget-create 'push-button
                   :help-echo "Update Spacemacs core and layers."
                   :action (lambda (&rest ignore) (spacemacs/switch-to-version))
                   :mouse-face 'highlight
                   :follow-link "\C-m"
                   (propertize "Update Spacemacs" 'face 'font-lock-keyword-face))
    (let ((len (- (line-end-position)
                  (line-beginning-position))))
      (spacemacs-buffer//center-line)
      (setq spacemacs-buffer--buttons-position (- (line-end-position)
                                                  (line-beginning-position)
                                                  len)))
    (insert "\n")
    (widget-create 'push-button
                   :help-echo "Update all ELPA packages to the latest versions."
                   :action (lambda (&rest ignore)
                             (configuration-layer/update-packages))
                   :mouse-face 'highlight
                   :follow-link "\C-m"
                   (propertize "Update Packages" 'face 'font-lock-keyword-face))
    (insert " ")
    (widget-create 'push-button
                   :help-echo
                   "Rollback ELPA package updates if something got borked."
                   :action (lambda (&rest ignore)
                             (call-interactively 'configuration-layer/rollback))
                   :mouse-face 'highlight
                   :follow-link "\C-m"
                   (propertize "Rollback Package Update"
                               'face 'font-lock-keyword-face))
    (spacemacs-buffer//center-line)
    (insert "\n")
    (widget-create 'push-button
                   :tag (propertize "Release Notes"
                                    'face 'font-lock-preprocessor-face)
                   :help-echo "Hide or show the Changelog"
                   :action (lambda (&rest ignore)
                             (spacemacs-buffer/toggle-note
                              (concat spacemacs-release-notes-directory
                                      spacemacs-buffer-version-info
                                      ".txt")
                              ;; if nil is returned,
                              ;; just delete the current note widgets
                              (spacemacs-buffer//insert-note-p 'release-note)))
                   :mouse-face 'highlight
                   :follow-link "\C-m")
    (insert " ")
    (widget-create 'url-link
                   :tag (propertize "Search in Spacemacs"
                                    'face 'font-lock-function-name-face)
                   :help-echo "Search Spacemacs contents."
                   :action
                   (lambda (&rest ignore)
                     (let ((comp-frontend
                            (cond
                             ((configuration-layer/layer-usedp 'helm)
                              'helm-spacemacs-help)
                             ((configuration-layer/layer-usedp 'ivy)
                              'ivy-spacemacs-help))))
                       (call-interactively comp-frontend)))
                   :mouse-face 'highlight
                   :follow-link "\C-m")
    (spacemacs-buffer//center-line)
    (insert "\n\n"))

(defun spacemacs-buffer//insert-string-list (list-display-name list)
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (insert
             "\n"
             (with-temp-buffer
               (insert el)
               (fill-paragraph)
               (goto-char (point-min))
               (insert "    - ")
               (while (= 0 (forward-line))
                 (insert "      "))
               (buffer-string))))
          list)))

(defun spacemacs-buffer//insert-file-list (list-display-name list)
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (insert "\n    ")
            (widget-create 'push-button
                           :action `(lambda (&rest ignore)
                                      (find-file-existing ,el))
                           :mouse-face 'highlight
                           :follow-link "\C-m"
                           :button-prefix ""
                           :button-suffix ""
                           :format "%[%t%]"
                           (abbreviate-file-name el)))
          list)))

(defun spacemacs-buffer//insert-bookmark-list (list-display-name list)
  (when (car list)
    (insert list-display-name)
    (mapc (lambda (el)
            (insert "\n    ")
            (let ((filename (bookmark-get-filename el)))
              (widget-create 'push-button
                             :action `(lambda (&rest ignore) (bookmark-jump ,el))
                             :mouse-face 'highlight
                             :follow-link "\C-m"
                             :button-prefix ""
                             :button-suffix ""
                             :format "%[%t%]"
                             (if filename
                                 (format "%s - %s"
                                         el (abbreviate-file-name filename))
                               (format "%s" el)))))
          list)))

(defun spacemacs-buffer//get-org-items (types)
  "Make a list of agenda file items for today of kind types"
  (require 'org-agenda)
  (let ((date (calendar-gregorian-from-absolute (org-today))))
    (apply #'append
           (loop for file in (org-agenda-files nil 'ifmode)
                 collect
                 (spacemacs-buffer//make-org-items
                  file
                  (apply 'org-agenda-get-day-entries file date
                         types))))))

(defun spacemacs-buffer//agenda-list ()
  "Returns today's agenda"
  (require 'org-agenda)
  (spacemacs-buffer//get-org-items org-agenda-entry-types))

(defun spacemacs-buffer//todo-list ()
  "Returns current todos"
  (require 'org-agenda)
  (spacemacs-buffer//get-org-items '(:todo)))

(defun spacemacs-buffer//make-org-items (file items)
  "make a spacemacs-buffer org item list"
  (loop
   for item in items
   collect
   (spacemacs-buffer//make-org-item file item)))

(defun spacemacs-buffer//make-org-item (file item)
  "make a spacemacs-buffer version of an org item"
  (list (cons "text"
              (get-text-property 0 'txt item))
        (cons "file" file)
        (cons "pos"
              (marker-position
               (get-text-property 0 'org-marker item)))
        (cons "time"
              (get-text-property 0 'time item))))

(defun spacemacs-buffer//org-jump (el)
  (require 'org-agenda)
  (find-file-other-window (cdr (assoc "file" el)))
  (widen)
  (goto-char (cdr (assoc "pos" el)))
  (when (derived-mode-p 'org-mode)
    (org-show-context 'agenda)
    (save-excursion
      (and (outline-next-heading)
           (org-flag-heading nil)))	; show the next heading
    (when (outline-invisible-p)
      (outline-show-entry))			; display invisible text
    (recenter (/ (window-height) 2))
    (org-back-to-heading t)
    (if (re-search-forward org-complex-heading-regexp nil t)
        (goto-char (match-beginning 4))))
  (run-hooks 'org-agenda-after-show-hook))

(defun spacemacs-buffer//insert-todo-list (list-display-name list)
  (when (car list)
    (insert list-display-name)
    (setq list (sort list
                     (lambda (a b)
                       (cond
                        ((eq "" (cdr (assoc "time" b)))
                         t)
                        ((eq "" (cdr (assoc "time" a)))
                         nil)
                        (t
                         (string< (cdr (assoc "time" a))
                                  (cdr (assoc "time" b))))))))
    (mapc (lambda (el)
            (insert "\n    ")
            (widget-create 'push-button
                           :action `(lambda (&rest ignore)
                                      (spacemacs-buffer//org-jump ',el))
                           :mouse-face 'highlight
                           :follow-link "\C-m"
                           :button-prefix ""
                           :button-suffix ""
                           :format "%[%t%]"
                           (format "%s %s %s"
                                   (abbreviate-file-name
                                    (cdr (assoc "file" el)))
                                   (if (not (eq "" (cdr (assoc "time" el))))
                                       (format "- %s -"
                                               (cdr (assoc "time" el)))
                                     "-")
                                   (cdr (assoc "text" el)))))
          list)))

(defun spacemacs//subseq (seq start end)
  "Use `cl-subseq`, but accounting for end points greater than the size of the
list. Return entire list if `END' is omitted."
  (let ((len (length seq)))
    (cl-subseq seq start (and (number-or-marker-p end)
                              (min len end)))))

(defun spacemacs-buffer//do-insert-startupify-lists ()
   (let ((list-separator "\n\n"))
      (mapc (lambda (els)
              (let ((el (or (car-safe els) els))
                    (list-size
                     (or (cdr-safe els)
                         spacemacs-buffer-startup-lists-length)))
                (cond
                 ((eq el 'warnings)
                  (when (spacemacs-buffer//insert-string-list
                         "Warnings:"
                         spacemacs-buffer--warnings)
                    (spacemacs//insert--shortcut "w" "Warnings:")
                    (insert list-separator)))
                 ((eq el 'recents)
                  (recentf-mode)
                  (when (spacemacs-buffer//insert-file-list
                         "Recent Files:"
                         (spacemacs//subseq recentf-list 0 list-size))
                    (spacemacs//insert--shortcut "r" "Recent Files:")
                    (insert list-separator)))
                 ((eq el 'todos)
                  (when (spacemacs-buffer//insert-todo-list
                         "ToDo:"
                         (spacemacs//subseq (spacemacs-buffer//todo-list)
                                            0 list-size))
                    (spacemacs//insert--shortcut "d" "ToDo:")
                    (insert list-separator)))
                 ((eq el 'agenda)
                  (when (spacemacs-buffer//insert-todo-list
                         "Agenda:"
                         (spacemacs//subseq (spacemacs-buffer//agenda-list)
                                            0 list-size))
                    (spacemacs//insert--shortcut "c" "Agenda:")
                    (insert list-separator)))
                 ((eq el 'bookmarks)
                  (when (configuration-layer/layer-usedp 'spacemacs-helm)
                    (helm-mode))
                  (require 'bookmark)
                  (when (spacemacs-buffer//insert-bookmark-list
                         "Bookmarks:"
                         (spacemacs//subseq (bookmark-all-names)
                                            0 list-size))
                    (spacemacs//insert--shortcut "b" "Bookmarks:")
                    (insert list-separator)))
                 ((and (eq el 'projects)
                       (fboundp 'projectile-mode))
                  (projectile-mode)
                  (when (spacemacs-buffer//insert-file-list
                         "Projects:"
                         (spacemacs//subseq (projectile-relevant-known-projects)
                                            0 list-size))
                    (spacemacs//insert--shortcut "p" "Projects:")
                    (insert list-separator))))))
            (append
             '(warnings)
             dotspacemacs-startup-lists))))

(defun spacemacs-buffer//get-buffer-width ()
  (save-excursion
    (goto-char 0)
    (let ((current-max 0))
      (while (not (eobp))
        (let ((line-length (- (line-end-position) (line-beginning-position))))
          (if (< current-max line-length)
              (setq current-max line-length)))
        (forward-line 1))
      current-max)))

(defun spacemacs-buffer//center-startupify-lists ()
  (let* ((lists-width (spacemacs-buffer//get-buffer-width))
         (margin (max 0 (- spacemacs-buffer--buttons-position
                           spacemacs-buffer-buttons-startup-lists-offset)))
         (final-padding (if (< spacemacs-buffer--banner-length (+ margin lists-width))
                            (max 0 (floor (/ (- spacemacs-buffer--banner-length lists-width) 2)))
                          margin)))
    (goto-char 0)
    (while (not (eobp))
      (line-beginning-position)
      (insert (make-string final-padding ?\ ))
      (forward-line))))

(defun spacemacs-buffer/insert-startupify-lists ()
  (interactive)
  (with-current-buffer (get-buffer spacemacs-buffer-name)
    (let ((buffer-read-only nil))
      (goto-char (point-max))
      (spacemacs-buffer/insert-page-break)
      (insert "\n")
      (save-restriction
        (narrow-to-region (point) (point))
        (spacemacs-buffer//do-insert-startupify-lists)
        (spacemacs-buffer//center-startupify-lists)))))

(defun spacemacs-buffer/goto-link-line ()
  "Set point to the beginning of the link line."
  (interactive)
  (with-current-buffer spacemacs-buffer-name
    (goto-char (point-min))
    (with-demoted-errors "spacemacs buffer error: %s"
      (widget-forward 1))))

(defun spacemacs-buffer//startup-hook ()
  "Code executed when Emacs has finished loading."
  (with-current-buffer (get-buffer spacemacs-buffer-name)
    (when dotspacemacs-startup-lists
      (spacemacs-buffer/insert-startupify-lists))
    (spacemacs-buffer//insert-footer)
    (if configuration-layer-error-count
        (progn
          (spacemacs-buffer-mode)
          (spacemacs-buffer/set-mode-line
           (format
            (concat "%s error(s) at startup! "
                    "Spacemacs may not be able to operate properly.")
            configuration-layer-error-count))
          (face-remap-add-relative 'mode-line
                                   '((:background "red") mode-line)))
      (spacemacs-buffer/set-mode-line spacemacs--default-mode-line)
      (spacemacs-buffer-mode))
    (force-mode-line-update)
    (spacemacs-buffer/goto-link-line)))

(defvar spacemacs-buffer--last-width nil
  "Previous width of spacemacs-buffer")

(defun spacemacs-buffer/goto-buffer (&optional refresh)
  "Create the special buffer for `spacemacs-buffer-mode' if it doesn't
already exist, and switch to it."
  (interactive)
  (let ((buffer-exists (buffer-live-p (get-buffer spacemacs-buffer-name)))
        (save-line nil))
    (when (or (not (eq spacemacs-buffer--last-width (window-width)))
              (not buffer-exists)
              refresh)
      (setq spacemacs-buffer--banner-length (window-width)
            spacemacs-buffer--last-width spacemacs-buffer--banner-length)
      (with-current-buffer (get-buffer-create spacemacs-buffer-name)
        (page-break-lines-mode)
        (save-excursion
          (when (> (buffer-size) 0)
            (set 'save-line (line-number-at-pos))
            (let ((inhibit-read-only t))
              (erase-buffer)))
          (spacemacs-buffer/set-mode-line "")
          ;; needed in case the buffer was deleted and we are recreating it
          (setq spacemacs-buffer--note-widgets nil)
          (spacemacs-buffer/insert-banner-and-buttons)
          ;; non-nil if emacs-startup-hook was run
          (if (bound-and-true-p spacemacs-initialized)
              (progn
                (configuration-layer/display-summary emacs-start-time)
                (when dotspacemacs-startup-lists
                  (spacemacs-buffer/insert-startupify-lists))
                (spacemacs-buffer//insert-footer)
                (spacemacs-buffer/set-mode-line spacemacs--default-mode-line)
                (force-mode-line-update)
                (spacemacs-buffer-mode))
            (add-hook 'emacs-startup-hook 'spacemacs-buffer//startup-hook t))))
      (if save-line
          (progn (goto-char (point-min))
                 (forward-line (1- save-line))
                 (forward-to-indentation 0))
        (spacemacs-buffer/goto-link-line))
      (switch-to-buffer spacemacs-buffer-name)
      (spacemacs//redisplay))))

(add-hook 'window-setup-hook
          (lambda ()
            (add-hook 'window-configuration-change-hook 'spacemacs-buffer//resize-on-hook)
            (spacemacs-buffer//resize-on-hook)))

(defun spacemacs-buffer//resize-on-hook ()
  (let ((space-win (get-buffer-window spacemacs-buffer-name))
        (frame-win (frame-selected-window)))
    (when (and dotspacemacs-startup-buffer-responsive
               space-win
               (not (window-minibuffer-p frame-win)))
      (with-selected-window space-win
        (spacemacs-buffer/goto-buffer)))))

(defun spacemacs-buffer/refresh ()
  "Force recreation of the spacemacs buffer."
  (interactive)
  (setq spacemacs-buffer--last-width nil)
  (spacemacs-buffer/goto-buffer t))

(defalias 'spacemacs/home 'spacemacs-buffer/refresh
  "Go to home Spacemacs buffer")

(defun spacemacs/home-delete-other-windows ()
  "Open home Spacemacs buffer and delete other windows.
Useful for making the home buffer the only visible buffer in the frame."
  (interactive)
  (spacemacs/home)
  (delete-other-windows))

(provide 'core-spacemacs-buffer)
