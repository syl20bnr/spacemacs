;;; core-space-macs-buffer.el --- Space-macs Core File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3
;;
;;; Commentary:
;;
;;; Code:

(defconst space-macs-buffer-version-info "0.300"
  "Current version used to display addition release information.")

(defconst space-macs-buffer-name "*space-macs*"
  "The name of the space-macs buffer.")

(defconst space-macs-buffer-logo-title "[S P A C E M A C S]"
  "The title displayed beneath the logo.")

(defconst space-macs-buffer-buttons-startup-lists-offset 25
  "Relative position between the home buffer buttons and startup lists.")

(defconst space-macs-buffer--window-width 80
  "Current width of the home buffer if responsive, 80 otherwise.
See `dotspace-macs-startup-buffer-responsive'.")

(defconst space-macs-buffer--cache-file
  (expand-file-name (concat space-macs-cache-directory "space-macs-buffer.el"))
  "Cache file for various persistent data for the space-macs startup buffer.")

(defvar space-macs-buffer-startup-lists-length 20
  "Length used for startup lists with otherwise unspecified bounds.
Set to nil for unbounded.")

(defvar space-macs-buffer--release-note-version nil
  "If nil the release note is displayed.
If non nil it contains a version number, if the version number is lesser than
the current version the release note it displayed")

(defvar space-macs-buffer--note-widgets nil
  "List of widgets used in currently inserted notes.
Allows to keep track of widgets to delete when removing them.")

(defvar space-macs-buffer--current-note-type nil
  "Type of note currently displayed.")

(defvar space-macs-buffer--fresh-install
  (not (file-exists-p dotspace-macs-filepath))
  "Non-nil if this e-macs instance if a fresh install.")

(defvar space-macs-buffer--buttons-position nil
  "Horizontal position of the home buffer buttons.
Internal use, do not set this variable.")

(defvar space-macs-buffer-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [down-mouse-1] 'widget-button-click)
    (define-key map (kbd "RET") 'widget-button-press)

    (define-key map [tab] 'widget-forward)
    (define-key map (kbd "J") 'widget-forward)
    (define-key map (kbd "C-i") 'widget-forward)

    (define-key map [backtab] 'widget-backward)
    (define-key map (kbd "K") 'widget-backward)

    (define-key map (kbd "C-r") 'space-macs-buffer/refresh)
    (define-key map "q" 'quit-window)
    map)
  "Keymap for space-macs buffer mode.")

(with-eval-after-load 'evil
  (evil-make-overriding-map space-macs-buffer-mode-map 'motion))

(define-derived-mode space-macs-buffer-mode fundamental-mode "Space-macs buffer"
  "Space-macs major mode for startup screen."
  :group 'space-macs
  :syntax-table nil
  :abbrev-table nil
  (page-break-lines-mode)
  (setq buffer-read-only t
        truncate-lines t)
  ;; needed to make tab work correctly in terminal
  (evil-define-key 'motion space-macs-buffer-mode-map
    (kbd "C-i") 'widget-forward)
  ;; motion state since this is a special mode
  (evil-set-initial-state 'space-macs-buffer-mode 'motion))

(defun space-macs-buffer//insert-ascii-banner-centered (file)
  "Insert the ascii banner contain in file and center it in the window.
FILE: the path to the file containing the banner."
  (insert
   (with-temp-buffer
     (insert-file-contents file)
     (let ((banner-width 0))
       (while (not (eobp))
         (let ((line-length (- (line-end-position) (line-beginning-position))))
           (if (< banner-width line-length)
               (setq banner-width line-length)))
         (forward-line 1))
       (goto-char 0)
       (let ((margin (max 0 (floor (/ (- space-macs-buffer--window-width
                                         banner-width) 2)))))
         (while (not (eobp))
           (insert (make-string margin ?\s))
           (forward-line 1)))
       (insert "\n"))
     (buffer-string))))

(defun space-macs-buffer/insert-banner-and-buttons ()
  "Choose a banner according to `dotspace-macs-startup-banner'and insert it.
in space-macs buffer along with quick buttons underneath.
Easter egg:
Doge special text banner can be reachable via `999', `doge' or `random*'.
Doge special text banner for dark themes can be reachable via `997',
`doge-inverted' or `random*'.
Cate special text banner can de reachable via `998', `cat' or `random*'.
`random' ignore special banners whereas `random*' does not."
  (let ((banner (space-macs-buffer//choose-banner))
        (buffer-read-only nil))
    (progn
      (when banner
        (space-macs-buffer/message (format "Banner: %s" banner))
        (if (image-type-available-p (intern (file-name-extension banner)))
            (space-macs-buffer//insert-image-banner banner)
          (space-macs-buffer//insert-ascii-banner-centered banner)))
      (space-macs-buffer//insert-buttons)
      (space-macs//redisplay))))

(defun space-macs-buffer/display-startup-note ()
  "Decide of the startup note and display it if relevant."
  (when (file-exists-p space-macs-buffer--cache-file)
    (load space-macs-buffer--cache-file nil (not init-file-debug)))
  (cond
   (space-macs-buffer--fresh-install
    ;; we assume the user is  new to space-macs and open the quickhelp
    (space-macs-buffer/toggle-note 'quickhelp)
    (setq space-macs-buffer--release-note-version space-macs-version)
    (space-macs/dump-vars-to-file '(space-macs-buffer--release-note-version)
                                 space-macs-buffer--cache-file))
   ((or (not space-macs-buffer--release-note-version)
        (version< space-macs-buffer--release-note-version
                  space-macs-version))
    ;; check the variable space-macs-buffer--release-note-version
    ;; to decide whether we show the release note
    (space-macs-buffer/toggle-note 'release-note)))
  (space-macs//redisplay))

(defun space-macs-buffer//choose-banner ()
  "Return the full path of a banner based on the dotfile value."
  (when dotspace-macs-startup-banner
    (cond ((eq 'official dotspace-macs-startup-banner)
           (if (and (display-graphic-p) (image-type-available-p 'png))
               space-macs-banner-official-png
             (space-macs-buffer//get-banner-path 1)))
          ((eq 'random dotspace-macs-startup-banner)
           (space-macs-buffer//choose-random-text-banner))
          ((eq 'random* dotspace-macs-startup-banner)
           (space-macs-buffer//choose-random-text-banner t))
          ((eq 'doge dotspace-macs-startup-banner)
           (space-macs-buffer//get-banner-path 999))
          ((eq 'doge-inverted dotspace-macs-startup-banner)
           (space-macs-buffer//get-banner-path 997))
          ((eq 'cat dotspace-macs-startup-banner)
           (space-macs-buffer//get-banner-path 998))
          ((integerp dotspace-macs-startup-banner)
           (space-macs-buffer//get-banner-path dotspace-macs-startup-banner))
          ((and dotspace-macs-startup-banner
                (image-type-available-p (intern (file-name-extension
                                                 dotspace-macs-startup-banner)))
                (display-graphic-p))
           (if (file-exists-p dotspace-macs-startup-banner)
               dotspace-macs-startup-banner
             (space-macs-buffer/warning (format "could not find banner %s"
                                               dotspace-macs-startup-banner))
             (space-macs-buffer//get-banner-path 1)))
          (t (space-macs-buffer//get-banner-path 1)))))

(defvar space-macs-buffer--random-banner nil
  "The random banner chosen.")

(defun space-macs-buffer//choose-random-text-banner (&optional all)
  "Return the full path of a banner chosen randomly.
If ALL is non-nil then truly all banners can be selected."
  (setq space-macs-buffer--random-banner
        (or space-macs-buffer--random-banner
            (let* ((files (directory-files space-macs-banner-directory t ".*\.txt"))
                   (count (length files))
                   ;; -2 to remove the two last ones (easter eggs)
                   (choice (random (- count (if all 0 2)))))
              (nth choice files)))))

(defun space-macs-buffer//get-banner-path (index)
  "Return the full path to banner with index INDEX."
  (concat space-macs-banner-directory (format "%03d-banner.txt" index)))

(defun space-macs-buffer//insert-image-banner (banner)
  "Display an image banner.
BANNER: the path to an ascii banner file."
  (when (file-exists-p banner)
    (let* ((title space-macs-buffer-logo-title)
           (spec (create-image banner))
           (size (image-size spec))
           (width (car size))
           (left-margin (max 0 (floor (- space-macs-buffer--window-width width) 2))))
      (insert (make-string left-margin ?\s))
      (insert-image spec)
      (insert "\n\n")
      (insert (make-string (max 0 (floor (/ (- space-macs-buffer--window-width
                                               (+ (length title) 1)) 2))) ?\s))
      (insert (format "%s\n\n" title)))))

(defun space-macs-buffer//insert-version ()
  "Insert the current version of Space-macs and e-macs.
Right justified, based on the Space-macs buffers window width."
  (with-current-buffer (get-buffer-create space-macs-buffer-name)
    (let ((version (format "%s@%s (%s)"
                           space-macs-version
                           e-macs-version
                           dotspace-macs-distribution))
          (buffer-read-only nil))
      (insert (format (format "%%%ds"
                              (if (display-graphic-p)
                                  space-macs-buffer--window-width
                                ;; terminal needs one less char
                                (1- space-macs-buffer--window-width)))
                      version))
      (insert "\n\n"))))

(defun space-macs-buffer//insert-footer ()
  "Insert the footer of the home buffer."
  (save-excursion
    (let* ((badge-path space-macs-badge-official-png)
           (badge (when (and (display-graphic-p)
                             (image-type-available-p
                              (intern (file-name-extension badge-path))))
                    (create-image badge-path)))
           (badge-size (when badge (car (image-size badge))))
           (heart-path space-macs-purple-heart-png)
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
        (space-macs-buffer/insert-page-break)
        (insert "\n")
        (when badge
          (insert-image badge)
          (space-macs-buffer//center-line badge-size))
        (when heart
          (when badge (insert "\n\n"))
          (insert build-lhs)
          (insert-image heart)
          (insert build-rhs)
          (space-macs-buffer//center-line (+ (length build-lhs)
                                            heart-size
                                            (length build-rhs)))
          (insert "\n"))))))

(defmacro space-macs-buffer||notes-adapt-caption-to-width (caption
                                                          caption-length
                                                          width)
  "Adapt caption string's length to the note's frame current width.
For internal use in `space-macs-buffer//notes-render-framed-text'.
CAPTION: string to be encrusted onto the note's frame
CAPTION-LENGTH: length of the caption
WIDTH: current external width of the note's frame."
  `(when (> ,caption-length (- ,width 6)) ; minimum frame width is 6
     (if (> ,width 8)
         (setq ,caption (concat (substring ,caption
                                           0
                                           (min -3 (- (- ,width 6 3)
                                                      ,caption-length)))
                                "..."))
       (setq ,caption nil
             ,caption-length 0))))

(defun space-macs-buffer//notes-render-framed-text
    (content &optional topcaption botcaption hpadding max-width min-width)
  "Return a formatted string framed with curved lines.
The width of the created frame is the width of the content, unless it does not
satisfy max-width or min-width.  Note that max-width can be limited by the
window's width.
CONTENT can be a text or a filepath.
TOPCAPTION is a text to be encrusted at the top of the frame.
BOTCAPTION is a text to be encrusted at the bottom of the frame.
HPADDING is the horizontal spacing between the text and the frame.  The vertical
         spacing is always one line.
MAX-WIDTH is the maximum width of the frame,  frame included.  When
          `dotspace-macs-startup-buffer-responsive' is t, MAX-WIDTH will be
          limited to the window's width.  MAX-WIDTH takes precedence over
          MIN-WIDTH.
MIN-WIDTH is the minimal width of the frame, frame included.  The frame will not
          shrink any thinner than MIN-WIDTH characters unless MAX-WIDTH says
          otherwise."
  (with-temp-buffer
    (if (not (file-exists-p content))
        (insert content)
      (insert-file-contents content)
      (goto-char (point-max))
      (when (eq ?\n (char-before))    ;; remove additional newline at eof
        (delete-char -1)))
    (let* ((hpadding (if hpadding hpadding 1))
           (text-width (space-macs-buffer//get-buffer-width))
           (width (+ 2 (* 2 hpadding) text-width))
           (fill-column text-width)
           (sentence-end-double-space nil)    ; needed by fill-region
           (paragraph-start "\f\\|[ \t]*$\\|[ \t]*[-+*] \\|[ \t]*[0-9]+[.)] ")
           (topcaption-length (if topcaption (length topcaption) 0))
           (botcaption-length (if botcaption (length botcaption) 0)))
      (setq max-width (or max-width width)
            min-width (or min-width 1)
            max-width (if (< max-width min-width) min-width max-width)
            max-width (if (> max-width space-macs-buffer--window-width)
                          space-macs-buffer--window-width
                        max-width))
      (when (< width min-width)
        (setq width min-width
              fill-column (max 0 (- min-width 2 (* hpadding 2)))))
      (when (> width max-width)
        (setq width max-width
              fill-column (max 0 (- max-width 2 (* hpadding 2)))))
      (space-macs-buffer||notes-adapt-caption-to-width topcaption
                                                      topcaption-length
                                                      width)
      (space-macs-buffer||notes-adapt-caption-to-width botcaption
                                                      botcaption-length
                                                      width)
      (fill-region (point-min) (point-max) nil nil)
      (concat
       "â•­â”€" (when topcaption (propertize (concat " " topcaption " ")
                                         'face
                                         '(:weight bold)))
       (make-string (max 0 (- width (if topcaption 6 4) topcaption-length)) ?â”€) "â”€â•®\n"
       (space-macs-buffer//notes-render-framed-line "" width hpadding)
       (mapconcat (lambda (line)
                    (space-macs-buffer//notes-render-framed-line line width hpadding))
                  (split-string (buffer-string) "\n" nil) "")
       (space-macs-buffer//notes-render-framed-line "" width hpadding)
       "â•°â”€" (when botcaption (propertize (concat " " botcaption " ")
                                         'face '(:weight bold)))
       (make-string (max 0 (- width (if botcaption 6 4) botcaption-length)) ?â”€)
       "â”€â•¯" (when botcaption "\n")))))


(defun space-macs-buffer//notes-render-framed-line (line width hpadding)
  "Return a formatted LINE with borders of a frame on each side.
WIDTH: external width of the frame.  LINE should be shorter than WIDTH.
HPADDING: horizontal padding on both sides of the framed string."
  (let ((fill (max 0 (- width 2 hpadding (length line)))))
    (concat "â”‚" (make-string hpadding ?\s) line (make-string fill ?\s)
            "â”‚\n")))

(defun space-macs-buffer//notes-insert-note
    (file topcaption botcaption &optional additional-widgets)
  "Insert the release note just under the banner.
FILE: the file that contains the content to show.
TOPCAPTION: the title of the note.
BOTCAPTION: a text to be encrusted at the bottom of the frame.
ADDITIONAL-WIDGETS: a function for inserting a widget under the frame."
  (save-excursion
    (goto-char (point-min))
    (search-forward "Search in Space-macs\]") ; TODO: this is dirty
    (forward-line)
    (let* ((buffer-read-only nil)
           (note (concat "\n"
                         (space-macs-buffer//notes-render-framed-text file
                                                                     topcaption
                                                                     botcaption
                                                                     2
                                                                     nil
                                                                     80))))
      (save-restriction
        (narrow-to-region (point) (point))
        (add-to-list 'space-macs-buffer--note-widgets (widget-create 'text :format "%v" note))
        (let* ((width (space-macs-buffer//get-buffer-width))
               (padding (max 0 (floor (/ (- space-macs-buffer--window-width
                                            width) 2)))))
          (goto-char (point-min))
          (while (not (eobp))
            (beginning-of-line)
            (insert (make-string padding ?\s))
            (forward-line))))
      (save-excursion
        (while (re-search-backward "\\[\\[\\(.*\\)\\]\\]" nil t)
          (make-text-button (match-beginning 1)
                            (match-end 1)
                            'type 'help-url
                            'help-args (list (match-string 1)))))
      (funcall additional-widgets)
      (space-macs-buffer//center-line)
      (delete-trailing-whitespace (line-beginning-position)
                                  (line-end-position)))))

(defun space-macs-buffer//notes-insert-quickhelp ()
  "Insert quickhelp."
  (let ((widget-func
         (lambda ()
           (add-to-list
            'space-macs-buffer--note-widgets
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
            'space-macs-buffer--note-widgets
            (widget-create 'push-button
                           :tag (propertize "e-macs Tutorial"
                                            'face 'font-lock-keyword-face)
                           :help-echo "Teach you how to use e-macs basics."
                           :action (lambda (&rest ignore)
                                     (call-interactively #'help-with-tutorial))
                           :mouse-face 'highlight
                           :follow-link "\C-m"))
           (widget-insert " ")
           (add-to-list
            'space-macs-buffer--note-widgets
            (widget-create 'push-button
                           :tag (propertize "Vim Migration Guide"
                                            'face 'font-lock-keyword-face)
                           :help-echo "Documentation for former vim users."
                           :action (lambda (&rest ignore)
                                     (space-macs/view-org-file
                                      (concat space-macs-docs-directory
                                              "VIMUSERS.org") "^" 'all))
                           :mouse-face 'highlight
                           :follow-link "\C-m")))))
    (space-macs-buffer//notes-insert-note (concat space-macs-info-directory
                                                 "quickhelp.txt")
                                         "Quick Help"
                                         nil
                                         widget-func)))

(defun space-macs-buffer//notes-insert-release-note ()
  "Insert release note."
  (let ((widget-func
         (lambda ()
           (add-to-list
            'space-macs-buffer--note-widgets
            (widget-create 'push-button
                           :tag (propertize "Click here for full change log"
                                            'face 'font-lock-warning-face)
                           :help-echo "Open the full change log."
                           :action
                           (lambda (&rest ignore)
                             (funcall 'space-macs/view-org-file
                                      (concat space-macs-start-directory
                                              "CHANGELOG.org")
                                      (format "Release %s.x"
                                              space-macs-buffer-version-info)
                                      'subtree))
                           :mouse-face 'highlight
                           :follow-link "\C-m")))))
    (space-macs-buffer//notes-insert-note (concat space-macs-release-notes-directory
                                                 space-macs-buffer-version-info
                                                 ".txt")
                                         (format "Important Notes (Release %s.x)"
                                                 space-macs-buffer-version-info)
                                         "Update your dotfile (SPC f e D) and\
 packages after every update"
                                         widget-func))
  (setq space-macs-buffer--release-note-version nil)
  (space-macs/dump-vars-to-file '(space-macs-buffer--release-note-version)
                               space-macs-buffer--cache-file))

(defun space-macs-buffer//notes-clear-notes-and-widgets ()
  "Remove existing note widgets if exists."
  (when space-macs-buffer--note-widgets
    (mapc 'widget-delete space-macs-buffer--note-widgets)
    (setq space-macs-buffer--note-widgets nil)
    (setq space-macs-buffer--release-note-version space-macs-version)
    (space-macs/dump-vars-to-file
     '(space-macs-buffer--release-note-version) space-macs-buffer--cache-file)))

(defun space-macs-buffer//notes-redisplay-current-note ()
  "Delete and rediplay the currently displayed note."
  (space-macs-buffer//notes-clear-notes-and-widgets)
  (let ((type space-macs-buffer--current-note-type))
    (cond
     ((eq type 'quickhelp) (space-macs-buffer//notes-insert-quickhelp))
     ((eq type 'release-note) (space-macs-buffer//notes-insert-release-note))
     (t))))

(defun space-macs-buffer/toggle-note (type)
  "Toggle the displayed note based on TYPE.
If TYPE is nil or unknown, just remove the currently displayed note.  Currently
allowed types are `quickhelp' and `release-note'"
  (space-macs-buffer//notes-clear-notes-and-widgets)
  (if (or (eq space-macs-buffer--current-note-type nil)
          (not (eq space-macs-buffer--current-note-type type)))
      (progn
        (setq space-macs-buffer--current-note-type type)
        (cond
         ((eq type 'quickhelp) (space-macs-buffer//notes-insert-quickhelp))
         ((eq type 'release-note) (space-macs-buffer//notes-insert-release-note))
         (t (setq space-macs-buffer--current-note-type nil)
            (message "Unknown note type: %s" 'type))))
    (setq space-macs-buffer--current-note-type nil)))

(defun space-macs-buffer/set-mode-line (format &optional redisplay)
  "Set mode-line format for space-macs buffer.
FORMAT: the `mode-line-format' variable e-macs will use to build the mode-line.
If REDISPLAY is non-nil then force a redisplay as well"
  (with-current-buffer (get-buffer-create space-macs-buffer-name)
    (setq mode-line-format format))
  (when redisplay (space-macs//redisplay)))

(defun space-macs-buffer/message (msg &rest args)
  "Display MSG in *Messages* prepended with '(Space-macs)'.
The message is displayed only if `init-file-debug' is non nil.
ARGS: format string arguments."
  (when init-file-debug
    (message "(Space-macs) %s" (apply 'format msg args))))

(defvar space-macs-buffer--errors nil
  "List of errors during startup.")

(defun space-macs-buffer/error (msg &rest args)
  "Display MSG as an Error message in `*Messages*' buffer.
ARGS: format string arguments."
  (let ((msg (apply 'format msg args)))
    (message "(Space-macs) Error: %s" msg)
    (when message-log-max
      (add-to-list 'space-macs-buffer--errors msg 'append))))

(defvar space-macs-buffer--warnings nil
  "List of warnings during startup.")

(defun space-macs-buffer/warning (msg &rest args)
  "Display MSG as a warning message but in buffer `*Messages*'.
ARGS: format string arguments."
  (let ((msg (apply 'format msg args)))
    (message "(Space-macs) Warning: %s" msg)
    (when message-log-max
      (add-to-list 'space-macs-buffer--warnings msg 'append))))

(defun space-macs-buffer/insert-page-break ()
  "Insert a page break line in space-macs buffer."
  (space-macs-buffer/append "\n\n"))

(defun space-macs-buffer/append (msg &optional messagebuf)
  "Append MSG to space-macs buffer.
If MESSAGEBUF is not nil then MSG is also written in message buffer."
  (with-current-buffer (get-buffer-create space-macs-buffer-name)
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (insert msg)
      (when messagebuf
        (message "(Space-macs) %s" msg)))))

(defun space-macs-buffer/replace-last-line (msg &optional messagebuf)
  "Replace the last line of the space-macs buffer with MSG.
If MESSAGEBUF is not nil then MSG is also written in message buffer."
  (with-current-buffer (get-buffer-create space-macs-buffer-name)
    (goto-char (point-max))
    (let ((buffer-read-only nil))
      (delete-region (line-beginning-position) (point-max))
      (insert msg)
      (when messagebuf
        (message "(Space-macs) %s" msg)))))

(defmacro space-macs-buffer||add-shortcut
    (shortcut-char search-label &optional no-next-line)
  "Add a single-key keybinding for quick navigation in the home buffer.
Navigation is done by searching for a specific word in the buffer.
SHORTCUT-CHAR: the key that the user will have to press.
SEARCH-LABEL: the word the cursor will be brought under (or on).
NO-NEXT-LINE: if nil the cursor is brought under the searched word."
  `(define-key space-macs-buffer-mode-map
     ,shortcut-char (lambda ()
                      (interactive)
                      (unless (search-forward ,search-label (point-max) t)
                        (search-backward ,search-label (point-min) t))
                      ,@(unless no-next-line
                          '((forward-line 1)))
                      (back-to-indentation))))

(defun space-macs-buffer//center-line (&optional real-width)
  "When point is at the end of a line, center it.
REAL-WIDTH: the real width of the line.  If the line contains an image, the size
            of that image will be considered to be 1 by the calculation method
            used in this function.  As a consequence, the caller must calculate
            himself the correct length of the line taking into account the
            images he inserted in it."
  (let* ((width (or real-width (current-column)))
         (margin (max 0 (floor (/ (- space-macs-buffer--window-width
                                     width)
                                  2)))))
    (beginning-of-line)
    (insert (make-string margin ?\s))
    (end-of-line)))

(defun space-macs-buffer//insert-buttons ()
  "Create and insert the interactive buttons under Space-macs banner."
  (goto-char (point-max))
  (space-macs-buffer||add-shortcut "m" "[?]" t)
  (widget-create 'url-link
                 :tag (propertize "?" 'face 'font-lock-doc-face)
                 :help-echo "Open the quickhelp."
                 :action (lambda (&rest ignore)
                           (space-macs-buffer/toggle-note 'quickhelp))
                 :mouse-face 'highlight
                 :follow-link "\C-m")
  (insert " ")
  (widget-create 'url-link
                 :tag (propertize "Homepage" 'face 'font-lock-keyword-face)
                 :help-echo "Open the Space-macs GitHub page in your browser."
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 "http://space-macs.org")
  (insert " ")
  (widget-create 'url-link
                 :tag (propertize "Documentation" 'face 'font-lock-keyword-face)
                 :help-echo "Open the Space-macs documentation in your browser."
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 "http://space-macs.org/doc/DOCUMENTATION.html")
  (insert " ")
  (widget-create 'url-link
                 :tag (propertize "Gitter Chat" 'face 'font-lock-keyword-face)
                 :help-echo
                 "Ask questions and chat with fellow users in our chat room."
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 "https://gitter.im/syl20bnr/space-macs")
  (insert " ")
  (widget-create 'push-button
                 :help-echo "Update Space-macs core and layers."
                 :action (lambda (&rest ignore) (space-macs/switch-to-version))
                 :mouse-face 'highlight
                 :follow-link "\C-m"
                 (propertize "Update Space-macs" 'face 'font-lock-keyword-face))
  (let ((len (- (line-end-position)
                (line-beginning-position))))
    (space-macs-buffer//center-line)
    (setq space-macs-buffer--buttons-position (- (line-end-position)
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
  (space-macs-buffer//center-line)
  (insert "\n")
  (widget-create 'push-button
                 :tag (propertize "Release Notes"
                                  'face 'font-lock-preprocessor-face)
                 :help-echo "Hide or show the Changelog"
                 :action (lambda (&rest ignore)
                           (space-macs-buffer/toggle-note 'release-note))
                 :mouse-face 'highlight
                 :follow-link "\C-m")
  (insert " ")
  (widget-create 'url-link
                 :tag (propertize "Search in Space-macs"
                                  'face 'font-lock-function-name-face)
                 :help-echo "Search Space-macs contents."
                 :action
                 (lambda (&rest ignore)
                   (let ((comp-frontend
                          (cond
                           ((configuration-layer/layer-used-p 'helm)
                            'helm-space-macs-help)
                           ((configuration-layer/layer-used-p 'ivy)
                            'ivy-space-macs-help))))
                     (call-interactively comp-frontend)))
                 :mouse-face 'highlight
                 :follow-link "\C-m")
  (space-macs-buffer//center-line)
  (insert "\n"))

(defun space-macs-buffer//insert-string-list (list-display-name list)
  "Insert a non-interactive startup list in the home buffer.
LIST-DISPLAY-NAME: the displayed title of the list.
LIST: a list of strings displayed as entries."
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

(defun space-macs-buffer//insert-file-list (list-display-name list)
  "Insert an interactive list of files in the home buffer.
LIST-DISPLAY-NAME: the displayed title of the list.
LIST: a list of string pathnames made interactive in this function."
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

(defun space-macs-buffer//insert-bookmark-list (list-display-name list)
  "Insert an interactive list of bookmarks entries (if any) in the home buffer.
LIST-DISPLAY-NAME: the displayed title of the list.
LIST: a list of string bookmark names made interactive in this function."
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

(defun space-macs-buffer//get-org-items (types)
  "Make a list of agenda file items for today of kind types.
TYPES: list of `org-mode' types to fetch."
  (require 'org-agenda)
  (let ((date (calendar-gregorian-from-absolute (org-today))))
    (apply #'append
           (cl-loop for file in (org-agenda-files nil 'ifmode)
                    collect
                    (space-macs-buffer//make-org-items
                     file
                     (apply 'org-agenda-get-day-entries file date
                            types))))))

(defun space-macs-buffer//agenda-list ()
  "Return today's agenda."
  (require 'org-agenda)
  (space-macs-buffer//get-org-items org-agenda-entry-types))

(defun space-macs-buffer//todo-list ()
  "Return current todos."
  (require 'org-agenda)
  (space-macs-buffer//get-org-items '(:todo)))

(defun space-macs-buffer//make-org-items (file items)
  "Make a space-macs-buffer org item list.
FILE: file name.
ITEMS:"
  (cl-loop
   for item in items
   collect
   (space-macs-buffer//make-org-item file item)))

(defun space-macs-buffer//make-org-item (file item)
  "Make a space-macs-buffer version of an org item.
FILE: file name.
ITEM:"
  (list (cons "text"
              (get-text-property 0 'txt item))
        (cons "file" file)
        (cons "pos"
              (marker-position
               (get-text-property 0 'org-marker item)))
        (cons "time"
              (get-text-property 0 'time item))))

(defun space-macs-buffer//org-jump (el)
  "Action executed when using an item in the home buffer's todo list.
EL: `org-agenda' element to jump to."
  (require 'org-agenda)
  (find-file-other-window (cdr (assoc "file" el)))
  (widen)
  (goto-char (cdr (assoc "pos" el)))
  (when (derived-mode-p 'org-mode)
    (org-show-context 'agenda)
    (save-excursion
      (and (outline-next-heading)
           (org-flag-heading nil)))    ; show the next heading
    (when (outline-invisible-p)
      (outline-show-entry))            ; display invisible text
    (recenter (/ (window-height) 2))
    (org-back-to-heading t)
    (if (re-search-forward org-complex-heading-regexp nil t)
        (goto-char (match-beginning 4))))
  (run-hooks 'org-agenda-after-show-hook))

(defun space-macs-buffer//insert-todo-list (list-display-name list)
  "Insert an interactive todo list of `org-agenda' entries in the home buffer.
LIST-DISPLAY-NAME: the displayed title of the list.
LIST: list of `org-agenda' entries in the todo list."
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
                                      (space-macs-buffer//org-jump ',el))
                           :mouse-face 'highlight
                           :follow-link "\C-m"
                           :button-prefix ""
                           :button-suffix ""
                           :format "%[%t%]"
                           (format "%s %s %s"
                                   (let ((filename (cdr (assoc "file" el))))
                                     (if dotspace-macs-home-shorten-agenda-source
                                         (file-name-nondirectory filename)
                                       (abbreviate-file-name filename)))
                                   (if (not (eq "" (cdr (assoc "time" el))))
                                       (format "- %s -"
                                               (cdr (assoc "time" el)))
                                     "-")
                                   (cdr (assoc "text" el)))))
          list)))

(defun space-macs//subseq (seq start end)
  "Adapted version of `cl-subseq'.
Use `cl-subseq', but accounting for end points greater than the size of the
list.  Return entire list if end is omitted.
SEQ, START and END are the same arguments as for `cl-subseq'"
  (let ((len (length seq)))
    (cl-subseq seq start (and (number-or-marker-p end)
                              (min len end)))))

(defun space-macs-buffer//do-insert-startupify-lists ()
  "Insert the startup lists in the current buffer."
  (let ((list-separator "\n\n"))
    (mapc (lambda (els)
            (let ((el (or (car-safe els) els))
                  (list-size
                   (or (cdr-safe els)
                       space-macs-buffer-startup-lists-length)))
              (cond
               ((eq el 'warnings)
                (when (space-macs-buffer//insert-string-list
                       "Errors:" space-macs-buffer--errors)
                  (space-macs-buffer||add-shortcut "e" "Errors:")
                  (insert list-separator))
                (when (space-macs-buffer//insert-string-list
                       "Warnings:" space-macs-buffer--warnings)
                  (space-macs-buffer||add-shortcut "w" "Warnings:")
                  (insert list-separator)))
               ((eq el 'recents)
                (recentf-mode)
                (when (space-macs-buffer//insert-file-list
                       "Recent Files:"
                       (space-macs//subseq recentf-list 0 list-size))
                  (space-macs-buffer||add-shortcut "r" "Recent Files:")
                  (insert list-separator)))
               ((eq el 'todos)
                (when (space-macs-buffer//insert-todo-list
                       "ToDo:"
                       (space-macs//subseq (space-macs-buffer//todo-list)
                                          0 list-size))
                  (space-macs-buffer||add-shortcut "d" "ToDo:")
                  (insert list-separator)))
               ((eq el 'agenda)
                (when (space-macs-buffer//insert-todo-list
                       "Agenda:"
                       (space-macs//subseq (space-macs-buffer//agenda-list)
                                          0 list-size))
                  (space-macs-buffer||add-shortcut "c" "Agenda:")
                  (insert list-separator)))
               ((eq el 'bookmarks)
                (when (configuration-layer/layer-used-p 'space-macs-helm)
                  (helm-mode))
                (require 'bookmark)
                (when (space-macs-buffer//insert-bookmark-list
                       "Bookmarks:"
                       (space-macs//subseq (bookmark-all-names)
                                          0 list-size))
                  (space-macs-buffer||add-shortcut "b" "Bookmarks:")
                  (insert list-separator)))
               ((and (eq el 'projects)
                     (fboundp 'projectile-mode))
                (projectile-mode)
                (when (space-macs-buffer//insert-file-list
                       "Projects:"
                       (space-macs//subseq (projectile-relevant-known-projects)
                                          0 list-size))
                  (space-macs-buffer||add-shortcut "p" "Projects:")
                  (insert list-separator))))))
          (append
           '(warnings)
           dotspace-macs-startup-lists))))

(defun space-macs-buffer//get-buffer-width ()
  "Return the length of longest line in the current buffer."
  (save-excursion
    (goto-char 0)
    (let ((current-max 0))
      (while (not (eobp))
        (let ((line-length (- (line-end-position) (line-beginning-position))))
          (if (< current-max line-length)
              (setq current-max line-length)))
        (forward-line 1))
      current-max)))

(defun space-macs-buffer//center-startup-lists ()
  "Center startup lists after they were inserted."
  (let* ((lists-width (space-macs-buffer//get-buffer-width))
         (margin (max 0 (- space-macs-buffer--buttons-position
                           space-macs-buffer-buttons-startup-lists-offset)))
         (final-padding (if (< space-macs-buffer--window-width
                               (+ margin lists-width))
                            (max 0 (floor (/ (- space-macs-buffer--window-width
                                                lists-width)
                                             2)))
                          margin)))
    (goto-char (point-min))
    (while (not (eobp))
      (beginning-of-line)
      (insert (make-string final-padding ?\s))
      (forward-line))))

(defun space-macs-buffer/insert-startup-lists ()
  "Insert startup lists in home buffer."
  (interactive)
  (with-current-buffer (get-buffer space-macs-buffer-name)
    (let ((buffer-read-only nil))
      (goto-char (point-max))
      (space-macs-buffer/insert-page-break)
      (insert "\n")
      (save-restriction
        (narrow-to-region (point) (point))
        (space-macs-buffer//do-insert-startupify-lists)
        (space-macs-buffer//center-startup-lists)))))

(defun space-macs-buffer/goto-link-line ()
  "Set point to the beginning of the link line."
  (interactive)
  (with-current-buffer space-macs-buffer-name
    (goto-char (point-min))
    (with-demoted-errors "space-macs buffer error: %s"
      (search-forward "[")
      (left-char 2))))

(defun space-macs-buffer//startup-hook ()
  "Code executed when e-macs has finished loading."
  (with-current-buffer (get-buffer space-macs-buffer-name)
    (when dotspace-macs-startup-lists
      (space-macs-buffer/insert-startup-lists))
    (space-macs-buffer//insert-footer)
    (if configuration-layer-error-count
        (progn
          (space-macs-buffer-mode)
          (face-remap-add-relative 'mode-line
                                   '((:background "red") mode-line))
          (space-macs-buffer/set-mode-line
           (format
            (concat "%s error(s) at startup! "
                    "Space-macs may not be able to operate properly.")
            configuration-layer-error-count) t))
      (space-macs-buffer/set-mode-line space-macs--default-mode-line)
      (space-macs-buffer-mode))
    (force-mode-line-update)
    (space-macs-buffer/goto-link-line)))

(defvar space-macs-buffer--last-width nil
  "Previous width of space-macs-buffer.")

(defun space-macs-buffer/goto-buffer (&optional refresh)
  "Create the special buffer for `space-macs-buffer-mode' and switch to it.
REFRESH if the buffer should be redrawn.

If a prefix argument is given, switch to it in an other, possibly new window."
  (interactive)
  (let ((buffer-exists (buffer-live-p (get-buffer space-macs-buffer-name)))
        (save-line nil))
    (when (not buffer-exists)
      (setq space-macs-buffer--note-widgets nil))
    (when (or (not (eq space-macs-buffer--last-width (window-width)))
              (not buffer-exists)
              refresh)
      (setq space-macs-buffer--window-width (if dotspace-macs-startup-buffer-responsive
                                               (window-width)
                                             80)
            space-macs-buffer--last-width space-macs-buffer--window-width)
      (with-current-buffer (get-buffer-create space-macs-buffer-name)
        (page-break-lines-mode)
        (save-excursion
          (when (> (buffer-size) 0)
            (set 'save-line (line-number-at-pos))
            (let ((inhibit-read-only t))
              (erase-buffer)))
          (space-macs-buffer/set-mode-line "")
          (if dotspace-macs-startup-buffer-show-version
              (space-macs-buffer//insert-version)
            (let ((inhibit-read-only t))
              (insert "\n")))
          (space-macs-buffer/insert-banner-and-buttons)
          (when (bound-and-true-p space-macs-initialized)
            (space-macs-buffer//notes-redisplay-current-note)
            (configuration-layer/display-summary e-macs-start-time)
            (when dotspace-macs-startup-lists
              (space-macs-buffer/insert-startup-lists))
            (space-macs-buffer//insert-footer)
            (space-macs-buffer/set-mode-line space-macs--default-mode-line)
            (force-mode-line-update)
            (space-macs-buffer-mode)))
        (if save-line
            (progn (goto-char (point-min))
                   (forward-line (1- save-line))
                   (forward-to-indentation 0))
          (space-macs-buffer/goto-link-line)))
      (if current-prefix-arg
          (switch-to-buffer-other-window space-macs-buffer-name)
        (switch-to-buffer space-macs-buffer-name))
      (space-macs//redisplay))))

(add-hook 'window-setup-hook
          (lambda ()
            (add-hook 'window-configuration-change-hook
                      'space-macs-buffer//resize-on-hook)
            (space-macs-buffer//resize-on-hook)))

(defun space-macs-buffer//resize-on-hook ()
  "Hook run on window resize events to redisplay the home buffer."
  ;; prevent space-macs buffer redisplay in the filetree window
  (unless (memq this-command '(neotree-find-project-root
                               neotree-show
                               neotree-toggle
                               space-macs/tree-macs-project-toggle
                               tree-macs
                               tree-macs-bookmark
                               tree-macs-find-file
                               tree-macs-select-window))
    (let ((home-buffer (get-buffer-window space-macs-buffer-name))
          (frame-win (frame-selected-window)))
      (when (and dotspace-macs-startup-buffer-responsive
                 home-buffer
                 (not (window-minibuffer-p frame-win)))
        (with-selected-window home-buffer
          (space-macs-buffer/goto-buffer))))))

(defun space-macs-buffer/refresh ()
  "Force recreation of the space-macs buffer."
  (interactive)
  (setq space-macs-buffer--last-width nil)
  (space-macs-buffer/goto-buffer t))

(defalias 'space-macs/home 'space-macs-buffer/refresh
  "Go to home Space-macs buffer")

(defun space-macs/home-delete-other-windows ()
  "Open home Space-macs buffer and delete other windows.
Useful for making the home buffer the only visible buffer in the frame."
  (interactive)
  (space-macs/home)
  (delete-other-windows))

(provide 'core-space-macs-buffer)

;;; core-space-macs-buffer ends here


