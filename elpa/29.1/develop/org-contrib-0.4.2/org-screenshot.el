;;; org-screenshot.el --- Take and manage screenshots in Org-mode files
;;
;; Copyright (C) 2009-2021 Free Software Foundation, Inc.
;;
;; Author: Max Mikhanosha <max@openchat.com>
;; Keywords: outlines, hypermedia, calendar, wp
;; Homepage: https://git.sr.ht/~bzg/org-contrib
;; Version: 8.0
;;
;; Released under the GNU General Public License version 3
;; see: https://www.gnu.org/licenses/gpl-3.0.html
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; NOTE: This library requires external screenshot taking executable "scrot",
;; which is available as a package from all major Linux distribution. If your
;; distribution does not have it, source can be found at:
;; 
;; http://freecode.com/projects/scrot
;;
;; org-screenshot.el have been tested with scrot version 0.8.
;; 
;; Usage:
;;
;;   (require 'org-screenshot)
;;
;;  Available commands with default bindings
;;
;;  `org-screenshot-take'              C-c M-s M-t  and   C-c M-s M-s
;;  
;;        Take the screenshot, C-u argument delays 1 second, double C-u 2 seconds
;;        triple C-u 3 seconds, and subsequent C-u add 2 seconds to the delay.
;;
;;        Screenshot area is selected with the mouse, or left-click on the window
;;        for an entire window.
;;        
;;  `org-screenshot-rotate-prev'       C-c M-s M-p   and C-c M-s C-p
;;  
;;        Rotate screenshot before the point to one before it (sorted by date)
;;        
;;  `org-screenshot-rotate-next'       C-c M-s M-n   and C-c M-s C-n
;;
;;        Rotate screenshot before the point to one after it
;;
;;  `org-screenshot-show-unused'       C-c M-s M-u   and C-c M-s u
;;
;;        Open dired buffer with screenshots that are not used in current
;;        Org buffer marked
;;
;; The screenshot take and rotate commands will update the inline images
;; if they are already shown, if you are inserting first screenshot in the Org
;; Buffer (and there are no other images shown), you need to manually display
;; inline images with C-c C-x C-v
;;
;; Screenshot take and rotate commands offer user to continue by by using single
;; keys, in a manner similar to to "repeat-char" of keyboard macros, user can
;; continue rotating screenshots by pressing just the last key of the binding
;;
;; For example: C-c M-s M-t creates the screenshot and then user can
;; repeatedly press M-p or M-n to rotate it back and forth with
;; previously taken ones.
;;

(require 'org)
(require 'dired)

(defgroup org-screenshot nil
  "Options for taking and managing screen-shots"
  :group 'org-link)

(defcustom org-screenshot-image-directory "./images/"
  "Directory in which screenshot image files will be stored, it
be automatically created if it doesn't already exist."
  :type 'string
  :group 'org-screenshot)

(defcustom org-screenshot-file-name-format "screenshot-%2.2d.png"
  "The string used to generate screenshot file name. 

Any %d format string recipe will be expanded with `format'
function with the argument of a screenshot sequence number.

A sequence like %XXXX will be replaced with string of the same
length as there are X's, consisting of random characters in the
range of [A-Za-z]."
  :type 'string
  :group 'org-screenshot)

(defcustom org-screenshot-max-tries 200
  "Number of times we will try to generate generate filename that
does not exist. With default `org-screenshot-name-format' its the
limit for number of screenshots, before `org-screenshot-take' is
unable to come up with a unique name."
  :type 'integer
  :group 'org-screenshot)

(defvar org-screenshot-map (make-sparse-keymap)
  "Map for OrgMode screenshot related commands")

;; prefix
(org-defkey org-mode-map (kbd "C-c M-s") org-screenshot-map)

;; Mnemonic is Control-C Meta "Screenshot" "Take"
(org-defkey org-screenshot-map (kbd "M-t") 'org-screenshot-take)
(org-defkey org-screenshot-map (kbd "M-s") 'org-screenshot-take)

;; No reason to require meta key, since its our own keymap
(org-defkey org-screenshot-map "s" 'org-screenshot-take)
(org-defkey org-screenshot-map "t" 'org-screenshot-take)

;; Rotations, the fast rotation user hint, would prefer the modifier
;; used by the original command that started the rotation
(org-defkey org-screenshot-map (kbd "M-n") 'org-screenshot-rotate-next)
(org-defkey org-screenshot-map (kbd "M-p") 'org-screenshot-rotate-prev)
(org-defkey org-screenshot-map (kbd "C-n") 'org-screenshot-rotate-next)
(org-defkey org-screenshot-map (kbd "C-p") 'org-screenshot-rotate-prev)

;; Show unused image files in Dired
(org-defkey org-screenshot-map (kbd "M-u") 'org-screenshot-show-unused)
(org-defkey org-screenshot-map (kbd "u") 'org-screenshot-show-unused)


(random t)

(defun org-screenshot-random-string (length)
  "Generate a random string of LENGTH consisting of random upper
case and lower case letters."
  (let ((name (make-string length ?x)))
    (dotimes (i length)
      (let ((n (random 52)))
        (aset name i (if (< n 26)
                         (+ ?a n)
                       (+ ?A n -26))))) 
    name))

(defvar org-screenshot-process nil
  "Currently running screenshot process")

(defvar org-screenshot-directory-seq-numbers (make-hash-table :test 'equal))

(defun org-screenshot-update-seq-number (directory &optional reset)
  "Set `org-screenshot-file-name-format' sequence number for the directory.
When RESET is NIL, increments the number stored, otherwise sets
RESET as a new number. Intended to be called if screenshot was
successful.  Updating of sequence number is done in two steps, so
aborted/canceled screenshot attempts don't increase the number"

  (setq directory (file-name-as-directory directory))
  (puthash directory (if reset
                         (if (numberp reset) reset 1)
                       (1+ (gethash directory
                                    org-screenshot-directory-seq-numbers
                                    0)))
           org-screenshot-directory-seq-numbers))

(defun org-screenshot-generate-file-name (directory)
  "Use `org-screenshot-name-format' to generate new screenshot
file name for a specific directory. Keeps re-generating name if
it already exists, up to `org-screenshot-max-tries'
times. Returns just the file, without directory part"
  (setq directory (file-name-as-directory directory))
  (when (file-exists-p directory)
    (let ((tries 0)
          name
          had-seq
          (case-fold-search nil))
      (while (and (< tries org-screenshot-max-tries)
                  (not name))
        (cl-incf tries)
        (let ((tmp org-screenshot-file-name-format)
              (seq-re "%[-0-9.]*d")
              (rand-re "%X+"))
          (when (string-match seq-re tmp)
            (let ((seq (gethash
                        directory
                        org-screenshot-directory-seq-numbers 1))) 
              (setq tmp 
                    (replace-regexp-in-string
                     seq-re (format (match-string 0 tmp) seq)
                     tmp)
                    had-seq t)))
          (when (string-match rand-re tmp)
            (setq tmp
                  (replace-regexp-in-string
                   rand-re (org-screenshot-random-string
                            (1- (length (match-string 0 tmp))))
                   tmp t)))
          (let ((fullname (concat directory tmp))) 
            (if (file-exists-p fullname)
                (when had-seq (org-screenshot-update-seq-number directory))
              (setq name tmp)))))
      name)))

(defun org-screenshot-image-directory ()
  "Return the `org-screenshot-image-directory', ensuring there is
trailing slash, and that it exists"
  (let ((dir (file-name-as-directory org-screenshot-image-directory)))
    (if (file-exists-p dir)
        dir
      (make-directory dir t)
      dir)))

(defvar org-screenshot-last-file nil
  "File name of the last taken or rotated screenshot file,
without directory")

(defun org-screenshot-process-done (process event file
                                            orig-buffer
                                            orig-delay
                                            orig-event)
  "Called when \"scrot\" process exits. PROCESS and EVENT are
same arguments as in `set-process-sentinel'.  ORIG-BUFFER,
ORIG-DELAY and ORIG-EVENT are Org Buffer, the screenshot delay
used, and LAST-INPUT-EVENT values from when screenshot was
initiated.
"
  (setq org-screenshot-process nil)
  (with-current-buffer (process-buffer process) 
    (if (not (equal event "finished\n"))
        (progn 
          (insert event) 
          (cond ((save-excursion
                   (goto-char (point-min))
                   (re-search-forward "Key was pressed" nil t))
                 (ding)
                 (message "Key was pressed, screenshot aborted"))
                (t 
                 (display-buffer (process-buffer process))
                 (message "Error running \"scrot\" program")
                 (ding))))
      (with-current-buffer orig-buffer 
        (let ((link (format "[[file:%s]]" file))) 
          (setq org-screenshot-last-file (file-name-nondirectory file))
          (let ((beg (point)))
            (insert link) 
            (when org-inline-image-overlays
              (org-display-inline-images nil t beg (point))))
          (unless (< orig-delay 3)
            (ding))
          (org-screenshot-rotate-continue t orig-event))))))


;;;###autoload
(defun org-screenshot-take (&optional delay)
  "Take a screenshot and insert link to it at point, if image
display is already on (see \\[org-toggle-inline-images])
screenshot will be displayed as an image

Screen area for the screenshot is selected with the mouse, left
click on a window screenshots that window, while left click and
drag selects a region. Pressing any key cancels the screen shot

With `C-u' universal argument waits one second after target is
selected before taking the screenshot. With double `C-u' wait two
seconds.

With triple `C-u' wait 3 seconds, and also rings the bell when
screenshot is done, any more `C-u' after that increases delay by
2 seconds
"
  (interactive "P")

  ;; probably easier way to count number of C-u C-u out there
  (setq delay
        (cond ((null delay) 0)
              ((integerp delay) delay)
              ((and (consp delay)
                    (integerp (car delay))
                    (cl-plusp (car delay)))
               (let ((num 1)
                     (limit (car delay))
                     (cnt 0))
                 (while (< num limit)
                   (setq num (* num 4)
                         cnt (+ cnt (if (< cnt 3) 1 2))))
                 cnt))
              (t (error "Invalid delay"))))
  (when (and org-screenshot-process
             (member (process-status org-screenshot-process)
                     '(run stop)))
    (error "scrot process is still running"))
  (let* ((name (org-screenshot-generate-file-name (org-screenshot-image-directory)))
         (file (format "%s%s" (org-screenshot-image-directory)
                       name))
         (path (expand-file-name file)))
    (when (get-buffer "*scrot*")
      (with-current-buffer (get-buffer "*scrot*")
        (erase-buffer)))
    (setq org-screenshot-process
          (or 
           (apply 'start-process
                  (append
                   (list "scrot" "*scrot*" "scrot" "-s" path)
                   (when (cl-plusp delay)
                     (list "-d" (format "%d" delay)))))
           (error "Unable to start scrot process")))
    (when org-screenshot-process 
      (if (cl-plusp delay) 
          (message "Click on a window, or select a rectangle (delay is %d sec)..."
                   delay)
        (message "Click on a window, or select a rectangle..."))
      (set-process-sentinel
       org-screenshot-process
       `(lambda (process event)
          (org-screenshot-process-done
           process event ,file ,(current-buffer) ,delay ',last-input-event))))))

(defvar org-screenshot-file-list nil
  "List of files in `org-screenshot-image-directory' used by
`org-screenshot-rotate-prev' and `org-screenshot-rotate-next'")

(defvar org-screenshot-rotation-index -1)

(make-variable-buffer-local 'org-screenshot-file-list)
(make-variable-buffer-local 'org-screenshot-rotation-index)

(defun org-screenshot-rotation-init (lastfile)
  "Initialize variable `org-screenshot-file-list' variable with
the list of PNG files in `org-screenshot-image-directory' sorted
by most recent first"
  (setq
   org-screenshot-rotation-index -1
   org-screenshot-file-list
   (let ((files (directory-files org-screenshot-image-directory
                                 t (image-file-name-regexp) t)))
     (mapcar 'file-name-nondirectory
             (sort files
                   (lambda (file1 file2)
                     (let ((mtime1 (nth 5 (file-attributes file1)))
                           (mtime2 (nth 5 (file-attributes file2))))
                       (setq mtime1 (+ (ash (first mtime1) 16)
                                       (second mtime1)))
                       (setq mtime2 (+ (ash (first mtime2) 16)
                                       (second mtime2)))
                       (> mtime1 mtime2)))))))
  (let ((n -1) (list org-screenshot-file-list))
    (while (and list (not (equal (pop list) lastfile)))
      (cl-incf n))
    (setq org-screenshot-rotation-index n)))

(defun org-screenshot-do-rotate (dir from-continue-rotating)
  "Rotate last screenshot with one of the previously taken
screenshots from the same directory. If DIR is negative, in the
other direction"
  (setq org-screenshot-last-file nil)
  (let* ((ourdir (file-name-as-directory (org-screenshot-image-directory)))
         done
         (link-re 
          ;; taken from `org-display-inline-images'
          (concat "\\[\\[\\(\\(file:\\)\\|\\([./~]\\)\\)\\([^]\n]+?"
                  (substring (image-file-name-regexp) 0 -2)
                  "\\)\\]"))
         newfile oldfile)
    (save-excursion 
      ;; Search for link to image file in the same directory before the point
      (while (not done)
        (if (not (re-search-backward link-re (point-min) t))
            (error "Unable to find link to image from %S directory before point" ourdir)
          (let ((file (concat (or (match-string 3) "") (match-string 4))))
            (when (equal (file-name-directory file)
                         ourdir)
              (setq done t
                    oldfile (file-name-nondirectory file))))))
      (when (or (null org-screenshot-file-list)
                (and (not from-continue-rotating) 
                     (not (member last-command
                                  '(org-screenshot-rotate-prev
                                    org-screenshot-rotate-next)))))
        (org-screenshot-rotation-init oldfile))
      (unless (> (length org-screenshot-file-list) 1)
        (error "Can't rotate a single image file"))
      (replace-match "" nil nil nil 1)

      (setq org-screenshot-rotation-index
            (mod (+ org-screenshot-rotation-index dir)
                 (length org-screenshot-file-list)) 
            newfile (nth org-screenshot-rotation-index
                         org-screenshot-file-list))
      ;; in case we started rotating from the file we just inserted,
      ;; advance one more time
      (when (equal oldfile newfile)
        (setq org-screenshot-rotation-index
              (mod (+ org-screenshot-rotation-index (if (cl-plusp dir) 1 -1))
                   (length org-screenshot-file-list))
              newfile (nth org-screenshot-rotation-index
                           org-screenshot-file-list)))
      (replace-match (concat "file:" ourdir
                             newfile)
                     t t nil 4))
    ;; out of save-excursion
    (setq org-screenshot-last-file newfile)
    (when org-inline-image-overlays
      (org-display-inline-images nil t (match-beginning 0) (point)))))

;;;###autoload
(defun org-screenshot-rotate-prev (dir)
  "Rotate last screenshot with one of the previously taken
screenshots from the same directory. If DIR is negative, rotate
in the other direction"
  (interactive "p")
  (org-screenshot-do-rotate dir nil)
  (when org-screenshot-last-file 
    (org-screenshot-rotate-continue nil nil)))

;;;###autoload
(defun org-screenshot-rotate-next (dir)
  "Rotate last screenshot with one of the previously taken
screenshots from the same directory. If DIR is negative, rotate
in the other direction"
  (interactive "p")
  (org-screenshot-do-rotate (- dir) nil)
  (when org-screenshot-last-file 
    (org-screenshot-rotate-continue nil nil)))

(defun org-screenshot-prefer-same-modifiers (list event)
  (if (not (eventp nil)) (car list) 
    (let (ret (keys list))
      (while (and (null ret) keys)
        (let ((key (car keys))) 
          (if (and (= 1 (length key)) 
                   (equal (event-modifiers event)
                          (event-modifiers (elt key 0))))
              (setq ret (car keys))
            (setq keys (cdr keys)))))
      (or ret (car list)))))

(defun org-screenshot-rotate-continue (from-take-screenshot orig-event)
  "Display the message with the name of the last changed
image-file and inform user that they can rotate by pressing keys
bound to `org-screenshot-rotate-next' and
`org-screenshot-rotate-prev' in `org-screenshot-map'

This works similarly to `kmacro-end-or-call-macro' so that user
can press a long key sequence to invoke the first command, and
then uses single keys to rotate, until unregognized key is
entered, at which point event will be unread"

  (let* ((event (if from-take-screenshot orig-event
                  last-input-event))
         done
         (prev-key
          (org-screenshot-prefer-same-modifiers
           (where-is-internal 'org-screenshot-rotate-prev
                              org-screenshot-map nil)
           event))
         (next-key
          (org-screenshot-prefer-same-modifiers
           (where-is-internal 'org-screenshot-rotate-next
                              org-screenshot-map nil)
           event))
         prev-key-str next-key-str)
    (when (and (= (length prev-key) 1)
               (= (length next-key) 1)) 
      (setq
       prev-key-str (format-kbd-macro prev-key nil)
       next-key-str (format-kbd-macro next-key nil)
       prev-key (elt prev-key 0)
       next-key (elt next-key 0))
      (while (not done)
        (message "%S - '%s' and '%s' to rotate"
                 org-screenshot-last-file prev-key-str next-key-str)
        (setq event (read-event))
        (cond ((equal event prev-key)
               (clear-this-command-keys t)
               (org-screenshot-do-rotate 1 t)
               (setq last-input-event nil))
              ((equal event next-key)
               (clear-this-command-keys t)
               (org-screenshot-do-rotate -1 t)
               (setq last-input-event nil))
              (t (setq done t)))) 
      (when last-input-event
        (clear-this-command-keys t)
        (setq unread-command-events (list last-input-event))))))

;;;###autoload
(defun org-screenshot-show-unused ()
  "Open A Dired buffer with unused screenshots marked"
  (interactive)
  (let ((files-in-buffer)
	dired-buffer
	had-any
	(image-re (image-file-name-regexp))
	beg end)
    (save-excursion
      (save-restriction
	(widen)
	(setq beg (or beg (point-min)) end (or end (point-max)))
	(goto-char beg)
	(let ((re (concat "\\[\\[\\(\\(file:\\)\\|\\([./~]\\)\\)\\([^]\n]+?"
			  (substring (image-file-name-regexp) 0 -2)
			  "\\)\\]"))
	      (case-fold-search t)
	      old file ov img type attrwidth width)
	  (while (re-search-forward re end t)
	    (setq file (concat (or (match-string 3) "") (match-string 4)))
	    (when (and (file-exists-p file)
		       (equal (file-name-directory file)
			      (org-screenshot-image-directory)))
	      (push (file-name-nondirectory file)
		    files-in-buffer))))))
    (setq dired-buffer (dired-noselect (org-screenshot-image-directory)))
    (with-current-buffer dired-buffer
      (dired-unmark-all-files ?\r)
      (dired-mark-if
       (let ((file (dired-get-filename 'no-dir t))) 
	 (and file (string-match image-re file)
	      (not (member file files-in-buffer))
	      (setq had-any t)))
       "Unused screenshot"))
    (when had-any (pop-to-buffer dired-buffer))))

(provide 'org-screenshot)
