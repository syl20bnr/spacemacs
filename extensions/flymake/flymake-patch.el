;;; flymake.el -- a universal on-the-fly syntax checker

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008, 2009
;;   Free Software Foundation, Inc.

;; Author:  Pavel Kobyakov <pk_at_work@yahoo.com>
;; Maintainer: Pavel Kobyakov <pk_at_work@yahoo.com>
;; Version: 0.3
;; Keywords: c languages tools

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Flymake is a minor Emacs mode performing on-the-fly syntax
;; checks using the external syntax check tool (for C/C++ this
;; is usually the compiler)

;;; Bugs/todo:

;; - Only uses "Makefile", not "makefile" or "GNUmakefile"
;;   (from http://bugs.debian.org/337339).

;; - Adding info messages
;;; Code:

(eval-when-compile (require 'cl))
(if (featurep 'xemacs) (require 'overlay))

(defvar flymake-is-running nil
  "If t, flymake syntax check process is running for the current buffer.")
(make-variable-buffer-local 'flymake-is-running)

(defvar flymake-timer nil
  "Timer for starting syntax check.")
(make-variable-buffer-local 'flymake-timer)

(defvar flymake-last-change-time nil
  "Time of last buffer change.")
(make-variable-buffer-local 'flymake-last-change-time)

(defvar flymake-check-start-time nil
  "Time at which syntax check was started.")
(make-variable-buffer-local 'flymake-check-start-time)

(defvar flymake-check-was-interrupted nil
  "Non-nil if syntax check was killed by `flymake-compile'.")
(make-variable-buffer-local 'flymake-check-was-interrupted)

(defvar flymake-err-info nil
  "Sorted list of line numbers and lists of err info in the form (file, err-text).")
(make-variable-buffer-local 'flymake-err-info)

(defvar flymake-new-err-info nil
  "Same as `flymake-err-info', effective when a syntax check is in progress.")
(make-variable-buffer-local 'flymake-new-err-info)

;;;; [[ cross-emacs compatibility routines
(defsubst flymake-makehash (&optional test)
  (if (fboundp 'make-hash-table)
      (if test (make-hash-table :test test) (make-hash-table))
    (with-no-warnings
      (makehash test))))

(defalias 'flymake-float-time
  (if (fboundp 'float-time)
      'float-time
    (if (featurep 'xemacs)
	(lambda ()
	  (multiple-value-bind (s0 s1 s2) (values-list (current-time))
	    (+ (* (float (ash 1 16)) s0) (float s1) (* 0.0000001 s2)))))))

(defalias 'flymake-replace-regexp-in-string
  (if (eval-when-compile (fboundp 'replace-regexp-in-string))
      'replace-regexp-in-string
    (lambda (regexp rep str)
      (replace-in-string str regexp rep))))

(defalias 'flymake-split-string
  (if (condition-case nil (equal (split-string " bc " " " t) '("bc"))
        (error nil))
      (lambda (str pattern) (split-string str pattern t))
    (lambda (str pattern)
      "Split STR into a list of substrings bounded by PATTERN.
Zero-length substrings at the beginning and end of the list are omitted."
      (let ((split (split-string str pattern)))
        (while (equal (car split) "") (setq split (cdr split)))
        (setq split (nreverse split))
        (while (equal (car split) "") (setq split (cdr split)))
        (nreverse split)))))

(defalias 'flymake-get-temp-dir
  (if (fboundp 'temp-directory)
      'temp-directory
    (lambda () temporary-file-directory)))

(defalias 'flymake-line-beginning-position
  (if (fboundp 'line-beginning-position)
      'line-beginning-position
    (lambda (&optional arg) (save-excursion (beginning-of-line arg) (point)))))

(defalias 'flymake-line-end-position
  (if (fboundp 'line-end-position)
      'line-end-position
    (lambda (&optional arg) (save-excursion (end-of-line arg) (point)))))


;;; Personalized functions to be used in general
(defun string-match-multi (reglist str)
  "Matches STR with each of the regex in REGLIST, return the results of string-match"
  (mapcar 
   (lambda (reg) (string-match reg str)) 
   reglist)
  )

(defun one-true (mylist)
  "Return t if one element of the list MYLIST return t, else it return nil"
  (let ((mylist mylist) (ret nil))
    (while mylist
      (if (car mylist)
	  (setq ret t))
      (setq mylist (cdr mylist)))
    ret
  ))


(defun flymake-posn-at-point-as-event (&optional position window dx dy)
  "Return pixel position of top left corner of glyph at POSITION,
relative to top left corner of WINDOW, as a mouse-1 click
event (identical to the event that would be triggered by clicking
mouse button 1 at the top left corner of the glyph).

POSITION and WINDOW default to the position of point in the
selected window.

DX and DY specify optional offsets from the top left of the glyph."
  (unless window (setq window (selected-window)))
  (unless position (setq position (window-point window)))
  (unless dx (setq dx 0))
  (unless dy (setq dy 0))

  (let* ((pos (posn-at-point position window))
         (x-y (posn-x-y pos))
         (edges (window-inside-pixel-edges window))
         (win-x-y (window-pixel-edges window)))
    ;; adjust for window edges
    (setcar (nthcdr 2 pos)
            (cons (+ (car x-y) (car  edges) (- (car win-x-y))  dx)
                  (+ (cdr x-y) (cadr edges) (- (cadr win-x-y)) dy)))
    (list 'mouse-1 pos)))

(defun flymake-popup-menu (menu-data)
  "Pop up the flymake menu at point, using the data MENU-DATA.
POS is a list of the form ((X Y) WINDOW), where X and Y are
pixels positions from the top left corner of WINDOW's frame.
MENU-DATA is a list of error and warning messages returned by
`flymake-make-err-menu-data'."
  (if (featurep 'xemacs)
      (let* ((pos         (flymake-get-point-pixel-pos))
             (x-pos       (nth 0 pos))
	     (y-pos       (nth 1 pos))
	     (fake-event-props  '(button 1 x 1 y 1)))
	(setq fake-event-props (plist-put fake-event-props 'x x-pos))
	(setq fake-event-props (plist-put fake-event-props 'y y-pos))
	(popup-menu (flymake-make-xemacs-menu menu-data)
                    (make-event 'button-press fake-event-props)))
    (x-popup-menu (if (eval-when-compile (fboundp 'posn-at-point))
                      (flymake-posn-at-point-as-event)
                    (list (flymake-get-point-pixel-pos) (selected-window)))
                  (flymake-make-emacs-menu menu-data))))

(defun flymake-make-emacs-menu (menu-data)
  "Return a menu specifier using MENU-DATA.
MENU-DATA is a list of error and warning messages returned by
`flymake-make-err-menu-data'.
See `x-popup-menu' for the menu specifier format."
  (let* ((menu-title     (nth 0 menu-data))
	 (menu-items     (nth 1 menu-data))
	 (menu-commands  (mapcar (lambda (foo)
                                   (cons (nth 0 foo) (nth 1 foo)))
                                 menu-items)))
    (list menu-title (cons "" menu-commands))))

(if (featurep 'xemacs) (progn

(defun flymake-nop ())

(defun flymake-make-xemacs-menu (menu-data)
  "Return a menu specifier using MENU-DATA."
  (let* ((menu-title     (nth 0 menu-data))
	 (menu-items     (nth 1 menu-data))
	 (menu-commands  nil))
    (setq menu-commands (mapcar (lambda (foo)
				  (vector (nth 0 foo) (or (nth 1 foo) '(flymake-nop)) t))
				menu-items))
    (cons menu-title menu-commands)))

)) ;; xemacs

(unless (eval-when-compile (fboundp 'posn-at-point))

(defun flymake-current-row ()
  "Return current row number in current frame."
  (if (fboundp 'window-edges)
      (+ (car (cdr (window-edges))) (count-lines (window-start) (point)))
    (count-lines (window-start) (point))))

(defun flymake-selected-frame ()
  (if (fboundp 'window-edges)
      (selected-frame)
    (selected-window)))

(defun flymake-get-point-pixel-pos ()
  "Return point position in pixels: (x, y)."
  (let ((mouse-pos  (mouse-position))
	(pixel-pos  nil)
	(ret        nil))
    (if (car (cdr mouse-pos))
	(progn
	  (set-mouse-position (flymake-selected-frame) (current-column) (flymake-current-row))
	  (setq pixel-pos (mouse-pixel-position))
	  (set-mouse-position (car mouse-pos) (car (cdr mouse-pos)) (cdr (cdr mouse-pos)))
	  (setq ret (list (car (cdr pixel-pos)) (cdr (cdr pixel-pos)))))
      (progn
	(setq ret '(0 0))))
    (flymake-log 3 "mouse pos is %s" ret)
    ret))

) ;; End of (unless (fboundp 'posn-at-point)

;;;; ]]

(defcustom flymake-log-level -1
  "Logging level, only messages with level lower or equal will be logged.
-1 = NONE, 0 = ERROR, 1 = WARNING, 2 = INFO, 3 = DEBUG"
  :group 'flymake
  :type 'integer)

(defun flymake-log (level text &rest args)
  "Log a message at level LEVEL.
If LEVEL is higher than `flymake-log-level', the message is
ignored.  Otherwise, it is printed using `message'.
TEXT is a format control string, and the remaining arguments ARGS
are the string substitutions (see `format')."
  (if (<= level flymake-log-level)
      (let* ((msg (apply 'format text args)))
	(message "%s" msg)
	;;XXX
	(with-temp-buffer
	  (insert msg)
	  (insert "\n"))
	;;   (flymake-save-buffer-in-file "d:/flymake.log" t)  ; make log file name customizable
	;;)
	)))

(defun flymake-ins-after (list pos val)
  "Insert VAL into LIST after position POS."
  (let ((tmp (copy-sequence list)))	; (???)
    (setcdr (nthcdr pos tmp) (cons val (nthcdr (1+ pos) tmp)))
    tmp))

(defun flymake-set-at (list pos val)
  "Set VAL at position POS in LIST."
  (let ((tmp (copy-sequence list)))	; (???)
    (setcar (nthcdr pos tmp) val)
    tmp))

(defvar flymake-processes nil
  "List of currently active flymake processes.")

(defvar flymake-output-residual nil)

(make-variable-buffer-local 'flymake-output-residual)

(defgroup flymake nil
  "A universal on-the-fly syntax checker."
  :version "23.1"
  :group 'tools)

(defcustom flymake-allowed-file-name-masks
  '(("\\.c\\'" flymake-simple-make-init)
    ("\\.cpp\\'" flymake-simple-make-init)
    ("\\.xml\\'" flymake-xml-init)
    ("\\.html?\\'" flymake-xml-init)
    ("\\.cs\\'" flymake-simple-make-init)
    ("\\.p[ml]\\'" flymake-perl-init)
    ("\\.php[345]?\\'" flymake-php-init)
    ("\\.h\\'" flymake-master-make-header-init flymake-master-cleanup)
    ("\\.java\\'" flymake-simple-make-java-init flymake-simple-java-cleanup)
    ("[0-9]+\\.tex\\'" flymake-master-tex-init flymake-master-cleanup)
    ("\\.tex\\'" flymake-simple-tex-init)
    ("\\.idl\\'" flymake-simple-make-init)
    ;; ("\\.cpp\\'" 1)
    ;; ("\\.java\\'" 3)
    ;; ("\\.h\\'" 2 ("\\.cpp\\'" "\\.c\\'")
    ;; ("[ \t]*#[ \t]*include[ \t]*\"\\([\w0-9/\\_\.]*[/\\]*\\)\\(%s\\)\"" 1 2))
    ;; ("\\.idl\\'" 1)
    ;; ("\\.odl\\'" 1)
    ;; ("[0-9]+\\.tex\\'" 2 ("\\.tex\\'")
    ;; ("[ \t]*\\input[ \t]*{\\(.*\\)\\(%s\\)}" 1 2 ))
    ;; ("\\.tex\\'" 1)
    )
  "*Files syntax checking is allowed for."
  :group 'flymake
  :type '(repeat (string symbol symbol symbol)))

(defun flymake-get-file-name-mode-and-masks (file-name)
  "Return the corresponding entry from `flymake-allowed-file-name-masks'."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (let ((fnm flymake-allowed-file-name-masks)
	(mode-and-masks  nil))
    (while (and (not mode-and-masks) fnm)
      (if (string-match (car (car fnm)) file-name)
	  (setq mode-and-masks (cdr (car fnm))))
      (setq fnm (cdr fnm)))
    (flymake-log 3 "file %s, init=%s" file-name (car mode-and-masks))
    mode-and-masks))

(defun flymake-can-syntax-check-file (file-name)
  "Determine whether we can syntax check FILE-NAME.
Return nil if we cannot, non-nil if we can."
  (if (flymake-get-init-function file-name) t nil))

(defun flymake-get-init-function (file-name)
  "Return init function to be used for the file."
  (let* ((init-f  (nth 0 (flymake-get-file-name-mode-and-masks file-name))))
    ;;(flymake-log 0 "calling %s" init-f)
    ;;(funcall init-f (current-buffer))
    init-f))

(defun flymake-get-cleanup-function (file-name)
  "Return cleanup function to be used for the file."
  (or (nth 1 (flymake-get-file-name-mode-and-masks file-name))
      'flymake-simple-cleanup))

(defun flymake-get-real-file-name-function (file-name)
  (or (nth 2 (flymake-get-file-name-mode-and-masks file-name))
      'flymake-get-real-file-name))

(defvar flymake-find-buildfile-cache (flymake-makehash 'equal))

(defun flymake-get-buildfile-from-cache (dir-name)
  (gethash dir-name flymake-find-buildfile-cache))

(defun flymake-add-buildfile-to-cache (dir-name buildfile)
  (puthash dir-name buildfile flymake-find-buildfile-cache))

(defun flymake-clear-buildfile-cache ()
  (clrhash flymake-find-buildfile-cache))

(defun flymake-find-buildfile (buildfile-name source-dir-name)
  "Find buildfile starting from current directory.
Buildfile includes Makefile, build.xml etc.
Return its file name if found, or nil if not found."
  (or (flymake-get-buildfile-from-cache source-dir-name)
      (let* ((file (locate-dominating-file source-dir-name buildfile-name)))
        (if file
            (progn
              (flymake-log 3 "found buildfile at %s" file)
              (flymake-add-buildfile-to-cache source-dir-name file)
              file)
          (progn
            (flymake-log 3 "buildfile for %s not found" source-dir-name)
            nil)))))

(defun flymake-fix-file-name (name)
  "Replace all occurrences of '\' with '/'."
  (when name
    (setq name (expand-file-name name))
    (setq name (abbreviate-file-name name))
    (setq name (directory-file-name name))
    name))

(defun flymake-same-files (file-name-one file-name-two)
  "Check if FILE-NAME-ONE and FILE-NAME-TWO point to same file.
Return t if so, nil if not."
  (equal (flymake-fix-file-name file-name-one)
	 (flymake-fix-file-name file-name-two)))

(defcustom flymake-master-file-dirs '("." "./src" "./UnitTest")
  "Dirs where to look for master files."
  :group 'flymake
  :type '(repeat (string)))

(defcustom flymake-master-file-count-limit 32
  "Max number of master files to check."
  :group 'flymake
  :type 'integer)

;; This is bound dynamically to pass a parameter to a sort predicate below
(defvar flymake-included-file-name)

(defun flymake-find-possible-master-files (file-name master-file-dirs masks)
  "Find (by name and location) all possible master files.
Master files are .cpp and .c for and .h.  Files are searched for
starting from the .h directory and max max-level parent dirs.
File contents are not checked."
  (let* ((dirs master-file-dirs)
	 (files  nil)
	 (done   nil))

    (while (and (not done) dirs)
      (let* ((dir (expand-file-name (car dirs) (file-name-directory file-name)))
	     (masks masks))
	(while (and (file-exists-p dir) (not done) masks)
	  (let* ((mask        (car masks))
		 (dir-files   (directory-files dir t mask)))

	    (flymake-log 3 "dir %s, %d file(s) for mask %s"
			 dir (length dir-files) mask)
	    (while (and (not done) dir-files)
	      (when (not (file-directory-p (car dir-files)))
		(setq files (cons (car dir-files) files))
		(when (>= (length files) flymake-master-file-count-limit)
		  (flymake-log 3 "master file count limit (%d) reached" flymake-master-file-count-limit)
		  (setq done t)))
	      (setq dir-files (cdr dir-files))))
	  (setq masks (cdr masks))))
      (setq dirs (cdr dirs)))
    (when files
      (let ((flymake-included-file-name (file-name-nondirectory file-name)))
	(setq files (sort files 'flymake-master-file-compare))))
    (flymake-log 3 "found %d possible master file(s)" (length files))
    files))

(defun flymake-master-file-compare (file-one file-two)
  "Compare two files specified by FILE-ONE and FILE-TWO.
This function is used in sort to move most possible file names
to the beginning of the list (File.h -> File.cpp moved to top)."
  (and (equal (file-name-sans-extension flymake-included-file-name)
	      (file-name-sans-extension (file-name-nondirectory file-one)))
       (not (equal file-one file-two))))

(defcustom flymake-check-file-limit 8192
  "Max number of chars to look at when checking possible master file."
  :group 'flymake
  :type 'integer)

(defun flymake-check-patch-master-file-buffer
       (master-file-temp-buffer
        master-file-name patched-master-file-name
        source-file-name patched-source-file-name
        include-dirs regexp)
  "Check if MASTER-FILE-NAME is a master file for SOURCE-FILE-NAME.
For .cpp master file this means it includes SOURCE-FILE-NAME (.h).
If yes, patch a copy of MASTER-FILE-NAME to include PATCHED-SOURCE-FILE-NAME
instead of SOURCE-FILE-NAME.
Whether a buffer for MATER-FILE-NAME exists, use it as a source
instead of reading master file from disk."
  (let* ((source-file-nondir (file-name-nondirectory source-file-name))
         (found                     nil)
	 (inc-name                  nil)
	 (search-limit              flymake-check-file-limit))
    (setq regexp
          (format regexp	; "[ \t]*#[ \t]*include[ \t]*\"\\(.*%s\\)\""
                  (regexp-quote source-file-nondir)))
    (unwind-protect
        (with-current-buffer master-file-temp-buffer
          (when (> search-limit (point-max))
            (setq search-limit (point-max)))
          (flymake-log 3 "checking %s against regexp %s"
                       master-file-name regexp)
          (goto-char (point-min))
          (while (and (< (point) search-limit)
                      (re-search-forward regexp search-limit t))
            (let ((match-beg   (match-beginning 1))
                  (match-end   (match-end 1)))

              (flymake-log 3 "found possible match for %s" source-file-nondir)
              (setq inc-name (match-string 1))
              (when (eq t (compare-strings
                           source-file-nondir nil nil
                           inc-name (- (length inc-name)
                                       (length source-file-nondir)) nil))
                (flymake-log 3 "inc-name=%s" inc-name)
                (when (flymake-check-include source-file-name inc-name
                                             include-dirs)
                  (setq found t)
                  ;;  replace-match is not used here as it fails in
                  ;; XEmacs with 'last match not a buffer' error as
                  ;; check-includes calls replace-in-string
                  (flymake-replace-region
                   match-beg match-end
                   (file-name-nondirectory patched-source-file-name))))
              (forward-line 1)))
          (when found
            (flymake-save-buffer-in-file patched-master-file-name)))
      ;;+(flymake-log 3 "killing buffer %s"
      ;;                (buffer-name master-file-temp-buffer))
      (kill-buffer master-file-temp-buffer))
    ;;+(flymake-log 3 "check-patch master file %s: %s" master-file-name found)
    (when found
      (flymake-log 2 "found master file %s" master-file-name))
    found))

(defun flymake-replace-region (beg end rep)
  "Replace text in BUFFER in region (BEG END) with REP."
  (save-excursion
    (goto-char end)
    ;; Insert before deleting, so as to better preserve markers's positions.
    (insert rep)
    (delete-region beg end)))

(defun flymake-read-file-to-temp-buffer (file-name)
  "Insert contents of FILE-NAME into newly created temp buffer."
  (let* ((temp-buffer (get-buffer-create (generate-new-buffer-name (concat "flymake:" (file-name-nondirectory file-name))))))
    (with-current-buffer temp-buffer
      (insert-file-contents file-name))
    temp-buffer))

(defun flymake-copy-buffer-to-temp-buffer (buffer)
  "Copy contents of BUFFER into newly created temp buffer."
  (with-current-buffer
      (get-buffer-create (generate-new-buffer-name
                          (concat "flymake:" (buffer-name buffer))))
    (insert-buffer-substring buffer)
    (current-buffer)))

(defun flymake-check-include (source-file-name inc-name include-dirs)
  "Check if SOURCE-FILE-NAME can be found in include path.
Return t if it can be found via include path using INC-NAME."
  (if (file-name-absolute-p inc-name)
      (flymake-same-files source-file-name inc-name)
    (while (and include-dirs
                (not (flymake-same-files
                      source-file-name
                      (concat (file-name-directory source-file-name)
                              "/" (car include-dirs)
                              "/" inc-name))))
      (setq include-dirs (cdr include-dirs)))
    include-dirs))

(defun flymake-find-buffer-for-file (file-name)
  "Check if there exists a buffer visiting FILE-NAME.
Return t if so, nil if not."
  (let ((buffer-name (get-file-buffer file-name)))
    (if buffer-name
	(get-buffer buffer-name))))

(defun flymake-create-master-file (source-file-name patched-source-file-name get-incl-dirs-f create-temp-f masks include-regexp)
  "Save SOURCE-FILE-NAME with a different name.
Find master file, patch and save it."
  (let* ((possible-master-files     (flymake-find-possible-master-files source-file-name flymake-master-file-dirs masks))
	 (master-file-count         (length possible-master-files))
	 (idx                       0)
	 (temp-buffer               nil)
	 (master-file-name          nil)
	 (patched-master-file-name  nil)
	 (found                     nil))

    (while (and (not found) (< idx master-file-count))
      (setq master-file-name (nth idx possible-master-files))
      (setq patched-master-file-name (funcall create-temp-f master-file-name "flymake_master"))
      (if (flymake-find-buffer-for-file master-file-name)
	  (setq temp-buffer (flymake-copy-buffer-to-temp-buffer (flymake-find-buffer-for-file master-file-name)))
	(setq temp-buffer (flymake-read-file-to-temp-buffer master-file-name)))
      (setq found
	    (flymake-check-patch-master-file-buffer
	     temp-buffer
	     master-file-name
	     patched-master-file-name
	     source-file-name
	     patched-source-file-name
	     (funcall get-incl-dirs-f (file-name-directory master-file-name))
	     include-regexp))
      (setq idx (1+ idx)))
    (if found
	(list master-file-name patched-master-file-name)
      (progn
	(flymake-log 3 "none of %d master file(s) checked includes %s" master-file-count
		     (file-name-nondirectory source-file-name))
	nil))))

(defun flymake-save-buffer-in-file (file-name)
  (make-directory (file-name-directory file-name) 1)
  (write-region nil nil file-name nil 566)
  (flymake-log 3 "saved buffer %s in file %s" (buffer-name) file-name))

(defun flymake-save-string-to-file (file-name data)
  "Save string DATA to file FILE-NAME."
  (write-region data nil file-name nil 566))

(defun flymake-read-file-to-string (file-name)
  "Read contents of file FILE-NAME and return as a string."
  (with-temp-buffer
    (insert-file-contents file-name)
    (buffer-substring (point-min) (point-max))))

(defun flymake-process-filter (process output)
  "Parse OUTPUT and highlight error lines.
It's flymake process filter."
  (let ((source-buffer (process-buffer process)))

    (flymake-log 3 "received %d byte(s) of output from process %d"
                 (length output) (process-id process))
    (when (buffer-live-p source-buffer)
      (with-current-buffer source-buffer
        (flymake-parse-output-and-residual output)))))

(defun flymake-process-sentinel (process event)
  "Sentinel for syntax check buffers."
  (when (memq (process-status process) '(signal exit))
    (let* ((exit-status       (process-exit-status process))
           (command           (process-command process))
           (source-buffer     (process-buffer process))
           (cleanup-f         (flymake-get-cleanup-function (buffer-file-name source-buffer))))

      (flymake-log 2 "process %d exited with code %d"
                   (process-id process) exit-status)
      (condition-case err
          (progn
            (flymake-log 3 "cleaning up using %s" cleanup-f)
            (when (buffer-live-p source-buffer)
              (with-current-buffer source-buffer
                (funcall cleanup-f)))

            (delete-process process)
            (setq flymake-processes (delq process flymake-processes))

            (when (buffer-live-p source-buffer)
              (with-current-buffer source-buffer

                (flymake-parse-residual)
                (flymake-post-syntax-check exit-status command)
                (setq flymake-is-running nil))))
        (error
         (let ((err-str (format "Error in process sentinel for buffer %s: %s"
                                source-buffer (error-message-string err))))
           (flymake-log 0 err-str)
           (with-current-buffer source-buffer
             (setq flymake-is-running nil))))))))

(defun flymake-post-syntax-check (exit-status command)
  (setq flymake-err-info flymake-new-err-info)
  (setq flymake-new-err-info nil)
  (setq flymake-err-info
        (flymake-fix-line-numbers
         flymake-err-info 1 (flymake-count-lines)))
  (flymake-delete-own-overlays)
  (flymake-highlight-err-lines flymake-err-info)
  (let (err-count warn-count info-count)
    (setq err-count (flymake-get-err-count flymake-err-info "e"))
    (setq warn-count  (flymake-get-err-count flymake-err-info "w"))
    (setq info-count  (flymake-get-err-count flymake-err-info "i"))
    
    (flymake-log 2 "%s: %d error(s), %d warning(s), %d info in %.2f second(s)"
		 (buffer-name) err-count warn-count info-count
		 (- (flymake-float-time) flymake-check-start-time))
    (setq flymake-check-start-time nil)

    (if (and (equal 0 err-count) (equal 0 warn-count) (equal 0 info-count))
	(if (equal 0 exit-status)
	    (flymake-report-status "" "")	; PASSED
	  (if (not flymake-check-was-interrupted)
	      (flymake-report-fatal-status "CFGERR"
					   (format "Configuration error has occured while running %s" command))
	    (flymake-report-status nil ""))) ; "STOPPED"
      (flymake-report-status (format "%d/%d/%d" err-count warn-count info-count) ""))))

(defun flymake-parse-output-and-residual (output)
  "Split OUTPUT into lines, merge in residual if necessary."
  (let* ((buffer-residual     flymake-output-residual)
         (total-output        (if buffer-residual (concat buffer-residual output) output))
         (lines-and-residual  (flymake-split-output total-output))
         (lines               (nth 0 lines-and-residual))
         (new-residual        (nth 1 lines-and-residual)))
    (setq flymake-output-residual new-residual)
    (setq flymake-new-err-info
          (flymake-parse-err-lines
           flymake-new-err-info lines))))

(defun flymake-parse-residual ()
  "Parse residual if it's non empty."
  (when flymake-output-residual
    (setq flymake-new-err-info
          (flymake-parse-err-lines
           flymake-new-err-info
           (list flymake-output-residual)))
    (setq flymake-output-residual nil)))

(defun flymake-er-make-er (line-no line-err-info-list)
  (list line-no line-err-info-list))

(defun flymake-er-get-line (err-info)
  (nth 0 err-info))

(defun flymake-er-get-line-err-info-list (err-info)
  (nth 1 err-info))

(defstruct (flymake-ler
            (:constructor nil)
            (:constructor flymake-ler-make-ler (file line type text &optional full-file)))
  file line type text full-file)

(defun flymake-ler-set-file (line-err-info file)
  (flymake-ler-make-ler file
			(flymake-ler-line line-err-info)
			(flymake-ler-type line-err-info)
			(flymake-ler-text line-err-info)
			(flymake-ler-full-file line-err-info)))

(defun flymake-ler-set-full-file (line-err-info full-file)
  (flymake-ler-make-ler (flymake-ler-file line-err-info)
			(flymake-ler-line line-err-info)
			(flymake-ler-type line-err-info)
			(flymake-ler-text line-err-info)
			full-file))

(defun flymake-ler-set-line (line-err-info line)
  (flymake-ler-make-ler (flymake-ler-file line-err-info)
			line
			(flymake-ler-type line-err-info)
			(flymake-ler-text line-err-info)
			(flymake-ler-full-file line-err-info)))

(defun flymake-get-line-err-count (line-err-info-list type)
  "Return number of errors of specified TYPE.
Value of TYPE is \"e\", \"w\" or \"i\"."
  (let* ((idx        0)
	 (count      (length line-err-info-list))
	 (err-count  0))

    (while (< idx count)
      (when (equal type (flymake-ler-type (nth idx line-err-info-list)))
	(setq err-count (1+ err-count)))
      (setq idx (1+ idx)))
    err-count))

(defun flymake-get-err-count (err-info-list type)
  "Return number of errors of specified TYPE for ERR-INFO-LIST."
  (let* ((idx        0)
	 (count      (length err-info-list))
	 (err-count  0))
    (while (< idx count)
      (setq err-count (+ err-count (flymake-get-line-err-count (nth 1 (nth idx err-info-list)) type)))
      (setq idx (1+ idx)))
    err-count))

(defun flymake-fix-line-numbers (err-info-list min-line max-line)
  "Replace line numbers with fixed value.
If line-numbers is less than MIN-LINE, set line numbers to MIN-LINE.
If line numbers is greater than MAX-LINE, set line numbers to MAX-LINE.
The reason for this fix is because some compilers might report
line number outside the file being compiled."
  (let* ((count     (length err-info-list))
	 (err-info  nil)
	 (line      0))
    (while (> count 0)
      (setq err-info (nth (1- count) err-info-list))
      (setq line (flymake-er-get-line err-info))
      (when (or (< line min-line) (> line max-line))
	(setq line (if (< line min-line) min-line max-line))
	(setq err-info-list (flymake-set-at err-info-list (1- count)
					    (flymake-er-make-er line
								(flymake-er-get-line-err-info-list err-info)))))
      (setq count (1- count))))
  err-info-list)

(defun flymake-highlight-err-lines (err-info-list)
  "Highlight error lines in BUFFER using info from ERR-INFO-LIST."
  (save-excursion
    (dolist (err err-info-list)
      (flymake-highlight-line (car err) (nth 1 err)))))

(defun flymake-overlay-p (ov)
  "Determine whether overlay OV was created by flymake."
  (and (overlayp ov) (overlay-get ov 'flymake-overlay)))

(defun flymake-make-overlay (beg end tooltip-text face mouse-face)
  "Allocate a flymake overlay in range BEG and END."
  (when (not (flymake-region-has-flymake-overlays beg end))
    (let ((ov (make-overlay beg end nil t t)))
      (overlay-put ov 'face           face)
      (overlay-put ov 'mouse-face     mouse-face)
      (overlay-put ov 'help-echo      tooltip-text)
      (overlay-put ov 'flymake-overlay  t)
      (overlay-put ov 'priority 100)
      ;;+(flymake-log 3 "created overlay %s" ov)
      ov)
    (flymake-log 3 "created an overlay at (%d-%d)" beg end)))

(defun flymake-delete-own-overlays ()
  "Delete all flymake overlays in BUFFER."
  (dolist (ol (overlays-in (point-min) (point-max)))
    (when (flymake-overlay-p ol)
      (delete-overlay ol)
      ;;+(flymake-log 3 "deleted overlay %s" ol)
      )))

(defun flymake-region-has-flymake-overlays (beg end)
  "Check if region specified by BEG and END has overlay.
Return t if it has at least one flymake overlay, nil if no overlay."
  (let ((ov                  (overlays-in beg end))
	(has-flymake-overlays  nil))
    (while (consp ov)
      (when (flymake-overlay-p (car ov))
	(setq has-flymake-overlays t))
      (setq ov (cdr ov)))
    has-flymake-overlays))

(defface flymake-errline
  '((((class color) (background dark)) (:background "Firebrick4"))
    (((class color) (background light)) (:background "LightPink"))
    (t (:bold t)))
  "Face used for marking error lines."
  :group 'flymake)

(defface flymake-warnline
  '((((class color) (background dark)) (:background "DarkBlue"))
    (((class color) (background light)) (:background "LightBlue2"))
    (t (:bold t)))
  "Face used for marking warning lines."
  :group 'flymake)

(defface flymake-infoline
  '((((class color) (background dark)) (:background "DarkGreen"))
    (((class color) (background light)) (:background "LightGreen"))
    (t (:bold t)))
  "Face used for marking warning lines."
  :group 'flymake)


(defun flymake-highlight-line (line-no line-err-info-list)
  "Highlight line LINE-NO in current buffer.
Perhaps use text from LINE-ERR-INFO-LIST to enhance highlighting."
  (goto-char (point-min))
  (forward-line (1- line-no))
  (let* ((line-beg (flymake-line-beginning-position))
	 (line-end (flymake-line-end-position))
	 (beg      line-beg)
	 (end      line-end)
	 (tooltip-text (flymake-ler-text (nth 0 line-err-info-list)))
	 (face     nil))

    (goto-char line-beg)
    (while (looking-at "[ \t]")
      (forward-char))

    (setq beg (point))

    (goto-char line-end)
    (while (and (looking-at "[ \t\r\n]") (> (point) 1))
      (backward-char))

    (setq end (1+ (point)))

    (when (<= end beg)
      (setq beg line-beg)
      (setq end line-end))

    (when (= end beg)
      (goto-char end)
      (forward-line)
      (setq end (point)))

    (if (> (flymake-get-line-err-count line-err-info-list "e") 0)
	(setq face 'flymake-errline)
      (if (> (flymake-get-line-err-count line-err-info-list "w") 0)
	  (setq face 'flymake-warnline)
	(setq face 'flymake-infoline)))

    (flymake-make-overlay beg end tooltip-text face nil)))

(defun flymake-parse-err-lines (err-info-list lines)
  "Parse err LINES, store info in ERR-INFO-LIST."
  (let* ((count              (length lines))
	 (idx                0)
	 (line-err-info      nil)
	 (real-file-name     nil)
	 (source-file-name   buffer-file-name)
	 (get-real-file-name-f (flymake-get-real-file-name-function source-file-name)))

    (while (< idx count)
      (setq line-err-info (flymake-parse-line (nth idx lines)))
      (when line-err-info
	(setq real-file-name (funcall get-real-file-name-f
                                      (flymake-ler-file line-err-info)))
	(setq line-err-info (flymake-ler-set-full-file line-err-info real-file-name))

	(when (flymake-same-files real-file-name source-file-name)
	  (setq line-err-info (flymake-ler-set-file line-err-info nil))
	  (setq err-info-list (flymake-add-err-info err-info-list line-err-info))))
      (flymake-log 3 "parsed '%s', %s line-err-info" (nth idx lines) (if line-err-info "got" "no"))
      (setq idx (1+ idx)))
    err-info-list))

(defun flymake-split-output (output)
  "Split OUTPUT into lines.
Return last one as residual if it does not end with newline char.
Returns ((LINES) RESIDUAL)."
  (when (and output (> (length output) 0))
    (let* ((lines (flymake-split-string output "[\n\r]+"))
	   (complete (equal "\n" (char-to-string (aref output (1- (length output))))))
	   (residual nil))
      (when (not complete)
	(setq residual (car (last lines)))
	(setq lines (butlast lines)))
      (list lines residual))))

(defun flymake-reformat-err-line-patterns-from-compile-el (original-list)
  "Grab error line patterns from ORIGINAL-LIST in compile.el format.
Convert it to flymake internal format."
  (let* ((converted-list '()))
    (dolist (item original-list)
      (setq item (cdr item))
      (let ((regexp (nth 0 item))
	    (file (nth 1 item))
	    (line (nth 2 item))
	    (col (nth 3 item)))
	(if (consp file)	(setq file (car file)))
	(if (consp line)	(setq line (car line)))
	(if (consp col)	(setq col (car col)))

	(when (not (functionp line))
	  (setq converted-list (cons (list regexp file line col) converted-list)))))
    converted-list))

(require 'compile)

(defvar flymake-err-line-patterns ; regexp file-idx line-idx col-idx (optional) text-idx(optional), match-end to end of string is error text
  (append
   '(
     ;; MS Visual C++ 6.0
     ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\)) \: \\(\\(error\\|warning\\|fatal error\\) \\(C[0-9]+\\):[ \t\n]*\\(.+\\)\\)"
      1 3 nil 4)
     ;; jikes
     ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)\:\\([0-9]+\\)\:[0-9]+\:[0-9]+\:[0-9]+\: \\(\\(Error\\|Warning\\|Caution\\|Semantic Error\\):[ \t\n]*\\(.+\\)\\)"
      1 3 nil 4)
     ;; MS midl
     ("midl[ ]*:[ ]*\\(command line error .*\\)"
      nil nil nil 1)
     ;; MS C#
     ("\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)(\\([0-9]+\\),[0-9]+)\: \\(\\(error\\|warning\\|fatal error\\) \\(CS[0-9]+\\):[ \t\n]*\\(.+\\)\\)"
      1 3 nil 4)
     ;; perl
     ("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 2 3 nil 1)
     ;; PHP
     ("\\(?:Parse\\|Fatal\\) error: \\(.*\\) in \\(.*\\) on line \\([0-9]+\\)" 2 3 nil 1)
     ;; LaTeX warnings (fileless) ("\\(LaTeX \\(Warning\\|Error\\): .*\\) on input line \\([0-9]+\\)" 20 3 nil 1)
     ;; ant/javac
     (" *\\(\\[javac\\] *\\)?\\(\\([a-zA-Z]:\\)?[^:(\t\n]+\\)\:\\([0-9]+\\)\:[ \t\n]*\\(.+\\)"
      2 4 nil 5))
   ;; compilation-error-regexp-alist)
   (flymake-reformat-err-line-patterns-from-compile-el compilation-error-regexp-alist-alist))
  "Patterns for matching error/warning lines.  Each pattern has the form
\(REGEXP FILE-IDX LINE-IDX COL-IDX ERR-TEXT-IDX).
Use `flymake-reformat-err-line-patterns-from-compile-el' to add patterns
from compile.el")


(defvar flymake-warn-line-regex
  '( "^[wW]arning" )
  "Patterns for recognizing if the line is a warning")

(defvar flymake-info-line-regex
  '( "^[iI]nfo" )
  "Patterns for recognizing if the line is an info message")

(defun flymake-parse-line (line)
  "Parse LINE to see if it is an error or warning.
Return its components if so, nil otherwise."
  (let ((raw-file-name nil)
	(line-no 0)
	(err-type "e")
	(err-text nil)
	(patterns flymake-err-line-patterns)
	(matched nil))
    (while (and patterns (not matched))
      (when (string-match (car (car patterns)) line)
	(let* ((file-idx (nth 1 (car patterns)))
	       (line-idx (nth 2 (car patterns))))

	  (setq raw-file-name (if file-idx (match-string file-idx line) nil))
	  (setq line-no       (if line-idx (string-to-number (match-string line-idx line)) 0))
	  (setq err-text      (if (> (length (car patterns)) 4)
				  (match-string (nth 4 (car patterns)) line)
				(flymake-patch-err-text (substring line (match-end 0)))))
	  (or err-text (setq err-text "<no error text>"))
	  ;; Matching for warnings
	  (when (and err-text (one-true (string-match-multi flymake-warn-line-regex err-text)))
	    (setq err-type "w"))
	  ;; Matching for info messages
	  (when (and err-text (one-true (string-match-multi flymake-info-line-regex err-text)))
	    (setq err-type "i"))
	  
	  (flymake-log 3 "parse line: type=%s file-idx=%s line-idx=%s file=%s line=%s text=%s" err-type file-idx line-idx
		       raw-file-name line-no err-text)
	  (setq matched t)))
      (setq patterns (cdr patterns)))
    (if matched
	(flymake-ler-make-ler raw-file-name line-no err-type err-text)
      ())))

(defun flymake-find-err-info (err-info-list line-no)
  "Find (line-err-info-list pos) for specified LINE-NO."
  (if err-info-list
      (let* ((line-err-info-list  nil)
	     (pos       0)
	     (count     (length err-info-list)))

	(while (and (< pos count) (< (car (nth pos err-info-list)) line-no))
	  (setq pos (1+ pos)))
	(when (and (< pos count) (equal (car (nth pos err-info-list)) line-no))
	  (setq line-err-info-list (flymake-er-get-line-err-info-list (nth pos err-info-list))))
	(list line-err-info-list pos))
    '(nil 0)))

(defun flymake-line-err-info-is-less-or-equal (line-one line-two)
  (or (string< (flymake-ler-type line-one) (flymake-ler-type line-two))
      (and (string= (flymake-ler-type line-one) (flymake-ler-type line-two))
	   (not (flymake-ler-file line-one)) (flymake-ler-file line-two))
      (and (string= (flymake-ler-type line-one) (flymake-ler-type line-two))
	   (or (and      (flymake-ler-file line-one)       (flymake-ler-file line-two))
	       (and (not (flymake-ler-file line-one)) (not (flymake-ler-file line-two)))))))

(defun flymake-add-line-err-info (line-err-info-list line-err-info)
  "Update LINE-ERR-INFO-LIST with the error LINE-ERR-INFO.
For the format of LINE-ERR-INFO, see `flymake-ler-make-ler'.
The new element is inserted in the proper position, according to
the predicate `flymake-line-err-info-is-less-or-equal'.
The updated value of LINE-ERR-INFO-LIST is returned."
  (if (not line-err-info-list)
      (list line-err-info)
    (let* ((count  (length line-err-info-list))
	   (idx    0))
      (while (and (< idx count) (flymake-line-err-info-is-less-or-equal (nth idx line-err-info-list) line-err-info))
	(setq idx (1+ idx)))
      (cond ((equal 0     idx)    (setq line-err-info-list (cons line-err-info line-err-info-list)))
	    (t                    (setq line-err-info-list (flymake-ins-after line-err-info-list (1- idx) line-err-info))))
      line-err-info-list)))

(defun flymake-add-err-info (err-info-list line-err-info)
  "Update ERR-INFO-LIST with the error LINE-ERR-INFO, preserving sort order.
Returns the updated value of ERR-INFO-LIST.
For the format of ERR-INFO-LIST, see `flymake-err-info'.
For the format of LINE-ERR-INFO, see `flymake-ler-make-ler'."
  (let* ((line-no             (if (flymake-ler-file line-err-info) 1 (flymake-ler-line line-err-info)))
	 (info-and-pos        (flymake-find-err-info err-info-list line-no))
	 (exists              (car info-and-pos))
	 (pos                 (nth 1 info-and-pos))
	 (line-err-info-list  nil)
	 (err-info            nil))

    (if exists
	(setq line-err-info-list (flymake-er-get-line-err-info-list (car (nthcdr pos err-info-list)))))
    (setq line-err-info-list (flymake-add-line-err-info line-err-info-list line-err-info))

    (setq err-info (flymake-er-make-er line-no line-err-info-list))
    (cond (exists             (setq err-info-list (flymake-set-at err-info-list pos err-info)))
	  ((equal 0 pos)      (setq err-info-list (cons err-info err-info-list)))
	  (t                  (setq err-info-list (flymake-ins-after err-info-list (1- pos) err-info))))
    err-info-list))

(defun flymake-get-project-include-dirs-imp (basedir)
  "Include dirs for the project current file belongs to."
  (if (flymake-get-project-include-dirs-from-cache basedir)
      (progn
	(flymake-get-project-include-dirs-from-cache basedir))
    ;;else
    (let* ((command-line  (concat "make -C "
				  (shell-quote-argument basedir)
				  " DUMPVARS=INCLUDE_DIRS dumpvars"))
	   (output        (shell-command-to-string command-line))
	   (lines         (flymake-split-string output "\n"))
	   (count         (length lines))
	   (idx           0)
	   (inc-dirs      nil))
      (while (and (< idx count) (not (string-match "^INCLUDE_DIRS=.*" (nth idx lines))))
	(setq idx (1+ idx)))
      (when (< idx count)
	(let* ((inc-lines  (flymake-split-string (nth idx lines) " *-I"))
	       (inc-count  (length inc-lines)))
	  (while (> inc-count 0)
	    (when (not (string-match "^INCLUDE_DIRS=.*" (nth (1- inc-count) inc-lines)))
	      (push (flymake-replace-regexp-in-string "\"" "" (nth (1- inc-count) inc-lines)) inc-dirs))
	    (setq inc-count (1- inc-count)))))
      (flymake-add-project-include-dirs-to-cache basedir inc-dirs)
      inc-dirs)))

(defcustom flymake-get-project-include-dirs-function 'flymake-get-project-include-dirs-imp
  "Function used to get project include dirs, one parameter: basedir name."
  :group 'flymake
  :type 'function)

(defun flymake-get-project-include-dirs (basedir)
  (funcall flymake-get-project-include-dirs-function basedir))

(defun flymake-get-system-include-dirs ()
  "System include dirs - from the 'INCLUDE' env setting."
  (let* ((includes (getenv "INCLUDE")))
    (if includes (flymake-split-string includes path-separator) nil)))

(defvar flymake-project-include-dirs-cache (flymake-makehash 'equal))

(defun flymake-get-project-include-dirs-from-cache (base-dir)
  (gethash base-dir flymake-project-include-dirs-cache))

(defun flymake-add-project-include-dirs-to-cache (base-dir include-dirs)
  (puthash base-dir include-dirs flymake-project-include-dirs-cache))

(defun flymake-clear-project-include-dirs-cache ()
  (clrhash flymake-project-include-dirs-cache))

(defun flymake-get-include-dirs (base-dir)
  "Get dirs to use when resolving local file names."
  (let* ((include-dirs (append '(".") (flymake-get-project-include-dirs base-dir) (flymake-get-system-include-dirs))))
    include-dirs))

;; (defun flymake-restore-formatting ()
;;   "Remove any formatting made by flymake."
;;   )

;; (defun flymake-get-program-dir (buffer)
;;   "Get dir to start program in."
;;   (unless (bufferp buffer)
;;     (error "Invalid buffer"))
;;   (with-current-buffer buffer
;;     default-directory))

(defun flymake-safe-delete-file (file-name)
  (when (and file-name (file-exists-p file-name))
    (delete-file file-name)
    (flymake-log 1 "deleted file %s" file-name)))

(defun flymake-safe-delete-directory (dir-name)
  (condition-case err
      (progn
	(delete-directory dir-name)
	(flymake-log 1 "deleted dir %s" dir-name))
    (error
     (flymake-log 1 "Failed to delete dir %s, error ignored" dir-name))))

(defcustom flymake-compilation-prevents-syntax-check t
  "If non-nil, syntax check won't be started in case compilation is running."
  :group 'flymake
  :type 'boolean)

(defun flymake-start-syntax-check ()
  "Start syntax checking for current buffer."
  (interactive)
  (flymake-log 3 "flymake is running: %s" flymake-is-running)
  (when (and (not flymake-is-running)
             (flymake-can-syntax-check-file buffer-file-name))
    (when (or (not flymake-compilation-prevents-syntax-check)
              (not (flymake-compilation-is-running))) ;+ (flymake-rep-ort-status buffer "COMP")
      (flymake-clear-buildfile-cache)
      (flymake-clear-project-include-dirs-cache)

      (setq flymake-check-was-interrupted nil)

      (let* ((source-file-name  buffer-file-name)
             (init-f (flymake-get-init-function source-file-name))
             (cleanup-f (flymake-get-cleanup-function source-file-name))
             (cmd-and-args (funcall init-f))
             (cmd          (nth 0 cmd-and-args))
             (args         (nth 1 cmd-and-args))
             (dir          (nth 2 cmd-and-args)))
        (if (not cmd-and-args)
            (progn
              (flymake-log 0 "init function %s for %s failed, cleaning up" init-f source-file-name)
              (funcall cleanup-f))
          (progn
            (setq flymake-last-change-time nil)
            (flymake-start-syntax-check-process cmd args dir)))))))

(defun flymake-start-syntax-check-process (cmd args dir)
  "Start syntax check process."
  (let* ((process nil))
    (condition-case err
	(progn
	  (when dir
	    (let ((default-directory dir))
	      (flymake-log 3 "starting process on dir %s" default-directory)))
	  (setq process (apply 'start-process "flymake-proc" (current-buffer) cmd args))
	  (set-process-sentinel process 'flymake-process-sentinel)
	  (set-process-filter process 'flymake-process-filter)
          (push process flymake-processes)

          (setq flymake-is-running t)
          (setq flymake-last-change-time nil)
          (setq flymake-check-start-time (flymake-float-time))

	  (flymake-report-status nil "*")
	  (flymake-log 2 "started process %d, command=%s, dir=%s"
		       (process-id process) (process-command process)
                       default-directory)
	  process)
      (error
       (let* ((err-str (format "Failed to launch syntax check process '%s' with args %s: %s"
			       cmd args (error-message-string err)))
	      (source-file-name buffer-file-name)
	      (cleanup-f        (flymake-get-cleanup-function source-file-name)))
	 (flymake-log 0 err-str)
	 (funcall cleanup-f)
	 (flymake-report-fatal-status "PROCERR" err-str))))))

(defun flymake-kill-process (proc)
  "Kill process PROC."
  (kill-process proc)
  (let* ((buf (process-buffer proc)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
	(setq flymake-check-was-interrupted t))))
  (flymake-log 1 "killed process %d" (process-id proc)))

(defun flymake-stop-all-syntax-checks ()
  "Kill all syntax check processes."
  (interactive)
  (while flymake-processes
    (flymake-kill-process (pop flymake-processes))))

(defun flymake-compilation-is-running ()
  (and (boundp 'compilation-in-progress)
       compilation-in-progress))

(defun flymake-compile ()
  "Kill all flymake syntax checks, start compilation."
  (interactive)
  (flymake-stop-all-syntax-checks)
  (call-interactively 'compile))

(defcustom flymake-no-changes-timeout 0.5
  "Time to wait after last change before starting compilation."
  :group 'flymake
  :type 'number)

(defun flymake-on-timer-event (buffer)
  "Start a syntax check for buffer BUFFER if necessary."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when (and (not flymake-is-running)
		 flymake-last-change-time
		 (> (- (flymake-float-time) flymake-last-change-time)
                    flymake-no-changes-timeout))

	(setq flymake-last-change-time nil)
	(flymake-log 3 "starting syntax check as more than 1 second passed since last change")
	(flymake-start-syntax-check)))))

(defun flymake-current-line-no ()
  "Return number of current line in current buffer."
  (count-lines (point-min) (if (eobp) (point) (1+ (point)))))

(defun flymake-count-lines ()
  "Return number of lines in buffer BUFFER."
  (count-lines (point-min) (point-max)))

(defun flymake-display-err-menu-for-current-line ()
  "Display a menu with errors/warnings for current line if it has errors and/or warnings."
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
	 (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
	 (menu-data           (flymake-make-err-menu-data line-no line-err-info-list))
	 (choice              nil))
    (if menu-data
	(progn
	  (setq choice (flymake-popup-menu menu-data))
	  (flymake-log 3 "choice=%s" choice)
	  (when choice
	    (eval choice)))
      (flymake-log 1 "no errors for line %d" line-no))))

(defun flymake-make-err-menu-data (line-no line-err-info-list)
  "Make a (menu-title (item-title item-action)*) list with errors/warnings from LINE-ERR-INFO-LIST."
  (let* ((menu-items  nil))
    (when line-err-info-list
      (let* ((count           (length line-err-info-list))
	     (menu-item-text  nil))
	(while (> count 0)
	  (setq menu-item-text (flymake-ler-text (nth (1- count) line-err-info-list)))
	  (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
		 (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
		 (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
	    (if file
		(setq menu-item-text (concat menu-item-text " - " file "(" (format "%d" line) ")")))
	    (setq menu-items (cons (list menu-item-text
					 (if file (list 'flymake-goto-file-and-line full-file line) nil))
				   menu-items)))
	  (setq count (1- count)))
	(flymake-log 3 "created menu-items with %d item(s)" (length menu-items))))
    (if menu-items
	(let* ((menu-title  (format "Line %d: %d error(s), %d warning(s)" line-no
				    (flymake-get-line-err-count line-err-info-list "e")
				    (flymake-get-line-err-count line-err-info-list "w"))))
	  (list menu-title menu-items))
      nil)))

(defun flymake-goto-file-and-line (file line)
  "Try to get buffer for FILE and goto line LINE in it."
  (if (not (file-exists-p file))
      (flymake-log 1 "File %s does not exist" file)
    (find-file file)
    (goto-char (point-min))
    (forward-line (1- line))))

;; flymake minor mode declarations
(defvar flymake-mode-line nil)

(make-variable-buffer-local 'flymake-mode-line)

(defvar flymake-mode-line-e-w nil)

(make-variable-buffer-local 'flymake-mode-line-e-w)

(defvar flymake-mode-line-status nil)

(make-variable-buffer-local 'flymake-mode-line-status)

(defun flymake-report-status (e-w &optional status)
  "Show status in mode line."
  (when e-w
    (setq flymake-mode-line-e-w e-w))
  (when status
    (setq flymake-mode-line-status status))
  (let* ((mode-line " Flymake"))
    (when (> (length flymake-mode-line-e-w) 0)
      (setq mode-line (concat mode-line ":" flymake-mode-line-e-w)))
    (setq mode-line (concat mode-line flymake-mode-line-status))
    (setq flymake-mode-line mode-line)
    (force-mode-line-update)))

(defun flymake-display-warning (warning)
  "Display a warning to user."
  (message-box warning))

(defcustom flymake-gui-warnings-enabled t
  "Enables/disables GUI warnings."
  :group 'flymake
  :type 'boolean)

(defun flymake-report-fatal-status (status warning)
  "Display a warning and switch flymake mode off."
  (when flymake-gui-warnings-enabled
    (flymake-display-warning (format "Flymake: %s. Flymake will be switched OFF" warning))
    )
  (flymake-mode 0)
  (flymake-log 0 "switched OFF Flymake mode for buffer %s due to fatal status %s, warning %s"
               (buffer-name) status warning))

(defcustom flymake-start-syntax-check-on-find-file t
  "Start syntax check on find file."
  :group 'flymake
  :type 'boolean)

;;;###autoload
(define-minor-mode flymake-mode
  "Minor mode to do on-the-fly syntax checking.
When called interactively, toggles the minor mode.
With arg, turn Flymake mode on if and only if arg is positive."
  :group 'flymake :lighter flymake-mode-line
  (cond

   ;; Turning the mode ON.
   (flymake-mode
    (if (not (flymake-can-syntax-check-file buffer-file-name))
        (flymake-log 2 "flymake cannot check syntax in buffer %s" (buffer-name))
      (add-hook 'after-change-functions 'flymake-after-change-function nil t)
      (add-hook 'after-save-hook 'flymake-after-save-hook nil t)
      (add-hook 'kill-buffer-hook 'flymake-kill-buffer-hook nil t)
      ;;+(add-hook 'find-file-hook 'flymake-find-file-hook)

      (flymake-report-status "" "")

      (setq flymake-timer
            (run-at-time nil 1 'flymake-on-timer-event (current-buffer)))

      (when flymake-start-syntax-check-on-find-file
        (flymake-start-syntax-check))))

   ;; Turning the mode OFF.
   (t
    (remove-hook 'after-change-functions 'flymake-after-change-function t)
    (remove-hook 'after-save-hook 'flymake-after-save-hook t)
    (remove-hook 'kill-buffer-hook 'flymake-kill-buffer-hook t)
    ;;+(remove-hook 'find-file-hook (function flymake-find-file-hook) t)

    (flymake-delete-own-overlays)

    (when flymake-timer
      (cancel-timer flymake-timer)
      (setq flymake-timer nil))

    (setq flymake-is-running nil))))

;;;###autoload
(defun flymake-mode-on ()
  "Turn flymake mode on."
  (flymake-mode 1)
  (flymake-log 1 "flymake mode turned ON for buffer %s" (buffer-name)))

;;;###autoload
(defun flymake-mode-off ()
  "Turn flymake mode off."
  (flymake-mode 0)
  (flymake-log 1 "flymake mode turned OFF for buffer %s" (buffer-name)))

(defcustom flymake-start-syntax-check-on-newline t
  "Start syntax check if newline char was added/removed from the buffer."
  :group 'flymake
  :type 'boolean)

(defun flymake-after-change-function (start stop len)
  "Start syntax check for current buffer if it isn't already running."
  ;;+(flymake-log 0 "setting change time to %s" (flymake-float-time))
  (let((new-text (buffer-substring start stop)))
    (when (and flymake-start-syntax-check-on-newline (equal new-text "\n"))
      (flymake-log 3 "starting syntax check as new-line has been seen")
      (flymake-start-syntax-check))
    (setq flymake-last-change-time (flymake-float-time))))

(defun flymake-after-save-hook ()
  (if (local-variable-p 'flymake-mode (current-buffer))	; (???) other way to determine whether flymake is active in buffer being saved?
      (progn
	(flymake-log 3 "starting syntax check as buffer was saved")
	(flymake-start-syntax-check)))) ; no more mode 3. cannot start check if mode 3 (to temp copies) is active - (???)

(defun flymake-kill-buffer-hook ()
  (when flymake-timer
    (cancel-timer flymake-timer)
    (setq flymake-timer nil)))

(defun flymake-find-file-hook ()
  ;;+(when flymake-start-syntax-check-on-find-file
  ;;+    (flymake-log 3 "starting syntax check on file open")
  ;;+    (flymake-start-syntax-check)
  ;;+)
  (when (and (not (local-variable-p 'flymake-mode (current-buffer)))
	     (flymake-can-syntax-check-file buffer-file-name))
    (flymake-mode)
    (flymake-log 3 "automatically turned ON flymake mode")))

(defun flymake-get-first-err-line-no (err-info-list)
  "Return first line with error."
  (when err-info-list
    (flymake-er-get-line (car err-info-list))))

(defun flymake-get-last-err-line-no (err-info-list)
  "Return last line with error."
  (when err-info-list
    (flymake-er-get-line (nth (1- (length err-info-list)) err-info-list))))

(defun flymake-get-next-err-line-no (err-info-list line-no)
  "Return next line with error."
  (when err-info-list
    (let* ((count  (length err-info-list))
	   (idx    0))
      (while (and (< idx count) (>= line-no (flymake-er-get-line (nth idx err-info-list))))
	(setq idx (1+ idx)))
      (if (< idx count)
	  (flymake-er-get-line (nth idx err-info-list))))))

(defun flymake-get-prev-err-line-no (err-info-list line-no)
  "Return previous line with error."
  (when err-info-list
    (let* ((count (length err-info-list)))
      (while (and (> count 0) (<= line-no (flymake-er-get-line (nth (1- count) err-info-list))))
	(setq count (1- count)))
      (if (> count 0)
	  (flymake-er-get-line (nth (1- count) err-info-list))))))

(defun flymake-skip-whitespace ()
  "Move forward until non-whitespace is reached."
  (while (looking-at "[ \t]")
    (forward-char)))

(defun flymake-goto-line (line-no)
  "Go to line LINE-NO, then skip whitespace."
  (goto-char (point-min))
  (forward-line (1- line-no))
  (flymake-skip-whitespace))

(defun flymake-goto-next-error ()
  "Go to next error in err ring."
  (interactive)
  (let ((line-no (flymake-get-next-err-line-no flymake-err-info (flymake-current-line-no))))
    (when (not line-no)
      (setq line-no (flymake-get-first-err-line-no flymake-err-info))
      (flymake-log 1 "passed end of file"))
    (if line-no
	(flymake-goto-line line-no)
      (flymake-log 1 "no errors in current buffer"))))

(defun flymake-goto-prev-error ()
  "Go to previous error in err ring."
  (interactive)
  (let ((line-no (flymake-get-prev-err-line-no flymake-err-info (flymake-current-line-no))))
    (when (not line-no)
      (setq line-no (flymake-get-last-err-line-no flymake-err-info))
      (flymake-log 1 "passed beginning of file"))
    (if line-no
	(flymake-goto-line line-no)
      (flymake-log 1 "no errors in current buffer"))))

(defun flymake-patch-err-text (string)
  (if (string-match "^[\n\t :0-9]*\\(.*\\)$" string)
      (match-string 1 string)
    string))

;;;; general init-cleanup and helper routines
(defun flymake-create-temp-inplace (file-name prefix)
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((temp-name   (concat (file-name-sans-extension file-name)
			      "_" prefix
			      (and (file-name-extension file-name)
				   (concat "." (file-name-extension file-name))))))
    (flymake-log 3 "create-temp-inplace: file=%s temp=%s" file-name temp-name)
    temp-name))

(defun flymake-create-temp-with-folder-structure (file-name prefix)
  (unless (stringp file-name)
    (error "Invalid file-name"))

  (let* ((dir       (file-name-directory file-name))
         ;; Not sure what this slash-pos is all about, but I guess it's just
         ;; trying to remove the leading / of absolute file names.
	 (slash-pos (string-match "/" dir))
	 (temp-dir  (expand-file-name (substring dir (1+ slash-pos))
                                      (flymake-get-temp-dir))))

    (file-truename (expand-file-name (file-name-nondirectory file-name)
                                     temp-dir))))

(defun flymake-delete-temp-directory (dir-name)
  "Attempt to delete temp dir created by `flymake-create-temp-with-folder-structure', do not fail on error."
  (let* ((temp-dir    (flymake-get-temp-dir))
	 (suffix      (substring dir-name (1+ (length temp-dir)))))

    (while (> (length suffix) 0)
      (setq suffix (directory-file-name suffix))
      ;;+(flymake-log 0 "suffix=%s" suffix)
      (flymake-safe-delete-directory
       (file-truename (expand-file-name suffix temp-dir)))
      (setq suffix (file-name-directory suffix)))))

(defvar flymake-temp-source-file-name nil)
(make-variable-buffer-local 'flymake-temp-source-file-name)

(defvar flymake-master-file-name nil)
(make-variable-buffer-local 'flymake-master-file-name)

(defvar flymake-temp-master-file-name nil)
(make-variable-buffer-local 'flymake-temp-master-file-name)

(defvar flymake-base-dir nil)
(make-variable-buffer-local 'flymake-base-dir)

(defun flymake-init-create-temp-buffer-copy (create-temp-f)
  "Make a temporary copy of the current buffer, save its name in buffer data and return the name."
  (let*  ((source-file-name       buffer-file-name)
	  (temp-source-file-name  (funcall create-temp-f source-file-name "flymake")))

    (flymake-save-buffer-in-file temp-source-file-name)
    (setq flymake-temp-source-file-name temp-source-file-name)
    temp-source-file-name))

(defun flymake-simple-cleanup ()
  "Do cleanup after `flymake-init-create-temp-buffer-copy'.
Delete temp file."
  (flymake-safe-delete-file flymake-temp-source-file-name)
  (setq flymake-last-change-time nil))

(defun flymake-get-real-file-name (file-name-from-err-msg)
  "Translate file name from error message to \"real\" file name.
Return full-name.  Names are real, not patched."
  (let* ((real-name		nil)
	 (source-file-name	buffer-file-name)
	 (master-file-name	flymake-master-file-name)
	 (temp-source-file-name	flymake-temp-source-file-name)
	 (temp-master-file-name	flymake-temp-master-file-name)
	 (base-dirs
          (list flymake-base-dir
                (file-name-directory source-file-name)
                (if master-file-name (file-name-directory master-file-name))))
	 (files (list (list source-file-name       source-file-name)
                      (list temp-source-file-name  source-file-name)
                      (list master-file-name       master-file-name)
                      (list temp-master-file-name  master-file-name))))

    (when (equal 0 (length file-name-from-err-msg))
      (setq file-name-from-err-msg source-file-name))

    (setq real-name (flymake-get-full-patched-file-name file-name-from-err-msg base-dirs files))
    ;; if real-name is nil, than file name from err msg is none of the files we've patched
    (if (not real-name)
	(setq real-name (flymake-get-full-nonpatched-file-name file-name-from-err-msg base-dirs)))
    (if (not real-name)
	(setq real-name file-name-from-err-msg))
    (setq real-name (flymake-fix-file-name real-name))
    (flymake-log 3 "get-real-file-name: file-name=%s real-name=%s" file-name-from-err-msg real-name)
    real-name))

(defun flymake-get-full-patched-file-name (file-name-from-err-msg base-dirs files)
  (let* ((base-dirs-count  (length base-dirs))
	 (file-count       (length files))
	 (real-name        nil))

    (while (and (not real-name) (> base-dirs-count 0))
      (setq file-count (length files))
      (while (and (not real-name) (> file-count 0))
	(let* ((this-dir        (nth (1- base-dirs-count) base-dirs))
	       (this-file       (nth 0 (nth (1- file-count) files)))
	       (this-real-name  (nth 1 (nth (1- file-count) files))))
	  ;;+(flymake-log 0 "this-dir=%s this-file=%s this-real=%s msg-file=%s" this-dir this-file this-real-name file-name-from-err-msg)
	  (when (and this-dir this-file (flymake-same-files
					 (expand-file-name file-name-from-err-msg this-dir)
					 this-file))
	    (setq real-name this-real-name)))
	(setq file-count (1- file-count)))
      (setq base-dirs-count (1- base-dirs-count)))
    real-name))

(defun flymake-get-full-nonpatched-file-name (file-name-from-err-msg base-dirs)
  (let* ((real-name  nil))
    (if (file-name-absolute-p file-name-from-err-msg)
	(setq real-name file-name-from-err-msg)
      (let* ((base-dirs-count  (length base-dirs)))
	(while (and (not real-name) (> base-dirs-count 0))
	  (let* ((full-name (expand-file-name file-name-from-err-msg
					      (nth (1- base-dirs-count) base-dirs))))
	    (if (file-exists-p full-name)
		(setq real-name full-name))
	    (setq base-dirs-count (1- base-dirs-count))))))
    real-name))

(defun flymake-init-find-buildfile-dir (source-file-name buildfile-name)
  "Find buildfile, store its dir in buffer data and return its dir, if found."
  (let* ((buildfile-dir
          (flymake-find-buildfile buildfile-name
                                  (file-name-directory source-file-name))))
    (if buildfile-dir
        (setq flymake-base-dir buildfile-dir)
      (flymake-log 1 "no buildfile (%s) for %s" buildfile-name source-file-name)
      (flymake-report-fatal-status
       "NOMK" (format "No buildfile (%s) found for %s"
                      buildfile-name source-file-name)))))

(defun flymake-init-create-temp-source-and-master-buffer-copy (get-incl-dirs-f create-temp-f master-file-masks include-regexp)
  "Find master file (or buffer), create its copy along with a copy of the source file."
  (let* ((source-file-name       buffer-file-name)
	 (temp-source-file-name  (flymake-init-create-temp-buffer-copy create-temp-f))
	 (master-and-temp-master (flymake-create-master-file
				  source-file-name temp-source-file-name
				  get-incl-dirs-f create-temp-f
				  master-file-masks include-regexp)))

    (if (not master-and-temp-master)
	(progn
	  (flymake-log 1 "cannot find master file for %s" source-file-name)
          (flymake-report-status "!" "")	; NOMASTER
          nil)
      (setq flymake-master-file-name (nth 0 master-and-temp-master))
      (setq flymake-temp-master-file-name (nth 1 master-and-temp-master)))))

(defun flymake-master-cleanup ()
  (flymake-simple-cleanup)
  (flymake-safe-delete-file flymake-temp-master-file-name))

;;;; make-specific init-cleanup routines
(defun flymake-get-syntax-check-program-args (source-file-name base-dir use-relative-base-dir use-relative-source get-cmd-line-f)
  "Create a command line for syntax check using GET-CMD-LINE-F."
  (funcall get-cmd-line-f
           (if use-relative-source
               (file-relative-name source-file-name base-dir)
             source-file-name)
           (if use-relative-base-dir
               (file-relative-name base-dir
                                   (file-name-directory source-file-name))
             base-dir)))

(defun flymake-get-make-cmdline (source base-dir)
  (list "make"
	(list "-s"
	      "-C"
	      base-dir
	      (concat "CHK_SOURCES=" source)
	      "SYNTAX_CHECK_MODE=1"
	      "check-syntax")))

(defun flymake-get-ant-cmdline (source base-dir)
  (list "ant"
	(list "-buildfile"
	      (concat base-dir "/" "build.xml")
	      (concat "-DCHK_SOURCES=" source)
	      "check-syntax")))

(defun flymake-simple-make-init-impl (create-temp-f use-relative-base-dir use-relative-source build-file-name get-cmdline-f)
  "Create syntax check command line for a directly checked source file.
Use CREATE-TEMP-F for creating temp copy."
  (let* ((args nil)
	 (source-file-name   buffer-file-name)
	 (buildfile-dir      (flymake-init-find-buildfile-dir source-file-name build-file-name)))
    (if buildfile-dir
	(let* ((temp-source-file-name  (flymake-init-create-temp-buffer-copy create-temp-f)))
	  (setq args (flymake-get-syntax-check-program-args temp-source-file-name buildfile-dir
							    use-relative-base-dir use-relative-source
							    get-cmdline-f))))
    args))

(defun flymake-simple-make-init ()
  (flymake-simple-make-init-impl 'flymake-create-temp-inplace t t "Makefile" 'flymake-get-make-cmdline))

(defun flymake-master-make-init (get-incl-dirs-f master-file-masks include-regexp)
  "Create make command line for a source file checked via master file compilation."
  (let* ((make-args nil)
	 (temp-master-file-name (flymake-init-create-temp-source-and-master-buffer-copy
                                 get-incl-dirs-f 'flymake-create-temp-inplace
				 master-file-masks include-regexp)))
    (when temp-master-file-name
      (let* ((buildfile-dir (flymake-init-find-buildfile-dir temp-master-file-name "Makefile")))
	(if  buildfile-dir
	    (setq make-args (flymake-get-syntax-check-program-args
			     temp-master-file-name buildfile-dir nil nil 'flymake-get-make-cmdline)))))
    make-args))

(defun flymake-find-make-buildfile (source-dir)
  (flymake-find-buildfile "Makefile" source-dir))

;;;; .h/make specific
(defun flymake-master-make-header-init ()
  (flymake-master-make-init 'flymake-get-include-dirs
			    '("\\.cpp\\'" "\\.c\\'")
			    "[ \t]*#[ \t]*include[ \t]*\"\\([[:word:]0-9/\\_.]*%s\\)\""))

;;;; .java/make specific
(defun flymake-simple-make-java-init ()
  (flymake-simple-make-init-impl 'flymake-create-temp-with-folder-structure nil nil "Makefile" 'flymake-get-make-cmdline))

(defun flymake-simple-ant-java-init ()
  (flymake-simple-make-init-impl 'flymake-create-temp-with-folder-structure nil nil "build.xml" 'flymake-get-ant-cmdline))

(defun flymake-simple-java-cleanup ()
  "Cleanup after `flymake-simple-make-java-init' -- delete temp file and dirs."
  (flymake-safe-delete-file flymake-temp-source-file-name)
  (when flymake-temp-source-file-name
    (flymake-delete-temp-directory
     (file-name-directory flymake-temp-source-file-name))))

;;;; perl-specific init-cleanup routines
(defun flymake-perl-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
	 (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "perl" (list "-wc " local-file))))

;;;; php-specific init-cleanup routines
(defun flymake-php-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
	 (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "php" (list "-f" local-file "-l"))))

;;;; tex-specific init-cleanup routines
(defun flymake-get-tex-args (file-name)
  ;;(list "latex" (list "-c-style-errors" file-name))
  (list "texify" (list "--pdf" "--tex-option=-c-style-errors" file-name)))

(defun flymake-simple-tex-init ()
  (flymake-get-tex-args (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace)))

(defun flymake-master-tex-init ()
  (let* ((temp-master-file-name (flymake-init-create-temp-source-and-master-buffer-copy
                                 'flymake-get-include-dirs-dot 'flymake-create-temp-inplace
				 '("\\.tex\\'")
				 "[ \t]*\\input[ \t]*{\\(.*%s\\)}")))
    (when temp-master-file-name
      (flymake-get-tex-args temp-master-file-name))))

(defun flymake-get-include-dirs-dot (base-dir)
  '("."))

;;;; xml-specific init-cleanup routines
(defun flymake-xml-init ()
  (list "xmlstarlet" (list "val" (flymake-init-create-temp-buffer-copy 'flymake-create-temp-inplace))))

(provide 'flymake)

;; arch-tag: 8f0d6090-061d-4cac-8862-7c151c4a02dd
;;; flymake.el ends here
