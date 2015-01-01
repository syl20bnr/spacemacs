;;; neotree.el --- A tree plugin like NerdTree for Vim

;; Copyright (C) 2014 jaypei

;; Author: jaypei <jaypei97159@gmail.com>
;; URL: https://github.com/jaypei/emacs-neotree
;; Version: 0.2.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; To use this file, put something like the following in your
;; ~/.emacs:
;;
;; (add-to-list 'load-path "/directory/containing/neotree/")
;; (require 'neotree)
;;
;; Type M-x neotree to start.
;;
;; To set options for NeoTree, type M-x customize, then select
;; Applications, NeoTree.
;;

;;; Code:

;;
;; Constants
;;

(defconst neo-buffer-name " *NeoTree*"
  "Name of the buffer where neotree shows directory contents.")

(defconst neo-hidden-files-regexp "^\\."
  "Hidden files regexp.
By default all filest starting with dot '.' including . and ..")

(defconst neo-header-height 5)

(eval-and-compile

  ;; Added in Emacs 24.3
  (unless (fboundp 'user-error)
    (defalias 'user-error 'error))

  ;; Added in Emacs 24.3 (mirrors/emacs@b335efc3).
  (unless (fboundp 'setq-local)
    (defmacro setq-local (var val)
      "Set variable VAR to value VAL in current buffer."
      (list 'set (list 'make-local-variable (list 'quote var)) val)))

  ;; Added in Emacs 24.3 (mirrors/emacs@b335efc3).
  (unless (fboundp 'defvar-local)
    (defmacro defvar-local (var val &optional docstring)
      "Define VAR as a buffer-local variable with default value VAL.
Like `defvar' but additionally marks the variable as being automatically
buffer-local wherever it is set."
      (declare (debug defvar) (doc-string 3))
      (list 'progn (list 'defvar var val docstring)
            (list 'make-variable-buffer-local (list 'quote var))))))


;;
;; Customization
;;

(defgroup neotree-options nil
  "Options for neotree."
  :prefix "neo-"
  :group 'neotree
  :link '(info-link "(neotree)Configuration"))

(defcustom neo-create-file-auto-open nil
  "*If non-nil, the file will auto open when created."
  :type 'boolean
  :group 'neotree)

(defcustom neo-dont-be-alone nil
  "*If non-nil, you cannot left neotree window alone."
  :type 'boolean
  :group 'neotree)

(defcustom neo-persist-show t
  "*If non-nil, NeoTree window will not be turned off while press C\-x 1."
  :type 'boolean
  :group 'neotree)

(defcustom neo-banner-message "Press ? for help."
  "*The banner message of neotree window."
  :type 'string
  :group 'neotree)

(defcustom neo-show-updir-line t
  "*If non-nil, show the updir line (..)."
  :type 'boolean
  :group 'neotree)

(defcustom neo-mode-line-type 'neotree
  "*The mode-line type to display, `default' is a non-modified mode-line,
`neotree' is a compact mode-line that shows useful information about the
 current node like the parent directory and the number of nodes,
`custom' uses the format stored in `neo-mode-line-custom-format',
`none' hide the mode-line."
  :group 'neotree
  :type '(choice (const default)
                 (const neotree)
                 (const custom)
                 (const none)))

(defcustom neo-mode-line-custom-format nil
  "*If `neo-mode-line-type' is set to `custom', this variable specifiy
the mode-line format."
  :type 'sexp
  :group 'neotree)

(defcustom neo-smart-open nil
  "*If non-nil, every time when the neotree window is opened, it will try to find current file and jump to node."
  :type 'boolean
  :group 'neotree)

(defcustom neo-show-hidden-files nil
  "*If non-nil, the hidden files are shown by default."
  :type 'boolean
  :group 'neotree)

(defcustom neo-window-width 25
  "*Specifies the width of the NeoTree window."
  :type 'integer
  :group 'neotree)

(defcustom neo-keymap-style 'default
  "The default keybindings for neotree-mode-map."
  :group 'neotree
  :type '(choice (const default)
                 (const concise)))

(defcustom neo-cwd-line-style 'text
  "The default header style."
  :group 'neotree
  :type '(choice (const text)
                 (const button)))

(defcustom neo-click-changes-root nil
  "*If non-nil, clicking on a directory will change the current root to the directory."
  :type 'boolean
  :group 'neotree)

(defcustom neo-auto-indent-point nil
  "*If non-nil the point is autmotically put on the first letter of a node."
  :type 'boolean
  :group 'neotree)

;;
;; Faces
;;

(defface neo-banner-face
  '((((background dark)) (:foreground "lightblue" :weight bold))
    (t                   (:foreground "DarkMagenta")))
  "*Face used for the banner in neotree buffer."
  :group 'neotree :group 'font-lock-highlighting-faces)
(defvar neo-banner-face 'neo-banner-face)

(defface neo-header-face
  '((((background dark)) (:foreground "White"))
    (t                   (:foreground "DarkMagenta")))
  "*Face used for the header in neotree buffer."
  :group 'neotree :group 'font-lock-highlighting-faces)
(defvar neo-header-face 'neo-header-face)

(defface neo-root-dir-face
  '((((background dark)) (:foreground "lightblue" :weight bold))
    (t                   (:foreground "DarkMagenta")))
  "*Face used for the root dir in neotree buffer."
  :group 'neotree :group 'font-lock-highlighting-faces)
(defvar neo-root-dir-face 'neo-root-dir-face)

(defface neo-dir-link-face
  '((((background dark)) (:foreground "DeepSkyBlue"))
    (t                   (:foreground "MediumBlue")))
  "*Face used for expand sign [+] in neotree buffer."
  :group 'neotree :group 'font-lock-highlighting-faces)
(defvar neo-dir-link-face 'neo-dir-link-face)

(defface neo-file-link-face
  '((((background dark)) (:foreground "White"))
    (t                   (:foreground "Black")))
  "*Face used for open file/dir in neotree buffer."
  :group 'neotree :group 'font-lock-highlighting-faces)
(defvar neo-file-link-face 'neo-file-link-face)

(defface neo-button-face
  '((t (:underline nil)))
  "*Face used for open file/dir in neotree buffer."
  :group 'neotree :group 'font-lock-highlighting-faces)
(defvar neo-button-face 'neo-button-face)

(defface neo-expand-btn-face
  '((((background dark)) (:foreground "SkyBlue"))
    (t                   (:foreground "DarkCyan")))
  "*Face used for open file/dir in neotree buffer."
  :group 'neotree :group 'font-lock-highlighting-faces)
(defvar neo-expand-btn-face 'neo-expand-btn-face)


;;
;; Variables
;;

(defvar neo-global--buffer nil)

(defvar neo-global--window nil)

(defvar neo-mode-line-format
  (list
   '(:eval
     (let* ((fname (neo-buffer--get-filename-current-line))
            (current (if fname fname neo-buffer--start-node))
            (parent (if fname (file-name-directory current) current))
            (nodes (neo-buffer--get-nodes parent))
            (dirs (car nodes))
            (files (cdr nodes))
            (ndirs (length dirs))
            (nfiles (length files))
            (index
             (when fname
               (1+ (if (file-directory-p current)
                       (neo-buffer--get-node-index current dirs)
                     (+ ndirs (neo-buffer--get-node-index current files)))))))
       (neo-buffer--compute-mode-line-format parent index ndirs nfiles))))
  "Neotree mode-line displaying information on the current node.
This mode-line format is used if `neo-mode-line-type' is set to `neotree'")

(defun neo-buffer--compute-mode-line-format (path index ndirs nfiles)
  "Return a formated string to be used in the `neotree' mode-line."
  (let* ((nall (+ ndirs nfiles))
         (has-dirs (> ndirs 0))
         (has-files (> nfiles 0))
         (msg-index (when index (format "[%s/%s] " index nall)))
         (msg-ndirs (when has-dirs (format (if has-files " (D:%s" " (D:%s)") ndirs)))
         (msg-nfiles (when has-files (format (if has-dirs " F:%s)" " (F:%s)") nfiles)))
         (msg-directory (file-name-nondirectory (directory-file-name parent)))
         (msg-directory-max-length (- (window-width)
                                      (length msg-index)
                                      (length msg-ndirs)
                                      (length msg-nfiles))))
    (setq msg-directory (if (<= (length msg-directory) msg-directory-max-length)
                            msg-directory
                          (concat (substring msg-directory
                                             0 (- msg-directory-max-length 3))
                                  "...")))
    (propertize
     (concat msg-index msg-directory msg-ndirs msg-nfiles)
     'help-echo parent)))

(defvar-local neo-buffer--start-node nil
  "Start node(i.e. directory) for the window.")

(defvar-local neo-buffer--start-line nil
  "Index of the start line of the root.")

(defvar-local neo-buffer--cursor-pos (cons nil 1)
  "To save the cursor position.
The car of the pair will store fullpath, and cdr will store line number.")

(defvar-local neo-buffer--last-window-pos (cons nil 1)
  "To save the scroll position for NeoTree window.")

(defvar-local neo-buffer--show-hidden-file-p nil
  "Show hidden nodes in tree.")

(defvar-local neo-buffer--expanded-node-list nil
  "A list of expanded dir nodes.")

(defvar-local neo-buffer--node-list nil
  "The model of current NeoTree buffer.")

(defvar-local neo-buffer--node-list-1 nil
  "The model of current NeoTree buffer (temp).")


;;
;; Major mode definitions
;;

(defvar neotree-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "SPC")     'neotree-enter)
    (define-key map (kbd "TAB")     'neotree-enter)
    (define-key map (kbd "RET")     'neotree-enter)
    (define-key map (kbd "|")       'neotree-enter-vertical-split)
    (define-key map (kbd "-")       'neotree-enter-horizontal-split)
    (define-key map (kbd "g")       'neotree-refresh)
    (define-key map (kbd "q")       'neotree-hide)
    (define-key map (kbd "p")       'previous-line)
    (define-key map (kbd "n")       'next-line)
    (define-key map (kbd "A")       'neotree-stretch-toggle)
    (define-key map (kbd "U")       'neotree-select-up-node)
    (define-key map (kbd "D")       'neotree-select-down-node)
    (define-key map (kbd "H")       'neotree-hidden-file-toggle)
    (define-key map (kbd "S")       'neotree-select-previous-sibling-node)
    (define-key map (kbd "s")       'neotree-select-next-sibling-node)
    (define-key map (kbd "C-x C-f") 'find-file-other-window)
    (define-key map (kbd "C-x 1")   'neotree-empty-fn)
    (define-key map (kbd "C-x 2")   'neotree-empty-fn)
    (define-key map (kbd "C-x 3")   'neotree-empty-fn)
    (define-key map (kbd "C-c C-f") 'find-file-other-window)
    (define-key map (kbd "C-c C-c") 'neotree-change-root)
    (cond
     ((eq neo-keymap-style 'default)
      (define-key map (kbd "C-c C-n") 'neotree-create-node)
      (define-key map (kbd "C-c C-d") 'neotree-delete-node)
      (define-key map (kbd "C-c C-r") 'neotree-rename-node))
     ((eq neo-keymap-style 'concise)
      (define-key map (kbd "C") 'neotree-change-root)
      (define-key map (kbd "c") 'neotree-create-node)
      (define-key map (kbd "+") 'neotree-create-node)
      (define-key map (kbd "d") 'neotree-delete-node)
      (define-key map (kbd "r") 'neotree-rename-node)
      (define-key map (kbd "e") 'neotree-enter)))
    map)
  "Keymap for `neotree-mode'.")

(define-derived-mode neotree-mode special-mode "NeoTree"
  "A major mode for displaying the directory tree in text mode."
  (setq indent-tabs-mode nil            ; only spaces
        buffer-read-only t              ; read only
        truncate-lines -1
        neo-buffer--show-hidden-file-p neo-show-hidden-files)
  (pcase neo-mode-line-type
    (`neotree
     (setq-local mode-line-format neo-mode-line-format)
     (add-hook 'post-command-hook 'force-mode-line-update nil t))
    (`none (setq-local mode-line-format nil))
    (`custom
     (setq-local mode-line-format neo-mode-line-custom-format)
     (add-hook 'post-command-hook 'force-mode-line-update nil t))
    (_ nil))
  ;; fix for electric-indent-mode
  ;; for emacs 24.4
  (if (fboundp 'electric-indent-local-mode)
      (electric-indent-local-mode -1)
    ;; for emacs 24.3 or less
    (add-hook 'electric-indent-functions
              (lambda (arg) 'no-indent) nil 'local))
  (when neo-auto-indent-point
    (add-hook 'post-command-hook 'neo-hook--node-first-letter nil t)))


;;
;; Macros
;;

(defmacro neo-global--with-buffer (&rest body)
  "Execute the forms in BODY with global NeoTree buffer."
  (declare (indent 0) (debug t))
  `(with-current-buffer (neo-global--get-buffer)
     ,@body))

(defmacro neo-global--with-window (&rest body)
  "Execute the forms in BODY with global NeoTree window."
  (declare (indent 0) (debug t))
  `(save-selected-window
     (neo-global--select-window)
     ,@body))

(defmacro neo-global--when-window (&rest body)
  "Execute the forms in BODY when selected window is NeoTree window."
  (declare (indent 0) (debug t))
  `(when (eq (selected-window) neo-global--window)
     ,@body))

(defmacro neo-buffer--save-excursion (&rest body)
  "Execute BODY in neotree window, then restore previous window configuration."
  `(save-window-excursion
     (let ((rlt nil))
       (switch-to-buffer (neo-global--get-buffer))
       (setq buffer-read-only nil)
       (setf rlt (progn ,@body))
       (setq buffer-read-only t)
       rlt)))


;;
;; Global methods
;;

(defun neo-global--window-exists-p ()
  "Return non-nil if neotree window exists."
  (and (not (null (window-buffer neo-global--window)))
       (eql (window-buffer neo-global--window) (neo-global--get-buffer))))

(defun neo-global--select-window ()
  "Select the NeoTree window."
  (interactive)
  (let ((window (neo-global--get-window t)))
    (select-window window)))

(defun neo-global--get-window (&optional auto-create-p)
  "Return the neotree window if it exists, else return nil.
But when the neotree window does not exist and AUTO-CREATE-P is non-nil,
it will create the neotree window and return it."
  (unless (neo-global--window-exists-p)
    (setf neo-global--window nil))
  (when (and (null neo-global--window)
             auto-create-p)
    (setq neo-global--window
          (neo-global--create-window)))
  neo-global--window)

(defun neo-global--get-first-window ()
  "Return the top-left window."
  (let (w)
    (if (null w)
        (setq w (window-at 0 0)))
    (if (null w)
        (setq w (selected-window)))
    w))

(defun neo-global--create-window ()
  "Create global neotree window."
  (let ((window nil)
        (buffer (neo-global--get-buffer))
        (curr-window-buffer (current-buffer)))
    (select-window (neo-global--get-first-window))
    (split-window-horizontally)
    (setq window (selected-window))
    (neo-window--init window buffer)
    (setq neo-global--window window)
    (select-window (or (get-buffer-window curr-window-buffer)
                       window))
    window))

(defun neo-global--get-buffer ()
  "Return the global neotree buffer if it exists."
  (if (not (equal (buffer-name neo-global--buffer)
                  neo-buffer-name))
      (setf neo-global--buffer nil))
  (if (null neo-global--buffer)
      (save-window-excursion
        (neo-buffer--create)))
  neo-global--buffer)

(defun neo-global--file-in-root-p (path)
  "Return non-nil if PATH in root dir."
  (neo-global--with-buffer
   (and (not (null neo-buffer--start-node))
        (neo-path--file-in-directory-p path neo-buffer--start-node))))

(defun neo-global--alone-p ()
  "Check whether the global neotree window is alone with some other window."
  (let ((windows (window-list)))
    (and (= (length windows)
            2)
         (member neo-global--window windows))))

(defun neo-global--open ()
  "Show the NeoTree widnow."
  (let ((valid-start-node-p nil))
    (neo-buffer--save-excursion
     (setf valid-start-node-p (neo-buffer--valid-start-node-p)))
    (if (not valid-start-node-p)
        (neo-global--open-dir (neo-path--get-working-dir))
      (neo-global--get-window t))))

(defun neo-global--open-dir (path)
  "Show the NeoTree window, and change root to PATH."
  (neo-global--get-window t)
  (neo-buffer--save-excursion
   (neo-buffer--change-root path)))

(defun neo-global--open-and-find (path)
  "Quick select node which specified PATH in NeoTree."
  (let ((npath path)
        root-dir)
    (when (null npath)
      (throw 'invalid-path "Invalid path to select."))
    (setq root-dir (if (file-directory-p npath)
                       npath (neo-path--updir npath)))
    (when (or (not (neo-global--window-exists-p))
              (not (neo-global--file-in-root-p npath)))
      (neo-global--open-dir root-dir))
    (neo-global--with-window
      (neo-buffer--select-file-node npath t))))

;;
;; Advices
;;

(defadvice delete-other-windows
  (around neotree-delete-other-windows activate)
  "Delete all windows except neotree."
  (interactive)
  (if neo-persist-show
      (mapc (lambda (window)
                (if (not (string-equal (buffer-name (window-buffer window)) neo-buffer-name))
                    (delete-window window)))
              (cdr (window-list)))
    ad-do-it))

(defadvice delete-window
  (around neotree-delete-window activate)
  "Stop to delete window which it is the last window except NeoTree."
  (if (and neo-dont-be-alone
           (not (eq window
                    neo-global--window))
           (neo-global--alone-p))
      (message "only one window other than neotree left. won't close")
    ad-do-it))

(defadvice mouse-drag-vertical-line
  (around neotree-drag-vertical-line (start-event) activate)
  "Drag and drop is not affected by the lock."
  (neo-global--with-buffer
   (neo-buffer--unlock-width))
  ad-do-it
  (neo-global--with-buffer
   (neo-buffer--lock-width))
  ad-return-value)

(eval-after-load 'popwin
  '(progn
     (defadvice popwin:popup-buffer
         (around neotree/popwin-popup-buffer activate)
       (let ((neobuffer (get-buffer neo-buffer-name))
             (neopersist neo-persist-show))
         (when neobuffer
           (setq neo-persist-show nil)
           (with-current-buffer (neo-global--get-buffer)
             (neo-buffer--unlock-width)))
         ad-do-it
         (when neobuffer
           (setq neo-persist-show neopersist)
           (with-current-buffer (neo-global--get-buffer)
             (neo-buffer--lock-width))
           ;; set neotree global window and buffer to the new ones
           ;; created by popwin
           (setq neo-global--buffer (get-buffer neo-buffer-name))
           (setq neo-global--window (get-buffer-window
                                     neo-global--buffer)))))

     (defadvice popwin:close-popup-window
         (around neotree/popwin-close-popup-window activate)
       (let ((neobuffer (get-buffer neo-buffer-name))
             (neopersist neo-persist-show))
         (when neobuffer
           (setq neo-persist-show nil)
           (with-current-buffer (neo-global--get-buffer)
             (neo-buffer--unlock-width)))
         ad-do-it
         (when neobuffer
           (setq neo-persist-show neopersist)
           (with-current-buffer (neo-global--get-buffer)
             (neo-buffer--lock-width))
           ;; set neotree global window and buffer to the new ones
           ;; created by popwin
           (setq neo-global--buffer (get-buffer neo-buffer-name))
           (setq neo-global--window (get-buffer-window
                                     neo-global--buffer)))))))

;;
;; Hooks
;;

(defun neo-hook--node-first-letter ()
  "Move point to the first letter of the current node."
  (when (or (eq this-command 'next-line)
            (eq this-command 'previous-line))
    (neo-point-auto-indent)))

;;
;; Util methods
;;

(defun neo-util--filter (condp lst)
"Apply CONDP to elements of LST keeping those that return non-nil.

Example:
    (neo-util--filter 'symbolp '(a \"b\" 3 d4))
         => (a d4)

This procedure does not work when CONDP is the `null' function."
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun neo-util--find (where which)
  "Find element of the list WHERE matching predicate WHICH."
  (catch 'found
    (dolist (elt where)
      (when (funcall which elt)
        (throw 'found elt)))
    nil))

(defun neo-util--make-printable-string (string)
  "Strip newline character from STRING, like 'Icon\n'."
  (replace-regexp-in-string "\n" "" string))

(defun neo-util--walk-dir (path)
  (let* ((full-path (neo-path--file-truename path)))
    (directory-files path 'full
                     directory-files-no-dot-files-regexp)))

(defun neo-util--hidden-path-filter (node)
  (if (not neo-buffer--show-hidden-file-p)
      (not (string-match neo-hidden-files-regexp
                         (neo-path--file-short-name node)))
    node))

(defun neo-str--trim-left (s)
  "Remove whitespace at the beginning of S."
  (if (string-match "\\`[ \t\n\r]+" s)
      (replace-match "" t t s)
    s))

(defun neo-str--trim-right (s)
  "Remove whitespace at the end of S."
  (if (string-match "[ \t\n\r]+\\'" s)
      (replace-match "" t t s)
    s))

(defun neo-str--trim (s)
  "Remove whitespace at the beginning and end of S."
  (neo-str--trim-left (neo-str--trim-right s)))

(defun neo-path--expand-name (path &optional current-dir)
  (or (if (file-name-absolute-p path) path)
      (let ((r-path path))
        (setq r-path (substitute-in-file-name r-path))
        (setq r-path (expand-file-name r-path current-dir))
        r-path)))

(defun neo-path--shorten (path length)
  "Shorten a given PATH to a specified LENGTH.
This is needed for paths, which are to long for the window to display
completely.  The function cuts of the first part of the path to remain
the last folder (the current one)."
    (if (> (string-width path) length)
	(concat "<" (substring path (- (- length 1))))
      path))

(defun neo-path--insert-chroot-button (label path face)
  (insert-button
   label
   'action '(lambda (x) (neotree-change-root))
   'follow-link t
   'face face
   'neo-full-path path))

(defun neo-path--insert-header-buttonized (path)
  "Shortens the PATH to (window-body-width) and displays any \
visible remains as buttons that, when clicked, navigate to that
parent directory."
  (let* ((dirs (reverse (maplist 'identity (reverse (split-string path "/" :omitnulls)))))
         (last (car-safe (car-safe (last dirs)))))
    (neo-path--insert-chroot-button "/" "/" 'neo-root-dir-face)
    (dolist (dir dirs)
      (if (string= (car dir) last)
        (neo-buffer--insert-with-face last 'neo-root-dir-face)
        (neo-path--insert-chroot-button
         (concat (car dir) "/")
         (apply 'neo-path--join (cons "/" (reverse dir)))
         'neo-root-dir-face))))
  ;;shorten the line if need be
  (when (> (current-column) (window-body-width))
    (forward-char (- (window-body-width)))
    (delete-region (point-at-bol) (point))
    (let* ((button (button-at (point)))
           (path (if button (overlay-get button 'neo-full-path) "/")))
      (neo-path--insert-chroot-button "<" path 'neo-root-dir-face))
    (end-of-line)))

(defun neo-path--updir (path)
  (let ((r-path (neo-path--expand-name path)))
    (if (and (> (length r-path) 0)
             (equal (substring r-path -1) "/"))
        (setq r-path (substring r-path 0 -1)))
    (if (eq (length r-path) 0)
        (setq r-path "/"))
    (directory-file-name
     (file-name-directory r-path))))

(defun neo-path--join (root &rest dirs)
  "Joins a series of directories together with ROOT and DIRS.
Like Python's os.path.join,
  (neo-path--join \"/tmp\" \"a\" \"b\" \"c\") => /tmp/a/b/c ."
  (or (if (not dirs) root)
      (let ((tdir (car dirs))
            (epath nil))
        (setq epath
              (or (if (equal tdir ".") root)
                  (if (equal tdir "..") (neo-path--updir root))
                  (neo-path--expand-name tdir root)))
        (apply 'neo-path--join
               epath
               (cdr dirs)))))

(defun neo-path--file-short-name (file)
  "Base file/directory name by FILE.
Taken from http://lists.gnu.org/archive/html/emacs-devel/2011-01/msg01238.html"
  (or (if (string= file "/") "/")
      (neo-util--make-printable-string (file-name-nondirectory (directory-file-name file)))))

(defun neo-path--file-truename (path)
  (let ((rlt (file-truename path)))
    (if (not (null rlt))
        (progn
          (if (and (file-directory-p rlt)
                   (> (length rlt) 0)
                   (not (equal (substring rlt -1) "/")))
              (setq rlt (concat rlt "/")))
          rlt)
      nil)))

(defun neo-path--has-subfile-p (dir)
  "To determine whether a directory(DIR) contain files."
  (and (file-exists-p dir)
       (file-directory-p dir)
       (neo-util--walk-dir dir)
       t))

(defun neo-path--match-path-directory (path)
  (let ((true-path (neo-path--file-truename path))
        (rlt-path nil))
    (setq rlt-path
          (catch 'rlt
            (if (file-directory-p true-path)
                (throw 'rlt true-path))
            (setq true-path
                  (file-name-directory true-path))
            (if (file-directory-p true-path)
                (throw 'rlt true-path))))
    (if (not (null rlt-path))
        (setq rlt-path (neo-path--join "." rlt-path "./")))
    rlt-path))

(defun neo-path--get-working-dir ()
  "Return a directory name of the current buffer."
  (file-name-as-directory (file-truename default-directory)))

(defun neo-path--strip (path)
  "Remove whitespace at the end of PATH."
  (let* ((rlt (neo-str--trim path))
         (pos (string-match "[\\\\/]+\\'" rlt)))
    (when pos
      (setq rlt (replace-match "" t t rlt))
      (when (eq (length rlt) 0)
        (setq rlt "/")))
    rlt))

(defun neo-path--file-equal-p (file1 file2)
  "Return non-nil if files FILE1 and FILE2 name the same file.
If FILE1 or FILE2 does not exist, the return value is unspecified."
  (let ((nfile1 (neo-path--strip file1))
        (nfile2 (neo-path--strip file2)))
    (file-equal-p nfile1 nfile2)))

(defun neo-path--file-in-directory-p (file dir)
  "Return non-nil if FILE is in DIR or a subdirectory of DIR.
A directory is considered to be \"in\" itself.
Return nil if DIR is not an existing directory."
  (let ((nfile (neo-path--strip file))
        (ndir (neo-path--strip dir)))
    (setq ndir (concat ndir "/"))
    (file-in-directory-p nfile ndir)))

(defun neo-util--kill-buffers-for-path (path)
  "Kill all buffers for files in PATH."
  (let ((buffer (find-buffer-visiting path)))
    (when buffer
      (kill-buffer buffer)))
  (dolist (filename (directory-files path t directory-files-no-dot-files-regexp))
    (let ((buffer (find-buffer-visiting filename)))
      (when buffer
        (kill-buffer buffer))
      (when (and
             (file-directory-p filename)
             (neo-path--has-subfile-p filename))
        (neo-util--kill-buffers-for-path filename)))))

(defun neo-point-auto-indent ()
  "Put the point on the first letter of the current node."
  (when (neo-buffer--get-filename-current-line)
    (beginning-of-line 1)
    (re-search-forward "[^-\s+]" (line-end-position 1) t)
    (backward-char 1)))

;;
;; Buffer methods
;;

(defun neo-buffer--newline-and-begin ()
  (newline)
  (beginning-of-line))

(defun neo-buffer--save-cursor-pos (&optional node-path line-pos)
  "Save cursor position."
  (let ((cur-node-path nil)
        (cur-line-pos nil)
        (ws-wind (selected-window))
        (ws-pos (window-start)))
    (setq cur-node-path (if node-path
                            node-path
                          (neo-buffer--get-filename-current-line)))
    (setq cur-line-pos (if line-pos
                           line-pos
                         (line-number-at-pos)))
    (setq neo-buffer--cursor-pos (cons cur-node-path cur-line-pos))
    (setq neo-buffer--last-window-pos (cons ws-wind ws-pos))))

(defun neo-buffer--goto-cursor-pos ()
  "Jump to saved cursor position."
  (let ((line-pos nil)
        (node (car neo-buffer--cursor-pos))
        (line-pos (cdr neo-buffer--cursor-pos))
        (ws-wind (car neo-buffer--last-window-pos))
        (ws-pos (cdr neo-buffer--last-window-pos)))
    (catch 'line-pos-founded
      (unless (null node)
        (setq line-pos 0)
        (mapc
         (lambda (x)
           (setq line-pos (1+ line-pos))
           (when (and (not (null x))
                      (not (null node))
                      (neo-path--file-equal-p x node))
             (throw 'line-pos-founded line-pos)))
         neo-buffer--node-list))
      (setq line-pos (cdr neo-buffer--cursor-pos))
      (throw 'line-pos-founded line-pos))
    ;; goto line
    (goto-char (point-min))
    (forward-line (1- line-pos))
    ;; scroll window
    (when (equal (selected-window) ws-wind)
      (set-window-start ws-wind ws-pos t))))

(defun neo-buffer--node-list-clear ()
  "Clear node list."
  (setq neo-buffer--node-list nil))

(defun neo-buffer--node-list-set (line-num path)
  "Set value in node list.
LINE-NUM is the index of node list.
PATH is value."
  (let ((node-list-length (length neo-buffer--node-list))
        (node-index line-num))
    (when (null node-index)
      (setq node-index (line-number-at-pos)))
    (when (< node-list-length node-index)
      (setq neo-buffer--node-list
            (vconcat neo-buffer--node-list
                     (make-vector (- node-index node-list-length) nil))))
    (aset neo-buffer--node-list (1- node-index) path))
  neo-buffer--node-list)

(defun neo-buffer--insert-with-face (content face)
  (let ((pos-start (point)))
    (insert content)
    (set-text-properties pos-start
                         (point)
                         (list 'face face))))

(defun neo-buffer--valid-start-node-p ()
  (and (not (null neo-buffer--start-node))
       (file-accessible-directory-p neo-buffer--start-node)))

(defun neo-buffer--create ()
  "Create and switch a NeoTree buffer."
  (setq neo-global--buffer
        (switch-to-buffer
         (generate-new-buffer-name neo-buffer-name)))
  (neotree-mode)
  ;; disable linum-mode
  (if (and (boundp 'linum-mode)
           (not (null linum-mode)))
      (linum-mode -1))
  neo-global--buffer)

(defun neo-buffer--insert-banner ()
  (unless (null neo-banner-message)
    (let ((start (point)))
      (insert neo-banner-message)
      (set-text-properties start (point) '(face neo-banner-face)))
    (neo-buffer--newline-and-begin)))

(defun neo-buffer--insert-root-entry (node)
  (when (or neo-banner-message neo-show-updir-line)
    (neo-buffer--newline-and-begin))
  (when neo-show-updir-line
    (insert-button ".."
                   'action '(lambda (x) (neotree-change-root))
                   'follow-link t
                   'face neo-file-link-face
                   'neo-full-path (neo-path--updir node))
    (let ((start (point)))
      (insert " (up a dir)")
      (set-text-properties start (point) '(face neo-header-face)))
    (neo-buffer--newline-and-begin))
  (neo-buffer--node-list-set nil node)
  (cond
   ((eq neo-cwd-line-style 'button)
    (neo-path--insert-header-buttonized node))
   (t
    (neo-buffer--insert-with-face (neo-path--shorten node (window-body-width))
                                  'neo-root-dir-face)))
  (neo-buffer--newline-and-begin))

(defun neo-buffer--insert-dir-entry (node depth expanded)
  (let ((btn-start-pos nil)
        (btn-end-pos nil)
        (node-short-name (neo-path--file-short-name node)))
    (insert-char ?\s (* (- depth 1) 2)) ; indent
    (setq btn-start-pos (point))
    (neo-buffer--insert-with-face (if expanded "-" "+")
                          'neo-expand-btn-face)
    (neo-buffer--insert-with-face (concat node-short-name "/")
                          'neo-dir-link-face)
    (setq btn-end-pos (point))
    (make-button btn-start-pos
                 btn-end-pos
                 'action '(lambda (_) (neotree-enter current-prefix-arg))
                 'follow-link t
                 'face neo-button-face
                 'neo-full-path node)
    (neo-buffer--node-list-set nil node)
    (neo-buffer--newline-and-begin)))

(defun neo-buffer--insert-file-entry (node depth)
  (let ((node-short-name (neo-path--file-short-name node)))
    (insert-char ?\s (* (- depth 1) 2)) ; indent
    (insert-button node-short-name
                   'action '(lambda (_) (neotree-enter current-prefix-arg))
                   'follow-link t
                   'face neo-file-link-face
                   'neo-full-path node)
    (neo-buffer--node-list-set nil node)
    (neo-buffer--newline-and-begin)))

(defun neo-buffer--get-nodes (path)
  (let* ((nodes (neo-util--walk-dir path))
         (comp  #'(lambda (x y)
                    (string< x y)))
         (nodes (neo-util--filter 'neo-util--hidden-path-filter nodes)))
    (cons (sort (neo-util--filter 'file-directory-p nodes) comp)
          (sort (neo-util--filter #'(lambda (f) (not (file-directory-p f))) nodes) comp))))

(defun neo-buffer--get-node-index (node nodes)
  "Return the index of NODE in NODES.

NODES can be a list of directory or files.
Return nil if NODE has not been found in NODES."
  (let ((i 0)
        (l (length nodes))
        (cur (car nodes))
        (rest (cdr nodes)))
    (while (and cur (not (equal cur node)))
      (setq i (1+ i))
      (setq cur (car rest))
      (setq rest (cdr rest)))
    (if (< i l) i)))

(defun neo-buffer--expanded-node-p (node)
  (if (neo-util--find neo-buffer--expanded-node-list
                #'(lambda (x) (equal x node)))
      t nil))

(defun neo-buffer--set-expand (node do-expand)
  "Set the expanded state of the NODE to DO-EXPAND.
Return the new expand state for NODE."
  (if (not do-expand)
      (setq neo-buffer--expanded-node-list
            (neo-util--filter
             #'(lambda (x) (not (equal node x)))
             neo-buffer--expanded-node-list))
    (push node neo-buffer--expanded-node-list))
  do-expand)

(defun neo-buffer--toggle-expand (node)
  (neo-buffer--set-expand node (not (neo-buffer--expanded-node-p node))))

(defun neo-buffer--insert-tree (path depth)
  (if (eq depth 1)
      (neo-buffer--insert-root-entry path))
  (let* ((contents (neo-buffer--get-nodes path))
         (nodes (car contents))
         (leafs (cdr contents)))
    (dolist (node nodes)
      (let ((expanded (neo-buffer--expanded-node-p node)))
        (neo-buffer--insert-dir-entry
         node depth expanded)
        (if expanded (neo-buffer--insert-tree (concat node "/") (+ depth 1)))))
    (dolist (leaf leafs)
      (neo-buffer--insert-file-entry leaf depth))))

(defun neo-buffer--refresh (save-pos)
  "Refresh the NeoTree buffer.
If SAVE-POS is non-nil, it will be auto save current line number."
  (let ((start-node neo-buffer--start-node))
    (neo-buffer--save-excursion
     ;; save context
     (when save-pos
       (neo-buffer--save-cursor-pos))
     ;; starting refresh
     (erase-buffer)
     (neo-buffer--node-list-clear)
     (neo-buffer--insert-banner)
     (setq neo-buffer--start-line neo-header-height)
     (neo-buffer--insert-tree start-node 1))
    ;; restore context
    (neo-buffer--goto-cursor-pos)))

(defun neo-buffer--get-button-current-line ()
  "Return the first button in current line."
  (let* ((btn-position nil)
         (pos-line-start (line-beginning-position))
         (pos-line-end (line-end-position))
         ;; NOTE: cannot find button when the button
         ;;       at beginning of the line
         (current-button (or (button-at (point))
                             (button-at pos-line-start))))
    (if (null current-button)
        (progn
          (setf btn-position
                (catch 'ret-button
                  (let* ((next-button (next-button pos-line-start))
                         (pos-btn nil))
                    (if (null next-button) (throw 'ret-button nil))
                    (setf pos-btn (overlay-start next-button))
                    (if (> pos-btn pos-line-end) (throw 'ret-button nil))
                    (throw 'ret-button pos-btn))))
          (if (null btn-position)
              nil
            (setf current-button (button-at btn-position)))))
    current-button))

(defun neo-buffer--get-filename-current-line (&optional default)
  "Return filename for first button in current line.
If there is no button in current line, then return DEFAULT."
  (let ((btn (neo-buffer--get-button-current-line)))
    (if (not (null btn))
        (button-get btn 'neo-full-path)
      default)))

(defun neo-buffer--lock-width ()
  "Lock the width size for NeoTree window."
  (setq window-size-fixed 'width))

(defun neo-buffer--unlock-width ()
  "Unlock the width size for NeoTree window."
  (setq window-size-fixed nil))

(defun neo-buffer--rename-node ()
  "Rename current node as another path."
  (interactive)
  (let* ((current-path (neo-buffer--get-filename-current-line))
         (buffer (find-buffer-visiting current-path))
         to-path
         msg)
    (unless (null current-path)
      (setq msg (format "Rename [%s] to: " (neo-path--file-short-name current-path)))
      (setq to-path (read-file-name msg (file-name-directory current-path)))
      (if buffer
          (with-current-buffer buffer
            (set-visited-file-name to-path nil t)))
      (rename-file current-path to-path)
      (neo-buffer--refresh t)
      (message "Rename successed."))))

(defun neo-buffer--select-file-node (file &optional recursive-p)
  "Select the node that corresponds to the FILE.
If RECURSIVE-P is non nil, find files will recursively."
  (let ((efile file)
        (iter-curr-dir nil)
        (file-node-find-p nil)
        (file-node-list nil))
    (unless (file-name-absolute-p efile)
      (setq efile (expand-file-name efile)))
    (setq iter-curr-dir efile)
    (catch 'return
      (while t
        (setq iter-curr-dir (neo-path--updir iter-curr-dir))
        (push iter-curr-dir file-node-list)
        (when (neo-path--file-equal-p iter-curr-dir neo-buffer--start-node)
          (setq file-node-find-p t)
          (throw 'return nil))
        (when (neo-path--file-equal-p iter-curr-dir "/")
          (setq file-node-find-p nil)
          (throw 'return nil))))
    (when file-node-find-p
      (dolist (p file-node-list)
        (neo-buffer--set-expand p t))
      (neo-buffer--save-cursor-pos file)
      (neo-buffer--refresh nil))))

(defun neo-buffer--change-root (root-dir)
  "Change the tree root to ROOT-DIR."
  (let ((path root-dir)
        start-path)
    (unless (and (file-exists-p path)
                 (file-directory-p path))
      (throw 'error "The path is not a valid directory."))
    (setq start-path (expand-file-name (substitute-in-file-name path)))
    (setq neo-buffer--start-node start-path)
    (cd start-path)
    (neo-buffer--save-cursor-pos path nil)
    (neo-buffer--refresh nil)))

(defun neo-buffer--get-nodes-for-select-down-node (path)
  "Return the node list for the down dir selection."
  (if path
      (when (file-name-directory path)
        (if (neo-buffer--expanded-node-p path)
            (neo-buffer--get-nodes path)
          (neo-buffer--get-nodes (file-name-directory path))))
    (neo-buffer--get-nodes (file-name-as-directory neo-buffer--start-node))))

(defun neo-buffer--get-nodes-for-sibling (path)
  "Return the node list for the sibling selection. Return nil of no nodes can
be found.
The returned list is a directory list if path is a directory, otherwise it is
a file list."
  (when path
    (let ((nodes (neo-buffer--get-nodes (file-name-directory path))))
      (if (file-directory-p path)
          (car nodes)
        (cdr nodes)))))

(defun neo-buffer--sibling (path &optional previous)
  "Return the next sibling of node PATH.
If PREVIOUS is non-nil the previous sibling is returned."
  (let* ((nodes (neo-buffer--get-nodes-for-sibling path)))
    (when nodes
      (let ((i (neo-buffer--get-node-index path nodes))
            (l (length nodes)))
        (if i (nth (mod (+ i (if previous -1 1)) l) nodes))))))

;;
;; Window methods
;;

(defun neo-window--init (window buffer)
  "Make WINDOW a NeoTree window.
NeoTree buffer is BUFFER."
  (neo-global--with-buffer
    (neo-buffer--unlock-width))
  (switch-to-buffer buffer)
  (neo-window--set-width window neo-window-width)
  (set-window-dedicated-p window t)
  (neo-global--with-buffer
    (neo-buffer--lock-width))
  window)

(defun neo-window--set-width (window n)
  "Make neotree widnow(WINDOW) N columns width."
  (let ((w (max n window-min-width)))
    (unless (null window)
      (if (> (window-width) w)
          (shrink-window-horizontally (- (window-width) w))
        (if (< (window-width) w)
            (enlarge-window-horizontally (- w (window-width))))))))

(defun neo-window--zoom (method)
  "Zoom the NeoTree window, the METHOD should one of these options:
'maximize 'minimize 'zoom-in 'zoom-out."
  (neo-buffer--unlock-width)
  (cond
   ((eq method 'maximize)
    (maximize-window))
   ((eq method 'minimize)
    (neo-window--set-width (selected-window) neo-window-width))
   ((eq method 'zoom-in)
    (shrink-window-horizontally 2))
   ((eq method 'zoom-out)
    (enlarge-window-horizontally 2)))
  (neo-buffer--lock-width))

(defun neo-window--minimize-p ()
  "Return non-nil when the NeoTree window is minimize."
  (<= (window-width) neo-window-width))

(defun neo-buffer--set-show-hidden-file-p (show-p)
  "If SHOW-P is non-nil, show hidden nodes in tree."
  (setq neo-buffer--show-hidden-file-p show-p)
  (neo-buffer--refresh t))


;;
;; Interactive functions
;;

;;;###autoload
(defun neotree-find (&optional path default-path)
  "Quick select node which specified PATH in NeoTree.
If path is nil and no buffer file name, then use DEFAULT-PATH,"
  (interactive)
  (let* ((ndefault-path (if default-path default-path
                          (neo-path--get-working-dir)))
         (npath (if path path
                  (or (buffer-file-name) ndefault-path)))
         (do-open-p nil))
    (if (and (not (neo-global--file-in-root-p npath))
             (neo-global--window-exists-p))
        (setq do-open-p (yes-or-no-p "File not found in root path, do you want to change root?"))
      (setq do-open-p t))
    (when do-open-p
        (neo-global--open-and-find npath))
    (when neo-auto-indent-point
      (neo-point-auto-indent)))
  (neo-global--select-window))

(defun neotree-previous-node ()
  "Jump to the previous node."
  (interactive)
  (backward-button 1 nil))

(defun neotree-next-node ()
  "Jump to the next node."
  (interactive)
  (forward-button 1 nil))

(defun neotree-click-changes-root-toggle ()
  "Toggle the variable neo-click-changes-root.
If true, clicking on a directory will change the current root to
the directory instead of showing the directory contents."
  (interactive)
  (setq neo-click-changes-root (not neo-click-changes-root)))

(defun neotree-enter (&optional arg)
  "Open a node, like 'o' in NERDTree.

If ARG is an integer then the node is opened in a window selected via
`window-numbering' (if available) according to the passed number.
If ARG is `|' then the node is opened in new vertically split window.
If ARG is `-' then the node is opened in new horizontally split window."
  (interactive "P")
  (let ((btn-full-path (neo-buffer--get-filename-current-line)))
    (unless (null btn-full-path)
      (if (file-directory-p btn-full-path)
          (if neo-click-changes-root
              (neotree-change-root)
            (progn
              (let ((new-state (neo-buffer--toggle-expand btn-full-path)))
                (neo-buffer--refresh t)
                (when neo-auto-indent-point
                  (when new-state (next-line))
                  (neo-point-auto-indent)))))
        (progn
          (if (eq (safe-length (window-list)) 1)
              (neo-global--with-buffer
                (neo-buffer--unlock-width)
                (split-window-horizontally)
                (neo-buffer--lock-width)))
          (neo-global--when-window
            (neo-window--zoom 'minimize))
          ;; select target window
          (if arg (cond
                   ;; select window with window numbering
                   ((and (integerp arg)
                         (boundp 'window-numbering-mode)
                         (symbol-value window-numbering-mode)
                         (fboundp 'select-window-by-number))
                    (select-window-by-number arg))
                   ;; open node in a new vertically split window
                   ((and (stringp arg) (string= arg "|"))
                    (select-window (get-mru-window))
                    (split-window-right)
                    (windmove-right))
                   ;; open node in a new horizontally split window
                   ((and (stringp arg) (string= arg "-"))
                    (select-window (get-mru-window))
                    (split-window-below)
                    (windmove-down)))
            ;; open node in last active window
            (select-window (get-mru-window)))
          (find-file btn-full-path))))
    btn-full-path))

(defun neotree-enter-vertical-split ()
  "Open the current node is a vertically split window."
  (interactive)
  (message "vertical")
  (neotree-enter "|"))

(defun neotree-enter-horizontal-split ()
  "Open the current node is a horizontally split window."
  (interactive)
  (neotree-enter "-"))

(defun neotree-change-root ()
  "Change root to current node dir.
If current node is a file, then it will do nothing.
If cannot find any node in current line, it equivalent to using `neotree-dir'."
  (interactive)
  (neo-global--select-window)
  (let ((btn-full-path (neo-buffer--get-filename-current-line)))
    (if (null btn-full-path)
        (call-interactively 'neotree-dir)
      (neo-global--open-dir btn-full-path))))

(defun neotree-select-up-node ()
  "Select the parent directory of the current node. Change the root if
necessary. "
  (interactive)
  (neo-global--select-window)
  (let* ((btn-full-path (neo-buffer--get-filename-current-line))
         (btn-parent-dir (if btn-full-path (file-name-directory btn-full-path)))
         (root-slash (file-name-as-directory neo-buffer--start-node)))
    (cond
     ((equal btn-parent-dir root-slash) (neo-global--open-dir root-slash))
     (btn-parent-dir (neotree-find btn-parent-dir))
     (t (neo-global--open-dir (file-name-directory
                               (directory-file-name root-slash)))))))

(defun neotree-select-down-node ()
  "Select an expanded directory or content directory according to the
current node, in this order:
- select the first expanded child node if the current node has one
- select the content of current node if it is expanded
- select the next expanded sibling if the current node is not expanded."
  (interactive)
  (let* ((btn-full-path (neo-buffer--get-filename-current-line))
         (path (if btn-full-path btn-full-path neo-buffer--start-node))
         (nodes (neo-buffer--get-nodes-for-select-down-node path)))
    (when nodes
      (if (or (equal path neo-buffer--start-node)
              (neo-buffer--expanded-node-p path))
          ;; select the first expanded child node
          (let ((expanded-dir (catch 'break
                                (dolist (node (car nodes))
                                  (if (neo-buffer--expanded-node-p node)
                                      (throw 'break node)))
                                nil)))
            (if expanded-dir
                (neotree-find expanded-dir)
              ;; select the directory content if needed
              (let ((dirs (car nodes))
                    (files (cdr nodes)))
                (if (> (length dirs) 0)
                    (neotree-find (car dirs))
                  (when (> (length files) 0)
                    (neotree-find (car files)))))))
        ;; select the next expanded sibling
        (let ((sibling (neo-buffer--sibling path)))
          (while (and (not (neo-buffer--expanded-node-p sibling))
                      (not (equal sibling path)))
            (setq sibling (neo-buffer--sibling sibling)))
          (when (not (string< sibling path))
            ;; select next expanded sibling
            (neotree-find sibling)))))))

(defun neotree-select-next-sibling-node ()
  "Select the next sibling of current node.
If the current node is the last node then the first node is selected."
  (interactive)
  (let ((sibling (neo-buffer--sibling (neo-buffer--get-filename-current-line))))
    (when sibling (neotree-find sibling))))

(defun neotree-select-previous-sibling-node ()
  "Select the previous sibling of current node.
If the current node is the first node then the last node is selected."
  (interactive)
  (let ((sibling (neo-buffer--sibling (neo-buffer--get-filename-current-line) t)))
    (when sibling (neotree-find sibling))))

(defun neotree-create-node (filename)
  "Create a file or directory use specified FILENAME in current node."
  (interactive
   (let* ((current-dir (neo-buffer--get-filename-current-line neo-buffer--start-node))
          (current-dir (neo-path--match-path-directory current-dir))
          (filename (read-file-name "Filename:" current-dir)))
     (if (file-directory-p filename)
         (setq filename (concat filename "/")))
     (list filename)))
  (catch 'rlt
    (let ((is-file nil))
      (when (= (length filename) 0)
        (throw 'rlt nil))
      (setq is-file (not (equal (substring filename -1) "/")))
      (when (file-exists-p filename)
        (message "File %S already exists." filename)
        (throw 'rlt nil))
      (when (and is-file
                 (yes-or-no-p (format "Do you want to create file %S ?"
                                      filename)))
        ;; NOTE: create a empty file
        (write-region "" nil filename)
        (neo-buffer--save-cursor-pos filename)
        (neo-buffer--refresh nil)
        (if neo-create-file-auto-open
            (find-file-other-window filename)))
      (when (and (not is-file)
                 (yes-or-no-p (format "Do you want to create directory %S?"
                                      filename)))
        (mkdir filename)
        (neo-buffer--save-cursor-pos filename)
        (neo-buffer--refresh nil)))))

(defun neotree-delete-node ()
  "Delete current node."
  (interactive)
  (let* ((filename (neo-buffer--get-filename-current-line))
         (buffer (find-buffer-visiting filename))
         (deleted-p nil))
    (catch 'end
      (if (null filename) (throw 'end nil))
      (if (not (file-exists-p filename)) (throw 'end nil))
      (if (not (yes-or-no-p (format "Do you really want to delete %S?"
                                    filename)))
          (throw 'end nil))
      (if (file-directory-p filename)
          ;; delete directory
          (progn
            (unless (neo-path--has-subfile-p filename)
              (delete-directory filename)
              (setq deleted-p t)
              (throw 'end nil))
            (when (yes-or-no-p
                   (format "%S is a directory, delete it recursively?"
                           filename))
              (when (yes-or-no-p
                     (format "kill buffers for files in directory %S?"
                             filename))
                (neo-util--kill-buffers-for-path filename))
              (delete-directory filename t)
              (setq deleted-p t)))
        ;; delete file
        (progn
          (delete-file filename)
          (when buffer
            (kill-buffer-ask buffer))
          (setq deleted-p t))))
    (when deleted-p
      (message "%S deleted." filename)
      (neo-buffer--refresh t))
    filename))

(defun neotree-rename-node ()
  "Rename current node."
  (interactive)
  (neo-buffer--rename-node))

(defun neotree-hidden-file-toggle ()
  "Toggle show hidden files."
  (interactive)
  (neo-buffer--set-show-hidden-file-p (not neo-buffer--show-hidden-file-p)))

(defun neotree-empty-fn ()
  "Used to bind the empty function to the shortcut."
  (interactive))

(defun neotree-refresh ()
  "Refresh the NeoTree buffer."
  (interactive)
  (neo-buffer--refresh t))

(defun neotree-stretch-toggle ()
  "Make the NeoTree window toggle maximize/minimize."
  (interactive)
  (neo-global--with-window
   (if (neo-window--minimize-p)
       (neo-window--zoom 'maximize)
     (neo-window--zoom 'minimize))))

;;;###autoload
(defun neotree-projectile-action ()
  "Integration with `Projectile'.

Usage:
    (setq projectile-switch-project-action 'neotree-projectile-action).

When running `projectile-switch-project' (C-c p p), `neotree' will change root
automatically."
  (interactive)
  (cond
   ((fboundp 'projectile-project-root)
    (neotree-dir (projectile-project-root)))
   (t
    (error "Projectile is not available"))))

;;;###autoload
(defun neotree-toggle ()
  "Toggle show the NeoTree window."
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (neotree-show)))

;;;###autoload
(defun neotree-show ()
  "Show the NeoTree widnow."
  (interactive)
  (if neo-smart-open
      (neotree-find)
    (neo-global--open))
  (neo-global--select-window))

;;;###autoload
(defun neotree-hide ()
  "Close the NeoTree window."
  (interactive)
  (if (neo-global--window-exists-p)
      (delete-window neo-global--window)))

;;;###autoload
(defun neotree-dir (path)
  "Show the NeoTree window, and change root to PATH."
  (interactive "DDirectory: ")
  (neo-global--open-dir path)
  (neo-global--select-window))

;;;###autoload
(defalias 'neotree 'neotree-show "Show the NeoTree window.")


(provide 'neotree)
;;; neotree.el ends here
