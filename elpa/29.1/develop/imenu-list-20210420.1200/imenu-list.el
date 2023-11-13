;;; imenu-list.el --- Show imenu entries in a separate buffer

;; Copyright (C) 2015-2021 Bar Magal & Contributors

;; Author: Bar Magal (2015)
;; Version: 0.9
;; Homepage: https://github.com/bmag/imenu-list
;; Package-Requires: ((emacs "24.3"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
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
;; Shows a list of imenu entries for the current buffer, in another
;; buffer with the name "*Ilist*".
;;
;; Activation and deactivation:
;; M-x imenu-list-minor-mode
;;
;; Key shortcuts from "*Ilist*" buffer:
;; <enter>: Go to current definition
;; <space>: display current definition
;; <tab>: expand/collapse subtree
;;
;; Change "*Ilist*" buffer's position and size:
;; `imenu-list-position', `imenu-list-size'.
;;
;; Should invoking `imenu-list-minor-mode' also select the "*Ilist*"
;; window?
;; `imenu-list-focus-after-activation'

;;; Code:

(require 'imenu)
(require 'cl-lib)
(require 'hideshow)

(defconst imenu-list-buffer-name "*Ilist*"
  "Name of the buffer that is used to display imenu entries.")

(defvar imenu-list--imenu-entries nil
  "Currently used imenu entires.
This is a copy of the imenu entries of the buffer we want to
display in the imenu-list buffer.")

(defvar imenu-list--line-entries nil
  "List of imenu entries displayed in the imenu-list buffer.
The first item in this list corresponds to the first line in the
imenu-list buffer, the second item matches the second line, and so on.")

(defvar imenu-list--displayed-buffer nil
  "The buffer who owns the saved imenu entries.")

(defvar imenu-list--last-location nil
  "Location from which last `imenu-list-update' was done.
Used to avoid updating if the point didn't move.")

;;; fancy display

(defgroup imenu-list nil
  "Variables for `imenu-list' package."
  :group 'imenu)

(defcustom imenu-list-persist-when-imenu-index-unavailable t
  "Whether or not to keep the old index if the new index is missing.
This option controls whether imenu-list will persist the entries
of the last current buffer during an attempt to update it from a
buffer that has no Imenu index.  Some users find this behavior
convenient for jumping back and forth between different buffers
when paired with window-purpose's x-code-1 configuration.

If you kill buffers often, set this to nil so x-code-1 will clear
the entries when focusing on a buffer that does not have an Imenu
index."
  :group 'imenu-list
  :type 'boolean)

(defcustom imenu-list-mode-line-format
  '("%e" mode-line-front-space mode-line-mule-info mode-line-client
    mode-line-modified mode-line-remote mode-line-frame-identification
    (:propertize "%b" face mode-line-buffer-id) " "
    (:eval (buffer-name imenu-list--displayed-buffer)) " "
    mode-line-end-spaces)
  "Local mode-line format for the imenu-list buffer.
This is the local value of `mode-line-format' to use in the imenu-list
buffer.  See `mode-line-format' for allowed values."
  :group 'imenu-list
  :type 'sexp)

(defcustom imenu-list-focus-after-activation nil
  "Whether or not to select imenu-list window after activation.
Non-nil to select the imenu-list window automatically when
`imenu-list-minor-mode' is activated."
  :group 'imenu-list
  :type 'boolean)

(defcustom imenu-list-update-current-entry t
  "Whether or not `imenu-list-update' shows the current entry.
If non-nil, imenu-list shows the current entry on the menu
automatically during update."
  :group 'imenu-list
  :type 'boolean)

(defcustom imenu-list-custom-position-translator nil
  "Custom translator of imenu positions to buffer positions.
Imenu can be customized on a per-buffer basis not to use regular buffer
positions as the positions that are stored in the imenu index.  In such
cases, imenu-list needs to know how to translate imenu positions back to
buffer positions.  `imenu-list-custom-position-translator' should be a
function that returns a position-translator function suitable for the
current buffer, or nil.  See `imenu-list-position-translator' for details."
  :group 'imenu-list
  :type 'function)

(defface imenu-list-entry-face
  '((t))
  "Basic face for imenu-list entries in the imenu-list buffer."
  :group 'imenu-list)

(defface imenu-list-entry-face-0
  '((((class color) (background light))
     :inherit imenu-list-entry-face
     :foreground "maroon")
    (((class color) (background dark))
     :inherit imenu-list-entry-face
     :foreground "gold"))
  "Face for outermost imenu-list entries (depth 0)."
  :group 'imenu-list)

(defface imenu-list-entry-subalist-face-0
  '((t :inherit imenu-list-entry-face-0
       :weight bold :underline t))
  "Face for subalist entries with depth 0."
  :group 'imenu-list)

(defface imenu-list-entry-face-1
  '((((class color) (background light))
     :inherit imenu-list-entry-face
     :foreground "dark green")
    (((class color) (background dark))
     :inherit imenu-list-entry-face
     :foreground "light green"))
  "Face for imenu-list entries with depth 1."
  :group 'imenu-list)

(defface imenu-list-entry-subalist-face-1
  '((t :inherit imenu-list-entry-face-1
       :weight bold :underline t))
  "Face for subalist entries with depth 1."
  :group 'imenu-list)

(defface imenu-list-entry-face-2
  '((((class color) (background light))
     :inherit imenu-list-entry-face
     :foreground "dark blue")
    (((class color) (background dark))
     :inherit imenu-list-entry-face
     :foreground "light blue"))
  "Face for imenu-list entries with depth 2."
  :group 'imenu-list)

(defface imenu-list-entry-subalist-face-2
  '((t :inherit imenu-list-entry-face-2
       :weight bold :underline t))
  "Face for subalist entries with depth 2."
  :group 'imenu-list)

(defface imenu-list-entry-face-3
  '((((class color) (background light))
     :inherit imenu-list-entry-face
     :foreground "orange red")
    (((class color) (background dark))
     :inherit imenu-list-entry-face
     :foreground "sandy brown"))
  "Face for imenu-list entries with depth 3."
  :group 'imenu-list)

(defface imenu-list-entry-subalist-face-3
  '((t :inherit imenu-list-entry-face-3
       :weight bold :underline t))
  "Face for subalist entries with depth 0."
  :group 'imenu-list)

(defun imenu-list--get-face (depth subalistp)
  "Get face for entry.
DEPTH is the depth of the entry in the list.
SUBALISTP non-nil means that there are more entries \"under\" the
current entry (current entry is a \"father\")."
  (cl-case depth
    (0 (if subalistp 'imenu-list-entry-subalist-face-0 'imenu-list-entry-face-0))
    (1 (if subalistp 'imenu-list-entry-subalist-face-1 'imenu-list-entry-face-1))
    (2 (if subalistp 'imenu-list-entry-subalist-face-2 'imenu-list-entry-face-2))
    (3 (if subalistp 'imenu-list-entry-subalist-face-3 'imenu-list-entry-face-3))
    (t (if subalistp 'imenu-list-entry-subalist-face-3 'imenu-list-entry-face-3))))

;;; collect entries

(defun imenu-list-rescan-imenu ()
  "Force imenu to rescan the current buffer."
  (setq imenu--index-alist nil)
  (imenu--make-index-alist))

(defun imenu-list-collect-entries ()
  "Collect all `imenu' entries of the current buffer."
  (imenu-list-rescan-imenu)
  (setq imenu-list--imenu-entries imenu--index-alist)
  (setq imenu-list--displayed-buffer (current-buffer)))


;;; print entries

(defun imenu-list--depth-string (depth)
  "Return a prefix string representing an entry's DEPTH."
  (let ((indents (cl-loop for i from 1 to depth collect "  ")))
    (format "%s%s"
            (mapconcat #'identity indents "")
            (if indents " " ""))))

(defun imenu-list--action-goto-entry (event)
  "Goto the entry that was clicked.
EVENT holds the data of what was clicked."
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event)))
        (ilist-buffer (get-buffer imenu-list-buffer-name)))
    (when (and (windowp window)
               (eql (window-buffer window) ilist-buffer))
      (with-current-buffer ilist-buffer
        (goto-char pos)
        (imenu-list-goto-entry)))))

(defun imenu-list--action-toggle-hs (event)
  "Toggle hide/show state of current block.
EVENT holds the data of what was clicked.
See `hs-minor-mode' for information on what is hide/show."
  (let ((window (posn-window (event-end event)))
        (pos (posn-point (event-end event)))
        (ilist-buffer (get-buffer imenu-list-buffer-name)))
    (when (and (windowp window)
               (eql (window-buffer window) ilist-buffer))
      (with-current-buffer ilist-buffer
        (goto-char pos)
        (hs-toggle-hiding)))))

(defun imenu-list--insert-entry (entry depth)
  "Insert a line for ENTRY with DEPTH."
  (if (imenu--subalist-p entry)
      (progn
        (insert (imenu-list--depth-string depth))
        (insert-button (format "+ %s" (car entry))
                       'face (imenu-list--get-face depth t)
                       'help-echo (format "Toggle: %s"
                                          (car entry))
                       'follow-link t
                       'action ;; #'imenu-list--action-goto-entry
                       #'imenu-list--action-toggle-hs)
        (insert "\n"))
    (insert (imenu-list--depth-string depth))
    (insert-button (format "%s" (car entry))
                   'face (imenu-list--get-face depth nil)
                   'help-echo (format "Go to: %s"
                                      (car entry))
                   'follow-link t
                   'action #'imenu-list--action-goto-entry)
    (insert "\n")))

(defun imenu-list--insert-entries-internal (index-alist depth)
  "Insert all imenu entries in INDEX-ALIST into the current buffer.
DEPTH is the depth of the code block were the entries are written.
Each entry is inserted in its own line.
Each entry is appended to `imenu-list--line-entries' as well."
  (dolist (entry index-alist)
    (setq imenu-list--line-entries (cons entry imenu-list--line-entries))
    (imenu-list--insert-entry entry depth)
    (when (imenu--subalist-p entry)
      (imenu-list--insert-entries-internal (cdr entry) (1+ depth)))))

(defun imenu-list-insert-entries ()
  "Insert all imenu entries into the current buffer.
The entries are taken from `imenu-list--imenu-entries'.
Each entry is inserted in its own line.
Each entry is appended to `imenu-list--line-entries' as well
 (`imenu-list--line-entries' is cleared in the beginning of this
function)."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (setq imenu-list--line-entries nil)
    (imenu-list--insert-entries-internal imenu-list--imenu-entries 0)
    (setq imenu-list--line-entries (nreverse imenu-list--line-entries))))


;;; goto entries

(defcustom imenu-list-after-jump-hook '(recenter)
  "Hook to run after jumping to an entry from the imenu-list buffer.
This hook is ran also when the focus remains on the imenu-list
buffer, or in other words: this hook is ran by both
`imenu-list-goto-entry' and `imenu-list-display-entry'."
  :group 'imenu-list
  :type 'hook)

(defun imenu-list--find-entry ()
  "Find in `imenu-list--line-entries' the entry in the current line."
  (nth (1- (line-number-at-pos)) imenu-list--line-entries))

(defun imenu-list--goto-entry (entry)
  "Jump to ENTRY in the original buffer."
  (pop-to-buffer imenu-list--displayed-buffer)
  (imenu entry)
  (run-hooks 'imenu-list-after-jump-hook)
  (imenu-list--show-current-entry))

(defun imenu-list-goto-entry ()
  "Switch to the original buffer and display the entry under point."
  (interactive)
  (imenu-list--goto-entry (imenu-list--find-entry)))

(defun imenu-list-display-entry ()
  "Display the symbol under `point' in the original buffer."
  (interactive)
  (save-selected-window
    (imenu-list-goto-entry)))

(defun imenu-list-ret-dwim ()
  "Jump to or toggle the entry at `point'."
  (interactive)
  (let ((entry (imenu-list--find-entry)))
    (if (imenu--subalist-p entry)
        (hs-toggle-hiding)
      (imenu-list--goto-entry entry))))

(defun imenu-list-display-dwim ()
  "Display or toggle the entry at `point'."
  (interactive)
  (save-selected-window
    (imenu-list-ret-dwim)))

(defalias 'imenu-list-<=
  (if (ignore-errors (<= 1 2 3))
      #'<=
    #'(lambda (x y z)
        "Return t if X <= Y and Y <= Z."
        (and (<= x y) (<= y z)))))

;; hide false-positive byte-compile warning. We only use these functions if
;; eglot is loaded.
(declare-function eglot--lsp-position-to-point "eglot")
(declare-function eglot-managed-p "eglot")

(defun imenu-list--translate-eglot-position (pos)
  "Get real position of position object POS created by eglot."
  ;; when Eglot is in charge of Imenu, then the index is created by `eglot-imenu', with a fallback to
  ;; `imenu-default-create-index-function' when `eglot-imenu' returns nil. If POS is an array, it means
  ;; it was created by `eglot-imenu' and we need to extract its position. Otherwise, it was created by
  ;; `imenu-default-create-index-function' and we should return it as-is.
  (if (arrayp pos)
      (eglot--lsp-position-to-point (plist-get (plist-get (aref pos 0) :range) :start) t)
    pos))

(defun imenu-list-position-translator ()
  "Get the correct position translator function for the current buffer.
A position translator is a function that takes a position as described in
`imenu--index-alist' and returns a number or marker that points to the
real position in the buffer that the position parameter points to.
This is necessary because positions in `imenu--index-alist' do not have to
be numbers or markers, although usually they are.  For example,
`semantic-create-imenu-index' uses overlays as position paramters.
If `imenu-list-custom-position-translator' is non-nil, then
`imenu-list-position-translator' asks it for a translator function.
If `imenu-list-custom-position-translator' is called and returns nil, then
continue with the regular logic to find a translator function."
  (cond
   ((and imenu-list-custom-position-translator
         (funcall imenu-list-custom-position-translator)))
   ((or (eq imenu-create-index-function 'semantic-create-imenu-index)
        (and (eq imenu-create-index-function
                 'spacemacs/python-imenu-create-index-python-or-semantic)
             (bound-and-true-p semantic-mode)))
    ;; semantic uses overlays, return overlay's start as position
    #'overlay-start)
   ((and (fboundp #'eglot-managed-p) (eglot-managed-p))
    #'imenu-list--translate-eglot-position)
   ;; default - return position as is
   (t #'identity)))

(defun imenu-list--current-entry ()
  "Find entry in `imenu-list--line-entries' matching current position."
  (let ((point-pos (point-marker))
        (offset (point-min-marker))
        (get-pos-fn (imenu-list-position-translator))
        match-entry)
    (dolist (entry imenu-list--line-entries match-entry)
      ;; "special entry" is described in `imenu--index-alist'
      (unless (imenu--subalist-p entry)
        (let* ((is-special-entry (listp (cdr entry)))
               (entry-pos-raw (if is-special-entry
                                  (cadr entry)
                                (cdr entry)))
               ;; sometimes imenu doesn't use numbers/markers as positions, so we
               ;; need to translate them back to "real" positions
               ;; (see https://github.com/bmag/imenu-list/issues/20)
               (entry-pos (funcall get-pos-fn entry-pos-raw)))
          (when (imenu-list-<= offset entry-pos point-pos)
            (setq offset entry-pos)
            (setq match-entry entry)))))))

(defun imenu-list--show-current-entry ()
  "Move the imenu-list buffer's point to the current position's entry."
  (when (get-buffer-window (imenu-list-get-buffer-create))
    (let ((line-number (cl-position (imenu-list--current-entry)
                                    imenu-list--line-entries
                                    :test 'equal)))
      (with-selected-window (get-buffer-window (imenu-list-get-buffer-create))
        (goto-char (point-min))
        (forward-line line-number)
        (hl-line-mode 1)))))

;;; window display settings

(defcustom imenu-list-size 0.3
  "Size (height or width) for the imenu-list buffer.
Either a positive integer (number of rows/columns) or a percentage."
  :group 'imenu-list
  :type 'number)

(defcustom imenu-list-position 'right
  "Position of the imenu-list buffer.
Either 'right, 'left, 'above or 'below.  This value is passed
directly to `split-window'."
  :group 'imenu-list
  :type '(choice (const above)
                 (const below)
                 (const left)
                 (const right)))

(defcustom imenu-list-auto-resize nil
  "If non-nil, auto-resize window after updating the imenu-list buffer.
Resizing the width works only for Emacs 24.4 and newer.  Resizing the
height doesn't suffer that limitation."
  :group 'imenu-list
  :type 'boolean)

(defcustom imenu-list-update-hook nil
  "Hook to run after updating the imenu-list buffer."
  :group 'imenu-list
  :type 'hook)

(defun imenu-list-split-size ()
  "Convert `imenu-list-size' to proper argument for `split-window'."
  (let ((frame-size (if (member imenu-list-position '(left right))
                        (frame-width)
                      (frame-height))))
    (cond ((integerp imenu-list-size) (- imenu-list-size))
          (t (- (round (* frame-size imenu-list-size)))))))

(defun imenu-list-display-buffer (buffer alist)
  "Display the imenu-list buffer at the side.
This function should be used with `display-buffer-alist'.
See `display-buffer-alist' for a description of BUFFER and ALIST."
  (or (get-buffer-window buffer)
      (let ((window (ignore-errors (split-window (frame-root-window) (imenu-list-split-size) imenu-list-position))))
        (when window
          ;; since Emacs 27.0.50, `window--display-buffer' doesn't take a
          ;; `dedicated' argument, so instead call `set-window-dedicated-p'
          ;; directly (works both on new and old Emacs versions)
          (window--display-buffer buffer window 'window alist)
          (set-window-dedicated-p window t)
          window))))

(defun imenu-list-install-display-buffer ()
  "Install imenu-list display settings to `display-buffer-alist'."
  (cl-pushnew `(,(concat "^" (regexp-quote imenu-list-buffer-name) "$")
                imenu-list-display-buffer)
              display-buffer-alist
              :test #'equal))

(defun imenu-list-purpose-display-condition (_purpose buffer _alist)
  "Display condition for use with window-purpose.
Return t if BUFFER is the imenu-list buffer.

This function should be used in `purpose-special-action-sequences'.
See `purpose-special-action-sequences' for a description of _PURPOSE,
BUFFER and _ALIST."
  (string-equal (buffer-name buffer) imenu-list-buffer-name))

;; hide false-positive byte-compile warning
(defvar purpose-special-action-sequences)

(defun imenu-list-install-purpose-display ()
  "Install imenu-list display settings for window-purpose.
Install entry for imenu-list in `purpose-special-action-sequences'."
  (cl-pushnew '(imenu-list-purpose-display-condition imenu-list-display-buffer)
              purpose-special-action-sequences
              :test #'equal))

(imenu-list-install-display-buffer)
(eval-after-load 'window-purpose
  '(imenu-list-install-purpose-display))


;;; define major mode

(defun imenu-list-get-buffer-create ()
  "Return the imenu-list buffer.
If it doesn't exist, create it."
  (or (get-buffer imenu-list-buffer-name)
      (let ((buffer (get-buffer-create imenu-list-buffer-name)))
        (with-current-buffer buffer
          (imenu-list-major-mode)
          buffer))))

(defun imenu-list-resize-window ()
  "Resize imenu-list window according to its content."
  (when imenu-list--line-entries
    (let ((fit-window-to-buffer-horizontally t))
      (mapc #'fit-window-to-buffer
            (get-buffer-window-list (imenu-list-get-buffer-create))))))

(defun imenu-list-update (&optional force-update)
  "Update the imenu-list buffer.
If the imenu-list buffer doesn't exist, create it.
If FORCE-UPDATE is non-nil, the imenu-list buffer is updated even if the
imenu entries did not change since the last update."
  (catch 'index-failure
    (let ((old-entries imenu-list--imenu-entries)
          (location (point-marker)))
      ;; don't update if `point' didn't move - fixes issue #11
      (unless (and (null force-update)
                   imenu-list--last-location
                   (marker-buffer imenu-list--last-location)
                   (= location imenu-list--last-location))
        (setq imenu-list--last-location location)
        (condition-case err
            (imenu-list-collect-entries)
          (imenu-unavailable (if imenu-list-persist-when-imenu-index-unavailable
                                 (throw 'index-failure nil)
                               (imenu-list-clear))))
        (when (or force-update
                  ;; check if Ilist buffer is alive, in case it was killed
                  ;; since last update
                  (null (get-buffer imenu-list-buffer-name))
                  (not (equal old-entries imenu-list--imenu-entries)))
          (with-current-buffer (imenu-list-get-buffer-create)
            (imenu-list-insert-entries)))
        (when imenu-list-update-current-entry
          (imenu-list--show-current-entry))
        (when imenu-list-auto-resize
          (imenu-list-resize-window))
        (run-hooks 'imenu-list-update-hook)
        nil))))

(defun imenu-list-clear ()
  "Clear the imenu-list buffer."
  (let ((imenu-buffer (get-buffer imenu-list-buffer-name)))
    (when imenu-buffer
      (setq imenu-list--imenu-entries nil
            imenu-list--line-entries nil)
      (with-current-buffer imenu-buffer
        (let ((inhibit-read-only t))
          (erase-buffer))))))

(defun imenu-list-refresh ()
  "Refresh imenu-list buffer."
  (interactive)
  (with-current-buffer imenu-list--displayed-buffer
    (imenu-list-update t)))

(defun imenu-list-show ()
  "Show the imenu-list buffer.
If the imenu-list buffer doesn't exist, create it."
  (interactive)
  (pop-to-buffer imenu-list-buffer-name))

(defun imenu-list-show-noselect ()
  "Show the imenu-list buffer, but don't select it.
If the imenu-list buffer doesn't exist, create it."
  (interactive)
  (display-buffer imenu-list-buffer-name))

;;;###autoload
(defun imenu-list-noselect ()
  "Update and show the imenu-list buffer, but don't select it.
If the imenu-list buffer doesn't exist, create it."
  (interactive)
  (imenu-list-update)
  (imenu-list-show-noselect))

;;;###autoload
(defun imenu-list ()
  "Update and show the imenu-list buffer.
If the imenu-list buffer doesn't exist, create it."
  (interactive)
  (imenu-list-update)
  (imenu-list-show))

;; hide false-positive byte-compile warning
(defvar imenu-list-minor-mode)

(defun imenu-list-quit-window ()
  "Disable `imenu-list-minor-mode' and hide the imenu-list buffer.
If `imenu-list-minor-mode' is already disabled, just call `quit-window'."
  (interactive)
  ;; the reason not to call `(imenu-list-minor-mode -1)' regardless of current
  ;; state, is that it quits all of imenu-list windows instead of just the
  ;; current one.
  (if imenu-list-minor-mode
      ;; disabling `imenu-list-minor-mode' also quits the window
      (imenu-list-minor-mode -1)
    (quit-window)))

(defvar imenu-list-major-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'imenu-list-ret-dwim)
    (define-key map (kbd "SPC") #'imenu-list-display-dwim)
    (define-key map (kbd "n") #'next-line)
    (define-key map (kbd "p") #'previous-line)
    (define-key map (kbd "TAB") #'hs-toggle-hiding)
    (define-key map (kbd "f") #'hs-toggle-hiding)
    (define-key map (kbd "g") #'imenu-list-refresh)
    (define-key map (kbd "q") #'imenu-list-quit-window)
    map))

(define-derived-mode imenu-list-major-mode special-mode "Ilist"
  "Major mode for showing the `imenu' entries of a buffer (an Ilist).
\\{imenu-list-mode-map}"
  (read-only-mode 1)
  (imenu-list-install-hideshow))
(add-hook 'imenu-list-major-mode-hook #'hs-minor-mode)

(defun imenu-list--set-mode-line ()
  "Locally change `mode-line-format' to `imenu-list-mode-line-format'."
  (setq-local mode-line-format imenu-list-mode-line-format))
(add-hook 'imenu-list-major-mode-hook #'imenu-list--set-mode-line)

(defun imenu-list-install-hideshow ()
  "Install imenu-list settings for hideshow."
  ;; "\\b\\B" is a regexp that can't match anything
  (setq-local comment-start "\\b\\B")
  (setq-local comment-end "\\b\\B")
  (setq hs-special-modes-alist
        (cl-delete 'imenu-list-major-mode hs-special-modes-alist :key #'car))
  (push `(imenu-list-major-mode "\\s-*\\+ " "\\s-*\\+ " ,comment-start imenu-list-forward-sexp nil)
        hs-special-modes-alist))

(defun imenu-list-forward-sexp (&optional arg)
  "Move to next entry of same depth.
This function is intended to be used by `hs-minor-mode'.  Don't use it
for anything else.
ARG is ignored."
  (beginning-of-line)
  (while (= (char-after) 32)
    (forward-char))
  ;; (when (= (char-after) ?+)
  ;;   (forward-char 2))
  (let ((spaces (- (point) (point-at-bol))))
    (forward-line)
    ;; ignore-errors in case we're at the last line
    (ignore-errors (forward-char spaces))
    (while (and (not (eobp))
                (= (char-after) 32))
      (forward-line)
      ;; ignore-errors in case we're at the last line
      (ignore-errors (forward-char spaces))))
  (forward-line -1)
  (end-of-line))

;;; define minor mode

(defvar imenu-list--timer nil)

(defcustom imenu-list-idle-update-delay idle-update-delay
  "Idle time delay before automatically updating the imenu-list buffer."
  :group 'imenu-list
  :type 'number
  :initialize 'custom-initialize-default
  :set (lambda (sym val)
         (prog1 (set-default sym val)
           (when imenu-list--timer (imenu-list-start-timer)))))

(defun imenu-list-start-timer ()
  "Start timer to auto-update imenu-list index and window."
  (imenu-list-stop-timer)
  (setq imenu-list--timer
        (run-with-idle-timer imenu-list-idle-update-delay t
                             #'imenu-list-update)))

(defun imenu-list-stop-timer ()
  "Stop timer to auto-update imenu-list index and window."
  (when imenu-list--timer
    (cancel-timer imenu-list--timer)
    (setq imenu-list--timer nil)))

(defcustom imenu-list-auto-update t
  "Whether imenu-list should automatically update its index.
If non-nil, imenu-list automatically updates the entries of its
index every `imenu-list-idle-update-delay' seconds.  When
updating this value from Lisp code, you should call
`imenu-list-start-timer' or `imenu-list-stop-timer' explicitly
afterwards."
  :group 'imenu-list
  :type 'boolean
  :set (lambda (sym val)
         (prog1 (set-default sym val)
           (if (and val (bound-and-true-p imenu-list-mode))
               (imenu-list-start-timer)
             (imenu-list-stop-timer)))))

(define-obsolete-function-alias 'imenu-list-update-safe 'imenu-list-update "imenu-list 0.10")

;;;###autoload
(define-minor-mode imenu-list-minor-mode
  nil :global t :group 'imenu-list
  (if imenu-list-minor-mode
      (progn
        (imenu-list-get-buffer-create)
        (when imenu-list-auto-update
          (imenu-list-start-timer))
        (let ((orig-buffer (current-buffer)))
          (if imenu-list-focus-after-activation
              (imenu-list-show)
            (imenu-list-show-noselect))
          (with-current-buffer orig-buffer
            (imenu-list-update t))))
    (imenu-list-stop-timer)
    (ignore-errors (quit-windows-on imenu-list-buffer-name))
    ;; make sure *Ilist* is buried even if it wasn't shown in any window
    (when (get-buffer imenu-list-buffer-name)
      (bury-buffer (get-buffer imenu-list-buffer-name)))))

;;;###autoload
(defun imenu-list-smart-toggle ()
  "Enable or disable `imenu-list-minor-mode' according to buffer's visibility.
If the imenu-list buffer is displayed in any window, disable
`imenu-list-minor-mode', otherwise enable it.
Note that all the windows in every frame searched, even invisible ones, not
only those in the selected frame."
  (interactive)
  (if (get-buffer-window imenu-list-buffer-name t)
      (imenu-list-minor-mode -1)
    (imenu-list-minor-mode 1)))

(provide 'imenu-list)

;;; imenu-list.el ends here
