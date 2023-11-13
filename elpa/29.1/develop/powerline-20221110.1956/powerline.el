;;; powerline.el --- Rewrite of Powerline

;; Copyright (C) 2012-2013 Donald Ephraim Curtis
;; Copyright (C) 2013 Jason Milkins
;; Copyright (C) 2012 Nicolas Rougier

;; Author: Donald Ephraim Curtis <dcurtis@milkbox.net>
;; URL: http://github.com/milkypostman/powerline/
;; Version: 2.5
;; Keywords: mode-line
;; Package-Requires: ((cl-lib "0.2"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Powerline is a library for customizing the mode-line that is based on the Vim
;; Powerline. A collection of predefined themes comes with the package.
;;

;;; Code:

(eval-and-compile (require 'powerline-themes))
(eval-and-compile (require 'powerline-separators))

(require 'cl-lib)

(defgroup powerline nil
  "Powerline, a prettier mode line."
  :group 'mode-line)

(defface powerline-active0 '((t (:inherit mode-line)))
  "Powerline face 0."
  :group 'powerline)

(defface powerline-active1 '((t (:background "grey17" :foreground "white" :inherit mode-line)))
  "Powerline face 1."
  :group 'powerline)

(defface powerline-active2 '((t (:background "grey40" :foreground "white" :inherit mode-line)))
  "Powerline face 2."
  :group 'powerline)

(defface powerline-inactive0
  '((t (:inherit mode-line-inactive)))
  "Powerline face 0."
  :group 'powerline)

(defface powerline-inactive1
  '((t (:background "grey11" :inherit mode-line-inactive)))
  "Powerline face 1."
  :group 'powerline)

(defface powerline-inactive2
  '((t (:background "grey20" :inherit mode-line-inactive)))
  "Powerline face 2."
  :group 'powerline)

(defface mode-line-buffer-id-inactive
  '((t (:inherit mode-line-buffer-id)))
  "Powerline mode-line face"
  :group 'powerline)

(defcustom powerline-default-separator 'arrow
  "The separator to use for the default theme.

Valid Values: alternate, arrow, arrow-fade, bar, box, brace,
butt, chamfer, contour, curve, rounded, roundstub, wave, zigzag,
slant, utf-8."
  :group 'powerline
  :type '(choice (const alternate)
                 (const arrow)
                 (const arrow-fade)
                 (const bar)
                 (const box)
                 (const brace)
                 (const butt)
                 (const chamfer)
                 (const contour)
                 (const curve)
                 (const rounded)
                 (const roundstub)
                 (const slant)
                 (const smooth-slant)
                 (const wave)
                 (const zigzag)
                 (const utf-8)
                 (const nil)))

(defcustom powerline-utf-8-separator-left #xe0b0
  "The unicode character number for the left facing separator"
  :group 'powerline
  :type  '(choice integer (const nil)))

(defcustom powerline-utf-8-separator-right #xe0b2
  "The unicode character number for the right facing separator"
  :group 'powerline
  :type  '(choice integer (const nil)))

(defcustom powerline-default-separator-dir '(left . right)
  "The separator direction to use for the default theme.

CONS of the form (DIR . DIR) denoting the lean of the
separators for the left and right side of the powerline.

DIR must be one of: left, right"
  :group 'powerline
  :type '(cons (choice :tag "Left Hand Side" (const left) (const right))
               (choice :tag "Right Hand Side" (const left) (const right))))

(defcustom powerline-height nil
  "Override the mode-line height."
  :group 'powerline
  :type '(choice integer (const nil)))

(defcustom powerline-text-scale-factor nil
  "Scale of mode-line font size to default text size.

Smaller mode-line fonts will be a float value less that 1.
Larger mode-line fonts require a float value greater than 1.

This is needed to make sure that text is properly aligned."
  :group 'powerline
  :type '(choice float integer (const nil)))

(defcustom powerline-buffer-size-suffix t
  "Display the buffer size suffix."
  :group 'powerline
  :type 'boolean)

(defcustom powerline-gui-use-vcs-glyph nil
  "Display a unicode character to represent a version control system.
Not always supported in GUI."
  :group 'powerline
  :type 'boolean)

(defcustom powerline-narrowed-indicator "Narrow"
  "A string to display in the mode-line when the buffer is narrowed."
  :group 'powerline
  :type 'string)

(defun pl/create-or-get-cache ()
  "Return a frame-local hash table that acts as a memoization cache for powerline.
Create one if the frame doesn't have one yet."
  (let ((table (frame-parameter nil 'powerline-cache)))
    (if (hash-table-p table) table (pl/reset-cache))))

(defun pl/reset-cache ()
  "Reset and return the frame-local hash table used for a memoization cache."
  (let ((table (make-hash-table :test 'equal)))
    ;; Store it as a frame-local variable
    (modify-frame-parameters nil `((powerline-cache . ,table)))
    table))

(defun powerline-current-separator ()
  "Get the current default separator. Always returns utf-8 in non-gui mode."
  (if window-system
      powerline-default-separator
    'utf-8))

;;
;; the frame-local powerline cache causes problems if included in a saved desktop,
;; so delete it before the desktop is saved.
;;
;; see https://github.com/milkypostman/powerline/issues/58
;;
;; It is better to put the following code into your init file for Emacs 24.4 or later.
;; (require 'frameset)
;; (push '(powerline-cache . :never) frameset-filter-alist)
;;
(defun powerline-delete-cache (&optional frame)
  "Set the FRAME cache to nil."
  (set-frame-parameter frame 'powerline-cache nil))

(defun powerline-desktop-save-delete-cache ()
  "Set all caches to nil.
This is not done if `frameset-filter-alist' has :never for powerline-cache."
  (unless (and (boundp 'frameset-filter-alist)
               (eq (cdr (assq 'powerline-cache frameset-filter-alist))
                   :never))
    (dolist (fr (frame-list)) (powerline-delete-cache fr))))

(add-hook 'desktop-save-hook 'powerline-desktop-save-delete-cache)

;; from memoize.el @ http://nullprogram.com/blog/2010/07/26/
(defun pl/memoize (func)
  "Memoize FUNC.
If argument is a symbol then install the memoized function over
the original function.  Use frame-local memoization."
  (cl-typecase func
    (symbol (fset func (pl/memoize-wrap-frame-local (symbol-function func))) func)
    (function (pl/memoize-wrap-frame-local func))))

(defun pl/memoize-wrap-frame-local (func)
  "Return the memoized version of FUNC.
The memoization cache is frame-local."
  (let ((funcid (cl-gensym)))
    `(lambda (&rest args)
       ,(concat (documentation func) (format "\n(memoized function %s)" funcid))
       (let* ((cache (pl/create-or-get-cache))
              (key (cons ',funcid args))
              (val (gethash key cache)))
         (if val
             val
           (puthash key (apply ,func args) cache))))))

(defun pl/separator-height ()
  "Get default height for rendering separators."
  (or powerline-height (frame-char-height)))

(defun powerline-reset ()
  "Reset memoized functions."
  (interactive)
  (pl/memoize (pl/alternate left))
  (pl/memoize (pl/alternate right))
  (pl/memoize (pl/arrow left))
  (pl/memoize (pl/arrow right))
  (pl/memoize (pl/arrow-fade left))
  (pl/memoize (pl/arrow-fade right))
  (pl/memoize (pl/bar left))
  (pl/memoize (pl/bar right))
  (pl/memoize (pl/box left))
  (pl/memoize (pl/box right))
  (pl/memoize (pl/brace left))
  (pl/memoize (pl/brace right))
  (pl/memoize (pl/butt left))
  (pl/memoize (pl/butt right))
  (pl/memoize (pl/chamfer left))
  (pl/memoize (pl/chamfer right))
  (pl/memoize (pl/contour left))
  (pl/memoize (pl/contour right))
  (pl/memoize (pl/curve left))
  (pl/memoize (pl/curve right))
  (pl/memoize (pl/rounded left))
  (pl/memoize (pl/rounded right))
  (pl/memoize (pl/roundstub left))
  (pl/memoize (pl/roundstub right))
  (pl/memoize (pl/slant left))
  (pl/memoize (pl/slant right))
  (pl/memoize (pl/smooth-slant left))
  (pl/memoize (pl/smooth-slant right))
  (pl/memoize (pl/wave left))
  (pl/memoize (pl/wave right))
  (pl/memoize (pl/zigzag left))
  (pl/memoize (pl/zigzag right))
  (pl/memoize (pl/nil left))
  (pl/memoize (pl/nil right))
  (pl/utf-8 left)
  (pl/utf-8 right)
  (pl/reset-cache))

(powerline-reset)

(defun pl/make-xpm (name color1 color2 data)
  "Return an XPM image with NAME using COLOR1 and COLOR2 bits specified in DATA.
COLOR1 signifies enabled, and COLOR2 signifies disabled."
  (when window-system
    (create-image
     (concat
      (format "/* XPM */
static char * %s[] = {
\"%i %i 2 1\",
\". c %s\",
\"  c %s\",
"
              (downcase (replace-regexp-in-string " " "_" name))
              (length (car data))
              (length data)
              (or (pl/hex-color color1) "None")
              (or (pl/hex-color color2) "None"))
      (let ((len  (length data))
            (idx  0))
        (apply 'concat
               (mapcar #'(lambda (dl)
                           (setq idx (+ idx 1))
                           (concat
                            "\""
                            (concat
                             (mapcar #'(lambda (d)
                                         (if (eq d 0)
                                             (string-to-char " ")
                                           (string-to-char ".")))
                                     dl))
                            (if (eq idx len)
                                "\"};"
                              "\",\n")))
                       data))))
     'xpm t :scale 1 :ascent 'center)))

(defun pl/percent-xpm
    (height pmax pmin winend winstart width color1 color2)
  "Generate percentage xpm of HEIGHT for PMAX to PMIN given WINEND and WINSTART.
Use WIDTH and COLOR1 and COLOR2."
  (let* ((height- (1- height))
         (fillstart (round (* height- (/ (float winstart) (float pmax)))))
         (fillend (round (* height- (/ (float winend) (float pmax)))))
         (data nil)
         (i 0))
    (while (< i height)
      (setq data (cons
                  (if (and (<= fillstart i)
                           (<= i fillend))
                      (append (make-list width 1))
                    (append (make-list width 0)))
                  data))
      (setq i (+ i 1)))
    (pl/make-xpm "percent" color1 color2 (reverse data))))

(pl/memoize 'pl/percent-xpm)

;;;###autoload
(defun powerline-hud (face1 face2 &optional width)
  "Return XPM of relative buffer location using FACE1 and FACE2 of optional WIDTH."
  (unless width (setq width 2))
  (let ((color1 (if face1 (face-background face1) "None"))
        (color2 (if face2 (face-background face2) "None"))
        (height (or powerline-height (frame-char-height)))
        pmax
        pmin
        (ws (window-start))
        (we (window-end)))
    (save-restriction
      (widen)
      (setq pmax (point-max))
      (setq pmin (point-min)))
    (pl/percent-xpm height pmax pmin we ws
                    (* (frame-char-width) width) color1 color2)))

;;;###autoload
(defun powerline-mouse (click-group click-type string)
  "Return mouse handler for CLICK-GROUP given CLICK-TYPE and STRING."
  (cond ((eq click-group 'minor)
         (cond ((eq click-type 'menu)
                `(lambda (event)
                   (interactive "@e")
                   (minor-mode-menu-from-indicator ,string)))
               ((eq click-type 'help)
                `(lambda (event)
                   (interactive "@e")
                   (describe-minor-mode-from-indicator ,string)))
               (t
                `(lambda (event)
                   (interactive "@e")
                   nil))))
        (t
         `(lambda (event)
            (interactive "@e")
            nil))))

;;;###autoload
(defun powerline-concat (&rest strings)
  "Concatonate STRINGS and pad sides by spaces."
  (concat
   " "
   (mapconcat 'identity (delq nil strings) " ")
   " "))

;;;###autoload
(defmacro defpowerline (name body)
  "Create function NAME by wrapping BODY with powerline padding an propetization."
  `(defun ,name
       (&optional face pad)
     (powerline-raw ,body face pad)))

(defun pl/property-substrings (str prop)
  "Return a list of substrings of STR when PROP change."
  (let ((beg 0) (end 0)
        (len (length str))
        (out))
    (while (< end (length str))
      (setq end (or (next-single-property-change beg prop str) len))
      (setq out (append out (list (substring str beg (setq beg end))))))
    out))

(defun pl/assure-list (item)
  "Assure that ITEM is a list."
  (if (listp item)
      item
    (list item)))

(defun pl/add-text-property (str prop val)
  (mapconcat
   (lambda (mm)
     (let ((cur (pl/assure-list (get-text-property 0 'face mm))))
       (propertize mm 'face (append cur (list val)))))
   (pl/property-substrings str prop)
   ""))

;;;###autoload
(defun powerline-raw (str &optional face pad)
  "Render STR as mode-line data using FACE and optionally PAD import.
PAD can be left (`l') or right (`r')."
  (when str
    (let* ((rendered-str (format-mode-line str))
           (padded-str (concat
                        (when (and (> (length rendered-str) 0) (eq pad 'l)) " ")
                        (if (listp str) rendered-str str)
                        (when (and (> (length rendered-str) 0) (eq pad 'r)) " "))))

      (if face
          (pl/add-text-property padded-str 'face face)
        padded-str))))

;;;###autoload
(defun powerline-fill (face reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when powerline-text-scale-factor
    (setq reserve (* powerline-text-scale-factor reserve)))
  (when (and window-system (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))
              'face face))

(defun powerline-fill-center (face reserve)
  "Return empty space using FACE to center of remaining space.
Leave RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when powerline-text-scale-factor
    (setq reserve (* powerline-text-scale-factor reserve)))
  (propertize " "
              'display `((space :align-to (- (+ center (.5 . right-margin)) ,reserve
                                             (.5 . left-margin))))
              'face face))

;;;###autoload (autoload 'powerline-major-mode "powerline")
(defpowerline powerline-major-mode
  (propertize (format-mode-line mode-name)
              'mouse-face 'mode-line-highlight
              'help-echo "Major mode\n\ mouse-1: Display major mode menu\n\ mouse-2: Show help for major mode\n\ mouse-3: Toggle minor modes"
              'local-map (let ((map (make-sparse-keymap)))
                           (define-key map [mode-line down-mouse-1]
                             `(menu-item ,(purecopy "Menu Bar") ignore
                                         :filter (lambda (_) (mouse-menu-major-mode-map))))
                           (define-key map [mode-line mouse-2] 'describe-mode)
                           (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
                           map)))

;;;###autoload (autoload 'powerline-minor-modes "powerline")
(defpowerline powerline-minor-modes
  (mapconcat (lambda (mm)
               (propertize mm
                           'mouse-face 'mode-line-highlight
                           'help-echo "Minor mode\n mouse-1: Display minor mode menu\n mouse-2: Show help for minor mode\n mouse-3: Toggle minor modes"
                           'local-map (let ((map (make-sparse-keymap)))
                                        (define-key map
                                          [mode-line down-mouse-1]
                                          (powerline-mouse 'minor 'menu mm))
                                        (define-key map
                                          [mode-line mouse-2]
                                          (powerline-mouse 'minor 'help mm))
                                        (define-key map
                                          [mode-line down-mouse-3]
                                          (powerline-mouse 'minor 'menu mm))
                                        (define-key map
                                          [header-line down-mouse-3]
                                          (powerline-mouse 'minor 'menu mm))
                                        map)))
             (split-string (format-mode-line minor-mode-alist))
             (propertize " " 'face face)))

;;;###autoload (autoload 'powerline-narrow "powerline")
(defpowerline powerline-narrow
  (when ;; (buffer-narrowed-p) introduced in Emacs 24.3.
      (/= (- (point-max) (point-min)) (buffer-size))
    (propertize powerline-narrowed-indicator
                'mouse-face 'mode-line-highlight
                'help-echo "mouse-1: Remove narrowing from the current buffer"
                'local-map (make-mode-line-mouse-map
                            'mouse-1 'mode-line-widen))))

;;;###autoload (autoload 'powerline-vc "powerline")
(defpowerline powerline-vc
  (when (and (buffer-file-name (current-buffer)) vc-mode)
    (if (and window-system (not powerline-gui-use-vcs-glyph))
        (format-mode-line '(vc-mode vc-mode))
      (format " %s%s"
              (char-to-string #xe0a0)
              (format-mode-line '(vc-mode vc-mode))))))

;;;###autoload (autoload 'powerline-encoding "powerline")
(defpowerline powerline-encoding
  (let ((buf-coding (format "%s" buffer-file-coding-system)))
    (if (string-match "\\(dos\\|unix\\|mac\\)" buf-coding)
        (match-string 1 buf-coding)
      buf-coding)))


;;;###autoload (autoload 'powerline-buffer-size "powerline")
(defpowerline powerline-buffer-size
  (propertize
   (if powerline-buffer-size-suffix
       "%I"
     "%i")
   'mouse-face 'mode-line-highlight
   'local-map (make-mode-line-mouse-map
               'mouse-1 (lambda () (interactive)
                          (setq powerline-buffer-size-suffix
                                (not powerline-buffer-size-suffix))
                          (force-mode-line-update)))))

;;;###autoload (autoload 'powerline-buffer-id "powerline")
(defun powerline-buffer-id (&optional face pad)
  (powerline-raw
   '(" " (:propertize
          mode-line-buffer-identification
          'face face
          'mouse-face 'mode-line-highlight
          'help-echo "Buffer name\n\ mouse-1: Previous buffer\n\ mouse-3: Next buffer"
          'local-map (let ((map (make-sparse-keymap)))
                       (define-key map [mode-line mouse-1] 'mode-line-previous-buffer)
                       (define-key map [mode-line mouse-3] 'mode-line-next-buffer)
                       map)))
   face pad))

;;;###autoload (autoload 'powerline-process "powerline")
(defpowerline powerline-process
  (cond
   ((symbolp mode-line-process) (symbol-value mode-line-process))
   ((listp mode-line-process) (format-mode-line mode-line-process))
   (t mode-line-process)))

(defvar pl/default-mode-line mode-line-format)

(defvar pl/minibuffer-selected-window-list '())

(defun pl/minibuffer-selected-window ()
  "Return the selected window when entereing the minibuffer."
  (when pl/minibuffer-selected-window-list
    (car pl/minibuffer-selected-window-list)))

(defun pl/minibuffer-setup ()
  "Save the `minibuffer-selected-window' to `pl/minibuffer-selected-window'."
  (push (minibuffer-selected-window) pl/minibuffer-selected-window-list))

(add-hook 'minibuffer-setup-hook 'pl/minibuffer-setup)

(defun pl/minibuffer-exit ()
  "Set `pl/minibuffer-selected-window' to nil."
  (pop pl/minibuffer-selected-window-list))

(add-hook 'minibuffer-exit-hook 'pl/minibuffer-exit)

(defvar powerline-selected-window (frame-selected-window)
  "Selected window.")

(defun powerline-set-selected-window ()
  "Set the variable `powerline-selected-window' appropriately."
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq powerline-selected-window (frame-selected-window))
    (force-mode-line-update)))

(defun powerline-unset-selected-window ()
  "Unset the variable `powerline-selected-window' and update the mode line."
  (setq powerline-selected-window nil)
  (force-mode-line-update))

(add-hook 'window-configuration-change-hook 'powerline-set-selected-window)

;; Watch focus changes
(if (boundp 'after-focus-change-function)
  (add-function :after after-focus-change-function
		(lambda ()
                  (if (frame-focus-state)
                      (powerline-set-selected-window)
                    (powerline-unset-selected-window))))
  (with-no-warnings
    (add-hook 'focus-in-hook 'powerline-set-selected-window)
    (add-hook 'focus-out-hook 'powerline-unset-selected-window)))

;; Executes after the window manager requests that the user's events
;; be directed to a different frame.
(defadvice handle-switch-frame (after powerline-handle-switch-frame activate)
  "Call `powerline-set-selected-window'."
  (powerline-set-selected-window))

(add-hook 'buffer-list-update-hook #'powerline-set-selected-window)

;;;###autoload (autoload 'powerline-selected-window-active "powerline")
(defun powerline-selected-window-active ()
  "Return whether the current window is active."
  (eq powerline-selected-window (selected-window)))

(defun powerline-revert ()
  "Revert to the default Emacs mode-line."
  (interactive)
  (setq-default mode-line-format pl/default-mode-line))

(defun pl/render (item)
  "Render a powerline ITEM."
  (cond
   ((and (listp item) (eq 'image (car item)))
    (propertize " " 'display item
                'face (plist-get (cdr item) :face)))
   (item item)))

(defun powerline-render (values)
  "Render a list of powerline VALUES."
  (mapconcat 'pl/render values ""))

(defun powerline-width (values)
  "Get the length of VALUES."
  (if values
      (let ((val (car values)))
        (+ (cond
            ((stringp val) (string-width (format-mode-line val)))
            ((and (listp val) (eq 'image (car val)))
             (car (image-size val)))
            (t 0))
           (powerline-width (cdr values))))
    0))


(provide 'powerline)

;;; powerline.el ends here
