;;; helm-utils.el --- Utilities Functions for helm. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2023 Thierry Volpiatto

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

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-help)

(declare-function helm-find-files-1 "helm-files" (fname &optional preselect))
(declare-function helm-grep-split-line "helm-grep" (line))
(declare-function popup-tip "ext:popup")
(declare-function markdown-show-entry "ext:markdown-mode.el")
(declare-function outline-show-subtree "outline")
(declare-function org-reveal "org")
(declare-function hs-show-block "hideshow.el")
(declare-function hs-show-all "hideshow.el")
(declare-function tab-bar-tabs "tab-bar")
(declare-function tab-bar-select-tab "tab-bar")
(declare-function dired-goto-file "dired")
(declare-function bookmark-get-filename "bookmark")
(declare-function package-installed-p "package")
(declare-function package-desc-dir "package")

(defvar hs-minor-mode)
(defvar hs-show-hook)
(defvar org-directory)
(defvar winner-boring-buffers)
(defvar bookmark-alist)
(defvar dired-buffers)
(defvar helm-show-completion-overlay)
(defvar helm-buffers-maybe-switch-to-tab)
(defvar helm-ff-transformer-show-only-basename)
(defvar helm-popup-tip-mode)
(defvar helm-ff-last-expanded-candidate-regexp)
(defvar helm-mode-find-file-target-alist)

(defgroup helm-utils nil
  "Utilities routines for Helm."
  :group 'helm)

(defcustom helm-su-or-sudo "sudo"
  "What command to use for root access."
  :type 'string
  :group 'helm-utils)

(defcustom helm-default-kbsize 1024.0
  "Default Kbsize to use for showing files size.
It is a float, usually 1024.0 but could be 1000.0 on some systems."
  :group 'helm-utils
  :type 'float)

(define-obsolete-variable-alias
  'helm-highlight-number-lines-around-point
  'helm-highlight-matches-around-point-max-lines
  "20160119")

(defcustom helm-highlight-matches-around-point-max-lines '(15 . 15)
  "Number of lines around point where matched items are highlighted.

Possible value are:
- A cons cell (x . y)
  Match x lines before point and y lines after point.
- An integer
  Positive means this number lines after point.
  Negative means this number lines before point.
  A zero value means highlight only inside matched lines.
- The symbol never
  Means do not highlight matched items. "
  :group 'helm-utils
  :type '(choice (cons (integer :tag "Match before")
                       (integer :tag "Match after"))
                 (const :tag "Match in line only" 0)
                 (integer :tag "Match after or before (+/-)")
                 (const  :tag "Never match" never)))

(defcustom helm-highlight-only-all-matches nil
  "Highlight only when all items match on the line when non nil.
See `helm-highlight-current-line'."
  :group 'helm-utils
  :type 'boolean)

(defcustom helm-buffers-to-resize-on-pa nil
  "A list of helm buffers where the helm-window should be reduced on PA.
Where PA means persistent action."
  :group 'helm-utils
  :type '(repeat (choice string)))

(defcustom helm-resize-on-pa-text-height 12
  "The size of the helm-window when resizing on persistent action."
  :group 'helm-utils
  :type 'integer)

(defcustom helm-sources-using-help-echo-popup '("Ack-Grep" "AG" "RG" "Gid" "Git-Grep")
  "Show the buffer name or the filename in a popup at selection."
  :group 'helm-utils
  :type '(repeat (choice string)))

(defcustom helm-html-decode-entities-function #'helm-html-decode-entities-string
  "Function used to decode HTML entities in HTML bookmarks.
Helm comes by default with `helm-html-decode-entities-string', if
you need something more sophisticated you can use
`w3m-decode-entities-string' if available.

In Emacs itself org-entities seem broken and `xml-substitute-numeric-entities'
supports only numeric entities."
  :group 'helm-utils
  :type 'function)


(defvar helm-goto-line-before-hook '(helm-save-current-pos-to-mark-ring)
  "Run before jumping to line.
This hook runs when jumping from `helm-goto-line', `helm-etags-default-action',
and `helm-imenu-default-action'.
This allows you to retrieve a previous position after using the different helm
tools for searching (etags, grep, gid, (m)occur etc...).
By default positions are added to `mark-ring'.
You can also add to register by using (or adding)
`helm-save-pos-to-register-before-jump' instead. In this case
last position is added to the register `helm-save-pos-before-jump-register'.")

(defvar helm-save-pos-before-jump-register ?_
  "The register where `helm-save-pos-to-register-before-jump' saves position.")

(defconst helm-html-entities-alist
  '(("&quot;"   . 34)   ;; "
    ("&gt;"     . 62)   ;; >
    ("&lt;"     . 60)   ;; <
    ("&amp;"    . 38)   ;; &
    ("&euro;"   . 8364) ;; €
    ("&Yuml;"   . 89)   ;; Y
    ("&iexcl;"  . 161)  ;; ¡
    ("&cent;"   . 162)  ;; ¢
    ("&pound;"  . 163)  ;; £
    ("&curren;" . 164)  ;; ¤
    ("&yen"     . 165)  ;; ¥
    ("&brvbar;" . 166)  ;; ¦
    ("&sect;"   . 167)  ;; §
    ("&uml;"    . 32)   ;; SPC
    ("&nbsp;"   . 160)  ;;   (non breaking space)
    ("&copy;"   . 169)  ;; ©
    ("&ordf;"   . 97)   ;; a
    ("&laquo;"  . 171)  ;; «
    ("&not;"    . 172)  ;; ¬
    ("&masr;"   . 174)  ;; ®
    ("&deg;"    . 176)  ;; °
    ("&plusmn;" . 177)  ;; ±
    ("&sup2;"   . 50)   ;; 2
    ("&sup3;"   . 51)   ;; 3
    ("&acute;"  . 39)   ;; '
    ("&micro;"  . 956)  ;; μ
    ("&para;"   . 182)  ;; ¶
    ("&middot;" . 183)  ;; ·
    ("&cedil;"  . 32)   ;; SPC
    ("&sup1;"   . 49)   ;; 1
    ("&ordm;"   . 111)  ;; o
    ("&raquo;"  . 187)  ;; »
    ("&frac14;" . 49)   ;; 1
    ("&frac12;" . 49)   ;; 1
    ("&frac34;" . 51)   ;; 3
    ("&iquest;" . 191)  ;; ¿
    ("&Agrave;" . 192)  ;; À
    ("&Aacute;" . 193)  ;; Á
    ("&Acirc;"  . 194)  ;; Â
    ("&Atilde;" . 195)  ;; Ã
    ("&Auml;"   . 196)  ;; Ä
    ("&Aring;"  . 197)  ;; Å
    ("&Aelig"   . 198)  ;; Æ
    ("&Ccedil;" . 199)  ;; Ç
    ("&Egrave;" . 200)  ;; È
    ("&Eacute;" . 201)  ;; É
    ("&Ecirc;"  . 202)  ;; Ê
    ("&Euml;"   . 203)  ;; Ë
    ("&Igrave;" . 204)  ;; Ì
    ("&Iacute;" . 205)  ;; Í
    ("&Icirc;"  . 206)  ;; Î
    ("&Iuml;"   . 207)  ;; Ï
    ("&eth;"    . 208)  ;; Ð
    ("&Ntilde;" . 209)  ;; Ñ
    ("&Ograve;" . 210)  ;; Ò
    ("&Oacute;" . 211)  ;; Ó
    ("&Ocirc;"  . 212)  ;; Ô
    ("&Otilde;" . 213)  ;; Õ
    ("&Ouml;"   . 214)  ;; Ö
    ("&times;"  . 215)  ;; ×
    ("&Oslash;" . 216)  ;; Ø
    ("&Ugrave;" . 217)  ;; Ù
    ("&Uacute;" . 218)  ;; Ú
    ("&Ucirc;"  . 219)  ;; Û
    ("&Uuml;"   . 220)  ;; Ü
    ("&Yacute;" . 221)  ;; Ý
    ("&thorn;"  . 222)  ;; Þ
    ("&szlig;"  . 223)  ;; ß
    ("&agrave;" . 224)  ;; à
    ("&aacute;" . 225)  ;; á
    ("&acirc;"  . 226)  ;; â
    ("&atilde;" . 227)  ;; ã
    ("&auml;"   . 228)  ;; ä
    ("&aring;"  . 229)  ;; å
    ("&aelig;"  . 230)  ;; æ
    ("&ccedil;" . 231)  ;; ç
    ("&egrave;" . 232)  ;; è
    ("&eacute;" . 233)  ;; é
    ("&ecirc;"  . 234)  ;; ê
    ("&euml;"   . 235)  ;; ë
    ("&igrave;" . 236)  ;; ì
    ("&iacute;" . 237)  ;; í
    ("&icirc;"  . 238)  ;; î
    ("&iuml;"   . 239)  ;; ï
    ("&eth;"    . 240)  ;; ð
    ("&ntilde;" . 241)  ;; ñ
    ("&ograve;" . 242)  ;; ò
    ("&oacute;" . 243)  ;; ó
    ("&ocirc;"  . 244)  ;; ô
    ("&otilde;" . 245)  ;; õ
    ("&ouml;"   . 246)  ;; ö
    ("&divide;" . 247)  ;; ÷
    ("&oslash;" . 248)  ;; ø
    ("&ugrave;" . 249)  ;; ù
    ("&uacute;" . 250)  ;; ú
    ("&ucirc;"  . 251)  ;; û
    ("&uuml;"   . 252)  ;; ü
    ("&yacute;" . 253)  ;; ý
    ("&thorn;"  . 254)  ;; þ
    ("&yuml;"   . 255)  ;; ÿ
    ("&reg;"    . 174)  ;; ®
    ("&shy;"    . 173)) ;; ­

  "Table of html character entities and values.
See https://www.freeformatter.com/html-entities.html")

(defvar helm-find-many-files-after-hook nil
  "Hook that runs at end of `helm-find-many-files'.")

(defvar helm-marked-buffer-name "*helm marked*")

;;; Faces.
;;
(defface helm-selection-line
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :inherit highlight :distant-foreground "black"))
  "Face used in the `helm-current-buffer' when jumping to a candidate."
  :group 'helm-faces)

(defface helm-match-item
  `((t ,@(and (>= emacs-major-version 27) '(:extend t))
       :inherit isearch))
  "Face used to highlight the item matched in a selected line."
  :group 'helm-faces)


;;; Utils functions
;;
;;
(defcustom helm-window-prefer-horizontal-split nil
  "Maybe switch to other window vertically when non nil.

Possible values are t, nil and `decide'.

When t switch vertically.
When nil switch horizontally.
When `decide' try to guess if it is possible to switch vertically
according to the setting of `split-width-threshold' and the size of
the window from where splitting is done.

Note that when using `decide' and `split-width-threshold' is nil, the
behavior is the same as with a nil value."
  :group 'helm-utils
  :type '(choice
           (const :tag "Split window vertically" t)
           (const :tag "Split window horizontally" nil)
           (symbol :tag "Guess how to split window" 'decide)))

(defcustom helm-window-show-buffers-function #'helm-window-decide-split-fn
  "The default function to use when opening several buffers at once.
It is typically used to rearrange windows."
  :group 'helm-utils
  :type '(choice
          (function :tag "Decide how to split according to number of candidates"
                    helm-window-decide-split-fn)
          (function :tag "Split windows vertically or horizontally"
                    helm-window-default-split-fn)
          (function :tag "Split in alternate windows"
                    helm-window-alternate-split-fn)
          (function :tag "Split windows in mosaic"
                    helm-window-mosaic-fn)))

(defun helm-window-show-buffers (buffers &optional other-window)
  "Show BUFFERS.

With more than one buffer marked switch to these buffers in separate windows.
If OTHER-WINDOW is non-nil, keep current buffer and switch to other buffers
in separate windows.
If a prefix arg is given split windows vertically."
  (let ((initial-ow-fn (if (cdr (window-list))
                           #'switch-to-buffer-other-window
                         #'helm-window-other-window)))
    (if (cdr buffers)
        (funcall helm-window-show-buffers-function buffers
                 (and other-window initial-ow-fn))
      (if other-window
          (funcall initial-ow-fn (car buffers))
        (helm-buffers-switch-to-buffer-or-tab (car buffers))))))

(defvar tab-bar-tab-name-function)
(declare-function tab-bar-switch-to-tab "tab-bar.el")
(declare-function tab-bar-tab-name-all "tab-bar.el")

(defun helm-buffers-switch-to-buffer-or-tab (buffer)
  "Switch to BUFFER in its tab if some."
  (if (and (fboundp 'tab-bar-mode)
           helm-buffers-maybe-switch-to-tab
           tab-bar-mode)
      (let* ((tab-bar-tab-name-function #'tab-bar-tab-name-all)
             (tabs (tab-bar-tabs))
             (tab-names (mapcar (lambda (tab)
                                  (cdr (assq 'name tab)))
                                tabs))
             (bname (buffer-name (get-buffer buffer)))
             (tab (helm-buffers--get-tab-from-name bname tabs)))
        (if (helm-buffers--buffer-in-tab-p bname tab-names)
            (progn
              (tab-bar-switch-to-tab (alist-get 'name tab))
              (select-window (get-buffer-window bname)))
          (switch-to-buffer buffer)))
    (switch-to-buffer buffer)))

(defun helm-buffers--get-tab-from-name (tab-name tabs)
  "Return tab from TABS when it contains TAB-NAME."
  (cl-loop for tab in tabs
           when (member tab-name (split-string (cdr (assq 'name tab)) ", " t))
           return tab))

(defun helm-buffers--buffer-in-tab-p (buffer-name tab-names)
  "Check if BUFFER-NAME is in TAB-NAMES list."
  (cl-loop for name in tab-names
           ;; Buf names are separated with "," in TAB-NAMES
           ;; e.g. '("tab-bar.el" "*scratch*, helm-buffers.el").
           thereis (member buffer-name (split-string name ", " t))))

(defun helm-window-decide-split-fn (candidates &optional other-window-fn)
  "Try to find the best split window fn according to the number of CANDIDATES."
  (let ((fn (cond ((>= (length candidates) 3)
                   #'helm-window-mosaic-fn)
                  ((>= (length candidates) 2)
                   #'helm-window-alternate-split-fn)
                  (t #'helm-window-default-split-fn))))
    (funcall fn candidates other-window-fn)))

(defun helm-window-default-split-fn (candidates &optional other-window-fn)
  "Split windows in one direction and balance them.

Direction can be controlled via `helm-window-prefer-horizontal-split'.
If a prefix arg is given split windows the other direction.
This function is suitable for `helm-window-show-buffers-function'."
  (if other-window-fn
      (funcall other-window-fn (car candidates))
    (switch-to-buffer (car candidates)))
  (save-selected-window
    (cl-loop with nosplit
             for b in (cdr candidates)
             when nosplit return
             (message "Too many buffers to visit simultaneously")
             do (condition-case _err
                    (helm-window-other-window b 'balance)
                  (error (setq nosplit t) nil)))))

(defun helm-window-alternate-split-fn (candidates &optional other-window-fn)
  "Split windows horizontally and vertically in alternate fashion.

Direction can be controlled via `helm-window-prefer-horizontal-split'.
If a prefix arg is given split windows the other direction.
This function is suitable for `helm-window-show-buffers-function'."
  (if other-window-fn
      (funcall other-window-fn (car candidates))
    (switch-to-buffer (car candidates)))
  (let (right-side)
    (save-selected-window
      (cl-loop with nosplit
               for b in (cdr candidates)
               when nosplit return
               (message "Too many buffers to visit simultaneously")
               do (condition-case _err
                      (let ((helm-current-prefix-arg right-side))
                        (helm-window-other-window b)
                        (setq right-side (not right-side)))
                    (error (setq nosplit t) nil))))))

(defun helm-window-mosaic-fn (candidates &optional other-window-fn)
  "Make an as-square-as-possible window mosaic of the CANDIDATES buffers.

If rectangular, the long side is in the direction given by
`helm-window-prefer-horizontal-split': if non-nil, it is horizontal, vertical
otherwise.
If OTHER-WINDOW-FN is non-nil, current windows are included in the mosaic.
This function is suitable for `helm-window-show-buffers-function'."
  (when other-window-fn
    (setq candidates (append (mapcar 'window-buffer (window-list)) candidates)))
  (delete-other-windows)
  (let* ((helm-window-prefer-horizontal-split
          (if (eq helm-window-prefer-horizontal-split 'decide)
              (and (numberp split-width-threshold)
                   (>= (window-width (selected-window))
                       split-width-threshold))
            helm-window-prefer-horizontal-split))
         mosaic-length-tile-count
         mosaic-width-tile-count
         mosaic-length-tile-size
         mosaic-width-tile-size
         next-window)
    ;; If 4 tiles, make 2x2 mosaic.
    ;; If 5-6 tiles, make 2x3 mosaic with direction depending on `helm-window-prefer-horizontal-split'.
    ;; If 7-9 tiles, make 3x3 mosaic.  And so on.
    (setq mosaic-length-tile-count (ceiling (sqrt (length candidates))))
    (setq mosaic-width-tile-count
          (if (<= (length candidates) (* mosaic-length-tile-count (1- mosaic-length-tile-count)))
              (1- mosaic-length-tile-count)
            mosaic-length-tile-count))
    ;; We lower-bound the tile size, otherwise the function would
    ;; fail during the first inner split.
    ;; There is consequently no need to check for errors when
    ;; splitting.
    (let ((frame-mosaic-length-direction-size (frame-height))
          (frame-mosaic-width-direction-size (frame-width))
          (window-mosaic-length-direction-min-size window-min-height)
          (window-mosaic-width-direction-min-size window-min-width))
      (if helm-window-prefer-horizontal-split
          (setq frame-mosaic-length-direction-size (frame-width)
                frame-mosaic-width-direction-size (frame-height)
                window-mosaic-length-direction-min-size window-min-width
                window-mosaic-width-direction-min-size window-min-height))
      (setq mosaic-length-tile-size (max
                                     (/ frame-mosaic-length-direction-size mosaic-length-tile-count)
                                     window-mosaic-length-direction-min-size)
            mosaic-width-tile-size (max
                                    (/ frame-mosaic-width-direction-size mosaic-width-tile-count)
                                    window-mosaic-width-direction-min-size))
      ;; Shorten `candidates' to `max-tiles' elements.
      (let ((max-tiles (* (/ frame-mosaic-length-direction-size mosaic-length-tile-size)
                          (/ frame-mosaic-width-direction-size mosaic-width-tile-size))))
        (when (> (length candidates) max-tiles)
          (message "Too many buffers to visit simultaneously")
          (setcdr (nthcdr (- max-tiles 1) candidates) nil))))
    ;; Make the mosaic.
    (while candidates
      (when (> (length candidates) mosaic-length-tile-count)
        (setq next-window (split-window nil
                                        mosaic-width-tile-size
                                        (not helm-window-prefer-horizontal-split))))
      (switch-to-buffer (pop candidates))
      (dotimes (_ (min (1- mosaic-length-tile-count) (length candidates)))
        (select-window (split-window nil
                                     mosaic-length-tile-size
                                     helm-window-prefer-horizontal-split))
        (switch-to-buffer (pop candidates)))
      (when next-window
        (select-window next-window)))))

(defun helm-window-other-window (buffer-or-name &optional balance)
  "Switch to BUFFER-OR-NAME in other window.
Direction can be controlled via `helm-window-prefer-horizontal-split'.
If a prefix arg is given split windows the other direction.
When argument BALANCE is provided `balance-windows'."
  (let* ((helm-window-prefer-horizontal-split
          (if (eq helm-window-prefer-horizontal-split 'decide)
              (and (numberp split-width-threshold)
                   (>= (window-width (selected-window))
                       split-width-threshold))
            helm-window-prefer-horizontal-split))
         (right-side (if helm-window-prefer-horizontal-split
                         (not helm-current-prefix-arg)
                       helm-current-prefix-arg)))
    (select-window (split-window nil nil right-side))
    (and balance (balance-windows))
    (switch-to-buffer buffer-or-name)))

(cl-defun helm-current-buffer-narrowed-p (&optional
                                          (buffer helm-current-buffer))
  "Check if BUFFER is narrowed.
Default is `helm-current-buffer'."
  (with-current-buffer buffer
    (let ((beg (point-min))
          (end (point-max))
          (total (buffer-size)))
      (or (/= beg 1) (/= end (1+ total))))))

(defun helm-goto-char (loc)
  "Go to char, revealing if necessary."
  (goto-char loc)
  (let ((fn (cond ((eq major-mode 'org-mode)
                   ;; On some old Emacs versions org may not be loaded.
                   (require 'org)
                   #'org-reveal)
                  ((and (boundp 'outline-minor-mode)
                        outline-minor-mode)
                   #'outline-show-subtree)
                  ((and (boundp 'hs-minor-mode)
                    hs-minor-mode)
                   #'hs-show-all)
                  ((and (boundp 'markdown-mode-map)
                        (derived-mode-p 'markdown-mode))
                   #'markdown-show-entry)))
        (hs-show-hook (list (lambda () (goto-char loc)))))
    ;; outline may fail in some conditions e.g. with markdown enabled
    ;; (Bug#1919).
    (condition-case-unless-debug nil
        (and fn (funcall fn))
      (error nil))))

(defun helm-goto-line (lineno &optional noanim)
  "Goto LINENO opening only outline headline if needed.
Animation is used unless NOANIM is non--nil."
  (helm-log-run-hook "helm-goto-line"
                     'helm-goto-line-before-hook)
  (helm-match-line-cleanup)
  (unless helm-alive-p
    (with-helm-current-buffer
      (unless helm-yank-point (setq helm-yank-point (point)))))
  (goto-char (point-min))
  (helm-goto-char (pos-bol lineno))
  (unless noanim
    (helm-highlight-current-line)))

(defun helm-save-pos-to-register-before-jump ()
  "Save current buffer position to `helm-save-pos-before-jump-register'.
To use this add it to `helm-goto-line-before-hook'."
  (with-helm-current-buffer
    (unless helm-in-persistent-action
      (point-to-register helm-save-pos-before-jump-register))))

(defun helm-save-current-pos-to-mark-ring ()
  "Save current buffer position to mark ring.
To use this add it to `helm-goto-line-before-hook'."
  (with-helm-current-buffer
    (unless helm-in-persistent-action
      (set-marker (mark-marker) (point))
      (push-mark (point) 'nomsg))))

(defun helm-displaying-source-names ()
  "Return the list of sources name for this helm session."
  (with-current-buffer helm-buffer
    (goto-char (point-min))
    (cl-loop with pos
          while (setq pos (next-single-property-change (point) 'helm-header))
          do (goto-char pos)
          collect (buffer-substring-no-properties (pos-bol)(pos-eol))
          do (forward-line 1))))

(defun helm-handle-winner-boring-buffers ()
  "Add `helm-buffer' to `winner-boring-buffers' when quitting/exiting helm.
Add this function to `helm-cleanup-hook' when you don't want to see helm buffers
after running winner-undo/redo."
  (require 'winner)
  (cl-pushnew helm-buffer winner-boring-buffers :test 'equal))
(add-hook 'helm-cleanup-hook #'helm-handle-winner-boring-buffers)

(defun helm-quit-and-find-file ()
  "Drop into `helm-find-files' from `helm'.
If current selection is a buffer or a file, `helm-find-files'
from its directory."
  (interactive)
  (with-helm-alive-p
    (require 'helm-grep)
    (require 'helm-elisp)
    (require 'bookmark) ; For bookmark-alist
    (let ((src (helm-get-current-source)))
      (helm-run-after-exit
       (lambda (f)
         ;; Ensure specifics `helm-execute-action-at-once-if-one'
         ;; fns don't run here.
         (let (helm-execute-action-at-once-if-one
               helm-actions-inherit-frame-settings) ; use this-command
           (if (file-exists-p f)
               (helm-find-files-1 (file-name-directory f)
                                   (format
                                    helm-ff-last-expanded-candidate-regexp
                                    (regexp-quote
                                     (if helm-ff-transformer-show-only-basename
                                         (helm-basename f) f))))
             (helm-find-files-1 f))))
       (helm--quit-and-find-file-default-file src)))))
(put 'helm-quit-and-find-file 'helm-only t)

(defun helm--quit-and-find-file-default-file (source)
  (let ((target-fn (or (helm-get-attr 'find-file-target source)
                       (assoc-default (helm-get-attr 'name source)
                                      helm-mode-find-file-target-alist))))
    ;; target-fn function may return nil, in this case fallback to default.
    (helm-aif (and target-fn (funcall target-fn source))
        it
      (let* ((sel                  (helm-get-selection nil nil source))
             (default-preselection (or (helm-default-directory)
                                       (buffer-file-name helm-current-buffer)
                                       default-directory)))
        (cond
         ((and (stringp sel) (or (file-remote-p sel)
                                 (file-exists-p sel)))
          (expand-file-name sel))
         ;; Url.
         ((and (stringp sel)
               helm--url-regexp
               (string-match helm--url-regexp sel))
          sel)
         ;; Exit brutally from a `with-helm-show-completion'
         ((and helm-show-completion-overlay
               (overlayp helm-show-completion-overlay))
          (delete-overlay helm-show-completion-overlay)
          (remove-hook 'helm-move-selection-after-hook 'helm-show-completion)
          (expand-file-name default-preselection))
         ;; Default.
         (t (expand-file-name default-preselection)))))))

(defun helm-generic-sort-fn (s1 s2)
  "Sort predicate function for helm candidates.
Args S1 and S2 can be single or (display . real) candidates,
that is sorting is done against real value of candidate."
  (let* ((qpattern (regexp-quote helm-pattern))
         (reg1  (concat "\\_<" qpattern "\\_>"))
         (reg2  (concat "\\_<" qpattern))
         (reg3  helm-pattern)
         (split (helm-remove-if-match
                 "\\`!" (helm-mm-split-pattern helm-pattern)))
         (str1  (if (consp s1) (cdr s1) s1))
         (str2  (if (consp s2) (cdr s2) s2))
         (score (lambda (str r1 r2 r3 lst)
                  (condition-case nil
                      (+ (if (string-match (concat "\\`" qpattern) str) 1 0)
                         (cond ((string-match r1 str) 5)
                               ((and (string-match " " qpattern)
                                     (car lst)
                                     (string-match
                                      (concat "\\_<" (regexp-quote (car lst))) str)
                                     (cl-loop for r in (cdr lst)
                                              always (string-match r str)))
                                4)
                               ((and (string-match " " qpattern)
                                     (cl-loop for r in lst
                                              always (string-match r str)))
                                3)
                               ((string-match r2 str) 2)
                               ((string-match r3 str) 1)
                               (t 0)))
                    (invalid-regexp 0))))
         (sc1 (get-text-property 0 'completion-score str1))
         (sc2 (get-text-property 0 'completion-score str2))
         (sc3 (if sc1 0 (funcall score str1 reg1 reg2 reg3 split)))
         (sc4 (if sc2 0 (funcall score str2 reg1 reg2 reg3 split))))
    (cond ((and sc1 sc2) ; helm-flex style.
           (> sc1 sc2))
          ((or (zerop (string-width qpattern))
               (and (zerop sc3) (zerop sc4)))
           (string-lessp str1 str2))
          ((= sc3 sc4)
           (< (length str1) (length str2)))
          (t (> sc3 sc4)))))

(cl-defun helm-file-human-size (size &optional (kbsize helm-default-kbsize))
  "Return a string showing SIZE of a file in human readable form.
SIZE can be an integer or a float depending on it's value.
`file-attributes' will take care of that to avoid overflow error.
KBSIZE is a floating point number, defaulting to `helm-default-kbsize'."
  (cl-loop with result = (cons "B" size)
           for i in '("k" "M" "G" "T" "P" "E" "Z" "Y")
           while (>= (cdr result) kbsize)
           do (setq result (cons i (/ (cdr result) kbsize)))
           finally return
           (helm-acase (car result)
             ("B" (format "%s" size))
             (t (format "%.1f%s" (cdr result) it)))))

(defun helm-directory-size (directory &optional recursive human)
  "Return the resulting size of the sum of all files in DIRECTORY.

If RECURSIVE is non nil return the size of all files in DIRECTORY and
its subdirectories.  With arg HUMAN format the size in a human
readable format,see `helm-file-human-size'."
  (cl-loop with files = (if recursive
                            (helm-walk-directory
                             directory
                             :path 'full
                             :directories t)
                          (directory-files
                           directory t
                           directory-files-no-dot-files-regexp))
           for file in files
           sum (nth 7 (file-attributes file)) into total
           finally return (if human
                              (helm-file-human-size total)
                            total)))

(cl-defun helm-file-attributes
    (file &key type links uid gid access-time modif-time
            status size mode gid-change inode device-num dired human-size
            mode-type mode-owner mode-group mode-other octal (string t))
  "Return `file-attributes' elements of FILE separately according to key value.
Availables keys are:
- TYPE: Same as nth 0 `files-attributes' if STRING is nil
        otherwise return either symlink, directory or file (default).
- LINKS: See nth 1 `files-attributes'.
- UID: See nth 2 `files-attributes'.
- GID: See nth 3 `files-attributes'.
- ACCESS-TIME: See nth 4 `files-attributes', however format time
               when STRING is non--nil (the default).
- MODIF-TIME: See nth 5 `files-attributes', same as above.
- STATUS: See nth 6 `files-attributes', same as above.
- SIZE: See nth 7 `files-attributes'.
- MODE: See nth 8 `files-attributes'.
- GID-CHANGE: See nth 9 `files-attributes'.
- INODE: See nth 10 `files-attributes'.
- DEVICE-NUM: See nth 11 `files-attributes'.
- DIRED: A line similar to what \\='ls -l' return.
- HUMAN-SIZE: The size in human form, see `helm-file-human-size'.
- MODE-TYPE, mode-owner,mode-group, mode-other: Split what
  nth 8 `files-attributes' return in four categories.
- OCTAL: The octal value of MODE-OWNER+MODE-GROUP+MODE-OTHER.
- STRING: When non--nil (default) `helm-file-attributes' return
          more friendly values.
If you want the same behavior as `files-attributes' ,
\(but with return values in proplist) use a nil value for STRING.
However when STRING is non--nil, time and type value are different from what
you have in `file-attributes'."
  (helm-aif (file-attributes file string)
      (let* ((all (cl-destructuring-bind
                        (type links uid gid access-time modif-time
                              status size mode gid-change inode device-num)
                      it
                    (list :type        (if string
                                           (cond ((stringp type) "symlink") ; fname
                                                 (type "directory") ; t
                                                 (t "file")) ; nil
                                         type)
                          :links       links
                          :uid         uid
                          :gid         gid
                          :access-time (if string
                                           (format-time-string
                                            "%Y-%m-%d %R" access-time)
                                         access-time)
                          :modif-time  (if string
                                           (format-time-string
                                            "%Y-%m-%d %R" modif-time)
                                         modif-time)
                          :status      (if string
                                           (format-time-string
                                            "%Y-%m-%d %R" status)
                                         status)
                          :size        size
                          :mode        mode
                          :gid-change  gid-change
                          :inode       inode
                          :device-num  device-num)))
             (perms (cl-getf all :mode))
             (modes (helm-split-mode-file-attributes perms)))
        (cond (type        (cl-getf all :type))
              (links       (cl-getf all :links))
              (uid         (cl-getf all :uid))
              (gid         (cl-getf all :gid))
              (access-time (cl-getf all :access-time))
              (modif-time  (cl-getf all :modif-time))
              (status      (cl-getf all :status))
              (size        (cl-getf all :size))
              (mode        perms)
              (gid-change  (cl-getf all :gid-change))
              (inode       (cl-getf all :inode))
              (device-num  (cl-getf all :device-num))
              (dired       (helm-file-attributes-dired-line all human-size))
              (human-size (helm-file-human-size (cl-getf all :size)))
              (mode-type  (cl-getf modes :mode-type))
              (mode-owner (cl-getf modes :user))
              (mode-group (cl-getf modes :group))
              (mode-other (cl-getf modes :other))
              (octal      (cl-getf modes :octal))
              (t          (append all modes))))))

(defun helm-file-attributes-dired-line (all &optional human-size)
  (format "%s %s %s:%s %s %s"
   (helm-split-mode-file-attributes
    (cl-getf all :mode) t)
   (number-to-string (cl-getf all :links))
   (cl-getf all :uid)
   (cl-getf all :gid)
   (if human-size
       (helm-file-human-size (cl-getf all :size))
     (int-to-string (cl-getf all :size)))
   (cl-getf all :modif-time)))

(defun helm-split-mode-file-attributes (modes &optional string)
  "Split MODES in a list of modes.
MODES is same as what (nth 8 (file-attributes \"foo\")) would return."
  (if (string-match "\\`\\(.\\)\\(...\\)\\(...\\)\\(...\\)\\'" modes)
      (let* ((type  (match-string 1 modes))
             (user  (match-string 2 modes))
             (group (match-string 3 modes))
             (other (match-string 4 modes))
             (octal (helm-ff-numeric-permissions (list user group other))))
        (if string
            (mapconcat 'identity (list type user group other octal) " ")
          (list :mode-type type :user user
                :group group :other other
                :octal octal)))
    (error "Wrong modes specification for %s" modes)))

(defun helm-ff-numeric-permissions (perms)
  "Return the numeric representation of PERMS.
PERMS is the list of permissions for owner, group and others."
  ;; `file-modes-symbolic-to-number' interpret its MODES argument as what would
  ;; result when calling such mode on a file with chmod, BTW we have to remove
  ;; all "-" like read-file-modes does.
  (helm-aand (listp perms)
             (apply 'format "u=%s,g=%s,o=%s" perms)
             (replace-regexp-in-string "-" "" it)
             (format "%o" (file-modes-symbolic-to-number it))))

(defun helm-format-columns-of-files (files)
  "Same as `dired-format-columns-of-files'.
Inlined here for compatibility."
  (let ((beg (point)))
    (completion--insert-strings files)
    (put-text-property beg (point) 'mouse-face nil)))

(defmacro with-helm-display-marked-candidates (buffer-or-name candidates &rest body)
  (declare (indent 0) (debug t))
  (helm-with-gensyms (buffer window winconf)
    `(let* ((,buffer (temp-buffer-window-setup ,buffer-or-name))
            (,winconf helm-last-frame-or-window-configuration)
            (helm-always-two-windows t)
            (helm-split-window-default-side
             (if (eq helm-split-window-default-side 'same)
                 'below helm-split-window-default-side))
            helm-split-window-inside-p
            helm-reuse-last-window-split-state
            ,window)
       (with-current-buffer ,buffer
         (helm-format-columns-of-files ,candidates))
       (unwind-protect
            (with-selected-window
                (setq ,window (temp-buffer-window-show
                               ,buffer
                               '(display-buffer-below-selected
                                 (window-height . fit-window-to-buffer))))
              (progn ,@body))
         (quit-window 'kill ,window)
         (and ,winconf (set-window-configuration ,winconf))))))

;;; Persistent Action Helpers
;;
;;
;; Internal
(defvar helm-match-line-overlay nil)
(defvar helm--match-item-overlays nil)

(cl-defun helm-highlight-current-line (&optional start end buf face)
  "Highlight current line and all matching items around it.

The number of lines around matched line where the matching items are
highlighted are defined by `helm-highlight-matches-around-point-max-lines'.
When the variable `helm-highlight-only-all-matches' is non nil only
the lines containing all matches (in case of multi match) are highlighted.

Optional arguments START, END and FACE are only here for debugging purpose."
  (let* ((start (or start (line-beginning-position)))
         (end (or end (1+ (line-end-position))))
         start-match end-match
         (args (list start end buf))
         (case-fold-search (if helm-alive-p
                               (helm-set-case-fold-search)
                             case-fold-search)))
    ;; Highlight the current line.
    (if (not helm-match-line-overlay)
        (setq helm-match-line-overlay (apply 'make-overlay args))
      (apply 'move-overlay helm-match-line-overlay args))
    (overlay-put helm-match-line-overlay
                 'face (or face 'helm-selection-line))
    ;; Now highlight matches only if we are in helm session, we are
    ;; maybe coming from helm-grep-mode or helm-moccur-mode buffers.
    (when helm-alive-p
      (cond (;; These 2 clauses have to be the first otherwise
             ;; `helm-highlight-matches-around-point-max-lines' is
             ;; compared as a number by other clauses and return an error.
             (eq helm-highlight-matches-around-point-max-lines 'never)
             (cl-return-from helm-highlight-current-line))
            ((consp helm-highlight-matches-around-point-max-lines)
             (setq start-match
                   (save-excursion
                     (forward-line
                      (- (car helm-highlight-matches-around-point-max-lines)))
                     (pos-bol))
                   end-match
                   (save-excursion
                     (forward-line
                      (cdr helm-highlight-matches-around-point-max-lines))
                     (pos-bol))))
            ((or (null helm-highlight-matches-around-point-max-lines)
                 (zerop helm-highlight-matches-around-point-max-lines))
             (setq start-match start
                   end-match   end))
            ((< helm-highlight-matches-around-point-max-lines 0)
             (setq start-match
                   (save-excursion
                     (forward-line
                      helm-highlight-matches-around-point-max-lines)
                     (pos-bol))
                   end-match start))
            ((> helm-highlight-matches-around-point-max-lines 0)
             (setq start-match start
                   end-match
                   (save-excursion
                     (forward-line
                      helm-highlight-matches-around-point-max-lines)
                     (pos-bol)))))
      (catch 'empty-line
        (let* ((regex-list (helm-remove-if-match
                            "\\`!" (helm-mm-split-pattern
                                    (if (with-helm-buffer
                                          ;; Needed for highlighting AG matches.
                                          (assq 'pcre (helm-get-current-source)))
                                        (helm--translate-pcre-to-elisp helm-input)
                                      helm-input))))
               (num-regex (length regex-list)))
          (save-excursion
            (goto-char start-match)
            (while (< (point) end-match)
              (let* ((start-line (line-beginning-position))
                     (end-line   (line-end-position))
                     all-matches)
                (dolist (r regex-list)
                  (let ((match-list '()))
                    (save-excursion
                      (goto-char start-line)
                      (while (condition-case _err
                                 (and (not (= start-line end-line))
                                      (if helm-migemo-mode
                                          (helm-mm-migemo-forward r end-line t)
                                        (re-search-forward r end-line t)))
                               (invalid-regexp nil))
                        (let ((s (match-beginning 0))
                              (e (match-end 0)))
                          (if (= s e)
                              (throw 'empty-line nil)
                            (push (cons s e) match-list)))))
                    (when match-list
                      (push match-list all-matches))))
                (when (and all-matches
                           (or (not helm-highlight-only-all-matches)
                               (eql (length all-matches) num-regex)))
                  (cl-loop for ml in all-matches
                           do (cl-loop for (s . e) in ml
                                       for ov = (make-overlay s e)
                                       do (progn
                                            (push ov helm--match-item-overlays)
                                            (overlay-put ov 'face 'helm-match-item)
                                            (overlay-put ov 'priority 1))))))
              (forward-line 1))))))
    (recenter)))

(defun helm--translate-pcre-to-elisp (regexp)
  "Should translate pcre REGEXP to elisp regexp.
Assume regexp is a pcre based regexp."
  (with-temp-buffer
    (insert " " regexp " ")
    (goto-char (point-min))
    (save-excursion
      ;; match (){}| unquoted
      (helm-awhile (and (re-search-forward "\\([(){}|]\\)" nil t)
                        (match-string 1))
        (let ((pos (match-beginning 1)))
          (if (eql (char-before pos) ?\\)
              (delete-region pos (1- pos))
              (replace-match (concat "\\" it) t t nil 1)))))
    ;; match \s or \S
    (helm-awhile (and (re-search-forward "\\S\\?\\(\\s\\[sS]\\)[^-]" nil t)
                      (match-string 1))
      (replace-match (concat it "-") t t nil 1))
    (buffer-substring (1+ (point-min)) (1- (point-max)))))

(defun helm-match-line-cleanup ()
  (when helm-match-line-overlay
    (delete-overlay helm-match-line-overlay)
    (setq helm-match-line-overlay nil))
  (when helm--match-item-overlays
    (mapc 'delete-overlay helm--match-item-overlays)))

(defun helm-match-line-cleanup-maybe ()
  (when (helm-empty-buffer-p)
    (helm-match-line-cleanup)))

(defun helm-match-line-update ()
  (when helm--match-item-overlays
    (mapc 'delete-overlay helm--match-item-overlays))
  (when helm-match-line-overlay
    (delete-overlay helm-match-line-overlay)
    (helm-highlight-current-line)))

(defun helm-persistent-autoresize-hook ()
  (when (and helm-buffers-to-resize-on-pa
             (member helm-buffer helm-buffers-to-resize-on-pa)
             (eq helm-split-window-state 'vertical))
    (set-window-text-height (helm-window) helm-resize-on-pa-text-height)))

(defun helm-match-line-cleanup-pulse ()
  (run-with-timer 0.3 nil #'helm-match-line-cleanup))

(add-hook 'helm-after-update-hook 'helm-match-line-cleanup-maybe)
(add-hook 'helm-after-persistent-action-hook 'helm-persistent-autoresize-hook)
(add-hook 'helm-cleanup-hook 'helm-match-line-cleanup)
(add-hook 'helm-after-action-hook 'helm-match-line-cleanup-pulse)
(add-hook 'helm-after-persistent-action-hook 'helm-match-line-update)

;;; Popup buffer-name or filename in grep/moccur/imenu-all.
;;
(defvar helm--show-help-echo-timer nil)

(defun helm-cancel-help-echo-timer ()
  (when helm--show-help-echo-timer
    (cancel-timer helm--show-help-echo-timer)
    (setq helm--show-help-echo-timer nil)))

(defun helm-maybe-show-help-echo ()
  (when helm--show-help-echo-timer
    (cancel-timer helm--show-help-echo-timer)
    (setq helm--show-help-echo-timer nil))
  (when (and helm-alive-p
             helm-popup-tip-mode
             (member (assoc-default 'name (helm-get-current-source))
                     helm-sources-using-help-echo-popup))
    (setq helm--show-help-echo-timer
          (run-with-timer
           1 nil
           (lambda ()
             (save-selected-window
               (with-helm-window
                 (helm-aif (get-text-property (pos-bol) 'help-echo)
                     (popup-tip (concat " " (abbreviate-file-name
                                             (replace-regexp-in-string "\n.*" "" it)))
                                :around nil
                                :point (save-excursion
                                         (end-of-visual-line) (point)))))))))))

;;;###autoload
(define-minor-mode helm-popup-tip-mode
    "Show help-echo informations in a popup tip at end of line."
  :global t
  (require 'popup)
  (if helm-popup-tip-mode
      (progn
        (add-hook 'helm-move-selection-after-hook 'helm-maybe-show-help-echo)
        (add-hook 'helm-cleanup-hook 'helm-cancel-help-echo-timer))
    (remove-hook 'helm-move-selection-after-hook 'helm-maybe-show-help-echo)
    (remove-hook 'helm-cleanup-hook 'helm-cancel-help-echo-timer)))

(defun helm-open-file-with-default-tool (file)
  "Open FILE with the default tool on this platform."
  (let (process-connection-type)
    (if (eq system-type 'windows-nt)
        (helm-w32-shell-execute-open-file file)
      (start-process "helm-open-file-with-default-tool"
                     nil
                     (cond ((eq system-type 'gnu/linux)
                            "xdg-open")
                           ((or (eq system-type 'darwin) ;; Mac OS X
                                (eq system-type 'macos)) ;; Mac OS 9
                            "open")
			   ((eq system-type 'cygwin)
			    "cygstart"))
                     file))))

(defun helm-open-dired (file)
  "Open a dired buffer in FILE's directory.
If FILE is a directory, open this directory."
  (require 'dired)
  (if (file-directory-p file)
      (dired file)
    (dired (file-name-directory file))
    (dired-goto-file file)))

(defun helm-find-file-as-root (candidate)
  (let* ((buf (helm-basename candidate))
         (host (file-remote-p candidate 'host))
         (remote-path (format "/%s:%s:%s"
                              helm-su-or-sudo
                              (or host "")
                              (expand-file-name
                               (if host
                                   (file-remote-p candidate 'localname)
                                 candidate))))
         non-essential)
    (if (buffer-live-p (get-buffer buf))
        (progn
          (set-buffer buf)
          (find-alternate-file remote-path))
      (find-file remote-path))))

(defun helm-find-many-files (_ignore)
  "Simple action that run `find-file' on marked candidates.
Run `helm-find-many-files-after-hook' at end."
  (let ((helm--reading-passwd-or-string t))
    (mapc 'find-file (helm-marked-candidates))
    (helm-log-run-hook "helm-find-many-files"
                       'helm-find-many-files-after-hook)))

(defun helm-read-repeat-string (prompt &optional count)
  "Prompt as many time PROMPT is not empty.
If COUNT is non--nil add a number after each prompt.
Return the list of strings entered in each prompt."
  (cl-loop with prt = prompt
           with elm
           while (not (string= elm ""))
           for n from 1
           do (when count
                (setq prt (format "%s (%s): "
                                  (replace-regexp-in-string
                                   ": " "" prompt)
                                  (int-to-string n))))
           collect (setq elm (helm-read-string prt)) into lis
           finally return (remove "" lis)))

(defun helm-html-bookmarks-to-alist (file url-regexp bmk-regexp)
  "Parse HTML bookmark FILE and return an alist with (title . url) as elements."
  (let (bookmarks-alist url title)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward "href=\\|^ *<DT><A HREF=" nil t)
        (forward-line 0)
        (when (re-search-forward url-regexp nil t)
          (setq url (match-string 0)))
        (when (re-search-forward bmk-regexp nil t)
          (setq title (url-unhex-string
                       (funcall helm-html-decode-entities-function
                               (match-string 1)))))
        (push (cons title url) bookmarks-alist)
        (forward-line)))
    (nreverse bookmarks-alist)))

(defun helm-html-entity-to-string (entity)
  "Replace an HTML ENTITY with its string value.
When unable to decode ENTITY returns nil."
  (helm-aif (assoc entity helm-html-entities-alist)
      (string (cdr it))
    (save-match-data
      (when (string-match "[0-9]+" entity)
        (string (string-to-number (match-string 0 entity)))))))

(defun helm-html-decode-entities-string (str)
  "Decode entities in the string STR."
  (save-match-data
    (with-temp-buffer
      (insert str)
      (goto-char (point-min))
      (while (re-search-forward "&#?\\([^;]*\\);" nil t)
        (helm-aif (helm-html-entity-to-string (match-string 0))
            (replace-match it)))
      (buffer-string))))

(provide 'helm-utils)

;;; helm-utils.el ends here
