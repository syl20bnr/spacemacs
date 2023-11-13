;;; evil-snipe.el --- emulate vim-sneak & vim-seek
;;
;; Copyright (C) 2014-20 Henrik Lissner
;;
;; Author: Henrik Lissner <http://github/hlissner>
;; Maintainer: Henrik Lissner <contact@henrik.io>
;; Created: December 5, 2014
;; Modified: August 21, 2023
;; Version: 2.1.3
;; Keywords: emulation, vim, evil, sneak, seek
;; Homepage: https://github.com/hlissner/evil-snipe
;; Package-Requires: ((emacs "24.4") (evil "1.2.12") (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.

;;; Commentary:
;;
;; Evil-snipe emulates vim-seek and/or vim-sneak in evil-mode.
;;
;; It provides 2-character versions of evil's f/F/t/T motions, for quick and
;; more accurately jumping around text, plus incremental highlighting (for
;; f/F/t/T as well).
;;
;; To enable globally:
;;
;;     (require 'evil-snipe)
;;     (evil-snipe-mode 1)
;;
;; To replace evil-mode's f/F/t/T functionality with (1-character) sniping:
;;
;;     (evil-snipe-override-mode 1)
;;
;; See included README.md for more information.
;;
;;; Code:

(require 'evil)
(eval-when-compile (require 'cl-lib))


;;
;;; Settings

(defgroup evil-snipe nil
  "An evil plugin that emulates vim-seek/vim-sneak's 2-character motions."
  :prefix "evil-snipe-"
  :group 'evil)

(defcustom evil-snipe-enable-highlight t
  "If non-nil, all matches will be highlighted after the initial jump.
Highlights will disappear as soon as you do anything afterwards, like move the
cursor."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-enable-incremental-highlight t
  "If non-nil, incrementally highlight matches.
Otherwise, only highlight after you've finished skulking."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-override-evil-repeat-keys t
  "If non-nil evil-snipe will replace evil's ; and , repeat keys with its own.

Only effective if `evil-snipe-override-mode' is active.

Evil-snipe improves these repeat keys with incremental highlighting and support
for repeating snipes."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-scope 'line
  "Dictates the scope of searches, which can be one of:

'line
  Search line after the cursor (this is vim-seek behavior) (default)
'buffer
  Search rest of the buffer after the cursor (vim-sneak behavior)
'visible
  Search rest of visible buffer (Is more performant than 'buffer, but will not
  highlight/jump past the visible buffer)
'whole-line
  Same as 'line, but highlight matches on either side of cursor
'whole-buffer
  Same as 'buffer, but highlight *all* matches in buffer
'whole-visible
  Same as 'visible, but highlight *all* visible matches in buffer

See `evil-snipe-repeat-scope' to change scope only when repeating snipes.

See `evil-snipe-spillover-scope' to fall back to another scope if the first
snipe yields no matches."
  :group 'evil-snipe
  :type '(choice
          (const :tag "Forward line" 'line)
          (const :tag "Forward buffer" 'buffer)
          (const :tag "Forward visible buffer" 'visible)
          (const :tag "Whole line" 'whole-line)
          (const :tag "Whole buffer" 'whole-buffer)
          (const :tag "Whole visible buffer" 'whole-visible)))

(defcustom evil-snipe-repeat-scope nil
  "Dictates the scope of repeat searches.
See `evil-snipe-scope' for possible settings. When nil, defaults to whatever
`evil-snipe-scope' is set to."
  :group 'evil-snipe
  :type 'symbol)

(defcustom evil-snipe-spillover-scope nil
  "If non-nil, snipe will expand the search scope to this when a snipe fails.

Accepts the same values as `evil-snipe-scope' and `evil-snipe-repeat-scope'.
Is only useful if set to the same or broader scope than either."
  :group 'evil-snipe
  :type 'symbol)

(defcustom evil-snipe-repeat-keys t
  "If non-nil, pressing s/S after a search will repeat it.
If `evil-snipe-override-evil' is non-nil, this applies to f/F/t/T as well."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-show-prompt t
  "If non-nil, show 'N>' prompt while sniping."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-smart-case nil
  "By default, searches are case sensitive.
If `evil-snipe-smart-case' is enabled, searches are case sensitive only if
search contains capital letters."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-auto-scroll nil
  "If non-nil, the window will scroll to follow the cursor."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-aliases '()
  "A list of characters mapped to regexps '(CHAR REGEX).
If CHAR is used in a snipe, it will be replaced with REGEX. These aliases apply
globally. To set an alias for a specific mode use:

    (add-hook 'c++-mode-hook
      (lambda ()
        (make-local-variable 'evil-snipe-aliases)
        (push '(?\[ \"[[{(]\") evil-snipe-aliases)))"
  :group 'evil-snipe
  :type '(repeat (cons (character :tag "Key")
                       (regexp :tag "Pattern"))))
(define-obsolete-variable-alias 'evil-snipe-symbol-groups 'evil-snipe-aliases "v2.0.0")

(defcustom evil-snipe-disabled-modes
  '(org-agenda-mode magit-mode git-rebase-mode elfeed-show-mode
    elfeed-search-mode ranger-mode magit-repolist-mode
    undo-tree-visualizer-mode tabulated-list-mode
    Info-mode calc-mode treemacs-mode ibuffer-mode dired-mode
    ;; `mu4e'
    mu4e-main-mode mu4e-view-mode mu4e-headers-mode mu4e~update-mail-mode
    ;; `notmuch'
    notmuch-tree-mode notmuch-hello-mode notmuch-search-mode notmuch-show-mode)
  "A list of major modes where evil-snipe should not activate."
  :group 'evil-snipe
  :type  '(list symbol))

(defvar evil-snipe-use-vim-sneak-bindings nil
  "Uses only Z and z under operator state, as vim-sneak does.

This frees the x binding in operator state, if user wishes to use cx for
evil-exchange or anything else.

MUST BE SET BEFORE EVIL-SNIPE IS LOADED.")

(defcustom evil-snipe-skip-leading-whitespace t
  "If non-nil, single char sniping (f/F/t/T) will skip over leading whitespaces
in a line (when you snipe for whitespace, e.g. f<space> or f<tab>)."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-tab-increment nil
  "If non-nil, pressing TAB while sniping will add another character to your
current search. For example, typing sab will search for 'ab'. In order to search
for 'abcd', you do sa<tab><tab>bcd.

If nil, TAB will search for literal tab characters."
  :group 'evil-snipe
  :type 'boolean)

(defcustom evil-snipe-char-fold nil
  "If non-nil, uses `char-fold-to-regexp' to include other ascii variants of a
search string. CURRENTLY EXPERIMENTAL.

e.g. The letter 'a' will match all of its accented cousins, even those composed
of multiple characters, as well as many other symbols like U+249C (PARENTHESIZED
LATIN SMALL LETTER A).

Only works in Emacs 25.1+."
  :group 'evil-snipe
  :type 'boolean)

;; State vars
(defvar evil-snipe--last nil)
(defvar evil-snipe--last-repeat nil)
(defvar evil-snipe--last-direction t)
(defvar evil-snipe--consume-match t)
(defvar evil-snipe--match-count 2
  "Number of characters to match.
Can be let-bound to create motions that search for N characters. Do not set
directly, unless you want to change the default number of characters to
search.")
(defvar evil-snipe--transient-map-func nil)


;;
;;; Faces

(defface evil-snipe-first-match-face '((t :inherit isearch))
  "Face for first match when sniping."
  :group 'evil-snipe)

(defface evil-snipe-matches-face '((t :inherit region))
  "Face for other matches when sniping."
  :group 'evil-snipe)


;;
;;; Helpers

(defun evil-snipe--case-p (data)
  "Return non-nil if a capital letter exists in DATA."
  (and evil-snipe-smart-case
       (let ((case-fold-search nil))
         (not (string-match-p "[A-Z]" (mapconcat #'cdr data ""))))))

(defun evil-snipe--process-key (key)
  (let ((keystr (char-to-string key)))
    (cons keystr (cond ((car (cdr (assoc key evil-snipe-aliases))))
                       (evil-snipe-char-fold (char-fold-to-regexp keystr))
                       (t (regexp-quote keystr))))))

(defun evil-snipe--collect-keys (&optional count forward-p)
  "Prompts for `evil-snipe--match-count' characters.

Number of characters is incremented with tab.
Backspace works for correcting yourself too.

COUNT determines the key interval and directionality. FORWARD-P can override
COUNT's directionality."
  (let ((echo-keystrokes 0) ; don't mess with the echo area, Emacs
        (count (or count 1))
        (i evil-snipe--match-count)
        keys)
    (unless forward-p
      (setq count (- count)))
    (catch 'abort
      (while (> i 0)
        (let* ((prompt (format "%d>%s" i (mapconcat #'char-to-string (reverse keys) "")))
               (key (evil-read-key (if evil-snipe-show-prompt prompt))))
          (cond
           ;; TAB adds more characters if `evil-snipe-tab-increment'
           ((and evil-snipe-tab-increment (eq key ?\t))  ;; TAB
            (cl-incf i))
           ;; Enter starts search with current chars
           ((memq key '(?\r ?\n))  ;; RET
            (throw 'abort (if (= i evil-snipe--match-count) 'repeat (reverse keys))))
           ;; Abort
           ((eq key ?\e)  ;; ESC
            (evil-snipe--cleanup)
            (throw 'abort 'abort))
           (t ; Otherwise, process key
            (cond ((eq key ?\d)  ; DEL (backspace) deletes a character
                   (cl-incf i)
                   (if (<= (length keys) 1)
                       (progn (evil-snipe--cleanup)
                              (throw 'abort 'abort))
                     (pop keys)))
                  (t ;; Otherwise add it
                   (push key keys)
                   (cl-decf i)))
            (when evil-snipe-enable-incremental-highlight
              (evil-snipe--cleanup)
              (evil-snipe--highlight-all count forward-p (mapcar #'evil-snipe--process-key (reverse keys)))
              (add-hook 'pre-command-hook #'evil-snipe--cleanup))))))
      (reverse keys))))

(defun evil-snipe--bounds (&optional forward-p count)
  "Return a cons cell containing (beg . end).

This represents the boundaries of the current search scope, determined from
`evil-snipe-scope'. If abs(COUNT) > 1, use `evil-snipe-spillover-scope'.

If FORWARD-P, expand scope from point onwards, otherwise do so backwards."
  (let* ((point+1 (1+ (point)))
         (evil-snipe-scope
          (or (and count (> (abs count) 1) evil-snipe-spillover-scope)
              evil-snipe-scope))
         (bounds (pcase evil-snipe-scope
                   (`line
                    (if forward-p
                        `(,point+1 . ,(line-end-position))
                      `(,(line-beginning-position) . ,(point))))
                   (`visible
                    (if forward-p
                        `(,point+1 . ,(1- (window-end)))
                      `(,(window-start) . ,(point))))
                   (`buffer
                    (if forward-p
                        `(,point+1 . ,(point-max))
                      `(,(point-min) . ,(point))))
                   (`whole-line
                    `(,(line-beginning-position) . ,(line-end-position)))
                   (`whole-visible
                    `(,(window-start) . ,(window-end)))
                   (`whole-buffer
                    `(,(point-min) . ,(point-max)))
                   (_
                    (error "Invalid scope: %s" evil-snipe-scope))))
         (end (cdr bounds)))
    (if (> (car bounds) end)
        (cons end end)
      bounds)))

(defun evil-snipe--highlight (beg end &optional first-p)
  "Highlight region between BEG and END.
If FIRST-P is t, then use `evil-snipe-first-p-match-face'"
  (when (and first-p (overlays-in beg end))
    (remove-overlays beg end 'category 'evil-snipe))
  (let ((overlay (make-overlay beg end nil nil nil)))
    (overlay-put overlay 'category 'evil-snipe)
    (overlay-put overlay 'face (if first-p
                                   'evil-snipe-first-match-face
                                 'evil-snipe-matches-face))
    overlay))

(defun evil-snipe--highlight-all (count forward-p data)
  "Highlight instances of keys in DATA at COUNT intervals.
Goes backward if FORWARD-P is nil."
  (let ((case-fold-search (evil-snipe--case-p data))
        (match (mapconcat #'cdr data ""))
        (bounds
         (let ((evil-snipe-scope
                (pcase evil-snipe-scope
                  (`whole-buffer 'whole-visible)
                  (`buffer 'visible)
                  (_ evil-snipe-scope))))
           (evil-snipe--bounds forward-p)))
        overlays)
    (save-excursion
      (goto-char (car bounds))
      (while (and (<= (point) (cdr bounds))
                  (re-search-forward match (cdr bounds) t 1))
        (let ((hl-beg (match-beginning 0))
              (hl-end (match-end 0)))
          (unless (or (invisible-p hl-beg)
                      (invisible-p hl-end))
            (cond ((and evil-snipe-skip-leading-whitespace
                        (looking-at-p "[ \t][ \t]+"))
                   (skip-chars-forward " \t")
                   (backward-char (- hl-end hl-beg)))
                  (t
                   (push (evil-snipe--highlight hl-beg hl-end)
                         overlays)))))))
    overlays))

(defun evil-snipe--cleanup ()
  "Disable overlays and clean up after evil-snipe."
  (when (or evil-snipe-local-mode evil-snipe-override-local-mode)
    (remove-overlays nil nil 'category 'evil-snipe)
    (remove-hook 'pre-command-hook #'evil-snipe--cleanup)))

(defun evil-snipe--disable-transient-map ()
  "Disable lingering transient map, if necessary."
  (when (functionp evil-snipe--transient-map-func)
    (funcall evil-snipe--transient-map-func)
    (setq evil-snipe--transient-map-func nil)))

(defun evil-snipe--transient-map (forward-key backward-key)
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map evil-snipe-parent-transient-map)
    (when evil-snipe-repeat-keys
      (define-key map forward-key  #'evil-snipe-repeat)
      (define-key map backward-key #'evil-snipe-repeat-reverse))
    map))


(defun evil-snipe-seek (count keys &optional keymap)
  "Perform a snipe with KEYS at COUNT intervals.
KEYS is a list of characters provided by <-c> and <+c> interactive codes. KEYMAP
is the transient map to activate afterwards."
  (pcase keys
    (`abort (setq evil-inhibit-operator t))
    ;; if <enter>, repeat last search
    (`repeat (if evil-snipe--last-direction
                 (evil-snipe-repeat count)
               (evil-snipe-repeat-reverse count)))
    ;; If KEYS is empty
    (`() (user-error "No keys provided!"))
    ;; Otherwise, perform the search
    (_
     (let ((data (mapcar #'evil-snipe--process-key keys)))
       (let ((case-fold-search (evil-snipe--case-p data))
             (count (or count (if evil-snipe--last-direction 1 -1)))
             (keymap (if (keymapp keymap) keymap)))
         (unless evil-snipe--last-repeat
           (setq evil-snipe--last (list count keys keymap
                                        evil-snipe--consume-match
                                        evil-snipe--match-count)))
         (evil-snipe--seek count data)
         (point))))))

(defun evil-snipe--seek-re (data scope count)
  (let ((regex (mapconcat #'cdr data ""))
        result)
    (when (and evil-snipe-skip-leading-whitespace
               (looking-at-p "[ \t]+")
               (string-match-p "^[ \t]+" (mapconcat #'car data "")))
      (setq regex (concat regex "[^ \t]")))
    (when (setq result (re-search-forward regex scope t count))
      (if (or (invisible-p (match-beginning 0))
              (invisible-p (1- (match-end 0))))
          (evil-snipe--seek-re data scope count)
        result))))

(defun evil-snipe--seek (count data &optional internal-p)
  "Perform a snipe and adjust cursor position depending on mode."
  (let ((orig-point (point))
        (forward-p (> count 0))
        (match (mapconcat #'cdr data "")))
    ;; Adjust search starting point
    (if forward-p (forward-char))
    (unless evil-snipe--consume-match
      (forward-char (if forward-p 1 -1)))
    (unwind-protect
        (cond ((cl-destructuring-bind (beg . end)
                   (evil-snipe--bounds forward-p count)
                 (evil-snipe--seek-re data (if forward-p end beg) count))
               ;; hi |
               (let ((beg (match-beginning 0))
                     (end (match-end 0))
                     (len (length (match-string 0)))
                     (evil-op-p (evil-operator-state-p))
                     (evil-vs-p (evil-visual-state-p)))
                 (when (and evil-snipe-skip-leading-whitespace
                            (string-match-p "^[ \t]+" (mapconcat #'car data "")))
                   (setq end (1- end)
                         len (1- len)))
                 ;; Adjust cursor end position
                 (if (not forward-p)
                     (goto-char (if evil-snipe--consume-match beg end))
                   (goto-char (if evil-vs-p
                                  (if evil-snipe--consume-match end beg)
                                (if evil-op-p end beg)))
                   (if evil-snipe--consume-match
                       (if evil-vs-p (backward-char))
                     (backward-char len)
                     (when (and (> len 1) (not evil-op-p))
                       (forward-char))))
                 ;; Follow the cursor
                 (when evil-snipe-auto-scroll
                   (save-excursion
                     (if (or (> (window-start) (point))
                             (< (window-end)   (point)))
                         (recenter)
                       (evil-scroll-line-down
                        (- (line-number-at-pos)
                           (line-number-at-pos orig-point))))))
                 (unless evil-op-p
                   (unless evil-vs-p
                     ;; Highlight first result (but not in operator/visual mode)
                     (when evil-snipe-enable-highlight
                       (evil-snipe--highlight beg end t)))
                   ;; Activate the repeat keymap
                   (when (and (boundp 'keymap) keymap)
                     (evil-snipe--disable-transient-map)
                     (setq evil-snipe--transient-map-func
                           (set-transient-map keymap))))))

              ;; Try to "spill over" into new scope on failed search
              (evil-snipe-spillover-scope
               (let ((evil-snipe-scope evil-snipe-spillover-scope)
                     evil-snipe-spillover-scope)
                 (evil-snipe--seek count data t))
               (setq internal-p t))

              ;; If, at last, it fails...
              (t
               (goto-char orig-point)
               (when (and evil-snipe--last-repeat (boundp 'keymap) keymap)
                 (evil-snipe--disable-transient-map)
                 (setq evil-snipe--transient-map-func
                       (set-transient-map keymap)))
               (user-error "Can't find %s" ; show invisible keys
                           (replace-regexp-in-string
                            "\t" "<TAB>"
                            (replace-regexp-in-string
                             "\s" "<SPC>"
                             (mapconcat #'car data ""))))))
      (unless internal-p
        (when evil-snipe-enable-highlight
          (evil-snipe--highlight-all count forward-p data))
        (add-hook 'pre-command-hook #'evil-snipe--cleanup)))
    (point)))

(evil-define-motion evil-snipe-repeat (count)
  "Repeat the last evil-snipe COUNT times."
  (interactive "<c>")
  (unless evil-snipe--last
    (user-error "Nothing to repeat"))
  (let ((last-count (nth 0 evil-snipe--last))
        (last-keys (nth 1 evil-snipe--last))
        (last-keymap (nth 2 evil-snipe--last))
        (last-consume-match (nth 3 evil-snipe--last))
        (last-match-count (nth 4 evil-snipe--last))
        (evil-snipe--last-repeat t)
        (evil-snipe-scope (or evil-snipe-repeat-scope evil-snipe-scope)))
    (let ((evil-snipe--consume-match last-consume-match)
          (evil-snipe--match-count last-match-count))
      (evil-snipe-seek (* (or count 1) (or last-count 1))
                       last-keys last-keymap))))

(evil-define-motion evil-snipe-repeat-reverse (count)
  "Repeat the inverse of the last evil-snipe `count' times"
  (interactive "<c>")
  (evil-snipe-repeat (or (and (integerp count) (- count)) -1)))

;;;###autoload
(cl-defmacro evil-snipe-def (n type forward-key backward-key
                               &key forward-fn backward-fn)
  "Define a N char snipe and bind it to FORWARD-KEY and BACKWARD-KEY.
TYPE can be inclusive or exclusive. Specify FORWARD-FN and/or BACKWARD-FN to
explicitly choose the function names."
  (let ((forward-fn  (or forward-fn
                         (intern (format "evil-snipe-%s" forward-key))))
        (backward-fn (or backward-fn
                         (intern (format "evil-snipe-%s" backward-key)))))
    `(progn
       (evil-define-motion ,forward-fn (count keys)
         ,(concat "Jumps to the next " (int-to-string n)
                  "-char match COUNT matches away. Including KEYS is a list of character codes.")
         :jump t
         (interactive
          (let ((count (if current-prefix-arg (prefix-numeric-value current-prefix-arg))))
            (list (progn (setq evil-snipe--last-direction t) count)
                  (let ((evil-snipe--match-count ,n))
                    (evil-snipe--collect-keys count evil-snipe--last-direction)))))
         (if (eq keys 'abort) (user-error "Aborted"))
         (let ((evil-snipe--consume-match (eq ,type 'inclusive)))
           (evil-snipe-seek
            count keys (evil-snipe--transient-map ,forward-key ,backward-key))))

       (evil-define-motion ,backward-fn (count keys)
         ,(concat "Performs an backwards `" (symbol-name forward-fn) "'.")
         :jump t
         (interactive
          (let ((count (when current-prefix-arg (prefix-numeric-value current-prefix-arg))))
            (list (progn (setq evil-snipe--last-direction nil) count)
                  (let ((evil-snipe--match-count ,n))
                    (evil-snipe--collect-keys count evil-snipe--last-direction)))))
         (if (eq keys 'abort) (user-error "Aborted"))
         (let ((evil-snipe--consume-match (eq ,type 'inclusive)))
           (evil-snipe-seek
            (or (and count (- count)) -1) keys
            (evil-snipe--transient-map ,forward-key ,backward-key)))))))

;;;###autoload (autoload 'evil-snipe-s "evil-snipe" nil t)
;;;###autoload (autoload 'evil-snipe-S "evil-snipe" nil t)
(evil-snipe-def 2 'inclusive "s" "S")

;;;###autoload (autoload 'evil-snipe-x "evil-snipe" nil t)
;;;###autoload (autoload 'evil-snipe-X "evil-snipe" nil t)
(evil-snipe-def 2 'exclusive "x" "X")

;;;###autoload (autoload 'evil-snipe-f "evil-snipe" nil t)
;;;###autoload (autoload 'evil-snipe-F "evil-snipe" nil t)
(evil-snipe-def 1 'inclusive "f" "F")

;;;###autoload (autoload 'evil-snipe-t "evil-snipe" nil t)
;;;###autoload (autoload 'evil-snipe-T "evil-snipe" nil t)
(evil-snipe-def 1 'exclusive "t" "T")


(defvar evil-snipe-local-mode-map
  (let ((map (make-sparse-keymap)))
    (evil-define-key* '(normal motion) map
      "s" #'evil-snipe-s
      "S" #'evil-snipe-S)
    (if evil-snipe-use-vim-sneak-bindings
        (evil-define-key* 'operator map
          "z" #'evil-snipe-x
          "Z" #'evil-snipe-X)
      (evil-define-key* 'operator map
        "z" #'evil-snipe-s
        "Z" #'evil-snipe-S
        "x" #'evil-snipe-x
        "X" #'evil-snipe-X))
    map))

(defvar evil-snipe-override-local-mode-map
  (let ((map (make-sparse-keymap)))
    (evil-define-key* 'motion map
      "f" #'evil-snipe-f
      "F" #'evil-snipe-F
      "t" #'evil-snipe-t
      "T" #'evil-snipe-T)
    (when evil-snipe-override-evil-repeat-keys
      (evil-define-key* 'motion map
        ";" #'evil-snipe-repeat
        "," #'evil-snipe-repeat-reverse))
    map))

(defvar evil-snipe-parent-transient-map
  (let ((map (make-sparse-keymap)))
    (define-key map ";" #'evil-snipe-repeat)
    (define-key map "," #'evil-snipe-repeat-reverse)
    map))

;;;###autoload
(defun turn-on-evil-snipe-mode ()
  "Enable evil-snipe-mode in the current buffer."
  (unless (apply #'derived-mode-p evil-snipe-disabled-modes)
    (evil-snipe-local-mode +1)))

;;;###autoload
(defun turn-on-evil-snipe-override-mode ()
  "Enable evil-snipe-mode in the current buffer."
  (unless (apply #'derived-mode-p evil-snipe-disabled-modes)
    (evil-snipe-override-local-mode +1)))

;;;###autoload
(defun turn-off-evil-snipe-mode ()
  "Disable `evil-snipe-local-mode' in the current buffer."
  (evil-snipe-local-mode -1))

;;;###autoload
(defun turn-off-evil-snipe-override-mode ()
  "Disable evil-snipe-override-mode in the current buffer."
  (evil-snipe-override-local-mode -1))

(when (fboundp 'advice-add)
  (advice-add #'evil-force-normal-state :before #'evil-snipe--cleanup))
(add-hook 'evil-insert-state-entry-hook #'evil-snipe--disable-transient-map)

(defvar evil-snipe--keymaps-init nil)
(defun evil-snipe--normalize-keymaps ()
  (unless evil-snipe--keymaps-init
    (set (make-local-variable 'evil-snipe--keymaps-init)
         (and (evil-normalize-keymaps) t))))

;;;###autoload
(define-minor-mode evil-snipe-local-mode
  "Enable `evil-snipe' in the current buffer."
  :lighter " snipe"
  :group 'evil-snipe
  (if evil-snipe-local-mode (evil-snipe--normalize-keymaps)))

;;;###autoload
(define-minor-mode evil-snipe-override-local-mode
  "Override evil-mode's f/F/t/T/;/, motions."
  :group 'evil-snipe
  (if evil-snipe-override-local-mode (evil-snipe--normalize-keymaps)))

;;;###autoload
(define-globalized-minor-mode evil-snipe-mode
  evil-snipe-local-mode turn-on-evil-snipe-mode)

;;;###autoload
(define-globalized-minor-mode evil-snipe-override-mode
  evil-snipe-override-local-mode turn-on-evil-snipe-override-mode)

(define-obsolete-variable-alias 'evil-snipe-mode-map 'evil-snipe-local-mode-map "2.0.8")
(define-obsolete-variable-alias 'evil-snipe-override-mode-map 'evil-snipe-override-local-mode-map "2.0.8")

(provide 'evil-snipe)
;;; evil-snipe.el ends here
