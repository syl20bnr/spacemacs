;;; evil-vars.el --- Settings and variables -*- lexical-binding: t -*-

;; Author: Vegard Øye <vegard_oye at hotmail.com>
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>

;; Version: 1.15.0

;;
;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is part of Evil.
;;
;; Evil is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Evil is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Evil.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(declare-function evil-add-command-properties "evil-common"
                  (command &rest properties))
(declare-function evil-update-insert-state-bindings "evil-maps"
                  (&optional _option-name remove force))

;;; Hooks

(defvar evil-after-load-hook nil
  "Functions to be run when loading of Evil is finished.
This hook can be used to execute some initialization routines
when Evil is completely loaded.")

(defvar evil-after-global-hook nil
  "Functions to be run after :global and :vglobal.")

(defcustom evil-goto-definition-functions
  '(evil-goto-definition-imenu
    evil-goto-definition-semantic
    evil-goto-definition-xref
    evil-goto-definition-search)
  "List of functions run until success by `evil-goto-definition'."
  :type 'hook
  :group 'evil)

;;; Initialization

(defvar evil-pending-custom-initialize nil
  "A list of pending initializations for custom variables.
Each element is a triple (FUNC VAR VALUE). When Evil is
completely loaded then the functions (funcall FUNC VAR VALUE) is
called for each element. FUNC should be a function suitable for
the :initialize property of `defcustom'.")

(defun evil-custom-initialize-pending-reset (var value)
  "Add a pending customization with `custom-initialize-reset'."
  (push (list 'custom-initialize-reset var value)
        evil-pending-custom-initialize))

(defun evil-run-pending-custom-initialize ()
  "Execute the pending initializations.
See `evil-pending-custom-initialize'."
  (dolist (init evil-pending-custom-initialize)
    (apply (car init) (cdr init)))
  (remove-hook 'evil-after-load-hook #'evil-run-pending-custom-initialize))
(add-hook 'evil-after-load-hook #'evil-run-pending-custom-initialize)

;;; Setters

(defun evil-set-toggle-key (key)
  "Set `evil-toggle-key' to KEY.
KEY must be readable by `read-kbd-macro'."
  (let ((old-key (read-kbd-macro
                  (if (boundp 'evil-toggle-key)
                      evil-toggle-key
                    "C-z")))
        (key (read-kbd-macro key)))
    (with-no-warnings
      (dolist (pair '((evil-motion-state-map evil-emacs-state)
                      (evil-insert-state-map evil-emacs-state)
                      (evil-emacs-state-map evil-exit-emacs-state)))
        (when (boundp (car pair))
          (let ((map (symbol-value (car pair)))
                (fun (cadr pair)))
            (when (keymapp map)
              (define-key map key fun)
              (define-key map old-key nil))))))))

(defun evil-set-custom-state-maps (var pending-var key _make newlist)
  "Change the list of special keymaps.
VAR         is the variable containing the list of keymaps.
PENDING-VAR is the variable containing the list of the currently pending
            keymaps.
KEY         the special symbol to be stored in the keymaps.
MAKE        the creation function of the special keymaps.
NEWLIST     the list of new special keymaps."
  (set-default pending-var newlist)
  (when (default-boundp var)
    (dolist (map (default-value var))
      (when (and (boundp (car map))
                 (keymapp (default-value (car map))))
        (define-key (default-value (car map)) (vector key) nil))))
  (set-default var newlist)
  (evil-update-pending-maps))

(defun evil-update-pending-maps (&optional _file)
  "Try to set pending special keymaps.
This function should be called from an `after-load-functions'
hook."
  (let ((maps '((evil-make-overriding-map . evil-pending-overriding-maps)
                (evil-make-intercept-map . evil-pending-intercept-maps))))
    (while maps
      (let* ((map (pop maps))
             (make (car map))
             (pending-var (cdr map))
             (pending (symbol-value pending-var))
             newlist)
        (while pending
          (let* ((map (pop pending))
                 (kmap (and (boundp (car map))
                            (keymapp (symbol-value (car map)))
                            (symbol-value (car map))))
                 (state (cdr map)))
            (if kmap
                (funcall make kmap state)
              (push map newlist))))
        (set-default pending-var newlist)))))

(defun evil-set-visual-newline-commands (var value)
  "Set the value of `evil-visual-newline-commands'.
Setting this variable changes the properties of the appropriate
commands."
  (with-no-warnings
    (when (default-boundp var)
      (dolist (cmd (default-value var))
        (evil-set-command-property cmd :exclude-newline nil)))
    (set-default var value)
    (dolist (cmd (default-value var))
      (evil-set-command-property cmd :exclude-newline t))))

(defun evil-set-custom-motions (var values)
  "Set the list of motion commands."
  (with-no-warnings
    (when (default-boundp var)
      (dolist (motion (default-value var))
        (evil-add-command-properties motion :keep-visual nil :repeat nil)))
    (set-default var values)
    (mapc #'evil-declare-motion (default-value var))))

;;; Customization group

(defgroup evil nil
  "Extensible vi layer."
  :group 'emulations
  :prefix 'evil-)

(defcustom evil-auto-indent t
  "\\<evil-normal-state-map>
Whether to auto-indent when opening lines with \\[evil-open-below] \
and \\[evil-open-above]."
  :type  'boolean
  :group 'evil)

(defcustom evil-shift-width 4
  "\\<evil-normal-state-map>
The number of columns by which a line is shifted.
This applies to the shifting operators \\[evil-shift-right] and \
\\[evil-shift-left]."
  :type 'integer
  :group 'evil)

(defcustom evil-shift-round t
  "\\<evil-normal-state-map>
Whether shifting rounds to the nearest multiple.
If non-nil, \\[evil-shift-right] and \\[evil-shift-left] adjust line
indentation to the nearest multiple of `evil-shift-width'."
  :type 'boolean
  :group 'evil)

(defcustom evil-indent-convert-tabs t
  "\\<evil-normal-state-map>
If non-nil, the \\[evil-indent] operator converts between leading tabs and spaces.
Whether tabs are converted to spaces or vice versa depends on the
value of `indent-tabs-mode'."
  :type 'boolean
  :group 'evil)

(defcustom evil-default-cursor t
  "The default cursor.
May be a cursor type as per `cursor-type', a color string as passed
to `set-cursor-color', a zero-argument function for changing the
cursor, or a list of the above."
  :type '(set symbol (cons symbol symbol) string function)
  :group 'evil)

(defvar evil-force-cursor nil
  "Overwrite the current states default cursor.")

(defcustom evil-start-of-line nil
  "Analogue of vim's `startofline'.
If nil, preserve column when making relevant movements of the cursor.
Otherwise, move the cursor to the start of the line."
  :type 'boolean
  :group 'evil)

(defcustom evil-repeat-move-cursor t
  "\\<evil-normal-state-map>
Whether repeating commands with \\[evil-repeat] may move the cursor.
If nil, the original cursor position is preserved, even if the command
normally would have moved the cursor."
  :type 'boolean
  :group 'evil)

(defcustom evil-cross-lines nil
  "\\<evil-motion-state-map>
Whether horizontal motions may move to other lines.  If non-nil,
certain motions that conventionally operate in a single line may move
the cursor to other lines.  Otherwise, they are restricted to the
current line.  This applies to \\[evil-backward-char], \
\\[evil-forward-char], \\[evil-find-char], \
\\[evil-find-char-backward], \\[evil-find-char-to], \
\\[evil-find-char-to-backward], \
\\<evil-normal-state-map>\\[evil-invert-char]."
  :type 'boolean
  :group 'evil)

(defcustom evil-backspace-join-lines t
  "Whether backward delete in insert state may join lines."
  :type 'boolean
  :group 'evil)

(defcustom evil-move-cursor-back t
  "Whether the cursor is moved backwards when exiting insert state.
If non-nil, the cursor moves \"backwards\" when exiting insert state,
so that it ends up on the character to the left.  Otherwise it remains
in place, on the character to the right.

See also `evil-move-beyond-eol'."
  :type 'boolean
  :group 'evil)

(defcustom evil-move-beyond-eol nil
  "Whether the cursor can move past the end of the line.
If non-nil, the cursor is allowed to move one character past the
end of the line, as in Emacs."
  :type 'boolean
  :group 'evil)

(defcustom evil-respect-visual-line-mode nil
  "\\<evil-motion-state-map>
Whether movement commands respect `visual-line-mode'.
If non-nil, `visual-line-mode' is generally respected when it is
on.  In this case, motions such as \\[evil-next-line] and
\\[evil-previous-line] navigate by visual lines (on the screen) rather
than \"physical\" lines (defined by newline characters).  If nil,
the setting of `visual-line-mode' is ignored.

This variable must be set before Evil is loaded."
  :type 'boolean
  :group 'evil)

(defcustom evil-repeat-find-to-skip-next t
  "Whether a repeat of t or T should skip an adjacent character."
  :type 'boolean
  :group 'evil)

(defcustom evil-kbd-macro-suppress-motion-error nil
  "\\<evil-motion-state-map>
Whether left/right motions signal errors in keyboard macros.
This variable only affects beginning-of-line or end-of-line errors
regarding the motions \\[evil-backward-char] and \\[evil-forward-char]
respectively.  This may be desired since such errors cause macro
definition or execution to be terminated.  There are four
possibilities:

- `record': errors are suppressed when recording macros, but not when
  replaying them.
- `replay': errors are suppressed when replaying macros, but not when
  recording them.
- `t': errors are suppressed in both cases.
- `nil': errors are never suppressed."
  :type '(radio (const :tag "No" :value nil)
                (const :tag "Record" :value record)
                (const :tag "Replay" :value replay)
                (const :tag "Both" :value t))
  :group 'evil)

(defcustom evil-track-eol t
  "\\<evil-motion-state-map>
Whether \\[evil-end-of-line] \"sticks\" the cursor to the end of the line.
If non-nil, vertical motions after \\[evil-end-of-line] maintain the cursor at the
end of the line, even if the target line is longer.  This is analogous
to `track-eol', but respects Evil's interpretation of end-of-line."
  :type 'boolean
  :group 'evil)

(defcustom evil-mode-line-format 'before
  "The position of the state tag in the mode line.
If set to `before' or `after', the tag is placed at the beginning
or the end of the mode-line, respectively.  If nil, there is no
tag.  Otherwise it should be a cons cell (WHERE . WHICH), where
WHERE is either `before' or `after', and WHICH is a symbol in
`mode-line-format'.  The tag is then placed before or after that
symbol, respectively."
  :type '(radio :value 'before
                (const :tag "No tag" nil)
                (const before)
                (const after)
                (cons :tag "Next to symbol"
                      (choice :value after
                              (const before)
                              (const after))
                      symbol))
  :group 'evil)

(defcustom evil-mouse-word 'evil-word
  "The thing-at-point symbol for double click selection.
The double-click starts visual state in a special word selection
mode. This symbol is used to determine the words to be
selected. Possible values are `evil-word' or `evil-WORD'."
  :type 'symbol
  :group 'evil)

(defcustom evil-bigword "^ \t\r\n"
  "The set of characters to be interpreted as WORD boundaries.
This is enclosed with square brackets and used as a regular
expression.  By default, whitespace characters are considered
WORD boundaries."
  :type 'string
  :group 'evil)

(defcustom evil-want-fine-undo nil
  "Whether actions are undone in several steps.
There are two possible choices: nil (\"no\") means that all
changes made during insert state, including a possible delete
after a change operation, are collected in a single undo step.
Non-nil (\"yes\") means that undo steps are determined according
to Emacs heuristics, and no attempt is made to aggregate changes.

For backward compatibility purposes, the value `fine' is
interpreted as `nil'.  This option was removed because it did not
work consistently."
  :type '(radio (const :tag "No" :value nil)
                (const :tag "Fine (obsolete)" :value fine)
                (const :tag "Yes" :value t))
  :group 'evil)

(defcustom evil-regexp-search t
  "\\<evil-motion-state-map>
Whether to use regular expressions for searching in \
\\[evil-search-forward] and \\[evil-search-backward]."
  :type  'boolean
  :group 'evil)

(defcustom evil-search-wrap t
  "\\<evil-motion-state-map>
Whether search with \\[evil-search-forward] and \
\\[evil-search-backward] wraps around the buffer.
If this is non-nil, search stops at the buffer boundaries."
  :type  'boolean
  :group 'evil)

(defcustom evil-flash-delay 2
  "\\<evil-motion-state-map>
Time in seconds to flash search matches after \\[evil-search-next] and \
\\[evil-search-previous]."
  :type  'number
  :group 'evil)

(defcustom evil-auto-balance-windows t
  "If non-nil window creation and deletion trigger rebalancing."
  :type 'boolean
  :group 'evil)

(defcustom evil-split-window-below nil
  "If non-nil split windows are created below."
  :type 'boolean
  :group 'evil)

(defcustom evil-vsplit-window-right nil
  "If non-nil vertically split windows with are created to the right."
  :type 'boolean
  :group 'evil)

(defcustom evil-esc-delay 0.01
  "The time, in seconds, to wait for another key after escape.
If no further event arrives during this time, the event is
translated to `ESC'.  Otherwise, it is translated according to
`input-decode-map'.  This does not apply in Emacs state, and may
also be inhibited by setting `evil-inhibit-esc'."
  :type 'number
  :group 'evil)

(defvar evil-esc-mode nil
  "Non-nil if `evil-esc-mode' is enabled.")

(defvar evil-esc-map nil
  "Original ESC prefix map in `input-decode-map'.
Used by `evil-esc-mode'.")

(defvar evil-inhibit-esc nil
  "If non-nil, the \"\\e\" event will never be translated to `escape'.")

(defcustom evil-intercept-esc 'always
  "Whether Evil should intercept the escape key.
In the terminal, escape and a meta key sequence both generate the
same event.  In order to distingush these, Evil uses
`input-decode-map'.  It is not necessary to do this in a graphical
Emacs session.  However, if you prefer to use \"C-[\" as escape (which
is identical to the terminal escape key code), this interception must
also happen in graphical Emacs sessions.  Set this variable to
`always', t (only in the terminal) or nil (never intercept)."
  :type '(radio (const :tag "Never" :value nil)
                (const :tag "In terminal only" :value t)
                (const :tag "Always" :value always))
  :group 'evil)

(defcustom evil-show-paren-range 0
  "The maximum distance between point and a parenthesis
which causes the parenthesis to be highlighted."
  :type 'integer
  :group 'evil)

(defcustom evil-ex-hl-update-delay 0.02
  "Time in seconds of idle before updating search highlighting.
Setting this to a period shorter than that of keyboard's repeat
rate allows highlights to update while scrolling."
  :type 'number
  :group 'evil)

(defcustom evil-highlight-closing-paren-at-point-states
  '(not emacs insert replace)
  "The states in which the closing parenthesis at point should be highlighted.
All states listed here highlight the closing parenthesis at
point (which is Vim's default behavior).  All others highlight the
parenthesis before point (which is Emacs default behavior). If
this list contains the symbol `not' then its meaning is inverted,
i.e. all states listed here highlight the closing parenthesis
before point."
  :type '(repeat symbol)
  :group 'evil)

(defcustom evil-kill-on-visual-paste t
  "Whether pasting in visual state adds the replaced text to the
kill ring, making it the default for the next paste. The default
replicates the default Vim behavior for `p'. This value is flipped by
`evil-paste-before' (\\[evil-paste-before])."
  :type 'boolean
  :group 'evil)

(defcustom evil-want-C-i-jump t
  "Whether `C-i' jumps forward in the jump list (like Vim).
Otherwise, `C-i' inserts a tab character."
  :type 'boolean
  :group 'evil
  :set #'(lambda (sym value)
           (set-default sym value)
           (when (boundp 'evil-motion-state-map)
             (cond
              ((and (not value)
                    (eq (lookup-key evil-motion-state-map (kbd "C-i"))
                        'evil-jump-forward))
               (define-key evil-motion-state-map (kbd "C-i") nil))
              ((and value
                    (not (lookup-key evil-motion-state-map (kbd "C-i"))))
               (define-key evil-motion-state-map (kbd "C-i")
                           'evil-jump-forward))))))

(defcustom evil-want-C-u-scroll nil
  "Whether `C-u' scrolls up (like Vim).
Otherwise, `C-u' applies a prefix argument.  The binding of
`C-u' mirrors Emacs behaviour by default due to the relative
ubiquity of prefix arguments."
  :type 'boolean
  :group 'evil
  :set #'(lambda (sym value)
           (set-default sym value)
           (when (boundp 'evil-motion-state-map)
             (cond
              ((and (not value)
                    (eq (lookup-key evil-motion-state-map (kbd "C-u"))
                        'evil-scroll-up))
               (define-key evil-motion-state-map (kbd "C-u") nil))
              ((and value
                    (not (lookup-key evil-motion-state-map (kbd "C-u"))))
               (define-key evil-motion-state-map (kbd "C-u")
                           'evil-scroll-up))))))

(defcustom evil-want-C-d-scroll t
  "Whether `C-d' scrolls down (like Vim)."
  :type 'boolean
  :group 'evil
  :set #'(lambda (sym value)
           (set-default sym value)
           (when (boundp 'evil-motion-state-map)
             (cond
              ((and (not value)
                    (eq (lookup-key evil-motion-state-map (kbd "C-d"))
                        'evil-scroll-down))
               (define-key evil-motion-state-map (kbd "C-d") nil))
              ((and value
                    (not (lookup-key evil-motion-state-map (kbd "C-d"))))
               (define-key evil-motion-state-map (kbd "C-d")
                           'evil-scroll-down))))))

(defcustom evil-want-C-u-delete nil
  "Whether `C-u' deletes back to indentation in insert state.
Otherwise, `C-u' applies a prefix argument.  The binding of
`C-u' mirrors Emacs behaviour by default due to the relative
ubiquity of prefix arguments."
  :type 'boolean
  :group 'evil
  :set #'(lambda (sym value)
           (set-default sym value)
           (when (and (boundp 'evil-insert-state-map)
                      (boundp 'evil-replace-state-map))
             (cond
              ((and (not value)
                    (eq (lookup-key evil-insert-state-map (kbd "C-u"))
                        'evil-delete-back-to-indentation))
               (define-key evil-insert-state-map (kbd "C-u") nil)
               (define-key evil-replace-state-map (kbd "C-u") nil))
              ((and value
                    (not (lookup-key evil-insert-state-map (kbd "C-u"))))
               (define-key evil-insert-state-map (kbd "C-u")
                           'evil-delete-back-to-indentation)
               (define-key evil-replace-state-map (kbd "C-u")
                           'evil-delete-back-to-indentation))))))

(defcustom evil-want-C-w-delete t
  "Whether `C-w' deletes a word in Insert/Ex/Search state."
  :type 'boolean
  :group 'evil
  :set (lambda (sym value)
         (set-default sym value)
         (when (and (boundp 'evil-insert-state-map)
                    (boundp 'evil-replace-state-map))
           (cond
            ((and (not value)
                  (eq (lookup-key evil-insert-state-map (kbd "C-w"))
                      'evil-delete-backward-word))
             (define-key evil-insert-state-map (kbd "C-w") 'evil-window-map)
             (define-key evil-replace-state-map (kbd "C-w") 'evil-window-map))
            ((and value
                  (eq (lookup-key evil-insert-state-map (kbd "C-w"))
                      'evil-window-map))
             (define-key evil-insert-state-map (kbd "C-w") 'evil-delete-backward-word)
             (define-key evil-replace-state-map (kbd "C-w") 'evil-delete-backward-word))))
         (when (boundp 'evil-command-line-map)
           (cond
            ((and (not value)
                  (eq (lookup-key evil-command-line-map (kbd "C-w"))
                      #'backward-kill-word))
             (define-key evil-command-line-map (kbd "C-w") nil))
            ((and value
                  (null (lookup-key evil-command-line-map (kbd "C-w"))))
             (define-key evil-command-line-map (kbd "C-w") #'backward-kill-word))))
         (when (boundp 'evil-ex-search-keymap)
           (cond
            ((and (not value)
                  (null (lookup-key evil-ex-search-keymap (kbd "C-w"))))
             (define-key evil-ex-search-keymap (kbd "C-w") 'evil-search-yank-word))
            ((and value
                  (eq (lookup-key evil-ex-search-keymap (kbd "C-w"))
                      'evil-search-yank-word))
             (define-key evil-ex-search-keymap (kbd "C-w") nil))))))

(defcustom evil-want-C-h-delete nil
  "Whether `C-h' deletes a char in Insert state."
  :type 'boolean
  :group 'evil
  :set #'(lambda (sym value)
           (set-default sym value)
           (when (and (boundp 'evil-insert-state-map)
                      (boundp 'evil-replace-state-map))
             (cond
              ((and (not value)
                    (eq (lookup-key evil-insert-state-map (kbd "C-h"))
                        'evil-delete-backward-char-and-join))
               (define-key evil-insert-state-map (kbd "C-h") nil)
               (define-key evil-replace-state-map (kbd "C-h") nil))
              ((and value
                    (not (lookup-key evil-insert-state-map (kbd "C-h"))))
               (define-key evil-insert-state-map (kbd "C-h")
                           'evil-delete-backward-char-and-join)
               (define-key evil-replace-state-map (kbd "C-h")
                           'evil-replace-backspace))))))

(defcustom evil-want-C-g-bindings nil
  "Whether `C-g' postfix can be used in bindings."
  :type 'boolean
  :group 'evil)

(defcustom evil-want-C-w-in-emacs-state nil
  "Whether `C-w' prefixes windows commands in Emacs state."
  :type 'boolean
  :group 'evil
  :set #'(lambda (sym value)
           (set-default sym value)
           (when (boundp 'evil-emacs-state-map)
             (cond
              ((and (not value)
                    (eq (lookup-key evil-emacs-state-map (kbd "C-w"))
                        'evil-window-map))
               (define-key evil-emacs-state-map (kbd "C-w") nil))
              ((and value
                    (not (lookup-key evil-emacs-state-map (kbd "C-w"))))
               (define-key evil-emacs-state-map (kbd "C-w") 'evil-window-map))))))

(defcustom evil-want-change-word-to-end t
  "Whether `cw' behaves like `ce'."
  :type 'boolean
  :group 'evil)

(defcustom evil-want-Y-yank-to-eol nil
  "Whether `Y' yanks to the end of the line.
The default behavior is to yank the whole line, like Vim."
  :group 'evil
  :type 'boolean
  :initialize #'evil-custom-initialize-pending-reset
  :set #'(lambda (sym value)
           (set-default sym value)
           (evil-add-command-properties
            'evil-yank-line
            :motion (if value
                        'evil-end-of-line-or-visual-line
                      'evil-line-or-visual-line))))

(defcustom evil-disable-insert-state-bindings nil
  "Whether insert state bindings should be used.
Bindings for escape, delete and `evil-toggle-key' are always
available. If this is non-nil, default Emacs bindings are by and
large accessible in insert state."
  :group 'evil
  :type 'boolean
  :initialize #'evil-custom-initialize-pending-reset
  :set #'(lambda (sym value)
           (set-default sym value)
           (evil-update-insert-state-bindings sym value)))

(defcustom evil-echo-state t
  "Whether to signal the current state in the echo area."
  :type 'boolean
  :group 'evil)

(defcustom evil-complete-all-buffers t
  "\\<evil-insert-state-map>
Whether completion looks for matches in all buffers.
This applies to \\[evil-complete-next] and \\[evil-complete-previous] \
in insert state."
  :type 'boolean
  :group 'evil)

(defcustom evil-search-wrap-ring-bell nil
  "Whether to ring the bell when search wraps around the buffer."
  :type  'boolean
  :group 'evil)

(defvar dabbrev-search-these-buffers-only)
(defvar dabbrev-case-distinction)
(defcustom evil-complete-next-func
  #'(lambda (arg)
      (require 'dabbrev)
      (let ((dabbrev-search-these-buffers-only
             (unless evil-complete-all-buffers
               (list (current-buffer))))
            dabbrev-case-distinction)
        (condition-case nil
            (if (eq last-command this-command)
                (dabbrev-expand nil)
              (dabbrev-expand (- (abs (or arg 1)))))
          (error (dabbrev-expand nil)))))
  "Completion function used by \
\\<evil-insert-state-map>\\[evil-complete-next]."
  :type 'function
  :group 'evil)

(defcustom evil-complete-previous-func
  #'(lambda (arg)
      (require 'dabbrev)
      (let ((dabbrev-search-these-buffers-only
             (unless evil-complete-all-buffers
               (list (current-buffer))))
            dabbrev-case-distinction)
        (dabbrev-expand arg)))
  "Completion function used by \
\\<evil-insert-state-map>\\[evil-complete-previous]."
  :type 'function
  :group 'evil)

(defcustom evil-complete-next-minibuffer-func 'minibuffer-complete
  "Minibuffer completion function used by \
\\<evil-insert-state-map>\\[evil-complete-next]."
  :type 'function
  :group 'evil)

(defcustom evil-complete-previous-minibuffer-func 'minibuffer-complete
  "Minibuffer completion function used by \
\\<evil-insert-state-map>\\[evil-complete-previous]."
  :type 'function
  :group 'evil)

(defcustom evil-complete-next-line-func
  #'(lambda (arg)
      (let ((hippie-expand-try-functions-list
             '(try-expand-line
               try-expand-line-all-buffers)))
        (hippie-expand arg)))
  "Minibuffer completion function used by \
\\<evil-insert-state-map>\\[evil-complete-next-line]."
  :type 'function
  :group 'evil)

(defcustom evil-complete-previous-line-func
  evil-complete-next-line-func
  "Minibuffer completion function used by \
\\<evil-insert-state-map>\\[evil-complete-previous-line]."
  :type 'function
  :group 'evil)

(defcustom evil-lookup-func #'woman
  "Lookup function used by \
\"\\<evil-motion-state-map>\\[evil-lookup]\"."
  :type 'function
  :group 'evil)

(defcustom evil-toggle-key "C-z"
  "The key used to change to and from Emacs state.
Must be readable by `read-kbd-macro'. For example: \"C-z\"."
  :type 'string
  :group 'evil
  :set #'(lambda (sym value)
           (evil-set-toggle-key value)
           (set-default sym value)))

(defcustom evil-default-state 'normal
  "The default Evil state.
This is the state a buffer starts in when it is not otherwise
configured (see `evil-set-initial-state' and
`evil-buffer-regexps').  The value may be one of `normal',
`insert', `visual', `replace', `operator', `motion' and `emacs'."
  :type  'symbol
  :group 'evil)

(defcustom evil-buffer-regexps
  '(("^ \\*load\\*" . nil))
  "Regular expressions determining the initial state for a buffer.
Entries have the form (REGEXP . STATE), where REGEXP is a regular
expression matching the buffer's name and STATE is one of `normal',
`insert', `visual', `replace', `operator', `motion', `emacs' and
`nil'.  If STATE is `nil', Evil is disabled in the buffer."
  :type '(alist :key-type string :value-type symbol)
  :group 'evil)

(defcustom evil-emacs-state-modes
  '(5x5-mode
    archive-mode
    bbdb-mode
    biblio-selection-mode
    blackbox-mode
    bookmark-bmenu-mode
    bookmark-edit-annotation-mode
    browse-kill-ring-mode
    bs-mode
    bubbles-mode
    bzr-annotate-mode
    calc-mode
    cfw:calendar-mode
    completion-list-mode
    Custom-mode
    custom-theme-choose-mode
    debugger-mode
    delicious-search-mode
    desktop-menu-blist-mode
    desktop-menu-mode
    doc-view-mode
    dun-mode
    dvc-bookmarks-mode
    dvc-diff-mode
    dvc-info-buffer-mode
    dvc-log-buffer-mode
    dvc-revlist-mode
    dvc-revlog-mode
    dvc-status-mode
    dvc-tips-mode
    ediff-mode
    ediff-meta-mode
    efs-mode
    Electric-buffer-menu-mode
    emms-browser-mode
    emms-mark-mode
    emms-metaplaylist-mode
    emms-playlist-mode
    ess-help-mode
    etags-select-mode
    fj-mode
    gc-issues-mode
    gdb-breakpoints-mode
    gdb-disassembly-mode
    gdb-frames-mode
    gdb-locals-mode
    gdb-memory-mode
    gdb-registers-mode
    gdb-threads-mode
    gist-list-mode
    git-rebase-mode
    gnus-article-mode
    gnus-browse-mode
    gnus-group-mode
    gnus-server-mode
    gnus-summary-mode
    gomoku-mode
    google-maps-static-mode
    ibuffer-mode
    jde-javadoc-checker-report-mode
    magit-cherry-mode
    magit-diff-mode
    magit-log-mode
    magit-log-select-mode
    magit-popup-mode
    magit-popup-sequence-mode
    magit-process-mode
    magit-reflog-mode
    magit-refs-mode
    magit-revision-mode
    magit-stash-mode
    magit-stashes-mode
    magit-status-mode
    mh-folder-mode
    monky-mode
    mpuz-mode
    mu4e-main-mode
    mu4e-headers-mode
    mu4e-view-mode
    notmuch-hello-mode
    notmuch-search-mode
    notmuch-show-mode
    notmuch-tree-mode
    occur-mode
    org-agenda-mode
    package-menu-mode
    pdf-outline-buffer-mode
    pdf-view-mode
    proced-mode
    rcirc-mode
    rebase-mode
    recentf-dialog-mode
    reftex-select-bib-mode
    reftex-select-label-mode
    reftex-toc-mode
    sldb-mode
    slime-inspector-mode
    slime-thread-control-mode
    slime-xref-mode
    snake-mode
    solitaire-mode
    sr-buttons-mode
    sr-mode
    sr-tree-mode
    sr-virtual-mode
    tar-mode
    tetris-mode
    tla-annotate-mode
    tla-archive-list-mode
    tla-bconfig-mode
    tla-bookmarks-mode
    tla-branch-list-mode
    tla-browse-mode
    tla-category-list-mode
    tla-changelog-mode
    tla-follow-symlinks-mode
    tla-inventory-file-mode
    tla-inventory-mode
    tla-lint-mode
    tla-logs-mode
    tla-revision-list-mode
    tla-revlog-mode
    tla-tree-lint-mode
    tla-version-list-mode
    twittering-mode
    urlview-mode
    vc-annotate-mode
    vc-dir-mode
    vc-git-log-view-mode
    vc-hg-log-view-mode
    vc-svn-log-view-mode
    vm-mode
    vm-summary-mode
    w3m-mode
    wab-compilation-mode
    xgit-annotate-mode
    xgit-changelog-mode
    xgit-diff-mode
    xgit-revlog-mode
    xhg-annotate-mode
    xhg-log-mode
    xhg-mode
    xhg-mq-mode
    xhg-mq-sub-mode
    xhg-status-extra-mode)
  "Modes that should come up in Emacs state."
  :type  '(repeat symbol)
  :group 'evil)

(defcustom evil-insert-state-modes
  '(comint-mode
    erc-mode
    eshell-mode
    geiser-repl-mode
    gud-mode
    inferior-apl-mode
    inferior-caml-mode
    inferior-emacs-lisp-mode
    inferior-j-mode
    inferior-python-mode
    inferior-scheme-mode
    inferior-sml-mode
    internal-ange-ftp-mode
    haskell-interactive-mode
    prolog-inferior-mode
    reb-mode
    shell-mode
    slime-repl-mode
    term-mode
    utop-mode
    wdired-mode)
  "Modes that should come up in Insert state."
  :type  '(repeat symbol)
  :group 'evil)

(defcustom evil-motion-state-modes
  '(apropos-mode
    Buffer-menu-mode
    calendar-mode
    color-theme-mode
    command-history-mode
    compilation-mode
    dictionary-mode
    ert-results-mode
    help-mode
    Info-mode
    Man-mode
    speedbar-mode
    undo-tree-visualizer-mode
    woman-mode)
  "Modes that should come up in Motion state."
  :type  '(repeat symbol)
  :group 'evil)

(defvar evil-pending-overriding-maps nil
  "An alist of pending overriding maps.")

(defvar evil-pending-intercept-maps nil
  "An alist of pending intercept maps.")

(defcustom evil-overriding-maps '()
  "Keymaps that should override Evil maps.
Entries have the form (MAP-VAR . STATE), where MAP-VAR is
a keymap variable and STATE is the state whose bindings
should be overridden. If STATE is nil, all states are
overridden."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'evil
  :set #'(lambda (var values)
           (set-default var values)
           (evil-set-custom-state-maps 'evil-overriding-maps
                                       'evil-pending-overriding-maps
                                       'override-state
                                       'evil-make-overriding-map
                                       values))
  :initialize #'evil-custom-initialize-pending-reset)

(add-hook 'after-load-functions #'evil-update-pending-maps)

(defcustom evil-intercept-maps
  '((edebug-mode-map . nil))
  "Keymaps that should intercept Evil maps.
Entries have the form (MAP-VAR . STATE), where MAP-VAR is
a keymap variable and STATE is the state whose bindings
should be intercepted. If STATE is nil, all states are
intercepted."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'evil
  :set #'(lambda (var values)
           (set-default var values)
           (evil-set-custom-state-maps 'evil-intercept-maps
                                       'evil-pending-intercept-maps
                                       'intercept-state
                                       'evil-make-intercept-map
                                       values))
  :initialize #'evil-custom-initialize-pending-reset)

(defcustom evil-motions
  '(back-to-indentation
    backward-char
    backward-list
    backward-paragraph
    backward-sentence
    backward-sexp
    backward-up-list
    backward-word
    beginning-of-buffer
    beginning-of-defun
    beginning-of-line
    beginning-of-visual-line
    c-beginning-of-defun
    c-end-of-defun
    diff-file-next
    diff-file-prev
    diff-hunk-next
    diff-hunk-prev
    down-list
    end-of-buffer
    end-of-defun
    end-of-line
    end-of-visual-line
    exchange-point-and-mark
    forward-char
    forward-list
    forward-paragraph
    forward-sentence
    forward-sexp
    forward-word
    goto-last-change
    ibuffer-backward-line
    ibuffer-forward-line
    isearch-abort
    isearch-cancel
    isearch-complete
    isearch-del-char
    isearch-delete-char
    isearch-edit-string
    isearch-exit
    isearch-highlight-regexp
    isearch-occur
    isearch-other-control-char
    isearch-other-meta-char
    isearch-printing-char
    isearch-query-replace
    isearch-query-replace-regexp
    isearch-quote-char
    isearch-repeat-backward
    isearch-repeat-forward
    isearch-ring-advance
    isearch-ring-retreat
    isearch-toggle-case-fold
    isearch-toggle-input-method
    isearch-toggle-regexp
    isearch-toggle-specified-input-method
    isearch-toggle-word
    isearch-yank-char
    isearch-yank-kill
    isearch-yank-line
    isearch-yank-word-or-char
    keyboard-quit
    left-char
    left-word
    mouse-drag-region
    mouse-save-then-kill
    mouse-set-point
    mouse-set-region
    mwheel-scroll
    move-beginning-of-line
    move-end-of-line
    next-error
    next-line
    paredit-backward
    paredit-backward-down
    paredit-backward-up
    paredit-forward
    paredit-forward-down
    paredit-forward-up
    pop-global-mark
    pop-tag-mark
    pop-to-mark-command
    previous-error
    previous-line
    right-char
    right-word
    scroll-down
    scroll-down-command
    scroll-up
    scroll-up-command
    sgml-skip-tag-backward
    sgml-skip-tag-forward
    up-list)
  "Non-Evil commands to initialize to motions."
  :type  '(repeat symbol)
  :group 'evil
  :set #'evil-set-custom-motions
  :initialize #'evil-custom-initialize-pending-reset)

(defcustom evil-visual-newline-commands
  '(LaTeX-section
    TeX-font)
  "Commands excluding the trailing newline of a Visual Line selection.
These commands work better without this newline."
  :type  '(repeat symbol)
  :group 'evil
  :set #'evil-set-visual-newline-commands
  :initialize #'evil-custom-initialize-pending-reset)

(defcustom evil-want-visual-char-semi-exclusive nil
  "DEPRECATED.  Will be removed in a future version.
Prefer to set `evil-v$-excludes-newline' to non-nil.

Visual character selection to beginning/end of line is exclusive.
If non nil then an inclusive visual character selection which
ends at the beginning or end of a line is turned into an
exclusive selection. Thus if the selected (inclusive) range ends
at the beginning of a line it is changed to not include the first
character of that line, and if the selected range ends at the end
of a line it is changed to not include the newline character of
that line."
  :type 'boolean
  :group 'evil)
(make-obsolete-variable
 'evil-want-visual-char-semi-exclusive
 "Semi-exclusivity prevents selecting text + 1st char of next line,
without having to introduce new niche functionality.
Prefer to set `evil-v$-excludes-newline' to non-nil."
 "1.15.0")

(defcustom evil-v$-excludes-newline nil
  "If non-nil, `evil-end-of-line' does not move as far as to include
the `\n' char at eol. This makes `v$' consistent with `$' used as a
motion (e.g. `v$y' is consistent with `y$' in normal state)."
  :type 'boolean
  :group 'evil)

(defcustom evil-text-object-change-visual-type t
  "Text objects change the current visual state type.
If non-nil then a text-object changes the type of the visual state to
its default selection type (e.g. a word object always changes to
charwise visual state). Otherwise the current visual state type is
preserved."
  :type 'boolean
  :group 'evil)

(defgroup evil-cjk nil
  "CJK support"
  :prefix "evil-cjk-"
  :group 'evil)

(defcustom evil-cjk-emacs-word-boundary nil
  "Determine word boundary exactly the same way as Emacs does."
  :type 'boolean
  :group 'evil-cjk)

(defcustom evil-cjk-word-separating-categories
  '(;; Kanji
    (?C . ?H) (?C . ?K) (?C . ?k) (?C . ?A) (?C . ?G)
    ;; Hiragana
    (?H . ?C) (?H . ?K) (?H . ?k) (?H . ?A) (?H . ?G)
    ;; Katakana
    (?K . ?C) (?K . ?H) (?K . ?k) (?K . ?A) (?K . ?G)
    ;; half-width Katakana
    (?k . ?C) (?k . ?H) (?k . ?K) ; (?k . ?A) (?k . ?G)
    ;; full-width alphanumeric
    (?A . ?C) (?A . ?H) (?A . ?K) ; (?A . ?k) (?A . ?G)
    ;; full-width Greek
    (?G . ?C) (?G . ?H) (?G . ?K) ; (?G . ?k) (?G . ?A)
    )
  "List of pair (cons) of categories to determine word boundary
used in `evil-cjk-word-boundary-p'. See the documentation of
`word-separating-categories'. Use `describe-categories' to see
the list of categories."
  :type '(alist :key-type (choice character (const nil))
                :value-type (choice character (const nil)))
  :group 'evil-cjk)

(defcustom evil-cjk-word-combining-categories
  '(;; default value in word-combining-categories
    (nil . ?^) (?^ . nil)
    ;; Roman
    (?r . ?k) (?r . ?A) (?r . ?G)
    ;; half-width Katakana
    (?k . ?r) (?k . ?A) (?k . ?G)
    ;; full-width alphanumeric
    (?A . ?r) (?A . ?k) (?A . ?G)
    ;; full-width Greek
    (?G . ?r) (?G . ?k) (?G . ?A)
    )
  "List of pair (cons) of categories to determine word boundary
used in `evil-cjk-word-boundary-p'. See the documentation of
`word-combining-categories'. Use `describe-categories' to see the
list of categories."
  :type '(alist :key-type (choice character (const nil))
                :value-type (choice character (const nil)))
  :group 'evil-cjk)

(defcustom evil-ex-complete-emacs-commands 'in-turn
  "TAB-completion for Emacs commands in ex command line.
This variable determines when Emacs commands are considered for
completion, always, never, or only if no Evil ex command is
available for completion."
  :group 'evil
  :type '(radio (const :tag "Only if no ex-command." :value in-turn)
                (const :tag "Never" :value nil)
                (const :tag "Always" :value t)))

(defface evil-ex-commands '(( nil
                              :underline t
                              :slant italic))
  "Face for the Evil command in completion in ex mode."
  :group 'evil)

(defface evil-ex-info '(( ((supports :slant))
                          :slant italic
                          :foreground "red"))
  "Face for the info message in ex mode."
  :group 'evil)

(defcustom evil-ex-visual-char-range nil
  "Type of default ex range in visual char state.
If non-nil the default range when starting an ex command from
character visual state is `<,`> otherwise it is '<,'>. In the
first case the ex command will be passed a region covering only
the visual selection. In the second case the passed region will
be extended to contain full lines."
  :group 'evil
  :type 'boolean)

(defvar evil--ex-global-active-p nil
  "If the :global command is running.
Used to change the behaviour of certain commands like :print.")

(defvar evil--ex-print-accumulator ""
  "Used by :print etc. to accumulate a string when invoked by :global etc.")

;; Searching
(defcustom evil-symbol-word-search nil
  "If nil then * and # search for words otherwise for symbols."
  :group 'evil
  :type 'boolean)

(defcustom evil-magic t
  "Meaning which characters in a pattern are magic.
The meaning of those values is the same as in Vim. Note that it
only has influence if the Evil search module is chosen in
`evil-search-module'."
  :group 'evil
  :type '(radio (const :tag "Very magic." :value very-magic)
                (const :tag "Magic" :value t)
                (const :tag "Nomagic" :value nil)
                (const :tag "Very nomagic" :value very-nomagic)))

(defcustom evil-ex-search-vim-style-regexp nil
  "If non-nil Vim-style backslash codes are supported in search patterns.
See `evil-transform-vim-style-regexp' for the supported backslash
codes.  Note that this only affects the search command if
`evil-search-module' is set to `evil-search'.  The isearch module
always uses plain Emacs regular expressions."
  :type 'boolean
  :group 'evil)

(defcustom evil-ex-interactive-search-highlight 'all-windows
  "Determine in which windows the interactive highlighting should be shown."
  :type '(radio (const :tag "All windows." all-windows)
                (const :tag "Selected window." selected-window)
                (const :tag "Disable highlighting." nil))
  :group 'evil)

(defcustom evil-ex-search-persistent-highlight t
  "If non-nil matches remain highlighted when the search ends."
  :type 'boolean
  :group 'evil)

(defcustom evil-ex-search-case 'smart
  "The case behaviour of the search command.
Smart case means that the pattern is case sensitive if and only
if it contains an upper case letter, otherwise it is case
insensitive."
  :type '(radio (const :tag "Case sensitive." sensitive)
                (const :tag "Case insensitive." insensitive)
                (const :tag "Smart case." smart))
  :group 'evil)

(defcustom evil-ex-substitute-case nil
  "The case behaviour of the search command.
Smart case means that the pattern is case sensitive if and only
if it contains an upper case letter, otherwise it is case
insensitive. If nil then the setting of `evil-ex-search-case' is
used."
  :type '(radio (const :tag "Same as interactive search." nil)
                (const :tag "Case sensitive." sensitive)
                (const :tag "Case insensitive." insensitive)
                (const :tag "Smart case." smart))
  :group 'evil)

(defcustom evil-ex-search-interactive t
  "If t search is interactive."
  :type 'boolean
  :group 'evil)

(defcustom evil-ex-search-incremental t
  "If t, use incremental search. Note that this only affects the
search command if `evil-search-module' is set to `evil-search'."
  :type 'boolean
  :group 'evil)

(defcustom evil-ex-search-highlight-all t
  "If t and interactive search is enabled, all matches are
highlighted."
  :type 'boolean
  :group 'evil)

(defcustom evil-ex-substitute-highlight-all t
  "If t all matches for the substitute pattern are highlighted."
  :type 'boolean
  :group 'evil)

(defcustom evil-ex-substitute-interactive-replace t
  "If t and substitute patterns are highlighted,
the replacement is shown interactively."
  :type 'boolean
  :group 'evil)

(defcustom evil-ex-substitute-global nil
  "If non-nil substitute patterns are global by default.
Standardly (if this variable is nil) a substitution works only on the
first match of the pattern in a line unless the \"g\" flag is
given, in which case the substitution happens on all matches in the
line. If this option is non-nil, this behavior is reversed."
  :type  'boolean
  :group 'evil)

(defface evil-ex-search '((t :inherit isearch))
  "Face for interactive search."
  :group 'evil)

(defface evil-ex-lazy-highlight '((t :inherit lazy-highlight))
  "Face for highlighting all matches in interactive search."
  :group 'evil)

(defface evil-ex-substitute-matches '((t :inherit lazy-highlight))
  "Face for interactive substitute matches."
  :group 'evil)

(defface evil-ex-substitute-replacement '((((supports :underline))
                                           :underline t
                                           :foreground "red"))
  "Face for interactive replacement text."
  :group 'evil)

(defcustom evil-command-window-height 7
  "Height (in lines) of the command line window.
Set to 0 to use the default height for `split-window'."
  :type 'integer
  :group 'evil)

(defcustom evil-display-shell-error-in-message nil
  "Show error output of a shell command in the error buffer.
If this variable is non-nil the error output of a shell command
goes to the messages buffer instead of being mixed with the
regular output. This happens only if the exit status of the
command is non-zero."
  :type 'boolean
  :group 'evil)

(defcustom evil-want-abbrev-expand-on-insert-exit t
  "If non-nil abbrevs will be expanded when leaving insert state
like in Vim, if `abbrev-mode' is on."
  :type 'boolean
  :group 'evil)

;;; Variables

(defmacro evil-define-local-var (symbol &optional initvalue docstring)
  "Define SYMBOL as permanent buffer local variable, and return SYMBOL.
The parameters are the same as for `defvar', but the variable
SYMBOL is made permanent buffer local."
  (declare (indent defun)
           (doc-string 3)
           (debug (symbolp &optional form stringp)))
  `(progn
     (defvar ,symbol ,initvalue ,docstring)
     (make-variable-buffer-local ',symbol)
     (put ',symbol 'permanent-local t)))

(evil-define-local-var evil-scroll-count 0
  "Hold last used prefix for `evil-scroll-up'
and `evil-scroll-down'.
Determine how many lines should be scrolled.
Default value is 0 - scroll half the screen.")

(evil-define-local-var evil-state nil
  "The current Evil state.
To change the state, use `evil-change-state'
or call the state function (e.g., `evil-normal-state').")

;; these may be used inside `evil-define-state'
(evil-define-local-var evil-next-state nil
  "The Evil state being switched to.")

(evil-define-local-var evil-previous-state-alist nil
  "For Each evil state the Evil state being switched from.")

(evil-define-local-var evil-previous-state nil
  "The Evil state being switched from.")

(defvar evil-execute-in-emacs-state-buffer nil
  "The buffer of the latest `evil-execute-in-emacs-state'.
When this command is being executed the current buffer is stored
in this variable. This is necessary in case the Emacs-command to
be called changes the current buffer.")

(evil-define-local-var evil-mode-line-tag nil
  "Mode-Line indicator for the current state.")
(put 'evil-mode-line-tag 'risky-local-variable t)

(defvar evil-global-keymaps-alist nil
  "Association list of keymap variables.
Entries have the form (MODE . KEYMAP), where KEYMAP
is the variable containing the keymap for MODE.")

(defvar evil-local-keymaps-alist nil
  "Association list of keymap variables that must be
reinitialized in each buffer. Entries have the form
\(MODE . KEYMAP), where KEYMAP is the variable containing
the keymap for MODE.")

(defvar evil-minor-mode-keymaps-alist nil
  "Association list of Evil states to minor-mode keymap alists.
Entries have the form (STATE . MODE-MAP-ALIST), where
MODE-MAP-ALIST is an alist taking the form of
`minor-mode-map-alist'.")

(defvar evil-state-properties nil
  "Specifications made by `evil-define-state'.
Entries have the form (STATE . PLIST), where PLIST is a property
list specifying various aspects of the state. To access a property,
use `evil-state-property'.")

(evil-define-local-var evil-mode-map-alist nil
  "Association list of keymaps to use for Evil modes.
Elements have the form (MODE . KEYMAP), with the first keymaps
having higher priority.")

(defvar evil-change-commands '(evil-change)
  "Commands that wrap or replace `evil-change'.
This list exists to apply an inconsistency with vim's change command
to commands that wrap or redefine it. See emacs-evil/evil#916.")

(defvar evil-transient-vars '(cua-mode transient-mark-mode select-active-regions)
  "List of variables pertaining to Transient Mark mode.")

(defvar evil-transient-vals nil
  "Association list of old values for Transient Mark mode variables.
Entries have the form (VARIABLE VALUE LOCAL), where LOCAL is
whether the variable was previously buffer-local.")

(defvar evil-no-display nil
  "If non-nil, various Evil displays are inhibited.
Use the macro `evil-without-display' to set this variable.")

(defvar evil-type-properties nil
  "Specifications made by `evil-define-type'.
Entries have the form (TYPE . PLIST), where PLIST is a property
list specifying functions for handling the type: expanding it,
describing it, etc.")

(defvar evil-interactive-alist nil
  "Association list of Evil-specific interactive codes.")

(evil-define-local-var evil-motion-marker nil
  "Marker for storing the starting position of a motion.")

(evil-define-local-var evil-this-type nil
  "Current motion type.")

(evil-define-local-var evil-this-type-modified nil
  "Non-nil iff current motion type has been modified by the user.
If the type has been modified, this variable contains the new
type.")

(evil-define-local-var evil-this-register nil
  "Current register.")

(defvar evil-last-=-register-input nil
  "Most recent input from the `=' register. A string.")

(defvar evil-this-macro nil
  "Current macro register.")

(evil-define-local-var evil-this-operator nil
  "Current operator.")

(evil-define-local-var evil-this-motion nil
  "Current motion.")

(evil-define-local-var evil-this-motion-count nil
  "Current motion count.")

;; In light of `evil-last-recorded-register' this should probably
;; be renamed to `evil-last-executed-register' but that may break configs
(defvar evil-last-register nil
  "The last executed register.")

(defvar evil-last-recorded-register nil
  "The last recorded register.")

(defvar evil-inhibit-operator nil
  "Inhibit current operator.
If an operator calls a motion and the motion sets this variable
to t, the operator code is not executed.")

(defvar evil-inhibit-operator-value nil
  "This variable is used to transfer the value
of `evil-inhibit-operator' from one local scope to another.")

;; used by `evil-define-operator'
(defvar evil-operator-range-beginning nil
  "Beginning of `evil-operator-range'.")

(defvar evil-operator-range-end nil
  "End of `evil-operator-range'.")

(defvar evil-operator-range-type nil
  "Type of `evil-operator-range'.")

(defvar evil-operator-range-motion nil
  "Motion of `evil-operator-range'.")

(defvar evil-operator-start-col nil
  "Used to restore column (where possible) after an operator has moved it.")

(defvar evil-restriction-stack nil
  "List of previous restrictions.
Using `evil-with-restriction' stores the previous values of
`point-min' and `point-max' as a pair in this list.")

(evil-define-local-var evil-markers-alist
  '((?\( . evil-backward-sentence-begin)
    (?\) . evil-forward-sentence-begin)
    (?{ . evil-backward-paragraph)
    (?} . evil-forward-paragraph)
    (?' . evil-jump-backward-swap)
    (?` . evil-jump-backward-swap)
    (?< . evil-visual-beginning)
    (?> . evil-visual-goto-end)
    (?. . (lambda ()
            (let (last-command)
              (goto-last-change nil)))))
  "Association list for markers.
Entries have the form (CHAR . DATA), where CHAR is the marker's
name and DATA is either a marker object as returned by `make-marker',
a variable, a movement function, or a cons cell (STRING NUMBER),
where STRING is a file path and NUMBER is a buffer position.
The global value of this variable holds markers available from
every buffer, while the buffer-local value holds markers available
only in the current buffer.")

(defconst evil-suppress-map (make-keymap)
  "Full keymap disabling default bindings to `self-insert-command'.")
(suppress-keymap evil-suppress-map t)

(defvar evil-read-key-map (make-sparse-keymap)
  "Keymap active during `evil-read-key'.
This keymap can be used to bind some commands during the
execution of `evil-read-key' which is usually used to read a
character argument for some commands, e.g. `evil-replace'.")

;; TODO: customize size of ring
(defvar evil-repeat-ring (make-ring 10)
  "A ring of repeat-informations to repeat the last command.")

(defvar evil-recording-repeat nil
  "Whether we are recording a repeat.")

(defvar evil-recording-current-command nil
  "Whether we are recording the current command for repeat.")

(defvar evil-repeat-changes nil
  "Accumulated buffer changes in reverse order for change-tracking commands.")

(defvar evil-repeat-info nil
  "Information accumulated during current repeat.")

(defvar evil-repeat-buffer nil
  "The buffer in which the repeat started.
If the buffer is changed, the repeat is cancelled.")

(defvar evil-repeat-pos nil
  "The point position at the start of a change-tracking command.")

(defvar evil-repeat-keys nil
  "The keys that invoked the current command.")

(defvar evil-last-repeat nil
  "Information about the latest repeat command.
This is a list of three elements (POINT COUNT UNDO-POINTER),
where POINT is the position of point before the latest repeat,
COUNT the count-argument of the latest repeat command and
UNDO-POINTER the head of the undo-list before the last command
has been repeated.")

(defvar evil-repeat-count nil
  "The explicit count when repeating a command.")

(defvar evil-maybe-remove-spaces nil
  "Flag to determine if newly inserted spaces should be removed.
See the function `evil-maybe-remove-spaces'.")

(evil-define-local-var evil-insert-count nil
  "The explicit count passed to a command starting Insert or Replace state.")

(evil-define-local-var evil-insert-vcount nil
  "The information about the number of following lines the
insertion should be repeated. This is list (LINE COLUMN COUNT)
where LINE is the line-number where the original insertion
started and COLUMN is either a number or function determining the
column where the repeated insertions should take place. COUNT is
number of repeats (including the original insertion).")

(defvar evil-insert-skip-empty-lines nil
  "Non-nil of the current insertion should not take place on
  lines at which the insertion point is behind the end of the
  line.")

(evil-define-local-var evil-insert-lines nil
  "Non-nil if the current insertion command is a line-insertion
command o or O.")

(evil-define-local-var evil-insert-repeat-info nil
  "Repeat information accumulated during an insertion.")

(evil-define-local-var evil-replace-alist nil
  "Association list of characters overwritten in Replace state.
The format is (POS . CHAR).")

(evil-define-local-var evil-echo-area-message nil
  "Previous value of `current-message'.")

(defvar evil-write-echo-area nil
  "If set to t inside `evil-save-echo-area', then the echo area
is not restored.")

(defvar evil-last-find nil
  "A pair (FUNCTION . CHAR) describing the lastest character
  search command.")

(defvar evil-last-paste nil
  "Information about the latest paste.
This should be a list (CMD COUNT POINT BEG END FIRSTVISUAL) where
CMD is the last paste-command (`evil-paste-before',
`evil-paste-after' or `evil-visual-paste'), COUNT is the repeat
count of the paste, POINT is the position of point before the
paste, BEG end END are the region of the inserted
text. FIRSTVISUAL is t if and only if the previous command was
the first visual paste (i.e. before any paste-pop).")

(evil-define-local-var evil-last-undo-entry nil
  "Information about the latest undo entry in the buffer.
This should be a pair (OBJ . CONS) where OBJ is the entry as an
object, and CONS is a copy of the entry.")

(evil-define-local-var evil-current-insertion nil
  "Information about the latest insertion in insert state.
This should be a pair (BEG . END) that describes the
buffer-region of the newly inserted text.")

(defvar evil-last-insertion nil
  "The last piece of inserted text.")

(defvar evil-last-small-deletion nil
  "The last piece of deleted text.
The text should be less than a line.")

(defvar evil-was-yanked-without-register t
  "Whether text being saved to the numbered-register ring was
not deleted and not yanked to a specific register.")

(defvar evil-paste-count nil
  "The count argument of the current paste command.")

(defvar evil--cursor-after nil
  "Internal flag for gp & gP.")

(defvar evil-temporary-undo nil
  "When undo is disabled in current buffer.
Certain commands depending on undo use this variable
instead of `buffer-undo-list'.")

(evil-define-local-var evil-undo-list-pointer nil
  "Everything up to this mark is united in the undo-list.")

(defvar evil-in-single-undo nil
  "Set to non-nil if the current undo steps are connected.")

(defvar evil-flash-timer nil
  "Timer for flashing search results.")

(defvar evil-search-prompt nil
  "String to use for search prompt.")

(defvar evil-search-forward-history nil
  "History of forward searches.")

(defvar evil-search-backward-history nil
  "History of backward searches.")

(defvar evil-inner-text-objects-map (make-sparse-keymap)
  "Keymap for inner text objects.")

(defvar evil-outer-text-objects-map (make-sparse-keymap)
  "Keymap for outer text objects.")

(defvar evil-window-map (make-sparse-keymap)
  "Keymap for window-related commands.")

(evil-define-local-var evil-input-method nil
  "Input method used in Insert state and Emacs state.")

;;; Visual state

(evil-define-local-var evil-visual-beginning nil
  "The beginning of the Visual selection, a marker.")

(evil-define-local-var evil-visual-end nil
  "The end of the Visual selection, a marker.")

(evil-define-local-var evil-visual-point nil
  "The position of point in Visual state, a marker.")

(evil-define-local-var evil-visual-mark nil
  "The position of mark in Visual state, a marker.")

(evil-define-local-var evil-visual-selection nil
  "The kind of Visual selection.
This is a selection as defined by `evil-define-visual-selection'.")

;; we could infer the direction by comparing `evil-visual-mark'
;; and `evil-visual-point', but destructive operations may
;; displace the markers
(evil-define-local-var evil-visual-direction 0
  "Whether point follows mark in Visual state.
Negative if point precedes mark, otherwise positive.
See also the function `evil-visual-direction'.")

(evil-define-local-var evil-visual-properties nil
  "Property list of miscellaneous Visual properties.")

(evil-define-local-var evil-visual-region-expanded nil
  "Whether the region matches the Visual selection.
That is, whether the positions of point and mark have been
expanded to coincide with the selection's boundaries.
This makes the selection available to functions acting
on Emacs' region.")

(defvar evil-transient-mouse-selection nil
  "Whether the region selected by the mouse is discarded by the next command.
By default, the region is not discarded.  If non-nil, exit visual state
immediately before the next command.  To use the region selected by the mouse,
execute a command which would usually enter visual state, such as
\\[evil-visual-char] or \\[evil-visual-block].")

(defvar evil--region-from-mouse nil
  "Whether the region was set by the mouse.")

(evil-define-local-var evil-visual-overlay nil
  "Overlay for highlighting the Visual selection.
Not used for blockwise selections, in which case
see `evil-visual-block-overlays'.")

(evil-define-local-var evil-visual-block-overlays nil
  "Overlays for Visual Block selection, one for each line.
They are reused to minimize flicker.")

(defvar evil-visual-alist nil
  "Association list of Visual selection functions.
Elements have the form (NAME . FUNCTION).")

(evil-define-local-var evil-visual-x-select-timer nil
  "Timer for updating the X selection in visual state.")

(defvar evil-visual-x-select-timeout 0.1
  "Time in seconds for the update of the X selection.")

(declare-function origami-open-all-nodes "ext:origami.el")
(declare-function origami-close-all-nodes "ext:origami.el")
(declare-function origami-toggle-node "ext:origami.el")
(declare-function origami-open-node "ext:origami.el")
(declare-function origami-open-node-recursively "ext:origami.el")
(declare-function origami-close-node "ext:origami.el")

(defvar evil-fold-list
  `(((vdiff-mode)
     :open-all   vdiff-open-all-folds
     :close-all  vdiff-close-all-folds
     :toggle     ,(lambda () (call-interactively 'vdiff-toggle-fold))
     :open       ,(lambda () (call-interactively 'vdiff-open-fold))
     :open-rec   ,(lambda () (call-interactively 'vdiff-open-fold))
     :close      ,(lambda () (call-interactively 'vdiff-close-fold)))
    ((vdiff-3way-mode)
     :open-all   vdiff-open-all-folds
     :close-all  vdiff-close-all-folds
     :toggle     ,(lambda () (call-interactively 'vdiff-toggle-fold))
     :open       ,(lambda () (call-interactively 'vdiff-open-fold))
     :open-rec   ,(lambda () (call-interactively 'vdiff-open-fold))
     :close      ,(lambda () (call-interactively 'vdiff-close-fold)))
    ((hs-minor-mode)
     :open-all   hs-show-all
     :close-all  hs-hide-all
     :toggle     hs-toggle-hiding
     :open       hs-show-block
     :open-rec   nil
     :close      hs-hide-block)
    ((hide-ifdef-mode)
     :open-all   show-ifdefs
     :close-all  hide-ifdefs
     :toggle     nil
     :open       show-ifdef-block
     :open-rec   nil
     :close      hide-ifdef-block)
    ((outline-mode
      outline-minor-mode
      org-mode
      markdown-mode)
     :open-all   show-all
     :close-all  ,(lambda ()
                    (with-no-warnings (hide-sublevels 1)))
     :toggle     outline-toggle-children
     :open       ,(lambda ()
                    (with-no-warnings
                      (show-entry)
                      (show-children)))
     :open-rec   show-subtree
     :close      hide-subtree)
    ((origami-mode)
     :open-all   ,(lambda () (origami-open-all-nodes (current-buffer)))
     :close-all  ,(lambda () (origami-close-all-nodes (current-buffer)))
     :toggle     ,(lambda () (origami-toggle-node (current-buffer) (point)))
     :open       ,(lambda () (origami-open-node (current-buffer) (point)))
     :open-rec   ,(lambda () (origami-open-node-recursively (current-buffer) (point)))
     :close      ,(lambda () (origami-close-node (current-buffer) (point)))))
  "Actions to be performed for various folding operations.

The value should be a list of fold handlers, were a fold handler has
the format:

  ((MODES) PROPERTIES)

MODES acts as a predicate, containing the symbols of all major or
minor modes for which the handler should match.  For example:

  \\='((outline-minor-mode org-mode) ...)

would match for either outline-minor-mode or org-mode, even though the
former is a minor mode and the latter is a major.

PROPERTIES specifies possible folding actions and the functions to be
applied in the event of a match on one (or more) of the MODES; the
supported properties are:

  - `:open-all'
    Open all folds.
  - `:close-all'
    Close all folds.
  - `:toggle'
    Toggle the display of the fold at point.
  - `:open'
    Open the fold at point.
  - `:open-rec'
    Open the fold at point recursively.
  - `:close'
    Close the fold at point.

Each value must be a function.  A value of `nil' will cause the action
to be ignored for that respective handler.  For example:

  `((org-mode)
     :close-all  nil
     :open       ,(lambda ()
                    (show-entry)
                    (show-children))
     :close      hide-subtree)

would ignore `:close-all' actions and invoke the provided functions on
`:open' or `:close'.")

;;; Ex

(define-obsolete-variable-alias 'evil-ex-map 'evil-ex-shortcut-map "1.15.0")
(defvar evil-ex-shortcut-map (make-sparse-keymap)
  "Keymap for Ex.
Key sequences bound in this map are immediately executed.")

;; Intentionally does not inherit from `minibuffer-local-map', as users
;; are encouraged to instead set this as the parent of that keymap.
(defvar evil-command-line-map (make-sparse-keymap)
  "Keymap used for the various Evil command-lines.
Modifying this keymap corresponds to using the \":cmap\" Vim command.
See `evil-ex-completion-map' and `evil-ex-search-keymap' which inherit
from this keymap.")

(defvar evil-ex-completion-map (make-sparse-keymap)
  "Keymap for Ex.")

(defvar evil-ex-initial-input nil
  "Additional initial content of the Ex command line.
This content of this variable is appended to the Ex command line
when Ex is started interactively.")

(defvar evil-ex-history nil
  "History of Ex commands.")

(define-obsolete-variable-alias
  'evil-ex-current-buffer 'evil-ex-original-buffer "1.15.0")
(defvar evil-ex-original-buffer nil
  "Buffer that was current when the Evil command line was started.")

(defvar evil-ex-point nil
  "The point position when the Ex command was called.")

(defvar evil-called-from-ex-p nil
  "Non-nil if a command is currently being called as an Ex command.")

(defvar evil-ex-range nil
  "The current range of the Ex command.")

(defvar evil-ex-bang nil
  "The \"!\" argument of the current Ex command.")

(defvar evil-ex-argument nil
  "The current argument of the Ex command.")

(defvar evil-previous-shell-command nil
  "The last shell command.")

;; Eval
(defvar evil-eval-history nil
  "History of eval input, from the `=' register.")

(defvar evil-eval-map (make-sparse-keymap)
  "Keymap for eval input.")

;; Searching
(defvar evil-ex-search-history nil
  "The history for the search command.")

(defvar evil-ex-search-direction nil
  "The direction of the current search, either `forward' or `backward'.")

(defvar evil-ex-search-count nil
  "The count of the current search.")

(defvar evil-ex-search-start-point nil
  "The point where the search started.")

(defvar evil-ex-search-overlay nil
  "The overlay for the current search result.")

(defvar evil-ex-search-pattern nil
  "The last search pattern.")

(defvar evil-ex-search-offset nil
  "The last search offset.")

(defvar evil-ex-search-match-beg nil
  "The beginning position of the last match.")

(defvar evil-ex-search-match-end nil
  "The end position of the last match.")

(defvar evil-ex-substitute-pattern nil
  "The last substitute pattern.")

(defvar evil-ex-substitute-replacement nil
  "The last substitute replacement.")

(defvar evil-ex-substitute-flags nil
  "The last substitute flags.")

(defvar evil-ex-substitute-current-replacement nil
  "The actual replacement.")

(defvar evil-ex-last-was-search nil
  "Non-nil if the previous was a search.
Otherwise the previous command is assumed as substitute.")

;; The lazy-highlighting framework
(evil-define-local-var evil-ex-active-highlights-alist nil
  "An alist of currently active highlights.")

(evil-define-local-var evil-ex-hl-update-timer nil
  "Time used for updating highlights.")

(defvar evil-ex-search-keymap (make-sparse-keymap)
  "Keymap used in ex-search-mode.")

(defcustom evil-want-empty-ex-last-command t
  "Whether to default to evil-ex-previous-command at empty ex prompt."
  :type 'boolean
  :group 'evil)

(defconst evil-version
  (eval-when-compile
    (with-temp-buffer
      (let ((dir (file-name-directory (or load-file-name
                                          byte-compile-current-file))))
        ;; git repository
        (if (and (file-exists-p (concat dir "/.git"))
                 (ignore-errors
                   (zerop (call-process "git" nil '(t nil) nil
                                        "rev-parse"
                                        "--short" "HEAD"))))
            (progn
              (goto-char (point-min))
              (concat "evil-git-"
                      (buffer-substring (point-min)
                                        (line-end-position))))
          ;; no repo, use plain version
          "1.15.0"))))
  "The current version of Evil")

(defcustom evil-want-integration t
  "Whether to load evil-integration.el.
This variable must be set before Evil is loaded."
  :type 'boolean
  :group 'evil)

(defcustom evil-want-keybinding t
  "Whether to load evil-keybindings.el.

This loads a set of keybindings for evil in other modes as well as
setting the initial evil state in those modes.

This variable must be set before evil is loaded."
  :type 'boolean
  :group 'evil)

(defcustom evil-want-minibuffer nil
  "Whether to enable Evil in minibuffer(s)."
  :type 'boolean
  :group 'evil
  :set #'(lambda (sym value)
           (set-default sym value)
           (if value
               (add-hook 'minibuffer-setup-hook 'evil-initialize)
             (remove-hook 'minibuffer-setup-hook 'evil-initialize))))

(defun evil--redo-placeholder (_count)
  (user-error "Customize `evil-undo-system' for redo functionality."))

(defvar evil-undo-function #'undo
  "Function to be used by `evil-undo'.
Customized via `evil-undo-system'.")

(defvar evil-redo-function #'evil--redo-placeholder
  "Function to be used by `evil-redo'.
Customized via `evil-undo-system'.")

(defun evil-set-undo-system (system)
  "Set `evil-undo-function' and `evil-redo-function' by SYSTEM."
  (cond
   ((not system)
    (setq evil-undo-function #'undo
          evil-redo-function #'evil--redo-placeholder))
   ((eq system 'undo-redo)
    (setq evil-undo-function #'undo-only
          evil-redo-function #'undo-redo))
   ((eq system 'undo-tree)
    (setq evil-undo-function 'undo-tree-undo
          evil-redo-function 'undo-tree-redo))
   ((eq system 'undo-fu)
    (setq evil-undo-function 'undo-fu-only-undo
          evil-redo-function 'undo-fu-only-redo))
   (t (error "Unknown undo system `%s'" system))))

(defcustom evil-undo-system nil
  "Undo system Evil should use.
If equal to `undo-tree' or `undo-fu', those packages must be
installed.  If equal to `undo-tree', `undo-tree-mode' must also be
activated.  If equal to `undo-redo', Evil uses commands natively
available in Emacs 28."
  :type '(choice (const :tag "Vanilla undo" nil)
                 (const undo-redo)
                 (const undo-tree)
                 (const undo-fu))
  :group 'evil
  :set #'(lambda (sym value)
           (evil-set-undo-system value)
           (set-default sym value)))

(defcustom evil-visual-update-x-selection-p t
  "Whether to update the X PRIMARY selection with the current visual region."
  :type  'boolean
  :group 'evil)

(defun evil-version ()
  (interactive)
  (message "Evil version %s" evil-version))

(provide 'evil-vars)

;;; evil-vars.el ends here
