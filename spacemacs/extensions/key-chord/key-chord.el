;;; key-chord.el --- map pairs of simultaneously pressed keys to commands
;;-------------------------------------------------------------------
;;
;; Copyright (C) 2003,2005,2008,2012 David Andersson
;;
;; This file is NOT part of Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;;-------------------------------------------------------------------

;; Author: David Andersson <l.david.andersson(at)sverige.nu>
;; Created: 27 April 2003
;; Version: 20140929.2246
;; X-Original-Version: 0.6 (2012-10-23)
;; Keywords: keyboard chord input

;;; Commentary:

;; ########   Compatibility   ########################################
;;
;; Works with Emacs-20.3, 20.6, 20.7, 21.2, 21.4, 22.1 and 23.1
;; Does not work with Emacs-19.31 nor XEmacs-20.4 and 21.4.

;; ########   Quick start   ########################################
;;
;; Add to your ~/.emacs
;;
;;      (require 'key-chord)
;;      (key-chord-mode 1)
;;
;; and some chords, for example
;;
;;      (key-chord-define-global "hj"     'undo)
;;      (key-chord-define-global ",."     "<>\C-b")

;; ########   Terminology   ########################################
;;
;; In this package, a "key chord" is two keys pressed simultaneously,
;; or a single key quickly pressed twice.
;;
;; (Sometimes pressing SHIFT and/or META plus another key is call a chord,
;; but not here. However SHIFT plus two normal keys can be a "key chord".)

;; ########   Description   ########################################
;;
;; Key chord mode acts like a global minor mode controlled by the function
;; `key-chord-mode'.
;;
;; Key chord definitions are stored in ordinary key-maps.
;; The function `key-chord-define-global' defines a chord in the global
;; key-map and `key-chord-define' defines a chord in a specified key-map,
;; for example for a specific mode.
;;
;; A TWO-key chord is two distinct keys pressed simultaneously (within
;; one tenth of a second, or so).
;;
;; Examples:
;;
;;      (key-chord-define-global ",."     "<>\C-b")
;;      (key-chord-define-global "hj"     'undo)
;;      (key-chord-define-global [?h ?j]  'undo)  ; the same
;;      (key-chord-define-global "jk"     'dabbrev-expand)
;;      (key-chord-define-global "cv"     'reindent-then-newline-and-indent)
;;      (key-chord-define-global "4r"     "$")
;;
;; Comma and dot pressed together insert a pair of angle brackets.
;; `h' and `j' pressed together invoke the undo command.
;; `j' and `k' pressed together invoke the dabbrev-expand command.
;; 'c' and 'v' pressed together insert a newline.
;; `4' and `r' pressed together insert a dollar sign.
;;
;; A ONE-key chord is a single key quickly pressed twice (within one third
;; of a second or so). 
;;
;; Examples:
;;
;;      (key-chord-define-global "''"     "`'\C-b")
;;      (key-chord-define-global ",,"     'indent-for-comment)
;;      (key-chord-define-global "qq"     "the ")
;;      (key-chord-define-global "QQ"     "The ")
;;
;; Tick (') pressed twice inserts a back-tick and a tick (`').
;; Comma (,) pressed twice indents for and/or inserts a comment.
;; `q' pressed twice inserts the word "the ".
;;
;; Examples: Mode specific chords
;;
;;      (key-chord-define c++-mode-map ";;"  "\C-e;")
;;      (key-chord-define c++-mode-map "{}"  "{\n\n}\C-p\t")
;;
;; The command `key-chord-describe' lists currently defined key chords.
;; The standard command `describe-bindings' (C-h b) will also show key chords.
;;
;; The standard command `describe-key' (C-h k) will accept a key chord and
;; show its definition. (Isn't that amazing. There is no explicit code to
;; carry out this functionality.)

;; ########   Tips   ########################################
;;
;; Don't chord key combinations that exists in the languages you typically
;; write. Otherwise, if you are typing fast, two key intended to be separate
;; letters might instead trig a chord.
;; E.g. "uu" would be a good chord in spanish but not in finnish, and
;; "hj" would be a good chord in english but not in swedish. 
;;
;; Don't rely solely on /usr/dict/words to find unusual combination.
;; For example "cv" or "fg" can be quite common in certain kinds of
;; programming. Grep your own texts to verify that a combination is unusual.
;; And don't forget to check both permutations: "fg" and "gf".
;;
;; Choose two keys that are close to each other on the keyboard, so they
;; can be quickly typed without effort. Chords involving two hands (as
;; opposed to two fingers on one hand) are harder to type (quickly).
;; The idea is that key chords are to replace function keys for functions
;; that are frequently performed while the hands are in writing position.
;;
;; Key chords might not work well over a slow network.

;; ########   Limitations   ########################################
;;
;; When recording keyboard macros, the time between keyboard inputs are not
;; recorded. Thus, the key-chord-input-method cannot know for sure if two keys
;; in a macro was a chord or not. The current solution remembers the first key
;; of the chords typed during macro recording, and keys that match those (and
;; are defined as chords) are considered key-chords during macro execution.
;; This knowledge is not saved with `name-last-kbd-macro', so they may
;; execute wrong if they contain pair of keys that match defined chords.
;;
;; Emacs will not call input-method-function for keys that have non numeric
;; codes or whos code is outside the range 32..126. Thus you cannot define
;; key chords involving function keys, control keys, or even your non-english
;; letters (on national keyboards) that otherwise are well positioned for
;; chording on your keyboard.
;; (I think chording left and right arrow keys would be useful, but cannot do.
;; I consider this a bug in Emacs. Input methods could happily return
;; unmodified *any* key they don't know about.)
;;
;; Key chords longer that 2 keys are not supported. It could be done, but I
;; don't think it is worth the trubbel since most keyboards will not reliably
;; send all key codes when 3 or more keys are pressed simultaneously.
;; It might also be a bit trickier to maintain performance.
;;
;; Key chord mode uses input-method-function. And so do internationalisation
;; packages (mule, quail, etc). Do not expect them to work well together.
;; The last one that gets the input-method-function rules.

;; ########   Implementation   ########################################
;;
;; Key chords piggy back in ordinary key maps, so they can be defined
;; per mode without having to add hooks to all modes.
;;
;; Key chord key codes are vectors beginning with the atom `key-chord'.
;; A two key chord, e.g. "hj", will add two entries in the key-map.
;; E.g. [key-chord ?h ?j] and [key-chord ?j ?h].
;;
;; When key-chord-mode is enabled input-method-function is set to
;; key-chord-input-method.

;; ########   To do   ########################################
;;
;; * Find a way to save key-chord info in keyboard macros.
;;
;; * Save previous value of input-method-function? And call it?
;;
;; * input-method-function is reset in *info* buffers! What to do?
;;
;; * How to enter interactively command OR string in key-chord-define-global?
;;
;; * Customize public vars (defcustom).

;; ########   History   ########################################
;;
;; 0.6 (2012-10-23) l.david.andersson(at)sverige.nu
;;      Add key-chord-define-local, key-chord-unset-local, key-chord-unset-global
;; 0.5 (2008-09-15) david(at)symsoft.se
;;      Bugfix sit-for; Improved examples; New E-mail in comment
;; 0.4 (2005-05-07) david(at)symsoft.se
;;      Slightly better macro heuristics; Added option key-chord-in-macros
;; 0.3 (2005-04-14) david(at)symsoft.se
;;      Require advice; More examples
;; 0.2 (2003-09-13) david(at)symsoft.se
;;      Quick and dirty fix for keyboard macros
;; 0.1 (2003-04-27) david(at)symsoft.se
;;      First release

;;; Code:

(defvar key-chord-two-keys-delay 0.1	; 0.05 or 0.1
  "Max time delay between two key press to be considered a key chord.")

(defvar key-chord-one-key-delay 0.2	; 0.2 or 0.3 to avoid first autorepeat
  "Max time delay between two press of the same key to be considered a key chord.
This should normally be a little longer than `key-chord-two-keys-delay'.")

(defvar key-chord-in-macros t
  "If nil, don't expand key chords when executing keyboard macros.
If non-nil, expand chord sequenses in macros, but only if a similar chord was
entered during the last interactive macro recording. (This carries a bit of
guesswork. We can't know for sure when executing whether two keys were
typed quickly or slowly when recorded.)")

;; Internal vars
(defvar key-chord-mode nil)

;; Shortcut for key-chord-input-method: no need to test a key again if it
;; didn't matched a chord the last time. Improves feedback during autorepeat.
(defvar key-chord-last-unmatched nil)

;; Macro heuristics: Keep track of which chords was used when the last macro
;; was defined. Or rather, only the first-char of the chords. Only expand
;; matching chords during macro execution.
(defvar key-chord-in-last-kbd-macro nil)
(defvar key-chord-defining-kbd-macro nil)

;;;###autoload
(defun key-chord-mode (arg)
  "Toggle key chord mode.
With positive ARG enable the mode. With zero or negative arg disable the mode.
A key chord is two keys that are pressed simultaneously, or one key quickly
pressed twice.
\nSee functions `key-chord-define-global', `key-chord-define-local', and
`key-chord-define' and variables `key-chord-two-keys-delay' and
`key-chord-one-key-delay'."

  (interactive "P")
  (setq key-chord-mode (if arg
			   (> (prefix-numeric-value arg) 0)
			 (not key-chord-mode)))
  (cond (key-chord-mode
	 (setq input-method-function 'key-chord-input-method)
	 (message "Key Chord mode on"))
	(t
	 (setq input-method-function nil)
	 (message "Key Chord mode off"))))

;;;###autoload
(defun key-chord-define-global (keys command)
  "Define a key-chord of the two keys in KEYS starting a COMMAND.
\nKEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed.
\nNote that KEYS defined locally in the current buffer will have precedence."
  (interactive "sSet key chord globally (2 keys): \nCSet chord \"%s\" to command: ")
  (key-chord-define (current-global-map) keys command))

;;;###autoload
(defun key-chord-define-local (keys command)
  "Locally define a key-chord of the two keys in KEYS starting a COMMAND.
\nKEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed.
\nThe binding goes in the current buffer's local map,
which in most cases is shared with all other buffers in the same major mode."
  (interactive "sSet key chord locally (2 keys): \nCSet chord \"%s\" to command: ")
  (key-chord-define (current-local-map) keys command))

(defun key-chord-unset-global (keys)
  "Remove global key-chord of the two keys in KEYS."
  (interactive "sUnset key chord globally (2 keys): ")
  (key-chord-define (current-local-map) keys nil))

(defun key-chord-unset-local (keys)
  "Remove local key-chord of the two keys in KEYS."
  (interactive "sUnset key chord locally (2 keys): ")
  (key-chord-define (current-local-map) keys nil))

;;;###autoload
(defun key-chord-define (keymap keys command)
  "Define in KEYMAP, a key-chord of the two keys in KEYS starting a COMMAND.
\nKEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed."
  (if (/= 2 (length keys))
      (error "Key-chord keys must have two elements"))
  ;; Exotic chars in a string are >255 but define-key wants 128..255 for those
  (let ((key1 (logand 255 (aref keys 0)))
	(key2 (logand 255 (aref keys 1))))
    (if (eq key1 key2)
	(define-key keymap (vector 'key-chord key1 key2) command)
      ;; else
      (define-key keymap (vector 'key-chord key1 key2) command)
      (define-key keymap (vector 'key-chord key2 key1) command))))

(defun key-chord-lookup-key1 (keymap key)
  "Like lookup-key but no third arg and no numeric return value."
  (let ((res (lookup-key keymap key)))
    (if (numberp res)
	nil
      ;; else
      res)))

(defun key-chord-lookup-key (key)
  "Lookup KEY in all current key maps."
  (let ((maps (current-minor-mode-maps))
	res)
    (while (and maps (not res))
      (setq res (key-chord-lookup-key1 (car maps) key)
	    maps (cdr maps)))
    (or res
	(if (current-local-map) 
	    (key-chord-lookup-key1 (current-local-map) key))
	(key-chord-lookup-key1 (current-global-map) key))))

(defun key-chord-describe ()
  "List key chord bindings in a help buffer.
\nTwo key chords will be listed twice and there will be Prefix Commands.
Please ignore that."
  (interactive)
  (describe-bindings [key-chord]))

(defun key-chord-input-method (first-char)
  "Input method controlled by key bindings with the prefix `key-chord'."
  (if (and (not (eq first-char key-chord-last-unmatched))
	   (key-chord-lookup-key (vector 'key-chord first-char)))
      (let ((delay (if (key-chord-lookup-key (vector 'key-chord first-char first-char))
		       key-chord-one-key-delay
		     ;; else
		     key-chord-two-keys-delay)))
	(if (if executing-kbd-macro
		(not (memq first-char key-chord-in-last-kbd-macro))
	      (sit-for delay 0 'no-redisplay))
	    (progn
	      (setq key-chord-last-unmatched nil)
	      (list first-char))
	  ;; else input-pending-p
	  (let* ((input-method-function nil)
		 (next-char (read-event))
		 (res (vector 'key-chord first-char next-char)))
	    (if (key-chord-lookup-key res)
		(progn
		  (setq key-chord-defining-kbd-macro
			(cons first-char key-chord-defining-kbd-macro))
		  (list 'key-chord first-char next-char))
	      ;; else put back next-char and return first-char
	      (setq unread-command-events (cons next-char unread-command-events))
	      (if (eq first-char next-char)
		  (setq key-chord-last-unmatched first-char))
	      (list first-char)))))
    ;; else no key-chord keymap
    (setq key-chord-last-unmatched first-char)
    (list first-char)))

(require 'advice)

(defadvice start-kbd-macro (after key-chord activate)
  (setq key-chord-defining-kbd-macro nil))

(defadvice end-kbd-macro (after key-chord activate)
  (setq key-chord-in-last-kbd-macro key-chord-defining-kbd-macro))

(provide 'key-chord)

;;; key-chord.el ends here
