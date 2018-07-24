;;; help-fns+.el --- Extensions to `help-fns.el'.
;;
;; Filename: help-fns+.el
;; Description: Extensions to `help-fns.el'.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2007-2017, Drew Adams, all rights reserved.
;; Created: Sat Sep 01 11:01:42 2007
;; Version: 0
;; Package-Requires: ()
;; Last-Updated: Thu Feb 23 07:33:32 2017 (-0800)
;;           By: dradams
;;     Update #: 2226
;; URL: https://www.emacswiki.org/emacs/download/help-fns%2b.el
;; Doc URL: http://emacswiki.org/HelpPlus
;; Keywords: help, faces, characters, packages, description
;; Compatibility: GNU Emacs: 22.x, 23.x, 24.x, 25.x
;;
;; Features that might be required by this library:
;;
;;   `button', `cl', `cl-lib', `gv', `help-fns', `help-mode', `info',
;;   `macroexp', `naked', `wid-edit', `wid-edit+'.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;    Extensions to `help-fns.el'.  Also includes a redefinition of
;;    `describe-face', which is from `faces.el'.
;;
;;    Note: As of Emacs 24.4, byte-compiling this file in one Emacs
;;    version and using the compiled file in another Emacs version
;;    does not work.
;;
;;
;;  Keys bound here:
;;
;;    `C-h B'      `describe-buffer'
;;    `C-h c'      `describe-command'     (replaces `describe-key-briefly')
;;    `C-h o'      `describe-option'
;;    `C-h C-c'    `describe-key-briefly' (replaces `C-h c')
;;    `C-h C-o'    `describe-option-of-type'
;;    `C-h M-c'    `describe-copying'     (replaces `C-h C-c')
;;    `C-h M-f'    `describe-file'
;;    `C-h M-k'    `describe-keymap'
;;    `C-h M-l'    `find-function-on-key'
;;
;;  Commands defined here:
;;
;;    `describe-buffer', `describe-command', `describe-file',
;;    `describe-keymap', `describe-option', `describe-option-of-type'.
;;
;;  User options defined here:
;;
;;    `help-cross-reference-manuals' (Emacs 23.2+).
;;
;;  Faces defined here:
;;
;;    `describe-variable-value' (Emacs 24+).
;;
;;  Non-interactive functions defined here:
;;
;;    `describe-mode-1', `help-all-exif-data',
;;    `help-commands-to-key-buttons', `help-custom-type',
;;    `help-documentation', `help-documentation-property' (Emacs 23+),
;;    `help-key-button-string', `help-remove-duplicates',
;;    `help-substitute-command-keys', `help-value-satisfies-type-p',
;;    `help-var-inherits-type-p', `help-var-is-of-type-p',
;;    `help-var-matches-type-p', `help-var-val-satisfies-type-p',
;;    `Info-first-index-occurrence' (Emacs 23.2+),
;;    `Info-indexed-find-file' (Emacs 23.2+), `Info-indexed-find-node'
;;    (Emacs 23.2+), `Info-index-entries-across-manuals' (Emacs
;;    23.2+), `Info-index-occurrences' (Emacs 23.2+),
;;    `Info-make-manuals-xref' (Emacs 23.2+).
;;
;;  Internal variables defined here:
;;
;;    `Info-indexed-file' (Emacs 23.2+), `Info-indexed-nodes' (Emacs
;;    23.2+), `variable-name-history'.
;;
;;
;;  ***** NOTE: The following command defined in `faces.el'
;;              has been REDEFINED HERE:
;;
;;  `describe-face'.
;;
;;
;;  ***** NOTE: The following command defined in `help.el'
;;              has been REDEFINED HERE:
;;
;;  `describe-mode'.
;;
;;
;;  ***** NOTE: The following functions defined in `help-fns.el'
;;              have been REDEFINED HERE:
;;
;;  `describe-function', `describe-function-1', `describe-variable',
;;  `help-fns--key-bindings', `help-fns--signature',
;;
;;
;;  ***** NOTE: The following command defined in `package.el'
;;              has been REDEFINED HERE:
;;
;;  `describe-package'.
;;
;;
;;  Put this in your initialization file (`~/.emacs'):
;;
;;    (require 'help-fns+)
;;
;;  Acknowledgement: Passing text properties on doc strings to the
;;  *Help* buffer is an idea from Johan bockgard.  He sent it on
;;  2007-01-24 to emacs-devel@gnu.org, Subject
;;  "display-completion-list should not strip text properties".
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;; 2016/09/17 dadams
;;     describe-function: Fix Emacs bug #24221: let FUNCTION be anonymous.
;; 2015/12/15 dadams
;;     describe-file: Remove `' around file name in title.
;; 2015/09/09 dadams
;;     describe-variable: Fixed test order for non-"" VARDOC, so it does not become t.
;; 2015/09/08 dadams
;;     describe-keymap: Added optional arg SEARCH-SYMBOLS-P.  Follow alias chain of symbol, and describe last one.
;;     describe-variable: Pick up doc from alias, if help-documentation-property returns "".
;; 2015/08/30 dadams
;;     describe-function-1: Typo: auto-do-load -> autoload-do-load.
;; 2015/08/22 dadams
;;     describe-keymap:
;;       Allow arg to be a keymap (not a keymap variable), when not interactive.  Suggestion by erjoalgo.
;; 2015/08/13 dadams
;;     describe-variable:
;;       PREDICATE arg to completing-read needs to use original buffer, not minibuffer, when test boundp.
;;       Fixes Emacs BUG #21252.
;; 2015/08/02 dadams
;;     Updated for Emacs 25
;;      help-fns--signature:
;;        Added arg RAW.  Return DOC if FUNCTION is a keymap.  Use help--make-usage-docstring.
;;        Use help--docstring-quote.  Insert "`X", not "(\` X)", when documenting `X.  Use substitute-command-keys
;;        on args to help-highlight-arguments.
;;      describe-function-1:
;;        Use indirect-function if subr (SIG-KEY).  Moved autoloads forward).  Use help-fns-short-filename.
;;        Use auto-do-load.  But do NOT use curly quotes - e.g., no extra substitute-command-name calls.
;; 2015/04/03 dadams
;;     Use char-before in place of looking-back, for chars before.  See Emacs bug #17284.
;; 2015/03/26 dadams
;;     describe-package: Fix guard to use emacs-minor-version 3, not 24.  Thx to Roshan Shariff.
;; 2015/03/23 dadams
;;     describe-variable (Emacs 24+): Fix terpri's so appearance is better.  Fill region for global value.
;; 2014/11/29 dadams
;;     Info-make-manuals-xref: Control number of newlines before.
;;     describe-function-1: Use same def for Emacs 25.
;;     describe-variable-value: Changed the default colors.
;;     describe-variable: Use face describe-variable-value always.  Fill region for value always.
;;                        Control number of newlines before and after Value:, and after manuals xref.
;; 2014/11/12 dadams
;;     describe-package:
;;       Added version for Emacs 24.4+ - Use package-alist, package--builtins, or package-archive-contents.
;; 2014/11/08 dadams
;;     describe-mode-1: Show major-mode and mode-function also, on a separate line (Emacs bug #18992), filling.
;; 2014/08/10 dadams
;;     describe-command: Bind completion-annotate-function for use with Icicles.
;; 2014/05/11 dadams
;;     help-substitute-command-keys: Bug: \= was not being removed - C-h f replace-regexp showed \=\N, not \N.
;;       Small loop for \=: changed \\\\=$ to \\\\=.
;;       Main loop, when escaped (\=) and odd: Skip the \=: concat before \= with after \=.
;; 2014/05/04 dadams
;;     Use called-interactively only for Emacs 23.2+, since we pass it an arg.
;; 2014/05/02 dadams
;;     describe-package: Updated for Emacs 24.4 - defstruct package-desc.
;; 2014/04/21 dadams
;;     with-selected-frame: Updated for Emacs 24.4.
;;     describe-face: Updated for Emacs 24.4: Try face-at-point for read-face-name default.
;;     describe-file, describe-keymap, describe-function:
;;       Updated for Emacs 24.4: Use with-help-window, not with-output-to-temp-buffer.  See bug #17109.
;;     describe-function-1: Created version for Emacs 24.4+
;;     help-key-button-string: Do not quote :type.
;;     describe-buffer, describe-mode-1, describe-function: Use called-interactively, if available.
;;     Removed autoload cookie for describe-function, describe-keymap (but why?).
;; 2014/03/06 dadams
;;     describe-variable: Fixed typo in regexp: [n] -> [\n].
;; 2014/01/04 dadams
;;     Added: describe-variable-value.
;;     describe-variable (Emacs 24+): Highlight the value with face describe-variable-value.
;; 2013/08/06 dadams
;;     describe-function: Ensure arg is a defined function before calling describe-function-1 (for Emacs 24+).
;; 2013/07/01 dadams
;;     Revert the filling part of yesterday's update.
;; 2013/06/30 dadams
;;     describe-variable for Emacs 24+:
;;       Update for vanilla Emacs 24.4.  Update for Emacs bug #14754: fill printed value so no long lines.
;; 2013/06/16 dadams
;;     describe-(variable|option(-of-type)): Fixed for dumb variable-at-point, which returns 0 for no var.
;; 2013/04/29 dadams
;;     describe-(function|command|variable|option|option-of-type):
;;       Provide default only if symbol is of the right type.  Put default in prompt.
;; 2013/02/08 dadams
;;     describe-variable: Updated wrt Emacs 24 build of 2013-01-30.
;; 2012/11/18 dadams
;;     describe-(variable|function): Add completion-candidate annotation: (option|comand).
;; 2012/10/28 dadams
;;     help-fns--key-bindings: Fixed: forgot to mapconcat over keys.
;; 2012/10/26 dadams
;;     Added: help-fns--key-bindings, help-fns--signature,
;;     Added Emacs 24.3+ version of describe-function-1.  Updated version for 23.2-24.2.
;;     help-substitute-command-keys: Fix for \= when no match for \[, \<, \{ past it.
;; 2012/09/24 dadams
;;     describe-file: Added optional arg NO-ERROR-P.
;; 2012/09/22 dadams
;;     Info-index-occurrences, Info-first-index-occurrence:
;;       Replace Info-directory call by short version.  Better Searching msg.
;; 2012/09/21 dadams
;;     Renamed Info-any-index-occurrences-p to Info-first-index-occurrence.
;;     Info-any-index-occurrences-p: Return the first successful lookup, not t.
;;     Info-index-entries-across-manuals, Info-index-occurrences, Info-any-index-occurrences-p:
;;       Added optional arg INDEX-NODES.
;;     Adjust calls to those fns accordingly, e.g., in define-button-type for help-info-manual-lookup
;;       and help-insert-xref-button in Info-make-manuals-xref.
;; 2012/07/20 dadams
;;     Added: describe-buffer, describe-mode-1.  Bound describe-buffer to C-h B.
;;     describe-mode: Redefined to use describe-mode-1.
;; 2012/07/03 dadams
;;     Info-make-manuals-xref, Info-index-entries-across-manuals, Info-index-occurrences,
;;       Info-any-index-occurrences-p:
;;         Added optional arg NOMSG.
;;     describe-(function|variable|file|package): No message if not interactive-p.
;;     describe-function-1: pass MSGP to Info-make-manuals-xref (i.e. msg always).
;;     describe-(mode|variable|face|keymap|package): Pass proper NOMSG arg to Info-make-manuals-xref.
;; 2012/01/11 dadams
;;     describe-variable: Remove * from beginning of doc string.
;; 2011/11/25 dadams
;;     Reverted yesterday's change and added IMPORTANT note to Commentary.
;; 2011/11/24 dadams
;;     Added Emacs 24 version of with-help-window.  They changed the signature of help-window-setup.
;; 2011/10/14 dadams
;;     describe-mode: Call help-documentation while in mode's buffer, in case no \\<...>.
;; 2011/10/08 dadams
;;     Info-make-manuals-xref: Do nothing if OBJECT is not a string or a symbol (e.g. is a keymap).
;; 2011/10/07 dadams
;;     Added soft require of naked.el.
;;     help-substitute-command-keys, describe-function-1: Use naked-key-description if available.
;; 2011/08/22 dadams
;;     describe-variable (Emacs 23+): Added terpri after Value: (for multiline value).
;; 2011/07/25 dadams
;;     describe-mode:
;;       Put call to help-documentation inside let for maj: else major-mode gets changed to help-mode.
;; 2011/06/26 dadams
;;     Added: help-commands-to-key-buttons, help-documentation(-property),
;;            help-key-button-string, help-substitute-command-keys (Emacs 23+).
;;     describe-(mode|variable|function-1|keymap) for Emacs 23+:
;;       Use help-documentation (with insert and button arg), instead of documentation (with princ).
;; 2011/06/22 dadams
;;     Info-make-manuals-xref: Added optional arg MANUALS.
;; 2011/06/20 dadams
;;     Info(-any)-index-occurrences(-p): Fix pattern: remove arbitrary prefix [^\n]*.
;;     Added, for Emacs 24+: describe-package.
;; 2011/06/14 dadams
;;     Added, for Emacs 23.2+: describe-mode.
;;     Info-make-manuals-xref: Added optional arg NO-NEWLINES-AFTER-P.
;; 2011/06/13 dadams
;;     Added: Info-any-index-occurrences-p.
;;     Info-make-manuals-xref: Use Info-any-index-occurrences-p, not Info-index-occurrences.
;; 2011/06/11 dadams
;;     Added, for Emacs 23.2+:
;;       describe-face, describe-function-1, help-cross-reference-manuals, Info-indexed-find-file,
;;       Info-indexed-find-node, Info-index-entries-across-manuals, Info-index-occurrences,
;;       Info-make-manuals-xref, Info-indexed-file, Info-indexed-nodes.
;;     describe-keymap: Emacs 23.2+: Added link to manuals.
;;     describe-variable: Updated Emacs 23 version, per vanilla.
;;                        Emacs 23.2+: Added link to manuals.
;;     Require info.el for Emacs 23.2+.
;; 2011/04/25 dadams
;;     describe-file: Incorporate autofile bookmark description.  Added optional arg.
;; 2011/03/31 dadams
;;     help-var-(matches|inherits)-type-p: Wrap string-match with save-match-data.
;; 2011/03/17 dadams
;;     describe-file: Added clickable thumbnail image to the help for an image file.
;; 2011/03/02 dadams
;;     Added: help-all-exif-data
;;     describe-file: Show all EXIF data, using help-all-exif-data.
;; 2011/02/22 dadams
;;     describe-file: Show also EXIF data for an image file.
;; 2011/01/04 dadams
;;     Removed autoload cookies from non def* sexps and define-key.
;; 2010/02/12 dadams
;;     Added variable-name-history.
;; 2009/08/30 dadams
;;     describe-keymap: Don't print nil if the map has no doc.
;; 2009/05/26 dadams
;;     describe-variable: Updated wrt latest Emacs 23:
;;       Added file-name-non-directory; removed substitute-command-keys.
;; 2008/09/13 dadams
;;     Updated for latest Emacs 23 CVS.
;;       describe-variable: Create separate version for Emacs 23.
;;       describe-function-1: No longer needed for Emacs 23, since my patch added.
;;       Added: with-selected-frame, with-help-window, at least temporarily.
;;     Require wid-edit.el.
;; 2008/09/02 dadams
;;     describe-function-1, describe-variable:
;;       Emacs 23 uses find-lisp-object-file-name.  Thx to Per Nordlow.
;; 2008/08/19 dadams
;;     describe-keymap: Use insert instead of princ for map part.  Thx to Chong Yidong.
;; 2008/05/20 dadams
;;     describe-function: Different prompt if prefix arg.
;; 2008/03/02 dadams
;;     Moved describe-file here from misc-cmds.el.  Bound to C-h M-f.
;;     Require cl.el at compile time.
;; 2008/02/01 dadams
;;     Bound M-l to find-function-on-key.
;; 2008/01/03 dadams
;;     Added: describe-function-1.  The redefinition fills overlong lines.
;; 2007/12/25 dadams
;;     help-var-inherits-type-p:
;;       Recheck var-type match after set var-type to its car.
;;       Handle string (regexp) TYPES elements.
;;     help-value-satisfies-type-p: Skip type check for string type (regexp).
;;     help-var-is-of-type-p: Doc string.  Use help-var-matches-type-p.
;;     Added: help-var-matches-type-p.
;; 2007/12/24 dadams
;;     help-var-inherits-type-p: Recheck type match after set var-type to its car.
;;     Added: help-custom-type.
;; 2007/12/23 dadams
;;     help-var-is-of-type-p:
;;       Added MODE arg.  Use help-var-inherits-type-p, help-var-val-satisfies-type-p.
;;       Redefined as MODE choice, not just a simple or.  Treat more cases.
;;     Added: help-var-inherits-type-p, help-var-val-satisfies-type-p,
;;            help-value-satisfies-type-p.
;;     describe-option-of-type: Prefix arg means use mode inherit-or-value.
;; 2007/12/22 dadams
;;     help-var-is-of-type-p:
;;       Check supertypes also.  Use both :validate and :match.
;;       Wrap type check in condition-case. Use widget-put instead of plist-put.
;;     Added soft require of wid-edit+.el.
;; 2007/12/21 dadams
;;     help-var-is-of-type-p: Use :validate, not :match, for the test.
;; 2007/12/20 dadams
;;     Moved describe-option-of-type to C-h C-o.
;; 2007/12/15 dadams
;;     Bound C-h c to describe-command and C-h C-c to describe-key-briefly.
;; 2007/12/07 dadams
;;     describe-option-of-type:
;;       Call describe-variable with nil buffer.  Use "nil" as default value.
;; 2007/12/06 dadams
;;     describe-option-of-type:
;;       If nil type, all defcustom vars are candidates.  Use custom-variable-p.
;;       Specific error if no such custom type.
;; 2007/12/04 dadams
;;     Added: describe-option-of-type, help-remove-duplicates, help-var-is-of-type-p.
;;     Bound o to describe-option, M-o to describe-option-of-type,
;;       C-c to describe-command, M-c to describe-copying.
;; 2007/11/28 dadams
;;     Renamed describe-bindings-in-map to describe-keymap.  Added keymap's doc string.
;; 2007/11/22 dadams
;;     Added: describe-bindings-in-map.  Bound to C-h M-k.
;; 2007/11/01 dadams
;;     Corrected require typo: help-mode -> help-fns.
;; 2007/10/18 dadams
;;     Created.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:


(require 'help-fns)

(require 'wid-edit+ nil t) ;; (no error if not found):
                           ;; redefined color widget (for help-var-is-of-type-p)
(require 'wid-edit) ;; widget-convert

(require 'naked nil t) ;; (no error if not found): naked-key-description

(when (or (> emacs-major-version 23)  (and (= emacs-major-version 23)  (> emacs-minor-version 1)))
  (require 'info)) ;; Info-virtual-files

(eval-when-compile (require 'cl)) ;; case, gentemp


;; Quiet the byte-compiler.
(defvar advertised-signature-table)
(defvar dir-local-variables-alist)
(defvar dir-locals-file)
(defvar file-local-variables-alist)
(defvar icicle-mode)                    ; In `icicles-mode.el'
(defvar icicle-pre-minibuffer-buffer)   ; In `icicles-var.el'
(defvar Info-indexed-nodes)             ; In `info.el'
(defvar help-cross-reference-manuals)   ; For Emacs < 23.2
(defvar help-enable-auto-load)          ; For Emacs < 24.3
(defvar package-alist)
(defvar package-archive-contents)
(defvar package--builtins)
(defvar package--initialized)

;;;;;;;;;;;;;;;;;;;;;;;;

(defvar variable-name-history () "Minibuffer history for variable names.")

(define-key help-map "B"    'describe-buffer)
(define-key help-map "c"    'describe-command)
(define-key help-map "o"    'describe-option)
(define-key help-map "\C-c" 'describe-key-briefly)
(define-key help-map "\C-o" 'describe-option-of-type)
(define-key help-map "\M-c" 'describe-copying)
(define-key help-map "\M-f" 'describe-file)
(define-key help-map "\M-k" 'describe-keymap)
(define-key help-map "\M-l" 'find-function-on-key)


;; Need Emacs 23 for version of `make-text-button' that accepts a string.
(when (> emacs-major-version 22)
  (defun help-documentation (function &optional raw add-help-buttons)
    "Same as `documentation', but optionally adds buttons for help.
Non-nil optional arg ADD-HELP-BUTTONS does that, adding buttons to key
descriptions, which link to the key's command help."
    (let ((raw-doc  (documentation function 'RAW)))
      (if raw raw-doc (help-substitute-command-keys raw-doc add-help-buttons))))

  (defun help-documentation-property (symbol prop &optional raw add-help-buttons)
    "Same as `documentation-property', but optionally adds buttons for help.
Non-nil optional arg ADD-HELP-BUTTONS does that, adding buttons to key
descriptions, which link to the key's command help."
    (let ((raw-doc  (documentation-property symbol prop 'RAW)))
      (if raw raw-doc (help-substitute-command-keys raw-doc add-help-buttons))))

  (defun help-commands-to-key-buttons (string)
    "Like `substitute-command-keys', but adds buttons for help on keys.
  Key descriptions become links to help about their commands."
    (help-substitute-command-keys string 'ADD-HELP-BUTTONS))

  (defun help-substitute-command-keys (string &optional add-help-buttons)
    "Same as `substitute-command-keys', but optionally adds buttons for help.
Non-nil optional arg ADD-HELP-BUTTONS does that, adding buttons to key
descriptions, which link to the key's command help."

    ;; REPEAT:
    ;;  Search for first occurrence of any of the patterns: \[...], \{...}, or \<...>.
    ;;  Handle escaping via \=, if present before the pattern or if there is no pattern match.
    ;;  If pattern is a keymap (\<...>): use it from then on.
    ;;  If pattern is a command (\[...]): (a) substitute its key description, (b) put a button on it.
    ;;  If pattern is a bindings spec (\{...}): just substitute the usual text.
    (with-syntax-table emacs-lisp-mode-syntax-table
      (let* ((strg          (copy-sequence string))
             (len-strg      (length strg))
             (ii            0)
             (jj            0)
             (newstrg       "")
             (re-command    "\\\\\\[\\(\\(\\sw\\|\\s_\\)+\\)\\]")
             (re-keymap     "\\\\<\\(\\(\\sw\\|\\s_\\)+\\)>")
             (re-bindings   "\\\\{\\(\\(\\sw\\|\\s_\\)+\\)}")
             (re-any        (concat "\\(" re-command  "\\|" re-keymap "\\|" re-bindings "\\)"))
             (keymap        (or overriding-terminal-local-map   overriding-local-map))
             (msg           nil)
             key bindings  ma  mc  mk  mb)
        (while (< ii len-strg)
          (setq key       nil
                bindings  ()
                strg      (substring strg ii))

          (save-match-data              ; ANY
            (setq ma  (string-match re-any strg))
            (cond ((not ma) ; No \[...], \{...}, or \<...>, but we need to handle \=
                   (setq jj       0
                         newstrg  (concat newstrg (replace-regexp-in-string
                                                   "\\\\=\\(.\\)" "\\1" strg nil nil nil jj)))
                   (when (match-beginning 1) (setq jj  (match-beginning 1)))
                   (setq ii  len-strg))
                  (t
                   (let ((escaped  nil)
                         (odd      nil))
                     (save-match-data
                       (let ((ma/=  ma))
                         (setq ii  ma)
                         (while (string-match "\\\\=" (substring strg 0 ma/=))
                           (setq odd   (not odd)
                                 ma/=  (match-beginning 0))
                           (when odd (setq ii       (- ii 2)
                                           escaped  ma/=)))))
                     (if (not escaped)
                         (setq ii       ma
                               jj       (match-end 0)
                               ma       (match-string-no-properties 0 strg)
                               newstrg  (concat newstrg (substring strg 0 ii)))
                       (setq jj       (match-end 0) ; End of \[...], \{...}, or \<...>
                             ma       (and (not odd)  (match-string-no-properties 0 strg))
                             newstrg  (if odd
                                          (concat newstrg
                                                  (substring strg 0 escaped) ; Before \='s
                                                  (substring strg (+ 2 escaped) ii)) ; After \='s
                                        (concat newstrg (substring strg 0 ii)))))))))

          (when ma

            (save-match-data            ; KEYMAP
              (setq ma  (copy-sequence ma))
              (setq mk  (string-match re-keymap ma))
              (setq mk  (and mk  (match-string-no-properties 0 ma)))
              (when mk
                (setq keymap  (intern (match-string-no-properties 1 ma)))
                (if (boundp keymap)
                    (setq keymap  (symbol-value keymap))
                  (setq msg  (format "\nUses keymap \"%s\", which is not currently defined.\n" keymap))
                  (setq keymap  (or overriding-terminal-local-map  overriding-local-map)))))

            (unless mk                  ; COMMAND
              (save-match-data
                (setq ma  (copy-sequence ma))
                (setq mc  (string-match re-command ma))
                (setq mc  (and mc  (match-string-no-properties 0 ma)))
                (setq mc  (and mc  (intern (substring mc 2 -1)))) ; Remove \[...] envelope
                (when mc
                  (let ((follow-remap  t))
                    (while (and (setq key  (where-is-internal mc keymap 'FIRSTONLY))
                                (vectorp key)  (> (length key) 1)  (eq 'remap (aref key 0))
                                (symbolp (aref key 1)) follow-remap)
                      (setq mc            (aref key 1)
                            follow-remap  nil)))
                  (setq key  (if key
                                 (if (fboundp 'naked-key-description)
                                     (naked-key-description key)
                                   (key-description key))
                               (concat "M-x " (symbol-name mc))))
                  (when add-help-buttons (setq key  (help-key-button-string key mc))))))

            (unless (or mk  mc)         ; BINDINGS
              (save-match-data
                (setq ma  (copy-sequence ma))
                (setq mb  (string-match re-bindings ma))
                (setq mb  (and mb  (match-string-no-properties 0 ma)))
                (when mb
                  (setq bindings  (intern (match-string-no-properties 1 ma)))
                  (cond ((boundp bindings)
                         (setq bindings  (substitute-command-keys mb))) ; Use original - no buttons.
                        (t
                         (setq msg  (format "\nUses keymap \"%s\", which is not currently defined.\n"
                                            bindings))
                         (setq bindings  nil))))))

            (unless mk (setq newstrg  (concat newstrg (or key  bindings  (substring strg ii jj)))))
            (setq ii  (or jj  len-strg))))

        (if (string= string newstrg)
            string ; Return original string, not a copy, if no changes.
          newstrg))))

  (defun help-key-button-string (key-description command)
    "Return a button for KEY-DESCRIPTION that links to the COMMAND description.
KEY-DESCRIPTION is a key-description string.
COMMAND is the command (a symbol) associated with the key described.
Return a copy of string KEY-DESCRIPTION with button properties added.
Clicking the button shows the help for COMMAND."
    (let ((new-key  (copy-sequence key-description)))
      (make-text-button new-key nil 'button (list t) :type 'help-function 'help-args (list command))
      new-key)))


(when (boundp 'Info-virtual-files)      ; Emacs 23.2+
  (defcustom help-cross-reference-manuals '(("emacs" "elisp"))
    "*Manuals to search, for a `*Help*' buffer link to the manuals.
A cons.

 The car is a list of manuals to search, or the symbol `all', to
  search all.  If nil, then do not create a cross-reference link.

 The cdr is a boolean:

  Non-`nil' means search the manuals, then create a cross-ref link:
        create it only if some search hits are found.

  `nil' means create a cross-ref link without searching manuals
        first (but only if there are some manuals to search)."
    :set #'(lambda (sym defs) (custom-set-default sym defs) (setq Info-indexed-nodes  ()))
    :type '(cons
            (choice :tag "Which Manuals"
             (repeat :tag "Specific Manuals (files)" string)
             (const  :tag "All Manuals" all))
            (boolean :tag "Search Before Creating Button?"))
    :group 'help)

  (defvar Info-indexed-file "*Indexed*"
    "Info file for virtual manual from `Info-index-entries-across-manuals'.")

  (defvar Info-indexed-nodes ()
    "Alist of cached nodes with matching index entries.
Each element is (NODENAME STRING MATCHES), where:
 NODENAME is the name of the node that is indexed,
 STRING is the search string passed to `Info-index-occurrences',
 MATCHES is a list of index matches found by `Info-index-occurrences'.

This has the same structure as `Info-apropos-nodes', but the search
was made by `Info-index-occurrences', not by `Info-apropos-matches',
so that matches are exact (ignoring case).")

  (defun Info-indexed-find-file (filename &optional _noerror)
    "Index-search implementation of `Info-find-file'."
    filename)

  (defun Info-indexed-find-node (_filename nodename &optional _no-going-back)
    "Index-search implementation of `Info-find-node-2'."
    (let* ((nodeinfo  (assoc nodename Info-indexed-nodes))
           (matches   (nth 2 nodeinfo)))
      (when matches
        (insert (format "\n\^_\nFile: %s,  Node: %s,  Up: Top\n\n" Info-indexed-file nodename))
        (insert "Index Matches\n")
        (insert "*************\n\n")
        (insert "Index entries that match `" (nth 1 nodeinfo) "':\n\n")
        (insert "\0\b[index\0\b]\n")
        (if (eq matches t)
            (insert "No matches found.\n")
          (insert "* Menu:\n\n")
          (dolist (entry  matches)
            (insert (format "* %-38s (%s)%s.%s\n"  (format "%s [%s]:" (nth 1 entry) (nth 0 entry))
                            (nth 0 entry)  (nth 2 entry)
                            (if (nth 3 entry) (format " (line %s)" (nth 3 entry)) ""))))))))
  (add-to-list 'Info-virtual-files '("\\`\\*Indexed\\*\\'"
                                     (find-file . Info-indexed-find-file)
                                     (find-node . Info-indexed-find-node)
                                     ;; (slow . t) ; $$$$$$ Useless here?
                                     ))

  (defun Info-make-manuals-xref (object &optional no-newlines-after-p manuals-spec nomsg)
    "Create a cross-ref link for index entries for OBJECT in manuals.
Non-`nil' optional arg NO-NEWLINES-AFTER-P means do not add two
newlines after the cross reference.

Optional arg MANUALS-SPEC controls which manuals to search.  It has
the same form as option `help-cross-reference-manuals', and it
defaults to the value of that option.

Do nothing if the car of MANUALS-SPEC is nil (no manuals to search).
If its cdr is `nil' then create the link without first searching any
manuals.  Otherwise, create the link only if there are search hits in
the manuals."
    (when (or (stringp object)  (symbolp object)) ; Exclude, e.g., a keymap as OBJECT.
      (unless manuals-spec (setq manuals-spec  help-cross-reference-manuals))
      (when (car manuals-spec) ; Create no link if no manuals to search.
        (let ((books         (car manuals-spec))
              (search-now-p  (cdr manuals-spec))
              (symb-name     (if (stringp object) object (symbol-name object))))
          (when (or (not search-now-p)
                    (save-current-buffer (Info-first-index-occurrence symb-name () books nomsg)))
            (let ((buffer-read-only  nil)
                  (nl-before         (cond ((and (eq ?\n (char-before)) ; Quicker than `looking-back', apparently.
                                                 (eq ?\n (char-before (1- (point))))) "")
                                           ((eq ?\n (char-before))                    "\n")
                                           (t                                         "\n\n"))))
              (insert (format "%sFor more information %s the " nl-before (if (cdr manuals-spec) "see" "check")))
              (help-insert-xref-button "manuals" 'help-info-manual-lookup symb-name () books)
              (insert ".")
              (unless no-newlines-after-p (insert "\n\n"))))))))

  (when (and (> emacs-major-version 21)
             (condition-case nil (require 'help-mode nil t) (error nil))
             (get 'help-xref 'button-category-symbol)) ; In `button.el'
    (define-button-type 'help-info-manual-lookup
        :supertype 'help-xref
        'help-function #'(lambda (string &optional index-nodes books nomsg)
                           (Info-index-entries-across-manuals string () books nomsg))
        'help-echo "mouse-2, RET: Look it up in the manuals"))

  (defun Info-index-entries-across-manuals (string &optional index-nodes manuals nomsg)
    "Look up STRING in Info MANUALS on your system.
Looks for exact matches (ignoring case): STRING is expected to be an
index entry.  Build an Info menu of the possible matches.

Optional arg INDEX-NODES are the index nodes in MANUALS to search.
 By default (nil value), all indexes are searched.
Optional arg MANUALS is the list of manuals to search, or the symbol
  `all', to search all.
Optional arg NOMSG non-nil means do not display a progress message."
    (let ((nodes  Info-indexed-nodes)
          nodename)
      (while (and nodes  (not (equal string (nth 1 (car nodes)))))  (setq nodes  (cdr nodes)))
      (if nodes
          (Info-find-node Info-indexed-file (car (car nodes)))
        (setq nodename  (format "Index for `%s'" string))
        (push (list nodename string (Info-index-occurrences string index-nodes manuals nomsg))
              Info-indexed-nodes)
        (Info-find-node Info-indexed-file nodename))))

  ;; Similar to `Info-apropos-matches', but using exact matches (ignoring case).
  (defun Info-index-occurrences (index-entry &optional index-nodes manuals nomsg)
    "Collect occurrences of INDEX-ENTRY in INDEX-NODES of MANUALS.
Return a list of the form ((FILE INDEX-ENTRY NODE LINE) ...), where:
 FILE is the name of an Info file,
 NODE is an Info node name,
 LINE is the line number of the INDEX-ENTRY occurrence in that node.

Optional arg INDEX-NODES are the index nodes in MANUALS to search.
 By default (nil value), search all indexes of each manual.
Optional arg MANUALS is the list of manuals to search, or the symbol
  `all', to search all.
Optional arg NOMSG non-nil means do not display a progress message."
    (unless (string= index-entry "")
      ;; Unlike `Info-apropos-matches', we match only the exact string as an index entry.
      (let ((pattern  (format "\n\\* +\\(%s\\):[ \t]+\\([^\n]+\\)\
\\.\\(?:[ \t\n]*(line +\\([0-9]+\\))\\)?"
                              (regexp-quote index-entry)))
            matches node)
        (unless nomsg
          (message "Searching indexes of %s..."
                   (cond ((eq manuals 'all) "all manuals")
                         ((null (cadr manuals)) (concat (car manuals) " manual"))
                         (t (concat "manuals " (mapconcat #'identity manuals ", "))))))
        (condition-case nil
            (with-temp-buffer
              (when (eq manuals 'all) (setq manuals  ()))
              (Info-mode)
              ;; Next two lines are essentially `(Info-directory)'.
              (info-initialize)
              (Info-find-node-2 "dir" "top" 'NO-GOING-BACK)
              (unless manuals
                (goto-char (point-min))
                (re-search-forward "\\* Menu: *\n" nil t)
                (let (manual)
                  (while (re-search-forward "\\*.*: *(\\([^)]+\\))" nil t)
                    ;; `add-to-list' ensures no dups in `manuals', so the `dolist' runs faster.
                    (setq manual  (match-string 1))
                    (set-text-properties 0 (length manual) nil manual)
                    (add-to-list 'manuals manual))))
              (dolist (manual  manuals)
                (unless nomsg (message "Searching indexes of manual `%s'..." manual))
                (when (or index-nodes
                          (setq index-nodes  (Info-index-nodes (Info-find-file manual))))
                  (Info-find-node manual (car index-nodes))
                  (while (progn (goto-char (point-min))
                                (while (re-search-forward pattern nil t)
                                  (setq matches  (cons (list manual
                                                             (match-string-no-properties 1)
                                                             (match-string-no-properties 2)
                                                             (match-string-no-properties 3))
                                                       matches)))
                                (setq index-nodes  (cdr index-nodes)
                                      node         (car index-nodes)))
                    (Info-goto-node node)))))
          (error nil))
        matches)))

  ;; Like `Info-index-occurrences', but return only the first occurrence found.
  (defun Info-first-index-occurrence (index-entry &optional index-nodes manuals nomsg)
    "Return nil or an occurrence of INDEX-ENTRY in INDEX-NODES of MANUALS.
Search INDEX-NODES and MANUALS in order.
A non-nil return value is the first first successful index lookup, in
the form (FILE INDEX-ENTRY NODE LINE) - see `Info-index-occurrences'.

Optional arg INDEX-NODES are the index nodes of MANUALS to search.
 By default (nil value), search all indexes of each manual.
Optional arg MANUALS is the list of manuals to search, or the symbol
  `all', to search all.
Optional arg NOMSG non-nil means do not display a progress message."
    (and (not (string= index-entry ""))
         ;; Unlike `Info-apropos-matches', we match only the exact string as an index entry.
         (let ((pattern  (format "\n\\* +\\(%s\\):[ \t]+\\([^\n]+\\)\
\\.\\(?:[ \t\n]*(line +\\([0-9]+\\))\\)?"
                                 (regexp-quote index-entry)))
               (found    nil)
               node)
           (unless nomsg
             (message "Searching indexes of %s..."
                      (cond ((eq manuals 'all) "all manuals")
                            ((null (cadr manuals)) (concat (car manuals) " manual"))
                            (t (concat "manuals " (mapconcat #'identity manuals ", "))))))
           (condition-case nil
               (with-temp-buffer
                 (when (eq manuals 'all) (setq manuals  ()))
                 (Info-mode)
                 ;; Next two lines are essentially `(Info-directory)'.
                 (info-initialize)
                 (Info-find-node-2 "dir" "top" 'NO-GOING-BACK)
                 (unless manuals
                   (goto-char (point-min))
                   (re-search-forward "\\* Menu: *\n" nil t)
                   (let (manual)
                     (while (re-search-forward "\\*.*: *(\\([^)]+\\))" nil t)
                       ;; `add-to-list' ensures no dups in `manuals', so the `dolist' runs faster.
                       (setq manual  (match-string 1))
                       (set-text-properties 0 (length manual) nil manual)
                       (add-to-list 'manuals manual))))
                 (setq found  (catch 'Info-first-index-occurrence
                                (dolist (manual  manuals)
                                  (unless nomsg
                                    (message "Searching indexes of manual `%s'..." manual))
                                  (when (or index-nodes
                                            (setq index-nodes  (Info-index-nodes
                                                                (Info-find-file manual))))
                                    (Info-find-node manual (car index-nodes))
                                    (while (progn (goto-char (point-min))
                                                  (when (re-search-forward pattern nil t)
                                                    (throw 'Info-first-index-occurrence
                                                      (list manual
                                                            (match-string-no-properties 1)
                                                            (match-string-no-properties 2)
                                                            (match-string-no-properties 3))))
                                                  (setq index-nodes  (cdr index-nodes)
                                                        node         (car index-nodes)))
                                      (Info-goto-node node))))
                                nil)))
             (error nil))
           found)))

  (defun describe-buffer (&optional buffer-name) ; Bound to `C-h B'
    "Describe the existing buffer named BUFFER-NAME.
The description includes the information provided by `describe-mode'.
By default, describe the current buffer."
    ;; (interactive "bDescribe buffer: ")
    (interactive "@")
    (unless buffer-name (setq buffer-name  (buffer-name)))
    (help-setup-xref `(describe-buffer ,buffer-name) (called-interactively-p 'interactive))
    (let ((buf  (get-buffer buffer-name)))
      (unless (and buf  (buffer-live-p buf))  (error(format "No such live buffer `%s'" buffer-name)))
      (let* ((file       (or (buffer-file-name buf)
                             (with-current-buffer buf
                               (and (eq major-mode 'dired-mode)  default-directory))))
             (help-text  (concat
                          (format "Buffer `%s'\n%s\n\n" buffer-name (make-string
                                                                     (+ 9 (length buffer-name)) ?-))
                          (and file  (format "File/directory:\t%s\n" file))
                          (format "Mode:\t\t%s\n"
                                  (with-current-buffer buf (format-mode-line mode-name)))
                          (format "Size in chars:\t%g\n" (buffer-size buf))
                          (with-current-buffer buf
                            (if (not buffer-display-time)
                                "Never displayed\n"
                              (format "Last displayed:\t%s\n"
                                      (format-time-string
                                       ;; Could use this, for short format: "%02H:%02M:%02S"
                                       ;; Or this, for a bit longer: "%_3a %_2l:%02M:%02S %_2p"
                                       "%a %b %e %T %Y (%z)"
                                       buffer-display-time))))
                          (format "Modified:\t%s\n" (if (buffer-modified-p buf) "yes" "no"))
                          (with-current-buffer buf
                            (format "Read-only:\t%s\n\n\n" (if buffer-read-only "yes" "no"))))))
        (with-help-window (help-buffer)
          (describe-mode-1 buf))
        (with-current-buffer (help-buffer)
          (let ((inhibit-read-only  t))
            (goto-char (point-min))
            (insert help-text))))))


  ;; REPLACE ORIGINAL
  ;;
  ;; Use `describe-mode-1', which is different from the original `describe-mode' in these ways:
  ;;
  ;;  1. Call `Info-make-manuals-xref' to create a cross-ref link to manuals.
  ;;  2. Add key-description buttons to command help.  Use `insert', not `princ'.
  ;;
  (defun describe-mode (&optional buffer)
    "Display documentation of current major mode and minor modes.
A brief summary of the minor modes comes first, followed by the
major mode description.  This is followed by detailed
descriptions of the minor modes, each on a separate page.

For this to work correctly for a minor mode, the mode's indicator
variable \(listed in `minor-mode-alist') must also be a function
whose documentation describes the minor mode."
    (interactive "@")
    (unless buffer (setq buffer  (current-buffer)))
    (help-setup-xref (list #'describe-mode buffer) (called-interactively-p 'interactive))
    (with-help-window (help-buffer) (describe-mode-1 buffer))
    nil)                       ; For the sake of IELM and maybe others

  (defun describe-mode-1 (buffer)
    "Helper for `describe-mode'.
Does everything except create the help window and set up the
back/forward buttons, so you can use this in other help commands that
have their own back/forward buttons."
    ;; For the sake of `help-do-xref' and `help-xref-go-back', do not switch buffers before calling `help-buffer'.
    (with-current-buffer buffer
      (let (minor-modes)
        ;; Older packages do not register in minor-mode-list but only in `minor-mode-alist'.
        (dolist (x minor-mode-alist)
          (setq x  (car x))
          (unless (memq x minor-mode-list) (push x minor-mode-list)))
        (dolist (mode minor-mode-list) ; Find enabled minor mode we will want to mention.
          ;; Document minor mode if listed in `minor-mode-alist', non-nil, and has a function def.
          (let ((fmode  (or (get mode :minor-mode-function) mode)))
            (and (boundp mode)  (symbol-value mode)  (fboundp fmode)
                 (let ((pretty-minor-mode  (if (string-match "\\(\\(-minor\\)?-mode\\)?\\'"
                                                             (symbol-name fmode))
                                               (capitalize (substring (symbol-name fmode)
                                                                      0 (match-beginning 0)))
                                             fmode)))
                   (push (list fmode pretty-minor-mode
                               (format-mode-line (assq mode minor-mode-alist)))
                         minor-modes)))))
        (setq minor-modes  (sort minor-modes (lambda (a b) (string-lessp (cadr a) (cadr b)))))
        (when minor-modes
          (princ "Enabled minor modes:\n")
          (make-local-variable 'help-button-cache)
          (with-current-buffer standard-output
            (dolist (mode  minor-modes)
              (let ((mode-function      (nth 0 mode))
                    (pretty-minor-mode  (nth 1 mode))
                    (indicator          (nth 2 mode)))
                (add-text-properties 0 (length pretty-minor-mode) '(face bold) pretty-minor-mode)
                (save-excursion
                  (goto-char (point-max))
                  (princ "\n\f\n")
                  (push (point-marker) help-button-cache)
                  ;; Document the minor modes fully.
                  (insert pretty-minor-mode)
                  (princ (format " minor mode:\n(`%s'; %s)\n" mode-function (if (zerop (length indicator))
                                                                                "no indicator"
                                                                              (format "indicator%s" indicator))))
                  (save-excursion
                    (fill-region-as-paragraph (line-beginning-position 0) (line-end-position 0) nil t t))
                  (with-current-buffer standard-output
                    (insert (help-documentation mode-function nil 'ADD-HELP-BUTTONS)))
                  (Info-make-manuals-xref mode-function
                                          t nil (not (called-interactively-p 'interactive)))) ; Link manuals.
                (insert-button pretty-minor-mode 'action (car help-button-cache)
                               'follow-link t 'help-echo "mouse-2, RET: show full information")
                (newline)))
            (forward-line -1)
            (fill-paragraph nil)
            (forward-line 1))
          (princ "\n(Information about these minor modes follows the major mode info.)\n\n"))
        (let ((mode  mode-name))        ; Document the major mode.
          (with-current-buffer standard-output
            (let ((start  (point)))
              (insert (format-mode-line mode nil nil buffer))
              (add-text-properties start (point) '(face bold)))))
        (princ " mode")
        (let* ((mode       major-mode)
               (file-name  (find-lisp-object-file-name mode nil)))
          (when file-name
            (princ (concat " defined in `" (file-name-nondirectory file-name) "'"))
            (with-current-buffer standard-output ; Make a hyperlink to the library.
              (save-excursion (re-search-backward "`\\([^`']+\\)'" nil t)
                              (help-xref-button 1 'help-function-def mode file-name))))
          (with-current-buffer standard-output
            (insert (format " (`%s'):\n" mode))
            (save-excursion
              (fill-region-as-paragraph (line-beginning-position 0) (line-end-position 0) nil t t))))
        (let* ((maj      major-mode)
               (maj-doc  (help-documentation maj nil 'ADD-HELP-BUTTONS)))
          (with-current-buffer standard-output
            (insert maj-doc)
            (Info-make-manuals-xref
             maj t nil (not (called-interactively-p 'interactive)))))))) ; Link to manuals.
  )



;; REPLACE ORIGINAL in `help-fns.el':
;;
;; 1. Preferred candidate is `symbol-nearest-point'.
;; 2. With a prefix argument, candidates are commands only.
;; 3. No no-function message if not called interactively.
;; 4. Works for anonymous functions too: lambda forms and byte-compiled functions. (Fixes Emacs bug #24221.)
;;
(defun describe-function (function &optional commandp)
  "Display the full documentation of FUNCTION (a symbol).
FUNCTION names an Emacs Lisp function, possibly a user command.
With a prefix argument, candidates are only commands (interactive).

Default candidate is: preferably the `symbol-nearest-point', or else
the innermost function call surrounding point
\(`function-called-at-point').
Return the description that was displayed, as a string."
  (interactive
   (let* ((fn                            (or (and (fboundp 'symbol-nearest-point)  (symbol-nearest-point))
                                             (function-called-at-point)))
          (enable-recursive-minibuffers  t)
          (completion-annotate-function  (lambda (fn) (and (commandp (intern-soft fn))  "  (command)")))
          (type                          (if current-prefix-arg 'command 'function))
          (prompt                        (format "Describe %s%s: " type
                                                 (if (if current-prefix-arg (commandp fn) (fboundp fn))
                                                     (format " (default %s)" fn)
                                                   "")))
          val)
     (setq val  (completing-read prompt obarray (if current-prefix-arg 'commandp 'fboundp) t nil nil
                                 (and (if current-prefix-arg (commandp fn) (fboundp fn))  (symbol-name fn))))
     (list (if (equal val "") fn (intern val)) current-prefix-arg)))
  (let* ((interactivep  (if (or (> emacs-major-version 23) ; Emacs 23.1 `called-interactively' accepts no arg.
                                (and (= emacs-major-version 23)  (> emacs-minor-version 1)))
                            (called-interactively-p 'interactive)
                          (interactive-p)))
         (err/msg-fn    (if interactivep #'message #'error))
         (fn/cmd-txt    (if commandp 'command 'function)))
    (if (and interactivep  (not function))
        (funcall err/msg-fn "You did not specify a function symbol") ; Avoid "Not a defined function: `nil'".
      (if (not (if commandp
                   (commandp function)
                 (or (functionp function) ; Allow anonymous functions (Emacs bug #24221).
                     (and function  (fboundp (intern-soft function)))))) ; Allow macros and special forms.
          (funcall err/msg-fn "Not a defined %s: `%S'" fn/cmd-txt function)
        (help-setup-xref (list #'describe-function function)
                         (if (or (> emacs-major-version 23) ; Emacs 23.1 `called-interactively' accepts no arg.
                                 (and (= emacs-major-version 23)  (> emacs-minor-version 1)))
                             (called-interactively-p 'interactive)
                           (interactive-p)))
        (save-excursion
          (if (fboundp 'with-help-window)
              (with-help-window  (help-buffer) ; Emacs 24.4 needs this - see Emacs bug #17109.
                (prin1 function)
                ;; Use " is " instead of ": " so it is easier to get the function name using `forward-sexp'.
                (princ " is ")
                (describe-function-1 function)
                (with-current-buffer standard-output (buffer-string))) ; Return help text.
            (with-output-to-temp-buffer (help-buffer)
              (prin1 function)
              ;; Use " is " instead of ": " so it is easier to get the function name using `forward-sexp'.
              (princ " is ")
              (describe-function-1 function)
              (print-help-return-message)
              (with-current-buffer standard-output (buffer-string))))))))) ; Return help text.



;; REPLACE ORIGINAL in `help-fns.el' (`help.el', for Emacs < 22):
;;
;; Fill long lines.  Add `,' before "which".
;;
(when (< emacs-major-version 23)
  (defun describe-function-1 (function)
    (let* ((def  (if (symbolp function) (symbol-function function) function))
           (beg  (if (commandp def)  "an interactive "  "a "))
           (pt1  (with-current-buffer (help-buffer) (point)))
           file-name string)
      (setq string  (cond ((or (stringp def)  (vectorp def)) "a keyboard macro")
                          ((subrp def) (if (eq 'unevalled (cdr (subr-arity def)))
                                           (concat beg "special form")
                                         (concat beg "built-in function")))
                          ((byte-code-function-p def) (concat beg "compiled Lisp function"))
                          ((symbolp def)
                           (while (symbolp (symbol-function def))
                             (setq def  (symbol-function def)))
                           (format "an alias for `%s'" def))
                          ((eq (car-safe def) 'lambda) (concat beg "Lisp function"))
                          ((eq (car-safe def) 'macro) "a Lisp macro")
                          ((eq (car-safe def) 'autoload)
                           (setq file-name (nth 1 def))
                           (format "%s autoloaded %s" (if (commandp def) "an interactive"  "an")
                                   (if (eq (nth 4 def) 'keymap)
                                       "keymap"
                                     (if (nth 4 def) "Lisp macro"  "Lisp function"))))
                          ((keymapp def)
                           (let ((is-full  nil)
                                 (elts     (cdr-safe def)))
                             (while elts
                               (when (char-table-p (car-safe elts))
                                 (setq is-full  t
                                       elts     ()))
                               (setq elts  (cdr-safe elts)))
                             (if is-full "a full keymap" "a sparse keymap")))
                          (t "")))
      (princ string)
      (with-current-buffer standard-output
        (save-excursion (save-match-data (when (re-search-backward "alias for `\\([^`']+\\)'" nil t)
                                           (help-xref-button 1 'help-function def)))))
      (unless file-name (setq file-name  (symbol-file function 'defun)))
      (setq file-name  (describe-simplify-lib-file-name file-name))
      (when (equal file-name "loaddefs.el")
        ;; Find the real def site of the preloaded function.  This is necessary only for defaliases.
        (let ((location  (condition-case nil
                             (find-function-search-for-symbol function nil "loaddefs.el")
                           (error nil))))
          (when location
            (with-current-buffer (car location)
              (goto-char (cdr location))
              (when (re-search-backward "^;;; Generated autoloads from \\(.*\\)" nil t)
                (setq file-name  (match-string 1)))))))
      (when (and (null file-name)  (subrp def)) ; Find the C source file name.
        (setq file-name  (if (get-buffer " *DOC*") (help-C-file-name def 'subr) 'C-source)))
      (when file-name
        (princ " in `")
        ;; We used to add `.el' to file name, but that's wrong when the user used `load-file'.
        (princ (if (eq file-name 'C-source) "C source code" file-name))
        (princ "'")
        (with-current-buffer standard-output ; Make a hyperlink to the library.
          (save-excursion (re-search-backward "`\\([^`']+\\)'" nil t)
                          (help-xref-button 1 'help-function-def function file-name))))
      (princ ".")
      (with-current-buffer (help-buffer)
        (fill-region-as-paragraph (save-excursion (goto-char pt1) (forward-line 0) (point)) (point)))
      (terpri)(terpri)
      (when (commandp function)
        (let ((pt2  (with-current-buffer (help-buffer) (point))))
          (if (and (eq function 'self-insert-command)
                   (eq (key-binding "a") 'self-insert-command)
                   (eq (key-binding "b") 'self-insert-command)
                   (eq (key-binding "c") 'self-insert-command))
              (princ "It is bound to many ordinary text characters.\n")
            (let* ((remapped  (command-remapping function))
                   (keys      (where-is-internal
                               (or remapped  function) overriding-local-map nil nil))
                   non-modified-keys)
              (dolist (key  keys) ; Which non-control non-meta keys run this command?
                (when (member (event-modifiers (aref key 0)) '(nil (shift)))
                  (push key non-modified-keys)))
              (when remapped
                (princ "It is remapped to `") (princ (symbol-name remapped)) (princ "'"))
              (when keys
                (princ (if remapped ", which is bound to "  "It is bound to "))
                ;; If lots of ordinary text characters run this command, don't mention them one by one.
                (if (< (length non-modified-keys) 10)
                    (princ (mapconcat (if (fboundp 'naked-key-description)
                                          #'naked-key-description
                                        #'key-description)
                                      keys ", "))
                  (dolist (key  non-modified-keys) (setq keys  (delq key keys)))
                  (if keys
                      (progn (princ (mapconcat (if (fboundp 'naked-key-description)
                                                   #'naked-key-description
                                                 #'key-description)
                                               keys ", "))
                             (princ ", and many ordinary text characters"))
                    (princ "many ordinary text characters"))))
              (when (or remapped  keys  non-modified-keys) (princ ".") (terpri))))
          (with-current-buffer (help-buffer) (fill-region-as-paragraph pt2 (point)))
          (terpri)))
      (let* ((arglist  (help-function-arglist def))
             (doc      (documentation function))
             (usage    (help-split-fundoc doc function)))
        (with-current-buffer standard-output
          ;; If definition is a keymap, skip arglist note.
          (unless (keymapp def)
            (let* ((use   (cond (usage  (setq doc  (cdr usage)) (car usage))
                                ((listp arglist) (format "%S" (help-make-usage function arglist)))
                                ((stringp arglist) arglist)
                                ;; Maybe the arglist is in the docstring of the alias.
                                ((let ((fun  function))
                                   (while (and (symbolp fun)
                                               (setq fun  (symbol-function fun))
                                               (not (setq usage  (help-split-fundoc (documentation fun)
                                                                                    function)))))
                                   usage)
                                 (car usage))
                                ((or (stringp def)  (vectorp def))
                                 (format "\nMacro: %s" (format-kbd-macro def)))
                                (t  "[Missing arglist.  Please make a bug report.]")))
                   (high  (help-highlight-arguments use doc)))
              (let ((fill-begin  (point)))
                (insert (car high) "\n")
                (fill-region fill-begin (point)))
              (setq doc  (cdr high))))
          (let ((obsolete  (and (symbolp function) ; function might be a lambda construct.
                                (get function 'byte-obsolete-info))))
            (when obsolete
              (princ "\nThis function is obsolete")
              (when (nth 2 obsolete) (insert (format " since %s" (nth 2 obsolete))))
              (insert ";\n" (if (stringp (car obsolete))
                                (car obsolete)
                              (format "use `%s' instead." (car obsolete)))
                      "\n"))
            (insert "\n" (or doc  "Not documented."))))))))


;; REPLACE ORIGINAL in `help-fns.el':
;;
;; 1. Call `Info-make-manuals-xref' to create a cross-ref link to manuals.
;; 2. Add key-description buttons to command help.
;;
(when (and (boundp 'Info-virtual-files)      ; Emacs 23.2 through 24.2
           (not (fboundp 'help-fns--autoloaded-p)))
  (defun describe-function-1 (function)
    (let* ((advised        (and (symbolp function)  (featurep 'advice)  (ad-get-advice-info function)))
           ;; If the function is advised, use the symbol that has the real def, if already set up.
           (real-function  (or (and advised  (let ((origname  (cdr (assq 'origname advised))))
                                               (and (fboundp origname)  origname)))
                               function))
           ;; Get the real definition.
           (def            (if (symbolp real-function) (symbol-function real-function) function))
           (aliased        (symbolp def))
           (real-def       (if aliased
                               (let ((fn  def))
                                 (while (and  (fboundp fn)  (symbolp (symbol-function fn)))
                                   (setq fn  (symbol-function fn)))
                                 fn)
                             def))
           (file-name      (find-lisp-object-file-name function def))
           (beg            (if (commandp def) "an interactive "  "a "))
           (pt1            (with-current-buffer (help-buffer) (point)))
           string errtype)
      (setq string  (cond ((or (stringp def)  (vectorp def))  "a keyboard macro")
                          ((subrp def)  (if (eq 'unevalled (cdr (subr-arity def)))
                                            (concat beg "special form")
                                          (concat beg "built-in function")))
                          ((byte-code-function-p def)  (concat beg "compiled Lisp function"))
                          ((symbolp def)
                           (while (and (fboundp def)  (symbolp (symbol-function def)))
                             (setq def  (symbol-function def)))
                           ;; Handle (defalias 'foo 'bar), where bar is undefined.
                           (unless (fboundp def) (setq errtype  'alias))
                           (format "an alias for `%s'" def))
                          ((eq (car-safe def) 'lambda)  (concat beg "Lisp function"))
                          ((eq (car-safe def) 'macro)  "a Lisp macro")
                          ((eq (car-safe def) 'closure)  (concat beg "Lisp closure"))
                          ((eq (car-safe def) 'autoload)
                           (format "%s autoloaded %s" (if (commandp def) "an interactive"  "an")
                                   (if (eq (nth 4 def) 'keymap)
                                       "keymap"
                                     (if (nth 4 def) "Lisp macro" "Lisp function"))))
                          ((keymapp def)  (let ((is-full  nil)
                                                (elts     (cdr-safe def)))
                                            (while elts
                                              (when (char-table-p (car-safe elts))
                                                (setq is-full  t
                                                      elts     ()))
                                              (setq elts  (cdr-safe elts)))
                                            (if is-full "a full keymap"  "a sparse keymap")))
                          (t  "")))
      (princ string)
      (if (eq errtype 'alias)
          (princ ",\nwhich is not defined.  Please make a bug report.")
        (with-current-buffer standard-output
          (save-excursion (save-match-data (when (re-search-backward "alias for `\\([^`']+\\)'" nil t)
                                             (help-xref-button 1 'help-function def)))))
        (when file-name
          (princ " in `")
          ;; We used to add `.el' to the file name, but that's wrong when the user used `load-file'.
          (princ (if (eq file-name 'C-source) "C source code" (file-name-nondirectory file-name)))
          (princ "'")
          ;; Make a hyperlink to the library.
          (with-current-buffer standard-output
            (save-excursion (re-search-backward "`\\([^`']+\\)'" nil t)
                            (help-xref-button 1 'help-function-def function file-name))))
        (princ ".")
        (with-current-buffer (help-buffer)
          (fill-region-as-paragraph (save-excursion (goto-char pt1) (forward-line 0) (point)) (point)))
        (terpri) (terpri)
        (when (commandp function)
          (let ((pt2       (with-current-buffer (help-buffer) (point)))
                (remapped  (command-remapping function)))
            (unless (memq remapped '(ignore undefined))
              (let ((keys  (where-is-internal (or remapped  function) overriding-local-map nil nil))
                    non-modified-keys)
                (if (and (eq function 'self-insert-command)
                         (vectorp (car-safe keys))
                         (consp (aref (car keys) 0)))
                    (princ "It is bound to many ordinary text characters.\n")
                  (dolist (key  keys) ; Which non-control non-meta keys run this command?
                    (when (member (event-modifiers (aref key 0)) '(nil (shift)))
                      (push key non-modified-keys)))
                  (when remapped
                    (princ "It is remapped to `") (princ (symbol-name remapped)) (princ "'"))
                  (when keys
                    (princ (if remapped ", which is bound to "  "It is bound to "))
                    ;; If lots of ordinary text chars run this command, don't mention them one by one.
                    (if (< (length non-modified-keys) 10)
                        (princ (mapconcat (if (fboundp 'naked-key-description)
                                              #'naked-key-description
                                            #'key-description)
                                          keys ", "))
                      (dolist (key  non-modified-keys)  (setq keys  (delq key keys)))
                      (if keys
                          (progn (princ (mapconcat (if (fboundp 'naked-key-description)
                                                       #'naked-key-description
                                                     #'key-description)
                                                   keys ", "))
                                 (princ ", and many ordinary text characters"))
                        (princ "many ordinary text characters"))))
                  (when (or remapped  keys  non-modified-keys)  (princ ".") (terpri)))))
            (with-current-buffer (help-buffer)
              (fill-region-as-paragraph pt2 (point))
              (unless (and (eq ?\n (char-before)) ; Quicker than `looking-back', apparently.
                           (eq ?\n (char-before (1- (point)))))
                (terpri)))))
        ;; `list*' etc. do not get this property until `cl-hack-byte-compiler' runs,
        ;; which is after bytecomp is loaded.
        (when (and (symbolp function)  (eq (get function 'byte-compile) 'cl-byte-compile-compiler-macro))
          (princ "This function has a compiler macro")
          (let ((lib  (get function 'compiler-macro-file)))
            (when (stringp lib)
              (princ (format " in `%s'" lib))
              (with-current-buffer standard-output
                (save-excursion (re-search-backward "`\\([^`']+\\)'" nil t)
                                (help-xref-button 1 'help-function-cmacro function lib)))))
          (princ ".\n\n"))
        (let* ((advertised  (gethash def advertised-signature-table t))
               (arglist     (if (listp advertised) advertised (help-function-arglist def)))
               (doc         (condition-case err
                                (help-documentation function nil 'ADD-HELP-BUTTONS)
                              (error (format "No Doc! %S" err))))
               (usage       (help-split-fundoc doc function)))
          (with-current-buffer standard-output
            (unless (keymapp function) ; If definition is a keymap, skip arglist note.
              (when usage (setq doc  (cdr usage)))
              (let* ((use   (cond ((and usage  (not (listp advertised)))  (car usage))
                                  ((listp arglist)  (format "%S" (help-make-usage function arglist)))
                                  ((stringp arglist)  arglist)
                                  ;; Maybe arglist is in doc string of a symbol this one is aliased to.
                                  ((let ((fun  real-function))
                                     (while (and (symbolp fun)
                                                 (setq fun  (symbol-function fun))
                                                 (not (setq usage  (help-split-fundoc
                                                                    (help-documentation
                                                                     fun nil 'ADD-HELP-BUTTONS)
                                                                    function)))))
                                     usage)
                                   (car usage))
                                  ((or (stringp def)  (vectorp def))
                                   (format "\nMacro: %s" (format-kbd-macro def)))
                                  (t  "[Missing arglist.  Please submit a bug report.]")))
                     (high  (help-highlight-arguments use doc)))
                (let ((fill-begin  (point)))
                  (insert (car high) "\n")
                  (fill-region fill-begin (point)))
                (setq doc  (cdr high))))
            ;; If this is a derived mode, link to the parent.
            (let ((parent-mode  (and (symbolp real-function)  (get real-function 'derived-mode-parent))))
              (when parent-mode
                (with-current-buffer standard-output
                  (insert "\nParent mode: `")
                  (let ((beg  (point)))
                    (insert (format "%s" parent-mode))
                    (make-text-button beg (point) 'type 'help-function 'help-args (list parent-mode))))
                (princ "'.\n")))
            (let* ((obsolete  (and (symbolp function) ; Function might be a lambda construct.
                                   (get function 'byte-obsolete-info)))
                   (use       (car obsolete)))
              (when obsolete
                (princ "\nThis function is obsolete")
                (when (nth 2 obsolete) (insert (format " since %s" (nth 2 obsolete))))
                (insert (cond ((stringp use)  (concat ";\n" use))
                              (use  (format ";\nuse `%s' instead." use))
                              (t  "."))
                        "\n"))
              (insert "\n")
              (when (and doc  (boundp 'Info-virtual-files)) ; Emacs 23.2+
                (Info-make-manuals-xref function)) ; Link to manuals.  (With progress message.)
              (insert (or doc  "Not documented.")))))))))

(when (fboundp 'help-fns--autoloaded-p) ; Emacs 24.3+


  ;; REPLACE ORIGINAL in `help-fns.el':
  ;;
  ;; Use `naked-key-description' if available, instead of `key-description'.
  ;;
  (defun help-fns--key-bindings (function)
    (when (commandp function)
      (let ((pt2       (with-current-buffer standard-output (point)))
            (remapped  (command-remapping function)))
        (unless (memq remapped '(ignore undefined))
          (let ((keys  (where-is-internal (or remapped  function) overriding-local-map nil nil))
                non-modified-keys)
            (if (and (eq function 'self-insert-command)
                     (vectorp (car-safe keys))
                     (consp (aref (car keys) 0)))
                (princ "It is bound to many ordinary text characters.\n")
              (dolist (key  keys) ; Which non-control non-meta keys run this command?
                (when (member (event-modifiers (aref key 0))  '(nil (shift)))  (push key non-modified-keys)))
              (when remapped
                (princ "Its keys are remapped to ")
                (princ (if (symbolp remapped) (concat "`" (symbol-name remapped) "'") "an anonymous command"))
                (princ ".\n"))
              (when keys
                (princ (if remapped "Without this remapping, it would be bound to " "It is bound to "))
                ;; If lots of ordinary text characters run this command, don't mention them one by one.
                (if (< (length non-modified-keys) 10)
                    (princ (mapconcat (if (fboundp 'naked-key-description)
                                          #'naked-key-description
                                        #'key-description)
                                      keys ", "))
                  (dolist (key  non-modified-keys) (setq keys  (delq key keys)))
                  (if keys
                      (progn (princ (mapconcat (if (fboundp 'naked-key-description)
                                                   #'naked-key-description
                                                 #'key-description)
                                               keys ", "))
                             (princ ", and many ordinary text characters"))
                    (princ "many ordinary text characters"))))
              (when (or remapped  keys  non-modified-keys) (princ ".") (terpri)))))
        (with-current-buffer standard-output
          (fill-region-as-paragraph pt2 (point))
          (unless (and (eq ?\n (char-before)) ; Quicker than `looking-back', apparently.
                       (eq ?\n (char-before (1- (point)))))
            (terpri))))))


  ;; REPLACE ORIGINAL in `help-fns.el'
  ;;
  ;; 1. Add key-description buttons to command help: Use `help-documentation', not `documentation'.
  ;; 2. Arg RAW is optional, so we can use this with older Emacs versions.
  ;;
  (defun help-fns--signature (function doc real-def real-function &optional raw) ; Keep RAW optional for old Emacs.
    (if (keymapp function)
        doc            ; If definition is a keymap, skip arglist note.
      (let* ((advertised  (gethash real-def advertised-signature-table t))
             (arglist     (if (listp advertised) advertised (help-function-arglist real-def)))
             (usage       (help-split-fundoc doc function)))
        (when usage (setq doc  (cdr usage)))
        (let* ((use   (cond ((and usage  (not (listp advertised))) (car usage))
                            ((listp arglist)
                             (if (fboundp 'help--make-usage-docstring)
                                 (help--make-usage-docstring function arglist) ; Emacs 25+.
                               (format "%S" (help-make-usage function arglist))))
                            ((stringp arglist) arglist)
                            ;; Maybe the arglist is in the docstring of a symbol this one is aliased to.
                            ((let ((fun  real-function))
                               (while (and (symbolp fun)
                                           (setq fun  (symbol-function fun))
                                           (not (setq usage  (help-split-fundoc
                                                              (help-documentation fun nil 'ADD-HELP-BUTTONS)
                                                              function)))))
                               usage)
                             (car usage))
                            ((or (stringp real-def)  (vectorp real-def))
                             (format "\nMacro: %s"
                                     (if (fboundp 'help--docstring-quote)
                                         (help--docstring-quote (format-kbd-macro real-def)) ; Emacs 25+.
                                       (format-kbd-macro real-def))))
                            (t "[Missing arglist.  Please submit a bug report.]")))
               ;; Insert "`X", not "(\` X)", when documenting `X.
               (use1   (replace-regexp-in-string  "\\`(\\\\=\\\\\\\\=` \\([^\n ]*\\))\\'"  "\\\\=`\\1" use t))
               (high   (if raw
                           (cons use1 doc)
                         (help-highlight-arguments (substitute-command-keys use1) (substitute-command-keys doc)))))
          (let ((fill-begin  (point))
                (high-usage  (car high))
                (high-doc    (cdr high)))
            (insert high-usage "\n")
            (fill-region fill-begin (point))
            high-doc)))))
  )

(when (and (= emacs-major-version 24)  (= emacs-minor-version 3))
  (defun describe-function-1 (function)
    (let* ((advised        (and (symbolp function)  (featurep 'advice)  (ad-get-advice-info function)))
           ;; If the function is advised, use the symbol that has the real def, if already set up.
           (real-function  (or (and advised  (let ((origname  (cdr (assq 'origname advised))))
                                               (and (fboundp origname)  origname)))
                               function))
           ;; Get the real definition.
           (def            (if (symbolp real-function) (symbol-function real-function) function))
           (aliased        (symbolp def))
           (real-def       (if aliased
                               (let ((fn  def))
                                 (while (and  (fboundp fn)  (symbolp (symbol-function fn)))
                                   (setq fn  (symbol-function fn)))
                                 fn)
                             def))
           (file-name      (find-lisp-object-file-name function def))
           (pt1            (with-current-buffer (help-buffer) (point)))
           (beg            (if (and (or (byte-code-function-p def)  (keymapp def)
                                        (memq (car-safe def) '(macro lambda closure)))
                                    file-name
                                    (help-fns--autoloaded-p function file-name))
                               (if (commandp def) "an interactive autoloaded " "an autoloaded ")
                             (if (commandp def) "an interactive " "a "))))
      ;; Print what kind of function-like object FUNCTION is.
      (princ (cond ((or (stringp def)  (vectorp def))  "a keyboard macro")
                   ((subrp def)  (if (eq 'unevalled (cdr (subr-arity def)))
                                     (concat beg "special form")
                                   (concat beg "built-in function")))
                   ((byte-code-function-p def)  (concat beg "compiled Lisp function"))
                   (aliased (format "an alias for `%s'" real-def))
                   ((eq (car-safe def) 'lambda)  (concat beg "Lisp function"))
                   ((eq (car-safe def) 'macro)  (concat beg "Lisp macro"))
                   ((eq (car-safe def) 'closure)  (concat beg "Lisp closure"))
                   ((autoloadp def)
                    (format "%s autoloaded %s" (if (commandp def) "an interactive"  "an")
                            (if (eq (nth 4 def) 'keymap)
                                "keymap"
                              (if (nth 4 def) "Lisp macro" "Lisp function"))))
                   ((keymapp def)  (let ((is-full  nil)
                                         (elts     (cdr-safe def)))
                                     (while elts
                                       (when (char-table-p (car-safe elts))
                                         (setq is-full  t
                                               elts     ()))
                                       (setq elts  (cdr-safe elts)))
                                     (concat beg (if is-full "keymap"  "sparse keymap"))))
                   (t  "")))
      (if (and aliased  (not (fboundp real-def)))
          (princ ",\nwhich is not defined.  Please submit a bug report.")
        (with-current-buffer standard-output
          (save-excursion (save-match-data (when (re-search-backward "alias for `\\([^`']+\\)'" nil t)
                                             (help-xref-button 1 'help-function real-def)))))
        (when file-name
          (princ " in `")
          ;; We used to add `.el' to the file name, but that's wrong when the user used `load-file'.
          (princ (if (eq file-name 'C-source) "C source code" (file-name-nondirectory file-name)))
          (princ "'")
          ;; Make a hyperlink to the library.
          (with-current-buffer standard-output
            (save-excursion (re-search-backward "`\\([^`']+\\)'" nil t)
                            (help-xref-button 1 'help-function-def function file-name))))
        (princ ".")
        (with-current-buffer (help-buffer)
          (fill-region-as-paragraph (save-excursion (goto-char pt1) (forward-line 0) (point)) (point)))
        (terpri) (terpri)
        (let* ((doc-raw  (documentation function 'RAW))
               ;; If the function is autoloaded, and its docstring has key substitution constructs,
               ;; load the library.  In any case, add help buttons.
               (doc      (if (and (autoloadp real-def)
                                  doc-raw
                                  help-enable-auto-load
                                  (string-match "\\([^\\]=\\|[^=]\\|\\`\\)\\\\[[{<]" doc-raw)
                                  (load (cadr real-def) t))
                             (help-substitute-command-keys doc-raw 'ADD-HELP-BUTTONS)
                           (condition-case err
                               (help-documentation function nil 'ADD-HELP-BUTTONS)
                             (error (format "No Doc! %S" err))))))
          (help-fns--key-bindings function)
          (with-current-buffer standard-output
            (setq doc  (help-fns--signature function doc real-def real-function))
            (help-fns--compiler-macro function)
            (help-fns--parent-mode function)
            (help-fns--obsolete function)
            (insert "\n")
            (when (and doc  (boundp 'Info-virtual-files)) ; Emacs 23.2+
              (Info-make-manuals-xref function)) ; Link to manuals.  (With progress message.)
            (insert (or doc  "Not documented."))))))))

(when (or (> emacs-major-version 24)  (and (= emacs-major-version 24)  (> emacs-minor-version 3)))
  (defun describe-function-1 (function)
    (let* ((advised        (and (symbolp function)
                                (featurep 'nadvice)
                                (advice--p (advice--symbol-function function))))
           ;; If the function is advised, use the symbol that has the real definition, if already set up.
           (real-function  (or (and advised  (advice--cd*r (advice--symbol-function function)))
                               function))
           ;; Get the real definition.
           (def            (if (symbolp real-function) (symbol-function real-function) real-function))
           (aliased        (or (symbolp def)
                               (and advised  (symbolp real-function)))) ; Advised & aliased function.
           (real-def       (cond (aliased     (let ((f  real-function))
                                                (while (and (fboundp f)  (symbolp (symbol-function f)))
                                                  (setq f  (symbol-function f)))
                                                f))
                                 ((subrp def) (intern (subr-name def)))
                                 (t           def)))
           (sig-key        (if (subrp def) (indirect-function real-def) real-def))
           (file-name      (find-lisp-object-file-name function def))
           (pt1            (with-current-buffer (help-buffer) (point)))
           (beg            (if (and (or (byte-code-function-p def)  (keymapp def)
                                        (memq (car-safe def) '(macro lambda closure)))
                                    (stringp file-name)
                                    (help-fns--autoloaded-p function file-name))
                               (if (commandp def) "an interactive autoloaded " "an autoloaded ")
                             (if (commandp def) "an interactive " "a "))))
      ;; Print what kind of function-like object FUNCTION is.
      (princ (cond ((or (stringp def)  (vectorp def))  "a keyboard macro")
                   ((subrp def)  (if (eq 'unevalled (cdr (subr-arity def)))
                                     (concat beg "special form")
                                   (concat beg "built-in function")))
                   ;; Aliases are Lisp functions, so we need to check aliases before functions.
                   ;; Do NOT use curly quotes, so do not need to wrap format string in `substitute-command-keys'.
                   (aliased (format "an alias for `%s'" real-def))
                   ((autoloadp def) (format "%s autoloaded %s"
                                            (if (commandp def) "an interactive" "an")
                                            (if (eq (nth 4 def) 'keymap)
                                                "keymap"
                                              (if (nth 4 def) "Lisp macro" "Lisp function"))))
                   ((or (eq (car-safe def) 'macro)
                        ;; For advised macros, DEF is a lambda expression or is `byte-code-function-p',
                        ;; so check macros before functions.
                        (macrop function))
                    (concat beg "Lisp macro"))
                   ((byte-code-function-p def) (concat beg "compiled Lisp function"))
                   ((eq (car-safe def) 'lambda) (concat beg "Lisp function"))
                   ((eq (car-safe def) 'closure)  (concat beg "Lisp closure"))
                   ((keymapp def)  (let ((is-full  nil)
                                         (elts     (cdr-safe def)))
                                     (while elts
                                       (when (char-table-p (car-safe elts))
                                         (setq is-full  t
                                               elts     ()))
                                       (setq elts  (cdr-safe elts)))
                                     (concat beg (if is-full "keymap"  "sparse keymap"))))
                   (t  "")))
      (if (and aliased  (not (fboundp real-def)))
          (princ ",\nwhich is not defined.  Please submit a bug report.")
        (with-current-buffer standard-output
          (save-excursion (save-match-data (when (re-search-backward "alias for `\\([^`']+\\)'" nil t)
                                             (help-xref-button 1 'help-function real-def)))))
        (when file-name
          (princ " in `")
          ;; We used to add `.el' to the file name, but that's wrong when the user used `load-file'.
          (princ (if (eq file-name 'C-source)
                     "C source code"
                   (if (fboundp 'help-fns-short-filename)
                       (help-fns-short-filename file-name) ; Emacs 25+
                     (file-name-nondirectory file-name))))
          (princ "'")
          ;; Make a hyperlink to the library.
          (with-current-buffer standard-output
            (save-excursion (re-search-backward "`\\([^`']+\\)'" nil t)
                            (help-xref-button 1 'help-function-def function file-name))))
        (princ ".")
        (with-current-buffer (help-buffer)
          (fill-region-as-paragraph (save-excursion (goto-char pt1) (forward-line 0) (point)) (point)))
        (terpri) (terpri)
        (let* ((doc-raw  (documentation function 'RAW))
               ;; If the function is autoloaded and its docstring has key substitution constructs, then
               ;; load the library.  In any case, add help buttons.
               (doc      (if (and (autoloadp real-def)
                                  doc-raw
                                  help-enable-auto-load
                                  (string-match "\\([^\\]=\\|[^=]\\|\\`\\)\\\\[[{<]" doc-raw)
                                  (autoload-do-load real-def))
                             (help-substitute-command-keys doc-raw 'ADD-HELP-BUTTONS)
                           (condition-case err
                               (help-documentation function nil 'ADD-HELP-BUTTONS)
                             (error (format "No Doc! %S" err))))))
          (help-fns--key-bindings function)
          (with-current-buffer standard-output
            (setq doc  (if (> emacs-major-version 24)
                           (help-fns--signature function doc-raw sig-key real-function nil)
                         (help-fns--signature function doc real-def real-function)))
            (run-hook-with-args 'help-fns-describe-function-functions function)
            (insert "\n")
            (when (and doc  (boundp 'Info-virtual-files)) ; Emacs 23.2+
              (Info-make-manuals-xref function)) ; Link to manuals.  (With progress message.)
            (insert (or doc  "Not documented."))))))))

;;;###autoload
(defun describe-command (function)      ; Bound to `C-h c'
  "Describe an Emacs command (interactive function).
Equivalent to using a prefix arg with `describe-function'.

If you use Icicles then in Icicle mode keys bound to the commands are
shown next to them in `*Completions*.  You can toggle this keys
display on/off using `C-x C-a'."
  (interactive
   (let ((fn                            (or (and (fboundp 'symbol-nearest-point)  (symbol-nearest-point))
                                            (function-called-at-point)))
         (enable-recursive-minibuffers  t)
         (completion-annotate-function  (and (boundp 'icicle-mode)  icicle-mode
                                             (lambda (cand)
                                               (with-current-buffer icicle-pre-minibuffer-buffer
                                                 (and (setq cand  (intern-soft cand))  (symbolp cand)
                                                      (let ((key  (where-is-internal cand nil t)))
                                                        (and key
                                                             (format "  %s" (icicle-key-description key)))))))))
         val)
     (setq val  (completing-read
                 (format "Describe command%s: " (if (commandp fn) (format " (default %s)" fn) ""))
                 obarray 'commandp t nil nil (and fn  (commandp fn)  (symbol-name fn))))
     (list (if (equal val "") fn (intern val)))))
  (describe-function function t))


;; REPLACE ORIGINAL in `help.el':
;;
;; 1. With a prefix argument, candidates are user variables (options) only.
;; 2. Preferred default candidate is `symbol-nearest-point'.
;; 3. Remove initial `*' from doc string (indicates it is a user variable).
;; 4. PREDICATE to `completing-read' uses original buffer (not minibuffer), when testing `boundp'.  (BUG #21252)
;; 5. Use `substitute-command-keys' on doc string.
;; 6. Preserve text properties.
;; 7. No message if not called interactively.
;;
(when (< emacs-major-version 23)
  (defun describe-variable (variable &optional buffer optionp)
    "Display the full documentation of VARIABLE (a symbol).
VARIABLE names an Emacs Lisp variable, possibly a user option.
With a prefix argument, candidates are user variables (options) only.
Default candidate is the `symbol-nearest-point'.
Return the documentation, as a string.
If VARIABLE has a buffer-local value in BUFFER (default to the current buffer),
it is displayed along with the global value."
    (interactive
     (let ((symb                          (or (and (fboundp 'symbol-nearest-point)  (symbol-nearest-point))
                                              (variable-at-point)))
           (enable-recursive-minibuffers  t)
           (completion-annotate-function  (lambda (var) (and (custom-variable-p (intern-soft var))  "  (option)")))
           (curbuf                        (current-buffer))
           val)
       (when (numberp symb) (setq symb  nil)) ; `variable-at-point' returns 0 when there is no var.
       (setq val  (completing-read
                   (format "Describe variable%s: "
                           (if (and symb  (boundp symb)) (format " (default %s)" symb) ""))
                   obarray (if current-prefix-arg
                               `(lambda (vv) (with-current-buffer ',curbuf (user-variable-p vv)))
                             `(lambda (vv) (with-current-buffer ',curbuf
                                        (or (boundp vv)  (get vv 'variable-documentation)))))
                   t nil nil (and (symbolp symb)  (boundp symb)  (symbol-name symb))))
       (list (if (equal val "") symb (intern val))
             nil
             current-prefix-arg)))
    (unless (buffer-live-p buffer) (setq buffer  (current-buffer)))
    (if (not (symbolp variable))
        (when (interactive-p) (message "You did not specify a variable"))
      (unless (or (not optionp)  (user-variable-p variable))
        (error "Not a defined Emacs user option: `%s'" variable))
      ;;$$ (unless (boundp variable) (error "Not a defined Emacs variable: `%s'" variable))
      (save-excursion
        (let* ((valvoid  (not (with-current-buffer buffer (boundp variable))))
               ;; Extract the value before setting up the output buffer,
               ;; in case `buffer' *is* the output buffer.
               (val      (and (not valvoid)  (buffer-local-value variable buffer)))
               val-start-pos)
          (help-setup-xref (list #'describe-variable variable buffer) (interactive-p))
          (with-output-to-temp-buffer (help-buffer)
            (with-current-buffer buffer
              (prin1 variable)
              ;; Make a hyperlink to the library if appropriate.  (Don't change the format of the
              ;; buffer's initial line in case anything expects the current format.)
              (let ((file-name  (symbol-file variable 'defvar)))
                (setq file-name  (describe-simplify-lib-file-name file-name))
                (when (equal file-name "loaddefs.el")
                  ;; Find the real def site of the preloaded variable.
                  (let ((location  (condition-case nil
                                       (find-variable-noselect variable file-name)
                                     (error nil))))
                    (when location
                      (with-current-buffer (car location)
                        (when (cdr location) (goto-char (cdr location)))
                        (when (re-search-backward "^;;; Generated autoloads from \\(.*\\)" nil t)
                          (setq file-name  (match-string 1)))))))
                (when (and (null file-name)  (integerp (get variable 'variable-documentation)))
                  ;; It's a var not defined in Elisp but in C.
                  (setq file-name  (if (get-buffer " *DOC*")
                                       (help-C-file-name variable 'var)
                                     'C-source)))
                (if file-name
                    (progn (princ " is a variable defined in `")
                           (princ (if (eq file-name 'C-source) "C source code" file-name))
                           (princ "'.\n")
                           (with-current-buffer standard-output
                             (save-excursion
                               (re-search-backward "`\\([^`']+\\)'" nil t)
                               (help-xref-button 1 'help-variable-def variable file-name)))
                           (if valvoid (princ "It is void as a variable.") (princ "Its ")))
                  (if valvoid (princ " is void as a variable.") (princ "'s "))))
              (unless valvoid
                (with-current-buffer standard-output
                  (setq val-start-pos  (point))
                  (princ "value is ") (terpri)
                  (let ((from  (point)))
                    (pp val)
                    ;; Hyperlinks in variable's value are quite frequently inappropriate
                    ;; e.g `C-h v <RET> features <RET>'
                    ;; (help-xref-on-pp from (point))
                    (when (< (point) (+ from 20)) (delete-region (1- from) from)))))
              (terpri)
              (when (local-variable-p variable)
                (princ (format "%socal in buffer %s; "
                               (if (get variable 'permanent-local) "Permanently l" "L")
                               (buffer-name)))
                (if (not (default-boundp variable))
                    (princ "globally void")
                  (let ((val  (default-value variable)))
                    (with-current-buffer standard-output
                      (princ "global value is ") (terpri)
                      ;; Fixme: `pp' can take an age if you happen to ask for a very large expression.
                      ;; We should probably print it raw once and check it's a sensible size before
                      ;; prettyprinting.  -- fx
                      (let ((from  (point)))
                        (pp val)
                        ;; See previous comment for this function.
                        ;; (help-xref-on-pp from (point))
                        (when (< (point) (+ from 20)) (delete-region (1- from) from)))))))
              ;; Add a note for variables that have been `make-var-buffer-local'.
              (when (and (local-variable-if-set-p variable)
                         (or (not (local-variable-p variable))
                             (with-temp-buffer (local-variable-if-set-p variable))))
                (princ "\nAutomatically becomes buffer-local when set in any fashion.\n"))
              (terpri)
              (with-current-buffer standard-output ; If the value is large, move it to the end.
                (when (> (count-lines (point-min) (point-max)) 10)
                  ;; Note that setting the syntax table like below makes `forward-sexp' move over a
                  ;; `'s' at the end of a symbol.
                  (set-syntax-table emacs-lisp-mode-syntax-table)
                  (goto-char val-start-pos)
                  ;; The line below previously read as
                  ;; (delete-region (point) (progn (end-of-line) (point)))
                  ;; which suppressed display of the buffer local value for large values.
                  (when (looking-at "value is") (replace-match ""))
                  (save-excursion (insert "\n\nValue:")
                                  (set (make-local-variable 'help-button-cache) (point-marker)))
                  (insert "value is shown ")
                  (insert-button "below" 'action help-button-cache 'follow-link t
                                 'help-echo "mouse-2, RET: show value")
                  (insert ".\n")))
              ;; Mention if it's an alias
              (let* ((alias     (condition-case nil (indirect-variable variable) (error variable)))
                     (obsolete  (get variable 'byte-obsolete-variable))
                     (safe-var  (get variable 'safe-local-variable))
                     (doc       (or (documentation-property variable 'variable-documentation)
                                    (documentation-property alias 'variable-documentation))))
                (when (and (> (length doc) 1)  (eq ?* (elt doc 0)))
                  (setq doc  (substring doc 1))) ; Remove any user-variable prefix `*'.
                (unless (eq alias variable)
                  (princ (format "\nThis variable is an alias for `%s'.\n" alias)))
                (when (or obsolete  safe-var) (terpri))
                (when obsolete
                  (princ "This variable is obsolete")
                  (when (cdr obsolete) (princ (format " since %s" (cdr obsolete))))
                  (princ ";") (terpri)
                  (princ (if (stringp (car obsolete))
                             (car obsolete)
                           (format "use `%s' instead." (car obsolete))))
                  (terpri))
                (when safe-var
                  (princ "This variable is safe as a file local variable ")
                  (princ "if its value\nsatisfies the predicate ")
                  (princ (if (byte-code-function-p safe-var)
                             "which is byte-compiled expression.\n"
                           (format "`%s'.\n" safe-var))))
                (princ "\nDocumentation:\n")
                ;; Use `insert', not `princ', to keep text properties.
                ;; Was: (princ (or doc  "Not documented as a variable.")))
                (with-current-buffer standard-output
                  (insert (or (substitute-command-keys doc)  "Not documented as a variable."))))
              ;; Make a link to customize if this variable can be customized.
              (when (custom-variable-p variable)
                (let ((customize-label  "customize"))
                  (terpri) (terpri) (princ (concat "You can " customize-label " this variable."))
                  (with-current-buffer standard-output
                    (save-excursion (re-search-backward (concat "\\(" customize-label "\\)") nil t)
                                    (help-xref-button 1 'help-customize-variable variable)))))
              (print-help-return-message)
              (with-current-buffer standard-output (buffer-string))))))))) ; Return the text displayed.

;;; This macro is no different from what is in vanilla Emacs 23+.
;;; Add it here so this file can be byte-compiled with Emacs 22 and used with Emacs 23+.
(defmacro with-selected-frame (frame &rest body)
  "Execute the forms in BODY with FRAME as the selected frame.
Save the selected frame, select FRAME, execute BODY, then restore the
originally selected frame.  Return the value of the last form in BODY.

This macro changes the order of neither the recently selected windows
nor the buffers in the buffer list.  See also `with-temp-buffer'."
  (declare (indent 1) (debug t))
  (let ((old-frame   (make-symbol "old-frame"))
        (old-buffer  (make-symbol "old-buffer")))
    `(let ((,old-frame   (selected-frame))
           (,old-buffer  (current-buffer)))
       (unwind-protect
            (progn (if (> emacs-major-version 22) (select-frame ,frame 'NORECORD) (select-frame ,frame))
                   ,@body)
         (when (frame-live-p ,old-frame)
           (if (> emacs-major-version 22) (select-frame ,old-frame 'NORECORD) (select-frame ,old-frame)))
         (when (buffer-live-p ,old-buffer) (set-buffer ,old-buffer))))))


;; REPLACE ORIGINAL in `help.el':
;;
;; 1. With a prefix argument, candidates are user variables (options) only.
;; 2. Preferred default candidate is `symbol-nearest-point'.
;; 3. PREDICATE to `completing-read' uses original buffer (not minibuffer), when testing `boundp'.  (BUG #21252)
;; 4. Preserve text properties.
;; 5. Remove initial `*' from doc string (indicates it is a user variable).
;; 6. Call `Info-make-manuals-xref' to create a cross-ref link to manuals (Emacs 23.3).
;; 7. Add key-description buttons to command help.  Use `insert', not `princ'.
;; 8. No no-function message if not called interactively.
;;
(when (= emacs-major-version 23)
  (defun describe-variable (variable &optional buffer frame optionp)
    "Display the full documentation of VARIABLE (a symbol).
VARIABLE names an Emacs Lisp variable, possibly a user option.
With a prefix argument, candidates are user variables (options) only.
Default candidate is the `symbol-nearest-point'.
Return the documentation, as a string.
If VARIABLE has a buffer-local value in BUFFER or FRAME
\(default to the current buffer and current frame),
it is displayed along with the global value."
    (interactive
     (let ((symb                          (or (and (fboundp 'symbol-nearest-point)  (symbol-nearest-point))
                                              (variable-at-point)))
           (enable-recursive-minibuffers  t)
           (completion-annotate-function  (lambda (var) (and (custom-variable-p (intern-soft var))  "  (option)")))
           (curbuf                        (current-buffer))
           val)
       (when (numberp symb) (setq symb  nil)) ; `variable-at-point' returns 0 when there is no var.
       (setq val  (completing-read
                   (format "Describe variable%s: "
                           (if (and symb  (boundp symb)) (format " (default %s)" symb) ""))
                   obarray
                   (if current-prefix-arg
                       `(lambda (vv) (with-current-buffer ',curbuf (user-variable-p vv)))
                     `(lambda (vv) (with-current-buffer ',curbuf
                                (or (get vv 'variable-documentation)  (and (boundp vv)  (not (keywordp vv)))))))
                   t nil nil (and (symbolp symb)  (boundp symb)  (symbol-name symb))))
       (list (if (equal val "") symb (intern val))
             nil
             nil
             current-prefix-arg)))
    (let (file-name)
      (unless (buffer-live-p buffer) (setq buffer (current-buffer)))
      (unless (frame-live-p frame) (setq frame (selected-frame)))
      (if (not (symbolp variable))
          (when (if (or (> emacs-major-version 23) ; Emacs 23.1 `called-interactively' accepts no arg.
                        (and (= emacs-major-version 23)  (> emacs-minor-version 1)))
                    (called-interactively-p 'interactive)
                  (interactive-p))
            (message "You did not specify a variable"))
        (unless (or (not optionp)  (user-variable-p variable))
          (error "Not a defined Emacs user option: `%s'" variable))
        ;;$$ (unless (boundp variable) (error "Not a defined Emacs variable: `%s'" variable))
        (save-excursion
          (let ((valvoid  (not (with-current-buffer buffer (boundp variable))))
                val val-start-pos locus)
            ;; Extract the value before setting up the output buffer, in case BUFFER *is* the
            ;; output buffer.
            (unless valvoid
              (with-selected-frame frame
                (with-current-buffer buffer
                  (setq val    (symbol-value variable)
                        locus  (variable-binding-locus variable)))))
            (help-setup-xref (list #'describe-variable variable buffer)
                             (if (or (> emacs-major-version 23)
                                     (and (= emacs-major-version 23)  (> emacs-minor-version 1)))
                                 (called-interactively-p 'interactive)
                               (interactive-p)))
            (with-help-window (help-buffer)
              (with-current-buffer buffer
                (prin1 variable)
                (setq file-name  (find-lisp-object-file-name variable 'defvar))
                (if file-name
                    (progn (princ " is a variable defined in `")
                           (princ (if (eq file-name 'C-source)
                                      "C source code"
                                    (file-name-nondirectory file-name)))
                           (princ "'.\n")
                           (with-current-buffer standard-output
                             (save-excursion
                               (re-search-backward "`\\([^`']+\\)'" nil t)
                               (help-xref-button 1 'help-variable-def variable file-name)))
                           (if valvoid (princ "It is void as a variable.") (princ "Its ")))
                  (if valvoid (princ " is void as a variable.") (princ "'s "))))
              (unless valvoid
                (with-current-buffer standard-output
                  (setq val-start-pos  (point))
                  (princ "value is ")
                  (let ((from  (point)))
                    (terpri)
                    (pp val)
                    (if (< (point) (+ 68 (line-beginning-position 0)))
                        (delete-region from (1+ from))
                      (delete-region (1- from) from)))))
              (terpri)
              (when locus
                (if (bufferp locus)
                    (princ (format "%socal in buffer %s; "
                                   (if (get variable 'permanent-local) "Permanently l" "L")
                                   (buffer-name)))
                  (princ (format "It is a frame-local variable; ")))
                (if (not (default-boundp variable))
                    (princ "globally void")
                  (let ((val  (default-value variable)))
                    (with-current-buffer standard-output
                      (princ "global value is ") (terpri)
                      ;; Fixme: `pp' can take an age if you happen to ask for a very large expression.
                      ;; We should probably print it raw once and check it's a sensible size before
                      ;; prettyprinting.  -- fx
                      (let ((from  (point)))
                        (pp val)
                        ;; See previous comment for this function.
                        ;; (help-xref-on-pp from (point))
                        (when (< (point) (+ from 20))  (delete-region (1- from) from))))))
                (terpri))
              (with-current-buffer standard-output ; If the value is large, move it to the end.
                (when (> (count-lines (point-min) (point-max))  10)
                  ;; Note that setting the syntax table like below makes `forward-sexp' move over a
                  ;; `'s' at the end of a symbol.
                  (set-syntax-table emacs-lisp-mode-syntax-table)
                  (goto-char val-start-pos)
                  ;; The line below previously read as
                  ;; (delete-region (point) (progn (end-of-line) (point)))
                  ;; which suppressed display of the buffer local value for large values.
                  (when (looking-at "value is") (replace-match ""))
                  (save-excursion (insert "\n\nValue:") (terpri)
                                  (set (make-local-variable 'help-button-cache) (point-marker)))
                  (insert "value is shown ")
                  (insert-button "below" 'action help-button-cache 'follow-link t
                                 'help-echo "mouse-2, RET: show value")
                  (insert ".\n")))
              (terpri)
              (let* ((alias     (condition-case nil (indirect-variable variable) (error variable)))
                     (obsolete  (get variable 'byte-obsolete-variable))
                     (use       (car obsolete))
                     (safe-var  (get variable 'safe-local-variable))
                     (vardoc    (help-documentation-property variable 'variable-documentation
                                                             nil 'ADD-HELP-BUTTONS))
                     (vardoc    (and (not (equal "" vardoc))  vardoc))
                     (doc       (or vardoc  (help-documentation-property alias 'variable-documentation
                                                                         nil 'ADD-HELP-BUTTONS)))
                     (extra-line  nil))
                (when (and (> (length doc) 1)  (eq ?* (elt doc 0)))
                  (setq doc  (substring doc 1))) ; Remove any user-variable prefix `*'.
                ;; Add a note for variables that have been `make-var-buffer-local'.
                (when (and (local-variable-if-set-p variable)
                           (or (not (local-variable-p variable))
                               (with-temp-buffer (local-variable-if-set-p variable))))
                  (setq extra-line  t)
                  (princ "  Automatically becomes buffer-local when set in any fashion.\n"))
                ;; Mention if it's an alias
                (unless (eq alias variable)
                  (setq extra-line  t)
                  (princ (format "  This variable is an alias for `%s'.\n" alias)))
                (when obsolete
                  (setq extra-line  t)
                  (princ "  This variable is obsolete")
                  (when (cdr obsolete) (princ (format " since %s" (cdr obsolete))))
                  (princ (cond ((stringp use)  (concat ";\n  " use))
                               (use  (format ";\n  use `%s' instead." (car obsolete)))
                               (t  ".")))
                  (terpri))
                (when (member (cons variable val) file-local-variables-alist)
                  (setq extra-line  t)
                  (if (member (cons variable val) dir-local-variables-alist)
                      (let ((file  (and (buffer-file-name)
                                        (not (file-remote-p (buffer-file-name)))
                                        (dir-locals-find-file (buffer-file-name)))))
                        (princ "  This variable is a directory local variable")
                        (when file
                          (princ (concat "\n  from the file \"" (if (consp file) (car file) file)
                                         "\"")))
                        (princ ".\n"))
                    (princ "  This variable is a file local variable.\n")))
                (when (memq variable ignored-local-variables)
                  (setq extra-line  t)
                  (princ "  This variable is ignored when used as a file local \
variable.\n"))
                ;; Can be both risky and safe, eg `auto-fill-function'.
                (when (risky-local-variable-p variable)
                  (setq extra-line  t)
                  (princ "  This variable is potentially risky when used as a \
file local variable.\n")
                  (when (assq variable safe-local-variable-values)
                    (princ "  However, you have added it to \
`safe-local-variable-values'.\n")))
                (when safe-var
                  (setq extra-line  t)
                  (princ "  This variable is safe as a file local variable ")
                  (princ "if its value\n  satisfies the predicate ")
                  (princ (if (byte-code-function-p safe-var)
                             "which is byte-compiled expression.\n"
                           (format "`%s'.\n" safe-var))))
                (when extra-line (terpri))
                (princ "Documentation:\n")
                (with-current-buffer standard-output
                  (insert (or doc  "Not documented as a variable."))))
              ;; Make a link to customize if this variable can be customized.
              (when (custom-variable-p variable)
                (let ((customize-label  "customize"))
                  (terpri) (terpri)
                  (princ (concat "You can " customize-label " this variable."))
                  (with-current-buffer standard-output
                    (save-excursion (re-search-backward (concat "\\(" customize-label "\\)") nil t)
                                    (help-xref-button 1 'help-customize-variable variable))))
                ;; Note variable's version or package version
                (let ((output  (describe-variable-custom-version-info variable)))
                  (when output (terpri) (terpri) (princ output))))
              (when (boundp 'Info-virtual-files) ; Emacs 23.2+
                (unless valvoid
                  (with-current-buffer standard-output ; Link to manuals.
                    (Info-make-manuals-xref variable nil nil
                                            (not (if (or (> emacs-major-version 23)
                                                         (and (= emacs-major-version 23)
                                                              (> emacs-minor-version 1)))
                                                     (called-interactively-p 'interactive)
                                                   (interactive-p)))))))
              (with-current-buffer standard-output (buffer-string))))))))) ; Return the text displayed.


;; REPLACE ORIGINAL in `help-fns.el':
;;
;; 1. With a prefix argument, candidates are user variables (options) only.
;; 2. Preferred default candidate is `symbol-nearest-point'.
;; 3. PREDICATE to `completing-read' uses original buffer (not minibuffer), when testing `boundp'.  (BUG #21252)
;; 4. Preserve text properties.
;; 5. Remove initial `*' from doc string (indicates it is a user variable).
;; 6. Call `Info-make-manuals-xref' to create a cross-ref link to manuals (Emacs 23.3).
;; 7. Add key-description buttons to command help.  Use `insert', not `princ'.
;; 8. No no-function message if not called interactively.
;;
(when (> emacs-major-version 23)

  (defface describe-variable-value '((((background dark)) (:foreground "#58DFFA4FFFFF")) ; a dark cyan
                                     (t (:foreground "Firebrick")))
           "*Face used to highlight the variable value, for `describe-variable'."
           :group 'help :group 'faces)

  (defun describe-variable (variable &optional buffer frame optionp)
    "Display the full documentation of VARIABLE (a symbol).
With a prefix argument, candidates are user variables (options) only.
Default candidate is the `symbol-nearest-point'.
Return the documentation, as a string.

VARIABLE names an Emacs Lisp variable, possibly a user option.
If VARIABLE has a buffer-local value in BUFFER or FRAME (default to
the current buffer and current frame) then it is displayed, along with
the global value."
    (interactive
     (let ((symb                          (or (and (fboundp 'symbol-nearest-point)  (symbol-nearest-point))
                                              (variable-at-point)))
           (enable-recursive-minibuffers  t)
           (completion-annotate-function  (lambda (vv) (and (custom-variable-p (intern-soft vv))  "  (option)")))
           (curbuf                        (current-buffer))
           val)
       (when (numberp symb) (setq symb  nil)) ; `variable-at-point' returns 0 when there is no var.
       (setq val (completing-read
                  (format "Describe variable%s: "
                          (if (and symb  (boundp symb)) (format " (default %s)" symb) ""))
                  obarray
                  (if current-prefix-arg
                      `(lambda (vv) (with-current-buffer ',curbuf (user-variable-p vv)))
                    `(lambda (vv) (with-current-buffer ',curbuf
                               (or (get vv 'variable-documentation)  (and (boundp vv)  (not (keywordp vv)))))))
                  t nil nil (and (symbolp symb)  (boundp symb)  (symbol-name symb))))
       (list (if (equal val "") symb (intern val))
             nil
             nil
             current-prefix-arg)))
    (let (file-name)
      (unless (buffer-live-p buffer) (setq buffer (current-buffer)))
      (unless (frame-live-p frame) (setq frame (selected-frame)))
      (if (not (symbolp variable))
          (when (called-interactively-p 'interactive) (message "You did not specify a variable"))
        (unless (or (not optionp)  (user-variable-p variable))
          (error "Not a defined Emacs user option: `%s'" variable))
        ;;$$ (unless (boundp variable) (error "Not a defined Emacs variable: `%s'" variable))
        (save-excursion
          (let ((valvoid           (not (with-current-buffer buffer (boundp variable))))
                (permanent-local   (get variable 'permanent-local))
                val val-start-pos  locus)
            ;; Extract the value before setting up the output buffer, in case BUFFER *is* the output buffer.
            (unless valvoid
              (with-selected-frame frame
                (with-current-buffer buffer
                  (setq val    (symbol-value variable)
                        locus  (variable-binding-locus variable)))))
            (help-setup-xref (list #'describe-variable variable buffer)
                             (called-interactively-p 'interactive))
            (with-help-window (help-buffer)
              (with-current-buffer buffer
                (prin1 variable)
                (setq file-name  (find-lisp-object-file-name variable 'defvar))
                (if file-name
                    (progn (princ " is a variable defined in `")
                           (princ (if (eq file-name 'C-source)
                                      "C source code"
                                    (file-name-nondirectory file-name)))
                           (princ "'.\n")
                           (with-current-buffer standard-output
                             (save-excursion (re-search-backward "`\\([^`']+\\)'" nil t)
                                             (help-xref-button 1 'help-variable-def variable file-name)))
                           (if valvoid (princ "It is void as a variable.") (princ "Its ")))
                  (if valvoid (princ " is void as a variable.") (princ "'s "))))
              (unless valvoid
                (with-current-buffer standard-output
                  (setq val-start-pos  (point))
                  (princ "value is ")
                  (let ((from       (point))
                        (line-beg   (line-beginning-position))
                        (print-rep  (let ((print-quoted  t)) (prin1-to-string val))))
                    (if (< (+ (length print-rep) (point) (- line-beg)) 68)
                        (progn (insert print-rep)
                               (put-text-property from (point) 'face 'describe-variable-value)
                               (terpri))
                      (terpri)
                      (unless (or (numberp val)  (symbolp val)  (characterp val)
                                  (and (stringp val)  (string-match-p "[\n]" val)))
                        (terpri))
                      (let ((opoint  (point)))
                        (pp val)
                        (save-excursion (fill-region-as-paragraph opoint (point) nil t t)))
                      (when (stringp val) (terpri))
                      (put-text-property from (point) 'face 'describe-variable-value)
                      (if (< (point) (+ 68 (line-beginning-position 0)))
                          (delete-region from (1+ from))
                        (delete-region (1- from) from)))
                    (let* ((sv       (get variable 'standard-value))
                           (origval  (and (consp sv)
                                          (condition-case nil (eval (car sv)) (error :help-eval-error)))))
                      (when (and (consp sv)
                                 (not (equal origval val))
                                 (not (equal origval :help-eval-error)))
                        (princ "\nOriginal value was \n")
                        (setq from  (point))
                        (unless (or (numberp origval)  (symbolp origval)  (characterp origval)
                                    (and (stringp origval)  (string-match-p "[\n]" origval)))
                          (terpri))
                        (let ((opoint  (point)))
                          (pp origval)
                          (save-excursion (fill-region-as-paragraph opoint (point) nil t t)))
                        (put-text-property from (point) 'face 'describe-variable-value)
                        (when (< (point) (+ from 20)) (delete-region (1- from) from) (terpri)))))))
              (terpri)
              (when locus
                (cond ((bufferp locus)
                       (terpri)
                       (princ (format "%socal in buffer `%s'; "
                                      (if (get variable 'permanent-local)  "Permanently l"  "L")
                                      (buffer-name buffer))))
                      ((framep locus)
                       (princ (format "It is a frame-local variable; ")))
                      ((terminal-live-p locus)
                       (princ (format "It is a terminal-local variable; ")))
                      (t (princ (format "It is local to %S" locus))))
                (if (not (default-boundp variable))
                    (progn (princ "globally void") (terpri))
                  (let ((global-val  (default-value variable)))
                    (with-current-buffer standard-output
                      (princ "global value is")
                      (if (eq val global-val)
                          (progn (princ " the same.") (terpri))
                        (princ ":") (terpri) (terpri)
                        ;; Fixme: `pp' can take an age if you happen to ask for a very large expression.
                        ;; We should probably print it raw once and check whether it is a sensible size,
                        ;; before prettyprinting.  -- fx
                        (let ((opoint  (point)))
                          (pp global-val)
                          (save-excursion (fill-region-as-paragraph opoint (point) nil t t))
                          (put-text-property opoint (point) 'face 'describe-variable-value)
                          ;; See previous comment for this function.  (help-xref-on-pp opoint (point))
                          (when (< (point) (+ opoint 20)) (delete-region (1- opoint) opoint))))))))
              (with-current-buffer standard-output ; If the value is large, move it to the end.
                (when (> (count-lines (point-min) (point-max)) 10)
                  ;; Note that setting the syntax table like below makes `forward-sexp' move over a
                  ;; `'s' at the end of a symbol.
                  (set-syntax-table emacs-lisp-mode-syntax-table)
                  (goto-char val-start-pos)
                  ;; The line below previously read as (delete-region (point) (progn (end-of-line) (point))),
                  ;; which suppressed display of the buffer local value for large values.
                  (when (looking-at "value is") (replace-match ""))
                  (save-excursion (let ((nl-before  (cond ((and (eq ?\n (char-before)) ; vs `looking-back'.
                                                                (eq ?\n (char-before (1- (point))))) "")
                                                          ((eq ?\n (char-before))                    "\n")
                                                          (t                                         "\n\n")))
                                        (nl-after   (cond ((looking-at   "[\n]")     "")
                                                          (t                         "\n"))))
                                    (insert (format "%sValue:%s" nl-before nl-after)))
                                  (set (make-local-variable 'help-button-cache) (point-marker)))
                  (insert "value is shown ")
                  (insert-button "below" 'action help-button-cache 'follow-link t
                                 'help-echo "mouse-2, RET: show value")
                  (insert ".\n")))
              (terpri)
              (let* ((alias     (condition-case nil (indirect-variable variable) (error variable)))
                     (obsolete  (get variable 'byte-obsolete-variable))
                     (use       (car obsolete))
                     (safe-var  (get variable 'safe-local-variable))
                     (vardoc    (help-documentation-property variable 'variable-documentation
                                                             nil 'ADD-HELP-BUTTONS))
                     (vardoc    (and (not (equal "" vardoc))  vardoc))
                     (doc       (or vardoc  (help-documentation-property alias 'variable-documentation
                                                                         nil 'ADD-HELP-BUTTONS)))
                     (extra-line  nil))
                (when (and (> (length doc) 1)  (eq ?* (elt doc 0)))
                  (setq doc  (substring doc 1))) ; Remove any user-variable prefix `*'.
                (cond ((and (local-variable-if-set-p variable) ; Mention if it's a local variable.
                            (or (not (local-variable-p variable))
                                (with-temp-buffer (local-variable-if-set-p variable))))
                       (setq extra-line  t)
                       (princ "  Automatically becomes ")
                       (when permanent-local (princ "permanently "))
                       (princ "buffer-local when set.\n"))
                      ((not permanent-local))
                      ((bufferp locus)  (princ "  This variable's buffer-local value is permanent.\n"))
                      (t (princ "  This variable's value is permanent when it is given a local binding.\n")))
                (unless (eq alias variable) ; Mention if it's an alias.
                  (setq extra-line  t)
                  (princ (format "  This variable is an alias for `%s'.\n" alias)))
                (when obsolete
                  (setq extra-line t)
                  (princ "  This variable is obsolete")
                  (when (nth 2 obsolete) (princ (format " since %s" (nth 2 obsolete))))
                  (princ (cond ((stringp use) (concat ";\n  " use))
                               (use           (format ";\n  use `%s' instead." (car obsolete)))
                               (t             ".")))
                  (terpri))
                (when (member (cons variable val) file-local-variables-alist)
                  (setq extra-line  t)
                  (if (member (cons variable val) dir-local-variables-alist)
                      (let ((file      (and (buffer-file-name)
                                            (not (file-remote-p (buffer-file-name)))
                                            (dir-locals-find-file (buffer-file-name))))
                            (dir-file  t))
                        (princ "  This variable's value is directory-local")
                        (if (null file)
                            (princ ".\n")
                          (princ ", set ")
                          (when (consp file) ; When result is from cache...
                            (if (nth 2 file) ; If cache element has an mtime, assume it came from a file.
                                (setq file  (expand-file-name dir-locals-file (car file)))
                              (setq dir-file  nil))) ; Otherwise, assume it was set directly.
                          (princ (if dir-file "by the file\n  `" "for the directory\n  `"))
                          (with-current-buffer standard-output
                            (insert-text-button file 'type 'help-dir-local-var-def
                                                'help-args (list variable file)))
                          (princ "'.\n")))
                    (princ "  This variable's value is file-local.\n")))
                (when (memq variable ignored-local-variables)
                  (setq extra-line  t)
                  (princ "  This variable is ignored when used as a file-local variable.\n"))
                (when (risky-local-variable-p variable) ; Can be both risky & safe, eg `auto-fill-function'.
                  (setq extra-line  t)
                  (princ "  This variable can be risky when used as a file-local variable.\n")
                  (when (assq variable safe-local-variable-values)
                    (princ "  However, it has been added to `safe-local-variable-values'.\n")))
                (when safe-var
                  (setq extra-line  t)
                  (princ "  This variable is safe as a file local variable ")
                  (princ "if its value\n  satisfies the predicate ")
                  (princ (if (byte-code-function-p safe-var)
                             "which is a byte-compiled expression.\n"
                           (format "`%s'.\n" safe-var))))
                (when extra-line (terpri))
                (princ "Documentation:\n")
                (with-current-buffer standard-output (insert (or doc  "Not documented as a variable."))))
              ;; Make a link to customize if this variable can be customized.
              (when (custom-variable-p variable)
                (let ((customize-label  "customize"))
                  (terpri) (terpri)
                  (princ (concat "You can " customize-label " this variable."))
                  (with-current-buffer standard-output
                    (save-excursion (re-search-backward (concat "\\(" customize-label "\\)") nil t)
                                    (help-xref-button 1 'help-customize-variable variable))))
                ;; Note variable's version or package version
                (let ((output  (describe-variable-custom-version-info variable)))
                  (when output (terpri) (terpri) (princ output))))
              (unless valvoid
                (with-current-buffer standard-output ; Link to manuals.
                  (Info-make-manuals-xref variable nil nil (not (called-interactively-p 'interactive)))
                  (let ((nb-nls  (cond ((looking-at "[\n][\n][\n]")  3)
                                       ((looking-at "[\n][\n]")      2)
                                       ((looking-at "[\n]")          1)
                                       (t                            0))))
                    (delete-region (- (line-beginning-position) nb-nls) (line-beginning-position)))))
              (with-current-buffer standard-output (buffer-string))))))))) ; Return the text displayed.

;;;###autoload
(defun describe-option (variable &optional buffer) ; Bound to `C-h o'
  "Describe an Emacs user variable (option).
Same as using a prefix arg with `describe-variable'."
  (interactive (let ((symb                          (or (and (fboundp 'symbol-nearest-point)
                                                             (symbol-nearest-point))
                                                        (variable-at-point)))
                     (enable-recursive-minibuffers  t))
                 (when (numberp symb) (setq symb  nil)) ; `variable-at-point' returns 0 when there is no var.
                 (list (intern (completing-read
                                (format "Describe user option%s: "
                                        (if (and symb  (user-variable-p symb))
                                            (format " (default %s)" symb)
                                          ""))
                                obarray 'user-variable-p
                                t nil nil (and symb  (user-variable-p symb)  (symbol-name symb)) t)))))
  (describe-variable variable buffer t))

;;;###autoload
(defun describe-option-of-type (type option) ; Bound to `C-h C-o'
  "Describe an Emacs user OPTION (variable) of a given `defcustom' TYPE.
A prefix argument determines the type-checking behavior:
 - None:         OPTION is defined with TYPE or a subtype of TYPE.
 - Plain `C-u':  OPTION is defined with TYPE or a subtype of TYPE,
                 or its current value is compatible with TYPE.
 - Negative:     OPTION is defined with TYPE (exact match).
 - Non-negative: OPTION is defined with TYPE (exact match),
                 or its current value is compatible with TYPE.

If TYPE is nil (default value) then *all* `defcustom' variables are
potential candidates.  That is different from using `describe-option',
because `describe-option' includes user-variable candidates not
defined with `defcustom' (with `*'-prefixed doc strings)."
  (interactive
   (let* ((symb     (or (and (fboundp 'symbol-nearest-point)  (symbol-nearest-point))  (variable-at-point)))
          (typ       (car (condition-case err
                              (read-from-string (let ((types  ()))
                                                  (mapatoms
                                                   (lambda (cand)
                                                     (when (custom-variable-p cand)
                                                       (push (list
                                                              (format
                                                               "%s"
                                                               (format "%S" (get cand 'custom-type))))
                                                             types))))
                                                  (completing-read "Describe option of type: "
                                                                   (help-remove-duplicates types)
                                                                   nil nil nil nil "nil")))
                            (end-of-file (error "No such custom type")))))
          (pref-arg  current-prefix-arg))
     (when (numberp symb) (setq symb  nil)) ; `variable-at-point' returns 0 when there is no var.
     (list typ
           (intern
            (completing-read
             (format "Option%s: " (if (and symb  (user-variable-p symb)) (format " (default %s)" symb) ""))
             obarray
             (lambda (v)
               (and (custom-variable-p v)
                    (or (not typ) ; Allow all vars if requested type = nil.
                        (help-var-is-of-type-p v (list typ) (cond ((not pref-arg)  'inherit)
                                                                  ((consp pref-arg)  'inherit-or-value)
                                                                  ((wholenump
                                                                    (prefix-numeric-value pref-arg))
                                                                   'direct-or-value)
                                                                  (t  'direct))))))
             t nil nil (and symb  (user-variable-p symb)  (symbol-name symb)) t)))))
  (describe-variable option nil t))

(defun help-var-is-of-type-p (variable types &optional mode)
  "Return non-nil if VARIABLE satisfies one of the custom types in TYPES.
TYPES is a list of `defcustom' type sexps or a list of regexp strings.
TYPES are matched, in order, against VARIABLE's type definition or
VARIABLE's current value, until one is satisfied or all are tried.

If TYPES is a list of regexps, then each is regexp-matched against
VARIABLE's custom type.

Otherwise, TYPES is a list of type sexps, each of which is a
definition acceptable for `defcustom' :type or the first symbol of
such a definition (e.g. `choice').  In this case, two kinds of type
comparison are possible:

1. VARIABLE's custom type, or its first symbol, is matched using
  `equal' against each type in TYPES.

2. VARIABLE's current value is checked against each type in TYPES to
   see if it satisfies one of them.  In this case, VARIABLE's own type
   is not used; VARIABLE might not even be typed - it could be a
   variable not defined using `defcustom'.

For any of the comparisons against VARIABLE's type, either that type
can be checked directly or its supertypes (inherited types) can also
be checked.

These different type-checking possibilities depend on the value of
argument MODE, as follows, and they determine the meaning of the
returned value:

`direct':   VARIABLE's type matches a member of list TYPES
`inherit':  VARIABLE's type matches or is a subtype of a TYPES member
`value':    VARIABLE is bound, and its value satisfies a type in TYPES
`inherit-or-value': `inherit' or `value', tested in that order
`direct-or-value':  `direct' or `value', tested in that order
anything else (default): `inherit'

VARIABLE's current value cannot satisfy a regexp type: it is
impossible to know which concrete types a value must match."
  (case mode
    ((nil inherit)     (help-var-inherits-type-p variable types))
    (inherit-or-value  (or (help-var-inherits-type-p variable types)
                           (help-var-val-satisfies-type-p variable types)))
    (value             (help-var-val-satisfies-type-p variable types))
    (direct            (help-var-matches-type-p variable types))
    (direct-or-value   (or (member (get variable 'custom-type) types)
                           (help-var-val-satisfies-type-p variable types)))
    (otherwise         (help-var-inherits-type-p variable types))))

(defun help-var-matches-type-p (variable types)
  "VARIABLE's type matches a member of TYPES."
  (catch 'help-type-matches
    (let ((var-type  (get variable 'custom-type)))
      (dolist (type  types)
        (when (if (stringp type)
                  (save-match-data (string-match type (format "%s" (format "%S" var-type))))
                (equal var-type type))
          (throw 'help-type-matches t))))
    nil))

(defun help-var-inherits-type-p (variable types)
  "VARIABLE's type matches or is a subtype of a member of list TYPES."
  (catch 'help-type-inherits
    (let ((var-type  (get variable 'custom-type)))
      (dolist (type  types)
        (while var-type
          (when (or (and (stringp type)
                         (save-match-data (string-match type (format "%s" (format "%S" var-type)))))
                    (equal type var-type))
            (throw 'help-type-inherits t))
          (when (consp var-type)  (setq var-type  (car var-type)))
          (when (or (and (stringp type)
                         (save-match-data (string-match type (format "%s" (format "%S" var-type)))))
                    (equal type var-type))
            (throw 'help-type-inherits t))
          (setq var-type  (car (get var-type 'widget-type))))
        (setq var-type  (get variable 'custom-type))))
    nil))

(defun help-var-val-satisfies-type-p (variable types)
  "VARIABLE is bound, and its value satisfies a type in the list TYPES."
  (and (boundp variable)
       (let ((val  (symbol-value variable)))
         (and (widget-convert (get variable 'custom-type))  (help-value-satisfies-type-p val types)))))

(defun help-value-satisfies-type-p (value types)
  "Return non-nil if VALUE satisfies a type in the list TYPES."
  (catch 'help-type-value-satisfies
    (dolist (type  types)
      (unless (stringp type)            ; Skip, for regexp type.
        (setq type  (widget-convert type))
        (when (condition-case nil ; Satisfies if either :match or :validate.
                  (progn (when (and (widget-get type :match)  (widget-apply type :match value))
                           (throw 'help-type-value-satisfies t))
                         (when (and (widget-get type :validate)
                                    (progn (widget-put type :value value)
                                           (not (widget-apply type :validate))))
                           (throw 'help-type-value-satisfies t)))
                (error nil))
          (throw 'help-type-value-satisfies t))))
    nil))

(defun help-custom-type (variable)
  "Returns the `defcustom' type of VARIABLE.
Returns nil if VARIABLE is not a user option.

Note: If the library that defines VARIABLE has not yet been loaded,
then `help-custom-type' loads it.  Be sure you want to do that
before you call this function."
  (and (custom-variable-p variable)
       (or (get variable 'custom-type)  (progn (custom-load-symbol variable)
                                               (get variable 'custom-type)))))

;; Borrowed from `ps-print.el'
(defun help-remove-duplicates (list)
  "Copy of LIST with duplicate elements removed.  Tested with `equal'."
  (let ((tail  list)
        new)
    (while tail
      (unless (member (car tail) new) (push (car tail) new))
      (pop tail))
    (nreverse new)))


;; REPLACE ORIGINAL in `faces.el':
;;
;; Call `Info-make-manuals-xref' to create a cross-ref link to manuals.
;;
(when (or (> emacs-major-version 23)  (and (= emacs-major-version 23)  (> emacs-minor-version 1)))
  (defun describe-face (face &optional frame)
    "Display the properties of face FACE on FRAME.
Interactively, FACE defaults to the faces of the character after point
and FRAME defaults to the selected frame.

If the optional argument FRAME is given, report on face FACE in that frame.
If FRAME is t, report on the defaults for face FACE (for new frames).
If FRAME is omitted or nil, use the selected frame."
    (interactive
     (list (read-face-name "Describe face" (if (> emacs-major-version 23)
                                               (or (face-at-point t)  'default)
                                             "= `default' face")
                           t)))
    (let* ((attrs      '((:family . "Family")
                         (:foundry . "Foundry")
                         (:width . "Width")
                         (:height . "Height")
                         (:weight . "Weight")
                         (:slant . "Slant")
                         (:foreground . "Foreground")
                         (:background . "Background")
                         (:underline . "Underline")
                         (:overline . "Overline")
                         (:strike-through . "Strike-through")
                         (:box . "Box")
                         (:inverse-video . "Inverse")
                         (:stipple . "Stipple")
                         (:font . "Font")
                         (:fontset . "Fontset")
                         (:inherit . "Inherit")))
           (max-width  (apply #'max (mapcar #'(lambda (x) (length (cdr x))) attrs))))
      (help-setup-xref (list #'describe-face face) (called-interactively-p 'interactive))
      (unless face (setq face  'default))
      (unless (listp face) (setq face  (list face)))
      (with-help-window (help-buffer)
        (with-current-buffer standard-output
          (dolist (f  face)
            (when (stringp f) (setq f  (intern f)))
            ;; We may get called for anonymous faces (i.e., faces expressed using prop-value plists).
            ;; Those can't be usefully customized, so ignore them.
            (when (symbolp f)
              (insert "Face: " (symbol-name f))
              (if (not (facep f))
                  (insert "   undefined face.\n")
                (let ((customize-label  "customize this face")
                      file-name)
                  (insert (concat " (" (propertize "sample" 'font-lock-face f) ")"))
                  (princ (concat " (" customize-label ")\n"))
                  ;; FIXME not sure how much of this belongs here, how much in `face-documentation'.
                  ;; The latter is not used much, but needs to return nil for undocumented faces.
                  (let ((alias  (get f 'face-alias))
                        (face   f)
                        obsolete)
                    (when alias
                      (setq face  alias)
                      (insert (format "\n  %s is an alias for the face `%s'.\n%s" f alias
                                      (if (setq obsolete  (get f 'obsolete-face))
                                          (format "  This face is obsolete%s; use `%s' instead.\n"
                                                  (if (stringp obsolete) (format " since %s" obsolete) "")
                                                  alias)
                                        ""))))
                    (insert "\nDocumentation:\n" (or (face-documentation face)
                                                     "Not documented as a face.")
                            "\n\n"))
                  (with-current-buffer standard-output
                    (save-excursion (re-search-backward (concat "\\(" customize-label "\\)") nil t)
                                    (help-xref-button 1 'help-customize-face f)))
                  (setq file-name  (find-lisp-object-file-name f 'defface))
                  (when file-name
                    (princ "Defined in `") (princ (file-name-nondirectory file-name)) (princ "'")
                    (save-excursion ; Make a hyperlink to the library.
                      (re-search-backward "`\\([^`']+\\)'" nil t)
                      (help-xref-button 1 'help-face-def f file-name))
                    (princ ".") (terpri) (terpri))
                  (dolist (a  attrs)
                    (let ((attr  (face-attribute f (car a) frame)))
                      (insert (make-string (- max-width (length (cdr a))) ?\s)
                              (cdr a) ": " (format "%s" attr))
                      (when (and (eq (car a) :inherit)  (not (eq attr 'unspecified)))
                        (save-excursion ; Make a hyperlink to the parent face.
                          (re-search-backward ": \\([^:]+\\)" nil t)
                          (help-xref-button 1 'help-face attr)))
                      (insert "\n")))
                  (when (boundp 'Info-virtual-files) ; Emacs 23.2+
                    (with-current-buffer standard-output ; Link to manuals.
                      (Info-make-manuals-xref f nil nil (not (called-interactively-p 'interactive)))))))
              (terpri))))))))

;;;###autoload
(defun describe-file (filename &optional internal-form-p no-error-p) ; Bound to `C-h M-f'
  "Describe the file named FILENAME.
If FILENAME is nil, describe current directory (`default-directory').

Starting with Emacs 22, if the file is an image file then:
 * Show a thumbnail of the image as well.
 * If you have command-line tool `exiftool' installed and in your
   `$PATH' or `exec-path', then show EXIF data (metadata) about the
   image.  See standard Emacs library `image-dired.el' for more
   information about `exiftool'.

If FILENAME is the name of an autofile bookmark and you use library
`Bookmark+', then show also the bookmark information (tags etc.).  In
this case, a prefix arg shows the internal form of the bookmark.

In Lisp code:

Non-nil optional arg INTERNAL-FORM-P shows the internal form.
Non-nil optional arg NO-ERROR-P prints an error message but does not
 raise an error."
  (interactive "FDescribe file: \nP")
  (unless filename (setq filename default-directory))
  (help-setup-xref `(describe-file ,filename ,internal-form-p ,no-error-p)
                   (if (or (> emacs-major-version 23) ; Emacs 23.1 `called-interactively' accepts no arg.
                           (and (= emacs-major-version 23)  (> emacs-minor-version 1)))
                       (called-interactively-p 'interactive)
                     (interactive-p)))
  (let ((attrs  (file-attributes filename))
        ;; Functions `bmkp-*' are defined in `bookmark+.el'.
        (bmk    (and (fboundp 'bmkp-get-autofile-bookmark)  (bmkp-get-autofile-bookmark filename))))
    (if (not attrs)
        (if no-error-p (message "Cannot open file `%s'" filename) (error "Cannot open file `%s'" filename))
      (let* ((type             (nth 0 attrs))
             (numlinks         (nth 1 attrs))
             (uid              (nth 2 attrs))
             (gid              (nth 3 attrs))
             (last-access      (nth 4 attrs))
             (last-mod         (nth 5 attrs))
             (last-status-chg  (nth 6 attrs))
             (size             (nth 7 attrs))
             (permissions      (nth 8 attrs))
             ;; Skip 9: t iff file's gid would change if file were deleted and recreated.
             (inode            (nth 10 attrs))
             (device           (nth 11 attrs))
             (thumb-string     (and (fboundp 'image-file-name-regexp) ; In `image-file.el' (Emacs 22+).
                                    (if (fboundp 'string-match-p)
                                        (string-match-p (image-file-name-regexp) filename)
                                      (save-match-data
                                        (string-match (image-file-name-regexp) filename)))
                                    (if (fboundp 'display-graphic-p) (display-graphic-p) window-system)
                                    (require 'image-dired nil t)
                                    (image-dired-get-thumbnail-image filename)
                                    (apply #'propertize "XXXX"
                                           `(display ,(append (image-dired-get-thumbnail-image filename)
                                                              '(:margin 10))
                                                     rear-nonsticky (display)
                                                     mouse-face highlight
                                                     follow-link t
                                                     help-echo "`mouse-2' or `RET': Show full image"
                                                     keymap (keymap
                                                             (mouse-2 . (lambda (e) (interactive "e")
                                                                           (find-file ,filename)))
                                                             (13 . (lambda () (interactive)
                                                                      (find-file ,filename))))))))
             (image-info       (and (require 'image-dired nil t)
                                    (fboundp 'image-file-name-regexp)
                                    (if (fboundp 'string-match-p)
                                        (string-match-p (image-file-name-regexp) filename)
                                      (save-match-data
                                        (string-match (image-file-name-regexp) filename)))
                                    (progn (when (if (or (> emacs-major-version 23)
                                                         (and (= emacs-major-version 23)
                                                              (> emacs-minor-version 1)))
                                                     (called-interactively-p 'interactive)
                                                   (interactive-p))
                                             (message "Gathering image data..."))  t)
                                    (condition-case nil
                                        (let ((all  (help-all-exif-data (expand-file-name filename))))
                                          (concat
                                           (and all
                                                (not (zerop (length all)))
                                                (format "\nImage Data (EXIF)\n-----------------\n%s" all))))
                                      (error nil))))
             (help-text        (concat
                                (format "%s\n%s\n\n" filename (make-string (length filename) ?-))
                                (format "File Type:                       %s\n"
                                        (cond ((eq t type)  "Directory")
                                              ((stringp type)  (format "Symbolic link to `%s'" type))
                                              (t  "Normal file")))
                                (format "Permissions:                %s\n" permissions)
                                (and (not (eq t type))  (format "Size in bytes:              %g\n" size))
                                (format-time-string
                                 "Time of last access:        %a %b %e %T %Y (%Z)\n" last-access)
                                (format-time-string
                                 "Time of last modification:  %a %b %e %T %Y (%Z)\n" last-mod)
                                (format-time-string
                                 "Time of last status change: %a %b %e %T %Y (%Z)\n" last-status-chg)
                                (format "Number of links:            %d\n" numlinks)
                                (format "User ID (UID):              %s\n" uid)
                                (format "Group ID (GID):             %s\n" gid)
                                (format "Inode:                      %S\n" inode)
                                (format "Device number:              %s\n" device)
                                image-info)))
        (if (fboundp 'with-help-window)
            (with-help-window (help-buffer)
              (when bmk
                (if internal-form-p
                    (let* ((bname     (bookmark-name-from-full-record bmk))
                           (bmk-defn  (format "Bookmark `%s'\n%s\n\n%s" bname
                                              (make-string (+ 11 (length bname)) ?-)
                                              (pp-to-string bmk))))
                      (princ bmk-defn) (terpri) (terpri))
                  (princ (bmkp-bookmark-description bmk 'NO-IMAGE)) (terpri) (terpri)))
              (princ help-text))
          (with-output-to-temp-buffer "*Help*"
            (when bmk
              (if internal-form-p
                  (let* ((bname     (bookmark-name-from-full-record bmk))
                         (bmk-defn  (format "Bookmark `%s'\n%s\n\n%s" bname
                                            (make-string (+ 11 (length bname)) ?-)
                                            (pp-to-string bmk))))
                    (princ bmk-defn) (terpri) (terpri))
                (princ (bmkp-bookmark-description bmk 'NO-IMAGE)) (terpri) (terpri)))
            (princ help-text)))
        (when thumb-string
          (with-current-buffer "*Help*"
            (save-excursion
              (goto-char (point-min))
              (let ((buffer-read-only  nil))
                (when (re-search-forward "Device number:.+\n" nil t) (insert thumb-string))))))
        help-text))))                   ; Return displayed text.

(defun help-all-exif-data (file)
  "Return all EXIF data from FILE, using command-line tool `exiftool'."
  (with-temp-buffer
    (delete-region (point-min) (point-max))
    (unless (eq 0 (call-process shell-file-name nil t nil shell-command-switch
                                (format "exiftool -All \"%s\"" file)))
      (error "Could not get EXIF data"))
    (buffer-substring (point-min) (point-max))))

(defun describe-keymap (keymap &optional search-symbols-p) ; Bound to `C-h M-k'
  "Describe key bindings in KEYMAP.
Interactively, prompt for a variable that has a keymap value.
Completion is available for the variable name.

Non-interactively:
* KEYMAP can be such a keymap variable or a keymap.
* Non-nil optional arg SEARCH-SYMBOLS-P means that if KEYMAP is not a
  symbol then search all variables for one whose value is KEYMAP."
  (interactive (list (intern (completing-read "Keymap: " obarray
                                              (lambda (m) (and (boundp m)  (keymapp (symbol-value m))))
                                              t nil 'variable-name-history))))
  (unless (and (symbolp keymap)  (boundp keymap)  (keymapp (symbol-value keymap)))
    (if (not (keymapp keymap))
        (error "%sot a keymap%s"
               (if (symbolp keymap) (format "`%S' is n" keymap) "N")
               (if (symbolp keymap) " variable" ""))
      (let ((sym  nil))
        (when search-symbols-p
          (setq sym  (catch 'describe-keymap
                       (mapatoms (lambda (symb) (when (and (boundp symb)
                                                      (eq (symbol-value symb) keymap)
                                                      (not (eq symb 'keymap))
                                                      (throw 'describe-keymap symb)))))
                       nil)))
        (unless sym
          (setq sym  (gentemp "KEYMAP OBJECT (no variable) "))
          (set sym keymap))
        (setq keymap  sym))))
  (setq keymap  (or (condition-case nil (indirect-variable keymap) (error nil))  keymap)) ; Follow aliasing.
  (let* ((name  (symbol-name keymap))
         (doc   (if (fboundp 'help-documentation-property) ; Emacs 23+
                    (help-documentation-property keymap 'variable-documentation nil 'ADD-HELP-BUTTONS)
                  (documentation-property keymap 'variable-documentation)))
         (doc   (and (not (equal "" doc))  doc)))
    (help-setup-xref (list #'describe-keymap keymap)
                     (if (or (> emacs-major-version 23) ; Emacs 23.1 `called-interactively' accepts no arg.
                             (and (= emacs-major-version 23)  (> emacs-minor-version 1)))
                         (called-interactively-p 'interactive)
                       (interactive-p)))
    (if (fboundp 'with-help-window)
        (with-help-window (help-buffer)
          (princ name) (terpri) (princ (make-string (length name) ?-)) (terpri) (terpri)
          (when doc
            (when (boundp 'Info-virtual-files) ; Emacs 23.2+
              (with-current-buffer "*Help*"    ; Link to manuals.
                (Info-make-manuals-xref name nil nil (not (if (or (> emacs-major-version 23)
                                                                  (and (= emacs-major-version 23)
                                                                       (> emacs-minor-version 1)))
                                                              (called-interactively-p 'interactive)
                                                            (interactive-p))))))
            (princ doc) (terpri) (terpri))
          ;; Use `insert' instead of `princ', so control chars (e.g. \377) insert correctly.
          (with-current-buffer "*Help*" (insert (substitute-command-keys (concat "\\{" name "}")))))
      (with-output-to-temp-buffer "*Help*"
        (princ name) (terpri) (princ (make-string (length name) ?-)) (terpri) (terpri)
        (when doc
          (when (boundp 'Info-virtual-files) ; Emacs 23.2+
            (with-current-buffer "*Help*"    ; Link to manuals.
              (Info-make-manuals-xref name nil nil (not (if (or (> emacs-major-version 23)
                                                                (and (= emacs-major-version 23)
                                                                     (> emacs-minor-version 1)))
                                                            (called-interactively-p 'interactive)
                                                          (interactive-p))))))
          (princ doc) (terpri) (terpri))
        ;; Use `insert' instead of `princ', so control chars (e.g. \377) insert correctly.
        (with-current-buffer "*Help*" (insert (substitute-command-keys (concat "\\{" name "}"))))))))


;; REPLACE ORIGINAL in `package.el':
;;
;; Call `Info-make-manuals-xref' to create a cross-ref link to manuals.
;;
(when (fboundp 'describe-package)       ; Emacs 24+

  (when (or (> emacs-major-version 24)  (and (= emacs-major-version 24)  (> emacs-minor-version 3)))
    (defun describe-package (package)
      "Display the full documentation of PACKAGE (a symbol)."
      (interactive
       (let* ((guess (function-called-at-point)))
         (require 'finder-inf nil t)
         ;; Load the package list if necessary (but don't activate them).
         (unless package--initialized
           (package-initialize t))
         (let ((packages (append (mapcar 'car package-alist)
                                 (mapcar 'car package-archive-contents)
                                 (mapcar 'car package--builtins))))
           (unless (memq guess packages)
             (setq guess nil))
           (setq packages (mapcar 'symbol-name packages))
           (let ((val
                  (completing-read (if guess
                                       (format "Describe package (default %s): "
                                               guess)
                                     "Describe package: ")
                                   packages nil t nil nil guess)))
             (list (intern val))))))
      (if (not (or (package-desc-p package) (and package (symbolp package))))
          (message "No package specified")
        (help-setup-xref (list #'describe-package package)
                         (called-interactively-p 'interactive))
        (with-help-window (help-buffer)
          (with-current-buffer standard-output
            (describe-package-1 package)
            (let* ((desc  (or (and (package-desc-p package)  package)
                              (cadr (assq package package-alist))
                              (let ((built-in  (assq package package--builtins)))
                                (if built-in
                                    (package--from-builtin built-in)
                                  (cadr (assq package package-archive-contents))))))
                   (name  (if desc (package-desc-name desc) package)))
              (setq package  name)
              (Info-make-manuals-xref (concat (symbol-name package) " package")
                                      nil nil (not (called-interactively-p 'interactive))))))))) ; Link to manuals

  (unless (or (> emacs-major-version 24)  (and (= emacs-major-version 24)  (> emacs-minor-version 3)))
    (defun describe-package (package)
      "Display the full documentation of PACKAGE (a symbol)."
      (interactive
       (let* ((guess  (function-called-at-point)))
         (require 'finder-inf nil t)
         ;; Load the package list if necessary (but don't activate them).
         (unless package--initialized (package-initialize t))
         (let ((packages  (append (mapcar 'car package-alist) (mapcar 'car package-archive-contents)
                                  (mapcar 'car package--builtins))))
           (unless (memq guess packages) (setq guess  nil))
           (setq packages  (mapcar 'symbol-name packages))
           (let ((val  (completing-read (if guess
                                            (format "Describe package (default %s): " guess)
                                          "Describe package: ")
                                        packages nil t nil nil guess)))
             (list (if (equal val "") guess (intern val)))))))
      (if (not (or (and (fboundp 'package-desc-p)  (package-desc-p package))
                   (and package (symbolp package))))
          (when (called-interactively-p 'interactive) (message "No package specified"))
        (help-setup-xref (list #'describe-package package) (called-interactively-p 'interactive))
        (with-help-window (help-buffer)
          (with-current-buffer standard-output
            (describe-package-1 package)
            (when (fboundp 'package-desc-name)  (setq package  (package-desc-name package))) ; Emacs 24.4
            (Info-make-manuals-xref (concat (symbol-name package) " package")
                                    nil nil (not (called-interactively-p 'interactive)))))))) ; Link to manuals

  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'help-fns+)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; help-fns+.el ends here
