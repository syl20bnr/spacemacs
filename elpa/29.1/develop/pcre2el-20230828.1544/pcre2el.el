;;; pcre2el.el --- regexp syntax converter -*- lexical-binding: t -*-

;; Copyright (C) 2012-2015 Jon Oddie <jonxfield@gmail.com>

;; Author:			joddie <jonxfield at gmail.com>
;; Hacked additionally by:	opensource at hardakers dot net
;; Created:			14 Feb 2012
;; Updated:			13 December 2015
;; Version:                     1.12
;; Url:                         https://github.com/joddie/pcre2el
;; Package-Requires:            ((emacs "25.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see `http://www.gnu.org/licenses/'.

;; This file incorporates work covered by the following copyright and
;; permission notice:
;;
;; Copyright (c) 1993-2002 Richard Kelsey and Jonathan Rees
;; Copyright (c) 1994-2002 by Olin Shivers and Brian D. Carlstrom.
;; Copyright (c) 1999-2002 by Martin Gasbichler.
;; Copyright (c) 2001-2002 by Michael Sperber.
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met: 1. Redistributions of source code must retain the above
;; copyright notice, this list of conditions and the following
;; disclaimer. 2. Redistributions in binary form must reproduce the
;; above copyright notice, this list of conditions and the following
;; disclaimer in the documentation and/or other materials provided
;; with the distribution. 3. The name of the authors may not be used
;; to endorse or promote products derived from this software without
;; specific prior written permission.
;;
;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS "AS IS" AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;; ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY
;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:

;; 1 Overview
;; ==========

;;   `pcre2el' or `rxt' (RegeXp Translator or RegeXp Tools) is a utility
;;   for working with regular expressions in Emacs, based on a
;;   recursive-descent parser for regexp syntax. In addition to converting
;;   (a subset of) PCRE syntax into its Emacs equivalent, it can do the
;;   following:

;;   - convert Emacs syntax to PCRE
;;   - convert either syntax to `rx', an S-expression based regexp syntax
;;   - untangle complex regexps by showing the parse tree in `rx' form and
;;     highlighting the corresponding chunks of code
;;   - show the complete list of strings (productions) matching a regexp,
;;     provided the list is finite
;;   - provide live font-locking of regexp syntax (so far only for Elisp
;;     buffers -- other modes on the TODO list)


;; 2 Usage
;; =======

;;   Enable `rxt-mode' or its global equivalent `rxt-global-mode' to get
;;   the default key-bindings. There are three sets of commands: commands
;;   that take a PCRE regexp, commands which take an Emacs regexp, and
;;   commands that try to do the right thing based on the current
;;   mode. Currently, this means Emacs syntax in `emacs-lisp-mode' and
;;   `lisp-interaction-mode', and PCRE syntax everywhere else.

;;   The default key bindings all begin with `C-c /' and have a mnemonic
;;   structure: `C-c / <source> <target>', or just `C-c / <target>' for the
;;   "do what I mean" commands. The complete list of key bindings is given
;;   here and explained in more detail below:

;;   - "Do-what-I-mean" commands:
;;     `C-c / /': `rxt-explain'
;;     `C-c / c': `rxt-convert-syntax'
;;     `C-c / x': `rxt-convert-to-rx'
;;     `C-c / '': `rxt-convert-to-strings'

;;   - Commands that work on a PCRE regexp:
;;     `C-c / p e': `rxt-pcre-to-elisp'
;;     `C-c / %': `pcre-query-replace-regexp'
;;     `C-c / p x': `rxt-pcre-to-rx'
;;     `C-c / p '': `rxt-pcre-to-strings'
;;     `C-c / p /': `rxt-explain-pcre'

;;   - Commands that work on an Emacs regexp:
;;     `C-c / e /': `rxt-explain-elisp'
;;     `C-c / e p': `rxt-elisp-to-pcre'
;;     `C-c / e x': `rxt-elisp-to-rx'
;;     `C-c / e '': `rxt-elisp-to-strings'
;;     `C-c / e t': `rxt-toggle-elisp-rx'
;;     `C-c / t': `rxt-toggle-elisp-rx'


;; 2.1 Interactive input and output
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   When used interactively, the conversion commands can read a regexp
;;   either from the current buffer or from the minibuffer. The output is
;;   displayed in the minibuffer and copied to the kill-ring.

;;   - When called with a prefix argument (`C-u'), they read a regular
;;     expression from the minibuffer literally, without further processing
;;     -- meaning there's no need to double the backslashes if it's an
;;     Emacs regexp.  This is the same way commands like
;;     `query-replace-regexp' read input.

;;   - When the region is active, they use they the region contents, again
;;     literally (without any translation of string syntax).

;;   - With neither a prefix arg nor an active region, the behavior depends
;;     on whether the command expects an Emacs regexp or a PCRE one.

;;     Commands that take an Emacs regexp behave like `C-x C-e': they
;;     evaluate the sexp before point (which could be simply a string
;;     literal) and use its value. This is designed for use in Elisp
;;     buffers. As a special case, if point is *inside* a string, it's
;;     first moved to the string end, so in practice they should work as
;;     long as point is somewhere within the regexp literal.

;;     Commands that take a PCRE regexp try to read a Perl-style delimited
;;     regex literal *after* point in the current buffer, including its
;;     flags. For example, putting point before the `m' in the following
;;     example and doing `C-c / p e' (`rxt-pcre-to-elisp') displays
;;     `\(?:bar\|foo\)', correctly stripping out the whitespace and
;;     comment:

;;     ,----
;;     | $x =~ m/  foo   |  (?# comment) bar /x
;;     `----

;;     The PCRE reader currently only works with `/ ... /' delimiters. It
;;     will ignore any preceding `m', `s', or `qr' operator, as well as the
;;     replacement part of an `s' construction.

;;     Readers for other PCRE-using languages are on the TODO list.

;;   The translation functions display their result in the minibuffer and
;;   copy it to the kill ring. When translating something into Elisp
;;   syntax, you might need to use the result either literally (e.g. for
;;   interactive input to a command like `query-replace-regexp'), or as a
;;   string to paste into Lisp code.  To allow both uses,
;;   `rxt-pcre-to-elisp' copies both versions successively to the
;;   kill-ring. The literal regexp without string quoting is the top
;;   element of the kill-ring, while the Lisp string is the
;;   second-from-top. You can paste the literal regexp somewhere by doing
;;   `C-y', or the Lisp string by `C-y M-y'.


;; 2.2 Syntax conversion commands
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   `rxt-convert-syntax' (`C-c / c') converts between Emacs and PCRE
;;   syntax, depending on the major mode in effect when called.
;;   Alternatively, you can specify the conversion direction explicitly by
;;   using either `rxt-pcre-to-elisp' (`C-c / p e') or `rxt-elisp-to-pcre'
;;   (`C-c / e p').

;;   Similarly, `rxt-convert-to-rx' (`C-c / x') converts either kind of
;;   syntax to `rx' form, while `rxt-convert-pcre-to-rx' (`C-c / p x') and
;;   `rxt-convert-elisp-to-rx' (`C-c / e x') convert to `rx' from a
;;   specified source type.

;;   In Elisp buffers, you can use `rxt-toggle-elisp-rx' (`C-c / t' or `C-c
;;   / e t') to switch the regexp at point back and forth between string
;;   and `rx' syntax. Point should either be within an `rx' or
;;   `rx-to-string' form or a string literal for this to work.


;; 2.3 PCRE mode (experimental)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   If you want to use emulated PCRE regexp syntax in all Emacs commands,
;;   try `pcre-mode', which uses Emacs's advice system to make all commands
;;   that read regexps using the minibuffer use emulated PCRE syntax.  It
;;   should also work with Isearch.

;;   This feature is still fairly experimental.  It may fail to work or do
;;   the wrong thing with certain commands.  Please report bugs.

;;   `pcre-query-replace-regexp' was originally defined to do query-replace
;;   using emulated PCRE regexps, and is now made somewhat obsolete by
;;   `pcre-mode'.  It is bound to `C-c / %' by default, by analogy with
;;   `M-%'.  Put the following in your `.emacs' if you want to use
;;   PCRE-style query replacement everywhere:

;;   ,----
;;   | (global-set-key [(meta %)] 'pcre-query-replace-regexp)
;;   `----

;; 2.5 Explain regexps
;; ~~~~~~~~~~~~~~~~~~~

;;   When syntax-highlighting isn't enough to untangle some gnarly regexp
;;   you find in the wild, try the 'explain' commands: `rxt-explain' (`C-c
;;   / /'), `rxt-explain-pcre' (`C-c / p') and `rxt-explain-elisp' (`C-c /
;;   e'). These display the original regexp along with its pretty-printed
;;   `rx' equivalent in a new buffer.  Moving point around either in the
;;   original regexp or the `rx' translation highlights corresponding
;;   pieces of syntax, which can aid in seeing things like the scope of
;;   quantifiers.

;;   I call them "explain" commands because the `rx' form is close to a
;;   plain syntax tree, and this plus the wordiness of the operators
;;   usually helps to clarify what is going on.  People who dislike Lisp
;;   syntax might disagree with this assessment.


;; 2.6 Generate all matching strings (productions)
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   Occasionally you come across a regexp which is designed to match a
;;   finite set of strings, e.g. a set of keywords, and it would be useful
;;   to recover the original set. (In Emacs you can generate such regexps
;;   using `regexp-opt'). The commands `rxt-convert-to-strings' (`C-c /
;;   ′'), `rxt-pcre-to-strings' (`C-c / p ′') or `rxt-elisp-to-strings'
;;   (`C-c / e ′') accomplish this by generating all the matching strings
;;   ("productions") of a regexp.  (The productions are copied to the kill
;;   ring as a Lisp list).

;;   An example in Lisp code:

;;   ,----
;;   | (regexp-opt '("cat" "caterpillar" "catatonic"))
;;   |    ;; => "\\(?:cat\\(?:atonic\\|erpillar\\)?\\)"
;;   | (rxt-elisp-to-strings "\\(?:cat\\(?:atonic\\|erpillar\\)?\\)")
;;   |     ;; => '("cat" "caterpillar" "catatonic")
;;   `----

;;   For obvious reasons, these commands only work with regexps that don't
;;   include any unbounded quantifiers like `+' or `*'. They also can't
;;   enumerate all the characters that match a named character class like
;;   `[[:alnum:]]'. In either case they will give a (hopefully meaningful)
;;   error message. Due to the nature of permutations, it's still possible
;;   for a finite regexp to generate a huge number of productions, which
;;   will eat memory and slow down your Emacs. Be ready with `C-g' if
;;   necessary.


;; 2.7 RE-Builder support
;; ~~~~~~~~~~~~~~~~~~~~~~

;;   The Emacs RE-Builder is a useful visual tool which allows using
;;   several different built-in syntaxes via `reb-change-syntax' (`C-c
;;   TAB'). It supports Elisp read and literal syntax and `rx', but it can
;;   only convert from the symbolic forms to Elisp, not the other way. This
;;   package hacks the RE-Builder to also work with emulated PCRE syntax,
;;   and to convert transparently between Elisp, PCRE and rx syntaxes. PCRE
;;   mode reads a delimited Perl-like literal of the form `/ ... /', and it
;;   should correctly support using the `x' and `s' flags.


;; 2.8 Use from Lisp
;; ~~~~~~~~~~~~~~~~~

;;   Example of using the conversion functions:
;;   ,----
;;   | (rxt-pcre-to-elisp "(abc|def)\\w+\\d+")
;;   |    ;; => "\\(\\(?:abc\\|def\\)\\)[_[:alnum:]]+[[:digit:]]+"
;;   `----

;;   All the conversion functions take a single string argument, the regexp
;;   to translate:

;;   - `rxt-pcre-to-elisp'
;;   - `rxt-pcre-to-rx'
;;   - `rxt-pcre-to-strings'
;;   - `rxt-elisp-to-pcre'
;;   - `rxt-elisp-to-rx'
;;   - `rxt-elisp-to-strings'


;; 3 Bugs and Limitations
;; ======================

;; 3.1 Limitations on PCRE syntax
;; ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

;;   PCRE has a complicated syntax and semantics, only some of which can be
;;   translated into Elisp. The following subset of PCRE should be
;;   correctly parsed and converted:

;;   - parenthesis grouping `( .. )', including shy matches `(?: ... )'
;;   - backreferences (various syntaxes), but only up to 9 per expression
;;   - alternation `|'
;;   - greedy and non-greedy quantifiers `*', `*?', `+', `+?', `?' and `??'
;;           (all of which are the same in Elisp as in PCRE)
;;   - numerical quantifiers `{M,N}'
;;   - beginning/end of string `\A', `\Z'
;;   - string quoting `\Q .. \E'
;;   - word boundaries `\b', `\B' (these are the same in Elisp)
;;   - single character escapes `\a', `\c', `\e', `\f', `\n', `\r', `\t',
;;     `\x', and `\octal digits' (but see below about non-ASCII characters)
;;   - character classes `[...]' including Posix escapes
;;   - character classes `\d', `\D', `\h', `\H', `\s', `\S', `\v', `\V'
;;           both within character class brackets and outside
;;   - word and non-word characters `\w' and `\W' (Emacs has the same
;;           syntax, but its meaning is different)
;;   - `s' (single line) and `x' (extended syntax) flags, in regexp
;;     literals, or set within the expression via `(?xs-xs)' or `(?xs-xs:
;;     .... )' syntax
;;   - comments `(?# ... )'

;;   Most of the more esoteric PCRE features can't really be supported by
;;   simple translation to Elisp regexps. These include the different
;;   lookaround assertions, conditionals, and the "backtracking control
;;   verbs" `(* ...)' . OTOH, there are a few other syntaxes which are
;;   currently unsupported and possibly could be:

;;   - `\L', `\U', `\l', `\u' case modifiers
;;   - `\g{...}' backreferences


;; 3.2 Other limitations
;; ~~~~~~~~~~~~~~~~~~~~~

;;   - The order of alternatives and characters in char classes sometimes
;;     gets shifted around, which is annoying.
;;   - Although the string parser tries to interpret PCRE's octal and
;;     hexadecimal escapes correctly, there are problems with matching
;;     8-bit characters that I don't use enough to properly understand,
;;     e.g.:
;;     ,----
;;     | (string-match-p (rxt-pcre-to-elisp "\\377") "\377") => nil
;;     `----
;;     A fix for this would be welcome.

;;   - Most of PCRE's rules for how `^', `\A', `$' and `\Z' interact with
;;     newlines are not implemented, since they seem less relevant to
;;     Emacs's buffer-oriented rather than line-oriented model.  However,
;;     the different meanings of the `.' metacharacter *are* implemented
;;     (it matches newlines with the `/s' flag, but not otherwise).

;;   - Not currently namespace clean (both `rxt-' and a couple of `pcre-'
;;     functions).


;; 3.3 TODO:
;; ~~~~~~~~~

;;   - Python-specific extensions to PCRE?
;;   - Language-specific stuff to enable regexp font-locking and explaining
;;     in different modes. Each language would need two functions, which
;;     could be kept in an alist:

;;     1. A function to read PCRE regexps, taking the string syntax into
;;        account. E.g., Python has single-quoted, double-quoted and raw
;;        strings, each with different quoting rules.  PHP has the kind of
;;        belt-and-suspenders solution you would expect: regexps are in
;;        strings, /and/ you have to include the `/ ...  /' delimiters!
;;        Duh.

;;     2. A function to copy faces back from the parsed string to the
;;        original buffer text. This has to recognize any escape sequences
;;        so they can be treated as a single character.


;; 4 Internal details
;; ==================

;;   `rxt' defines an internal syntax tree representation of regular
;;   expressions, parsers for Elisp and PCRE syntax, and 'unparsers'
;;   to convert the internal representation to PCRE or `rx' syntax.
;;   Converting from the internal representation to Emacs syntax is
;;   done by converting to `rx' form and passing it to `rx-to-string'.
;;   See `rxt-parse-re', `rxt-adt->pcre', and `rxt-adt->rx' for
;;   details.

;;   This code is partially based on Olin Shivers' reference SRE
;;   implementation in scsh, although it is simplified in some respects and
;;   extended in others. See `scsh/re.scm', `scsh/spencer.scm' and
;;   `scsh/posixstr.scm' in the `scsh' source tree for details. In
;;   particular, `pcre2el' steals the idea of an abstract data type for
;;   regular expressions and the general structure of the string regexp
;;   parser and unparser. The data types for character sets are extended in
;;   order to support symbolic translation between character set
;;   expressions without assuming a small (Latin1) character set. The
;;   string parser is also extended to parse a bigger variety of
;;   constructions, including POSIX character classes and various Emacs and
;;   Perl regexp assertions. Otherwise, only the bare minimum of scsh's
;;   abstract data type is implemented.


;; 5 Soapbox
;; =========

;;   Emacs regexps have their annoyances, but it is worth getting used to
;;   them. The Emacs assertions for word boundaries, symbol boundaries, and
;;   syntax classes depending on the syntax of the mode in effect are
;;   especially useful. (PCRE has `\b' for word-boundary, but AFAIK it
;;   doesn't have separate assertions for beginning-of-word and
;;   end-of-word). Other things that might be done with huge regexps in
;;   other languages can be expressed more understandably in Elisp using
;;   combinations of `save-excursion' with the various searches (regexp,
;;   literal, skip-syntax-forward, sexp-movement functions, etc.).

;;   There's not much point in using `rxt-pcre-to-elisp' to use PCRE
;;   notation in a Lisp program you're going to maintain, since you still
;;   have to double all the backslashes.  Better to just use the converted
;;   result (or better yet, the `rx' form).


;; 6 History and acknowledgments
;; =============================

;;   This was originally created out of an answer to a stackoverflow
;;   question:
;;   [http://stackoverflow.com/questions/9118183/elisp-mechanism-for-converting-pcre-regexps-to-emacs-regexps]

;;   Thanks to:

;;   - Wes Hardaker (hardaker) for the initial inspiration and subsequent
;;     hacking
;;   - priyadarshan for requesting RX support
;;   - Daniel Colascione (dcolascione) for a patch to support Emacs's
;;     explicitly-numbered match groups
;;   - Aaron Meurer (asmeurer) for requesting Isearch support
;;   - Philippe Vaucher (silex) for a patch to support `ibuffer-do-replace-regexp'
;;     in PCRE mode

;;; Code:

(require 'cl-lib)
(require 'rx)
(require 're-builder)
(require 'advice)
(require 'ring)
(require 'pcase)

;;; Customization group
(defgroup rxt nil
  "Regex syntax converter and utilities."
  :version 1.2
  :group 'tools
  :group 'lisp
  :link '(emacs-commentary-link :tag "commentary" "pcre2el.el")
  :link '(emacs-library-link :tag "lisp file" "pcre2el.el")
  :link '(url-link :tag "web page" "https://github.com/joddie/pcre2el"))

(defface rxt-highlight-face
  '((((min-colors 16581375) (background light)) :background "#eee8d5")
    (((min-colors 16581375) (background dark)) :background "#222222"))
  "Face for highlighting corresponding regex syntax in
 `rxt-explain' buffers."
  :group 'rxt)

(defcustom rxt-verbose-rx-translation nil
  "Non-nil if `rxt-pcre-to-rx' and `rxt-elisp-to-rx' should use
 verbose `rx' primitives.

Verbose primitives are things like `line-start' instead of `bol',
etc."
  :group 'rxt
  :type 'boolean)

(defcustom rxt-explain-verbosely t
  "Non-nil if `rxt-explain-elisp' and `rxt-explain-pcre' should
 use verbose `rx' primitives.

This overrides the value of `rxt-verbose-rx-translation' for
these commands only."
  :group 'rxt
  :type 'boolean)


;;;; Macros and functions for writing interactive input and output

;; Macros for handling return values.  If called interactively,
;; display the value in the echo area and copy it to the kill ring,
;; otherwise just return the value.  PCREs are copied as unquoted
;; strings for yanking into Perl, JS, etc.  `rx' forms and other sexps
;; are copied as `read'-able literals for yanking into Elisp buffers.
;; Emacs regexps are copied twice: once as an unquoted value for
;; interactive use, and once as a readable string literal for yanking
;; into Elisp buffers.
(defmacro rxt-return-pcre (expr)
  (let ((value (make-symbol "value")))
    `(let ((,value ,expr))
       (when (called-interactively-p 'any)
         (rxt--kill-pcre ,value))
       ,value)))

(defmacro rxt-return-sexp (expr)
  (let ((value (make-symbol "value")))
    `(let ((,value ,expr))
       (when (called-interactively-p 'any)
         (rxt--kill-sexp ,value))
       ,value)))

(defmacro rxt-return-emacs-regexp (expr)
  (let ((value (make-symbol "value")))
    `(let ((,value ,expr))
       (when (called-interactively-p 'any)
         (rxt--kill-emacs-regexp ,value))
       ,value)))

(defun rxt--kill-sexp (value)
  (let ((lisp-literal (prin1-to-string value)))
    (message "%s" lisp-literal)
    (kill-new lisp-literal)))

(defun rxt--kill-pcre (value)
  (message "%s" value)
  (kill-new value))

(defun rxt--kill-emacs-regexp (value)
  (let ((lisp-literal (prin1-to-string value)))
    (message "%s" value)
    (kill-new lisp-literal)
    (kill-new value)))

;; Read an Elisp regexp interactively.
;;
;; Three possibilities:
;;
;; 1) With a prefix arg, reads literally from the minibuffer, w/o
;; using string syntax -- just like query-replace-regexp, etc.
;;
;; 2) If the region is active, use the text of the region literally
;; (again w/o string syntax)
;;
;; 3) Otherwise, eval the sexp before point (which might be a string
;; literal or an expression) and use its value. Falls back to method
;; (1) if this fails to produce a string value.
;;
(cl-defun rxt-interactive/elisp (&optional (prompt "Emacs regexp: "))
  (list
   (cond (current-prefix-arg
          (read-string prompt))

         ((use-region-p)
          (buffer-substring-no-properties (region-beginning) (region-end)))

         (t
          (condition-case nil
              (save-excursion
                (while (nth 3 (syntax-ppss)) (forward-char))
                (let ((re (eval (elisp--preceding-sexp))))
                  (if (stringp re) re
                    (read-string prompt))))
            (error
             (read-string prompt)))))))

;; Read a PCRE regexp interactively.
;;
;; Three possibilities: As above, except that without prefix arg or
;; active region, tries to read a delimited regexp literal like /.../,
;; m/.../, or qr/.../ following point in the current buffer. Falls
;; back to reading from minibuffer if that fails.
;;
;; Returns the regexp, with flags as text properties.
;;
;; TODO: Different delimiters
(cl-defun rxt-interactive/pcre (&optional (prompt "PCRE regexp: "))
  (list
   (cond (current-prefix-arg
          (rxt--read-pcre prompt))

         ((use-region-p)
          (buffer-substring-no-properties (region-beginning) (region-end)))

         (t
          (condition-case nil
              (rxt-read-delimited-pcre)
            (error                     ; Fall back to reading from minibuffer
             (rxt--read-pcre prompt)))))
   nil))

(define-minor-mode rxt--read-pcre-mode
    "Minor-mode with key-bindings for toggling PCRE flags.

You should not normally call this directly.  It will be enabled
in minibuffers for `read-regexp' and in the `re-builder' buffer
when `pcre-mode' is active.  These bindings will also be added to
`isearch-mode-map' in `pcre-mode'."
  :initial-value nil
  :lighter nil
  :keymap
  `((,(kbd "C-c s") . ,#'rxt--toggle-s-mode)
    (,(kbd "C-c x") . ,#'rxt--toggle-x-mode)
    (,(kbd "C-c i") . ,#'rxt--toggle-i-mode)))

(defun rxt--read-pcre (prompt)
  "Read a PCRE regexp for translation, together with option flags.

The `s', `x', and `i' flags can be toggled using the following
commands: \\<rxt--read-pcre-mode-map>

\\[rxt--toggle-s-mode] : toggle `s' (single-line) mode
\\[rxt--toggle-x-mode] : toggle `x' (extended) mode
\\[rxt--toggle-i-mode] : toggle `i' (case-insensitive) mode

In single-line mode, `.' will also match newlines.
In extended mode, whitespace is ignored.

Case-insensitive mode emulates matching without case,
independently of Emacs's builtin `case-fold-search' setting.
Note that this does not apply to backreferences."
  (minibuffer-with-setup-hook #'rxt--read-pcre-mode
    (read-from-minibuffer prompt)))

(defun rxt--toggle-s-mode ()
  "Toggle emulated PCRE single-line (s) flag."
  (interactive)
  (rxt--toggle-flag ?s))

(defun rxt--toggle-x-mode ()
  "Toggle emulated PCRE extended (x) flag."
  (interactive)
  (rxt--toggle-flag ?x))

(defun rxt--toggle-i-mode ()
  "Toggle emulated PCRE case-insensitive (i) flag."
  (interactive)
  (rxt--toggle-flag ?i))

(defun rxt--toggle-flag (char)
  "Toggle CHAR, a PCRE flag."
  (cond
    ((derived-mode-p 'reb-mode)         ; RE-Builder
     (rxt--toggle-flag-re-builder char))
    ((minibufferp)
     (rxt--toggle-flag-minibuffer char))
    (isearch-mode
     (rxt--toggle-flag-isearch char))
    (t
     (error "Not in minibuffer, RE-Builder or isearch mode."))))

(defun rxt--toggle-flag-re-builder (char)
  (save-excursion
    (goto-char (point-max))
    (search-backward "/")
    (forward-char)
    (when (looking-at (rx (* (any ?i ?s ?x))))
      (let ((inhibit-modification-hooks t))
        (replace-match (rxt--xor-flags (match-string 0) char) t t))))
  (reb-do-update))

(defun rxt--toggle-flag-minibuffer (char)
  (setf (buffer-substring (minibuffer-prompt-end) (point-max))
        (rxt--toggle-flag-string (minibuffer-contents) char))
  (when
      (and (= (point) (minibuffer-prompt-end))
           (looking-at (rx "(?" (group (+ (any ?i ?s ?x))) ")")))
    (forward-sexp)))

(defun rxt--toggle-flag-isearch (char)
  (when isearch-regexp
    (setq isearch-string
          (rxt--toggle-flag-string isearch-string char))
    (setq isearch-message
          (mapconcat #'isearch-text-char-description isearch-string ""))
    (isearch-search-and-update)))

(defun rxt--toggle-flag-string (string char)
  (if (string-match (rx string-start "(?" (group (+ (any ?i ?s ?x))) ")")
                    string)
      (let ((flags (rxt--xor-flags (match-string 1 string) char)))
        (if (string= flags "")
            (replace-match "" t t string)
          (replace-match flags t t string 1)))
    (format "(?%c)%s" char string)))

(defun rxt--xor-flags (flags char)
  (concat
   (sort
    (cl-set-exclusive-or (string-to-list flags) (list char))
    #'<)))


;;;; Minor mode for using emulated PCRE syntax

(defvar pcre-old-isearch-search-fun-function nil
  "Original value of `isearch-search-fun-function' before entering `pcre-mode.'

This function is wrapped by `pcre-isearch-search-fun-function'
and restored on exit from `pcre-mode'.")
(make-variable-buffer-local 'pcre-old-isearch-search-fun-function)

(defvar pcre-old-isearch-key-bindings nil
  "Alist of key-bindings to restore in `isearch-mode-map' on exiting `pcre-mode'.")

;;;###autoload
(define-minor-mode pcre-mode
  "Use emulated PCRE syntax for regexps wherever possible.

Advises the `interactive' specs of `read-regexp' and the
following other functions so that they read PCRE syntax and
translate to its Emacs equivalent:

- `align-regexp'
- `find-tag-regexp'
- `sort-regexp-fields'
- `isearch-message-prefix'
- `ibuffer-do-replace-regexp'

Also alters the behavior of `isearch-mode' when searching by regexp."
  :init-value nil
  :lighter " PCRE"
  :keymap nil
  :global t

  (if pcre-mode
      ;; Enabling
      (progn
        ;; Enable advice
        (ad-enable-regexp "pcre-mode")
        ;; Set up isearch hooks
        (add-hook 'isearch-mode-hook #'pcre-isearch-mode-hook)
        (add-hook 'isearch-mode-end-hook #'pcre-isearch-mode-end-hook)
        ;; Add the keybindings of `rxt--read-pcre-mode-map' to
        ;; `isearch-mode-map' (so that they do not cause an exit from
        ;; `isearch-mode'), and save any existing bindings for those
        ;; keys to restore on exit from `pcre-mode'.
        (setq pcre-old-isearch-key-bindings
              (cl-loop for key being the key-seqs of rxt--read-pcre-mode-map
                       for def = (lookup-key isearch-mode-map key)
                       collect (cons (copy-sequence key)
                                     (if (numberp def) nil def))))
        (cl-loop for key being the key-seqs of rxt--read-pcre-mode-map
                 using (key-bindings def)
                 do (define-key isearch-mode-map key def)))

    ;; Disable advice
    (ad-disable-regexp "pcre-mode")
    ;; Remove from isearch hooks
    (remove-hook 'isearch-mode-hook #'pcre-isearch-mode-hook)
    (remove-hook 'isearch-mode-end-hook #'pcre-isearch-mode-end-hook)
    ;; Restore key-bindings
    (cl-loop for (key . def) in pcre-old-isearch-key-bindings
             do (define-key isearch-mode-map key def)))

  ;; "Activating" advice re-computes the function definitions, which
  ;; is necessary whether enabling or disabling
  (ad-activate-regexp "pcre-mode"))

;;; Cache of PCRE -> Elisp translations
(defvar pcre-mode-cache-size 100
  "Number of PCRE-to-Emacs translations to keep in the `pcre-mode' cache.")

(defvar pcre-mode-cache (make-hash-table :test 'equal)
  "Cache of PCRE-to-Emacs translations used in `pcre-mode'.

Keys are PCRE regexps, values are their Emacs equivalents.")

(defvar pcre-mode-reverse-cache (make-hash-table :test 'equal)
  "Cache of original PCREs translated to Emacs syntax in `pcre-mode'.

Keys are translated Emacs regexps, values are their original PCRE
form.  This is used to display the original PCRE regexp in place
of its translated form.")

(defvar pcre-cache-ring (make-ring pcre-mode-cache-size)
  "Ring of PCRE-to-Emacs translations used in `pcre-mode'.

When the ring fills up, the oldest element is removed and the
corresponding entries are deleted from the hash tables
`pcre-mode-cache' and `pcre-mode-reverse-cache'.")

(defun pcre-to-elisp/cached (pcre)
  "Translate PCRE to Emacs syntax, caching both forms."
  (or (gethash pcre pcre-mode-cache)
      (let ((elisp (rxt-pcre-to-elisp pcre)))
        (pcre-set-cache pcre elisp)
        elisp)))

(defun pcre-set-cache (pcre-regexp emacs-regexp)
  "Add a PCRE-to-Emacs translation to the `pcre-mode' cache."
  (when (and (not (zerop (length pcre-regexp)))
             (not (zerop (length emacs-regexp)))
             (not (gethash pcre-regexp pcre-mode-cache)))
    (if (= (ring-length pcre-cache-ring) (ring-size pcre-cache-ring))
        (let* ((old-item (ring-remove pcre-cache-ring))
               (old-pcre (car old-item))
               (old-emacs (cdr old-item)))
          (remhash old-pcre pcre-mode-cache)
          (remhash old-emacs pcre-mode-reverse-cache))
      (puthash pcre-regexp emacs-regexp pcre-mode-cache)
      (puthash emacs-regexp pcre-regexp pcre-mode-reverse-cache)
      (ring-insert pcre-cache-ring (cons pcre-regexp emacs-regexp)))))

;;; Isearch advice
(defun pcre-isearch-mode-hook ()
  (when (not (eq isearch-search-fun-function #'isearch-search-fun-default))
    (message "Warning: pcre-mode overriding existing isearch function `%s'"
             isearch-search-fun-function))
  ;; Prevent an infinite loop, if a previous isearch in pcre-mode
  ;; exited without restoring the original search function for some
  ;; reason
  (unless (eq isearch-search-fun-function #'pcre-isearch-search-fun-function)
    (setq pcre-old-isearch-search-fun-function isearch-search-fun-function))
  (set (make-local-variable 'isearch-search-fun-function)
       #'pcre-isearch-search-fun-function))

(defun pcre-isearch-mode-end-hook ()
  (setq isearch-search-fun-function pcre-old-isearch-search-fun-function))

(defun pcre-isearch-search-fun-function ()
  "Enable isearching using emulated PCRE syntax.

This is set as the value of `isearch-search-fun-function' when
`pcre-mode' is enabled.  Returns a function which searches using
emulated PCRE regexps when `isearch-regexp' is true."
  (lambda (string bound noerror)
    (let ((real-search-function
           (funcall (or pcre-old-isearch-search-fun-function 'isearch-search-fun-default))))
      (if (not isearch-regexp)
          (funcall real-search-function string bound noerror)
        ;; Raise an error if the regexp ends in an incomplete escape
        ;; sequence (= odd number of backslashes).
        ;; TODO: Perhaps this should really be handled in rxt-pcre-to-elisp?
        (if (isearch-backslash string) (rxt-error "Trailing backslash"))
        (funcall real-search-function
                 (pcre-to-elisp/cached string) bound noerror)))))

(defadvice isearch-message-prefix (after pcre-mode disable)
  "Add \"PCRE\" to the Isearch message when searching by regexp in `pcre-mode'."
  (when (and isearch-regexp
             ;; Prevent an inaccurate message if our callback was
             ;; removed somehow
             (eq isearch-search-fun-function #'pcre-isearch-search-fun-function))
    (let ((message ad-return-value))
      ;; Some hackery to give replacement the same fontification as
      ;; the original
      (when
          (let ((case-fold-search t)) (string-match "regexp" message))
        (let* ((match (match-string 0 message))
               (properties (text-properties-at 0 match))
               (replacement (apply #'propertize "PCRE regexp" properties))
               (new-message (replace-match replacement t t message)))
          (setq ad-return-value new-message))))))

(defadvice isearch-fallback
  (before pcre-mode (want-backslash &optional allow-invalid to-barrier) disable)
  "Hack to fall back correctly in `pcre-mode'. "
  ;; A dirty hack to the internals of isearch.  Falling back to a
  ;; previous match position is necessary when the (Emacs) regexp ends
  ;; in "*", "?", "\{" or "\|": this is handled in
  ;; `isearch-process-search-char' by calling `isearch-fallback' with
  ;; `t' for the value of the first parameter, `want-backslash', in
  ;; the last two cases.  With PCRE regexps, falling back should take
  ;; place on "*", "?", "{" or "|", with no backslashes required.
  ;; This advice handles the last two cases by unconditionally setting
  ;; `want-backslash' to nil.
  (ad-set-arg 0 nil))

(defadvice isearch-edit-string
    (around pcre-mode disable)
  "Add PCRE mode-toggling keys to Isearch minibuffer in regexp mode."
  (if isearch-regexp
      (minibuffer-with-setup-hook
          #'rxt--read-pcre-mode
        ad-do-it)
    ad-do-it))

;;; evil-mode advice
(defadvice evil-search-function
    (around pcre-mode (forward regexp-p wrap) disable)
  (if (and regexp-p (not isearch-mode))
      (let ((real-search-function ad-do-it))
        (setq ad-return-value
              (pcre-decorate-search-function real-search-function)))
    ad-do-it))

(eval-after-load 'evil
  '(when pcre-mode
    (ad-enable-advice 'evil-search-function 'around 'pcre-mode)
    (ad-activate 'evil-search-function)))

(defun pcre-decorate-search-function (real-search-function)
  (lambda (string &optional bound noerror count)
    (funcall real-search-function
             (pcre-to-elisp/cached string)
             bound noerror count)))

;;; Other hooks and defadvices

;;;###autoload
(defun pcre-query-replace-regexp ()
  "Perform `query-replace-regexp' using PCRE syntax.

Consider using `pcre-mode' instead of this function."
  (interactive)
  (let ((old-pcre-mode pcre-mode))
    (unwind-protect
        (progn
          (pcre-mode +1)
          (call-interactively #'query-replace-regexp))
      (pcre-mode (if old-pcre-mode 1 0)))))


(defadvice add-to-history
  (before pcre-mode (history-var newelt &optional maxelt keep-all) disable)
  "Add the original PCRE to query-replace history in `pcre-mode'."
  (when (eq history-var query-replace-from-history-variable)
    (let ((original (gethash newelt pcre-mode-reverse-cache)))
      (when original
        (ad-set-arg 1 original)))))

(defadvice query-replace-descr
  (before pcre-mode (from) disable)
  "Use the original PCRE in Isearch prompts in `pcre-mode'."
  (let ((original (gethash from pcre-mode-reverse-cache)))
    (when original
      (ad-set-arg 0 original))))

;;; The `interactive' specs of the following functions are lifted
;;; wholesale from the original built-ins, which see.
(defadvice read-regexp
  (around pcre-mode first (prompt &optional defaults history) disable)
  "Read regexp using PCRE syntax and convert to Elisp equivalent."
  (ad-set-arg 0 (concat "[PCRE] " prompt))
  (minibuffer-with-setup-hook
      #'rxt--read-pcre-mode
    ad-do-it)
  (setq ad-return-value
        (pcre-to-elisp/cached ad-return-value)))

(defadvice align-regexp
  (before pcre-mode first (beg end regexp &optional group spacing repeat) disable)
  "Read regexp using PCRE syntax and convert to Elisp equivalent."
  (interactive
   (append
    (list (region-beginning) (region-end))
    (if current-prefix-arg
        (list (rxt-pcre-to-elisp
               (read-string "Complex align using PCRE regexp: "
                            "(\\s*)"))
              (string-to-number
               (read-string
                "Parenthesis group to modify (justify if negative): " "1"))
              (string-to-number
               (read-string "Amount of spacing (or column if negative): "
                            (number-to-string align-default-spacing)))
              (y-or-n-p "Repeat throughout line? "))
      (list (concat "\\(\\s-*\\)"
                    (rxt-pcre-to-elisp
                     (read-string "Align PCRE regexp: ")))
            1 align-default-spacing nil)))))

(defadvice ibuffer-do-replace-regexp
  (before pcre-mode first (from-str to-str) disable)
  "Read regexp using PCRE syntax and convert to Elisp equivalent."
  (interactive
   (let* ((from-str (read-from-minibuffer "[PCRE] Replace regexp: "))
          (to-str (read-from-minibuffer (concat "[PCRE] Replace " from-str " with: "))))
     (list (rxt-pcre-to-elisp from-str) to-str))))

(defadvice find-tag-regexp
  (before pcre-mode first (regexp &optional next-p other-window) disable)
  "Read regexp using PCRE syntax and convert to Elisp equivalent.
Perform `find-tag-regexp' using emulated PCRE regexp syntax."
  (interactive
   (let ((args (find-tag-interactive "[PCRE] Find tag regexp: " t)))
     (list (rxt-pcre-to-elisp (nth 0 args))
           (nth 1 args) (nth 2 args)))))

(defadvice sort-regexp-fields
  (before pcre-mode first (reverse record-regexp key-regexp beg end) disable)
  "Read regexp using PCRE syntax and convert to Elisp equivalent."
  (interactive "P\nsPCRE regexp specifying records to sort: \n\
sPCRE regexp specifying key within record: \nr")
  (ad-set-arg 1 (rxt-pcre-to-elisp (ad-get-arg 1)))
  (ad-set-arg 2 (rxt-pcre-to-elisp (ad-get-arg 2))))



;;; Commands that take Emacs-style regexps as input

;;;###autoload
(defun rxt-elisp-to-pcre (regexp)
  "Translate REGEXP, a regexp in Emacs Lisp syntax, to Perl-compatible syntax.

Interactively, reads the regexp in one of three ways. With a
prefix arg, reads from minibuffer without string escaping, like
`query-replace-regexp'. Without a prefix arg, uses the text of
the region if it is active. Otherwise, uses the result of
evaluating the sexp before point (which might be a string regexp
literal or an expression that produces a string).

Displays the translated PCRE regexp in the echo area and copies
it to the kill ring.

Emacs regexp features such as syntax classes which cannot be
translated to PCRE will cause an error."
  (interactive (rxt-interactive/elisp))
  (rxt-return-pcre (rxt-adt->pcre (rxt-parse-elisp regexp))))

;;;###autoload
(defun rxt-elisp-to-rx (regexp)
  "Translate REGEXP, a regexp in Emacs Lisp syntax, to `rx' syntax.

See `rxt-elisp-to-pcre' for a description of the interactive
behavior and `rx' for documentation of the S-expression based
regexp syntax."
  (interactive (rxt-interactive/elisp))
  (rxt-return-sexp (rxt-adt->rx (rxt-parse-elisp regexp))))

;;;###autoload
(defun rxt-elisp-to-strings (regexp)
  "Return a list of all strings matched by REGEXP, an Emacs Lisp regexp.

See `rxt-elisp-to-pcre' for a description of the interactive behavior.

This is useful primarily for getting back the original list of
strings from a regexp generated by `regexp-opt', but it will work
with any regexp without unbounded quantifiers (*, +, {2, } and so
on).

Throws an error if REGEXP contains any infinite quantifiers."
  (interactive (rxt-interactive/elisp))
  (rxt-return-sexp (rxt-adt->strings (rxt-parse-elisp regexp))))

;;;###autoload
(defun rxt-toggle-elisp-rx ()
  "Toggle the regexp near point between Elisp string and rx syntax."
  (interactive)
  ;; First, position point before the regex form near point (either
  ;; a string literal or a list beginning `rx' or `rx-to-string').
  (let* ((context (syntax-ppss))
         (string-start (nth 8 context)))
    (cond (string-start (goto-char string-start))
          ((looking-back "\"" nil t) (backward-sexp))
          ((looking-at "\"") nil)
          (t
           ;; Search backwards, leaving point in place on error
           (goto-char
            (save-excursion
              (skip-syntax-forward "-")
              (while (not (looking-at
                           (rx "(" (or "rx" "rx-to-string") symbol-end)))
                (backward-up-list))
              (point))))))

  ;; Read and replace the regex following point
  (let* ((regex (read (current-buffer)))
         (print-escape-newlines t))
    (save-excursion
      (if (listp regex)
          ;; Replace rx form with string value
          (prin1 (eval regex) (current-buffer))
        ;; Pretty-print rx form
        (save-restriction
          (let* ((start (point))
                 (rx--syntax-codes (rxt-elisp-to-rx regex))
                 (rx--form
                  (pcase rx--syntax-codes
                    (`(seq . ,rest) `(rx . ,rest))
                    (form           `(rx ,form)))))
            (rxt-print rx--form)
            (narrow-to-region start (point)))
          (pp-buffer)
          ;; remove the extra newline that pp-buffer inserts
          (goto-char (point-max))
          (delete-region
           (point)
           (save-excursion (skip-chars-backward " \t\n") (point))))))
    (kill-sexp -1)
    (indent-pp-sexp)))



;;; Commands that translate PCRE to other formats

;;;###autoload
(defun rxt-pcre-to-elisp (pcre &optional flags)
  "Translate PCRE, a regexp in Perl-compatible syntax, to Emacs Lisp.

Interactively, uses the contents of the region if it is active,
otherwise reads from the minibuffer. Prints the Emacs translation
in the echo area and copies it to the kill ring.

PCRE regexp features that cannot be translated into Emacs syntax
will cause an error. See the commentary section of pcre2el.el for
more details."
  (interactive (rxt-interactive/pcre))
  (rxt-return-emacs-regexp
   (rx-to-string
    (rxt-pcre-to-rx (rxt--add-flags pcre flags))
    t)))

;;;###autoload
(defalias 'pcre-to-elisp 'rxt-pcre-to-elisp)

;;;###autoload
(defun rxt-pcre-to-rx (pcre &optional flags)
  "Translate PCRE, a regexp in Perl-compatible syntax, to `rx' syntax.

See `rxt-pcre-to-elisp' for a description of the interactive behavior."
  (interactive (rxt-interactive/pcre))
  (rxt-return-sexp (rxt-adt->rx (rxt-parse-pcre (rxt--add-flags pcre flags)))))

;;;###autoload
(defun rxt-pcre-to-strings (pcre &optional flags)
  "Return a list of all strings matched by PCRE, a Perl-compatible regexp.

See `rxt-elisp-to-pcre' for a description of the interactive
behavior and `rxt-elisp-to-strings' for why this might be useful.

Throws an error if PCRE contains any infinite quantifiers."
  (interactive (rxt-interactive/pcre))
  (rxt-return-sexp (rxt-adt->strings (rxt-parse-pcre (rxt--add-flags pcre flags)))))

(defun rxt--add-flags (pcre flags)
  "Prepend FLAGS to PCRE."
  (if (not (zerop (length flags)))
      (concat "(?" flags ")" pcre)
    pcre))


;;; Regexp explaining functions to display pretty-printed rx syntax

;; When the `rxt-explain' flag is non-nil, `rxt-adt->rx' records
;; location information for each element of the generated `rx' form,
;; allowing highlighting corresponding pieces of syntax at point.
(defvar rxt-explain nil)

(defvar rxt-highlight-overlays nil
  "List of active location-highlighting overlays in rxt-help-mode buffer.")

;;;###autoload
(defun rxt-explain-elisp (regexp)
  "Insert the pretty-printed `rx' syntax for REGEXP in a new buffer.

REGEXP is a regular expression in Emacs Lisp syntax. See
`rxt-elisp-to-pcre' for a description of how REGEXP is read
interactively."
  (interactive (rxt-interactive/elisp))
  (let ((rxt-explain t)
        (rxt-verbose-rx-translation rxt-explain-verbosely))
    (rxt-pp-rx regexp (rxt-elisp-to-rx regexp))))

;;;###autoload
(defun rxt-explain-pcre (regexp &optional flags)
  "Insert the pretty-printed `rx' syntax for REGEXP in a new buffer.

REGEXP is a regular expression in PCRE syntax. See
`rxt-pcre-to-elisp' for a description of how REGEXP is read
interactively."
  (interactive (rxt-interactive/pcre))
  (let ((rxt-explain t)
        (rxt-verbose-rx-translation rxt-explain-verbosely))
    (rxt-pp-rx regexp (rxt-pcre-to-rx regexp flags))))

;;;###autoload
(defun rxt-quote-pcre (text)
  "Return a PCRE regexp which matches TEXT literally.

Any PCRE metacharacters in TEXT will be quoted with a backslash."
  (rxt-adt->pcre (rxt-string text)))


;;;; Commands that depend on the major mode in effect

;; Macro: interactively call one of two functions depending on the
;; major mode
(defmacro rxt-mode-dispatch (elisp-function pcre-function)
  `(if (memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
       (call-interactively #',elisp-function)
     (call-interactively #',pcre-function)))

;;;###autoload
(defun rxt-explain ()
  "Pop up a buffer with pretty-printed `rx' syntax for the regex at point.

Chooses regex syntax to read based on current major mode, calling
`rxt-explain-elisp' if buffer is in `emacs-lisp-mode' or
`lisp-interaction-mode', or `rxt-explain-pcre' otherwise."
  (interactive)
  (rxt-mode-dispatch rxt-explain-elisp rxt-explain-pcre))

;;;###autoload
(defun rxt-convert-syntax ()
  "Convert regex at point to other kind of syntax, depending on major mode.

For buffers in `emacs-lisp-mode' or `lisp-interaction-mode',
calls `rxt-elisp-to-pcre' to convert to PCRE syntax. Otherwise,
calls `rxt-pcre-to-elisp' to convert to Emacs syntax.

The converted syntax is displayed in the echo area and copied to
the kill ring; see the two functions named above for details."
  (interactive)
  (rxt-mode-dispatch rxt-elisp-to-pcre rxt-pcre-to-elisp))

;;;###autoload
(defun rxt-convert-to-rx ()
  "Convert regex at point to RX syntax. Chooses Emacs or PCRE syntax by major mode."
  (interactive)
  (rxt-mode-dispatch rxt-elisp-to-rx rxt-pcre-to-rx))

;;;###autoload
(defun rxt-convert-to-strings ()
  "Convert regex at point to RX syntax. Chooses Emacs or PCRE syntax by major mode."
  (interactive)
  (rxt-mode-dispatch rxt-elisp-to-strings rxt-pcre-to-strings))



;;; Minor mode and keybindings

(defvar rxt-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Generic
    (define-key map (kbd "C-c / /") 'rxt-explain)
    (define-key map (kbd "C-c / c") 'rxt-convert-syntax)
    (define-key map (kbd "C-c / x") 'rxt-convert-to-rx)
    (define-key map (kbd "C-c / '") 'rxt-convert-to-strings)

    ;; From PCRE
    (define-key map (kbd "C-c / p /") 'rxt-explain-pcre)
    (define-key map (kbd "C-c / p e") 'rxt-pcre-to-elisp)
    (define-key map (kbd "C-c / p x") 'rxt-pcre-to-rx)
    (define-key map (kbd "C-c / p '") 'rxt-pcre-to-strings)

    ;; From Elisp
    (define-key map (kbd "C-c / e /") 'rxt-explain-elisp)
    (define-key map (kbd "C-c / e p") 'rxt-elisp-to-pcre)
    (define-key map (kbd "C-c / e x") 'rxt-elisp-to-rx)
    (define-key map (kbd "C-c / e '") 'rxt-elisp-to-strings)
    (define-key map (kbd "C-c / e t") 'rxt-toggle-elisp-rx)
    (define-key map (kbd "C-c / t") 'rxt-toggle-elisp-rx)
    
    ;; Search
    (define-key map (kbd "C-c / %") 'pcre-query-replace-regexp)

    map)
  "Keymap for `rxt-mode'.")

;;;###autoload
(define-minor-mode rxt-mode
  "Regex translation utilities."
  :init-value nil
  :lighter nil
  :keymap rxt-mode-map)

;;;###autoload
(defun turn-on-rxt-mode ()
  "Turn on `rxt-mode' in the current buffer."
  (interactive)
  (rxt-mode 1))

;;;###autoload
(define-globalized-minor-mode rxt-global-mode rxt-mode
  turn-on-rxt-mode)


;;;; Syntax explanations

;; Major mode for displaying pretty-printed S-exp syntax
(define-derived-mode rxt-help-mode emacs-lisp-mode "Regexp Explain"
  (setq buffer-read-only t)
  (add-hook 'post-command-hook 'rxt-highlight-text nil t)
  (rxt-highlight-text))

;; Hack: stop paredit-mode interfering with `rxt-print'
(eval-when-compile (declare-function paredit-mode "paredit.el"))
(add-hook 'rxt-help-mode-hook
          (lambda ()
            (if (and (boundp 'paredit-mode)
                     paredit-mode)
                (paredit-mode 0))))

(define-key rxt-help-mode-map "q" 'quit-window)
(define-key rxt-help-mode-map "z" 'kill-this-buffer)
(define-key rxt-help-mode-map "n" 'next-line)
(define-key rxt-help-mode-map "p" 'previous-line)
(define-key rxt-help-mode-map "f" 'forward-list)
(define-key rxt-help-mode-map "b" 'backward-list)
(define-key rxt-help-mode-map "u" 'backward-up-list)
(define-key rxt-help-mode-map "d" 'down-list)

(defvar rxt--print-with-overlays nil)
(defvar rxt--print-depth 0)

(defconst rxt--print-char-alist
  '((?\a . "\\a")
    (?\b . "\\b")
    (?\t . "\\t")
    (?\n . "\\n")
    (?\v . "\\v")
    (?\f . "\\f")
    (?\r . "\\r")
    (?\e . "\\e")
    (?\s . "\\s")
    (?\\ . "\\\\")
    (?\d . "\\d"))
  "Alist of characters to print using an escape sequence in Elisp source.
See (info \"(elisp) Basic Char Syntax\").")

(defconst rxt--whitespace-display-regexp
  (rx-to-string `(any ,@(mapcar #'car rxt--print-char-alist))))

(defconst rxt--print-special-chars
  '(?\( ?\) ?\\ ?\| ?\; ?\' ?\` ?\" ?\# ?\. ?\,)
  "Characters which require a preceding backslash in Elisp source.
See (info \"(elisp) Basic Char Syntax\").")

(defun rxt-pp-rx (regexp rx)
  "Display string regexp REGEXP with its `rx' form RX in an `rxt-help-mode' buffer."
  (with-current-buffer (get-buffer-create "* Regexp Explain *")
    (let ((print-escape-newlines t)
          (inhibit-read-only t))
      (erase-buffer)
      (rxt-help-mode)
      (insert (rxt--propertize-whitespace regexp))
      (newline 2)
      (save-excursion
        (let ((sexp-begin (point))
              (rxt--print-with-overlays t))
          (rxt-print rx)
          (narrow-to-region sexp-begin (point))
          (pp-buffer)
          (widen)))
      (rxt-highlight-text))
    (pop-to-buffer (current-buffer))))

(cl-defun rxt-print (rx)
  "Insert RX, an `rx' form, into the current buffer, optionally adding overlays.

Similar to `print' or `prin1', but ensures that `rx' forms are
printed readably, using character or integer syntax depending on
context.

If `rxt--print-with-overlays' is non-nil, also creates overlays linking
elements of RX to their corresponding locations in the source
string (see `rxt-explain-elisp', `rxt-explain-pcre' and
`rxt--make-help-overlays')."
  (let ((start (point)))
    (cl-typecase rx
      (cons
       (pcase rx
         (`(,(and (or `repeat `**) head)
             ,(and (pred integerp) from)
             ,(and (pred integerp) to)
             . ,rest)
           (insert (format "(%s %d %d" head from to))
           (rxt--print-list-tail rest))
         (`(,(and (or `repeat `= `>=) head)
             ,(and (pred integerp) n)
             . ,rest)
           (insert (format "(%s %d" head n))
           (rxt--print-list-tail rest))
         (_
          (rxt--print-list-tail rx t))))
      (symbol
       (cl-case rx
         ;; `print' escapes the ? characters in the rx operators *?
         ;; and +?, but this looks bad and is not strictly necessary:
         ;; (eq (read "*?") (read "*\\?"))     => t
         ;; (eq (read "+?") (read "+\\?"))     => t
         ((*? +?) (insert (symbol-name rx)))
         (t (prin1 rx (current-buffer)))))
      (string
       (insert (rxt--propertize-whitespace (prin1-to-string rx))))
      (character
       (cond
         ((eq ?  rx)
          (insert "?"))
         ((memq rx rxt--print-special-chars)
          (insert "?\\" rx))
         ((assq rx rxt--print-char-alist)
          (insert "?" (assoc-default rx rxt--print-char-alist)))
         (t
          (insert "?" (char-to-string rx)))))
      (t
       (prin1 rx (current-buffer))))
    (when rxt--print-with-overlays
      (rxt--make-help-overlays rx start (point)))))

(defun rxt--print-list-tail (tail &optional open-paren)
  (let ((rxt--print-depth (1+ rxt--print-depth)))
    (let ((done nil))
      (while (not done)
        (cl-typecase tail
          (null
           (insert ")")
           (setq done t))
          (cons
           (if open-paren
               (progn
                 (insert "(")
                 (setq open-paren nil))
             (insert " "))
           (rxt-print (car tail))
           (setq tail (cdr tail)))
          (t
           (insert " . ")
           (rxt-print tail)
           (insert ")")
           (setq done t)))))))

(defun rxt--make-help-overlays (rx start end)
  (let ((location (rxt-location rx)))
    (when (and location
               (rxt-location-start location)
               (rxt-location-end location))
      (let* ((sexp-begin (copy-marker start t))
             (sexp-end (copy-marker end))
             (sexp-bounds (list sexp-begin sexp-end))

             (source-begin (1+ (rxt-location-start location)))
             (source-end   (1+ (rxt-location-end   location)))
             (source-bounds (list source-begin source-end))

             (bounds (list source-bounds sexp-bounds))

             (sexp-ol (make-overlay sexp-begin sexp-end (current-buffer) t nil))
             (source-ol (make-overlay source-begin source-end (current-buffer) t nil)))
        (dolist (ol (list sexp-ol source-ol))
          (overlay-put ol 'priority rxt--print-depth)
          (overlay-put ol 'rxt-bounds bounds))))))

(defun rxt--propertize-whitespace (string)
  (let ((string (copy-sequence string))
        (start 0))
    (while (string-match rxt--whitespace-display-regexp string start)
      (put-text-property (match-beginning 0) (match-end 0)
                         'display
                         (assoc-default (string-to-char (match-string 0 string))
                                        rxt--print-char-alist)
                         string)
      (setq start (match-end 0)))
    string))

(defun rxt-highlight-text ()
  "Highlight the regex syntax at point and its corresponding RX/string form."
  (let ((all-bounds (get-char-property (point) 'rxt-bounds)))
    (mapc #'delete-overlay rxt-highlight-overlays)
    (setq rxt-highlight-overlays nil)
    (dolist (bounds all-bounds)
      (cl-destructuring-bind (begin end) bounds
        (let ((overlay (make-overlay begin end)))
          (push overlay rxt-highlight-overlays)
          (overlay-put overlay 'face 'rxt-highlight-face))))))


;;;; Error handling

(if (fboundp 'define-error)
    (define-error 'rxt-invalid-regexp "Invalid regexp" 'invalid-regexp)
  (put 'rxt-invalid-regexp
         'error-conditions
         '(rxt-invalid-regexp invalid-regexp error))
  (put 'rxt-invalid-regexp 'error-message "Invalid regexp"))

(defun rxt-error (&rest format-args)
  (signal 'rxt-invalid-regexp (list (apply #'format format-args))))


;;;; Regexp syntax tree data type

;; Base class from which other elements of the syntax-tree inherit
(cl-defstruct rxt-syntax-tree)

;; Struct representing the original source location
(cl-defstruct rxt-location
  source                                ; Either a string or a buffer
  start end                             ; Offsets, 0- or 1-indexed as appropriate
  )

(defun rxt-location-text (location)
  (if (not (rxt-location-p location))
      nil
    (let ((start  (rxt-location-start  location))
          (end    (rxt-location-end    location))
          (source (rxt-location-source location)))
      (cond
        ((buffer-live-p source)
         (with-current-buffer source
           (buffer-substring-no-properties start end)))
        ((stringp source)
         (substring source start end))
        (t nil)))))

;; Hash table mapping from syntax-tree elements to source locations.
(defvar rxt-location-map (make-hash-table :weakness 'key))

(defun rxt-location (object)
  (gethash object rxt-location-map))

(gv-define-setter rxt-location (value object)
  `(puthash ,object ,value rxt-location-map))

(defun rxt-source-text (object)
  (rxt-location-text (rxt-location object)))

(defun rxt-to-string (tree)
  "Return a readable representation of TREE, a regex syntax-tree object."
  (or (rxt-source-text tree)
      (let ((print-level 1))
        (prin1-to-string tree))))
(defalias 'rxt-syntax-tree-readable 'rxt-to-string)

;; FIXME
(defvar rxt-pcre-case-fold nil)

;; Literal string
(cl-defstruct
    (rxt-string
      (:constructor rxt-string (chars &optional case-fold))
      (:include rxt-syntax-tree))
  chars
  (case-fold rxt-pcre-case-fold))

(defun rxt-empty-string ()
  (rxt-string ""))

(defun rxt-trivial-p (re)
  (and (rxt-string-p re)
       (equal (rxt-string-chars re) "")))

;;; Other primitives
(cl-defstruct (rxt-primitive
               (:constructor rxt-primitive (pcre rx))
               (:include rxt-syntax-tree))
  pcre rx)

(defun rxt-bos () (rxt-primitive "\\A" 'bos))
(defun rxt-eos () (rxt-primitive "\\Z" 'eos))

(defun rxt-bol () (rxt-primitive "^" 'bol))
(defun rxt-eol () (rxt-primitive "$" 'eol))

;; FIXME
(defun rxt-anything () (rxt-primitive "." 'anything))
(defun rxt-nonl () (rxt-primitive "." 'nonl))

(defun rxt-word-boundary () (rxt-primitive "\\b" 'word-boundary))
(defun rxt-not-word-boundary () (rxt-primitive "\\B" 'not-word-boundary))

(defun rxt-wordchar () (rxt-primitive "\\w" 'wordchar))
(defun rxt-not-wordchar () (rxt-primitive "\\W" 'not-wordchar))

(defun rxt-symbol-start () (rxt-primitive nil 'symbol-start))
(defun rxt-symbol-end () (rxt-primitive nil 'symbol-end))

(defun rxt-bow () (rxt-primitive nil 'bow))
(defun rxt-eow () (rxt-primitive nil 'eow))

;;; Sequence
(cl-defstruct
    (rxt-seq
     (:constructor make-rxt-seq (elts))
     (:include rxt-syntax-tree))
  elts)

;; Slightly smart sequence constructor:
;; - Flattens nested sequences
;; - Drops trivial "" elements
;; - Empty sequence => ""
;; - Singleton sequence is reduced to its one element.
(defun rxt-seq (&rest res)        ; Flatten nested seqs & drop ""'s.
  (let ((res (rxt-seq-flatten res)))
    (if (consp res)
        (if (consp (cdr res))
            (make-rxt-seq res) ; General case
          (car res))           ; Singleton sequence
      (rxt-empty-string))))      ; Empty seq -- ""

(defun rxt-seq-flatten (res)
  (if (consp res)
      (let ((re (car res))
            (tail (rxt-seq-flatten (cdr res))))
        (cond ((rxt-seq-p re)           ; Flatten nested seqs
               (append (rxt-seq-flatten (rxt-seq-elts re)) tail))
              ((rxt-trivial-p re) tail) ; Drop trivial elts
              ((and (rxt-string-p re)   ; Flatten strings
                    (consp tail)
                    (rxt-string-p (car tail)))
               (cons
                (rxt-string-concat re (car tail))
                (cdr tail)))
              (t (cons re tail))))
    '()))

(defun rxt-string-concat (str1 str2)
  (if (not (eq (rxt-string-case-fold str1)
               (rxt-string-case-fold str2)))
      (make-rxt-seq (list str1 str2))
    (let ((result
           (rxt-string (concat (rxt-string-chars str1)
                               (rxt-string-chars str2))
                       (rxt-string-case-fold str1)))
          (first (rxt-location str1))
          (last  (rxt-location str2)))
      (when (and first last)
        (setf (rxt-location result)
              (make-rxt-location :source (rxt-location-source first)
                                 :start  (rxt-location-start first)
                                 :end    (rxt-location-end last))))
      result)))

;;; Choice (alternation/union)
(cl-defstruct
    (rxt-choice
     (:constructor make-rxt-choice (elts))
     (:include rxt-syntax-tree))
  elts)

;;; The empty choice represents a regexp that never matches in any context
(defvar rxt-empty (make-rxt-choice nil))
(defun rxt-empty-p (re)
  (or
   (and (rxt-choice-p re)
        (null (rxt-choice-elts re)))
   (rxt-empty-char-set-p re)))

(defun rxt-choice (&rest alternatives)
  "Construct the alternation (union) of several regexps.

ALTERNATIVES should be a list of `rxt-syntax-tree' objects.
The return value is an `rxt-choice' object representing a regexp
which matches any one of ALTERNATIVES, but simplified in the
following ways:

- If ALTERNATIVES contains only one element, it is returned unchanged.

- All existing `rxt-choice' elements in ALTERNATIVES are replaced
  by a flat list of their subexpressions: symbolically,
  a|(b|(c|d)) is replaced by a|b|c|d

- All character sets and single-character strings in ALTERNATIVES
  are combined together into one or two character sets,
  respecting case-folding behaviour."
  (cl-destructuring-bind (other-elements char-set case-fold-char-set)
      (rxt--simplify-alternatives alternatives)
    (let ((simplified-alternatives
           (append (if (not (rxt-empty-p char-set))
                       (list char-set)
                     '())
                   (if (not (rxt-empty-p case-fold-char-set))
                       (list case-fold-char-set)
                     '())
                   other-elements)))
      (pcase simplified-alternatives
        (`()
          rxt-empty)
        (`(,element)
          element)
        (_
         (make-rxt-choice simplified-alternatives))))))

(defun rxt--simplify-alternatives (alternatives)
  "Simplify a set of regexp alternatives.

ALTERNATIVES should be a list of `rxt-syntax-tree' objects to be combined
into an `rxt-choice' structure.  The result is a three-element
list (OTHER-ELEMENTS CHAR-SET CASE-FOLDED-CHAR-SET):

- CHAR-SET is an `rxt-char-set-union' containing the union of all
  case-sensitive character sets and single-character strings in
  RES.

- CASE-FOLDED-CHAR-SET is similar but combines all the
  case-insensitive character sets and single-character strings.

- OTHER-ELEMENTS is a list of all other elements, with all
  `rxt-choice' structures replaced by a flat list of their
  component subexpressions."
  (if (null alternatives)
      (list '()
            (make-rxt-char-set-union :case-fold nil)
            (make-rxt-char-set-union :case-fold t))
    (let* ((re (car alternatives)))
      (cl-destructuring-bind (tail char-set case-fold-char-set)
          (rxt--simplify-alternatives (cdr alternatives))
        (cond ((rxt-choice-p re)         ; Flatten nested choices
               (list
                (append (rxt-choice-elts re) tail)
                char-set
                case-fold-char-set))

              ((rxt-empty-p re)          ; Drop empty re's.
               (list tail char-set case-fold-char-set))

              ((rxt-char-set-union-p re) ; Fold char sets together
               (if (rxt-char-set-union-case-fold re)
                   (list tail
                         char-set
                         (rxt-char-set-union case-fold-char-set re))
                 (list tail
                       (rxt-char-set-union char-set re)
                       case-fold-char-set)))

              ((and (rxt-string-p re)    ; Same for 1-char strings
                    (= 1 (length (rxt-string-chars re))))
               (if (rxt-string-case-fold re)
                   (list tail
                         char-set
                         (rxt-char-set-union case-fold-char-set re))
                 (list tail
                       (rxt-char-set-union char-set re)
                       case-fold-char-set)))

              (t                         ; Otherwise.
               (list (cons re tail) char-set case-fold-char-set)))))))

;;; Repetition
(cl-defstruct (rxt-repeat
               (:include rxt-syntax-tree))
  from to body greedy)

(cl-defun rxt-repeat (from to body &optional (greedy t))
  (if (equal to 0)
      (rxt-empty-string)
    (make-rxt-repeat :from from :to to
                     :body body :greedy greedy)))

;;; Submatch
(cl-defstruct
    (rxt-submatch
     (:constructor rxt-submatch (body))
     (:include rxt-syntax-tree))
  body)

;;; Numbered submatch (Emacs only)
(cl-defstruct
    (rxt-submatch-numbered
     (:constructor rxt-submatch-numbered (n body))
     (:include rxt-syntax-tree))
  n
  body)

;;; Backreference
(cl-defstruct
    (rxt-backref
     (:constructor rxt-backref (n))
     (:include rxt-syntax-tree))
  n)

;;; Syntax classes (Emacs only)
(cl-defstruct (rxt-syntax-class
               (:include rxt-syntax-tree))
  symbol)

(defun rxt-syntax-class (symbol)
  (if (assoc symbol rx--syntax-codes)
      (make-rxt-syntax-class :symbol symbol)
    (rxt-error "Invalid syntax class symbol `%s'" symbol)))

;;; Character categories (Emacs only)
(cl-defstruct (rxt-char-category
               (:include rxt-syntax-tree))
  symbol)

(defun rxt-char-category (symbol)
  (if (assoc symbol rx--categories)
      (make-rxt-char-category :symbol symbol)
    (rxt-error "Invalid character category symbol `%s'" symbol)))


;;; Char sets
;; <rxt-char-set> ::= <rxt-char-set-union>
;;                  | <rxt-char-set-negation>
;;                  | <rxt-char-set-intersection>

(cl-defstruct (rxt-char-set (:include rxt-syntax-tree)))

;; An rxt-char-set-union represents the union of any number of
;; characters, character ranges, and POSIX character classes: anything
;; that can be represented in string notation as a class [ ... ]
;; without the negation operator.
(cl-defstruct (rxt-char-set-union
                (:include rxt-char-set))
  chars    ; list of single characters
  ranges   ; list of ranges (from . to)
  classes  ; list of character classes
  (case-fold rxt-pcre-case-fold))

;; Test for empty character set
(defun rxt-empty-char-set-p (cset)
  (and (rxt-char-set-union-p cset)
       (null (rxt-char-set-union-chars cset))
       (null (rxt-char-set-union-ranges cset))
       (null (rxt-char-set-union-classes cset))))

;; Simple union constructor
(defun rxt-char-set-union (&rest items)
  "Construct an regexp character set representing the union of ITEMS.

Each element of ITEMS may be either: a character; a
single-character string; a single-character `rxt-string' object;
a cons, (FROM . TO) representing a range of characters; a symbol,
representing a named character class; or an `rxt-char-set-union'
object.  All `rxt-char-set-union' objects in ITEMS must have the
same `case-fold' property."
  (let ((chars '())
        (ranges '())
        (classes '())
        (case-fold 'undetermined))
    (dolist (item items)
      (cl-etypecase item
        (character
         (push item chars))

        (string
         (cl-assert (= 1 (length item)))
         (push (string-to-char item) chars))

        (rxt-string
         (cl-assert (= 1 (length (rxt-string-chars item))))
         (push (string-to-char (rxt-string-chars item)) chars))

        (cons                           ; range (from . to)
         (cl-check-type (car item) character)
         (cl-check-type (cdr item) character)
         (push item ranges))

        (symbol                         ; named character class
         (push item classes))

        (rxt-char-set-union
         (if (eq case-fold 'undetermined)
             (setq case-fold (rxt-char-set-union-case-fold item))
           (unless (eq case-fold (rxt-char-set-union-case-fold item))
             (error "Cannot construct union of char-sets with unlike case-fold setting: %S" item)))
         (setq chars (nconc chars (rxt-char-set-union-chars item)))
         (setq ranges (nconc ranges (rxt-char-set-union-ranges item)))
         (setq classes (nconc classes (rxt-char-set-union-classes item))))))

    (make-rxt-char-set-union :chars chars :ranges ranges :classes classes
                             :case-fold (if (eq case-fold 'undetermined)
                                            rxt-pcre-case-fold
                                          case-fold))))

(defun rxt--all-char-set-union-chars (char-set)
  "Return a list of all characters in CHAR-SET."
  (cl-assert (rxt-char-set-union-p char-set))
  (append
   (rxt-char-set-union-chars char-set)
   (cl-loop for (start . end) in (rxt-char-set-union-ranges char-set)
            nconc (cl-loop for char from start to end collect char))))

(defun rxt--simplify-char-set (char-set &optional case-fold-p)
  "Return a minimal char-set to match the same characters as CHAR-SET.

With optional argument CASE-FOLD-P, return a char-set which
emulates case-folding behaviour by including both uppercase and
lowercase versions of all characters in CHAR-SET."
  (cl-assert (rxt-char-set-union-p char-set))
  (let* ((classes (rxt-char-set-union-classes char-set))
         (all-chars
          (if case-fold-p
              (cl-loop for char in (rxt--all-char-set-union-chars char-set)
                       nconc (list (upcase char) (downcase char)))
            (rxt--all-char-set-union-chars char-set)))
         (all-ranges
          (rxt--extract-ranges (rxt--remove-redundant-chars all-chars classes))))
    (let ((singletons nil)
          (ranges nil))
      (cl-loop for (start . end) in all-ranges
               do
               (cond ((= start end) (push start singletons))
                     ((= (1+ start) end)
                      (push start singletons)
                      (push end singletons))
                     (t (push (cons start end) ranges))))
      (make-rxt-char-set-union :chars (nreverse singletons)
                               :ranges (nreverse ranges)
                               :classes classes
                               :case-fold (if case-fold-p
                                              nil
                                            (rxt-char-set-union-case-fold char-set))))))

(defun rxt--remove-redundant-chars (chars classes)
  "Remove all characters which match a character class in CLASSES from CHARS."
  (if (null classes)
      chars
    (string-to-list
     (replace-regexp-in-string
      (rx-to-string `(any ,@classes))
      ""
      (apply #'string chars)))))

(defun rxt--extract-ranges (chars)
  "Return a list of all contiguous ranges in CHARS.

CHARS should be a list of characters (integers).  The return
value is a list of conses (START . END) representing ranges, such
that the union of all the ranges represents the same of
characters as CHARS.

Example:
    (rxt--extract-ranges (list ?a ?b ?c ?q ?x ?y ?z))
        => ((?a . ?c) (?q . ?q) (?x . ?z))"
  (let ((array
         (apply #'vector
                (cl-remove-duplicates
                 (sort (copy-sequence chars) #'<)))))
    (cl-labels
        ((recur (start end)
           (if (< end start)
               nil
             (let ((min (aref array start))
                   (max (aref array end)))
               (if (= (- max min) (- end start))
                   (list (cons min max))
                 (let* ((split-point (/ (+ start end) 2))
                        (left (recur start split-point))
                        (right (recur (1+ split-point) end)))
                   (pcre2el-merge left right))))))
         (pcre2el-merge (left right)
           (cond ((null left) right)
                 ((null right) left)
                 (t
                  (let ((last-left (car (last left)))
                        (first-right (car right)))
                    (if (= (1+ (cdr last-left))
                           (car first-right))
                        (append (cl-subseq left 0 -1)
                                (list
                                 (cons (car last-left)
                                       (cdr first-right)))
                                (cl-subseq right 1))
                      (append left right)))))))
      (recur 0 (1- (length array))))))

;;; Set complement of character set, syntax class, or character
;;; category

;; In general, all character sets that can be represented in string
;; notation as [^ ... ] (but see `rxt-char-set-intersection', below), plus
;; Emacs' \Sx and \Cx constructions.
(cl-defstruct (rxt-char-set-negation
               (:include rxt-char-set))
  elt)

(defun rxt-negate (char-set)
  "Construct the logical complement (negation) of CHAR-SET.

CHAR-SET may be any of the following types: `rxt-char-set-union',
`rxt-syntax-class', `rxt-char-category', or `rxt-char-set-negation'."
  (cl-etypecase char-set
    ((or rxt-char-set-union rxt-syntax-class rxt-char-category)
     (make-rxt-char-set-negation :elt char-set))

    (rxt-char-set-negation
     (rxt-char-set-negation-elt char-set))))

;;; Intersections of char sets

;; These are difficult to represent in general, but can be constructed
;; in Perl using double negation; for example: [^\Wabc] means the set
;; complement of [abc] with respect to the universe of "word
;; characters": (& (~ (~ word)) (~ ("abc"))) == (& word (~ ("abc")))
;; == (- word ("abc"))

(cl-defstruct (rxt-char-set-intersection
               (:include rxt-char-set))
  elts)

;; Intersection constructor
(defun rxt-char-set-intersection (&rest charsets)
  (let ((elts '())
        (cmpl (make-rxt-char-set-union)))
    (dolist (cset (rxt-int-flatten charsets))
      (cond
       ((rxt-char-set-negation-p cset)
        ;; Fold negated charsets together: ~A & ~B = ~(A|B)
        (setq cmpl (rxt-char-set-union cmpl (rxt-char-set-negation-elt cset))))

       ((rxt-char-set-union-p cset)
        (push cset elts))

       (t
        (rxt-error "Can't take intersection of non-character-set %S" cset))))

    (if (null elts)
        (rxt-negate cmpl)
      (unless (rxt-empty-char-set-p cmpl)
        (push (rxt-negate cmpl) elts))
      (if (null (cdr elts))
          (car elts)      ; singleton case
        (make-rxt-char-set-intersection :elts elts)))))

;; Constructor helper: flatten nested intersections
(defun rxt-int-flatten (csets)
  (if (consp csets)
      (let ((cset (car csets))
            (tail (rxt-int-flatten (cdr csets))))
        (if (rxt-char-set-intersection-p cset)
            (append (rxt-int-flatten (rxt-char-set-intersection-elts cset)) tail)
          (cons cset tail)))
    '()))



;;;; Macros for building the parser

(defmacro rxt-token-case (&rest cases)
  "Consume a token at point and evaluate corresponding forms.

CASES is a list of `cond'-like clauses, (REGEXP BODY ...) where
the REGEXPs define possible tokens which may appear at point. The
CASES are considered in order. For each case, if the text at
point matches REGEXP, then point is moved to the end of the
matched token, the corresponding BODY is evaluated and their
value returned. The matched token is available within the BODY
forms as (match-string 0).

There can be a default case where REGEXP is `t', which evaluates
the corresponding FORMS but does not move point.

Returns `nil' if none of the CASES matches."
  (declare (debug (&rest (sexp &rest form))))
  `(cond
     ,@(cl-loop for (token . action) in cases
                collect
                (if (eq token t)
                    `(t ,@action)
                  `((looking-at ,token)
                    (goto-char (match-end 0))
                    ,@action)))))

(defmacro rxt-with-source-location (&rest body)
  "Evaluate BODY and record source location information on its value. 

BODY may evaluate to any kind of object, but its value should
generally not be `eq' to any other object."
  (declare (debug (&rest form)))
  (let ((begin (make-symbol "begin"))
        (value (make-symbol "value")))
    `(let ((,begin (point))
           (,value ,(macroexp-progn body)))
       (setf (rxt-location ,value)
             (make-rxt-location :source rxt-source-text-string
                                :start  (1- ,begin)
                                :end    (1- (point))))
       ,value)))

;; Read PCRE + flags
(defun rxt-read-delimited-pcre ()
  "Read a Perl-style delimited regexp and flags from the current buffer.

Point should be before the regexp literal before calling
this. Currently only regexps delimited by / ... / are supported.
A preceding \"m\", \"qr\" or \"s\" will be ignored, as will the
replacement string in an s/.../.../ construction.

Returns two strings: the regexp and the flags."
  (save-excursion
    (skip-syntax-forward "-")

    ;; Skip m, qr, s
    (let ((is-subst (rxt-token-case
                     ("s" t)
                     ((rx (or "m" "qr")) nil))))

      (when (not (looking-at "/"))
        (error "Only Perl regexps delimited by slashes are supported"))
      (let ((beg (match-end 0))
            (delim (rx (not (any "\\"))
                       (group "/"))))
        (search-forward-regexp delim)
        (let ((end (match-beginning 1)))
          (when is-subst (search-forward-regexp delim))
          (let ((pcre (buffer-substring-no-properties beg end)))
            (rxt-token-case
             ("[gimosx]*"
              (rxt--add-flags pcre (match-string-no-properties 0))))))))))


;;;; Elisp and PCRE string notation parser

;;; Parser constants
(defconst rxt-pcre-char-set-alist
  `((?w .                               ; "word" characters
        (?_ alnum))
    (?d .                               ; digits
        (digit))
    (?h .                               ; horizontal whitespace
        (#x0009 #x0020 #x00A0 #x1680 #x180E #x2000 #x2001 #x2002 #x2003
                #x2004 #x2005 #x2006 #x2007 #x2008 #x2009 #x200A #x202F
                #x205F #x3000))
    (?s .                               ; whitespace
        (9 10 12 13 32))
    (?v .                               ; vertical whitespace
        (#x000A #x000B #x000C #x000D #x0085 #x2028 #x2029))))

(defconst rxt-pcre-named-classes-regexp
  (rx "[:"
      (submatch
       (or "alnum" "alpha" "ascii" "blank" "cntrl" "digit" "graph" "lower"
           "print" "punct" "space" "upper" "word" "xdigit"))
      ":]"))

(defconst rxt-elisp-named-classes-regexp
  (rx "[:"
      (submatch
       (or "alnum" "alpha" "ascii" "blank" "cntrl" "digit" "graph" "lower"
           "print" "punct" "space" "upper" "word" "xdigit"
           "unibyte" "nonascii" "multibyte"))
      ":]"))

;;; The following dynamically bound variables control the operation of
;;; the parser (see `rxt-parse-re'.)
(defvar rxt-parse-pcre nil
  "t if the rxt string parser is parsing PCRE syntax, nil for Elisp syntax.

This should only be let-bound internally, never set otherwise.")

(defvar rxt-pcre-extended-mode nil
  "t if the rxt string parser is emulating PCRE's \"extended\" mode.

In extended mode (indicated by /x in Perl/PCRE), whitespace
outside of character classes and \\Q...\\E quoting is ignored,
and a `#' character introduces a comment that extends to the end
of line.")

(defvar rxt-pcre-s-mode nil
  "t if the rxt string parser is emulating PCRE's single-line \"/s\" mode.

When /s is used, PCRE's \".\" matches newline characters, which
otherwise it would not match.")

(defvar rxt-pcre-case-fold nil
  "non-nil to emulate PCRE's case-insensitive \"/i\" mode in translated regexps.")

(defvar rxt-branch-end-regexp nil)
(defvar rxt-choice-regexp nil)
(defvar rxt-brace-begin-regexp nil)
(defvar rxt-m-to-n-brace-regexp nil)
(defvar rxt-m-to-?-brace-regexp nil)
(defvar rxt-m-brace-regexp nil)
(defvar rxt-named-classes-regexp nil)

(defvar rxt-subgroup-count nil)
(defvar rxt-source-text-string nil)

(defun rxt-parse-pcre (re)
  (rxt-parse-re re t))

(defun rxt-parse-elisp (re)
  (rxt-parse-re re nil))

(defun rxt-parse-re (re pcre-p)
  (let* ((rxt-parse-pcre pcre-p)
         (rxt-pcre-extended-mode nil)
         (rxt-pcre-s-mode        nil)
         (rxt-pcre-case-fold     nil)

         ;; Bind regexps to match syntax that differs between PCRE and
         ;; Elisp only in the addition of a backslash "\"
         (escape (if pcre-p "" "\\"))
         (rxt-choice-regexp
          (rx-to-string `(seq ,escape "|")))
         (rxt-branch-end-regexp
          (rx-to-string `(or buffer-end
                             (seq ,escape (or "|" ")")))))
         (rxt-brace-begin-regexp
          (rx-to-string `(seq ,escape "{")))
         (rxt-m-to-n-brace-regexp
          (rx-to-string
           `(seq
             (submatch (* (any "0-9"))) "," (submatch (+ (any "0-9")))
             ,escape "}")))
         (rxt-m-to-?-brace-regexp
          (rx-to-string
           `(seq (submatch (+ (any "0-9"))) "," ,escape "}")))
         (rxt-m-brace-regexp
          (rx-to-string
           `(seq (submatch (+ (any "0-9"))) ,escape "}")))

         ;; Named character classes [: ... :] differ slightly
         (rxt-named-classes-regexp
          (if pcre-p
              rxt-pcre-named-classes-regexp
            rxt-elisp-named-classes-regexp))

         (rxt-subgroup-count 0)
         (case-fold-search nil))
    (with-temp-buffer
      (insert re)
      (goto-char (point-min))
      (let ((rxt-source-text-string re))
        (rxt-parse-exp)))))

;; Parse a complete regex: a number of branches separated by | or
;; \|, as determined by `rxt-branch-end-regexp'.
(defun rxt-parse-exp ()
  ;; These variables are let-bound here because in PCRE mode they may
  ;; be set internally by (?x) or (?s) constructions, whose scope
  ;; lasts until the end of a sub-expression
  (rxt-with-source-location
   (let ((rxt-pcre-extended-mode rxt-pcre-extended-mode)
         (rxt-pcre-s-mode rxt-pcre-s-mode)
         (rxt-pcre-case-fold rxt-pcre-case-fold))
     (if (eobp)
         (rxt-seq)
       (let ((branches '()))
         (cl-block nil
           (while t
             (let ((branch (rxt-parse-branch)))
               (push branch branches)
               (rxt-token-case
                (rxt-choice-regexp nil)
                (t (cl-return (apply #'rxt-choice (reverse branches)))))))))))))

;; Skip over whitespace and comments in PCRE extended mode
(defun rxt-extended-skip ()
  (when rxt-pcre-extended-mode
    (skip-syntax-forward "-")
    (while (looking-at "#")
      (beginning-of-line 2)
      (skip-syntax-forward "-"))))

;; Parse a regexp "branch": a sequence of pieces
(defun rxt-parse-branch ()
  (rxt-extended-skip)
  (rxt-with-source-location
   (let ((pieces '())
         (branch-start-p t))
     (while (not (looking-at rxt-branch-end-regexp))
       (push (rxt-parse-piece branch-start-p) pieces)
       (setq branch-start-p nil))
     (apply #'rxt-seq (reverse pieces)))))

;; Parse a regexp "piece": an atom (`rxt-parse-atom') plus any
;; following quantifiers
(defun rxt-parse-piece (&optional branch-begin)
  (rxt-extended-skip)
  (rxt-with-source-location
   (let ((atom (rxt-parse-atom branch-begin)))
     (rxt-parse-quantifiers atom))))

;; Parse any and all quantifiers after ATOM and return the quantified
;; regexp, or ATOM unchanged if no quantifiers
(defun rxt-parse-quantifiers (atom)
  (catch 'done
    (while (not (eobp))
      (let ((atom1 (rxt-parse-quantifier atom)))
        (if (eq atom1 atom)
            (throw 'done t)
          (setq atom atom1)))))
  atom)

;; Possibly parse a single quantifier after ATOM and return the
;; quantified atom, or ATOM if no quantifier
(defun rxt-parse-quantifier (atom)
  (rxt-extended-skip)
  (rxt-token-case
   ((rx "*?") (rxt-repeat 0 nil atom nil))
   ((rx "*")  (rxt-repeat 0 nil atom t))
   ((rx "+?") (rxt-repeat 1 nil atom nil))
   ((rx "+")  (rxt-repeat 1 nil atom t))
   ((rx "??") (rxt-repeat 0 1 atom nil))
   ((rx "?")  (rxt-repeat 0 1 atom t))
   ;; Brace expression "{M,N}", "{M,}", "{M}"
   (rxt-brace-begin-regexp
    (cl-destructuring-bind (from to)
        (rxt-parse-braces)
      (rxt-repeat from to atom)))
   ;; No quantifiers found
   (t atom)))

;; Parse a regexp atom, i.e. an element that binds to any following
;; quantifiers. This includes characters, character classes,
;; parenthesized groups, assertions, etc.
(defun rxt-parse-atom (&optional branch-begin)
  (if (eobp)
      (rxt-error "Unexpected end of regular expression")
    (if rxt-parse-pcre
        (rxt-parse-atom/pcre)
      (rxt-parse-atom/el branch-begin))))

(defun rxt-parse-atom/common ()
  (rxt-token-case
   ((rx "[")   (rxt-parse-char-class))
   ((rx "\\b") (rxt-word-boundary))
   ((rx "\\B") (rxt-not-word-boundary))))

(defun rxt-parse-atom/el (branch-begin)
  (rxt-with-source-location
   (or (rxt-parse-atom/common)
       (rxt-token-case
        ;; "." wildcard
        ((rx ".") (rxt-nonl))
        ;; "^" and "$" are metacharacters only at beginning or end of a
        ;; branch in Elisp; elsewhere they are literals
        ((rx "^")
         (if branch-begin
             (rxt-bol)
           (rxt-string "^")))
        ((rx "$")
         (if (looking-at rxt-branch-end-regexp)
             (rxt-eol)
           (rxt-string "$")))
        ;; Beginning & end of string, word, symbol
        ((rx "\\`") (rxt-bos))
        ((rx "\\'") (rxt-eos))
        ((rx "\\<") (rxt-bow))
        ((rx "\\>") (rxt-eow))
        ((rx "\\_<") (rxt-symbol-start))
        ((rx "\\_>") (rxt-symbol-end))
        ;; Subgroup
        ((rx "\\(") (rxt-parse-subgroup/el))
        ;; Word/non-word characters (meaning depending on syntax table)
        ((rx "\\w") (rxt-wordchar))
        ((rx "\\W") (rxt-not-wordchar))
        ;; Other syntax categories
        ((rx "\\" (submatch (any ?S ?s)) (submatch nonl))
         (let ((negated (string= (match-string 1) "S"))
               (syntax
                (car (rassoc (string-to-char (match-string 2))
                              rx--syntax-codes))))
           (if syntax
               (let ((re (rxt-syntax-class syntax)))
                 (if negated (rxt-negate re) re))
             (rxt-error "Invalid syntax class `\\%s'" (match-string 0)))))
        ;; Character categories
        ((rx "\\" (submatch (any ?C ?c)) (submatch nonl))
         (let ((negated (string= (match-string 1) "C"))
               (category
                (car (rassoc (string-to-char (match-string 2))
                             rx--categories))))
           (if category
               (let ((re (rxt-char-category category)))
                 (if negated (rxt-negate re) re))
             (rxt-error "Invalid character category `%s'" (match-string 0)))))
        ;; Backreference
        ((rx (seq "\\" (submatch (any "1-9"))))
         (rxt-backref (string-to-number (match-string 1))))
        ;; Other escaped characters
        ((rx (seq "\\" (submatch nonl)))
         (rxt-string (match-string 1)))
        ;; Normal characters
        ((rx (or "\n" nonl))
         (rxt-string (match-string 0)))))))

(defun rxt-parse-atom/pcre ()
  (rxt-extended-skip)
  (rxt-with-source-location
   (or
    ;; Is it an atom that's the same in Elisp?
    (rxt-parse-atom/common)
    ;; Is it common to PCRE regex and character class syntax?
    (let ((char (rxt-parse-escapes/pcre)))
      (and char
           (rxt-string (char-to-string char))))
    ;; Otherwise:
    (rxt-token-case
     ;; "." wildcard
     ((rx ".")
      (if rxt-pcre-s-mode
          (rxt-anything)
        (rxt-nonl)))
     ;; Beginning & end of string/line
     ((rx "^") (rxt-bol))
     ((rx "$") (rxt-eol))
     ((rx "\\A") (rxt-bos))
     ((rx "\\Z") (rxt-eos))
     ;; Subgroup
     ((rx "(") (rxt-parse-subgroup/pcre))
     ;; Metacharacter quoting
     ((rx "\\Q")
      ;; It would seem simple to take all the characters between \Q
      ;; and \E and make an rxt-string, but \Q...\E isn't an atom:
      ;; any quantifiers afterward should bind only to the last
      ;; character, not the whole string.
      (let ((begin (point)))
        (search-forward "\\E" nil t)
        (let* ((end (match-beginning 0))
               (str (buffer-substring-no-properties begin (1- end)))
               (char (char-to-string (char-before end))))
          (rxt-seq (rxt-string str)
                   (rxt-parse-quantifiers (rxt-string char))))))
     ;; Pre-defined character sets
     ((rx "\\" (submatch (any "d" "D" "h" "H" "s" "S" "v" "V" "w" "W")))
      (rxt--pcre-char-set (string-to-char (match-string 1))))
     ;; \ + digits: backreference or octal char?
     ((rx "\\" (submatch (+ (any "0-9"))))
      (let* ((digits (match-string 1))
             (dec (string-to-number digits)))
        ;; from "man pcrepattern": If the number is less than 10, or if
        ;; there have been at least that many previous capturing left
        ;; parentheses in the expression, the entire sequence is taken
        ;; as a back reference.
        (if (and (> dec 0)
                 (or (< dec 10)
                     (>= rxt-subgroup-count dec)))
            (progn
              (when rxt-pcre-case-fold
                (display-warning
                 'rxt "Backreferences with case-folding are handled poorly"))
              (rxt-backref dec))
          ;; from "man pcrepattern": if the decimal number is greater
          ;; than 9 and there have not been that many capturing
          ;; subpatterns, PCRE re-reads up to three octal digits
          ;; following the backslash, and uses them to generate a data
          ;; character. Any subsequent digits stand for themselves.
          (goto-char (match-beginning 1))
          (re-search-forward (rx (** 0 3 (any "0-7"))))
          (rxt-string (char-to-string (string-to-number (match-string 0) 8))))))
     ;; Other escaped characters
     ((rx "\\" (submatch nonl)) (rxt-string (match-string 1)))
     ;; Everything else
     ((rx (or (any "\n") nonl)) (rxt-string (match-string 0)))))))

(defun rxt-parse-escapes/pcre ()
  "Consume a one-char PCRE escape at point and return its codepoint equivalent.

Handles only those character escapes which have the same meaning
in character classes as outside them."
  (rxt-token-case
   ((rx "\\a") #x07) ; bell
   ((rx "\\e") #x1b) ; escape
   ((rx "\\f") #x0c) ; formfeed
   ((rx "\\n") #x0a) ; linefeed
   ((rx "\\r") #x0d) ; carriage return
   ((rx "\\t") #x09) ; tab
   ;; Control character
   ((rx "\\c" (submatch nonl))
    ;; from `man pcrepattern':
    ;; The precise effect of \cx is as follows: if x is a lower case
    ;; letter, it is converted to upper case.  Then bit 6 of the
    ;; character (hex 40) is inverted.
    (logxor (string-to-char (upcase (match-string 1))) #x40))
   ;; Hex escapes
   ((rx "\\x" (submatch (** 1 2 (any "0-9" "A-Z" "a-z"))))
    (string-to-number (match-string 1) 16))
   ((rx "\\x{" (submatch (* (any "0-9" "A-Z" "a-z"))) "}")
    (string-to-number (match-string 1) 16))))

(defun rxt-parse-subgroup/pcre ()
  (catch 'return
    (let ((shy nil)
          (extended-mode rxt-pcre-extended-mode)
          (single-line-mode rxt-pcre-s-mode)
          (case-fold rxt-pcre-case-fold))
      (rxt-extended-skip)
      ;; Check for special (? ..) and (* ...) syntax
      (rxt-token-case
       ((rx "?")                        ; (?
        (rxt-token-case
         ((rx ")")                      ; Empty group (?)
          (throw 'return (rxt-empty-string)))
         (":" (setq shy t))             ; Shy group (?:
         ("#"                           ; Comment   (?#
          (search-forward ")")
          (throw 'return (rxt-empty-string)))
         ((rx (or                       ; Flags     (?isx-isx
               (seq (group (* (any "gimosx"))) "-" (group (+ (any "gimosx"))))
               (seq (group (+ (any "gimosx"))))))
          (let ((token (match-string 0))
                (on (or (match-string 1) (match-string 3)))
                (off (or (match-string 2) "")))
            (if (cl-find ?x on)  (setq extended-mode t))
            (if (cl-find ?s on)  (setq single-line-mode t))
            (if (cl-find ?i on)  (setq case-fold t))
            (if (cl-find ?x off) (setq extended-mode nil))
            (if (cl-find ?s off) (setq single-line-mode nil))
            (if (cl-find ?i off) (setq case-fold nil))
            (when (string-match-p "[gmo]" token)
              (display-warning
               'rxt (format "Unhandled PCRE flags in (?%s" token))))
          (rxt-token-case
           (":" (setq shy t))        ; Shy group with flags (?isx-isx: ...
           (")"                      ; Set flags (?isx-isx)
            ;; Set flags for the remainder of the current subexpression
            (setq rxt-pcre-extended-mode extended-mode
                  rxt-pcre-s-mode single-line-mode
                  rxt-pcre-case-fold case-fold)
            (throw 'return (rxt-empty-string)))))
         ;; Other constructions like (?=, (?!, etc. are not recognised
         (t (rxt-error "Unrecognized PCRE extended construction `(?%c'"
                       (char-after)))))

       ;; No special (* ...) verbs are recognised
       ((rx "*")
        (let ((begin (point)))
          (search-forward ")" nil 'go-to-end)
          (rxt-error "Unrecognized PCRE extended construction `(*%s'"
                     (buffer-substring begin (point))))))

      ;; Parse the remainder of the subgroup
      (unless shy (cl-incf rxt-subgroup-count))
      (let* ((rxt-pcre-extended-mode extended-mode)
             (rxt-pcre-s-mode single-line-mode)
             (rxt-pcre-case-fold case-fold)
             (rx (rxt-parse-exp)))
        (rxt-extended-skip)
        (rxt-token-case
         (")" (if shy rx (rxt-submatch rx)))
         (t (rxt-error "Subexpression missing close paren")))))))

(defun rxt-parse-subgroup/el ()
  (let ((kind
         (rxt-token-case
          ((rx "?:")
           (cl-incf rxt-subgroup-count)
           'shy)
          ((rx "?" (group (+ (in "0-9"))) ":")
           (let ((n (string-to-number (match-string 1))))
             (when (< rxt-subgroup-count n)
               (setf rxt-subgroup-count n))
             n))
          ((rx "?") ; Reserved
           (rxt-error "Unknown match group sequence")))))
    (let ((rx (rxt-parse-exp)))
      (rxt-token-case
       ((rx "\\)")
        (cond ((eq kind 'shy) rx)
              ((numberp kind)
               (rxt-submatch-numbered kind rx))
              (t (rxt-submatch rx))))
       (t (rxt-error "Subexpression missing close paren"))))))

(defun rxt-parse-braces ()
  (rxt-token-case
   (rxt-m-to-n-brace-regexp
    (list (string-to-number (match-string 1))
          (string-to-number (match-string 2))))
   (rxt-m-to-?-brace-regexp
    (list (string-to-number (match-string 1)) nil))
   (rxt-m-brace-regexp
    (let ((a (string-to-number (match-string 1))))
      (list a a)))
   (t
    (let ((begin (point)))
      (search-forward "}" nil 'go-to-end)
      (rxt-error "Bad brace expression {%s"
                 (buffer-substring-no-properties begin (point)))))))

;; Parse a character set range [...]
(defun rxt-parse-char-class ()
  (when (eobp)
    (rxt-error "Missing close right bracket in regexp"))
  (rxt-with-source-location
   (let* ((negated (rxt-token-case
                    ((rx "^") t)
                    (t nil)))
          (begin (point))
          (result
           (if negated
               (rxt-negate (rxt-char-set-union))
             (rxt-char-set-union)))
          (transformer
           (if negated #'rxt-negate #'identity))
          (builder
           (if negated #'rxt-char-set-intersection #'rxt-choice)))
     (catch 'done
       (while t
         (when (eobp)
           (rxt-error "Missing close right bracket in regexp"))
         (if (and (looking-at (rx "]"))
                  (not (= (point) begin)))
             (throw 'done result)
           (let ((piece (funcall transformer (rxt-parse-char-class-piece))))
             (setq result (funcall builder result piece))))))
     (forward-char)                     ; Skip over closing "]"
     result)))

;; Parse a single character, a character range, or a posix class
;; within a character set context.  Returns an `rxt-char-set'.
(defun rxt-parse-char-class-piece ()
  (let ((atom (rxt-parse-char-class-atom)))
    (cl-typecase atom
      (rxt-char-set                     ; return unchanged
       atom)
      (integer                          ; character: check for range
       (let ((range-end (rxt-maybe-parse-range-end)))
         (if range-end
             (rxt-char-set-union (cons atom range-end))
           (rxt-char-set-union atom))))
      (t                                ; transform into character set
       (rxt-char-set-union atom)))))

;; Parse a single character or named class within a charset.
;;
;; Returns an integer (a character), a symbol (representing a named
;; character class) or an `rxt-char-set' (for pre-defined character
;; classes like \d, \W, etc.)
(defun rxt-parse-char-class-atom ()
  (or
   ;; First, check for PCRE-specific backslash sequences
   (and rxt-parse-pcre
        (rxt-parse-char-class-atom/pcre))
   ;; Char-class syntax
   (rxt-token-case
    ;; Named classes [:alnum:], ...
    (rxt-named-classes-regexp
     (intern (match-string 1)))
    ;; Error on unknown posix-class-like syntax
    ((rx "[:" (* (any "a-z")) ":]")
     (rxt-error "Unknown posix character class `%s'" (match-string 0)))
    ;; Error on [= ... ]= collation syntax
    ((rx "[" (submatch (any "." "="))
         (* (any "a-z")) (backref 1) "]")
     (rxt-error "Unsupported collation syntax `%s'" (match-string 0)))
    ;; Other characters stand for themselves
    ((rx (or "\n" nonl))
     (string-to-char (match-string 0))))))

;; Parse backslash escapes inside PCRE character classes
(defun rxt-parse-char-class-atom/pcre ()
  (or (rxt-parse-escapes/pcre)
      (rxt-token-case
       ;; Backslash + digits => octal char
       ((rx "\\" (submatch (** 1 3 (any "0-7"))))
        (string-to-number (match-string 1) 8))
       ;; Pre-defined character sets
       ((rx "\\" (submatch (any "d" "D" "h" "H" "s" "S" "v" "V" "w" "W")))
        (rxt--pcre-char-set (string-to-char (match-string 1))))
       ;; "\b" inside character classes is a backspace
       ((rx "\\b") ?\C-h)
       ;; Ignore other escapes
       ((rx "\\" (submatch nonl))
        (string-to-char (match-string 1))))))

;; Look for a range tail (the "-z" in "a-z") after parsing a single
;; character within in a character set.  Returns either a character
;; representing the range end, or nil.
(defun rxt-maybe-parse-range-end ()
  (let ((range-end nil) (end-position nil))
    (when (looking-at (rx "-" (not (any "]"))))
      (save-excursion
        (forward-char)
        (setq range-end (rxt-parse-char-class-atom)
              end-position (point))))

    (if (characterp range-end)
        ;; This is a range: move point after it and return the ending character
        (progn
          (goto-char end-position)
          range-end)
      ;; Not a range.
      nil)))

;; Return the pre-defined PCRE char-set associated with CHAR: i.e. \d
;; is digits, \D non-digits, \s space characters, etc.
(defun rxt--pcre-char-set (char)
  (let* ((base-char (downcase char))
         (negated (/= char base-char))
         (elements (assoc-default base-char rxt-pcre-char-set-alist))
         (base-char-set (apply #'rxt-char-set-union elements)))
    (if negated
        (rxt-negate base-char-set)
      base-char-set)))


;;;; Unparser to `rx' syntax

(defconst rxt-rx-verbose-equivalents
  '((bol . line-start)
    (eol . line-end)
    (nonl . not-newline)
    (bos . string-start)
    (eos . string-end)
    (bow . word-start)
    (eow . word-end)
    (seq . sequence))
  "Alist of verbose equivalents for short `rx' primitives.")

(defun rxt-rx-symbol (sym)
  (if rxt-verbose-rx-translation
      (or (assoc-default sym rxt-rx-verbose-equivalents)
          sym)
    sym))

(defun rxt-adt->rx (re)
  (let ((rx
         (cl-typecase re
          (rxt-primitive
           (rxt-rx-symbol (rxt-primitive-rx re)))

          (rxt-string
           (if (or (not (rxt-string-case-fold re))
                   (string= "" (rxt-string-chars re)))
               (rxt-string-chars re)
             `(seq
               ,@(cl-loop for char across (rxt-string-chars re)
                          collect `(any ,(upcase char) ,(downcase char))))))

          (rxt-seq
           `(seq ,@(mapcar #'rxt-adt->rx (rxt-seq-elts re))))

          (rxt-choice
           `(or ,@(mapcar #'rxt-adt->rx (rxt-choice-elts re))))

          (rxt-submatch
           (if (rxt-seq-p (rxt-submatch-body re))
               `(submatch
                 ,@(mapcar #'rxt-adt->rx (rxt-seq-elts (rxt-submatch-body re))))
             `(submatch ,(rxt-adt->rx (rxt-submatch-body re)))))

          (rxt-submatch-numbered
           (if (rxt-seq-p (rxt-submatch-numbered-p re))
               `(,(rxt-rx-symbol 'submatch-n)
                  ,(rxt-submatch-numbered-n re)
                  ,@(mapcar #'rxt-adt->rx
                            (rxt-seq-elts
                             (rxt-submatch-numbered-body re))))
             `(,(rxt-rx-symbol 'submatch-n)
                ,(rxt-submatch-numbered-n re)
                ,(rxt-adt->rx (rxt-submatch-numbered-body re)))))

          (rxt-backref
           (let ((n (rxt-backref-n re)))
             (if (<= n 9)
                 `(backref ,(rxt-backref-n re))
               (rxt-error "Too many backreferences (%s)" n))))

          (rxt-syntax-class
           `(syntax ,(rxt-syntax-class-symbol re)))

          (rxt-char-category
           `(category ,(rxt-char-category-symbol re)))

          (rxt-repeat
           (let ((from (rxt-repeat-from re))
                 (to (rxt-repeat-to re))
                 (greedy (rxt-repeat-greedy re))
                 (body (rxt-adt->rx (rxt-repeat-body re))))
             (if rxt-verbose-rx-translation
                 (let ((rx
                        (cond
                          ((and (zerop from) (null to))
                           `(zero-or-more ,body))
                          ((and (equal from 1) (null to))
                           `(one-or-more ,body))
                          ((and (zerop from) (equal to 1))
                           `(zero-or-one ,body))
                          ((null to)
                           `(>= ,from ,body))
                          ((equal from to)
                           `(repeat ,from ,body))
                          (t
                           `(** ,from ,to ,body)))))
                   (if greedy
                       (if rxt-explain
                           rx           ; Readable but not strictly accurate. Fixme?
                         `(maximal-match ,rx))
                     `(minimal-match ,rx)))
               (cond
                 ((and (zerop from) (null to))
                  `(,(if greedy '* '*?) ,body))
                 ((and (equal from 1) (null to))
                  `(,(if greedy '+ '+?) ,body))
                 ((and (zerop from) (equal to 1))
                  `(,(if greedy '\? ??) ,body))
                 ((null to)
                  `(>= ,from ,body))
                 ((equal from to)
                  `(= ,from ,body))
                 (t
                  `(** ,from ,to ,body))))))

          (rxt-char-set-union
           (let* ((case-fold (rxt-char-set-union-case-fold re))
                  (re (rxt--simplify-char-set re case-fold))
                  (chars (rxt-char-set-union-chars re))
                  (ranges (rxt-char-set-union-ranges re))
                  (classes (rxt-char-set-union-classes re))
                  (case-fold (rxt-char-set-union-case-fold re)))
             ;; Do not let the byte compiler optimize this variable out.
             (ignore case-fold)
             (if (and (null chars) (null ranges) (= 1 (length classes)))
                 (car classes)
               `(any ,@chars ,@ranges ,@classes))))

          (rxt-char-set-negation
           `(not ,(rxt-adt->rx (rxt-char-set-negation-elt re))))

          (t
           (rxt-error "No RX translation of `%s'" (rxt-to-string re))))))

    ;; Store source information on each fragment of the generated RX
    ;; sexp for rxt-explain mode
    (when rxt-explain
      ;; Use gensyms to store unique source information for multiple
      ;; occurrences of primitives like `bol'
      (when (symbolp rx)
        (setq rx (make-symbol (symbol-name rx))))
      (setf (rxt-location rx) (rxt-location re)))
    rx))


;;;; 'Unparser' to PCRE notation

;;; Based on scsh/posixstr.scm in scsh

;; To ensure that the operator precedence in the generated regexp does
;; what we want, we need to keep track of what kind of production is
;; returned from each step. Therefore these functions return a string
;; and a numeric "level" which lets the function using the generated
;; regexp know whether it has to be parenthesized:
;;
;; 0: an already parenthesized expression
;;
;; 1: a "piece" binds to any succeeding quantifiers
;;
;; 2: a "branch", or concatenation of pieces, needs parenthesizing to
;; bind to quantifiers
;;
;; 3: a "top", or alternation of branches, needs parenthesizing to
;; bind to quantifiers or to concatenation
;;
;; This idea is stolen straight out of the scsh implementation.

(defun rxt-adt->pcre (re)
  (cl-destructuring-bind (s _) (rxt-adt->pcre/lev re) s))

(defun rxt-adt->pcre/lev (re)
  (cl-typecase re
    (rxt-primitive
     (let ((s (rxt-primitive-pcre re)))
       (if s
           (list s 1)
         (rxt-error "No PCRE translation of `%s'" (rxt-to-string re)))))

   (rxt-string (rxt-string->pcre re))
   (rxt-seq (rxt-seq->pcre re))
   (rxt-choice (rxt-choice->pcre re))
   (rxt-submatch (rxt-submatch->pcre re))
   (rxt-backref
    (list (format "\\%d" (rxt-backref-n re)) 1))
   (rxt-repeat (rxt-repeat->pcre re))

   ((or rxt-char-set-union rxt-char-set-negation)
    (rxt-char-set->pcre re))
   
   ;; FIXME
   ;; ((rxt-char-set-intersection re) (rxt-char-set-intersection->pcre re))

   (t
    (rxt-error "No PCRE translation of `%s'" (rxt-to-string re)))))

(defconst rxt-pcre-metachars (rx (any "\\^.$|()[]*+?{}")))
(defconst rxt-pcre-charset-metachars (rx (any "]" "[" "\\" "^" "-")))

(defun rxt-string->pcre (re)
  (let ((chars (rxt-string-chars re)))
    (list
     (replace-regexp-in-string
      rxt-pcre-metachars
      "\\\\\\&" chars)
     ;; A one-character string is a 'piece' (it binds to a following
     ;; quantifier).  A longer string is a 'branch' (it has to be
     ;; enclosed in parentheses to bind to a following quantifier).
     (if (> (length chars) 1) 2 1))))

(defun rxt-seq->pcre (re)
  (let ((elts (rxt-seq-elts re)))
    (if (null elts)
        ""
      (rxt-seq-elts->pcre elts))))

(defun rxt-seq-elts->pcre (elts)
  (cl-destructuring-bind
      (s lev) (rxt-adt->pcre/lev (car elts))
    (if (null (cdr elts))
        (list s lev)
      (cl-destructuring-bind
          (s1 lev1) (rxt-seq-elts->pcre (cdr elts))
        (list (concat (rxt-paren-if-necessary s lev)
                      (rxt-paren-if-necessary s1 lev1))
              2)))))

(defun rxt-paren-if-necessary (s lev)
  (if (< lev 3)
      s
    (concat "(?:" s ")")))

(defun rxt-choice->pcre (re)
  (let ((elts (rxt-choice-elts re)))
    (if (null elts)
        nil
      (rxt-choice-elts->pcre elts))))

(defun rxt-choice-elts->pcre (elts)
  (cl-destructuring-bind
      (s lev) (rxt-adt->pcre/lev (car elts))
    (if (null (cdr elts))
        (list s lev)
      (cl-destructuring-bind
          (s1 _lev1) (rxt-choice-elts->pcre (cdr elts))
        (list (concat s "|" s1) 3)))))

(defun rxt-submatch->pcre (re)
  (cl-destructuring-bind
      (s _lev) (rxt-adt->pcre/lev (rxt-submatch-body re))
    (list (concat "(" s ")") 0)))

(defun rxt-repeat->pcre (re)
  (let ((from (rxt-repeat-from re))
        (to (rxt-repeat-to re))
        (body (rxt-repeat-body re))
        (greedy (rxt-repeat-greedy re)))
    (cl-destructuring-bind
        (s lev) (rxt-adt->pcre/lev body)
      (cond
       ((and to (= from 1) (= to 1)) (list s lev))
       ((and to (= from 0) (= to 0)) (list "" 2))
       (t
        (when (> lev 1)     ; parenthesize non-atoms
          (setq s (concat "(?:" s ")")
                lev 0))
        (list (if to
                  (cond ((and (= from 0) (= to 1))
                         (concat s (if greedy "?" "??")))
                        ((= from to)
                         (concat s "{" (number-to-string to) "}"))
                        (t
                         (concat s "{" (number-to-string from)
                                 "," (number-to-string to) "}")))
                (cond ((= from 0)
                       (concat s (if greedy "*" "*?")))
                      ((= from 1)
                       (concat s (if greedy "+" "+?")))
                      (t (concat s "{" (number-to-string from) ",}"))))
              1))))))

(defun rxt-char-set->pcre (re)
  (cond ((rxt-char-set-union-p re)
         (list
          (concat "[" (rxt-char-set->pcre/chars re) "]") 1))

        ((rxt-char-set-negation-p re)
         (let ((elt (rxt-char-set-negation-elt re)))
           (if (rxt-char-set-union-p elt)
               (list
                (concat "[^" (rxt-char-set->pcre/chars elt) "]") 1)
             (rxt-error "No PCRE translation of `%s'" (rxt-to-string elt)))))

        (t
         (rxt-error "Non-char-set in rxt-char-set->pcre: %s" re))))

;; Fortunately, easier in PCRE than in POSIX!
(defun rxt-char-set->pcre/chars (re)
  (cl-flet
      ((escape
        (char)
        (let ((s (char-to-string char)))
          (cond ((string-match rxt-pcre-charset-metachars s)
                 (concat "\\" s))

                ((and (not (string= s " "))
                      (string-match "[^[:graph:]]" s))
                 (format "\\x{%x}" char))

                (t s)))))

    (let ((chars (rxt-char-set-union-chars re))
          (ranges (rxt-char-set-union-ranges re))
          (classes (rxt-char-set-union-classes re)))

      (concat
       (mapconcat #'escape chars "")
       (mapconcat #'(lambda (rg)
                      (format "%s-%s"
                              (escape (car rg))
                              (escape (cdr rg))))
                  ranges "")
       (mapconcat #'(lambda (class)
                      (format "[:%s:]" class))
                  classes "")))))


;;;; Generate all productions of a finite regexp

(defun rxt-adt->strings (re)
  (cl-typecase re
    (rxt-primitive
     (list ""))
    (rxt-string
     (list (rxt-string-chars re)))
    (rxt-seq
     (rxt-seq-elts->strings (rxt-seq-elts re)))
    (rxt-choice
     (rxt-choice-elts->strings (rxt-choice-elts re)))
    (rxt-submatch
     (rxt-adt->strings (rxt-submatch-body re)))
    (rxt-submatch-numbered
     (rxt-adt->strings (rxt-submatch-numbered-body re)))
    (rxt-repeat
     (rxt-repeat->strings re))
    (rxt-char-set-union
     (rxt-char-set->strings re))
    (t
     (error "Can't generate productions of %s"
            (rxt-syntax-tree-readable re)))))

(defun rxt-concat-product (heads tails)
  (cl-mapcan
   (lambda (hs)
     (mapcar
      (lambda (ts) (concat hs ts))
      tails))
   heads))

(defun rxt-seq-elts->strings (elts)
  (if (null elts)
      '("")
    (let ((heads (rxt-adt->strings (car elts)))
          (tails (rxt-seq-elts->strings (cdr elts))))
      (rxt-concat-product heads tails))))

(defun rxt-choice-elts->strings (elts)
  (if (null elts)
      '()
    (append (rxt-adt->strings (car elts))
            (rxt-choice-elts->strings (cdr elts)))))

(defun rxt-repeat->strings (re)
  (let ((from (rxt-repeat-from re))
        (to (rxt-repeat-to re)))
    (if (not to)
        (error "Can't generate all productions of unbounded repeat \"%s\""
               (rxt-syntax-tree-readable re))
      (let ((strings (rxt-adt->strings (rxt-repeat-body re))))
        (rxt-repeat-n-m->strings from to strings)))))

(defun rxt-repeat-n-m->strings (from to strings)
  (cond
   ((zerop to) '(""))
   ((= to from) (rxt-repeat-n->strings from strings))
   (t           ; to > from
    (let* ((strs-n (rxt-repeat-n->strings from strings))
           (accum (cl-copy-list strs-n)))
      (dotimes (_i (- to from))
        (setq strs-n (rxt-concat-product strs-n strings))
        (setq accum (nconc accum strs-n)))
      accum))))

(defun rxt-repeat-n->strings (n strings)
  ;; n > 1
  (cond ((zerop n) '(""))
        ((= n 1) strings)
        (t
         (rxt-concat-product
          (rxt-repeat-n->strings (- n 1) strings)
          strings))))

(defun rxt-char-set->strings (re)
  (if (rxt-char-set-union-classes re)
      (error "Can't generate all productions of named character classes in \"%s\""
             (rxt-syntax-tree-readable re))
    (let ((chars (mapcar #'char-to-string (rxt-char-set-union-chars re))))
      (dolist (range (rxt-char-set-union-ranges re))
        (let ((end (cdr range)))
          (cl-do ((i (car range) (+ i 1)))
              ((> i end))
            (push (char-to-string i) chars))))
      chars)))


;;;; RE-Builder hacks

(defadvice reb-update-modestring
  (after rxt () activate compile)
  "This function is hacked for emulated PCRE syntax and regexp conversion."
  (setq reb-mode-string
        (concat
         (format " (%s)" reb-re-syntax)
         reb-mode-string))
  (force-mode-line-update))

(defadvice reb-change-syntax
  (around rxt (&optional syntax) activate compile)
  "This function is hacked for emulated PCRE syntax and regexp conversion."
  (interactive
   (list (intern
          (completing-read (format "Select syntax (%s): " reb-re-syntax)
                           '(read string pcre sregex rx)
                           nil t "" nil (symbol-name reb-re-syntax)))))
  (unless (memq syntax '(read string pcre lisp-re sregex rx))
    (error "Invalid syntax: %s" syntax))
  (let ((re-builder-buffer (get-buffer reb-buffer)))
    (setq reb-re-syntax syntax)
    (when re-builder-buffer
      (with-current-buffer reb-target-buffer
        (cl-case syntax
          (rx
           (let ((rx (rxt-elisp-to-rx reb-regexp)))
             (setq reb-regexp-src
                   (with-temp-buffer
                     (insert "\n" "'")
                     (rxt-print rx)
                     (buffer-string)))))
          (pcre
           (setq reb-regexp-src (rxt-elisp-to-pcre reb-regexp)))))
      (with-current-buffer re-builder-buffer
        ;; Hack: prevent reb-auto-update from clobbering the
        ;; reb-regexp-src we just set
        (let ((inhibit-modification-hooks t))
          (reb-initialize-buffer))
        ;; Enable flag-toggling bindings for PCRE syntax
        (rxt--re-builder-switch-pcre-mode)))))

(defadvice reb-read-regexp
  (around rxt () activate compile)
  "This function is hacked for emulated PCRE syntax and regexp conversion."
  (if (eq reb-re-syntax 'pcre)
      (setq ad-return-value
            (save-excursion
              (goto-char (point-min))
              (rxt-read-delimited-pcre)))
    ad-do-it))

(defadvice reb-insert-regexp
  (around rxt () activate compile)
  "This function is hacked for emulated PCRE syntax and regexp conversion."
  (if (eq reb-re-syntax 'pcre)
      (let ((src (if (fboundp #'reb-target-value)
                     (reb-target-value 'reb-regexp-src)
                   (reb-target-binding reb-regexp-src))))
        (if src
            (insert "\n/" (replace-regexp-in-string "/" "\\/" src t t) "/")
          (insert "\n//")))
    ad-do-it))

(defadvice reb-cook-regexp
  (around rxt (re) activate compile)
  "This function is hacked for emulated PCRE syntax and regexp conversion."
  (if (eq reb-re-syntax 'pcre)
      (setq ad-return-value (rxt-pcre-to-elisp re))
    ad-do-it))

(defadvice reb-update-regexp
  (around rxt () activate compile)
  "This function is hacked for emulated PCRE syntax and regexp conversion."
  (setq ad-return-value
        (let* ((re-src (reb-read-regexp))
               (re (reb-cook-regexp re-src)))
          (with-current-buffer reb-target-buffer
            (let ((oldre reb-regexp))
              (prog1
                  (not (string= oldre re))
                (setq reb-regexp re)
                ;; Update the source re if format requires translation
                (when (or (reb-lisp-syntax-p) (eq reb-re-syntax 'pcre))
                  (setq reb-regexp-src re-src))))))))

(defun rxt--re-builder-switch-pcre-mode ()
  (rxt--read-pcre-mode
   (if (eq reb-re-syntax 'pcre) 1 0)))

(add-hook 'reb-mode-hook #'rxt--re-builder-switch-pcre-mode)

(provide 'rxt)
(provide 'pcre2el)


;;; pcre2el.el ends here
