;;; cdlatex.el --- Fast input methods for LaTeX environments and math  -*- lexical-binding: t; -*-
;; Copyright (c) 2010-2022  Free Software Foundation, Inc.
;;
;; Author: Carsten Dominik <carsten.dominik@gmail.com>
;; Keywords: tex
;; Version: 4.18d
;;
;; This file is not part of GNU Emacs.
;;
;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; cdlatex.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with cdlatex.el. If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; CDLaTeX is a minor mode supporting fast insertion of environment
;; templates and math stuff in LaTeX.
;;
;; To turn CDLaTeX Minor Mode on and off in a particular buffer, use
;; `M-x cdlatex-mode'.
;;
;; To turn on CDLaTeX Minor Mode for all LaTeX files, add one of the
;; following lines to your .emacs file:
;;
;;   (add-hook 'LaTeX-mode-hook #'turn-on-cdlatex)   ; with AUCTeX LaTeX mode
;;   (add-hook 'latex-mode-hook #'turn-on-cdlatex)   ; with Emacs latex mode
;;
;; For key bindings, see further down in this documentation.
;;
;; CDLaTeX requires texmathp.el (which is distributed with AUCTeX) to
;; auto-insert $...$ when needed, so we recommend you install AUCTeX to get
;; this functionality.
;;
;;--------------------------------------------------------------------------
;;
;; OVERVIEW
;; ========
;;
;; CDLaTeX is a minor mode supporting mainly mathematical and scientific
;; text development with LaTeX.  CDLaTeX is really about speed.  AUCTeX
;; (the major mode I recommend for editing LaTeX files) does have a hook
;; based system for inserting environments and macros - but while this is
;; useful and general, it is sometimes slow to use.  CDLaTeX tries to be
;; quick, with very few and easy to remember keys, and intelligent
;; on-the-fly help.
;;
;; 1. ABBREVIATIONS.
;;    -------------
;;    CDLaTeX has an abbrev-like mechanism to insert full LaTeX
;;    environments and other templates into the buffer.  Abbreviation
;;    expansion is triggered with the TAB key only, not with SPC or RET.
;;    For example, typing "ite<TAB>" inserts an itemize environment.  A
;;    full list of defined abbreviations is available with the command
;;    `C-c ?' (`cdlatex-command-help').
;;
;;    1a. ENVIRONMENT TEMPLATES
;;        ---------------------
;;        Typing `C-c {' (`cdlatex-environment') uses the minibuffer to
;;        complete the name of a LaTeX environment and inserts a template
;;        for this environment into the buffer.  These environment
;;        templates also contain labels created with RefTeX.  In a
;;        template, text needs to be filled in at various places, which we
;;        call "points of interest".  You can use the TAB key to jump to
;;        the next point of interest in the template.  If there is an
;;        active region, the region will be wrapped into the environment,
;;        ignoring the template content.
;;
;;        For many frequently used LaTeX environments, abbreviations are
;;        available.  Most of the time, the abbreviation consists of the
;;        first three letters of the environment name: `equ<TAB>' expands
;;        into
;;            \begin{equation}
;;            \label{eq:1}
;;
;;            \end{equation}
;;
;;        Similarly, `ali<TAB>' inserts an AMS-LaTeX align environment
;;        template etc.  For a full list of environment abbreviations, use
;;        `C-c ?'.
;;
;;        Use the command `C-c -' (`cdlatex-item') to insert a generalized
;;        new "item" in any "list"-like environment.  For example, in an
;;        itemize environment, this inserts "\item", in an enumerate
;;        environment it inserts "\item\label{item:25}" and in an eqnarray
;;        environment, it inserts "\label{eq:25} \n & &".  When
;;        appropriate, newlines are inserted, and the previous item is also
;;        closed with "\\".  `cdlatex-item' can also be invoked with the
;;        abbreviation "it<TAB>".
;;
;;    1b. MATH TEMPLATES
;;        --------------
;;        Abbreviations are also used to insert simple math templates
;;        into the buffer.  The cursor will be positioned properly.  For
;;        example, typing `fr<TAB>' will insert "\frac{}{}" with the
;;        cursor in the first pair of parenthesis.  Typing `lr(<TAB>'
;;        will insert a "\left( \right)" pair and position the cursor in
;;        between, etc.  Again, the TAB key can be used to jump to the
;;        points in the template where additional text has to be
;;        inserted.  For example in the `\frac{}{}' template, it will
;;        move you from the first argument to the second and then out of
;;        the second.  For a list of available templates, type `C-c ?'.
;;        As a special case, the left-right parenthesis combinations can
;;        also be applied after writing the content by selecting the
;;        content and typing '( or '[ or '{ or '< or '| .
;;
;; 2. MATHEMATICAL SYMBOLS
;;    --------------------
;;    This feature is similar to the functionality in the Math minor mode
;;    of AUCTeX, and to the input methods of the X-Symbol package.  It is
;;    introduced by the backquote character.  Backquote followed by any
;;    character inserts a LaTeX math macro into the buffer.  If
;;    necessary, a pair of "$" is inserted to switch to math mode.  For
;;    example, typing "`a" inserts "$\alpha$".  Since LaTeX defines many
;;    more mathematical symbols than the alphabet has letters, different
;;    sets of math macros are provided.  We call the different sets
;;    "levels".  On each level, another LaTeX macro is assigned to a
;;    given letter.  To select the different levels, simply press the
;;    backquote character several times before pressing the letter.  For
;;    example, typing "`d" inserts "\delta" (level 1), and typing "``d"
;;    inserts "\partial" (level 2).  Similarly, "`e" inserts "\epsilon"
;;    and "``e" inserts "\varepsilon".
;;
;;    On each level, on-thy-fly help will pop up automatically if you
;;    hesitate to press the next key.  The help screen is a window which
;;    lists all math macros available on the current level.  Initially,
;;    when you type slowly, this window will pop up each time you press
;;    backquote.  However, after you have learned the different keys, you
;;    will type more quickly and the help window is not shown.  Try it
;;    out: First press "`" (backquote), wait for the help window and then
;;    press "a" to get "\alpha".  Then press "`" and "b" as a quick
;;    sequence to get "\beta", without the help window.
;;
;;    The LaTeX macros available through this mechanism are fully
;;    configurable - see the variable `cdlatex-math-symbol-alist'.
;;
;; 3. ACCENTS AND FONTS
;;    -----------------
;;    Putting accents on mathematical characters and/or changing the font
;;    of a character uses key combinations with the quote character "'"
;;    as a prefix.  The accent or font change is applied to the character
;;    or LaTeX macro *before* point.  For example
;;
;;      Keys                            Result
;;      --------------------------------------------------------------------
;;      a'~                             ERROR                 % in text mode
;;      $a'~                            \tilde{a}             % in math mode
;;      a':                             \ddot{a}
;;      ab'b                            \textbf{ab}           % in text mode
;;      $ab'b                           a\mathbf{b}           % in math mode
;;      \alpha'.                        \dot{\alpha}
;;      r_{dust}'r                      r_\mathrm{dust}       % in math mode
;;      <SPC> 'e                        \emph{}
;;      this is important   M-2 'b      this \textbf{is important}
;;
;;    As you can see:
;;    - using math accents like ~ outside math mode will throw an error.
;;    - the font change used automatically adapts to math mode.
;;    - if the item before point is a LaTeX macro, the change applies to
;;      the whole macro.
;;    - in text mode, the change applies to the entire word before point,
;;      while in math mode only the last character is modified.
;;    - if the character before point is white space, a dollar or an
;;      opening parenthesis, this command just opens an empty template
;;      and positions the cursor inside.
;;    - when a numeric prefix argument is supplied, the command acts on
;;      whole words before the cursor.
;;    - Furthermore, if there is an active region, the change will apply
;;      to the content of that region.
;;
;;    In order to insert a normal quote, you can press the quote
;;    character twice.  Also, if the key character is not associated with
;;    an accent or font, the quote will be inserted.  For example, "'t"
;;    and "'s" insert just that, so that normal text typing will not be
;;    disturbed.  Just like during the insertion of math macros (see above
;;    under (4.)), automatic on-the-fly help will pop up when you pause
;;    after hitting the quote character, but will be suppressed when you
;;    continue quickly.  The available accents and also the prefix key
;;    can be can be configured - see documentation of the variables
;;    `cdlatex-math-modify-alist' and `cdlatex-math-modify-prefix'.
;;
;; 4. PAIR INSERTION of (), [], {}, and $$
;;    ------------------------------------
;;    Dollars and parens can be inserted as pairs.  When you type the
;;    opening delimiter, the closing delimiter will be inserted as well,
;;    and the cursor positioned between them.  You can configure which
;;    delimiter are inserted pairwise by configuring the variable
;;    `cdlatex-paired-parens'.
;;
;;    If the region is active when you press $, the region will be
;;    bracketed by a pair of $ characters.
;;
;;    Also, the keys `_' and `^' will insert "_{}" and "^{}",
;;    respectively, and, if necessary, also a pair of dollar signs to
;;    switch to math mode.  You can use TAB to exit paired parenthesis.
;;    As a special case, when you use TAB to exit a pair of braces that
;;    belong to a subscript or superscript, CDLaTeX removes the braces if
;;    the sub/superscript consists of a single character.  For example
;;    typing "$10^3<TAB>" inserts "$10^3$", but typing "$10^34<TAB>"
;;    inserts "$10^{34}$".
;;
;;    If you press `_' or `^' twice, the template inserted will be
;;    `_{\mathrm{}}' or `^{\mathrm }', respectively, to insert a roman
;;    sub/super-script.  Style guides require that all sub- and
;;    superscripts that are descriptive (so not a mathematical or
;;    physical quantity themselves) need to be roman.  So $x_i$ is i
;;    is an index, but $x_{\rm max}$ to indicate the maximum value.  You
;;    can disable this behavior through the variable
;;    `cdlatex-make-sub-superscript-roman-if-pressed-twice'.
;;
;; 5. THE OVERLOADED TAB KEY
;;    ----------------------
;;    You may have noticed that we use the TAB key for many different
;;    purposes in this package.  While this may seem confusing, I have
;;    gotten used to this very much.  Hopefully this will work for you as
;;    well: "when in doubt, press TAB".  Here is a summary of what happens
;;    when you press the TAB key:
;;
;;    The function first tries to expand any abbreviation before point.
;;
;;    If there is none, it cleans up short subscripts and superscripts at
;;    point.  I.e., is the cursor is just before the closing brace in
;;    "a^{2}", it changes it to "a^2", since this is more readable.  If
;;    you want to keep the braces also for simple superscripts and
;;    subscripts, set the variable `cdlatex-simplify-sub-super-scripts'
;;    to nil.
;;
;;    After that, the TAB function jumps to the next point of interest in
;;    a LaTeX text where one would reasonably expect that more input can
;;    be put in.  This does *not* use special markers in the template,
;;    but a heuristic method which works quite well.  For the detailed
;;    rules which govern this feature, check the documentation of the
;;    function `cdlatex-tab'.
;;
;;-----------------------------------------------------------------------------
;;
;; CONFIGURATION EXAMPLES
;; ======================
;;
;; Check out the documentation of the variables in the configuration
;; section.  The variables must be set before cdlatex-mode is turned on,
;; or, at the latext, in `cdlatex-mode-hook', in order to be effective.
;; When changing the variables, toggle the mode off and on to make sure
;; that everything is up to date.
;;
;; Here is how you might configure CDLaTeX to provide environment templates
;; (including automatic labels) for two theorem-like environments.
;;
;;   (setq cdlatex-env-alist
;;      '(("axiom" "\\begin{axiom}\nAUTOLABEL\n?\n\\end{axiom}\n" nil)
;;        ("theorem" "\\begin{theorem}\nAUTOLABEL\n?\n\\end{theorem}\n" nil)))
;;
;; The "AUTOLABEL" indicates the place where an automatic label should be
;; inserted, using RefTeX.  The question mark defines the position of the
;; cursor after the template has been inserted into the buffer.
;;
;; You could also define your own keyword commands "axm" and "thr" to make
;; the template insertion quicker (e.g. `axm<TAB>' and `thm<TAB>'):
;;
;; (setq cdlatex-command-alist
;;  '(("axm" "Insert axiom env"   "" cdlatex-environment ("axiom") t nil)
;;    ("thr" "Insert theorem env" "" cdlatex-environment ("theorem") t nil)))
;;
;; Here is how to add new math symbols to CDLaTeX's list: In order to put
;; all rightarrow commands onto `>, ``>, ```>, and ````> (i.e. several
;; backquotes followed by >) and all leftarrow commands onto '<, ``<, ```<,
;; and ````<,  you could do this in .emacs:
;;
;;   (setq cdlatex-math-symbol-alist
;; '((?< ("\\leftarrow" "\\Leftarrow" "\\longleftarrow" "\\Longleftarrow"))
;;   (?> ("\\rightarrow" "\\Rightarrow" "\\longrightarrow" "\\Longrightarrow"))
;;    ))
;;
;; To change the prefix key for math accents and font switching, you could
;; do something like
;;
;;   (setq cdlatex-math-modify-prefix [f7])
;;-----------------------------------------------------------------------------
;;
;; KEY BINDINGS
;;
;; Here is the default set of keybindings from CDLaTeX.  A menu is also
;; installed.
;;
;;   $         cdlatex-dollar
;;   (         cdlatex-pbb
;;   {         cdlatex-pbb
;;   [         cdlatex-pbb
;;   |         cdlatex-pbb
;;   <         cdlatex-pbb
;;   ^         cdlatex-sub-superscript
;;   _         cdlatex-sub-superscript
;;
;;   TAB       cdlatex-tab
;;   C-c ?     cdlatex-command-help
;;   C-c {     cdlatex-environment
;;   C-c -     cdlatex-item
;;   `         cdlatex-math-symbol
;;   '         cdlatex-math-modify
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; FAQ
;;
;; - Some people find it disturbing that the quote character (') is active
;;   for math accents and font switching.  I have tried to avoid any letters
;;   which are frequently following ' in normal text.  For example, 's and 't
;;   insert just this.  If you still prefer a different prefix key, just
;;   configure the variable `cdlatex-math-modify-prefix'.
;;
;; - To insert a backquote into the buffer, use C-q `
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;;;;;

;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'reftex)

;;; Begin of Configuration Section ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Configuration Variables and User Options for CDLaTeX ------------------

(defgroup cdlatex nil
  "LaTeX label and citation support."
  :tag "CDLaTeX"
  :link '(url-link :tag "Home Page" "http://zon.astro.uva.nl/~dominik/Tools/")
  :prefix "cdlatex-"
  :group 'tex)

(defun cdlatex-customize ()
  "Call the customize function with cdlatex as argument."
  (interactive)
  (cond
   ((fboundp 'customize-browse)
    (customize-browse 'cdlatex))
   ((fboundp 'customize-group)
    (customize-group 'cdlatex))
   (t (error "No customization available"))))

(defun cdlatex-create-customize-menu ()
  "Create a full customization menu for CDLaTeX."
  (interactive)
  (if (fboundp 'customize-menu-create)
      (progn
        (easy-menu-change
         '("CDLTX") "Customize"
         `(["Browse CDLaTeX group" cdlatex-customize t]
           "---"
           ,(customize-menu-create 'cdlatex)
           ["Set" Custom-set t]
           ["Save" Custom-save t]
           ["Reset to Current" Custom-reset-current t]
           ["Reset to Saved" Custom-reset-saved t]
           ["Reset to Standard Settings" Custom-reset-standard t]))
        (message "\"CDLTX\"-menu now contains full customization menu"))
    (error "Cannot expand menu (outdated version of cus-edit.el)")))

;; Configuration of KEYWORD commands ------------------------------------

(defgroup cdlatex-keyword-commands nil
  "How to type a keyword in the buffer and hit TAB to execute."
  :group 'cdlatex)

(defcustom cdlatex-command-alist nil
  "List of abbrev-like commands, available with keyword and TAB.
See `cdlatex-command-alist-default' for examples.  This list only
defines additions to the defaults.  For a full list of active commands,
press \\[cdlatex-command-help].
Each element of this list is again a list with the following items:
0. KEYWORD     The key that has to be typed into the text.
1. DOCSTRING   A documentation string, less than 60 characters long.
2. REPLACE     The text to be substituted for the keyword, if any.
3. HOOK        A function to be called.
4. ARGS        Optional list of arguments to the function.
5. TEXTFLAG    non-nil means this keyword command is active in textmode.
6. MATHFLAG    non-nil means this keyword command is active in math mode."
  :group 'cdlatex-keyword-commands
  :type '(repeat
          (list (string   :tag "Keyword    ")
                (string   :tag "Docstring  ")
                (string   :tag "Replacement")
                (function :tag "Hook       ")
                (sexp     :tag "Arguments  ")
                (boolean  :tag "Available in Text mode")
                (boolean  :tag "Available in Math mode"))))

(defcustom cdlatex-tab-hook nil
  "List of functions called by TAB before the default command is executed.
These functions are called each time TAB is pressed.  They may parse the
environment and take an action.  The function should return t when it
successful executed an action and other TAB actions should *not* be tried.
When a return value is nil, other hook functions are tried, followed by the
default action of TAB (see documentation of the command `cdlatex-tab'."
  :group 'cdlatex-keyword-commands
  :type '(repeat (function :tag "Function" :value nil)))

;; Configuration of environment templates -------------------------------

(defgroup cdlatex-environment-support nil
  "Template-based insertion of LaTeX environments."
  :group 'cdlatex)

(defcustom cdlatex-env-alist nil
  "Association list of LaTeX environments and the corresponding templates.
The car of the list is a keyword to identify the environment.
the two following items in the list are the templates for environment
and item.  See `cdlatex-env-alist-default' for examples.  Any entries
in this variable will be added to the default definitions."
  :group 'cdlatex-environment-support
  :type '(repeat
          (list :tag ""
                (string :format "ENVIRONMENT %v" "")
                (text   :format "ENVIRONMENT TEMPLATE\n%v" "")
                (choice :tag "ITEM"
                        (const :tag "none" nil)
                        (text  :tag "template" :format "TEMPLATE\n%v" "")))))

(defcustom cdlatex-insert-auto-labels-in-env-templates t
  "Non-nil means the environment templates CDLaTeX will contain labels.
This variable may be set to t, nil, or a string of label type letters
indicating the label types for which it should be true."
  :group 'cdlatex-making-and-inserting-labels
  :type '(choice :tag "Insert labels in templates"
                 (const  :tag "always" t)
                 (const  :tag "never" nil)
                 (string :tag "selected label types" "")))

;; Configuration of Math character insertion and accents ----------------

(defgroup cdlatex-math-support nil
  "Support for mathematical symbols and accents in CDLaTeX."
  :group 'cdlatex)

(defcustom cdlatex-math-symbol-prefix ?`
  "Prefix key for `cdlatex-math-symbol'.
This may be a character, a string readable with `read-kbd-macro', or a
Lisp vector."
  :group 'cdlatex-math-support
  :type '(choice
          (character)
          (string :value "" :tag "kbd readable string")
          (sexp :value [] :tag "a lisp vector")))

(defcustom cdlatex-math-symbol-direct-bindings '(nil nil nil)
  "How to bind the math symbols directly.
This is a list of key binding descriptions for different levels of
math symbols.  First entry for level 1 etc.
Each entry consists of a prefix key and a list of modifiers for the
character.  The prefix key can be nil, or any of a character, a
`read-kbd-macro' readable string or a vector.
Examples:
`((nil alt))'                   bind `\\delta' to `A-d'.
`((\"C-c C-f\"))'               bind `\\delta' to `C-c C-f d'.
`((nil alt) (nil alt control))' bind `\\delta' to `A-d' and
                                `\\partial' (which is on level 2)
                                to `A-C-d'"
  :group 'cdlatex-math-support
  :type '(repeat
          (choice
           (const :tag "No binding of this level" nil)
           (cons
            :tag "Specify a binding"
            :value (nil alt)
            (choice
             (const :tag "No prefix" nil)
             (character :value ?@)
             (string :value "" :tag "kbd readable string")
             (sexp :value [] :tag "a lisp vector"))
            (set :tag "Modifiers for the final character" :greedy t
                 (const control)
                 (const meta)
                 (const alt)
                 (const super)
                 (const hyper))))))

(defcustom cdlatex-math-symbol-alist nil
  "Key characters and math symbols for fast access with the prefix key.
First element is a character, followed by a number of strings attached to
this key.  When the string contains a question mark, this is where the
cursor will be positioned after insertion of the string into the buffer.
See `cdlatex-math-symbol-alist-default' for an example.  Any entry defined
here will replace the corresponding entry of the default list.  The
defaults implement 3 levels of symbols so far: Level 1 for Greek letters
and standard symbols, level 2 for variations of level 1, and level 3 for
functions and operators."
  :group 'cdlatex-math-support
  :type '(repeat
          (list
           (character ?a)
           (repeat (string :tag "macro" "")))))

(defcustom cdlatex-math-modify-prefix ?'
  "Prefix key for `cdlatex-math-modify'.
It can be a character, a string interpretable with `read-kbd-macro',
or a Lisp vector."
  :group 'cdlatex-math-support
  :type '(choice
          (character)
          (string :value "" :tag "kbd readable string")
          (sexp :value [] :tag "a lisp vector")))

(defcustom cdlatex-modify-backwards t
  "Non-nil means, `cdlatex-math-modify' modifies char before point.
Nil means, always insert only an empty modification form.  This is also
the case if the character before point is white or some punctuation."
  :group 'cdlatex-math-support
  :type 'boolean)

(defcustom cdlatex-math-modify-alist nil
  "List description of the LaTeX math accents.
See `cdlatex-math-modify-alist-default' for an example.  Any entries in this
variable will be added to the default.
Each element contains 6 items:
0. key:      The character that is the key for a the accent.
1. mathcmd:  The LaTeX command associated with the accent in math mode
2. textcmd:  The LaTeX command associated with the accent in text mode
3. type:     t   if command with argument (e.g. \\tilde{a}).
             nil if style (e.g. {\\cal a}).
4. rmdot:    t   if the dot on i and j has to be removed.
5. it        t   if italic correction is required."
  :group 'cdlatex-math-support
  :type '(repeat
          (list (character :tag "Key character ")
                (choice :tag "TeX macro inside  math mode"
                        (string "")
                        (const :tag "none" nil))
                (choice :tag "TeX macro outside math mode"
                        (string "")
                        (const :tag "none" nil))
                (boolean :tag "Type             " :on "Command" :off "Style")
                (boolean :tag "Remove dot in i/j")
                (boolean :tag "Italic correction"))))

(defcustom cdlatex-make-sub-superscript-roman-if-pressed-twice t
  "Non-nil means, pressing `^' or `_' twice inserts roman sub/superscript."
  :group 'cdlatex-math-support
  :type 'boolean)

(defcustom cdlatex-use-dollar-to-ensure-math t
  "Non-nil means, use $...$ to force a math mode setting where needed.
When nil, use \\(...\\) instead."
  :group 'cdlatex-math-support
  :type '(boolean))

;; Miscellaneous configurations -----------------------------------------

(defgroup cdlatex-miscellaneous-configurations nil
  "Collection of further configurations."
  :group 'cdlatex)

(defcustom cdlatex-use-fonts t
  "Non-nil means, use fonts in label menu and on-the-fly help.
Font-lock must be loaded as well to actually get fontified display."
  :group 'cdlatex-miscellaneous-configurations
  :type '(boolean))

(defcustom cdlatex-takeover-parenthesis t
  "Non-nil means, cdlatex is allowed to take over the parenthesis insertion.
This means it will redefine the `(', `{', and `[' keys."
  :group 'cdlatex-miscellaneous-configurations
  :type '(boolean))

(defcustom cdlatex-takeover-dollar t
  "Non-nil means, cdlatex is allowed to take over the $.
This means it will redefine the `$' keys."
  :group 'cdlatex-miscellaneous-configurations
  :type '(boolean))

(defcustom cdlatex-takeover-subsuperscript t
  "Non-nil means, cdlatex is allowed to take over the ^ and _ keys."
  :group 'cdlatex-miscellaneous-configurations
  :type '(boolean))

(defcustom cdlatex-paired-parens "$[{"
  "String with the opening parens you want to have inserted paired.
The following parens are allowed here: `$([{|<'.
I recommend to set this to '$[{' as these have syntactical meaning in
TeX and are required to be paired.  TAB is a good way to move out of paired
parens."
  :group 'cdlatex-miscellaneous-configurations
  :type '(string :tag "Opening delimiters"))

(defcustom cdlatex-simplify-sub-super-scripts t
  "Non-nil means, TAB will simplify sub- and superscripts at point.
When you use TAB to exit from a sub- or superscript which is a single
letter, the parenthesis will be removed."
  :group 'cdlatex-miscellaneous-configurations
  :type '(boolean))

(defcustom cdlatex-sub-super-scripts-outside-math-mode t
  "Non-nil means, inserting ^ or _ will add dollars outside math environment.
So in text mode surrounding dollars and braces will be added with `_' and `^'.
When nil, `_' and `^' will just self-insert."
  :group 'cdlatex-miscellaneous-configurations
  :type '(boolean))

(defcustom cdlatex-auto-help-delay 1.5
  "Number of idle seconds before display of auto-help.
When executing `cdlatex-math-symbol' or `cdlatex-math-modify', display
automatic help when idle for more than this amount of time."
  :group 'cdlatex-miscellaneous-configurations
  :type 'number)

;;;============================================================================
;;;
;;; Define the formal stuff for a minor mode named CDLaTeX.
;;;

(defun cdlatex-show-commentary ()
  "Use the finder to view the file documentation from `cdlatex.el'."
  (interactive)
  (require 'finder)
  (finder-commentary "cdlatex.el"))

(defvar cdlatex-mode nil
  "Determines if CDLaTeX minor mode is active.")
(make-variable-buffer-local 'cdlatex-mode)

(defun cdlatex-get-kbd-vector (obj)
  "Ensure OBJ can be used as a keybinding."
  (cond ((vectorp obj) obj)
        ((integerp obj) (vector obj))
        ((stringp obj) (read-kbd-macro obj))
        ((and (fboundp 'characterp) (characterp obj))
         (vector obj))        ; XEmacs only
        (t nil)))

(defvar cdlatex-mode-map
  (let ((map (make-sparse-keymap)))
    (if cdlatex-takeover-dollar
        (define-key map  "$"         #'cdlatex-dollar))
    (if cdlatex-takeover-parenthesis
        (progn
          (define-key map  "("         #'cdlatex-pbb)
          (define-key map  "{"         #'cdlatex-pbb)
          (define-key map  "["         #'cdlatex-pbb)
          (define-key map  "|"         #'cdlatex-pbb)
          (define-key map  "<"         #'cdlatex-pbb)))
    (if cdlatex-takeover-subsuperscript
        (progn
          (define-key map  "^"         #'cdlatex-sub-superscript)
          (define-key map  "_"         #'cdlatex-sub-superscript)))
    
    (define-key map  "\t"        #'cdlatex-tab)
    (define-key map  "\C-c?"     #'cdlatex-command-help)
    (define-key map  "\C-c{"     #'cdlatex-environment)
    (define-key map  [(control return)] #'cdlatex-item)
    
    (define-key map (cdlatex-get-kbd-vector cdlatex-math-symbol-prefix)
      #'cdlatex-math-symbol)
    (define-key map (cdlatex-get-kbd-vector cdlatex-math-modify-prefix)
      #'cdlatex-math-modify)
    map)
  "Keymap for CDLaTeX minor mode.")

;;;###autoload
(defun turn-on-cdlatex ()
  "Turn on CDLaTeX minor mode."
  (cdlatex-mode t))

;;;###autoload
(define-minor-mode cdlatex-mode
  "Minor mode for editing scientific LaTeX documents.

Here is a list of features: \\<cdlatex-mode-map>

                           KEYWORD COMMANDS
                           ----------------

Many CDLaTeX commands are activated with an abbrev-like
mechanism.  When a keyword is typed followed \\[cdlatex-tab], the
keyword is deleted from the buffer and a command is executed.
You can get a full list of these commands with
\\[cdlatex-command-help].  For example, when you type `fr<TAB>',
CDLaTeX will insert \\frac{}{}.

When inserting templates like \\='\\frac{}{}\\=', the cursor is
positioned properly.  Use \\[cdlatex-tab] to move through
templates.  \\[cdlatex-tab] also kills unnecessary braces around
subscripts and superscripts at point.

                     MATH CHARACTERS AND ACCENTS
                     ---------------------------

\\[cdlatex-math-symbol] followed by any character inserts a LaTeX
math character
      e.g. \\[cdlatex-math-symbol]e
        => \\epsilon

\\[cdlatex-math-symbol]\\[cdlatex-math-symbol] followed by any
character inserts other LaTeX math character
      e.g. \\[cdlatex-math-symbol]\\[cdlatex-math-symbol]e
        => \\varepsilon
\\[cdlatex-math-modify]  followed by character puts a math
accent on a letter or symbol
      e.g. \\[cdlatex-math-symbol]a\\[cdlatex-math-modify]~
        => \\tilde{\\alpha}

CDLaTeX is aware of the math environments in LaTeX and modifies
the workings of some functions according to the current status.

                             ONLINE HELP
                             -----------

After pressing \\[cdlatex-math-symbol] or
\\[cdlatex-math-modify], CDLaTeX waits for a short time for the
second character.  If that does not come, it will pop up a window
displaying the available characters and their meanings.

                             KEY BINDINGS
                             ------------
\\{cdlatex-mode-map}

Under X, many functions will be available also in a menu on the menu bar.

Entering `cdlatex-mode' calls the hook cdlatex-mode-hook."
  :lighter " CDL"
  (when cdlatex-mode
    (cdlatex-compute-tables)))

(defalias 'cdlatex--texmathp
  (if (fboundp 'texmathp) #'texmathp
    ;; FIXME: Maybe we could do better, but why bother: the users who want it
    ;; can install AUCTeX.  Tho maybe we should move texmathp into its
    ;; own package so it can be used even when AUCTeX is not
    ;; installed/activated.
    #'ignore))

;;; ===========================================================================
;;;
;;; Functions that check out the surroundings

(defun cdlatex-dollars-balanced-to-here (&optional from)
  "Non-nil if the dollars are balanced between start of paragraph and point.
Unless the optional argument FROM specifies the position from
where to check for balance, the beginning of the current
paragraph is used."
  (save-excursion
    (let ((answer t) (pos (point)))
      (if from
          (goto-char from)
        (backward-paragraph 1))
      (if (not (bobp)) (backward-char 1))
      (while (re-search-forward "[^\\]\\$+" pos t)
        (if (/= (char-after (match-beginning 0)) ?\\)
            (setq answer (not answer))))
      (setq answer answer))))

(defun cdlatex-number-of-backslashes-is-odd ()
  "Count backslashes before point and return t if number is odd."
  (let ((odd nil))
    (save-excursion
      (while (equal (preceding-char) ?\\)
        (progn
          (forward-char -1)
          (setq odd (not odd)))))
    (setq odd odd)))

;; ============================================================================
;;
;; Some generally useful functions

(defun cdlatex-uniquify (alist &optional keep-list)
  "Return a list of all elements in ALIST, but each car only once.
Elements of KEEP-LIST are not removed even if duplicate."
  (let (new elm)
    (while alist
      (setq elm (car alist)
            alist (cdr alist))
      (if (or (member (car elm) keep-list)
              (not (assoc (car elm) new)))
          (setq new (cons elm new))))
    (setq new (nreverse new))
    new))

(defun cdlatex-use-fonts ()
  "Return t if we can and want to use fonts."
  (and window-system
       cdlatex-use-fonts
       (boundp 'font-lock-keyword-face)))

;;; ---------------------------------------------------------------------------
;;;
;;; Insert pairs of $$ (), etc.

;; Alist connection opening with closing delimiters
(defconst cdlatex-parens-pairs '(("(".")") ("["."]") ("{"."}")
                               ("|"."|") ("<".">")))

(defun cdlatex-pbb ()
  "Insert a pair of parens, brackets or braces."
  (interactive)
  (let ((paren (char-to-string (event-basic-type last-command-event))))
    (if (and (stringp cdlatex-paired-parens)
             (string-match (regexp-quote paren) cdlatex-paired-parens)
             (not (cdlatex-number-of-backslashes-is-odd)))
        (progn
          (insert paren)
          (insert (cdr (assoc paren cdlatex-parens-pairs)))
          (forward-char -1))
      (insert paren))))

(defun cdlatex-ensure-math ()
  "Make sure we are in math mode."
  (unless (if (fboundp 'texmathp) (texmathp) t)
    (if cdlatex-use-dollar-to-ensure-math
        (cdlatex-dollar)
      (insert "\\(\\)")
      (backward-char 2))))

(defun cdlatex-dollar (&optional arg)
  "Insert a pair of dollars unless number of backslashes before point is odd.
With ARG, insert pair of double dollars."
  (interactive "P")
  (cond
   ((region-active-p)
    (let ((s (region-beginning)) (e (region-end)))
      (goto-char s)
      (insert "$")
      (goto-char (1+ e))
      (insert "$")))
   ((cdlatex-number-of-backslashes-is-odd)
    (insert "$"))
   ((cdlatex--texmathp)
    (defvar texmathp-why)
    (if (and (stringp (car texmathp-why))
             (equal (substring (car texmathp-why) 0 1) "$"))
        (progn
          (insert (car texmathp-why))
          (save-excursion
            (goto-char (cdr texmathp-why))
            (if (pos-visible-in-window-p)
                (sit-for 1))))
      (message "No dollars inside a math environment!")
      (ding)))
   ((and (stringp cdlatex-paired-parens)
         (string-match "\\$" cdlatex-paired-parens))
    (if arg
        (if (bolp)
            (progn (insert "$$\n\n$$\n") (backward-char 4))
          (insert "$$  $$") (backward-char 3))
      (insert "$$") (backward-char 1)))
   (arg
    (if (bolp) (insert "$$\n") (insert "$$")))
   (t (insert "$"))))

(defun cdlatex-sub-superscript ()
  "Insert ^{} or _{} unless the number of backslashes before point is odd.
When not in LaTeX math environment, _{} and ^{} will have dollars.
When pressed twice, make the sub/superscript roman."
  (interactive)
  (if (and cdlatex-make-sub-superscript-roman-if-pressed-twice
           (equal this-command last-command))
      (progn
        (insert "\\mathrm{}")
        (backward-char 1))
    (if (cdlatex-number-of-backslashes-is-odd)
        ;; Quoted
        (insert (event-basic-type last-command-event))
      ;; Check if we are in math mode, if not switch to or only add _ or ^
      (if (not (or (cdlatex--texmathp)
                   cdlatex-sub-super-scripts-outside-math-mode))
          (insert (event-basic-type last-command-event))
        (cdlatex-ensure-math)
        ;; Insert the normal template.
        (insert (event-basic-type last-command-event))
        (insert "{}")
        (forward-char -1)))))

(defun cdlatex-lr-pair ()
  "Insert a \\left-\\right pair of parens."
  (interactive)
  (let* ((paren (char-to-string (preceding-char)))
         (close (cdr (assoc paren cdlatex-parens-pairs)))
         (paren1 paren)
         (close1 close))
    (if (string= paren "<") (setq paren1 "\\langle" close1 "\\rangle"))
    (if (string= paren "{") (setq paren1 "\\{" close1 "\\}"))
    (backward-delete-char 1)
    (if (and (stringp cdlatex-paired-parens)
             (string-match (regexp-quote paren) cdlatex-paired-parens)
             (string= close (char-to-string (following-char))))
        ; parens are inserted paired, and there is already a closing parenthesis
        (delete-char 1))
    (insert "\\left" paren1 " ? \\right" close1)
    (cdlatex-position-cursor)))

;;; ===========================================================================
;;;
;;; Keyword controlled commands and cursor movement

(defvar cdlatex-command-alist-comb nil)

(defun cdlatex-tab ()
  "This function is intended to do many cursor movements.
It is bound to the tab key since tab does nothing useful in a TeX file.

This function first calls all functions in `cdlatex-tab-hook', which see.

If none of those functions returns t, the command  first tries to expand
any command keyword before point.

If there is none, it cleans up short subscripts and superscripts at point.
I.e. it changes a^{2} into a^2, since this is more readable.  This feature
can be disabled by setting `cdlatex-simplify-sub-super-scripts' to nil.

Then it jumps to the next point in a LaTeX text where one would reasonably
expect that more input can be put in.
To do that, the cursor is moved according to the following rules:

The cursor stops...
- before closing brackets if `preceding-char' is any of -({[]})
- after  closing brackets, but not if `following-char' is any of ({[_^
- just after $, if the cursor was before that $.
- at end of non-empty lines
- at the beginning of empty lines
- before a SPACE at beginning of line
- after first of several SPACE

Sounds strange?  Try it out!"
  (interactive)
  (catch 'stop

    ;; try hook stuff
    (let ((funcs cdlatex-tab-hook))
      (while funcs (if (funcall (pop funcs)) (throw 'stop t))))

    ;; try command expansion
    (let ((pos (point)) exp math-mode)
      (backward-word 1)
      (while (eq (following-char) ?$) (forward-char 1))
      (setq exp (buffer-substring-no-properties (point) pos))
      (setq exp (assoc exp cdlatex-command-alist-comb))
      (when exp
        (setq math-mode (cdlatex--texmathp))
        (when (or (and (not math-mode) (nth 5 exp))
                  (and math-mode (nth 6 exp)))
          (delete-char (- pos (point)))
          (insert (nth 2 exp))
          ;; call the function if there is one defined
          (and (nth 3 exp)
               (if (nth 4 exp)
                   (apply (nth 3 exp) (nth 4 exp))
                 (funcall (nth 3 exp))))
          (throw 'stop t)))
      (goto-char pos))

    ;; Check for simplification of sub and superscripts
    (cond
     ((looking-at "}\\|\\]\\|)")
      (forward-char -3)
      (if (and (looking-at "[_^]{[-+0-9a-zA-Z]}")
               cdlatex-simplify-sub-super-scripts)
          ;; simplify sub/super script
          (progn (forward-char 1)
                 (delete-char 1)
                 (forward-char 1)
                 (delete-char 1))
        (forward-char 4))
      (if (looking-at "[^_^({[]")
          ;; stop after closing bracket, unless ^_[{( follow
          (throw 'stop t)))
     ((= (following-char) ?$)
      (while (= (following-char) ?$) (forward-char 1))
      (throw 'stop t))
     ((= (following-char) ?\ )
      ;; stop after first of many spaces
      (forward-char 1)
      (re-search-forward "[^ ]")
      (if (/= (preceding-char) ?\n) (forward-char -1)))
     (t
      (forward-char 1)))

    ;; move to next possible stopping site and check out the place
    (while (re-search-forward "[ )}\n]\\|\\]" (point-max) t)
      (forward-char -1)
      (cond
       ((= (following-char) ?\ )
        ;; stop at first space or b-o-l
        (if (not (bolp)) (forward-char 1)) (throw 'stop t))
       ((= (following-char) ?\n)
        ;; stop at line end, but not after \\
        (if (and (bolp) (not (eobp)))
            (throw 'stop t)
          (if (equal "\\\\" (buffer-substring-no-properties
                             (- (point) 2) (point)))
              (forward-char 1)
            (throw 'stop t))))
       (t
        ;; Stop before )}] if preceding-char is any parenthesis
        (if (or (= (char-syntax (preceding-char)) ?\()
                (= (char-syntax (preceding-char)) ?\))
                (= (preceding-char) ?-))
            (throw 'stop t)
          (forward-char 1)
          (if (looking-at "[^_^({[]")
              ;; stop after closing bracket, unless ^_[{( follow
              (throw 'stop t))))))))

(defun cdlatex-command-help (&optional arg)
  "Show the available cdlatex commands in the help buffer.
With a non-nil ARG, or a prefix argument, run texdoc instead."
  (interactive "P")
  (if arg
      (call-interactively 'TeX-documentation-texdoc)
    (with-output-to-temp-buffer " *CDLaTeX Help*"
      (princ "                    AVAILABLE KEYWORD COMMANDS WITH CDLaTeX\n")
      (princ "                    --------------------------------------\n")
      (princ "To execute, type keyword into buffer followed by TAB.\n\n")
      (let ((cmdlist cdlatex-command-alist-comb) item key doc text math)
        (while cmdlist
          (setq item (car cmdlist)
                cmdlist (cdr cmdlist)
                key (car item)
                doc (nth 1 item)
                text (nth 5 item)
                math (nth 6 item))
          (princ (format "%-10.10s %-58.58s %4s/%4s\n" key
                         (if (> (length doc) 59)
                             (substring doc 0 59)
                           doc)
                         (if text "TEXT" "")
                         (if math "MATH" ""))))))))

;;; ---------------------------------------------------------------------------
;;;
;;; Cursor position after insertion of forms

(defun cdlatex-position-cursor ()
  "Search back to question mark, delete it, leave point there."
  (if (search-backward "\?" (- (point) 100) t)
      (delete-char 1)))

;;; ---------------------------------------------------------------------------
;;;
;;; Environments
;;;
;;
;; The following code implements insertion of LaTeX environments
;; I prefer these environment over AUCTeX's definitions, since they give
;; my memory more support and don't prompt for anything.

(defvar cdlatex-env-alist-comb nil)

(defun cdlatex-environment (&optional environment item)
  "Complete the name of an ENVIRONMENT and insert it.
If the environment is not found in the list, a \\begin \\end pair is
inserted.  Any keywords AUTOLABEL will be replaced by an automatic label
statement.  Any keywords AUTOFILE will prompt the user for a file name
\(with completion) and insert that file name.  If a template starts with
\"\\\\\", the function will make sure that a double backslash occurs before
the template.  This is mainly useful for \"items\" of environments, where
\"\\\\\" is often needed as separator."
  (interactive)
  (if (cdlatex-region-active-p)
      (cdlatex-wrap-environment environment)
    (let ((env environment) begpos (endmarker (make-marker))
          (auto-label cdlatex-insert-auto-labels-in-env-templates)
          template prefix)
      (if (not env)
          (setq env (completing-read "Environment: " cdlatex-env-alist-comb nil nil "")))
      (if (looking-back "^\\([ \t]*\\)\\([^ \t].*\\)?" (point-at-bol))
          (progn
            (setq prefix (match-string 1))
            (if (match-end 2) (progn (newline) (insert prefix)))))
      (setq begpos (point))
      (if (try-completion env cdlatex-env-alist-comb)
          (progn
            (setq template (nth (if item 2 1)
                                (assoc env cdlatex-env-alist-comb)))
            (if (string= (substring template 0 2) "\\\\")
                ;; Need a double backslash to teminate previous item
                (progn
                  (setq template (substring template 2))
                  (if (not (save-excursion
                             (re-search-backward "\\\\\\\\[ \t\n]*\\="
                                                 (- (point) 20) t)))
                      (save-excursion
                        (skip-chars-backward " \t\n")
                        (insert "\\\\")))))
            (setq template
                  (replace-regexp-in-string "\n" (concat "\n" prefix) template))
            (insert template))
        (insert "\\begin{" env "}\n?\n\\end{" env "}\n"))
      (move-marker endmarker (point))

      ;; Look for AUTOFILE requests
      (goto-char begpos)
      (while (search-forward "AUTOFILE" (marker-position endmarker) t)
        (backward-delete-char 8)
        (call-interactively 'cdlatex-insert-filename))

      ;; Look for AUTOLABEL requests
      (goto-char begpos)
      (while (search-forward "AUTOLABEL" (marker-position endmarker) t)
        (backward-delete-char 9)
        (if (and auto-label (fboundp 'reftex-label))
            (reftex-label env)
          (save-excursion
            (beginning-of-line 1)
            (if (looking-at "[ \t]*\n")
                (kill-line 1)))))

      ;; Position cursor at the first question-mark
      (goto-char begpos)
      (if (search-forward "?" (marker-position endmarker) t)
          (backward-delete-char 1)))))

(defun cdlatex-wrap-environment (&optional environment)
  "Wrap the active region into ENVIRONMENT.
If the environment is not given, ask for it using completion."
  (let ((env environment)
        (beg (move-marker (make-marker) (region-beginning)))
        (end (move-marker (make-marker) (region-end))))
    (if (not env)
        (setq env (completing-read "Environment: "
                                   cdlatex-env-alist-comb nil nil "")))
    (goto-char beg)
    (if (not (looking-back "^[ \t]*" (point-at-bol))) (newline))
    (insert "\\begin{" env "}\n")
    (goto-char end)
    (if (not (looking-back "^[ \t]*" (point-at-bol))) (newline))
    (insert "\\end{" env "}\n")
    ))

(defun cdlatex-item ()
  "Insert an \\item and provide a label if the environments supports that.
In eqnarrays this inserts a new line with two ampersands.  It will also
add two backslashes to the previous line if required."
  (interactive)
  (let* ((env (car (car (reftex-what-environment t))))
         (envl (assoc env cdlatex-env-alist-comb)))

    (if (not env) (error "No open environment at point"))
    (if (or (< (length envl) 3)
            (null (nth 2 envl))
            (and (stringp (nth 2 envl))
                 (string= (nth 2 envl) "")))
        (error "No item defined for %s environment" env))
    (cdlatex-environment env t)))

(defun cdlatex-comment-at-point ()
  "Return t if point is inside a TeX comment."
  (let ((end (point))
        (start (progn (beginning-of-line 1) (point))))
    (goto-char end)
    (save-match-data
      (string-match "^%\\|[^\\]%" (buffer-substring start end)))))

(defun cdlatex-insert-filename (&optional absolute)
  "Insert a file name, with completion.
The path to the file will be relative to the current directory if the file
is in the current directory or a subdirectory.  Otherwise, the link will
be as completed in the minibuffer (i.e. normally relative to the users
HOME directory).
With optional prefix ABSOLUTE, insert the absolute path."
  (interactive "P")
  (let ((file (read-file-name "File: " nil "")))
    (let ((pwd (file-name-as-directory (expand-file-name "."))))
      (cond
       (absolute
        (insert (expand-file-name file)))
       ((string-match (concat "^" (regexp-quote pwd) "\\(.+\\)")
                      (expand-file-name file))
        (insert (match-string 1 (expand-file-name file))))
       (t (insert (expand-file-name file)))))))


;;; ===========================================================================
;;;
;;; Math characters and modifiers

;; The actual value of the following variable is calculated
;; by `cdlatex-compute-tables'.  It holds the number of levels of math symbols
(defvar cdlatex-math-symbol-no-of-levels 1)
(defvar cdlatex-math-symbol-alist-comb nil)
(defvar cdlatex-math-modify-alist-comb nil)

(defvar zmacs-regions)
(defun cdlatex-region-active-p ()
  "Is `transient-mark-mode' on and the region active?
Works on both Emacs and XEmacs."
  (if (featurep 'xmeacs)
      (and zmacs-regions (region-active-p))
    (and transient-mark-mode mark-active)))

(defun cdlatex-math-symbol ()
  "Read a char from keyboard and insert corresponding math char.
The combinations are defined in `cdlatex-math-symbol-alist'.  If not in a LaTeX
math environment, you also get a pair of dollars."
  (interactive)
  (let* ((cell (cdlatex-read-char-with-help
                cdlatex-math-symbol-alist-comb
                1 cdlatex-math-symbol-no-of-levels
                "Math symbol level %d of %d: "
                "AVAILABLE MATH SYMBOLS.  [%c]=next level "
                cdlatex-math-symbol-prefix
                (get 'cdlatex-math-symbol-alist-comb 'cdlatex-bindings)))
         (char (car cell))
         (level (cdr cell))
         (entry (assoc char cdlatex-math-symbol-alist-comb))
         (symbol (nth level entry)))

    (if (or (not symbol)
            (not (stringp symbol))
            (equal symbol ""))
        (error "No such math symbol %c on level %d" char level))

    (cdlatex-ensure-math)

    (insert symbol)
    (when (string-match "\\?" symbol)
      (cdlatex-position-cursor))))

(defun cdlatex-read-char-with-help (alist start-level max-level prompt-format
                                          header-format prefix bindings)
  "Read a char from keyboard and provide help if necessary."
  (interactive)
  (let (char (help-is-on nil)
             (level start-level))
    (catch 'exit
      (save-window-excursion
        (while t
          (if help-is-on
              (progn
                (cdlatex-turn-on-help
                 (concat (format header-format prefix)
                         (if (assoc level bindings)
                             (concat "  Direct binding are `"
                                     (cdr (assoc level bindings)) "' etc.")
                           ""))
                 level alist help-is-on nil)))
          (message prompt-format level max-level)
          (if (and (not help-is-on)
                   (sit-for cdlatex-auto-help-delay))
              (setq char ?\?)
            (setq char (read-char)))
          (cond
           ((= char ?\C-g)
            (keyboard-quit))
           ((= char ?\?)
            (if help-is-on
                (progn
                  (setq help-is-on (+ help-is-on (- (window-height) 1)))
                  (if (> help-is-on (count-lines (point-min) (point-max)))
                      (setq help-is-on 1)))
              (setq help-is-on 1)))
           ((or (= char ?\ )
                (equal char prefix))
            (setq level (if (= level cdlatex-math-symbol-no-of-levels)
                            1
                          (1+ level))))
           (t (throw 'exit (cons char level)))))))))

;; The following code implements the possibility to modify a character
;; by an accent or style when point is behind it.  This is more naturally
;; then the usual way.  E.g. \tilde{a}  can be typed as a'~

(defun cdlatex-math-modify (arg)
  "Modify previous char/group/macro with math accent/style.
This macro modifies the character or TeX macro or TeX group BEFORE point
with a math accent or a style.
If the character before point is white space, an empty modifying form
is inserted and the cursor positioned properly.
If the object before point looks like word, this macro modifies the last
character of it.  The math-related changes happen only when the cursor
is actually inside a LaTeX math environment.
Some of the modification characters denote font style changes like bold
or italic.  These also work in text mode, on the word before point or,
if applicable, the active region.
  In normal text, it does just a self-insert.
The accent and style commands and their properties are defined in the
constant `cdlatex-math-modify-alist'."
  (interactive "P")
  (catch 'exit

    (let ((inside-math (cdlatex--texmathp))
          (win (selected-window))
          (savedpos (make-marker))
          char (help-is-on nil) ass acc rmdot it cmd extrabrac
          before after)
      (catch 'exit1
        (save-window-excursion
          (while t
            (if help-is-on
                (progn
                  (cdlatex-turn-on-help
                   "AVAILABLE MODIFIERS. (?=SCROLL)"
                   (if inside-math 1 2)
                   cdlatex-math-modify-alist-comb help-is-on t)
                  (message "Math modify: "))
              (message "Math modify: (?=HELP)"))

            (if (and (not help-is-on)
                     (sit-for cdlatex-auto-help-delay))
                (setq char ?\?)
              (setq char (read-char)))

            (cond
             ((= char ?\C-g)
              (keyboard-quit))
             ((= char ?\?)
              (if help-is-on
                  (progn
                    (setq help-is-on (+ help-is-on (- (window-height) 1)))
                    (if (> help-is-on (count-lines (point-min) (point-max)))
                        (setq help-is-on 1)))
                (setq help-is-on 1)))
             ((equal char cdlatex-math-modify-prefix)
              (select-window win)
              (insert cdlatex-math-modify-prefix)
              (message "")
              (throw 'exit t))
             (t (throw 'exit1 t))))))
      (message "")
      (setq ass (assoc char cdlatex-math-modify-alist-comb))
      (if (not ass)
          (progn
            (insert cdlatex-math-modify-prefix char)
            (throw 'exit t)))
      (setq ass    (cdr ass))
      (setq cmd    (nth (if inside-math 0 1) ass))
      (setq acc    (nth 2 ass))
      (setq rmdot  (nth 3 ass))
      (setq it     (nth 4 ass))
      (if (not cmd)
          (progn
            (message "No such modifier `%c' %s math mode" char
                     (if inside-math "inside" "outside"))
            (insert cdlatex-math-modify-prefix char)
            (throw 'exit t)))
      (if (string-match "\\(.*\\)\\?\\(.*\\)" cmd)
          (setq before (match-string 1 cmd) after (match-string 2 cmd)))
      (cond
       ((cdlatex-region-active-p)
        (let ((beg (min (region-beginning) (region-end)))
              (end (max (region-beginning) (region-end))))
          (goto-char end)
          (move-marker savedpos (point))
          (goto-char beg)
          (if before
              (insert before)
            (insert "{")
            (if acc (forward-char -1))
            (insert cmd)
            (if (not acc) (insert " ")))
          (goto-char savedpos)
          (if after
              (insert after)
            (insert "}"))))
       (arg
        (move-marker savedpos (point))
        (backward-word arg)
        (if before
            (insert before)
          (insert "{")
          (if acc (forward-char -1))
          (insert cmd)
          (if (not acc) (insert " ")))
        (goto-char savedpos)
        (if after
            (insert after)
          (insert "}")))
       ((or (bolp)
            (not cdlatex-modify-backwards)
            (memq (preceding-char) '(?\  ?$ ?- ?{ ?\( )))
        ;; Just insert empty form and position cursor
        (if (string-match "\\?" cmd)
            (insert cmd)
          (if acc
              (insert cmd "{?")
            (insert "{" cmd " ?"))
          (if it (insert "\\/"))
          (insert "}"))
        (search-backward "?")
        (delete-char 1))
       (t
        ;; Modify preceding character or word
        (move-marker savedpos (point))
        (if (= (preceding-char) ?\})
            ;; its a group
            (progn (setq extrabrac nil)
                   (backward-list 1)
                   (if (not acc) (forward-char 1)))
          ;; not a group
          (forward-char -1)
          (if (looking-at "[a-zA-Z]")
              ;; a character: look if word or macro
              (progn
                (setq extrabrac t)
                (re-search-backward "[^a-zA-Z]")
                (cond
                 ((= (following-char) ?\\))
                 ((not inside-math) (forward-char 1))
                 (t (goto-char savedpos)
                    (forward-char -1)
                    (if (and rmdot (let (case-fold-search) (looking-at "[ij]")))
                        (progn (insert "\\")
                               (forward-char 1)
                               (insert "math")
                               (move-marker savedpos (point))
                               (forward-char -6))))))
            (setq extrabrac t)))
        (if extrabrac (progn (insert "{")
                             (if acc (forward-char -1))))
        (insert cmd)
        (if (not acc) (insert " "))
        (goto-char savedpos)
        (if extrabrac (insert "}")))))))

;; And here is the help function for the symbol insertions stuff

(defun cdlatex-turn-on-help (header level alist offset &optional sparse)
  "Show help-window for ALIST."
  (let ((cnt 0) (all-chars "")
        (flock (cdlatex-use-fonts)) this-char value)
    (if sparse
        (setq all-chars (concat (mapcar #'car alist)))
      (setq all-chars "aA0 bB1!cC2@dD3#eE4$fF5%gG6^hH7&iI8
jJ9?kK+~lL-_mM*|nN/\\oO=\"pP()qQ[]rR{}sS<>tT`'uU.:vV

wW

xX

yY

zZ

"))
    (if (get-buffer-window " *CDLaTeX Help*")
        (select-window (get-buffer-window " *CDLaTeX Help*"))
      (switch-to-buffer-other-window " *CDLaTeX Help*"))
    (if buffer-read-only (read-only-mode 'toggle))
    (erase-buffer)
    (make-local-variable 'truncate-lines)
    (setq truncate-lines t)
    (insert (concat header "\n\n"))

    (while (not (equal "" all-chars))
      (setq cnt (1+ cnt))
      (setq this-char (string-to-char all-chars))
      (setq all-chars (substring all-chars 1))
      (cond
       ( (= this-char ?\?)
         (setq value "SCROLL"))
       ( (member this-char '(?\C-j ?\C-m))
         (setq this-char ?\ )
         (setq value ""))
       ( t
         (setq value (nth level (assoc this-char alist)))
         (if (not value) (setq value ""))
         (if (string-match "\\(.*?\\) \\? \\(.*\\)" value)
             (setq value (concat (match-string 1 value) (match-string 2 value))))))
      (setq this-char (char-to-string this-char)
            value (if (> (length value) 15)
                      (concat (substring value 0 13) "..")
                    (substring (concat value "               ") 0 15)))
      (if flock
          (put-text-property 0  15
                             'face 'font-lock-keyword-face value))

      (insert this-char "  " value "  ")
      (if (= (* 4 (/ cnt 4)) cnt) (insert "\n")))
    (unless (one-window-p t)
      (enlarge-window (1+(- (count-lines 1 (point)) (window-height)))))
    (goto-char (point-min)) (forward-line (1- offset))
    (beginning-of-line 1)
    (recenter 0)))

;;; ---------------------------------------------------------------------------
;;;
;;; Data Section: Definition of large constants

(defconst cdlatex-command-alist-default
  '(
    ("pref"      "Make page reference"
     "" reftex-reference nil t t)
    ("ref"       "Make reference"
     "" reftex-reference nil t t)

    ("lbl"       "Insert automatic label at point"
     "" reftex-label nil t t)

    ("ct"        "Insert \\cite"
     "\\cite{?}" cdlatex-position-cursor nil t nil)
    ("cte"       "Make a citation interactively"
     "" reftex-citation nil t nil)
    ("cite{"       "Make a citation interactively"
     "cite{" reftex-citation nil t nil)

    ("beg"       "Complete an environment name and insert template"
     "" cdlatex-environment nil t t)
    ("env"       "Complete an environment name and insert template"
     "" cdlatex-environment nil t t)
    ("it"        "New item in current environment"
     "" cdlatex-item nil t t)
    ("ite"       "Insert an ITEMIZE environment template"
     "" cdlatex-environment ("itemize") t nil)
    ("enu"       "Insert an ENUMERATE environment template"
     "" cdlatex-environment ("enumerate") t nil)
    ("equ"       "Insert an EQUATION environment template"
     "" cdlatex-environment ("equation") t nil)
    ("eqn"       "Insert an EQUATION environment template"
     "" cdlatex-environment ("eqnarray") t nil)
    ("ali"       "Insert an ALIGN environment template"
     "" cdlatex-environment ("align") t nil)
    ("ali*"      "Insert an ALIGN* environment template"
     "" cdlatex-environment ("align*") t nil)
    ("alit"      "Insert an ALIGNAT environment template"
     "" cdlatex-environment ("alignat") t nil)
    ("alit*"     "Insert an ALIGNAT* environment template"
     "" cdlatex-environment ("alignat*") t nil)
    ("xal"       "Insert a XALIGNAT environment template"
     "" cdlatex-environment ("xalignat") t nil)
    ("xal*"      "Insert a XALIGNAT* environment template"
     "" cdlatex-environment ("xalignat*") t nil)
    ("xxa"       "Insert a XXALIGNAT environment template"
     "" cdlatex-environment ("xxalignat") t nil)
    ("xxa*"      "Insert a XXALIGNAT environment template"
     "" cdlatex-environment ("xxalignat") t nil)
    ("mul"       "Insert a MULTINE environment template"
     "" cdlatex-environment ("multline") t nil)
    ("mul*"      "Insert a MULTINE* environment template"
     "" cdlatex-environment ("multline*") t nil)
    ("gat"       "Insert a GATHER environment template"
     "" cdlatex-environment ("gather") t nil)
    ("gat*"      "Insert a GATHER* environment template"
     "" cdlatex-environment ("gather*") t nil)
    ("spl"       "Insert SPLIT environment template"
     "" cdlatex-environment ("split") nil t)
    ("fla"       "Insert a FLALIGN environment template"
     "" cdlatex-environment ("flalign") t nil)
    ("fla*"      "Insert a FLALIGN* environment template"
     "" cdlatex-environment ("flalign*") t nil)
    ("fg"        "Insert a FIGURE environment template"
     "" cdlatex-environment ("figure") t nil)

    ("sn"        "Insert a \\section{} statement"
     "\\section{?}" cdlatex-position-cursor nil t nil)
    ("ss"        "Insert a \\subsection{} statement"
     "\\subsection{?}" cdlatex-position-cursor nil t nil)
    ("sss"       "Insert a \\subsubsection{} statement"
     "\\subsubsection{?}" cdlatex-position-cursor nil t nil)
    ("pf"        "Insert a \\paragraph{} statement"
     "\\paragraph{?}" cdlatex-position-cursor nil t nil)
    ("sp"        "Insert a \\subparagraph{} statement"
     "\\subparagraph{?}" cdlatex-position-cursor nil t nil)
    ("ssp"       "Insert a \\subsubparagraph{} statement"
     "\\subsubparagraph{?}" cdlatex-position-cursor nil t nil)

    ("cl"        "Insert \\centerline"
     "\\centerline{?}" cdlatex-position-cursor nil t nil)
    ("inc"        "Insert \\includegraphics with file name"
     "\\includegraphics[]{?}" (lambda ()
                                (cdlatex-position-cursor)
                                (call-interactively 'cdlatex-insert-filename)
                                (forward-char 1))
     nil t nil)
    ("lr("       "Insert a \\left( \\right) pair"
     "(" cdlatex-lr-pair nil nil t)
    ("lr["        "Insert a \\left[ \\right] pair"
     "[" cdlatex-lr-pair nil nil t)
    ("lr{"        "Insert a \\left{ \\right} pair"
     "{" cdlatex-lr-pair nil nil t)
    ("lr<"        "Insert a \\left\\langle \\right\\rangle pair"
     "<" cdlatex-lr-pair nil nil t)
    ("lr|"        "Insert a \\left| \\right| pair"
     "|" cdlatex-lr-pair nil nil t)
    ("caseeq"     "Insert a = { construct"
     "\\left\\{ \n\\begin{array}{l@{\\quad:\\quad}l}\n? & \\\\\n & \n\\end{array}\\right." cdlatex-position-cursor nil nil t)
    ("fr"         "Insert \\frac{}{}"
     "\\frac{?}{}"           cdlatex-position-cursor nil nil t)
    ("sq"         "Insert \\sqrt{}"
     "\\sqrt{?}"             cdlatex-position-cursor nil nil t)
    ("intl"       "Insert \\int\\limits_{}^{}"
     "\\int\\limits_{?}^{}"  cdlatex-position-cursor nil nil t)
    ("suml"       "Insert \\sum\\limits_{}^{}"
     "\\sum\\limits_{?}^{}"  cdlatex-position-cursor nil nil t)
    ("nonum"      "Insert \\nonumber\\\\"
     "\\nonumber\\\\\n"      nil nil nil t)
    ("fn"         "Make a footnote"
     "\\footnote{?}" cdlatex-position-cursor nil t nil)
    ("qq"         "Insert \\quad"
     "\\quad"        nil nil t t)
    ("qqq"        "Insert \\qquad"
     "\\qquad"       nil nil t t)
    )
  "Default for `cdlatex-command-alist'.")

(defconst cdlatex-math-modify-alist-default
  '(
    ( ?\.   "\\dot"               nil        t   t   nil )
    ( ?\:   "\\ddot"              nil        t   t   nil )
    ( ?\~   "\\tilde"             nil        t   t   nil )
    ( ?N    "\\widetilde"         nil        t   t   nil )
    ( ?o    "\\mathring"          nil        t   t   nil )
    ( ?^    "\\hat"               nil        t   t   nil )
    ( ?H    "\\widehat"           nil        t   t   nil )
    ( ?\-   "\\bar"               nil        t   t   nil )
    ( ?T    "\\overline"          nil        t   nil nil )
    ( ?\_   "\\underline"         nil        t   nil nil )
    ( ?\]   "\\overbrace"         nil        t   nil nil )
    ( ?\}   "\\underbrace"        nil        t   nil nil )
    ( ?\>   "\\vec"               nil        t   t   nil )
    ( ?/    "\\grave"             nil        t   t   nil )
    ( ?\\   "\\acute"             nil        t   t   nil )
    ( ?v    "\\check"             nil        t   t   nil )
    ( ?u    "\\breve"             nil        t   t   nil )
    ( ?m    "\\mbox"              nil        t   nil nil )
    ( ?t    "\\text"              nil        t   nil nil )
    ( ?c    "\\mathcal"           nil        t   nil nil )
    ( ?r    "\\mathrm"            "\\textrm" t   nil nil )
    ( ?i    "\\mathit"            "\\textit" t   nil nil )
    ( ?l    nil                   "\\textsl" t   nil nil )
    ( ?b    "\\mathbf"            "\\textbf" t   nil nil )
    ( ?e    "\\mathem"            "\\emph"   t   nil nil )
    ( ?y    "\\mathtt"            "\\texttt" t   nil nil )
    ( ?f    "\\mathsf"            "\\textsf" t   nil nil )
    ( ?0    "\\textstyle"         nil        nil nil nil )
    ( ?1    "\\displaystyle"      nil        nil nil nil )
    ( ?2    "\\scriptstyle"       nil        nil nil nil )
    ( ?3    "\\scriptscriptstyle" nil        nil nil nil )
    ( ?\(   "\\left( ? \\right)"  nil        nil nil nil )
    ( ?\[   "\\left[ ? \\right]"  nil        nil nil nil )
    ( ?\{   "\\left\\{ ? \\right\\}" nil     nil nil nil )
    ( ?<    "\\left< ? \\right>"  nil        nil nil nil )
    ( ?|    "\\left| ? \\right|"  nil        nil nil nil )
    )
  "Default for `cdlatex-math-modify-alist'.")

(defconst cdlatex-math-symbol-alist-default
  '(
    ( ?a  ("\\alpha"          ))
    ( ?A  ("\\forall"         "\\aleph"))
    ( ?b  ("\\beta"           ))
    ( ?B  (""                 ))
    ( ?c  (""                 ""                "\\cos"))
    ( ?C  (""                 ""                "\\arccos"))
    ( ?d  ("\\delta"          "\\partial"))
    ( ?D  ("\\Delta"          "\\nabla"))
    ( ?e  ("\\epsilon"        "\\varepsilon"    "\\exp"))
    ( ?E  ("\\exists"         ""                "\\ln"))
    ( ?f  ("\\phi"            "\\varphi"))
    ( ?F  (""                 ))
    ( ?g  ("\\gamma"          ""                "\\lg"))
    ( ?G  ("\\Gamma"          ""                "10^{?}"))
    ( ?h  ("\\eta"            "\\hbar"))
    ( ?H  (""                 ))
    ( ?i  ("\\in"             "\\imath"))
    ( ?I  (""                 "\\Im"))
    ( ?j  (""                 "\\jmath"))
    ( ?J  (""                 ))
    ( ?k  ("\\kappa"          ))
    ( ?K  (""                 ))
    ( ?l  ("\\lambda"         "\\ell"           "\\log"))
    ( ?L  ("\\Lambda"         ))
    ( ?m  ("\\mu"             ))
    ( ?M  (""                 ))
    ( ?n  ("\\nu"             ""                "\\ln"))
    ( ?N  ("\\nabla"          ""                "\\exp"))
    ( ?o  ("\\omega"          ))
    ( ?O  ("\\Omega"          "\\mho"))
    ( ?p  ("\\pi"             "\\varpi"))
    ( ?P  ("\\Pi"             ))
    ( ?q  ("\\theta"          "\\vartheta"))
    ( ?Q  ("\\Theta"          ))
    ( ?r  ("\\rho"            "\\varrho"))
    ( ?R  (""                 "\\Re"))
    ( ?s  ("\\sigma"          "\\varsigma"      "\\sin"))
    ( ?S  ("\\Sigma"          ""                "\\arcsin"))
    ( ?t  ("\\tau"            ""                "\\tan"))
    ( ?T  (""                 ""                "\\arctan"))
    ( ?u  ("\\upsilon"        ))
    ( ?U  ("\\Upsilon"        ))
    ( ?v  ("\\vee"            ))
    ( ?V  ("\\Phi"            ))
    ( ?w  ("\\xi"             ))
    ( ?W  ("\\Xi"             ))
    ( ?x  ("\\chi"            ))
    ( ?X  (""                 ))
    ( ?y  ("\\psi"            ))
    ( ?Y  ("\\Psi"            ))
    ( ?z  ("\\zeta"           ))
    ( ?Z  (""                 ))
    ( ?   (""                 ))
    ( ?0  ("\\emptyset"       ))
    ( ?1  (""                 ))
    ( ?2  (""                 ))
    ( ?3  (""                 ))
    ( ?4  (""                 ))
    ( ?5  (""                 ))
    ( ?6  (""                 ))
    ( ?7  (""                 ))
    ( ?8  ("\\infty"          ))
    ( ?9  (""                 ))
    ( ?!  ("\\neg"            ))
    ( ?@  (""                 ))
    ( ?#  (""                 ))
    ( ?$  (""                 ))
    ( ?%  (""                 ))
    ( ?^  ("\\uparrow"        ))
    ( ?&  ("\\wedge"          ))
    ( ?\? (""                 ))
    ( ?~  ("\\approx"         "\\simeq"))
    ( ?_  ("\\downarrow"      ))
    ( ?+  ("\\cup"            ))
    ( ?-  ("\\leftrightarrow" "\\longleftrightarrow" ))
    ( ?*  ("\\times"          ))
    ( ?/  ("\\not"            ))
    ( ?|  ("\\mapsto"         "\\longmapsto"))
    ( ?\\ ("\\setminus"       ))
    ( ?\" (""                 ))
    ( ?=  ("\\Leftrightarrow" "\\Longleftrightarrow"))
    ( ?\( ("\\langle"         ))
    ( ?\) ("\\rangle"         ))
    ( ?\[ ("\\Leftarrow"      "\\Longleftarrow"))
    ( ?\] ("\\Rightarrow"     "\\Longrightarrow"))
    ( ?{  ("\\subset"         ))
    ( ?}  ("\\supset"         ))
    ( ?<  ("\\leftarrow"      "\\longleftarrow"     "\\min"))
    ( ?>  ("\\rightarrow"     "\\longrightarrow"    "\\max"))
    ( ?`  (""                 ))
    ( ?'  ("\\prime"          ))
    ( ?.  ("\\cdot"           ))
    )
  "Default for `cdlatex-math-symbol-alist'.")

;;; ---------------------------------------------------------------------------

(defconst cdlatex-env-alist-default
  '(
;;------------------------------------
( "abstract"
"\\begin{abstract}
?
\\end{abstract}"
nil
)
;;------------------------------------
( "appendix"
"\\begin{appendix}
?
\\end{appendix}"
nil
)
;;------------------------------------
( "array"
"\\begin{array}[tb]{?lcrp{width}*{num}{lcrp{}}|}
 & & & \\\\
\\end{array}"
" & & &"
)
;;------------------------------------
( "center"
"\\begin{center}
? \\\\
\\end{center}"
nil
)
;;------------------------------------
( "deflist"
"\\begin{deflist}{width-text}
\\item ?
\\end{deflist}"
"\\item ?"
)
;;------------------------------------
( "description"
"\\begin{description}
\\item[?]
\\end{description}"
"\\item[?] "
)
;;------------------------------------
( "displaymath"
"\\begin{displaymath}
?
\\end{displaymath}"
nil
)
;;------------------------------------
( "document"
"\\begin{document}
?
\\end{document}"
nil
)
;;------------------------------------
( "enumerate"
"\\begin{enumerate}
\\itemAUTOLABEL ?
\\end{enumerate}"
"\\itemAUTOLABEL ?"
)
;;------------------------------------
( "eqnarray"
"\\begin{eqnarray}
AUTOLABEL
? &  & \\\\
\\end{eqnarray}"
"\\\\AUTOLABEL
? &  & "
)
;;------------------------------------
( "eqnarray*"
"\\begin{eqnarray*}
? & & \\\\
\\end{eqnarray*}"
"\\\\? & & "
)
;;------------------------------------
( "equation"
"\\begin{equation}
AUTOLABEL
?
\\end{equation}"
nil
)
;;------------------------------------
( "figure"
"\\begin{figure}[htbp]
\\centerline{\\includegraphics[]{AUTOFILE}}
\\caption[]{AUTOLABEL ?}
\\end{figure}"
nil
)
;;------------------------------------
( "figure*"
"\\begin{figure*}[htbp]
\\centerline{\includegraphics[]{AUTOFILE}
\\end{figure*}"
nil
)
;;------------------------------------
( "flushleft"
"\\begin{flushleft}
? \\\\
\\end{flushleft}"
"\\\\?"
)
;;------------------------------------
( "flushright"
"\\begin{flushright}
? \\\\
\\end{flushright}"
"\\\\?"
)
;;------------------------------------
( "fussypar"
"\\begin{fussypar}
?
\\end{fussypar}"
nil
)
;;------------------------------------
( "itemize"
"\\begin{itemize}
\\item ?
\\end{itemize}"
"\\item ?"
)
;;------------------------------------
( "letter"
"\\begin{letter}
?
\\end{letter}"
nil
)
;;------------------------------------
( "list"
"\\begin{list}{}{}
\\item ?
\\end{list}"
"\\item ?"
)
;;------------------------------------
( "math"
"\\begin{math}
?
\\end{math}"
nil
)
;;------------------------------------
( "minipage"
"\\begin{minipage}[bt]{?.cm}

\\end{minipage}"
nil
)
;;------------------------------------
( "picture"
"\\begin{picture}(,)(,)
?
\\end{picture}"
nil
)
;;------------------------------------
( "quotation"
"\\begin{quotation}
?
\\end{quotation}"
nil
)
;;------------------------------------
( "quote"
"\\begin{quote}
?
\\end{quote}"
nil
)
;;------------------------------------
( "sloppypar"
"\\begin{sloppypar}
?
\\end{sloppypar}"
nil
)
;;------------------------------------
( "tabbing"
"\\begin{tabbing}
? \\=  \\=  \\=  \\= \\\\[0.5ex]
 \\>  \\>  \\>  \\> \\\\
\\end{tabbing}"
"\\\\?"
)
;;------------------------------------
( "table"
"\\begin{table}[htbp]
\\caption[]{AUTOLABEL ?}
\\vspace{4mm}

\\end{table}"
nil
)
;;------------------------------------
( "tabular"
"\\begin{tabular}[tb]{lcrp{width}*{num}{lcrp{}}|}
?
\\end{tabular}"
nil
)
;;------------------------------------
( "tabular*"
"\\begin{tabular*}{width}[tb]{lcrp{width}*{num}{lcrp{}}|}
?
\\end{tabular*}"
nil
)
;;------------------------------------
( "thebibliography"
"\\begin{thebibliography}{}

\\bibitem[?]{}

\\end{thebibliography}"
"
\\bibitem[?]{}
")
;;------------------------------------
( "theindex"
"\\begin{theindex}
?
\\end{theindex}"
nil
)
;;------------------------------------
( "titlepage"
"\\begin{titlepage}

\\title{?}

\\author{
 \\\\
 \\\\
%\\thanks{}
%\\and
}

\\date{\\today}

\\end{titlepage}"
nil
)
;;------------------------------------
( "trivlist"
"\\begin{trivlist}
?
\\end{trivlist}"
nil
)
;;------------------------------------
( "verbatim"
"\\begin{verbatim}
?
\\end{verbatim}"
nil
)
;;------------------------------------
( "verbatim*"
"\\begin{verbatim*}
?
\\end{verbatim*}"
nil
)
;;------------------------------------
( "verse"
"\\begin{verse}
? \\\\
\\end{verse}"
nil
)
;;------------------------------------
;; AMS-LaTeX
( "align"
"\\begin{align}
AUTOLABEL
?
\\end{align}"
"\\\\AUTOLABEL
?")
;;------------------------------------
( "align*"
"\\begin{align*}
?
\\end{align*}"
"\\\\?")
;;------------------------------------
( "alignat"
"\\begin{alignat}{?}
AUTOLABEL

\\end{alignat}"
"\\\\AUTOLABEL
?")
;;------------------------------------
( "alignat*"
"\\begin{alignat*}{?}

\\end{alignat*}"
"\\\\?")
;;------------------------------------
( "xalignat"
"\\begin{xalignat}{?}
AUTOLABEL

\\end{xalignat}"
"\\\\AUTOLABEL
?")
;;------------------------------------
( "xalignat*"
"\\begin{xalignat*}{?}

\\end{xalignat*}"
"\\\\?")
;;------------------------------------
( "xxalignat"
"\\begin{xxalignat}{?}

\\end{xxalignat}"
"\\\\?")
;;------------------------------------
("multline"
"\\begin{multline}
AUTOLABEL
?
\\end{multline}"
"\\\\AUTOLABEL
?")
;;------------------------------------
("multline*"
"\\begin{multline*}
?
\\end{multline*}"
"?")
;;------------------------------------
( "flalign"
"\\begin{flalign}
AUTOLABEL
?
\\end{flalign}"
"\\\\AUTOLABEL
?"
)
;;------------------------------------
( "flalign*"
"\\begin{flalign*}
?
\\end{flalign*}"
"\\\\?"
)
;;------------------------------------
( "gather"
"\\begin{gather}
AUTOLABEL
?
\\end{gather}"
"\\\\AUTOLABEL
?")
;;------------------------------------
( "gather*"
"\\begin{gather*}
?
\\end{gather*}"
"\\\\?")
;;------------------------------------
( "split"
"\\begin{split}
?
\\end{split}"
"\\\\?")
;;------------------------------------
;;; SOME NON-STANDARD ENVIRONMENTS
;; figure environment for the epsf macro package
( "epsfigure"
"\\begin{figure}[htbp]
\\centerline{\\epsfxsize=\\textwidth \\epsffile{?.eps}}
\\caption[]{AUTOLABEL}
\\end{figure}"
nil
)
;;------------------------------------
;; table environment for AASTeX
( "deluxetable"
"\\begin{deluxetable}{?lcrp{width}*{num}{lcrp{}}}
\\tablecolumns{}
\\tablewidth{0pt}
\\tablecaption{AUTOLABEL }
\\tablehead{ \\colhead{} & \colhead{} & \\multicolumn{3}{c}{} }
\\startdata
 &  & \\nl
\\enddata
\\end{deluxetable}"
nil
)
;;------------------------------------
;; figure environments for A&A
( "aafigure"
"\\begin{figure}
\\resizebox{\\hsize}{!}{\\includegraphics{?.eps}}
\\caption[]{AUTOLABEL}
\\end{figure}"
nil
)
;;------------------------------------
( "aafigure*"
"\\begin{figure*}
\\resizebox{12cm}{!}{\\includegraphics{?.eps}}
\\caption[]{AUTOLABEL}
\\end{figure*}"
nil
)
))

;;; ---------------------------------------------------------------------------
;;;
;;; Functions to compile the tables, reset the mode etc.

(defun cdlatex-reset-mode ()
  "Reset CDLaTeX Mode.
Required to implement changes to some list variables.  This
function will compile the information in `cdlatex-label-alist'
and similar variables.  It is called when CDLaTeX is first used,
and after changes to these variables via
`cdlatex-add-to-label-alist'."
  (interactive)
  (cdlatex-compute-tables))

(defalias 'cdlatex-nop #'ignore)

(defun cdlatex-compute-tables ()
  "Update tables not connected with ref and cite support."
  (setq cdlatex-env-alist-comb
        (cdlatex-uniquify
         (append cdlatex-env-alist
                 cdlatex-env-alist-default)))
  ;; Merge the user specified lists with the defaults
  (setq cdlatex-command-alist-comb
        (cdlatex-uniquify
         (append cdlatex-command-alist
                 cdlatex-command-alist-default)))
  (setq cdlatex-math-symbol-alist-comb
        (cdlatex-uniquify
         (mapcar (lambda (x)
                   (if (listp (nth 1 x))
                       (cons (car x) (nth 1 x))
                     x))
                 (append cdlatex-math-symbol-alist
                         cdlatex-math-symbol-alist-default))))
  (setq cdlatex-math-modify-alist-comb
        (cdlatex-uniquify
         (append cdlatex-math-modify-alist
                 cdlatex-math-modify-alist-default)))

  ;; find out how many levels are needed for the math symbol stuff
  (setq cdlatex-math-symbol-no-of-levels
        (1- (apply #'max (mapcar #'length cdlatex-math-symbol-alist-comb))))

  ;; The direct key bindings.
  (let (map dummy-map prefix modifiers symbol bindings)
    (cl-loop for level from 1 to cdlatex-math-symbol-no-of-levels do
             (setq dummy-map (make-sparse-keymap))
             (setq prefix (car (nth (1- level)
                                    cdlatex-math-symbol-direct-bindings)))
             (setq modifiers (cdr
                              (nth (1- level)
                                   cdlatex-math-symbol-direct-bindings)))
             (when (or prefix modifiers)
               (cond
                ((stringp prefix) (setq prefix (read-kbd-macro prefix)))
                ((integerp prefix) (setq prefix (vector prefix))))

               (if (null prefix)
                   (setq map cdlatex-mode-map)
                 (setq map (make-keymap))
                 (define-key cdlatex-mode-map prefix (setq map
                                                           (make-keymap))))
               (define-key dummy-map
                           (vector (append modifiers (list ?a))) #'cdlatex-nop)
               (push (cons level (substitute-command-keys
                                  "\\<dummy-map>\\[cdlatex-nop]"))
                     bindings)
               (mapc (lambda (entry)
                       (setq symbol (nth level entry))
                       (when (and symbol (stringp symbol)
                                  (not (equal "" symbol)))
                         (define-key
                          map (vector (append modifiers (list (car entry))))
                          (list 'lambda '() '(interactive)
                                (list 'cdlatex-insert-math symbol)))))
                     cdlatex-math-symbol-alist-comb)))
    (put 'cdlatex-math-symbol-alist-comb 'cdlatex-bindings bindings)))

(defun cdlatex-insert-math (string)
  "Insert STRING in math-mode."
  (cdlatex-ensure-math)
  (insert string)
  (if (string-match "\\?" string)
      (cdlatex-position-cursor)))

;;; cdlatex-electric-index-mode

(defgroup cdlatex-electricindex nil
  "LaTeX electric digit indices."
  :tag "cdlatex-electricindex"
  :link '(url-link :tag "Home Page" "https://github.com/cdominik/cdlatex")
  :prefix "cdlatex-electricindex-"
  :group 'tex)

(defvar cdlatex-electricindex-mode nil
  "Determines if cdlatex-electricindex minor mode is active.")
(make-variable-buffer-local 'cdlatex-electricindex-mode)

(defvar cdlatex-electricindex-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map  "1"        #'cdlatex-electricindex-digit)
    (define-key map  "2"        #'cdlatex-electricindex-digit)
    (define-key map  "3"        #'cdlatex-electricindex-digit)
    (define-key map  "4"        #'cdlatex-electricindex-digit)
    (define-key map  "5"        #'cdlatex-electricindex-digit)
    (define-key map  "6"        #'cdlatex-electricindex-digit)
    (define-key map  "7"        #'cdlatex-electricindex-digit)
    (define-key map  "8"        #'cdlatex-electricindex-digit)
    (define-key map  "9"        #'cdlatex-electricindex-digit)
    (define-key map  "0"        #'cdlatex-electricindex-digit)
    map)
  "Keymap for cdlatex-electricindex minor mode.")

;;;###autoload
(defun turn-on-cdlatex-electricindex ()
  "Turn on cdlatex-electricindex minor mode."
  (cdlatex-electricindex-mode t))

;;;###autoload
(define-minor-mode cdlatex-electricindex-mode
  "Minor mode for electric insertion of numbered indixes.

cdlatex-electricindex is a minor mode supporting fast digit index
insertation in LaTeX math. For example typing x 1 2 will insert
x_{12}.

To turn cdlatex-electricindex Minor Mode on and off in a
particular buffer, use `M-x cdlatex-electricindex-mode'.

To turn on cdlatex-electricindex Minor Mode for all LaTeX files,
add one of the following lines to your .emacs file:

    (add-hook 'latex-mode-hook #'turn-on-cdlatex-electricindex)

This index insertion will only work when the cursor is in a LaTeX
math environment, based on (texmathp). If texmathp is not
available, math math-mode will be assumed.

Entering `cdlatex-electricindex-mode' calls the hook
`cdlatex-electricindex-mode-hook'."
  :lighter " EI")

(defun cdlatex-electricindex-active-here ()
  (if (eq major-mode 'latex-mode)
      (if (fboundp 'texmathp)
          (texmathp)
        t)
    t))

(defun cdlatex-electricindex-digit ()
  "Insert digit, maybe as an index to a quantity in math environment."
  (interactive)
  (if (not (cdlatex-electricindex-active-here))
      (self-insert-command 1)
    (let ((digit (char-to-string (event-basic-type last-command-event))))
      (if (looking-back "[a-zA-Z]" (1- (point)))
          (insert "_" digit " ")
        (if (looking-back "\\(_[0-9]\\) ?" (- (point) 3))
            (progn
              (goto-char (match-beginning 1))
              (forward-char 1)
              (insert "{")
              (forward-char 1)
              (insert digit "}")
              (if (looking-at " ")
                  (forward-char 1)
                (insert " ")))
          (if (looking-back "_{\\([0-9]+\\)} ?"
                            (max (- (point) 10) (point-min)))
              (save-excursion
                (goto-char (match-end 1))
                (insert digit))
            (self-insert-command 1)))))))

;;; Menus --------------------------------------------------------------------

;; Define a menu for the menu bar if Emacs is running under X

(require 'easymenu)

(easy-menu-define
 cdlatex-mode-menu cdlatex-mode-map
 "Menu used in CDLaTeX mode."
 '("CDLTX"
   ["\\begin{...} \\label"   cdlatex-environment t]
   ["\\item \\label"         cdlatex-item t]
   "----"
   ["Insert Math Symbol"     cdlatex-math-symbol t]
   ["Modify Math Symbol"     cdlatex-math-modify t]
   "----"
   ("Customize"
    ["Browse CDLaTeX group" cdlatex-customize t]
    "---"
    ["Build Full Customize Menu" cdlatex-create-customize-menu
     (fboundp 'customize-menu-create)])
   "----"
   ["Toggle electricindex"      cdlatex-electricindex-mode t]
   "----"
   ["Show documentation"      cdlatex-show-commentary t]
   ["Help with KEYWORD Cmds" cdlatex-command-help t]
   ["Reset CDLaTeX Mode"       cdlatex-reset-mode t]))

;(eval-after-load "cus-edit"
;  '(and (fboundp 'customize-menu-create) (cdlatex-create-customize-menu)))

;;; Run Hook ------------------------------------------------------------------

(run-hooks 'cdlatex-load-hook)

;;; That's it! ----------------------------------------------------------------

; Make sure tables are compiled
(cdlatex-compute-tables)

(provide 'cdlatex)

;;;============================================================================

;;; cdlatex.el ends here
