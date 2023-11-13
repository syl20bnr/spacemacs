;;; markdown-mode.el --- Major mode for Markdown-formatted text -*- lexical-binding: t; -*-

;; Copyright (C) 2007-2023 Jason R. Blevins and markdown-mode
;; contributors (see the commit log for details).

;; Author: Jason R. Blevins <jblevins@xbeta.org>
;; Maintainer: Jason R. Blevins <jblevins@xbeta.org>
;; Created: May 24, 2007
;; Version: 2.7-alpha
;; Package-Requires: ((emacs "27.1"))
;; Keywords: Markdown, GitHub Flavored Markdown, itex
;; URL: https://jblevins.org/projects/markdown-mode/

;; This file is not part of GNU Emacs.

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

;; See the README.md file for details.


;;; Code:

(require 'easymenu)
(require 'outline)
(require 'thingatpt)
(require 'cl-lib)
(require 'url-parse)
(require 'button)
(require 'color)
(require 'rx)
(require 'subr-x)

(defvar jit-lock-start)
(defvar jit-lock-end)
(defvar flyspell-generic-check-word-predicate)
(defvar electric-pair-pairs)
(defvar sh-ancestor-alist)

(declare-function project-roots "project")
(declare-function sh-set-shell "sh-script")
(declare-function mailcap-file-name-to-mime-type "mailcap")
(declare-function dnd-get-local-file-name "dnd")

;; for older emacs<29
(declare-function mailcap-mime-type-to-extension "mailcap")
(declare-function file-name-with-extension "files")
(declare-function yank-media-handler "yank-media")


;;; Constants =================================================================

(defconst markdown-mode-version "2.7-alpha"
  "Markdown mode version number.")

(defconst markdown-output-buffer-name "*markdown-output*"
  "Name of temporary buffer for markdown command output.")


;;; Global Variables ==========================================================

(defvar markdown-reference-label-history nil
  "History of used reference labels.")

(defvar markdown-live-preview-mode nil
  "Sentinel variable for command `markdown-live-preview-mode'.")

(defvar markdown-gfm-language-history nil
  "History list of languages used in the current buffer in GFM code blocks.")

(defvar markdown-follow-link-functions nil
  "Functions used to follow a link.
Each function is called with one argument, the link's URL. It
should return non-nil if it followed the link, or nil if not.
Functions are called in order until one of them returns non-nil;
otherwise the default link-following function is used.")


;;; Customizable Variables ====================================================

(defvar markdown-mode-hook nil
  "Hook run when entering Markdown mode.")

(defvar markdown-before-export-hook nil
  "Hook run before running Markdown to export XHTML output.
The hook may modify the buffer, which will be restored to it's
original state after exporting is complete.")

(defvar markdown-after-export-hook nil
  "Hook run after XHTML output has been saved.
Any changes to the output buffer made by this hook will be saved.")

(defgroup markdown nil
  "Major mode for editing text files in Markdown format."
  :prefix "markdown-"
  :group 'text
  :link '(url-link "https://jblevins.org/projects/markdown-mode/"))

(defcustom markdown-command (let ((command (cl-loop for cmd in '("markdown" "pandoc" "markdown_py")
                                                    when (executable-find cmd)
                                                    return (file-name-nondirectory it))))
                              (or command "markdown"))
  "Command to run markdown."
  :group 'markdown
  :type '(choice (string :tag "Shell command") (repeat (string)) function))

(defcustom markdown-command-needs-filename nil
  "Set to non-nil if `markdown-command' does not accept input from stdin.
Instead, it will be passed a filename as the final command line
option.  As a result, you will only be able to run Markdown from
buffers which are visiting a file."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-open-command nil
  "Command used for opening Markdown files directly.
For example, a standalone Markdown previewer.  This command will
be called with a single argument: the filename of the current
buffer.  It can also be a function, which will be called without
arguments."
  :group 'markdown
  :type '(choice file function (const :tag "None" nil)))

(defcustom markdown-open-image-command nil
  "Command used for opening image files directly.
This is used at `markdown-follow-link-at-point'."
  :group 'markdown
  :type '(choice file function (const :tag "None" nil)))

(defcustom markdown-hr-strings
  '("-------------------------------------------------------------------------------"
    "* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *"
    "---------------------------------------"
    "* * * * * * * * * * * * * * * * * * * *"
    "---------"
    "* * * * *")
  "Strings to use when inserting horizontal rules.
The first string in the list will be the default when inserting a
horizontal rule.  Strings should be listed in decreasing order of
prominence (as in headings from level one to six) for use with
promotion and demotion functions."
  :group 'markdown
  :type '(repeat string))

(defcustom markdown-bold-underscore nil
  "Use two underscores when inserting bold text instead of two asterisks."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-italic-underscore nil
  "Use underscores when inserting italic text instead of asterisks."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-marginalize-headers nil
  "When non-nil, put opening atx header markup in a left margin.

This setting goes well with `markdown-asymmetric-header'.  But
sadly it conflicts with `linum-mode' since they both use the
same margin."
  :group 'markdown
  :type 'boolean
  :safe 'booleanp
  :package-version '(markdown-mode . "2.4"))

(defcustom markdown-marginalize-headers-margin-width 6
  "Character width of margin used for marginalized headers.
The default value is based on there being six heading levels
defined by Markdown and HTML.  Increasing this produces extra
whitespace on the left.  Decreasing it may be preferred when
fewer than six nested heading levels are used."
  :group 'markdown
  :type 'integer
  :safe 'natnump
  :package-version '(markdown-mode . "2.4"))

(defcustom markdown-asymmetric-header nil
  "Determines if atx header style will be asymmetric.
Set to a non-nil value to use asymmetric header styling, placing
header markup only at the beginning of the line. By default,
balanced markup will be inserted at the beginning and end of the
line around the header title."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-indent-function 'markdown-indent-line
  "Function to use to indent."
  :group 'markdown
  :type 'function)

(defcustom markdown-indent-on-enter t
  "Determines indentation behavior when pressing \\[newline].
Possible settings are nil, t, and \\='indent-and-new-item.

When non-nil, pressing \\[newline] will call `newline-and-indent'
to indent the following line according to the context using
`markdown-indent-function'.  In this case, note that
\\[electric-newline-and-maybe-indent] can still be used to insert
a newline without indentation.

When set to \\='indent-and-new-item and the point is in a list item
when \\[newline] is pressed, the list will be continued on the next
line, where a new item will be inserted.

When set to nil, simply call `newline' as usual.  In this case,
you can still indent lines using \\[markdown-cycle] and continue
lists with \\[markdown-insert-list-item].

Note that this assumes the variable `electric-indent-mode' is
non-nil (enabled).  When it is *disabled*, the behavior of
\\[newline] and `\\[electric-newline-and-maybe-indent]' are
reversed."
  :group 'markdown
  :type '(choice (const :tag "Don't automatically indent" nil)
                 (const :tag "Automatically indent" t)
                 (const :tag "Automatically indent and insert new list items" indent-and-new-item)))

(defcustom markdown-enable-wiki-links nil
  "Syntax highlighting for wiki links.
Set this to a non-nil value to turn on wiki link support by default.
Support can be toggled later using the `markdown-toggle-wiki-links'
function or \\[markdown-toggle-wiki-links]."
  :group 'markdown
  :type 'boolean
  :safe 'booleanp
  :package-version '(markdown-mode . "2.2"))

(defcustom markdown-wiki-link-alias-first t
  "When non-nil, treat aliased wiki links like [[alias text|PageName]].
Otherwise, they will be treated as [[PageName|alias text]]."
  :group 'markdown
  :type 'boolean
  :safe 'booleanp)

(defcustom markdown-wiki-link-search-subdirectories nil
  "When non-nil, search for wiki link targets in subdirectories.
This is the default search behavior for GitHub and is
automatically set to t in `gfm-mode'."
  :group 'markdown
  :type 'boolean
  :safe 'booleanp
  :package-version '(markdown-mode . "2.2"))

(defcustom markdown-wiki-link-search-parent-directories nil
  "When non-nil, search for wiki link targets in parent directories.
This is the default search behavior of Ikiwiki."
  :group 'markdown
  :type 'boolean
  :safe 'booleanp
  :package-version '(markdown-mode . "2.2"))

(defcustom markdown-wiki-link-search-type nil
  "Searching type for markdown wiki link.

sub-directories: search for wiki link targets in sub directories
parent-directories: search for wiki link targets in parent directories
project: search for wiki link targets under project root"
  :group 'markdown
  :type '(set
          (const :tag "search wiki link from subdirectories" sub-directories)
          (const :tag "search wiki link from parent directories" parent-directories)
          (const :tag "search wiki link under project root" project))
  :package-version '(markdown-mode . "2.5"))

(make-obsolete-variable 'markdown-wiki-link-search-subdirectories 'markdown-wiki-link-search-type "2.5")
(make-obsolete-variable 'markdown-wiki-link-search-parent-directories 'markdown-wiki-link-search-type "2.5")

(defcustom markdown-wiki-link-fontify-missing nil
  "When non-nil, change wiki link face according to existence of target files.
This is expensive because it requires checking for the file each time the buffer
changes or the user switches windows.  It is disabled by default because it may
cause lag when typing on slower machines."
  :group 'markdown
  :type 'boolean
  :safe 'booleanp
  :package-version '(markdown-mode . "2.2"))

(defcustom markdown-uri-types
  '("acap" "cid" "data" "dav" "fax" "file" "ftp"
    "geo" "gopher" "http" "https" "imap" "ldap" "mailto"
    "mid" "message" "modem" "news" "nfs" "nntp"
    "pop" "prospero" "rtsp" "service" "sip" "tel"
    "telnet" "tip" "urn" "vemmi" "wais")
  "Link types for syntax highlighting of URIs."
  :group 'markdown
  :type '(repeat (string :tag "URI scheme")))

(defcustom markdown-url-compose-char
  '(?∞ ?… ?⋯ ?# ?★ ?⚓)
  "Placeholder character for hidden URLs.
This may be a single character or a list of characters. In case
of a list, the first one that satisfies `char-displayable-p' will
be used."
  :type '(choice
          (character :tag "Single URL replacement character")
          (repeat :tag "List of possible URL replacement characters"
                  character))
  :package-version '(markdown-mode . "2.3"))

(defcustom markdown-blockquote-display-char
  '("▌" "┃" ">")
  "String to display when hiding blockquote markup.
This may be a single string or a list of string. In case of a
list, the first one that satisfies `char-displayable-p' will be
used."
  :type '(choice
          (string :tag "Single blockquote display string")
          (repeat :tag "List of possible blockquote display strings" string))
  :package-version '(markdown-mode . "2.3"))

(defcustom markdown-hr-display-char
  '(?─ ?━ ?-)
  "Character for hiding horizontal rule markup.
This may be a single character or a list of characters.  In case
of a list, the first one that satisfies `char-displayable-p' will
be used."
  :group 'markdown
  :type '(choice
          (character :tag "Single HR display character")
          (repeat :tag "List of possible HR display characters" character))
  :package-version '(markdown-mode . "2.3"))

(defcustom markdown-definition-display-char
  '(?⁘ ?⁙ ?≡ ?⌑ ?◊ ?:)
  "Character for replacing definition list markup.
This may be a single character or a list of characters.  In case
of a list, the first one that satisfies `char-displayable-p' will
be used."
  :type '(choice
          (character :tag "Single definition list character")
          (repeat :tag "List of possible definition list characters" character))
  :package-version '(markdown-mode . "2.3"))

(defcustom markdown-enable-math nil
  "Syntax highlighting for inline LaTeX and itex expressions.
Set this to a non-nil value to turn on math support by default.
Math support can be enabled, disabled, or toggled later using
`markdown-toggle-math' or \\[markdown-toggle-math]."
  :group 'markdown
  :type 'boolean
  :safe 'booleanp)
(make-variable-buffer-local 'markdown-enable-math)

(defcustom markdown-enable-html t
  "Enable font-lock support for HTML tags and attributes."
  :group 'markdown
  :type 'boolean
  :safe 'booleanp
  :package-version '(markdown-mode . "2.4"))

(defcustom markdown-enable-highlighting-syntax nil
  "Enable highlighting syntax."
  :group 'markdown
  :type 'boolean
  :safe 'booleanp
  :package-version '(markdown-mode . "2.5"))

(defcustom markdown-css-paths nil
  "List of URLs of CSS files to link to in the output XHTML."
  :group 'markdown
  :type '(repeat (string :tag "CSS File Path")))

(defcustom markdown-content-type "text/html"
  "Content type string for the http-equiv header in XHTML output.
When set to an empty string, this attribute is omitted.  Defaults to
`text/html'."
  :group 'markdown
  :type 'string)

(defcustom markdown-coding-system nil
  "Character set string for the http-equiv header in XHTML output.
Defaults to `buffer-file-coding-system' (and falling back to
`utf-8' when not available).  Common settings are `iso-8859-1'
and `iso-latin-1'.  Use `list-coding-systems' for more choices."
  :group 'markdown
  :type 'coding-system)

(defcustom markdown-export-kill-buffer t
  "Kill output buffer after HTML export.
When non-nil, kill the HTML output buffer after
exporting with `markdown-export'."
  :group 'markdown
  :type 'boolean
  :safe 'booleanp
  :package-version '(markdown-mode . "2.4"))

(defcustom markdown-xhtml-header-content ""
  "Additional content to include in the XHTML <head> block."
  :group 'markdown
  :type 'string)

(defcustom markdown-xhtml-body-preamble ""
  "Content to include in the XHTML <body> block, before the output."
  :group 'markdown
  :type 'string
  :safe 'stringp
  :package-version '(markdown-mode . "2.4"))

(defcustom markdown-xhtml-body-epilogue ""
  "Content to include in the XHTML <body> block, after the output."
  :group 'markdown
  :type 'string
  :safe 'stringp
  :package-version '(markdown-mode . "2.4"))

(defcustom markdown-xhtml-standalone-regexp
  "^\\(<\\?xml\\|<!DOCTYPE\\|<html\\)"
  "Regexp indicating whether `markdown-command' output is standalone XHTML."
  :group 'markdown
  :type 'regexp)

(defcustom markdown-link-space-sub-char "_"
  "Character to use instead of spaces when mapping wiki links to filenames."
  :group 'markdown
  :type 'string)

(defcustom markdown-reference-location 'header
  "Position where new reference definitions are inserted in the document."
  :group 'markdown
  :type '(choice (const :tag "At the end of the document" end)
                 (const :tag "Immediately after the current block" immediately)
                 (const :tag "At the end of the subtree" subtree)
                 (const :tag "Before next header" header)))

(defcustom markdown-footnote-location 'end
  "Position where new footnotes are inserted in the document."
  :group 'markdown
  :type '(choice (const :tag "At the end of the document" end)
                 (const :tag "Immediately after the current block" immediately)
                 (const :tag "At the end of the subtree" subtree)
                 (const :tag "Before next header" header)))

(defcustom markdown-footnote-display '((raise 0.2) (height 0.8))
  "Display specification for footnote markers and inline footnotes.
By default, footnote text is reduced in size and raised.  Set to
nil to disable this."
  :group 'markdown
  :type '(choice (sexp :tag "Display specification")
                 (const :tag "Don't set display property" nil))
  :package-version '(markdown-mode . "2.4"))

(defcustom markdown-sub-superscript-display
  '(((raise -0.3) (height 0.7)) . ((raise 0.3) (height 0.7)))
  "Display specification for subscript and superscripts.
The car is used for subscript, the cdr is used for superscripts."
  :group 'markdown
  :type '(cons (choice (sexp :tag "Subscript form")
                       (const :tag "No lowering" nil))
               (choice (sexp :tag "Superscript form")
                       (const :tag "No raising" nil)))
  :package-version '(markdown-mode . "2.4"))

(defcustom markdown-unordered-list-item-prefix "  * "
  "String inserted before unordered list items."
  :group 'markdown
  :type 'string)

(defcustom markdown-ordered-list-enumeration t
  "When non-nil, use enumerated numbers(1. 2. 3. etc.) for ordered list marker.
While nil, always uses '1.' for the marker"
  :group 'markdown
  :type 'boolean
  :package-version '(markdown-mode . "2.5"))

(defcustom markdown-nested-imenu-heading-index t
  "Use nested or flat imenu heading index.
A nested index may provide more natural browsing from the menu,
but a flat list may allow for faster keyboard navigation via tab
completion."
  :group 'markdown
  :type 'boolean
  :safe 'booleanp
  :package-version '(markdown-mode . "2.2"))

(defcustom markdown-add-footnotes-to-imenu t
  "Add footnotes to end of imenu heading index."
  :group 'markdown
  :type 'boolean
  :safe 'booleanp
  :package-version '(markdown-mode . "2.4"))

(defcustom markdown-make-gfm-checkboxes-buttons t
  "When non-nil, make GFM checkboxes into buttons."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-use-pandoc-style-yaml-metadata nil
  "When non-nil, allow YAML metadata anywhere in the document."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-split-window-direction 'any
  "Preference for splitting windows for static and live preview.
The default value is \\='any, which instructs Emacs to use
`split-window-sensibly' to automatically choose how to split
windows based on the values of `split-width-threshold' and
`split-height-threshold' and the available windows.  To force
vertically split (left and right) windows, set this to \\='vertical
or \\='right.  To force horizontally split (top and bottom) windows,
set this to \\='horizontal or \\='below.

If this value is \\='any and `display-buffer-alist' is set then
`display-buffer' is used for open buffer function"
  :group 'markdown
  :type '(choice (const :tag "Automatic" any)
                 (const :tag "Right (vertical)" right)
                 (const :tag "Below (horizontal)" below))
  :package-version '(markdown-mode . "2.2"))

(defcustom markdown-live-preview-window-function
  #'markdown-live-preview-window-eww
  "Function to display preview of Markdown output within Emacs.
Function must update the buffer containing the preview and return
the buffer."
  :group 'markdown
  :type 'function)

(defcustom markdown-live-preview-delete-export 'delete-on-destroy
  "Delete exported HTML file when using `markdown-live-preview-export'.
If set to \\='delete-on-export, delete on every export. When set to
\\='delete-on-destroy delete when quitting from command
`markdown-live-preview-mode'. Never delete if set to nil."
  :group 'markdown
  :type '(choice
          (const :tag "Delete on every export" delete-on-export)
          (const :tag "Delete when quitting live preview" delete-on-destroy)
          (const :tag "Never delete" nil)))

(defcustom markdown-list-indent-width 4
  "Depth of indentation for markdown lists.
Used in `markdown-demote-list-item' and
`markdown-promote-list-item'."
  :group 'markdown
  :type 'integer)

(defcustom markdown-enable-prefix-prompts t
  "Display prompts for certain prefix commands.
Set to nil to disable these prompts."
  :group 'markdown
  :type 'boolean
  :safe 'booleanp
  :package-version '(markdown-mode . "2.3"))

(defcustom markdown-gfm-additional-languages nil
  "Extra languages made available when inserting GFM code blocks.
Language strings must have be trimmed of whitespace and not
contain any curly braces. They may be of arbitrary
capitalization, though."
  :group 'markdown
  :type '(repeat (string :validate markdown-validate-language-string)))

(defcustom markdown-gfm-use-electric-backquote t
  "Use `markdown-electric-backquote' when backquote is hit three times."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-gfm-downcase-languages t
  "If non-nil, downcase suggested languages.
This applies to insertions done with
`markdown-electric-backquote'."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-edit-code-block-default-mode 'normal-mode
  "Default mode to use for editing code blocks.
This mode is used when automatic detection fails, such as for GFM
code blocks with no language specified."
  :group 'markdown
  :type '(choice function (const :tag "None" nil))
  :package-version '(markdown-mode . "2.4"))

(defcustom markdown-gfm-uppercase-checkbox nil
  "If non-nil, use [X] for completed checkboxes, [x] otherwise."
  :group 'markdown
  :type 'boolean
  :safe 'booleanp)

(defcustom markdown-hide-urls nil
  "Hide URLs of inline links and reference tags of reference links.
Such URLs will be replaced by a single customizable
character, defined by `markdown-url-compose-char', but are still part
of the buffer.  Links can be edited interactively with
\\[markdown-insert-link] or, for example, by deleting the final
parenthesis to remove the invisibility property. You can also
hover your mouse pointer over the link text to see the URL.
Set this to a non-nil value to turn this feature on by default.
You can interactively set the value of this variable by calling
`markdown-toggle-url-hiding', pressing \\[markdown-toggle-url-hiding],
or from the menu Markdown > Links & Images menu."
  :group 'markdown
  :type 'boolean
  :safe 'booleanp
  :package-version '(markdown-mode . "2.3"))
(make-variable-buffer-local 'markdown-hide-urls)

(defcustom markdown-translate-filename-function #'identity
  "Function to use to translate filenames when following links.
\\<markdown-mode-map>\\[markdown-follow-thing-at-point] and \\[markdown-follow-link-at-point]
call this function with the filename as only argument whenever
they encounter a filename (instead of a URL) to be visited and
use its return value instead of the filename in the link.  For
example, if absolute filenames are actually relative to a server
root directory, you can set
`markdown-translate-filename-function' to a function that
prepends the root directory to the given filename."
  :group 'markdown
  :type 'function
  :risky t
  :package-version '(markdown-mode . "2.4"))

(defcustom markdown-max-image-size nil
  "Maximum width and height for displayed inline images.
This variable may be nil or a cons cell (MAX-WIDTH . MAX-HEIGHT).
When nil, use the actual size.  Otherwise, use ImageMagick to
resize larger images to be of the given maximum dimensions.  This
requires Emacs to be built with ImageMagick support."
  :group 'markdown
  :package-version '(markdown-mode . "2.4")
  :type '(choice
          (const :tag "Use actual image width" nil)
          (cons (choice (sexp :tag "Maximum width in pixels")
                        (const :tag "No maximum width" nil))
                (choice (sexp :tag "Maximum height in pixels")
                        (const :tag "No maximum height" nil)))))

(defcustom markdown-mouse-follow-link t
  "Non-nil means mouse on a link will follow the link.
This variable must be set before loading markdown-mode."
  :group 'markdown
  :type 'boolean
  :safe 'booleanp
  :package-version '(markdown-mode . "2.5"))

(defcustom markdown-table-align-p t
  "Non-nil means that table is aligned after table operation."
  :group 'markdown
  :type 'boolean
  :safe 'booleanp
  :package-version '(markdown-mode . "2.5"))

(defcustom markdown-fontify-whole-heading-line nil
  "Non-nil means fontify the whole line for headings.
This is useful when setting a background color for the
markdown-header-face-* faces."
  :group 'markdown
  :type 'boolean
  :safe 'booleanp
  :package-version '(markdown-mode . "2.5"))


;;; Markdown-Specific `rx' Macro ==============================================

;; Based on python-rx from python.el.
(eval-and-compile
  (defconst markdown-rx-constituents
    `((newline . ,(rx "\n"))
      ;; Note: #405 not consider markdown-list-indent-width however this is never used
      (indent . ,(rx (or (repeat 4 " ") "\t")))
      (block-end . ,(rx (and (or (one-or-more (zero-or-more blank) "\n") line-end))))
      (numeral . ,(rx (and (one-or-more (any "0-9#")) ".")))
      (bullet . ,(rx (any "*+:-")))
      (list-marker . ,(rx (or (and (one-or-more (any "0-9#")) ".")
                              (any "*+:-"))))
      (checkbox . ,(rx "[" (any " xX") "]")))
    "Markdown-specific sexps for `markdown-rx'")

  (defun markdown-rx-to-string (form &optional no-group)
    "Markdown mode specialized `rx-to-string' function.
This variant supports named Markdown expressions in FORM.
NO-GROUP non-nil means don't put shy groups around the result."
    (let ((rx-constituents (append markdown-rx-constituents rx-constituents)))
      (rx-to-string form no-group)))

  (defmacro markdown-rx (&rest regexps)
    "Markdown mode specialized rx macro.
This variant of `rx' supports common Markdown named REGEXPS."
    (cond ((null regexps)
           (error "No regexp"))
          ((cdr regexps)
           (markdown-rx-to-string `(and ,@regexps) t))
          (t
           (markdown-rx-to-string (car regexps) t)))))


;;; Regular Expressions =======================================================

(defconst markdown-regex-comment-start
  "<!--"
  "Regular expression matches HTML comment opening.")

(defconst markdown-regex-comment-end
  "--[ \t]*>"
  "Regular expression matches HTML comment closing.")

(defconst markdown-regex-link-inline
  "\\(?1:!\\)?\\(?2:\\[\\)\\(?3:\\^?\\(?:\\\\\\]\\|[^]]\\)*\\|\\)\\(?4:\\]\\)\\(?5:(\\)\\s-*\\(?6:[^)]*?\\)\\(?:\\s-+\\(?7:\"[^\"]*\"\\)\\)?\\s-*\\(?8:)\\)"
  "Regular expression for a [text](file) or an image link ![text](file).
Group 1 matches the leading exclamation point (optional).
Group 2 matches the opening square bracket.
Group 3 matches the text inside the square brackets.
Group 4 matches the closing square bracket.
Group 5 matches the opening parenthesis.
Group 6 matches the URL.
Group 7 matches the title (optional).
Group 8 matches the closing parenthesis.")

(defconst markdown-regex-link-reference
  "\\(?1:!\\)?\\(?2:\\[\\)\\(?3:[^]^][^]]*\\|\\)\\(?4:\\]\\)\\(?5:\\[\\)\\(?6:[^]]*?\\)\\(?7:\\]\\)"
  "Regular expression for a reference link [text][id].
Group 1 matches the leading exclamation point (optional).
Group 2 matches the opening square bracket for the link text.
Group 3 matches the text inside the square brackets.
Group 4 matches the closing square bracket for the link text.
Group 5 matches the opening square bracket for the reference label.
Group 6 matches the reference label.
Group 7 matches the closing square bracket for the reference label.")

(defconst markdown-regex-reference-definition
  "^ \\{0,3\\}\\(?1:\\[\\)\\(?2:[^]\n]+?\\)\\(?3:\\]\\)\\(?4::\\)\\s *\\(?5:.*?\\)\\s *\\(?6: \"[^\"]*\"$\\|$\\)"
  "Regular expression for a reference definition.
Group 1 matches the opening square bracket.
Group 2 matches the reference label.
Group 3 matches the closing square bracket.
Group 4 matches the colon.
Group 5 matches the URL.
Group 6 matches the title attribute (optional).")

(defconst markdown-regex-footnote
  "\\(?1:\\[\\^\\)\\(?2:.+?\\)\\(?3:\\]\\)"
  "Regular expression for a footnote marker [^fn].
Group 1 matches the opening square bracket and carat.
Group 2 matches only the label, without the surrounding markup.
Group 3 matches the closing square bracket.")

(defconst markdown-regex-header
  "^\\(?:\\(?1:[^\r\n\t -].*\\)\n\\(?:\\(?2:=+\\)\\|\\(?3:-+\\)\\)\\|\\(?4:#+[ \t]+\\)\\(?5:.*?\\)\\(?6:[ \t]+#+\\)?\\)$"
  "Regexp identifying Markdown headings.
Group 1 matches the text of a setext heading.
Group 2 matches the underline of a level-1 setext heading.
Group 3 matches the underline of a level-2 setext heading.
Group 4 matches the opening hash marks of an atx heading and whitespace.
Group 5 matches the text, without surrounding whitespace, of an atx heading.
Group 6 matches the closing whitespace and hash marks of an atx heading.")

(defconst markdown-regex-header-setext
  "^\\([^\r\n\t -].*\\)\n\\(=+\\|-+\\)$"
  "Regular expression for generic setext-style (underline) headers.")

(defconst markdown-regex-header-atx
  "^\\(#+\\)[ \t]+\\(.*?\\)[ \t]*\\(#*\\)$"
  "Regular expression for generic atx-style (hash mark) headers.")

(defconst markdown-regex-hr
  (rx line-start
      (group (or (and (repeat 3 (and "*" (? " "))) (* (any "* ")))
                 (and (repeat 3 (and "-" (? " "))) (* (any "- ")))
                 (and (repeat 3 (and "_" (? " "))) (* (any "_ ")))))
      line-end)
  "Regular expression for matching Markdown horizontal rules.")

(defconst markdown-regex-code
  "\\(?:\\`\\|[^\\]\\)\\(?1:\\(?2:`+\\)\\(?3:\\(?:.\\|\n[^\n]\\)*?[^`]\\)\\(?4:\\2\\)\\)\\(?:[^`]\\|\\'\\)"
  "Regular expression for matching inline code fragments.

Group 1 matches the entire code fragment including the backquotes.
Group 2 matches the opening backquotes.
Group 3 matches the code fragment itself, without backquotes.
Group 4 matches the closing backquotes.

The leading, unnumbered group ensures that the leading backquote
character is not escaped.
The last group, also unnumbered, requires that the character
following the code fragment is not a backquote.
Note that \\(?:.\\|\n[^\n]\\) matches any character, including newlines,
but not two newlines in a row.")

(defconst markdown-regex-kbd
  "\\(?1:<kbd>\\)\\(?2:\\(?:.\\|\n[^\n]\\)*?\\)\\(?3:</kbd>\\)"
  "Regular expression for matching <kbd> tags.
Groups 1 and 3 match the opening and closing tags.
Group 2 matches the key sequence.")

(defconst markdown-regex-gfm-code-block-open
  "^[[:blank:]]*\\(?1:```\\)\\(?2:[[:blank:]]*{?[[:blank:]]*\\)\\(?3:[^`[:space:]]+?\\)?\\(?:[[:blank:]]+\\(?4:.+?\\)\\)?\\(?5:[[:blank:]]*}?[[:blank:]]*\\)$"
  "Regular expression matching opening of GFM code blocks.
Group 1 matches the opening three backquotes and any following whitespace.
Group 2 matches the opening brace (optional) and surrounding whitespace.
Group 3 matches the language identifier (optional).
Group 4 matches the info string (optional).
Group 5 matches the closing brace (optional), whitespace, and newline.
Groups need to agree with `markdown-regex-tilde-fence-begin'.")

(defconst markdown-regex-gfm-code-block-close
  "^[[:blank:]]*\\(?1:```\\)\\(?2:\\s *?\\)$"
  "Regular expression matching closing of GFM code blocks.
Group 1 matches the closing three backquotes.
Group 2 matches any whitespace and the final newline.")

(defconst markdown-regex-pre
  "^\\(    \\|\t\\).*$"
  "Regular expression for matching preformatted text sections.")

(defconst markdown-regex-list
  (markdown-rx line-start
               ;; 1. Leading whitespace
               (group (* blank))
               ;; 2. List marker: a numeral, bullet, or colon
               (group list-marker)
               ;; 3. Trailing whitespace
               (group (+ blank))
               ;; 4. Optional checkbox for GFM task list items
               (opt (group (and checkbox (* blank)))))
  "Regular expression for matching list items.")

(defconst markdown-regex-bold
  "\\(?1:^\\|[^\\]\\)\\(?2:\\(?3:\\*\\*\\|__\\)\\(?4:[^ \n\t\\]\\|[^ \n\t]\\(?:.\\|\n[^\n]\\)*?[^\\ ]\\)\\(?5:\\3\\)\\)"
  "Regular expression for matching bold text.
Group 1 matches the character before the opening asterisk or
underscore, if any, ensuring that it is not a backslash escape.
Group 2 matches the entire expression, including delimiters.
Groups 3 and 5 matches the opening and closing delimiters.
Group 4 matches the text inside the delimiters.")

(defconst markdown-regex-italic
  "\\(?:^\\|[^\\]\\)\\(?1:\\(?2:[*_]\\)\\(?3:[^ \n\t\\]\\|[^ \n\t*]\\(?:.\\|\n[^\n]\\)*?[^\\ ]\\)\\(?4:\\2\\)\\)"
  "Regular expression for matching italic text.
The leading unnumbered matches the character before the opening
asterisk or underscore, if any, ensuring that it is not a
backslash escape.
Group 1 matches the entire expression, including delimiters.
Groups 2 and 4 matches the opening and closing delimiters.
Group 3 matches the text inside the delimiters.")

(defconst markdown-regex-strike-through
  "\\(?1:^\\|[^\\]\\)\\(?2:\\(?3:~~\\)\\(?4:[^ \n\t\\]\\|[^ \n\t]\\(?:.\\|\n[^\n]\\)*?[^\\ ]\\)\\(?5:~~\\)\\)"
  "Regular expression for matching strike-through text.
Group 1 matches the character before the opening tilde, if any,
ensuring that it is not a backslash escape.
Group 2 matches the entire expression, including delimiters.
Groups 3 and 5 matches the opening and closing delimiters.
Group 4 matches the text inside the delimiters.")

(defconst markdown-regex-gfm-italic
  "\\(?:^\\|[^\\]\\)\\(?1:\\(?2:[*_]\\)\\(?3:[^ \\]\\2\\|[^ ]\\(?:.\\|\n[^\n]\\)*?\\)\\(?4:\\2\\)\\)"
  "Regular expression for matching italic text in GitHub Flavored Markdown.
Underscores in words are not treated as special.
Group 1 matches the entire expression, including delimiters.
Groups 2 and 4 matches the opening and closing delimiters.
Group 3 matches the text inside the delimiters.")

(defconst markdown-regex-blockquote
  "^[ \t]*\\(?1:[A-Z]?>\\)\\(?2:[ \t]*\\)\\(?3:.*\\)$"
  "Regular expression for matching blockquote lines.
Also accounts for a potential capital letter preceding the angle
bracket, for use with Leanpub blocks (asides, warnings, info
blocks, etc.).
Group 1 matches the leading angle bracket.
Group 2 matches the separating whitespace.
Group 3 matches the text.")

(defconst markdown-regex-line-break
  "[^ \n\t][ \t]*\\(  \\)\n"
  "Regular expression for matching line breaks.")

(defconst markdown-regex-escape
  "\\(\\\\\\)."
  "Regular expression for matching escape sequences.")

(defconst markdown-regex-wiki-link
  "\\(?:^\\|[^\\]\\)\\(?1:\\(?2:\\[\\[\\)\\(?3:[^]|]+\\)\\(?:\\(?4:|\\)\\(?5:[^]]+\\)\\)?\\(?6:\\]\\]\\)\\)"
  "Regular expression for matching wiki links.
This matches typical bracketed [[WikiLinks]] as well as \\='aliased
wiki links of the form [[PageName|link text]].
The meanings of the first and second components depend
on the value of `markdown-wiki-link-alias-first'.

Group 1 matches the entire link.
Group 2 matches the opening square brackets.
Group 3 matches the first component of the wiki link.
Group 4 matches the pipe separator, when present.
Group 5 matches the second component of the wiki link, when present.
Group 6 matches the closing square brackets.")

(defconst markdown-regex-uri
  (concat "\\(" (regexp-opt markdown-uri-types) ":[^]\t\n\r<>; ]+\\)")
  "Regular expression for matching inline URIs.")

;; CommanMark specification says scheme length is 2-32 characters
(defconst markdown-regex-angle-uri
  (concat "\\(<\\)\\([a-z][a-z0-9.+-]\\{1,31\\}:[^]\t\n\r<>,;()]+\\)\\(>\\)")
  "Regular expression for matching inline URIs in angle brackets.")

(defconst markdown-regex-email
  "<\\(\\(?:\\sw\\|\\s_\\|\\s.\\)+@\\(?:\\sw\\|\\s_\\|\\s.\\)+\\)>"
  "Regular expression for matching inline email addresses.")

(defsubst markdown-make-regex-link-generic ()
  "Make regular expression for matching any recognized link."
  (concat "\\(?:" markdown-regex-link-inline
          (when markdown-enable-wiki-links
            (concat "\\|" markdown-regex-wiki-link))
          "\\|" markdown-regex-link-reference
          "\\|" markdown-regex-angle-uri "\\)"))

(defconst markdown-regex-gfm-checkbox
  " \\(\\[[ xX]\\]\\) "
  "Regular expression for matching GFM checkboxes.
Group 1 matches the text to become a button.")

(defconst markdown-regex-blank-line
  "^[[:blank:]]*$"
  "Regular expression that matches a blank line.")

(defconst markdown-regex-block-separator
  "\n[\n\t\f ]*\n"
  "Regular expression for matching block boundaries.")

(defconst markdown-regex-block-separator-noindent
  (concat "\\(\\`\\|\\(" markdown-regex-block-separator "\\)[^\n\t\f ]\\)")
  "Regexp for block separators before lines with no indentation.")

(defconst markdown-regex-math-inline-single
  "\\(?:^\\|[^\\]\\)\\(?1:\\$\\)\\(?2:\\(?:[^\\$]\\|\\\\.\\)*\\)\\(?3:\\$\\)"
  "Regular expression for itex $..$ math mode expressions.
Groups 1 and 3 match the opening and closing dollar signs.
Group 2 matches the mathematical expression contained within.")

(defconst markdown-regex-math-inline-double
  "\\(?:^\\|[^\\]\\)\\(?1:\\$\\$\\)\\(?2:\\(?:[^\\$]\\|\\\\.\\)*\\)\\(?3:\\$\\$\\)"
  "Regular expression for itex $$..$$ math mode expressions.
Groups 1 and 3 match opening and closing dollar signs.
Group 2 matches the mathematical expression contained within.")

(defconst markdown-regex-math-display
  (rx line-start (* blank)
      (group (group (repeat 1 2 "\\")) "[")
      (group (*? anything))
      (group (backref 2) "]")
      line-end)
  "Regular expression for \[..\] or \\[..\\] display math.
Groups 1 and 4 match the opening and closing markup.
Group 3 matches the mathematical expression contained within.
Group 2 matches the opening slashes, and is used internally to
match the closing slashes.")

(defsubst markdown-make-tilde-fence-regex (num-tildes &optional end-of-line)
  "Return regexp matching a tilde code fence at least NUM-TILDES long.
END-OF-LINE is the regexp construct to indicate end of line; $ if
missing."
  (format "%s%d%s%s" "^[[:blank:]]*\\([~]\\{" num-tildes ",\\}\\)"
          (or end-of-line "$")))

(defconst markdown-regex-tilde-fence-begin
  (markdown-make-tilde-fence-regex
   3 "\\([[:blank:]]*{?\\)[[:blank:]]*\\([^[:space:]]+?\\)?\\(?:[[:blank:]]+\\(.+?\\)\\)?\\([[:blank:]]*}?[[:blank:]]*\\)$")
  "Regular expression for matching tilde-fenced code blocks.
Group 1 matches the opening tildes.
Group 2 matches (optional) opening brace and surrounding whitespace.
Group 3 matches the language identifier (optional).
Group 4 matches the info string (optional).
Group 5 matches the closing brace (optional) and any surrounding whitespace.
Groups need to agree with `markdown-regex-gfm-code-block-open'.")

(defconst markdown-regex-declarative-metadata
  "^[ \t]*\\(?:-[ \t]*\\)?\\([[:alpha:]][[:alpha:] _-]*?\\)\\([:=][ \t]*\\)\\(.*\\)$"
  "Regular expression for matching declarative metadata statements.
This matches MultiMarkdown metadata as well as YAML and TOML
assignments such as the following:

    variable: value

or

    variable = value")

(defconst markdown-regex-pandoc-metadata
  "^\\(%\\)\\([ \t]*\\)\\(.*\\(?:\n[ \t]+.*\\)*\\)"
  "Regular expression for matching Pandoc metadata.")

(defconst markdown-regex-yaml-metadata-border
  "\\(-\\{3\\}\\)$"
  "Regular expression for matching YAML metadata.")

(defconst markdown-regex-yaml-pandoc-metadata-end-border
  "^\\(\\.\\{3\\}\\|\\-\\{3\\}\\)$"
  "Regular expression for matching YAML metadata end borders.")

(defsubst markdown-get-yaml-metadata-start-border ()
  "Return YAML metadata start border depending upon whether Pandoc is used."
  (concat
   (if markdown-use-pandoc-style-yaml-metadata "^" "\\`")
   markdown-regex-yaml-metadata-border))

(defsubst markdown-get-yaml-metadata-end-border (_)
  "Return YAML metadata end border depending upon whether Pandoc is used."
  (if markdown-use-pandoc-style-yaml-metadata
      markdown-regex-yaml-pandoc-metadata-end-border
    markdown-regex-yaml-metadata-border))

(defconst markdown-regex-inline-attributes
  "[ \t]*\\(?:{:?\\)[ \t]*\\(?:\\(?:#[[:alpha:]_.:-]+\\|\\.[[:alpha:]_.:-]+\\|\\w+=['\"]?[^\n'\"}]*['\"]?\\),?[ \t]*\\)+\\(?:}\\)[ \t]*$"
  "Regular expression for matching inline identifiers or attribute lists.
Compatible with Pandoc, Python Markdown, PHP Markdown Extra, and Leanpub.")

(defconst markdown-regex-leanpub-sections
  (concat
   "^\\({\\)\\("
   (regexp-opt '("frontmatter" "mainmatter" "backmatter" "appendix" "pagebreak"))
   "\\)\\(}\\)[ \t]*\n")
  "Regular expression for Leanpub section markers and related syntax.")

(defconst markdown-regex-sub-superscript
  "\\(?:^\\|[^\\~^]\\)\\(?1:\\(?2:[~^]\\)\\(?3:[+-\u2212]?[[:alnum:]]+\\)\\(?4:\\2\\)\\)"
  "The regular expression matching a sub- or superscript.
The leading un-numbered group matches the character before the
opening tilde or carat, if any, ensuring that it is not a
backslash escape, carat, or tilde.
Group 1 matches the entire expression, including markup.
Group 2 matches the opening markup--a tilde or carat.
Group 3 matches the text inside the delimiters.
Group 4 matches the closing markup--a tilde or carat.")

(defconst markdown-regex-include
  "^\\(?1:<<\\)\\(?:\\(?2:\\[\\)\\(?3:.*\\)\\(?4:\\]\\)\\)?\\(?:\\(?5:(\\)\\(?6:.*\\)\\(?7:)\\)\\)?\\(?:\\(?8:{\\)\\(?9:.*\\)\\(?10:}\\)\\)?$"
  "Regular expression matching common forms of include syntax.
Marked 2, Leanpub, and other processors support some of these forms:

<<[sections/section1.md]
<<(folder/filename)
<<[Code title](folder/filename)
<<{folder/raw_file.html}

Group 1 matches the opening two angle brackets.
Groups 2-4 match the opening square bracket, the text inside,
and the closing square bracket, respectively.
Groups 5-7 match the opening parenthesis, the text inside, and
the closing parenthesis.
Groups 8-10 match the opening brace, the text inside, and the brace.")

(defconst markdown-regex-pandoc-inline-footnote
  "\\(?1:\\^\\)\\(?2:\\[\\)\\(?3:\\(?:.\\|\n[^\n]\\)*?\\)\\(?4:\\]\\)"
  "Regular expression for Pandoc inline footnote^[footnote text].
Group 1 matches the opening caret.
Group 2 matches the opening square bracket.
Group 3 matches the footnote text, without the surrounding markup.
Group 4 matches the closing square bracket.")

(defconst markdown-regex-html-attr
  "\\(\\<[[:alpha:]:-]+\\>\\)\\(\\s-*\\(=\\)\\s-*\\(\".*?\"\\|'.*?'\\|[^'\">[:space:]]+\\)?\\)?"
  "Regular expression for matching HTML attributes and values.
Group 1 matches the attribute name.
Group 2 matches the following whitespace, equals sign, and value, if any.
Group 3 matches the equals sign, if any.
Group 4 matches single-, double-, or un-quoted attribute values.")

(defconst markdown-regex-html-tag
  (concat "\\(</?\\)\\(\\w+\\)\\(\\(\\s-+" markdown-regex-html-attr
          "\\)+\\s-*\\|\\s-*\\)\\(/?>\\)")
  "Regular expression for matching HTML tags.
Groups 1 and 9 match the beginning and ending angle brackets and slashes.
Group 2 matches the tag name.
Group 3 matches all attributes and whitespace following the tag name.")

(defconst markdown-regex-html-entity
  "\\(&#?[[:alnum:]]+;\\)"
  "Regular expression for matching HTML entities.")

(defconst markdown-regex-highlighting
  "\\(?1:^\\|[^\\]\\)\\(?2:\\(?3:==\\)\\(?4:[^ \n\t\\]\\|[^ \n\t]\\(?:.\\|\n[^\n]\\)*?[^\\ ]\\)\\(?5:==\\)\\)"
"Regular expression for matching highlighting text.
Group 1 matches the character before the opening equal, if any,
ensuring that it is not a backslash escape.
Group 2 matches the entire expression, including delimiters.
Groups 3 and 5 matches the opening and closing delimiters.
Group 4 matches the text inside the delimiters.")


;;; Syntax ====================================================================

(defvar markdown--syntax-properties
  (list 'markdown-tilde-fence-begin nil
        'markdown-tilde-fence-end nil
        'markdown-fenced-code nil
        'markdown-yaml-metadata-begin nil
        'markdown-yaml-metadata-end nil
        'markdown-yaml-metadata-section nil
        'markdown-gfm-block-begin nil
        'markdown-gfm-block-end nil
        'markdown-gfm-code nil
        'markdown-list-item nil
        'markdown-pre nil
        'markdown-blockquote nil
        'markdown-hr nil
        'markdown-comment nil
        'markdown-heading nil
        'markdown-heading-1-setext nil
        'markdown-heading-2-setext nil
        'markdown-heading-1-atx nil
        'markdown-heading-2-atx nil
        'markdown-heading-3-atx nil
        'markdown-heading-4-atx nil
        'markdown-heading-5-atx nil
        'markdown-heading-6-atx nil
        'markdown-metadata-key nil
        'markdown-metadata-value nil
        'markdown-metadata-markup nil)
  "Property list of all Markdown syntactic properties.")

(defvar markdown-literal-faces
  '(markdown-inline-code-face
    markdown-pre-face
    markdown-math-face
    markdown-url-face
    markdown-plain-url-face
    markdown-language-keyword-face
    markdown-language-info-face
    markdown-metadata-key-face
    markdown-metadata-value-face
    markdown-html-entity-face
    markdown-html-tag-name-face
    markdown-html-tag-delimiter-face
    markdown-html-attr-name-face
    markdown-html-attr-value-face
    markdown-reference-face
    markdown-footnote-marker-face
    markdown-line-break-face
    markdown-comment-face)
  "A list of markdown-mode faces that contain literal text.
Literal text treats backslashes literally, rather than as an
escape character (see `markdown-match-escape').")

(defsubst markdown-in-comment-p (&optional pos)
  "Return non-nil if POS is in a comment.
If POS is not given, use point instead."
  (get-text-property (or pos (point)) 'markdown-comment))

(defun markdown--face-p (pos faces)
  "Return non-nil if face of POS contain FACES."
  (let ((face-prop (get-text-property pos 'face)))
    (if (listp face-prop)
        (cl-loop for face in face-prop
                 thereis (memq face faces))
      (memq face-prop faces))))

(defsubst markdown--math-block-p (&optional pos)
  (when markdown-enable-math
    (markdown--face-p (or pos (point)) '(markdown-math-face))))

(defun markdown-syntax-propertize-extend-region (start end)
  "Extend START to END region to include an entire block of text.
This helps improve syntax analysis for block constructs.
Returns a cons (NEW-START . NEW-END) or nil if no adjustment should be made.
Function is called repeatedly until it returns nil. For details, see
`syntax-propertize-extend-region-functions'."
  (save-match-data
    (save-excursion
      (let* ((new-start (progn (goto-char start)
                               (skip-chars-forward "\n")
                               (if (re-search-backward "\n\n" nil t)
                                   (min start (match-end 0))
                                 (point-min))))
             (new-end (progn (goto-char end)
                             (skip-chars-backward "\n")
                             (if (re-search-forward "\n\n" nil t)
                                 (max end (match-beginning 0))
                               (point-max))))
             (code-match (markdown-code-block-at-pos new-start))
             ;; FIXME: The `code-match' can return bogus values
             ;; when text has been inserted/deleted!
             (new-start (min (or (and code-match (cl-first code-match))
                                 (point-max))
                             new-start))
             (code-match (and (< end (point-max))
                              (markdown-code-block-at-pos end)))
             (new-end (max (or (and code-match (cl-second code-match)) 0)
                           new-end)))

        (unless (and (eq new-start start) (eq new-end end))
          (cons new-start (min new-end (point-max))))))))

(defun markdown-font-lock-extend-region-function (start end _)
  "Used in `jit-lock-after-change-extend-region-functions'.
Delegates to `markdown-syntax-propertize-extend-region'. START
and END are the previous region to refontify."
  (let ((res (markdown-syntax-propertize-extend-region start end)))
    (when res
      ;; syntax-propertize-function is not called when character at
      ;; (point-max) is deleted, but font-lock-extend-region-functions
      ;; are called.  Force a syntax property update in that case.
      (when (= end (point-max))
        ;; This function is called in a buffer modification hook.
        ;; `markdown-syntax-propertize' doesn't save the match data,
        ;; so we have to do it here.
        (save-match-data
          (markdown-syntax-propertize (car res) (cdr res))))
      (setq jit-lock-start (car res)
            jit-lock-end (cdr res)))))

(defun markdown--cur-list-item-bounds ()
  "Return a list describing the list item at point.
Assumes that match data is set for `markdown-regex-list'.  See the
documentation for `markdown-cur-list-item-bounds' for the format of
the returned list."
  (save-excursion
    (let* ((begin (match-beginning 0))
           (indent (length (match-string-no-properties 1)))
           (nonlist-indent (- (match-end 3) (match-beginning 0)))
           (marker (buffer-substring-no-properties
                    (match-beginning 2) (match-end 3)))
           (checkbox (match-string-no-properties 4))
           (match (butlast (match-data t)))
           (end (markdown-cur-list-item-end nonlist-indent)))
      (list begin end indent nonlist-indent marker checkbox match))))

(defun markdown--append-list-item-bounds (marker indent cur-bounds bounds)
  "Update list item BOUNDS given list MARKER, block INDENT, and CUR-BOUNDS.
Here, MARKER is a string representing the type of list and INDENT
is an integer giving the indentation, in spaces, of the current
block.  CUR-BOUNDS is a list of the form returned by
`markdown-cur-list-item-bounds' and BOUNDS is a list of bounds
values for parent list items.  When BOUNDS is nil, it means we are
at baseline (not inside of a nested list)."
  (let ((prev-indent (or (cl-third (car bounds)) 0)))
    (cond
     ;; New list item at baseline.
     ((and marker (null bounds))
      (list cur-bounds))
     ;; List item with greater indentation (four or more spaces).
     ;; Increase list level by consing CUR-BOUNDS onto BOUNDS.
     ((and marker (>= indent (+ prev-indent markdown-list-indent-width)))
      (cons cur-bounds bounds))
     ;; List item with greater or equal indentation (less than four spaces).
     ;; Keep list level the same by replacing the car of BOUNDS.
     ((and marker (>= indent prev-indent))
      (cons cur-bounds (cdr bounds)))
     ;; Lesser indentation level.
     ;; Pop appropriate number of elements off BOUNDS list (e.g., lesser
     ;; indentation could move back more than one list level).  Note
     ;; that this block need not be the beginning of list item.
     ((< indent prev-indent)
      (while (and (> (length bounds) 1)
                  (setq prev-indent (cl-third (cadr bounds)))
                  (< indent (+ prev-indent markdown-list-indent-width)))
        (setq bounds (cdr bounds)))
      (cons cur-bounds bounds))
     ;; Otherwise, do nothing.
     (t bounds))))

(defun markdown-syntax-propertize-list-items (start end)
  "Propertize list items from START to END.
Stores nested list item information in the `markdown-list-item'
text property to make later syntax analysis easier.  The value of
this property is a list with elements of the form (begin . end)
giving the bounds of the current and parent list items."
  (save-excursion
    (goto-char start)
    (let ((prev-list-line -100)
          bounds level pre-regexp)
      ;; Find a baseline point with zero list indentation
      (markdown-search-backward-baseline)
      ;; Search for all list items between baseline and END
      (while (and (< (point) end)
                  (re-search-forward markdown-regex-list end 'limit))
        ;; Level of list nesting
        (setq level (length bounds))
        ;; Pre blocks need to be indented one level past the list level
        (setq pre-regexp (format "^\\(    \\|\t\\)\\{%d\\}" (1+ level)))
        (beginning-of-line)
        (cond
         ;; Reset at headings, horizontal rules, and top-level blank lines.
         ;; Propertize baseline when in range.
         ((markdown-new-baseline)
          (setq bounds nil))
         ;; Make sure this is not a line from a pre block
         ((and (looking-at-p pre-regexp)
               ;; too indented line is also treated as list if previous line is list
               (>= (- (line-number-at-pos) prev-list-line) 2)))
         ;; If not, then update levels and propertize list item when in range.
         (t
          (let* ((indent (current-indentation))
                 (cur-bounds (markdown--cur-list-item-bounds))
                 (first (cl-first cur-bounds))
                 (last (cl-second cur-bounds))
                 (marker (cl-fifth cur-bounds)))
            (setq bounds (markdown--append-list-item-bounds
                          marker indent cur-bounds bounds))
            (when (and (<= start (point)) (<= (point) end))
              (setq prev-list-line (line-number-at-pos first))
              (put-text-property first last 'markdown-list-item bounds)))))
        (end-of-line)))))

(defun markdown-syntax-propertize-pre-blocks (start end)
  "Match preformatted text blocks from START to END."
  (save-excursion
    (goto-char start)
    (let (finish)
      ;; Use loop for avoiding too many recursive calls
      ;; https://github.com/jrblevin/markdown-mode/issues/512
      (while (not finish)
        (let ((levels (markdown-calculate-list-levels))
              indent pre-regexp close-regexp open close)
          (while (and (< (point) end) (not close))
            ;; Search for a region with sufficient indentation
            (if (null levels)
                (setq indent 1)
              (setq indent (1+ (length levels))))
            (setq pre-regexp (format "^\\(    \\|\t\\)\\{%d\\}" indent))
            (setq close-regexp (format "^\\(    \\|\t\\)\\{0,%d\\}\\([^ \t]\\)" (1- indent)))

            (cond
             ;; If not at the beginning of a line, move forward
             ((not (bolp)) (forward-line))
             ;; Move past blank lines
             ((markdown-cur-line-blank-p) (forward-line))
             ;; At headers and horizontal rules, reset levels
             ((markdown-new-baseline) (forward-line) (setq levels nil))
             ;; If the current line has sufficient indentation, mark out pre block
             ;; The opening should be preceded by a blank line.
             ((and (markdown-prev-line-blank) (looking-at pre-regexp))
              (setq open (match-beginning 0))
              (while (and (or (looking-at-p pre-regexp) (markdown-cur-line-blank-p))
                          (not (eobp)))
                (forward-line))
              (skip-syntax-backward "-")
              (forward-line)
              (setq close (point)))
             ;; If current line has a list marker, update levels, move to end of block
             ((looking-at markdown-regex-list)
              (setq levels (markdown-update-list-levels
                            (match-string 2) (current-indentation) levels))
              (markdown-end-of-text-block))
             ;; If this is the end of the indentation level, adjust levels accordingly.
             ;; Only match end of indentation level if levels is not the empty list.
             ((and (car levels) (looking-at-p close-regexp))
              (setq levels (markdown-update-list-levels
                            nil (current-indentation) levels))
              (markdown-end-of-text-block))
             (t (markdown-end-of-text-block))))

          (if (and open close)
              ;; Set text property data and continue to search
              (put-text-property open close 'markdown-pre (list open close))
            (setq finish t))))
      nil)))

(defconst markdown-fenced-block-pairs
  `(((,markdown-regex-tilde-fence-begin markdown-tilde-fence-begin)
     (markdown-make-tilde-fence-regex markdown-tilde-fence-end)
     markdown-fenced-code)
    ((markdown-get-yaml-metadata-start-border markdown-yaml-metadata-begin)
     (markdown-get-yaml-metadata-end-border markdown-yaml-metadata-end)
     markdown-yaml-metadata-section)
    ((,markdown-regex-gfm-code-block-open markdown-gfm-block-begin)
     (,markdown-regex-gfm-code-block-close markdown-gfm-block-end)
     markdown-gfm-code))
  "Mapping of regular expressions to \"fenced-block\" constructs.
These constructs are distinguished by having a distinctive start
and end pattern, both of which take up an entire line of text,
but no special pattern to identify text within the fenced
blocks (unlike blockquotes and indented-code sections).

Each element within this list takes the form:

  ((START-REGEX-OR-FUN START-PROPERTY)
   (END-REGEX-OR-FUN END-PROPERTY)
   MIDDLE-PROPERTY)

Each *-REGEX-OR-FUN element can be a regular expression as a string, or a
function which evaluates to same. Functions for START-REGEX-OR-FUN accept no
arguments, but functions for END-REGEX-OR-FUN accept a single numerical argument
which is the length of the first group of the START-REGEX-OR-FUN match, which
can be ignored if unnecessary. `markdown-maybe-funcall-regexp' is used to
evaluate these into \"real\" regexps.

The *-PROPERTY elements are the text properties applied to each part of the
block construct when it is matched using
`markdown-syntax-propertize-fenced-block-constructs'. START-PROPERTY is applied
to the text matching START-REGEX-OR-FUN, END-PROPERTY to END-REGEX-OR-FUN, and
MIDDLE-PROPERTY to the text in between the two. The value of *-PROPERTY is the
`match-data' when the regexp was matched to the text. In the case of
MIDDLE-PROPERTY, the value is a false match data of the form \\='(begin end), with
begin and end set to the edges of the \"middle\" text. This makes fontification
easier.")

(defun markdown-text-property-at-point (prop)
  (get-text-property (point) prop))

(defsubst markdown-maybe-funcall-regexp (object &optional arg)
  (cond ((functionp object)
         (if arg (funcall object arg) (funcall object)))
        ((stringp object) object)
        (t (error "Object cannot be turned into regex"))))

(defsubst markdown-get-start-fence-regexp ()
  "Return regexp to find all \"start\" sections of fenced block constructs.
Which construct is actually contained in the match must be found separately."
  (mapconcat
   #'identity
   (mapcar (lambda (entry) (markdown-maybe-funcall-regexp (caar entry)))
           markdown-fenced-block-pairs)
   "\\|"))

(defun markdown-get-fenced-block-begin-properties ()
  (cl-mapcar (lambda (entry) (cl-cadar entry)) markdown-fenced-block-pairs))

(defun markdown-get-fenced-block-end-properties ()
  (cl-mapcar (lambda (entry) (cl-cadadr entry)) markdown-fenced-block-pairs))

(defun markdown-get-fenced-block-middle-properties ()
  (cl-mapcar #'cl-third markdown-fenced-block-pairs))

(defun markdown-find-previous-prop (prop &optional lim)
  "Find previous place where property PROP is non-nil, up to LIM.
Return a cons of (pos . property). pos is point if point contains
non-nil PROP."
  (let ((res
         (if (get-text-property (point) prop) (point)
           (previous-single-property-change
            (point) prop nil (or lim (point-min))))))
    (when (and (not (get-text-property res prop))
               (> res (point-min))
               (get-text-property (1- res) prop))
      (cl-decf res))
    (when (and res (get-text-property res prop)) (cons res prop))))

(defun markdown-find-next-prop (prop &optional lim)
  "Find next place where property PROP is non-nil, up to LIM.
Return a cons of (POS . PROPERTY) where POS is point if point
contains non-nil PROP."
  (let ((res
         (if (get-text-property (point) prop) (point)
           (next-single-property-change
            (point) prop nil (or lim (point-max))))))
    (when (and res (get-text-property res prop)) (cons res prop))))

(defun markdown-min-of-seq (map-fn seq)
  "Apply MAP-FN to SEQ and return element of SEQ with minimum value of MAP-FN."
  (cl-loop for el in seq
           with min = 1.0e+INF          ; infinity
           with min-el = nil
           do (let ((res (funcall map-fn el)))
                (when (< res min)
                  (setq min res)
                  (setq min-el el)))
           finally return min-el))

(defun markdown-max-of-seq (map-fn seq)
  "Apply MAP-FN to SEQ and return element of SEQ with maximum value of MAP-FN."
  (cl-loop for el in seq
           with max = -1.0e+INF          ; negative infinity
           with max-el = nil
           do (let ((res (funcall map-fn el)))
                (when (and res (> res max))
                  (setq max res)
                  (setq max-el el)))
           finally return max-el))

(defun markdown-find-previous-block ()
  "Find previous block.
Detect whether `markdown-syntax-propertize-fenced-block-constructs' was
unable to propertize the entire block, but was able to propertize the beginning
of the block. If so, return a cons of (pos . property) where the beginning of
the block was propertized."
  (let ((start-pt (point))
        (closest-open
         (markdown-max-of-seq
          #'car
          (cl-remove-if
           #'null
           (cl-mapcar
            #'markdown-find-previous-prop
            (markdown-get-fenced-block-begin-properties))))))
    (when closest-open
      (let* ((length-of-open-match
              (let ((match-d
                     (get-text-property (car closest-open) (cdr closest-open))))
                (- (cl-fourth match-d) (cl-third match-d))))
             (end-regexp
              (markdown-maybe-funcall-regexp
               (cl-caadr
                (cl-find-if
                 (lambda (entry) (eq (cl-cadar entry) (cdr closest-open)))
                 markdown-fenced-block-pairs))
               length-of-open-match))
             (end-prop-loc
              (save-excursion
                (save-match-data
                  (goto-char (car closest-open))
                  (and (re-search-forward end-regexp start-pt t)
                       (match-beginning 0))))))
        (and (not end-prop-loc) closest-open)))))

(defun markdown-get-fenced-block-from-start (prop)
  "Return limits of an enclosing fenced block from its start, using PROP.
Return value is a list usable as `match-data'."
  (catch 'no-rest-of-block
    (let* ((correct-entry
            (cl-find-if
             (lambda (entry) (eq (cl-cadar entry) prop))
             markdown-fenced-block-pairs))
           (begin-of-begin (cl-first (markdown-text-property-at-point prop)))
           (middle-prop (cl-third correct-entry))
           (end-prop (cl-cadadr correct-entry))
           (end-of-end
            (save-excursion
              (goto-char (match-end 0))   ; end of begin
              (unless (eobp) (forward-char))
              (let ((mid-prop-v (markdown-text-property-at-point middle-prop)))
                (if (not mid-prop-v)    ; no middle
                    (progn
                      ;; try to find end by advancing one
                      (let ((end-prop-v
                             (markdown-text-property-at-point end-prop)))
                        (if end-prop-v (cl-second end-prop-v)
                          (throw 'no-rest-of-block nil))))
                  (set-match-data mid-prop-v)
                  (goto-char (match-end 0))   ; end of middle
                  (beginning-of-line)         ; into end
                  (cl-second (markdown-text-property-at-point end-prop)))))))
      (list begin-of-begin end-of-end))))

(defun markdown-get-fenced-block-from-middle (prop)
  "Return limits of an enclosing fenced block from its middle, using PROP.
Return value is a list usable as `match-data'."
  (let* ((correct-entry
          (cl-find-if
           (lambda (entry) (eq (cl-third entry) prop))
           markdown-fenced-block-pairs))
         (begin-prop (cl-cadar correct-entry))
         (begin-of-begin
          (save-excursion
            (goto-char (match-beginning 0))
            (unless (bobp) (forward-line -1))
            (beginning-of-line)
            (cl-first (markdown-text-property-at-point begin-prop))))
         (end-prop (cl-cadadr correct-entry))
         (end-of-end
          (save-excursion
            (goto-char (match-end 0))
            (beginning-of-line)
            (cl-second (markdown-text-property-at-point end-prop)))))
    (list begin-of-begin end-of-end)))

(defun markdown-get-fenced-block-from-end (prop)
  "Return limits of an enclosing fenced block from its end, using PROP.
Return value is a list usable as `match-data'."
  (let* ((correct-entry
          (cl-find-if
           (lambda (entry) (eq (cl-cadadr entry) prop))
           markdown-fenced-block-pairs))
         (end-of-end (cl-second (markdown-text-property-at-point prop)))
         (middle-prop (cl-third correct-entry))
         (begin-prop (cl-cadar correct-entry))
         (begin-of-begin
          (save-excursion
            (goto-char (match-beginning 0)) ; beginning of end
            (unless (bobp) (backward-char)) ; into middle
            (let ((mid-prop-v (markdown-text-property-at-point middle-prop)))
              (if (not mid-prop-v)
                  (progn
                    (beginning-of-line)
                    (cl-first (markdown-text-property-at-point begin-prop)))
                (set-match-data mid-prop-v)
                (goto-char (match-beginning 0))   ; beginning of middle
                (unless (bobp) (forward-line -1)) ; into beginning
                (beginning-of-line)
                (cl-first (markdown-text-property-at-point begin-prop)))))))
    (list begin-of-begin end-of-end)))

(defun markdown-get-enclosing-fenced-block-construct (&optional pos)
  "Get \"fake\" match data for block enclosing POS.
Returns fake match data which encloses the start, middle, and end
of the block construct enclosing POS, if it exists. Used in
`markdown-code-block-at-pos'."
  (save-excursion
    (when pos (goto-char pos))
    (beginning-of-line)
    (car
     (cl-remove-if
      #'null
      (cl-mapcar
       (lambda (fun-and-prop)
         (cl-destructuring-bind (fun prop) fun-and-prop
           (when prop
             (save-match-data
               (set-match-data (markdown-text-property-at-point prop))
               (funcall fun prop)))))
       `((markdown-get-fenced-block-from-start
          ,(cl-find-if
            #'markdown-text-property-at-point
            (markdown-get-fenced-block-begin-properties)))
         (markdown-get-fenced-block-from-middle
          ,(cl-find-if
            #'markdown-text-property-at-point
            (markdown-get-fenced-block-middle-properties)))
         (markdown-get-fenced-block-from-end
          ,(cl-find-if
            #'markdown-text-property-at-point
            (markdown-get-fenced-block-end-properties)))))))))

(defun markdown-propertize-end-match (reg end fence-spec middle-begin)
  "Get match for REG up to END, if exists, and propertize appropriately.
FENCE-SPEC is an entry in `markdown-fenced-block-pairs' and
MIDDLE-BEGIN is the start of the \"middle\" section of the block."
  (when (re-search-forward reg end t)
    (let ((close-begin (match-beginning 0)) ; Start of closing line.
          (close-end (match-end 0))         ; End of closing line.
          (close-data (match-data t)))      ; Match data for closing line.
      ;; Propertize middle section of fenced block.
      (put-text-property middle-begin close-begin
                         (cl-third fence-spec)
                         (list middle-begin close-begin))
      ;; If the block is a YAML block, propertize the declarations inside
      (when (< middle-begin close-begin) ;; workaround #634
        (markdown-syntax-propertize-yaml-metadata middle-begin close-begin))
      ;; Propertize closing line of fenced block.
      (put-text-property close-begin close-end
                         (cl-cadadr fence-spec) close-data))))

(defun markdown--triple-quote-single-line-p (begin)
  (save-excursion
    (goto-char begin)
    (save-match-data
      (and (search-forward "```" nil t)
           (search-forward "```" (line-end-position) t)))))

(defun markdown-syntax-propertize-fenced-block-constructs (start end)
  "Propertize according to `markdown-fenced-block-pairs' from START to END.
If unable to propertize an entire block (if the start of a block is within START
and END, but the end of the block is not), propertize the start section of a
block, then in a subsequent call propertize both middle and end by finding the
start which was previously propertized."
  (let ((start-reg (markdown-get-start-fence-regexp)))
    (save-excursion
      (goto-char start)
      ;; start from previous unclosed block, if exists
      (let ((prev-begin-block (markdown-find-previous-block)))
        (when prev-begin-block
          (let* ((correct-entry
                  (cl-find-if (lambda (entry)
                                (eq (cdr prev-begin-block) (cl-cadar entry)))
                              markdown-fenced-block-pairs))
                 (enclosed-text-start (1+ (car prev-begin-block)))
                 (start-length
                  (save-excursion
                    (goto-char (car prev-begin-block))
                    (string-match
                     (markdown-maybe-funcall-regexp
                      (caar correct-entry))
                     (buffer-substring
                      (line-beginning-position) (line-end-position)))
                    (- (match-end 1) (match-beginning 1))))
                 (end-reg (markdown-maybe-funcall-regexp
                           (cl-caadr correct-entry) start-length)))
            (markdown-propertize-end-match
             end-reg end correct-entry enclosed-text-start))))
      ;; find all new blocks within region
      (while (re-search-forward start-reg end t)
        ;; we assume the opening constructs take up (only) an entire line,
        ;; so we re-check the current line
        (let* ((block-start (match-beginning 0))
               (cur-line (buffer-substring (line-beginning-position) (line-end-position)))
               ;; find entry in `markdown-fenced-block-pairs' corresponding
               ;; to regex which was matched
               (correct-entry
                (cl-find-if
                 (lambda (fenced-pair)
                   (string-match-p
                    (markdown-maybe-funcall-regexp (caar fenced-pair))
                    cur-line))
                 markdown-fenced-block-pairs))
               (enclosed-text-start
                (save-excursion (1+ (line-end-position))))
               (end-reg
                (markdown-maybe-funcall-regexp
                 (cl-caadr correct-entry)
                 (if (and (match-beginning 1) (match-end 1))
                     (- (match-end 1) (match-beginning 1))
                   0)))
               (prop (cl-cadar correct-entry)))
          (when (or (not (eq prop 'markdown-gfm-block-begin))
                    (not (markdown--triple-quote-single-line-p block-start)))
            ;; get correct match data
            (save-excursion
              (beginning-of-line)
              (re-search-forward
               (markdown-maybe-funcall-regexp (caar correct-entry))
               (line-end-position)))
            ;; mark starting, even if ending is outside of region
            (put-text-property (match-beginning 0) (match-end 0) prop (match-data t))
            (markdown-propertize-end-match
             end-reg end correct-entry enclosed-text-start)))))))

(defun markdown-syntax-propertize-blockquotes (start end)
  "Match blockquotes from START to END."
  (save-excursion
    (goto-char start)
    (while (and (re-search-forward markdown-regex-blockquote end t)
                (not (markdown-code-block-at-pos (match-beginning 0))))
      (put-text-property (match-beginning 0) (match-end 0)
                         'markdown-blockquote
                         (match-data t)))))

(defun markdown-syntax-propertize-hrs (start end)
  "Match horizontal rules from START to END."
  (save-excursion
    (goto-char start)
    (while (re-search-forward markdown-regex-hr end t)
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (goto-char beg)
        (unless (or (markdown-on-heading-p)
                    (markdown-code-block-at-point-p))
          (put-text-property beg end 'markdown-hr (match-data t)))
        (goto-char end)))))

(defun markdown-syntax-propertize-yaml-metadata (start end)
  "Propertize elements inside YAML metadata blocks from START to END.
Assumes region from START and END is already known to be the interior
region of a YAML metadata block as propertized by
`markdown-syntax-propertize-fenced-block-constructs'."
  (save-excursion
    (goto-char start)
    (cl-loop
     while (re-search-forward markdown-regex-declarative-metadata end t)
     do (progn
          (put-text-property (match-beginning 1) (match-end 1)
                             'markdown-metadata-key (match-data t))
          (put-text-property (match-beginning 2) (match-end 2)
                             'markdown-metadata-markup (match-data t))
          (put-text-property (match-beginning 3) (match-end 3)
                             'markdown-metadata-value (match-data t))))))

(defun markdown-syntax-propertize-headings (start end)
  "Match headings of type SYMBOL with REGEX from START to END."
  (goto-char start)
  (while (re-search-forward markdown-regex-header end t)
    (unless (markdown-code-block-at-pos (match-beginning 0))
      (put-text-property
       (match-beginning 0) (match-end 0) 'markdown-heading
       (match-data t))
      (put-text-property
       (match-beginning 0) (match-end 0)
       (cond ((match-string-no-properties 2) 'markdown-heading-1-setext)
             ((match-string-no-properties 3) 'markdown-heading-2-setext)
             (t (let ((atx-level (length (markdown-trim-whitespace
                                          (match-string-no-properties 4)))))
                  (intern (format "markdown-heading-%d-atx" atx-level)))))
       (match-data t)))))

(defun markdown-syntax-propertize-comments (start end)
  "Match HTML comments from the START to END."
  ;; Implement by loop instead of recursive call for avoiding
  ;; exceed max-lisp-eval-depth issue
  ;; https://github.com/jrblevin/markdown-mode/issues/536
  (let (finish)
    (goto-char start)
    (while (not finish)
      (let* ((in-comment (nth 4 (syntax-ppss)))
             (comment-begin (nth 8 (syntax-ppss))))
        (cond
         ;; Comment start
         ((and (not in-comment)
               (re-search-forward markdown-regex-comment-start end t)
               (not (markdown-inline-code-at-point-p))
               (not (markdown-code-block-at-point-p)))
          (let ((open-beg (match-beginning 0)))
            (put-text-property open-beg (1+ open-beg)
                               'syntax-table (string-to-syntax "<"))
            (goto-char (min (1+ (match-end 0)) end (point-max)))))
         ;; Comment end
         ((and in-comment comment-begin
               (re-search-forward markdown-regex-comment-end end t))
          (let ((comment-end (match-end 0)))
            (put-text-property (1- comment-end) comment-end
                               'syntax-table (string-to-syntax ">"))
            ;; Remove any other text properties inside the comment
            (remove-text-properties comment-begin comment-end
                                    markdown--syntax-properties)
            (put-text-property comment-begin comment-end
                               'markdown-comment (list comment-begin comment-end))
            (goto-char (min comment-end end (point-max)))))
         ;; Nothing found
         (t (setq finish t)))))
    nil))

(defun markdown-syntax-propertize (start end)
  "Function used as `syntax-propertize-function'.
START and END delimit region to propertize."
  (with-silent-modifications
    (save-excursion
      (remove-text-properties start end markdown--syntax-properties)
      (markdown-syntax-propertize-fenced-block-constructs start end)
      (markdown-syntax-propertize-list-items start end)
      (markdown-syntax-propertize-pre-blocks start end)
      (markdown-syntax-propertize-blockquotes start end)
      (markdown-syntax-propertize-headings start end)
      (markdown-syntax-propertize-hrs start end)
      (markdown-syntax-propertize-comments start end))))


;;; Markup Hiding =============================================================

(defconst markdown-markup-properties
  '(face markdown-markup-face invisible markdown-markup)
  "List of properties and values to apply to markup.")

(defconst markdown-language-keyword-properties
  '(face markdown-language-keyword-face invisible markdown-markup)
  "List of properties and values to apply to code block language names.")

(defconst markdown-language-info-properties
  '(face markdown-language-info-face invisible markdown-markup)
  "List of properties and values to apply to code block language info strings.")

(defconst markdown-include-title-properties
  '(face markdown-link-title-face invisible markdown-markup)
  "List of properties and values to apply to included code titles.")

(defcustom markdown-hide-markup nil
  "Determines whether markup in the buffer will be hidden.
When set to nil, all markup is displayed in the buffer as it
appears in the file.  An exception is when `markdown-hide-urls'
is non-nil.
Set this to a non-nil value to turn this feature on by default.
You can interactively toggle the value of this variable with
`markdown-toggle-markup-hiding', \\[markdown-toggle-markup-hiding],
or from the Markdown > Show & Hide menu.

Markup hiding works by adding text properties to positions in the
buffer---either the `invisible' property or the `display' property
in cases where alternative glyphs are used (e.g., list bullets).
This does not, however, affect printing or other output.
Functions such as `htmlfontify-buffer' and `ps-print-buffer' will
not honor these text properties.  For printing, it would be better
to first convert to HTML or PDF (e.g,. using Pandoc)."
  :group 'markdown
  :type 'boolean
  :safe 'booleanp
  :package-version '(markdown-mode . "2.3"))
(make-variable-buffer-local 'markdown-hide-markup)

(defun markdown-toggle-markup-hiding (&optional arg)
  "Toggle the display or hiding of markup.
With a prefix argument ARG, enable markup hiding if ARG is positive,
and disable it otherwise.
See `markdown-hide-markup' for additional details."
  (interactive (list (or current-prefix-arg 'toggle)))
  (setq markdown-hide-markup
        (if (eq arg 'toggle)
            (not markdown-hide-markup)
          (> (prefix-numeric-value arg) 0)))
  (if markdown-hide-markup
      (progn (add-to-invisibility-spec 'markdown-markup)
             (message "markdown-mode markup hiding enabled"))
    (progn (remove-from-invisibility-spec 'markdown-markup)
           (message "markdown-mode markup hiding disabled")))
  (markdown-reload-extensions))


;;; Font Lock =================================================================

(require 'font-lock)

(defgroup markdown-faces nil
  "Faces used in Markdown Mode."
  :group 'markdown
  :group 'faces)

(defface markdown-italic-face
  '((t (:inherit italic)))
  "Face for italic text."
  :group 'markdown-faces)

(defface markdown-bold-face
  '((t (:inherit bold)))
  "Face for bold text."
  :group 'markdown-faces)

(defface markdown-strike-through-face
  '((t (:strike-through t)))
  "Face for strike-through text."
  :group 'markdown-faces)

(defface markdown-markup-face
  '((t (:inherit shadow :slant normal :weight normal)))
  "Face for markup elements."
  :group 'markdown-faces)

(defface markdown-header-rule-face
  '((t (:inherit markdown-markup-face)))
  "Base face for headers rules."
  :group 'markdown-faces)

(defface markdown-header-delimiter-face
  '((t (:inherit markdown-markup-face)))
  "Base face for headers hash delimiter."
  :group 'markdown-faces)

(defface markdown-list-face
  '((t (:inherit markdown-markup-face)))
  "Face for list item markers."
  :group 'markdown-faces)

(defface markdown-blockquote-face
  '((t (:inherit font-lock-doc-face)))
  "Face for blockquote sections."
  :group 'markdown-faces)

(defface markdown-code-face
  '((t (:inherit fixed-pitch)))
  "Face for inline code, pre blocks, and fenced code blocks.
This may be used, for example, to add a contrasting background to
inline code fragments and code blocks."
  :group 'markdown-faces)

(defface markdown-inline-code-face
  '((t (:inherit (markdown-code-face font-lock-constant-face))))
  "Face for inline code."
  :group 'markdown-faces)

(defface markdown-pre-face
  '((t (:inherit (markdown-code-face font-lock-constant-face))))
  "Face for preformatted text."
  :group 'markdown-faces)

(defface markdown-table-face
  '((t (:inherit (markdown-code-face))))
  "Face for tables."
  :group 'markdown-faces)

(defface markdown-language-keyword-face
  '((t (:inherit font-lock-type-face)))
  "Face for programming language identifiers."
  :group 'markdown-faces)

(defface markdown-language-info-face
  '((t (:inherit font-lock-string-face)))
  "Face for programming language info strings."
  :group 'markdown-faces)

(defface markdown-link-face
  '((t (:inherit link)))
  "Face for links."
  :group 'markdown-faces)

(defface markdown-missing-link-face
  '((t (:inherit font-lock-warning-face)))
  "Face for missing links."
  :group 'markdown-faces)

(defface markdown-reference-face
  '((t (:inherit markdown-markup-face)))
  "Face for link references."
  :group 'markdown-faces)

(defface markdown-footnote-marker-face
  '((t (:inherit markdown-markup-face)))
  "Face for footnote markers."
  :group 'markdown-faces)

(defface markdown-footnote-text-face
  '((t (:inherit font-lock-comment-face)))
  "Face for footnote text."
  :group 'markdown-faces)

(defface markdown-url-face
  '((t (:inherit font-lock-string-face)))
  "Face for URLs that are part of markup.
For example, this applies to URLs in inline links:
[link text](http://example.com/)."
  :group 'markdown-faces)

(defface markdown-plain-url-face
  '((t (:inherit markdown-link-face)))
  "Face for URLs that are also links.
For example, this applies to plain angle bracket URLs:
<http://example.com/>."
  :group 'markdown-faces)

(defface markdown-link-title-face
  '((t (:inherit font-lock-comment-face)))
  "Face for reference link titles."
  :group 'markdown-faces)

(defface markdown-line-break-face
  '((t (:inherit font-lock-constant-face :underline t)))
  "Face for hard line breaks."
  :group 'markdown-faces)

(defface markdown-comment-face
  '((t (:inherit font-lock-comment-face)))
  "Face for HTML comments."
  :group 'markdown-faces)

(defface markdown-math-face
  '((t (:inherit font-lock-string-face)))
  "Face for LaTeX expressions."
  :group 'markdown-faces)

(defface markdown-metadata-key-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for metadata keys."
  :group 'markdown-faces)

(defface markdown-metadata-value-face
  '((t (:inherit font-lock-string-face)))
  "Face for metadata values."
  :group 'markdown-faces)

(defface markdown-gfm-checkbox-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for GFM checkboxes."
  :group 'markdown-faces)

(defface markdown-highlight-face
  '((t (:inherit highlight)))
  "Face for mouse highlighting."
  :group 'markdown-faces)

(defface markdown-hr-face
  '((t (:inherit markdown-markup-face)))
  "Face for horizontal rules."
  :group 'markdown-faces)

(defface markdown-html-tag-name-face
  '((t (:inherit font-lock-type-face)))
  "Face for HTML tag names."
  :group 'markdown-faces)

(defface markdown-html-tag-delimiter-face
  '((t (:inherit markdown-markup-face)))
  "Face for HTML tag delimiters."
  :group 'markdown-faces)

(defface markdown-html-attr-name-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for HTML attribute names."
  :group 'markdown-faces)

(defface markdown-html-attr-value-face
  '((t (:inherit font-lock-string-face)))
  "Face for HTML attribute values."
  :group 'markdown-faces)

(defface markdown-html-entity-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for HTML entities."
  :group 'markdown-faces)

(defface markdown-highlighting-face
  '((t (:background "yellow" :foreground "black")))
  "Face for highlighting."
  :group 'markdown-faces)

(defcustom markdown-header-scaling nil
  "Whether to use variable-height faces for headers.
When non-nil, `markdown-header-face' will inherit from
`variable-pitch' and the scaling values in
`markdown-header-scaling-values' will be applied to
headers of levels one through six respectively."
  :type 'boolean
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (set-default symbol value)
         (markdown-update-header-faces value))
  :group 'markdown-faces
  :package-version '(markdown-mode . "2.2"))

(defcustom markdown-header-scaling-values
  '(2.0 1.7 1.4 1.1 1.0 1.0)
  "List of scaling values for headers of level one through six.
Used when `markdown-header-scaling' is non-nil."
  :type '(repeat float)
  :initialize #'custom-initialize-default
  :set (lambda (symbol value)
         (set-default symbol value)
         (markdown-update-header-faces markdown-header-scaling value)))

(defmacro markdown--dotimes-when-compile (i-n body)
  (declare (indent 1) (debug ((symbolp form) form)))
  (let ((var (car i-n))
        (n (cadr i-n))
        (code ()))
    (dotimes (i (eval n t))
      (push (eval body `((,var . ,i))) code))
    `(progn ,@(nreverse code))))

(defface markdown-header-face
  `((t (:inherit (,@(when markdown-header-scaling '(variable-pitch))
                  font-lock-function-name-face)
        :weight bold)))
  "Base face for headers.")

(markdown--dotimes-when-compile (num 6)
  (let* ((num1 (1+ num))
         (face-name (intern (format "markdown-header-face-%s" num1))))
    `(defface ,face-name
       (,'\` ((t (:inherit markdown-header-face
                  :height
                  (,'\, (if markdown-header-scaling
                            (float (nth ,num markdown-header-scaling-values))
                          1.0))))))
       (format "Face for level %s headers.
You probably don't want to customize this face directly. Instead
you can customize the base face `markdown-header-face' or the
variable-height variable `markdown-header-scaling'." ,num1))))

(defun markdown-update-header-faces (&optional scaling scaling-values)
  "Update header faces, depending on if header SCALING is desired.
If so, use given list of SCALING-VALUES relative to the baseline
size of `markdown-header-face'."
  (dotimes (num 6)
    (let* ((face-name (intern (format "markdown-header-face-%s" (1+ num))))
           (scale (cond ((not scaling) 1.0)
                        (scaling-values (float (nth num scaling-values)))
                        (t (float (nth num markdown-header-scaling-values))))))
      (unless (get face-name 'saved-face) ; Don't update customized faces
        (set-face-attribute face-name nil :height scale)))))

(defun markdown-syntactic-face (state)
  "Return font-lock face for characters with given STATE.
See `font-lock-syntactic-face-function' for details."
  (let ((in-comment (nth 4 state)))
    (cond
     (in-comment 'markdown-comment-face)
     (t nil))))

(defcustom markdown-list-item-bullets
  '("●" "◎" "○" "◆" "◇" "►" "•")
  "List of bullets to use for unordered lists.
It can contain any number of symbols, which will be repeated.
Depending on your font, some reasonable choices are:
♥ ● ◇ ✚ ✜ ☯ ◆ ♠ ♣ ♦ ❀ ◆ ◖ ▶ ► • ★ ▸."
  :group 'markdown
  :type '(repeat (string :tag "Bullet character"))
  :package-version '(markdown-mode . "2.3"))

(defun markdown--footnote-marker-properties ()
  "Return a font-lock facespec expression for footnote marker text."
  `(face markdown-footnote-marker-face
         ,@(when markdown-hide-markup
             `(display ,markdown-footnote-display))))

(defun markdown--pandoc-inline-footnote-properties ()
  "Return a font-lock facespec expression for Pandoc inline footnote text."
  `(face markdown-footnote-text-face
         ,@(when markdown-hide-markup
             `(display ,markdown-footnote-display))))

(defvar markdown-mode-font-lock-keywords
  `((markdown-match-yaml-metadata-begin . ((1 'markdown-markup-face)))
    (markdown-match-yaml-metadata-end . ((1 'markdown-markup-face)))
    (markdown-match-yaml-metadata-key . ((1 'markdown-metadata-key-face)
                                         (2 'markdown-markup-face)
                                         (3 'markdown-metadata-value-face)))
    (markdown-match-gfm-open-code-blocks . ((1 markdown-markup-properties)
                                            (2 markdown-markup-properties nil t)
                                            (3 markdown-language-keyword-properties nil t)
                                            (4 markdown-language-info-properties nil t)
                                            (5 markdown-markup-properties nil t)))
    (markdown-match-gfm-close-code-blocks . ((0 markdown-markup-properties)))
    (markdown-fontify-gfm-code-blocks)
    (markdown-fontify-tables)
    (markdown-match-fenced-start-code-block . ((1 markdown-markup-properties)
                                               (2 markdown-markup-properties nil t)
                                               (3 markdown-language-keyword-properties nil t)
                                               (4 markdown-language-info-properties nil t)
                                               (5 markdown-markup-properties nil t)))
    (markdown-match-fenced-end-code-block . ((0 markdown-markup-properties)))
    (markdown-fontify-fenced-code-blocks)
    (markdown-match-pre-blocks . ((0 'markdown-pre-face)))
    (markdown-fontify-headings)
    (markdown-match-declarative-metadata . ((1 'markdown-metadata-key-face)
                                            (2 'markdown-markup-face)
                                            (3 'markdown-metadata-value-face)))
    (markdown-match-pandoc-metadata . ((1 'markdown-markup-face)
                                       (2 'markdown-markup-face)
                                       (3 'markdown-metadata-value-face)))
    (markdown-fontify-hrs)
    (markdown-match-code . ((1 markdown-markup-properties prepend)
                            (2 'markdown-inline-code-face prepend)
                            (3 markdown-markup-properties prepend)))
    (,markdown-regex-kbd . ((1 markdown-markup-properties)
                            (2 'markdown-inline-code-face)
                            (3 markdown-markup-properties)))
    (markdown-fontify-angle-uris)
    (,markdown-regex-email . 'markdown-plain-url-face)
    (markdown-match-html-tag . ((1 'markdown-html-tag-delimiter-face t)
                                (2 'markdown-html-tag-name-face t)
                                (3 'markdown-html-tag-delimiter-face t)
                                ;; Anchored matcher for HTML tag attributes
                                (,markdown-regex-html-attr
                                 ;; Before searching, move past tag
                                 ;; name; set limit at tag close.
                                 (progn
                                   (goto-char (match-end 2)) (match-end 3))
                                 nil
                                 . ((1 'markdown-html-attr-name-face)
                                    (3 'markdown-html-tag-delimiter-face nil t)
                                    (4 'markdown-html-attr-value-face nil t)))))
    (,markdown-regex-html-entity . 'markdown-html-entity-face)
    (markdown-fontify-list-items)
    (,markdown-regex-footnote . ((1 markdown-markup-properties)    ; [^
                                 (2 (markdown--footnote-marker-properties)) ; label
                                 (3 markdown-markup-properties)))  ; ]
    (,markdown-regex-pandoc-inline-footnote . ((1 markdown-markup-properties)   ; ^
                                               (2 markdown-markup-properties)   ; [
                                               (3 (markdown--pandoc-inline-footnote-properties)) ; text
                                               (4 markdown-markup-properties))) ; ]
    (markdown-match-includes . ((1 markdown-markup-properties)
                                (2 markdown-markup-properties nil t)
                                (3 markdown-include-title-properties nil t)
                                (4 markdown-markup-properties nil t)
                                (5 markdown-markup-properties)
                                (6 'markdown-url-face)
                                (7 markdown-markup-properties)))
    (markdown-fontify-inline-links)
    (markdown-fontify-reference-links)
    (,markdown-regex-reference-definition . ((1 'markdown-markup-face) ; [
                                             (2 'markdown-reference-face) ; label
                                             (3 'markdown-markup-face)    ; ]
                                             (4 'markdown-markup-face)    ; :
                                             (5 'markdown-url-face)       ; url
                                             (6 'markdown-link-title-face))) ; "title" (optional)
    (markdown-fontify-plain-uris)
    ;; Math mode $..$
    (markdown-match-math-single . ((1 'markdown-markup-face prepend)
                                   (2 'markdown-math-face append)
                                   (3 'markdown-markup-face prepend)))
    ;; Math mode $$..$$
    (markdown-match-math-double . ((1 'markdown-markup-face prepend)
                                   (2 'markdown-math-face append)
                                   (3 'markdown-markup-face prepend)))
    ;; Math mode \[..\] and \\[..\\]
    (markdown-match-math-display . ((1 'markdown-markup-face prepend)
                                    (3 'markdown-math-face append)
                                    (4 'markdown-markup-face prepend)))
    (markdown-match-bold . ((1 markdown-markup-properties prepend)
                            (2 'markdown-bold-face append)
                            (3 markdown-markup-properties prepend)))
    (markdown-match-italic . ((1 markdown-markup-properties prepend)
                              (2 'markdown-italic-face append)
                              (3 markdown-markup-properties prepend)))
    (,markdown-regex-strike-through . ((3 markdown-markup-properties)
                                       (4 'markdown-strike-through-face)
                                       (5 markdown-markup-properties)))
    (markdown--match-highlighting . ((3 markdown-markup-properties)
                                     (4 'markdown-highlighting-face)
                                     (5 markdown-markup-properties)))
    (,markdown-regex-line-break . (1 'markdown-line-break-face prepend))
    (markdown-match-escape . ((1 markdown-markup-properties prepend)))
    (markdown-fontify-sub-superscripts)
    (markdown-match-inline-attributes . ((0 markdown-markup-properties prepend)))
    (markdown-match-leanpub-sections . ((0 markdown-markup-properties)))
    (markdown-fontify-blockquotes)
    (markdown-match-wiki-link . ((0 'markdown-link-face prepend))))
  "Syntax highlighting for Markdown files.")

;; Footnotes
(defvar-local markdown-footnote-counter 0
  "Counter for footnote numbers.")

(defconst markdown-footnote-chars
  "[[:alnum:]-]"
  "Regular expression matching any character for a footnote identifier.")

(defconst markdown-regex-footnote-definition
  (concat "^ \\{0,3\\}\\[\\(\\^" markdown-footnote-chars "*?\\)\\]:\\(?:[ \t]+\\|$\\)")
  "Regular expression matching a footnote definition, capturing the label.")


;;; Compatibility =============================================================

(defun markdown--pandoc-reference-p ()
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (when (and bounds (char-before (car bounds)))
      (= (char-before (car bounds)) ?@))))

(defun markdown-flyspell-check-word-p ()
  "Return t if `flyspell' should check word just before point.
Used for `flyspell-generic-check-word-predicate'."
  (save-excursion
    (goto-char (1- (point)))
    ;; https://github.com/jrblevin/markdown-mode/issues/560
    ;; enable spell check YAML meta data
    (if (or (and (markdown-code-block-at-point-p)
                 (not (markdown-text-property-at-point 'markdown-yaml-metadata-section)))
            (markdown-inline-code-at-point-p)
            (markdown-in-comment-p)
            (markdown--face-p (point) '(markdown-reference-face
                                        markdown-markup-face
                                        markdown-plain-url-face
                                        markdown-inline-code-face
                                        markdown-url-face))
            (markdown--pandoc-reference-p))
        (prog1 nil
          ;; If flyspell overlay is put, then remove it
          (let ((bounds (bounds-of-thing-at-point 'word)))
            (when bounds
              (cl-loop for ov in (overlays-in (car bounds) (cdr bounds))
                       when (overlay-get ov 'flyspell-overlay)
                       do
                       (delete-overlay ov)))))
      t)))


;;; Markdown Parsing Functions ================================================

(defun markdown-cur-line-blank-p ()
  "Return t if the current line is blank and nil otherwise."
  (save-excursion
    (beginning-of-line)
    (looking-at-p markdown-regex-blank-line)))

(defun markdown-prev-line-blank ()
  "Return t if the previous line is blank and nil otherwise.
If we are at the first line, then consider the previous line to be blank."
  (or (= (line-beginning-position) (point-min))
      (save-excursion
        (forward-line -1)
        (looking-at markdown-regex-blank-line))))

(defun markdown-prev-line-blank-p ()
  "Like `markdown-prev-line-blank', but preserve `match-data'."
  (save-match-data (markdown-prev-line-blank)))

(defun markdown-next-line-blank-p ()
  "Return t if the next line is blank and nil otherwise.
If we are at the last line, then consider the next line to be blank."
  (or (= (line-end-position) (point-max))
      (save-excursion
        (forward-line 1)
        (markdown-cur-line-blank-p))))

(defun markdown-prev-line-indent ()
  "Return the number of leading whitespace characters in the previous line.
Return 0 if the current line is the first line in the buffer."
  (save-excursion
    (if (= (line-beginning-position) (point-min))
        0
      (forward-line -1)
      (current-indentation))))

(defun markdown-next-line-indent ()
  "Return the number of leading whitespace characters in the next line.
Return 0 if line is the last line in the buffer."
  (save-excursion
    (if (= (line-end-position) (point-max))
        0
      (forward-line 1)
      (current-indentation))))

(defun markdown-new-baseline ()
  "Determine if the current line begins a new baseline level.
Assume point is positioned at beginning of line."
  (or (looking-at markdown-regex-header)
      (looking-at markdown-regex-hr)
      (and (= (current-indentation) 0)
           (not (looking-at markdown-regex-list))
           (markdown-prev-line-blank))))

(defun markdown-search-backward-baseline ()
  "Search backward baseline point with no indentation and not a list item."
  (end-of-line)
  (let (stop)
    (while (not (or stop (bobp)))
      (re-search-backward markdown-regex-block-separator-noindent nil t)
      (when (match-end 2)
        (goto-char (match-end 2))
        (cond
         ((markdown-new-baseline)
          (setq stop t))
         ((looking-at-p markdown-regex-list)
          (setq stop nil))
         (t (setq stop t)))))))

(defun markdown-update-list-levels (marker indent levels)
  "Update list levels given list MARKER, block INDENT, and current LEVELS.
Here, MARKER is a string representing the type of list, INDENT is an integer
giving the indentation, in spaces, of the current block, and LEVELS is a
list of the indentation levels of parent list items.  When LEVELS is nil,
it means we are at baseline (not inside of a nested list)."
  (cond
   ;; New list item at baseline.
   ((and marker (null levels))
    (setq levels (list indent)))
   ;; List item with greater indentation (four or more spaces).
   ;; Increase list level.
   ((and marker (>= indent (+ (car levels) markdown-list-indent-width)))
    (setq levels (cons indent levels)))
   ;; List item with greater or equal indentation (less than four spaces).
   ;; Do not increase list level.
   ((and marker (>= indent (car levels)))
    levels)
   ;; Lesser indentation level.
   ;; Pop appropriate number of elements off LEVELS list (e.g., lesser
   ;; indentation could move back more than one list level).  Note
   ;; that this block need not be the beginning of list item.
   ((< indent (car levels))
    (while (and (> (length levels) 1)
                (< indent (+ (cadr levels) markdown-list-indent-width)))
      (setq levels (cdr levels)))
    levels)
   ;; Otherwise, do nothing.
   (t levels)))

(defun markdown-calculate-list-levels ()
  "Calculate list levels at point.
Return a list of the form (n1 n2 n3 ...) where n1 is the
indentation of the deepest nested list item in the branch of
the list at the point, n2 is the indentation of the parent
list item, and so on.  The depth of the list item is therefore
the length of the returned list.  If the point is not at or
immediately  after a list item, return nil."
  (save-excursion
    (let ((first (point)) levels indent pre-regexp)
      ;; Find a baseline point with zero list indentation
      (markdown-search-backward-baseline)
      ;; Search for all list items between baseline and LOC
      (while (and (< (point) first)
                  (re-search-forward markdown-regex-list first t))
        (setq pre-regexp (format "^\\(    \\|\t\\)\\{%d\\}" (1+ (length levels))))
        (beginning-of-line)
        (cond
         ;; Make sure this is not a header or hr
         ((markdown-new-baseline) (setq levels nil))
         ;; Make sure this is not a line from a pre block
         ((looking-at-p pre-regexp))
         ;; If not, then update levels
         (t
          (setq indent (current-indentation))
          (setq levels (markdown-update-list-levels (match-string 2)
                                                    indent levels))))
        (end-of-line))
      levels)))

(defun markdown-prev-list-item (level)
  "Search backward from point for a list item with indentation LEVEL.
Set point to the beginning of the item, and return point, or nil
upon failure."
  (let (bounds indent prev)
    (setq prev (point))
    (forward-line -1)
    (setq indent (current-indentation))
    (while
        (cond
         ;; List item
         ((and (looking-at-p markdown-regex-list)
               (setq bounds (markdown-cur-list-item-bounds)))
          (cond
           ;; Stop and return point at item of equal indentation
           ((= (nth 3 bounds) level)
            (setq prev (point))
            nil)
           ;; Stop and return nil at item with lesser indentation
           ((< (nth 3 bounds) level)
            (setq prev nil)
            nil)
           ;; Stop at beginning of buffer
           ((bobp) (setq prev nil))
           ;; Continue at item with greater indentation
           ((> (nth 3 bounds) level) t)))
         ;; Stop at beginning of buffer
         ((bobp) (setq prev nil))
         ;; Continue if current line is blank
         ((markdown-cur-line-blank-p) t)
         ;; Continue while indentation is the same or greater
         ((>= indent level) t)
         ;; Stop if current indentation is less than list item
         ;; and the next is blank
         ((and (< indent level)
               (markdown-next-line-blank-p))
          (setq prev nil))
         ;; Stop at a header
         ((looking-at-p markdown-regex-header) (setq prev nil))
         ;; Stop at a horizontal rule
         ((looking-at-p markdown-regex-hr) (setq prev nil))
         ;; Otherwise, continue.
         (t t))
      (forward-line -1)
      (setq indent (current-indentation)))
    prev))

(defun markdown-next-list-item (level)
  "Search forward from point for the next list item with indentation LEVEL.
Set point to the beginning of the item, and return point, or nil
upon failure."
  (let (bounds indent next)
    (setq next (point))
    (if (looking-at markdown-regex-header-setext)
        (goto-char (match-end 0)))
    (forward-line)
    (setq indent (current-indentation))
    (while
        (cond
         ;; Stop at end of the buffer.
         ((eobp) nil)
         ;; Continue if the current line is blank
         ((markdown-cur-line-blank-p) t)
         ;; List item
         ((and (looking-at-p markdown-regex-list)
               (setq bounds (markdown-cur-list-item-bounds)))
          (cond
           ;; Continue at item with greater indentation
           ((> (nth 3 bounds) level) t)
           ;; Stop and return point at item of equal indentation
           ((= (nth 3 bounds) level)
            (setq next (point))
            nil)
           ;; Stop and return nil at item with lesser indentation
           ((< (nth 3 bounds) level)
            (setq next nil)
            nil)))
         ;; Continue while indentation is the same or greater
         ((>= indent level) t)
         ;; Stop if current indentation is less than list item
         ;; and the previous line was blank.
         ((and (< indent level)
               (markdown-prev-line-blank-p))
          (setq next nil))
         ;; Stop at a header
         ((looking-at-p markdown-regex-header) (setq next nil))
         ;; Stop at a horizontal rule
         ((looking-at-p markdown-regex-hr) (setq next nil))
         ;; Otherwise, continue.
         (t t))
      (forward-line)
      (setq indent (current-indentation)))
    next))

(defun markdown-cur-list-item-end (level)
  "Move to end of list item with pre-marker indentation LEVEL.
Return the point at the end when a list item was found at the
original point.  If the point is not in a list item, do nothing."
  (let (indent)
    (forward-line)
    (setq indent (current-indentation))
    (while
        (cond
         ;; Stop at end of the buffer.
         ((eobp) nil)
         ;; Continue while indentation is the same or greater
         ((>= indent level) t)
         ;; Continue if the current line is blank
         ((looking-at markdown-regex-blank-line) t)
         ;; Stop if current indentation is less than list item
         ;; and the previous line was blank.
         ((and (< indent level)
               (markdown-prev-line-blank))
          nil)
         ;; Stop at a new list items of the same or lesser
         ;; indentation, headings, and horizontal rules.
         ((looking-at (concat "\\(?:" markdown-regex-list
                              "\\|" markdown-regex-header
                              "\\|" markdown-regex-hr "\\)"))
          nil)
         ;; Otherwise, continue.
         (t t))
      (forward-line)
      (setq indent (current-indentation)))
    ;; Don't skip over whitespace for empty list items (marker and
    ;; whitespace only), just move to end of whitespace.
    (if (save-excursion
          (beginning-of-line)
          (looking-at (concat markdown-regex-list "[ \t]*$")))
        (goto-char (match-end 3))
      (skip-chars-backward " \t\n"))
    (end-of-line)
    (point)))

(defun markdown-cur-list-item-bounds ()
  "Return bounds for list item at point.
Return a list of the following form:

    (begin end indent nonlist-indent marker checkbox match)

The named components are:

  - begin: Position of beginning of list item, including leading indentation.
  - end: Position of the end of the list item, including list item text.
  - indent: Number of characters of indentation before list marker (an integer).
  - nonlist-indent: Number characters of indentation, list
    marker, and whitespace following list marker (an integer).
  - marker: String containing the list marker and following whitespace
            (e.g., \"- \" or \"* \").
  - checkbox: String containing the GFM checkbox portion, if any,
    including any trailing whitespace before the text
    begins (e.g., \"[x] \").
  - match: match data for markdown-regex-list

As an example, for the following unordered list item

   - item

the returned list would be

    (1 14 3 5 \"- \" nil (1 6 1 4 4 5 5 6))

If the point is not inside a list item, return nil."
  (car (get-text-property (line-beginning-position) 'markdown-list-item)))

(defun markdown-list-item-at-point-p ()
  "Return t if there is a list item at the point and nil otherwise."
  (save-match-data (markdown-cur-list-item-bounds)))

(defun markdown-prev-list-item-bounds ()
  "Return bounds of previous item in the same list of any level.
The return value has the same form as that of
`markdown-cur-list-item-bounds'."
  (save-excursion
    (let ((cur-bounds (markdown-cur-list-item-bounds))
          (beginning-of-list (save-excursion (markdown-beginning-of-list)))
          stop)
      (when cur-bounds
        (goto-char (nth 0 cur-bounds))
        (while (and (not stop) (not (bobp))
                    (re-search-backward markdown-regex-list
                                        beginning-of-list t))
          (unless (or (looking-at markdown-regex-hr)
                      (markdown-code-block-at-point-p))
            (setq stop (point))))
        (markdown-cur-list-item-bounds)))))

(defun markdown-next-list-item-bounds ()
  "Return bounds of next item in the same list of any level.
The return value has the same form as that of
`markdown-cur-list-item-bounds'."
  (save-excursion
    (let ((cur-bounds (markdown-cur-list-item-bounds))
          (end-of-list (save-excursion (markdown-end-of-list)))
          stop)
      (when cur-bounds
        (goto-char (nth 0 cur-bounds))
        (end-of-line)
        (while (and (not stop) (not (eobp))
                    (re-search-forward markdown-regex-list
                                       end-of-list t))
          (unless (or (looking-at markdown-regex-hr)
                      (markdown-code-block-at-point-p))
            (setq stop (point))))
        (when stop
          (markdown-cur-list-item-bounds))))))

(defun markdown-beginning-of-list ()
  "Move point to beginning of list at point, if any."
  (interactive)
  (let ((orig-point (point))
        (list-begin (save-excursion
                      (markdown-search-backward-baseline)
                      ;; Stop at next list item, regardless of the indentation.
                      (markdown-next-list-item (point-max))
                      (when (looking-at markdown-regex-list)
                        (point)))))
    (when (and list-begin (<= list-begin orig-point))
      (goto-char list-begin))))

(defun markdown-end-of-list ()
  "Move point to end of list at point, if any."
  (interactive)
  (let ((start (point))
        (end (save-excursion
               (when (markdown-beginning-of-list)
                 ;; Items can't have nonlist-indent <= 1, so this
                 ;; moves past all list items.
                 (markdown-next-list-item 1)
                 (skip-syntax-backward "-")
                 (unless (eobp) (forward-char 1))
                 (point)))))
    (when (and end (>= end start))
      (goto-char end))))

(defun markdown-up-list ()
  "Move point to beginning of parent list item."
  (interactive)
  (let ((cur-bounds (markdown-cur-list-item-bounds)))
    (when cur-bounds
      (markdown-prev-list-item (1- (nth 3 cur-bounds)))
      (let ((up-bounds (markdown-cur-list-item-bounds)))
        (when (and up-bounds (< (nth 3 up-bounds) (nth 3 cur-bounds)))
          (point))))))

(defun markdown-bounds-of-thing-at-point (thing)
  "Call `bounds-of-thing-at-point' for THING with slight modifications.
Does not include trailing newlines when THING is \\='line.  Handles the
end of buffer case by setting both endpoints equal to the value of
`point-max', since an empty region will trigger empty markup insertion.
Return bounds of form (beg . end) if THING is found, or nil otherwise."
  (let* ((bounds (bounds-of-thing-at-point thing))
         (a (car bounds))
         (b (cdr bounds)))
    (when bounds
      (when (eq thing 'line)
        (cond ((and (eobp) (markdown-cur-line-blank-p))
               (setq a b))
              ((char-equal (char-before b) ?\^J)
               (setq b (1- b)))))
      (cons a b))))

(defun markdown-reference-definition (reference)
  "Find out whether Markdown REFERENCE is defined.
REFERENCE should not include the square brackets.
When REFERENCE is defined, return a list of the form (text start end)
containing the definition text itself followed by the start and end
locations of the text.  Otherwise, return nil.
Leave match data for `markdown-regex-reference-definition'
intact additional processing."
  (let ((reference (downcase reference)))
    (save-excursion
      (goto-char (point-min))
      (catch 'found
        (while (re-search-forward markdown-regex-reference-definition nil t)
          (when (string= reference (downcase (match-string-no-properties 2)))
            (throw 'found
                   (list (match-string-no-properties 5)
                         (match-beginning 5) (match-end 5)))))))))

(defun markdown-get-defined-references ()
  "Return all defined reference labels and their line numbers.
They does not include square brackets)."
  (save-excursion
    (goto-char (point-min))
    (let (refs)
      (while (re-search-forward markdown-regex-reference-definition nil t)
        (let ((target (match-string-no-properties 2)))
          (cl-pushnew
           (cons (downcase target)
                 (markdown-line-number-at-pos (match-beginning 2)))
           refs :test #'equal :key #'car)))
      (reverse refs))))

(defun markdown-get-used-uris ()
  "Return a list of all used URIs in the buffer."
  (save-excursion
    (goto-char (point-min))
    (let (uris)
      (while (re-search-forward
              (concat "\\(?:" markdown-regex-link-inline
                      "\\|" markdown-regex-angle-uri
                      "\\|" markdown-regex-uri
                      "\\|" markdown-regex-email
                      "\\)")
              nil t)
        (unless (or (markdown-inline-code-at-point-p)
                    (markdown-code-block-at-point-p))
          (cl-pushnew (or (match-string-no-properties 6)
                          (match-string-no-properties 10)
                          (match-string-no-properties 12)
                          (match-string-no-properties 13))
                      uris :test #'equal)))
      (reverse uris))))

(defun markdown-inline-code-at-pos (pos)
  "Return non-nil if there is an inline code fragment at POS.
Return nil otherwise.  Set match data according to
`markdown-match-code' upon success.
This function searches the block for a code fragment that
contains the point using `markdown-match-code'.  We do this
because `thing-at-point-looking-at' does not work reliably with
`markdown-regex-code'.

The match data is set as follows:
Group 1 matches the opening backquotes.
Group 2 matches the code fragment itself, without backquotes.
Group 3 matches the closing backquotes."
  (save-excursion
    (goto-char pos)
    (let ((old-point (point))
          (end-of-block (progn (markdown-end-of-text-block) (point)))
          found)
      (markdown-beginning-of-text-block)
      (while (and (markdown-match-code end-of-block)
                  (setq found t)
                  (< (match-end 0) old-point)))
      (let ((match-group (if (eq (char-after (match-beginning 0)) ?`) 0 1)))
        (and found                                        ; matched something
             (<= (match-beginning match-group) old-point) ; match contains old-point
             (> (match-end 0) old-point))))))

(defun markdown-inline-code-at-pos-p (pos)
  "Return non-nil if there is an inline code fragment at POS.
Like `markdown-inline-code-at-pos`, but preserves match data."
  (save-match-data (markdown-inline-code-at-pos pos)))

(defun markdown-inline-code-at-point ()
  "Return non-nil if the point is at an inline code fragment.
See `markdown-inline-code-at-pos' for details."
  (markdown-inline-code-at-pos (point)))

(defun markdown-inline-code-at-point-p (&optional pos)
  "Return non-nil if there is inline code at the POS.
This is a predicate function counterpart to
`markdown-inline-code-at-point' which does not modify the match
data.  See `markdown-code-block-at-point-p' for code blocks."
  (save-match-data (markdown-inline-code-at-pos (or pos (point)))))

(defun markdown-code-block-at-pos (pos)
  "Return match data list if there is a code block at POS.
Uses text properties at the beginning of the line position.
This includes pre blocks, tilde-fenced code blocks, and GFM
quoted code blocks.  Return nil otherwise."
  (let ((bol (save-excursion (goto-char pos) (line-beginning-position))))
    (or (get-text-property bol 'markdown-pre)
        (let* ((bounds (markdown-get-enclosing-fenced-block-construct pos))
               (second (cl-second bounds)))
          (if second
              ;; chunks are right open
              (when (< pos second)
                bounds)
            bounds)))))

;; Function was renamed to emphasize that it does not modify match-data.
(defalias 'markdown-code-block-at-point 'markdown-code-block-at-point-p)

(defun markdown-code-block-at-point-p (&optional pos)
  "Return non-nil if there is a code block at the POS.
This includes pre blocks, tilde-fenced code blocks, and GFM
quoted code blocks.  This function does not modify the match
data.  See `markdown-inline-code-at-point-p' for inline code."
  (save-match-data (markdown-code-block-at-pos (or pos (point)))))

(defun markdown-heading-at-point (&optional pos)
  "Return non-nil if there is a heading at the POS.
Set match data for `markdown-regex-header'."
  (let ((match-data (get-text-property (or pos (point)) 'markdown-heading)))
    (when match-data
      (set-match-data match-data)
      t)))

(defun markdown-pipe-at-bol-p ()
  "Return non-nil if the line begins with a pipe symbol.
This may be useful for tables and Pandoc's line_blocks extension."
  (char-equal (char-after (line-beginning-position)) ?|))


;;; Markdown Font Lock Matching Functions =====================================

(defun markdown-range-property-any (begin end prop prop-values)
  "Return t if PROP from BEGIN to END is equal to one of the given PROP-VALUES.
Also returns t if PROP is a list containing one of the PROP-VALUES.
Return nil otherwise."
  (let (props)
    (catch 'found
      (dolist (loc (number-sequence begin end))
        (when (setq props (get-text-property loc prop))
          (cond ((listp props)
                 ;; props is a list, check for membership
                 (dolist (val prop-values)
                   (when (memq val props) (throw 'found loc))))
                (t
                 ;; props is a scalar, check for equality
                 (dolist (val prop-values)
                   (when (eq val props) (throw 'found loc))))))))))

(defun markdown-range-properties-exist (begin end props)
  (cl-loop
   for loc in (number-sequence begin end)
   with result = nil
   while (not
          (setq result
                (cl-some (lambda (prop) (get-text-property loc prop)) props)))
   finally return result))

(defun markdown-match-inline-generic (regex last &optional faceless)
  "Match inline REGEX from the point to LAST.
When FACELESS is non-nil, do not return matches where faces have been applied."
  (when (re-search-forward regex last t)
    (let ((bounds (markdown-code-block-at-pos (match-beginning 1)))
          (face (and faceless (text-property-not-all
                               (match-beginning 0) (match-end 0) 'face nil))))
      (cond
       ;; In code block: move past it and recursively search again
       (bounds
        (when (< (goto-char (cl-second bounds)) last)
          (markdown-match-inline-generic regex last faceless)))
       ;; When faces are found in the match range, skip over the match and
       ;; recursively search again.
       (face
        (when (< (goto-char (match-end 0)) last)
          (markdown-match-inline-generic regex last faceless)))
       ;; Keep match data and return t when in bounds.
       (t
        (<= (match-end 0) last))))))

(defun markdown-match-code (last)
  "Match inline code fragments from point to LAST."
  (unless (bobp)
    (backward-char 1))
  (when (markdown-search-until-condition
         (lambda ()
           (and
            ;; Advance point in case of failure, but without exceeding last.
            (goto-char (min (1+ (match-beginning 1)) last))
            (not (markdown-in-comment-p (match-beginning 1)))
            (not (markdown-in-comment-p (match-end 1)))
            (not (markdown-code-block-at-pos (match-beginning 1)))))
         markdown-regex-code last t)
    (set-match-data (list (match-beginning 1) (match-end 1)
                          (match-beginning 2) (match-end 2)
                          (match-beginning 3) (match-end 3)
                          (match-beginning 4) (match-end 4)))
    (goto-char (min (1+ (match-end 0)) last (point-max)))
    t))

(defun markdown--gfm-markup-underscore-p (begin end)
  (let ((is-underscore (eql (char-after begin) ?_)))
    (if (not is-underscore)
        t
      (save-excursion
        (save-match-data
          (goto-char begin)
          (and (looking-back "\\(?:^\\|[[:blank:][:punct:]]\\)" (1- begin))
               (progn
                 (goto-char end)
                 (looking-at-p "\\(?:[[:blank:][:punct:]]\\|$\\)"))))))))

(defun markdown-match-bold (last)
  "Match inline bold from the point to LAST."
  (when (markdown-match-inline-generic markdown-regex-bold last)
    (let ((is-gfm (derived-mode-p 'gfm-mode))
          (begin (match-beginning 2))
          (end (match-end 2)))
      (if (or (markdown-inline-code-at-pos-p begin)
              (markdown-inline-code-at-pos-p end)
              (markdown-in-comment-p)
              (markdown-range-property-any
               begin begin 'face '(markdown-url-face
                                   markdown-plain-url-face))
              (markdown-range-property-any
               begin end 'face '(markdown-hr-face
                                 markdown-math-face))
              (and is-gfm (not (markdown--gfm-markup-underscore-p begin end))))
          (progn (goto-char (min (1+ begin) last))
                 (when (< (point) last)
                   (markdown-match-bold last)))
        (set-match-data (list (match-beginning 2) (match-end 2)
                              (match-beginning 3) (match-end 3)
                              (match-beginning 4) (match-end 4)
                              (match-beginning 5) (match-end 5)))
        t))))

(defun markdown-match-italic (last)
  "Match inline italics from the point to LAST."
  (let* ((is-gfm (derived-mode-p 'gfm-mode))
         (regex (if is-gfm
                    markdown-regex-gfm-italic
                  markdown-regex-italic)))
    (when (and (markdown-match-inline-generic regex last)
               (not (markdown--face-p
                     (match-beginning 1)
                     '(markdown-html-attr-name-face markdown-html-attr-value-face))))
      (let ((begin (match-beginning 1))
            (end (match-end 1))
            (close-end (match-end 4)))
        (if (or (eql (char-before begin) (char-after begin))
                (markdown-inline-code-at-pos-p begin)
                (markdown-inline-code-at-pos-p (1- end))
                (markdown-in-comment-p)
                (markdown-range-property-any
                 begin begin 'face '(markdown-url-face
                                     markdown-plain-url-face
                                     markdown-markup-face))
                (markdown-range-property-any
                 begin end 'face '(markdown-bold-face
                                   markdown-list-face
                                   markdown-hr-face
                                   markdown-math-face))
                (and is-gfm
                     (or (char-equal (char-after begin) (char-after (1+ begin))) ;; check bold case
                         (not (markdown--gfm-markup-underscore-p begin close-end)))))
            (progn (goto-char (min (1+ begin) last))
                   (when (< (point) last)
                     (markdown-match-italic last)))
          (set-match-data (list (match-beginning 1) (match-end 1)
                                (match-beginning 2) (match-end 2)
                                (match-beginning 3) (match-end 3)
                                (match-beginning 4) (match-end 4)))
          t)))))

(defun markdown--match-highlighting (last)
  (when markdown-enable-highlighting-syntax
    (re-search-forward markdown-regex-highlighting last t)))

(defun markdown-match-escape (last)
  "Match escape characters (backslashes) from point to LAST.
Backlashes only count as escape characters outside of literal
regions (e.g. code blocks). See `markdown-literal-faces'."
  (catch 'found
    (while (search-forward-regexp markdown-regex-escape last t)
      (let* ((face (get-text-property (match-beginning 1) 'face))
             (face-list (if (listp face) face (list face))))
        ;; Ignore any backslashes with a literal face.
        (unless (cl-intersection face-list markdown-literal-faces)
          (throw 'found t))))))

(defun markdown-match-math-generic (regex last)
  "Match REGEX from point to LAST.
REGEX is either `markdown-regex-math-inline-single' for matching
$..$ or `markdown-regex-math-inline-double' for matching $$..$$."
  (when (markdown-match-inline-generic regex last)
    (let ((begin (match-beginning 1)) (end (match-end 1)))
      (prog1
          (if (or (markdown-range-property-any
                   begin end 'face
                   '(markdown-inline-code-face markdown-bold-face))
                  (markdown-range-properties-exist
                   begin end
                   (markdown-get-fenced-block-middle-properties)))
              (markdown-match-math-generic regex last)
            t)
        (goto-char (1+ (match-end 0)))))))

(defun markdown-match-list-items (last)
  "Match list items from point to LAST."
  (let* ((first (point))
         (pos first)
         (prop 'markdown-list-item)
         (bounds (car (get-text-property pos prop))))
    (while
        (and (or (null (setq bounds (car (get-text-property pos prop))))
                 (< (cl-first bounds) pos))
             (< (point) last)
             (setq pos (next-single-property-change pos prop nil last))
             (goto-char pos)))
    (when bounds
      (set-match-data (cl-seventh bounds))
      ;; Step at least one character beyond point. Otherwise
      ;; `font-lock-fontify-keywords-region' infloops.
      (goto-char (min (1+ (max (line-end-position) first))
                      (point-max)))
      t)))

(defun markdown-match-math-single (last)
  "Match single quoted $..$ math from point to LAST."
  (when markdown-enable-math
    (when (and (char-equal (char-after) ?$)
               (not (bolp))
               (not (char-equal (char-before) ?\\))
               (not (char-equal (char-before) ?$)))
      (forward-char -1))
    (markdown-match-math-generic markdown-regex-math-inline-single last)))

(defun markdown-match-math-double (last)
  "Match double quoted $$..$$ math from point to LAST."
  (when markdown-enable-math
    (when (and (< (1+ (point)) (point-max))
               (char-equal (char-after) ?$)
               (char-equal (char-after (1+ (point))) ?$)
               (not (bolp))
               (not (char-equal (char-before) ?\\))
               (not (char-equal (char-before) ?$)))
      (forward-char -1))
    (markdown-match-math-generic markdown-regex-math-inline-double last)))

(defun markdown-match-math-display (last)
  "Match bracketed display math \[..\] and \\[..\\] from point to LAST."
  (when markdown-enable-math
    (markdown-match-math-generic markdown-regex-math-display last)))

(defun markdown-match-propertized-text (property last)
  "Match text with PROPERTY from point to LAST.
Restore match data previously stored in PROPERTY."
  (let ((saved (get-text-property (point) property))
        pos)
    (unless saved
      (setq pos (next-single-property-change (point) property nil last))
      (unless (= pos last)
        (setq saved (get-text-property pos property))))
    (when saved
      (set-match-data saved)
      ;; Step at least one character beyond point. Otherwise
      ;; `font-lock-fontify-keywords-region' infloops.
      (goto-char (min (1+ (max (match-end 0) (point)))
                      (point-max)))
      saved)))

(defun markdown-match-pre-blocks (last)
  "Match preformatted blocks from point to LAST.
Use data stored in \\='markdown-pre text property during syntax
analysis."
  (markdown-match-propertized-text 'markdown-pre last))

(defun markdown-match-gfm-code-blocks (last)
  "Match GFM quoted code blocks from point to LAST.
Use data stored in \\='markdown-gfm-code text property during syntax
analysis."
  (markdown-match-propertized-text 'markdown-gfm-code last))

(defun markdown-match-gfm-open-code-blocks (last)
  (markdown-match-propertized-text 'markdown-gfm-block-begin last))

(defun markdown-match-gfm-close-code-blocks (last)
  (markdown-match-propertized-text 'markdown-gfm-block-end last))

(defun markdown-match-fenced-code-blocks (last)
  "Match fenced code blocks from the point to LAST."
  (markdown-match-propertized-text 'markdown-fenced-code last))

(defun markdown-match-fenced-start-code-block (last)
  (markdown-match-propertized-text 'markdown-tilde-fence-begin last))

(defun markdown-match-fenced-end-code-block (last)
  (markdown-match-propertized-text 'markdown-tilde-fence-end last))

(defun markdown-match-blockquotes (last)
  "Match blockquotes from point to LAST.
Use data stored in \\='markdown-blockquote text property during syntax
analysis."
  (markdown-match-propertized-text 'markdown-blockquote last))

(defun markdown-match-hr (last)
  "Match horizontal rules comments from the point to LAST."
  (markdown-match-propertized-text 'markdown-hr last))

(defun markdown-match-comments (last)
  "Match HTML comments from the point to LAST."
  (when (and (skip-syntax-forward "^<" last))
    (let ((beg (point)))
      (when (and (skip-syntax-forward "^>" last) (< (point) last))
        (forward-char)
        (set-match-data (list beg (point)))
        t))))

(defun markdown-match-generic-links (last ref)
  "Match inline links from point to LAST.
When REF is non-nil, match reference links instead of standard
links with URLs.
This function should only be used during font-lock, as it
determines syntax based on the presence of faces for previously
processed elements."
  ;; Search for the next potential link (not in a code block).
  (let ((prohibited-faces '(markdown-pre-face
                            markdown-code-face
                            markdown-inline-code-face
                            markdown-comment-face))
        found)
    (while
        (and (not found) (< (point) last)
             (progn
               ;; Clear match data to test for a match after functions returns.
               (set-match-data nil)
               ;; Preliminary regular expression search so we can return
               ;; quickly upon failure.  This doesn't handle malformed links
               ;; or nested square brackets well, so if it passes we back up
               ;; continue with a more precise search.
               (re-search-forward
                (if ref
                    markdown-regex-link-reference
                  markdown-regex-link-inline)
                last 'limit)))
      ;; Keep searching if this is in a code block, inline code, or a
      ;; comment, or if it is include syntax. The link text portion
      ;; (group 3) may contain inline code or comments, but the
      ;; markup, URL, and title should not be part of such elements.
      (if (or (markdown-range-property-any
               (match-beginning 0) (match-end 2) 'face prohibited-faces)
              (markdown-range-property-any
               (match-beginning 4) (match-end 0) 'face prohibited-faces)
              (and (char-equal (char-after (line-beginning-position)) ?<)
                   (char-equal (char-after (1+ (line-beginning-position))) ?<)))
          (set-match-data nil)
        (setq found t))))
  ;; Match opening exclamation point (optional) and left bracket.
  (when (match-beginning 2)
    (let* ((bang (match-beginning 1))
           (first-begin (match-beginning 2))
           ;; Find end of block to prevent matching across blocks.
           (end-of-block (save-excursion
                           (progn
                             (goto-char (match-beginning 2))
                             (markdown-end-of-text-block)
                             (point))))
           ;; Move over balanced expressions to closing right bracket.
           ;; Catch unbalanced expression errors and return nil.
           (first-end (condition-case nil
                          (and (goto-char first-begin)
                               (scan-sexps (point) 1))
                        (error nil)))
           ;; Continue with point at CONT-POINT upon failure.
           (cont-point (min (1+ first-begin) last))
           second-begin second-end url-begin url-end
           title-begin title-end)
      ;; When bracket found, in range, and followed by a left paren/bracket...
      (when (and first-end (< first-end end-of-block) (goto-char first-end)
                 (char-equal (char-after (point)) (if ref ?\[ ?\()))
        ;; Scan across balanced expressions for closing parenthesis/bracket.
        (setq second-begin (point)
              second-end (condition-case nil
                             (scan-sexps (point) 1)
                           (error nil)))
        ;; Check that closing parenthesis/bracket is in range.
        (if (and second-end (<= second-end end-of-block) (<= second-end last))
            (progn
              ;; Search for (optional) title inside closing parenthesis
              (when (and (not ref) (search-forward "\"" second-end t))
                (setq title-begin (1- (point))
                      title-end (and (goto-char second-end)
                                     (search-backward "\"" (1+ title-begin) t))
                      title-end (and title-end (1+ title-end))))
              ;; Store URL/reference range
              (setq url-begin (1+ second-begin)
                    url-end (1- (or title-begin second-end)))
              ;; Set match data, move point beyond link, and return
              (set-match-data
               (list (or bang first-begin) second-end  ; 0 - all
                     bang (and bang (1+ bang))         ; 1 - bang
                     first-begin (1+ first-begin)      ; 2 - markup
                     (1+ first-begin) (1- first-end)   ; 3 - link text
                     (1- first-end) first-end          ; 4 - markup
                     second-begin (1+ second-begin)    ; 5 - markup
                     url-begin url-end                 ; 6 - url/reference
                     title-begin title-end             ; 7 - title
                     (1- second-end) second-end))      ; 8 - markup
              ;; Nullify cont-point and leave point at end and
              (setq cont-point nil)
              (goto-char second-end))
          ;; If no closing parenthesis in range, update continuation point
          (setq cont-point (min end-of-block second-begin))))
      (cond
       ;; On failure, continue searching at cont-point
       ((and cont-point (< cont-point last))
        (goto-char cont-point)
        (markdown-match-generic-links last ref))
       ;; No more text, return nil
       ((and cont-point (= cont-point last))
        nil)
       ;; Return t if a match occurred
       (t t)))))

(defun markdown-match-angle-uris (last)
  "Match angle bracket URIs from point to LAST."
  (when (markdown-match-inline-generic markdown-regex-angle-uri last)
    (goto-char (1+ (match-end 0)))))

(defun markdown-match-plain-uris (last)
  "Match plain URIs from point to LAST."
  (when (markdown-match-inline-generic markdown-regex-uri last t)
    (goto-char (1+ (match-end 0)))))

(defvar markdown-conditional-search-function #'re-search-forward
  "Conditional search function used in `markdown-search-until-condition'.
Made into a variable to allow for dynamic let-binding.")

(defun markdown-search-until-condition (condition &rest args)
  (let (ret)
    (while (and (not ret) (apply markdown-conditional-search-function args))
      (setq ret (funcall condition)))
    ret))

(defun markdown-metadata-line-p (pos regexp)
  (save-excursion
    (or (= (line-number-at-pos pos) 1)
        (progn
          (forward-line -1)
          ;; skip multi-line metadata
          (while (and (looking-at-p "^\\s-+[[:alpha:]]")
                      (> (line-number-at-pos (point)) 1))
            (forward-line -1))
          (looking-at-p regexp)))))

(defun markdown-match-generic-metadata (regexp last)
  "Match metadata declarations specified by REGEXP from point to LAST.
These declarations must appear inside a metadata block that begins at
the beginning of the buffer and ends with a blank line (or the end of
the buffer)."
  (let* ((first (point))
         (end-re "\n[ \t]*\n\\|\n\\'\\|\\'")
         (block-begin (goto-char 1))
         (block-end (re-search-forward end-re nil t)))
    (if (and block-end (> first block-end))
        ;; Don't match declarations if there is no metadata block or if
        ;; the point is beyond the block.  Move point to point-max to
        ;; prevent additional searches and return return nil since nothing
        ;; was found.
        (progn (goto-char (point-max)) nil)
      ;; If a block was found that begins before LAST and ends after
      ;; point, search for declarations inside it.  If the starting is
      ;; before the beginning of the block, start there. Otherwise,
      ;; move back to FIRST.
      (goto-char (if (< first block-begin) block-begin first))
      (if (and (re-search-forward regexp (min last block-end) t)
               (markdown-metadata-line-p (point) regexp))
          ;; If a metadata declaration is found, set match-data and return t.
          (let ((key-beginning (match-beginning 1))
                (key-end (match-end 1))
                (markup-begin (match-beginning 2))
                (markup-end (match-end 2))
                (value-beginning (match-beginning 3)))
            (set-match-data (list key-beginning (point) ; complete metadata
                                  key-beginning key-end ; key
                                  markup-begin markup-end ; markup
                                  value-beginning (point))) ; value
            t)
        ;; Otherwise, move the point to last and return nil
        (goto-char last)
        nil))))

(defun markdown-match-declarative-metadata (last)
  "Match declarative metadata from the point to LAST."
  (markdown-match-generic-metadata markdown-regex-declarative-metadata last))

(defun markdown-match-pandoc-metadata (last)
  "Match Pandoc metadata from the point to LAST."
  (markdown-match-generic-metadata markdown-regex-pandoc-metadata last))

(defun markdown-match-yaml-metadata-begin (last)
  (markdown-match-propertized-text 'markdown-yaml-metadata-begin last))

(defun markdown-match-yaml-metadata-end (last)
  (markdown-match-propertized-text 'markdown-yaml-metadata-end last))

(defun markdown-match-yaml-metadata-key (last)
  (markdown-match-propertized-text 'markdown-metadata-key last))

(defun markdown-match-wiki-link (last)
  "Match wiki links from point to LAST."
  (when (and markdown-enable-wiki-links
             (not markdown-wiki-link-fontify-missing)
             (markdown-match-inline-generic markdown-regex-wiki-link last))
    (let ((begin (match-beginning 1)) (end (match-end 1)))
      (if (or (markdown-in-comment-p begin)
              (markdown-in-comment-p end)
              (markdown-inline-code-at-pos-p begin)
              (markdown-inline-code-at-pos-p end)
              (markdown-code-block-at-pos begin))
          (progn (goto-char (min (1+ begin) last))
                 (when (< (point) last)
                   (markdown-match-wiki-link last)))
        (set-match-data (list begin end))
        t))))

(defun markdown-match-inline-attributes (last)
  "Match inline attributes from point to LAST."
  ;; #428 re-search-forward markdown-regex-inline-attributes is very slow.
  ;; So use simple regex for re-search-forward and use markdown-regex-inline-attributes
  ;; against matched string.
  (when (markdown-match-inline-generic "[ \t]*\\({\\)\\([^\n]*\\)}[ \t]*$" last)
    (if (not (string-match-p markdown-regex-inline-attributes (match-string 0)))
        (markdown-match-inline-attributes last)
      (unless (or (markdown-inline-code-at-pos-p (match-beginning 0))
                  (markdown-inline-code-at-pos-p (match-end 0))
                  (markdown-in-comment-p))
        t))))

(defun markdown-match-leanpub-sections (last)
  "Match Leanpub section markers from point to LAST."
  (when (markdown-match-inline-generic markdown-regex-leanpub-sections last)
    (unless (or (markdown-inline-code-at-pos-p (match-beginning 0))
                (markdown-inline-code-at-pos-p (match-end 0))
                (markdown-in-comment-p))
      t)))

(defun markdown-match-includes (last)
  "Match include statements from point to LAST.
Sets match data for the following seven groups:
Group 1: opening two angle brackets
Group 2: opening title delimiter (optional)
Group 3: title text (optional)
Group 4: closing title delimiter (optional)
Group 5: opening filename delimiter
Group 6: filename
Group 7: closing filename delimiter"
  (when (markdown-match-inline-generic markdown-regex-include last)
    (let ((valid (not (or (markdown-in-comment-p (match-beginning 0))
                          (markdown-in-comment-p (match-end 0))
                          (markdown-code-block-at-pos (match-beginning 0))))))
      (cond
       ;; Parentheses and maybe square brackets, but no curly braces:
       ;; match optional title in square brackets and file in parentheses.
       ((and valid (match-beginning 5)
             (not (match-beginning 8)))
        (set-match-data (list (match-beginning 1) (match-end 7)
                              (match-beginning 1) (match-end 1)
                              (match-beginning 2) (match-end 2)
                              (match-beginning 3) (match-end 3)
                              (match-beginning 4) (match-end 4)
                              (match-beginning 5) (match-end 5)
                              (match-beginning 6) (match-end 6)
                              (match-beginning 7) (match-end 7))))
       ;; Only square brackets present: match file in square brackets.
       ((and valid (match-beginning 2)
             (not (match-beginning 5))
             (not (match-beginning 7)))
        (set-match-data (list (match-beginning 1) (match-end 4)
                              (match-beginning 1) (match-end 1)
                              nil nil
                              nil nil
                              nil nil
                              (match-beginning 2) (match-end 2)
                              (match-beginning 3) (match-end 3)
                              (match-beginning 4) (match-end 4))))
       ;; Only curly braces present: match file in curly braces.
       ((and valid (match-beginning 8)
             (not (match-beginning 2))
             (not (match-beginning 5)))
        (set-match-data (list (match-beginning 1) (match-end 10)
                              (match-beginning 1) (match-end 1)
                              nil nil
                              nil nil
                              nil nil
                              (match-beginning 8) (match-end 8)
                              (match-beginning 9) (match-end 9)
                              (match-beginning 10) (match-end 10))))
       (t
        ;; Not a valid match, move to next line and search again.
        (forward-line)
        (when (< (point) last)
          (setq valid (markdown-match-includes last)))))
      valid)))

(defun markdown-match-html-tag (last)
  "Match HTML tags from point to LAST."
  (when (and markdown-enable-html
             (markdown-match-inline-generic markdown-regex-html-tag last t))
    (set-match-data (list (match-beginning 0) (match-end 0)
                          (match-beginning 1) (match-end 1)
                          (match-beginning 2) (match-end 2)
                          (match-beginning 9) (match-end 9)))
    t))


;;; Markdown Font Fontification Functions =====================================

(defvar markdown--first-displayable-cache (make-hash-table :test #'equal))

(defun markdown--first-displayable (seq)
  "Return the first displayable character or string in SEQ.
SEQ may be an atom or a sequence."
  (let ((c (gethash seq markdown--first-displayable-cache t)))
    (if (not (eq c t))
        c
      (puthash seq
               (let ((seq (if (listp seq) seq (list seq))))
                 (cond ((stringp (car seq))
                        (cl-find-if
                         (lambda (str)
                           (and (mapcar #'char-displayable-p (string-to-list str))))
                         seq))
                       ((characterp (car seq))
                        (cl-find-if #'char-displayable-p seq))))
               markdown--first-displayable-cache))))

(defun markdown--marginalize-string (level)
  "Generate atx markup string of given LEVEL for left margin."
  (let ((margin-left-space-count
         (- markdown-marginalize-headers-margin-width level)))
    (concat (make-string margin-left-space-count ? )
            (make-string level ?#))))

(defun markdown-marginalize-update-current ()
  "Update the window configuration to create a left margin."
  (if window-system
      (let* ((header-delimiter-font-width
              (window-font-width nil 'markdown-header-delimiter-face))
             (margin-pixel-width (* markdown-marginalize-headers-margin-width
                                    header-delimiter-font-width))
             (margin-char-width (/ margin-pixel-width (default-font-width))))
        (set-window-margins nil margin-char-width))
    ;; As a fallback, simply set margin based on character count.
    (set-window-margins nil (1+ markdown-marginalize-headers-margin-width))))

(defun markdown-fontify-headings (last)
  "Add text properties to headings from point to LAST."
  (when (markdown-match-propertized-text 'markdown-heading last)
    (let* ((level (markdown-outline-level))
           (heading-face
            (intern (format "markdown-header-face-%d" level)))
           (heading-props `(face ,heading-face))
           (left-markup-props
            `(face markdown-header-delimiter-face
                   ,@(cond
                      (markdown-hide-markup
                       `(display ""))
                      (markdown-marginalize-headers
                       `(display ((margin left-margin)
                                  ,(markdown--marginalize-string level)))))))
           (right-markup-props
            `(face markdown-header-delimiter-face
                   ,@(when markdown-hide-markup `(display ""))))
           (rule-props `(face markdown-header-rule-face
                              ,@(when markdown-hide-markup `(display "")))))
      (if (match-end 1)
          ;; Setext heading
          (progn (add-text-properties
                  (match-beginning 1) (match-end 1) heading-props)
                 (if (= level 1)
                     (add-text-properties
                      (match-beginning 2) (match-end 2) rule-props)
                   (add-text-properties
                    (match-beginning 3) (match-end 3) rule-props)))
        ;; atx heading
        (let ((header-end
               (if markdown-fontify-whole-heading-line
                   (min (point-max) (1+ (match-end 0)))
                 (match-end 0))))
          (add-text-properties
           (match-beginning 4) (match-end 4) left-markup-props)

          ;; If closing tag is present
          (if (match-end 6)
              (progn
                (if markdown-hide-markup
                    (progn
                      (add-text-properties
                       (match-beginning 5) header-end heading-props)
                      (add-text-properties
                       (match-beginning 6) (match-end 6) right-markup-props))
                  (add-text-properties
                   (match-beginning 5) (match-end 5) heading-props)
                  (add-text-properties
                   (match-beginning 6) header-end right-markup-props)))
            ;; If closing tag is not present
            (add-text-properties
             (match-beginning 5) header-end heading-props))
          )))
    t))

(defun markdown-fontify-tables (last)
  (when (re-search-forward "|" last t)
    (when (markdown-table-at-point-p)
      (font-lock-append-text-property
       (line-beginning-position) (min (1+ (line-end-position)) (point-max))
       'face 'markdown-table-face))
    (forward-line 1)
    t))

(defun markdown-fontify-blockquotes (last)
  "Apply font-lock properties to blockquotes from point to LAST."
  (when (markdown-match-blockquotes last)
    (let ((display-string
           (markdown--first-displayable markdown-blockquote-display-char)))
      (add-text-properties
       (match-beginning 1) (match-end 1)
       (if markdown-hide-markup
           `(face markdown-blockquote-face display ,display-string)
         `(face markdown-markup-face)))
      (font-lock-append-text-property
       (match-beginning 0) (match-end 0) 'face 'markdown-blockquote-face)
      t)))

(defun markdown-fontify-list-items (last)
  "Apply font-lock properties to list markers from point to LAST."
  (when (markdown-match-list-items last)
    (when (not (markdown-code-block-at-point-p (match-beginning 2)))
      (let* ((indent (length (match-string-no-properties 1)))
             (level (/ indent markdown-list-indent-width)) ;; level = 0, 1, 2, ...
             (bullet (nth (mod level (length markdown-list-item-bullets))
                          markdown-list-item-bullets)))
        (add-text-properties
         (match-beginning 2) (match-end 2) '(face markdown-list-face))
        (when markdown-hide-markup
          (cond
           ;; Unordered lists
           ((string-match-p "[\\*\\+-]" (match-string 2))
            (add-text-properties
             (match-beginning 2) (match-end 2) `(display ,bullet)))
           ;; Definition lists
           ((string-equal ":" (match-string 2))
            (let ((display-string
                   (char-to-string (markdown--first-displayable
                                    markdown-definition-display-char))))
              (add-text-properties (match-beginning 2) (match-end 2)
                                   `(display ,display-string))))))))
    t))

(defun markdown--fontify-hrs-view-mode (hr-char)
  (if (and hr-char (display-supports-face-attributes-p '(:extend t)))
      (add-text-properties
       (match-beginning 0) (match-end 0)
       `(face
         (:inherit markdown-hr-face :underline t :extend t)
         font-lock-multiline t
         display "\n"))
    (let ((hr-len (and hr-char (/ (1- (window-body-width)) (char-width hr-char)))))
      (add-text-properties
       (match-beginning 0) (match-end 0)
       `(face
         markdown-hr-face font-lock-multiline t
         display ,(make-string hr-len hr-char))))))

(defun markdown-fontify-hrs (last)
  "Add text properties to horizontal rules from point to LAST."
  (when (markdown-match-hr last)
    (let ((hr-char (markdown--first-displayable markdown-hr-display-char)))
      (if (and markdown-hide-markup hr-char)
          (markdown--fontify-hrs-view-mode hr-char)
        (add-text-properties
         (match-beginning 0) (match-end 0)
         `(face markdown-hr-face font-lock-multiline t)))
      t)))

(defun markdown-fontify-sub-superscripts (last)
  "Apply text properties to sub- and superscripts from point to LAST."
  (when (markdown-search-until-condition
         (lambda () (and (not (markdown-code-block-at-point-p))
                         (not (markdown-inline-code-at-point-p))
                         (not (markdown-in-comment-p))
                         (not (markdown--math-block-p))))
         markdown-regex-sub-superscript last t)
    (let* ((subscript-p (string= (match-string 2) "~"))
           (props
            (if subscript-p
                (car markdown-sub-superscript-display)
              (cdr markdown-sub-superscript-display)))
           (mp (list 'face 'markdown-markup-face
                     'invisible 'markdown-markup)))
      (when markdown-hide-markup
        (put-text-property (match-beginning 3) (match-end 3)
                           'display props))
      (add-text-properties (match-beginning 2) (match-end 2) mp)
      (add-text-properties (match-beginning 4) (match-end 4) mp)
      t)))


;;; Syntax Table ==============================================================

(defvar markdown-mode-syntax-table
  (let ((tab (make-syntax-table text-mode-syntax-table)))
    (modify-syntax-entry ?\" "." tab)
    tab)
  "Syntax table for `markdown-mode'.")


;;; Element Insertion =========================================================

(defun markdown-ensure-blank-line-before ()
  "If previous line is not already blank, insert a blank line before point."
  (unless (bolp) (insert "\n"))
  (unless (or (bobp) (looking-back "\n\\s-*\n" nil)) (insert "\n")))

(defun markdown-ensure-blank-line-after ()
  "If following line is not already blank, insert a blank line after point.
Return the point where it was originally."
  (save-excursion
    (unless (eolp) (insert "\n"))
    (unless (or (eobp) (looking-at-p "\n\\s-*\n")) (insert "\n"))))

(defun markdown-wrap-or-insert (s1 s2 &optional thing beg end)
  "Insert the strings S1 and S2, wrapping around region or THING.
If a region is specified by the optional BEG and END arguments,
wrap the strings S1 and S2 around that region.
If there is an active region, wrap the strings S1 and S2 around
the region.  If there is not an active region but the point is at
THING, wrap that thing (which defaults to word).  Otherwise, just
insert S1 and S2 and place the point in between.  Return the
bounds of the entire wrapped string, or nil if nothing was wrapped
and S1 and S2 were only inserted."
  (let (a b bounds new-point)
    (cond
     ;; Given region
     ((and beg end)
      (setq a beg
            b end
            new-point (+ (point) (length s1))))
     ;; Active region
     ((use-region-p)
      (setq a (region-beginning)
            b (region-end)
            new-point (+ (point) (length s1))))
     ;; Thing (word) at point
     ((setq bounds (markdown-bounds-of-thing-at-point (or thing 'word)))
      (setq a (car bounds)
            b (cdr bounds)
            new-point (+ (point) (length s1))))
     ;; No active region and no word
     (t
      (setq a (point)
            b (point))))
    (goto-char b)
    (insert s2)
    (goto-char a)
    (insert s1)
    (when new-point (goto-char new-point))
    (if (= a b)
        nil
      (setq b (+ b (length s1) (length s2)))
      (cons a b))))

(defun markdown-point-after-unwrap (cur prefix suffix)
  "Return desired position of point after an unwrapping operation.
CUR gives the position of the point before the operation.
Additionally, two cons cells must be provided.  PREFIX gives the
bounds of the prefix string and SUFFIX gives the bounds of the
suffix string."
  (cond ((< cur (cdr prefix)) (car prefix))
        ((< cur (car suffix)) (- cur (- (cdr prefix) (car prefix))))
        ((<= cur (cdr suffix))
         (- cur (+ (- (cdr prefix) (car prefix))
                   (- cur (car suffix)))))
        (t cur)))

(defun markdown-unwrap-thing-at-point (regexp all text)
  "Remove prefix and suffix of thing at point and reposition the point.
When the thing at point matches REGEXP, replace the subexpression
ALL with the string in subexpression TEXT.  Reposition the point
in an appropriate location accounting for the removal of prefix
and suffix strings.  Return new bounds of string from group TEXT.
When REGEXP is nil, assumes match data is already set."
  (when (or (null regexp)
            (thing-at-point-looking-at regexp))
    (let ((cur (point))
          (prefix (cons (match-beginning all) (match-beginning text)))
          (suffix (cons (match-end text) (match-end all)))
          (bounds (cons (match-beginning text) (match-end text))))
      ;; Replace the thing at point
      (replace-match (match-string text) t t nil all)
      ;; Reposition the point
      (goto-char (markdown-point-after-unwrap cur prefix suffix))
      ;; Adjust bounds
      (setq bounds (cons (car prefix)
                         (- (cdr bounds) (- (cdr prefix) (car prefix))))))))

(defun markdown-unwrap-things-in-region (beg end regexp all text)
  "Remove prefix and suffix of all things in region from BEG to END.
When a thing in the region matches REGEXP, replace the
subexpression ALL with the string in subexpression TEXT.
Return a cons cell containing updated bounds for the region."
  (save-excursion
    (goto-char beg)
    (let ((removed 0) len-all len-text)
      (while (re-search-forward regexp (- end removed) t)
        (setq len-all (length (match-string-no-properties all)))
        (setq len-text (length (match-string-no-properties text)))
        (setq removed (+ removed (- len-all len-text)))
        (replace-match (match-string text) t t nil all))
      (cons beg (- end removed)))))

(defun markdown-insert-hr (arg)
  "Insert or replace a horizontal rule.
By default, use the first element of `markdown-hr-strings'.  When
ARG is non-nil, as when given a prefix, select a different
element as follows.  When prefixed with \\[universal-argument],
use the last element of `markdown-hr-strings' instead.  When
prefixed with an integer from 1 to the length of
`markdown-hr-strings', use the element in that position instead."
  (interactive "*P")
  (when (thing-at-point-looking-at markdown-regex-hr)
    (delete-region (match-beginning 0) (match-end 0)))
  (markdown-ensure-blank-line-before)
  (cond ((equal arg '(4))
         (insert (car (reverse markdown-hr-strings))))
        ((and (integerp arg) (> arg 0)
              (<= arg (length markdown-hr-strings)))
         (insert (nth (1- arg) markdown-hr-strings)))
        (t
         (insert (car markdown-hr-strings))))
  (markdown-ensure-blank-line-after))

(defun markdown--insert-common (start-delim end-delim regex start-group end-group face
                                            &optional skip-space)
  (if (use-region-p)
      ;; Active region
      (let* ((bounds (markdown-unwrap-things-in-region
                      (region-beginning) (region-end)
                      regex start-group end-group))
             (beg (car bounds))
             (end (cdr bounds)))
        (when (and beg skip-space)
          (save-excursion
            (goto-char beg)
            (skip-chars-forward "[ \t]")
            (setq beg (point))))
        (when (and end skip-space)
          (save-excursion
            (goto-char end)
            (skip-chars-backward "[ \t]")
            (setq end (point))))
        (markdown-wrap-or-insert start-delim end-delim nil beg end))
    (if (markdown--face-p (point) (list face))
        (save-excursion
          (while (and (markdown--face-p (point) (list face)) (not (bobp)))
            (forward-char -1))
          (forward-char (- (1- (length start-delim)))) ;; for delimiter
          (unless (bolp)
            (forward-char -1))
          (when (looking-at regex)
            (markdown-unwrap-thing-at-point nil start-group end-group)))
      (if (thing-at-point-looking-at regex)
          (markdown-unwrap-thing-at-point nil start-group end-group)
        (markdown-wrap-or-insert start-delim end-delim 'word nil nil)))))

(defun markdown-insert-bold ()
  "Insert markup to make a region or word bold.
If there is an active region, make the region bold.  If the point
is at a non-bold word, make the word bold.  If the point is at a
bold word or phrase, remove the bold markup.  Otherwise, simply
insert bold delimiters and place the point in between them."
  (interactive)
  (let ((delim (if markdown-bold-underscore "__" "**")))
    (markdown--insert-common delim delim markdown-regex-bold 2 4 'markdown-bold-face t)))

(defun markdown-insert-italic ()
  "Insert markup to make a region or word italic.
If there is an active region, make the region italic.  If the point
is at a non-italic word, make the word italic.  If the point is at an
italic word or phrase, remove the italic markup.  Otherwise, simply
insert italic delimiters and place the point in between them."
  (interactive)
  (let ((delim (if markdown-italic-underscore "_" "*")))
    (markdown--insert-common delim delim markdown-regex-italic 1 3 'markdown-italic-face t)))

(defun markdown-insert-strike-through ()
  "Insert markup to make a region or word strikethrough.
If there is an active region, make the region strikethrough.  If the point
is at a non-bold word, make the word strikethrough.  If the point is at a
strikethrough word or phrase, remove the strikethrough markup.  Otherwise,
simply insert bold delimiters and place the point in between them."
  (interactive)
  (markdown--insert-common
   "~~" "~~" markdown-regex-strike-through 2 4 'markdown-strike-through-face t))

(defun markdown-insert-code ()
  "Insert markup to make a region or word an inline code fragment.
If there is an active region, make the region an inline code
fragment.  If the point is at a word, make the word an inline
code fragment.  Otherwise, simply insert code delimiters and
place the point in between them."
  (interactive)
  (if (use-region-p)
      ;; Active region
      (let ((bounds (markdown-unwrap-things-in-region
                     (region-beginning) (region-end)
                     markdown-regex-code 1 3)))
        (markdown-wrap-or-insert "`" "`" nil (car bounds) (cdr bounds)))
    ;; Code markup removal, code markup for word, or empty markup insertion
    (if (markdown-inline-code-at-point)
        (markdown-unwrap-thing-at-point nil 0 2)
      (markdown-wrap-or-insert "`" "`" 'word nil nil))))

(defun markdown-insert-kbd ()
  "Insert markup to wrap region or word in <kbd> tags.
If there is an active region, use the region.  If the point is at
a word, use the word.  Otherwise, simply insert <kbd> tags and
place the point in between them."
  (interactive)
  (if (use-region-p)
      ;; Active region
      (let ((bounds (markdown-unwrap-things-in-region
                     (region-beginning) (region-end)
                     markdown-regex-kbd 0 2)))
        (markdown-wrap-or-insert "<kbd>" "</kbd>" nil (car bounds) (cdr bounds)))
    ;; Markup removal, markup for word, or empty markup insertion
    (if (thing-at-point-looking-at markdown-regex-kbd)
        (markdown-unwrap-thing-at-point nil 0 2)
      (markdown-wrap-or-insert "<kbd>" "</kbd>" 'word nil nil))))

(defun markdown-insert-inline-link (text url &optional title)
  "Insert an inline link with TEXT pointing to URL.
Optionally, the user can provide a TITLE."
  (let ((cur (point)))
    (setq title (and title (concat " \"" title "\"")))
    (insert (concat "[" text "](" url title ")"))
    (cond ((not text) (goto-char (+ 1 cur)))
          ((not url) (goto-char (+ 3 (length text) cur))))))

(defun markdown-insert-inline-image (text url &optional title)
  "Insert an inline link with alt TEXT pointing to URL.
Optionally, also provide a TITLE."
  (let ((cur (point)))
    (setq title (and title (concat " \"" title "\"")))
    (insert (concat "![" text "](" url title ")"))
    (cond ((not text) (goto-char (+ 2 cur)))
          ((not url) (goto-char (+ 4 (length text) cur))))))

(defun markdown-insert-reference-link (text label &optional url title)
  "Insert a reference link and, optionally, a reference definition.
The link TEXT will be inserted followed by the optional LABEL.
If a URL is given, also insert a definition for the reference
LABEL according to `markdown-reference-location'.  If a TITLE is
given, it will be added to the end of the reference definition
and will be used to populate the title attribute when converted
to XHTML.  If URL is nil, insert only the link portion (for
example, when a reference label is already defined)."
  (insert (concat "[" text "][" label "]"))
  (when url
    (markdown-insert-reference-definition
     (if (string-equal label "") text label)
     url title)))

(defun markdown-insert-reference-image (text label &optional url title)
  "Insert a reference image and, optionally, a reference definition.
The alt TEXT will be inserted followed by the optional LABEL.
If a URL is given, also insert a definition for the reference
LABEL according to `markdown-reference-location'.  If a TITLE is
given, it will be added to the end of the reference definition
and will be used to populate the title attribute when converted
to XHTML.  If URL is nil, insert only the link portion (for
example, when a reference label is already defined)."
  (insert (concat "![" text "][" label "]"))
  (when url
    (markdown-insert-reference-definition
     (if (string-equal label "") text label)
     url title)))

(defun markdown-insert-reference-definition (label &optional url title)
  "Add definition for reference LABEL with URL and TITLE.
LABEL is a Markdown reference label without square brackets.
URL and TITLE are optional.  When given, the TITLE will
be used to populate the title attribute when converted to XHTML."
  ;; END specifies where to leave the point upon return
  (let ((end (point)))
    (cl-case markdown-reference-location
      (end         (goto-char (point-max)))
      (immediately (markdown-end-of-text-block))
      (subtree     (markdown-end-of-subtree))
      (header      (markdown-end-of-defun)))
    ;; Skip backwards over local variables.  This logic is similar to the one
    ;; used in ‘hack-local-variables’.
    (when (and enable-local-variables (eobp))
      (search-backward "\n\f" (max (- (point) 3000) (point-min)) :move)
      (when (let ((case-fold-search t))
              (search-forward "Local Variables:" nil :move))
        (beginning-of-line 0)
        (when (eq (char-before) ?\n) (backward-char))))
    (unless (or (markdown-cur-line-blank-p)
                (thing-at-point-looking-at markdown-regex-reference-definition))
      (insert "\n"))
    (insert "\n[" label "]: ")
    (if url
        (insert url)
      ;; When no URL is given, leave point at END following the colon
      (setq end (point)))
    (when (> (length title) 0)
      (insert " \"" title "\""))
    (unless (looking-at-p "\n")
      (insert "\n"))
    (goto-char end)
    (when url
      (message
       (markdown--substitute-command-keys
        "Reference [%s] was defined, press \\[markdown-do] to jump there")
       label))))

(defcustom markdown-link-make-text-function nil
  "Function that automatically generates a link text for a URL.

If non-nil, this function will be called by
`markdown--insert-link-or-image' and the result will be the
default link text. The function should receive exactly one
argument that corresponds to the link URL."
  :group 'markdown
  :type 'function
  :package-version '(markdown-mode . "2.5"))

(defcustom markdown-disable-tooltip-prompt nil
  "Disable prompt for tooltip when inserting a link or image.

If non-nil, `markdown-insert-link' and `markdown-insert-link'
will not prompt the user to insert a tooltip text for the given
link or image."
  :group 'markdown
  :type 'boolean
  :safe 'booleanp
  :package-version '(markdown-mode . "2.5"))

(defun markdown--insert-link-or-image (image)
  "Interactively insert new or update an existing link or image.
When IMAGE is non-nil, insert an image.  Otherwise, insert a link.
This is an internal function called by
`markdown-insert-link' and `markdown-insert-image'."
  (cl-multiple-value-bind (begin end text uri ref title)
      (if (use-region-p)
          ;; Use region as either link text or URL as appropriate.
          (let ((region (buffer-substring-no-properties
                         (region-beginning) (region-end))))
            (if (string-match markdown-regex-uri region)
                ;; Region contains a URL; use it as such.
                (list (region-beginning) (region-end)
                      nil (match-string 0 region) nil nil)
              ;; Region doesn't contain a URL, so use it as text.
              (list (region-beginning) (region-end)
                    region nil nil nil)))
        ;; Extract and use properties of existing link, if any.
        (markdown-link-at-pos (point)))
    (let* ((ref (when ref (concat "[" ref "]")))
           (defined-refs (mapcar #'car (markdown-get-defined-references)))
           (defined-ref-cands (mapcar (lambda (ref) (concat "[" ref "]")) defined-refs))
           (used-uris (markdown-get-used-uris))
           (uri-or-ref (completing-read
                        "URL or [reference]: "
                        (append defined-ref-cands used-uris)
                        nil nil (or uri ref)))
           (ref (cond ((string-match "\\`\\[\\(.*\\)\\]\\'" uri-or-ref)
                       (match-string 1 uri-or-ref))
                      ((string-equal "" uri-or-ref)
                       "")))
           (uri (unless ref uri-or-ref))
           (text-prompt (if image
                            "Alt text: "
                          (if ref
                              "Link text: "
                            "Link text (blank for plain URL): ")))
           (text (or text (and markdown-link-make-text-function uri
                               (funcall markdown-link-make-text-function uri))))
           (text (completing-read text-prompt defined-refs nil nil text))
           (text (if (= (length text) 0) nil text))
           (plainp (and uri (not text)))
           (implicitp (string-equal ref ""))
           (ref (if implicitp text ref))
           (definedp (and ref (markdown-reference-definition ref)))
           (ref-url (unless (or uri definedp)
                      (completing-read "Reference URL: " used-uris)))
           (title (unless (or plainp definedp markdown-disable-tooltip-prompt)
                    (read-string "Title (tooltip text, optional): " title)))
           (title (if (= (length title) 0) nil title)))
      (when (and image implicitp)
        (user-error "Reference required: implicit image references are invalid"))
      (when (and begin end)
        (delete-region begin end))
      (cond
       ((and (not image) uri text)
        (markdown-insert-inline-link text uri title))
       ((and image uri text)
        (markdown-insert-inline-image text uri title))
       ((and ref text)
        (if image
            (markdown-insert-reference-image text (unless implicitp ref) nil title)
          (markdown-insert-reference-link text (unless implicitp ref) nil title))
        (unless definedp
          (markdown-insert-reference-definition ref ref-url title)))
       ((and (not image) uri)
        (markdown-insert-uri uri))))))

(defun markdown-insert-link ()
  "Insert new or update an existing link, with interactive prompt.
If the point is at an existing link or URL, update the link text,
URL, reference label, and/or title.  Otherwise, insert a new link.
The type of link inserted (inline, reference, or plain URL)
depends on which values are provided:

*   If a URL and TEXT are given, insert an inline link: [TEXT](URL).
*   If [REF] and TEXT are given, insert a reference link: [TEXT][REF].
*   If only TEXT is given, insert an implicit reference link: [TEXT][].
*   If only a URL is given, insert a plain link: <URL>.

In other words, to create an implicit reference link, leave the
URL prompt empty and to create a plain URL link, leave the link
text empty.

If there is an active region, use the text as the default URL, if
it seems to be a URL, or link text value otherwise.

If a given reference is not defined, this function will
additionally prompt for the URL and optional title.  In this case,
the reference definition is placed at the location determined by
`markdown-reference-location'.  In addition, it is possible to
have the `markdown-link-make-text-function' function, if non-nil,
define the default link text before prompting the user for it.

If `markdown-disable-tooltip-prompt' is non-nil, the user will
not be prompted to add or modify a tooltip text.

Through updating the link, this function can be used to convert a
link of one type (inline, reference, or plain) to another type by
selectively adding or removing information via the prompts."
  (interactive)
  (markdown--insert-link-or-image nil))

(defun markdown-insert-image ()
  "Insert new or update an existing image, with interactive prompt.
If the point is at an existing image, update the alt text, URL,
reference label, and/or title. Otherwise, insert a new image.
The type of image inserted (inline or reference) depends on which
values are provided:

*   If a URL and ALT-TEXT are given, insert an inline image:
    ![ALT-TEXT](URL).
*   If [REF] and ALT-TEXT are given, insert a reference image:
    ![ALT-TEXT][REF].

If there is an active region, use the text as the default URL, if
it seems to be a URL, or alt text value otherwise.

If a given reference is not defined, this function will
additionally prompt for the URL and optional title.  In this case,
the reference definition is placed at the location determined by
`markdown-reference-location'.

Through updating the image, this function can be used to convert an
image of one type (inline or reference) to another type by
selectively adding or removing information via the prompts."
  (interactive)
  (markdown--insert-link-or-image t))

(defun markdown-insert-uri (&optional uri)
  "Insert markup for an inline URI.
If there is an active region, use it as the URI.  If the point is
at a URI, wrap it with angle brackets.  If the point is at an
inline URI, remove the angle brackets.  Otherwise, simply insert
angle brackets place the point between them."
  (interactive)
  (if (use-region-p)
      ;; Active region
      (let ((bounds (markdown-unwrap-things-in-region
                     (region-beginning) (region-end)
                     markdown-regex-angle-uri 0 2)))
        (markdown-wrap-or-insert "<" ">" nil (car bounds) (cdr bounds)))
    ;; Markup removal, URI at point, new URI, or empty markup insertion
    (if (thing-at-point-looking-at markdown-regex-angle-uri)
        (markdown-unwrap-thing-at-point nil 0 2)
      (if uri
          (insert "<" uri ">")
        (markdown-wrap-or-insert "<" ">" 'url nil nil)))))

(defun markdown-insert-wiki-link ()
  "Insert a wiki link of the form [[WikiLink]].
If there is an active region, use the region as the link text.
If the point is at a word, use the word as the link text.  If
there is no active region and the point is not at word, simply
insert link markup."
  (interactive)
  (if (use-region-p)
      ;; Active region
      (markdown-wrap-or-insert "[[" "]]" nil (region-beginning) (region-end))
    ;; Markup removal, wiki link at at point, or empty markup insertion
    (if (thing-at-point-looking-at markdown-regex-wiki-link)
        (if (or markdown-wiki-link-alias-first
                (null (match-string 5)))
            (markdown-unwrap-thing-at-point nil 1 3)
          (markdown-unwrap-thing-at-point nil 1 5))
      (markdown-wrap-or-insert "[[" "]]"))))

(defun markdown-remove-header ()
  "Remove header markup if point is at a header.
Return bounds of remaining header text if a header was removed
and nil otherwise."
  (interactive "*")
  (or (markdown-unwrap-thing-at-point markdown-regex-header-atx 0 2)
      (markdown-unwrap-thing-at-point markdown-regex-header-setext 0 1)))

(defun markdown-insert-header (&optional level text setext)
  "Insert or replace header markup.
The level of the header is specified by LEVEL and header text is
given by TEXT.  LEVEL must be an integer from 1 and 6, and the
default value is 1.
When TEXT is nil, the header text is obtained as follows.
If there is an active region, it is used as the header text.
Otherwise, the current line will be used as the header text.
If there is not an active region and the point is at a header,
remove the header markup and replace with level N header.
Otherwise, insert empty header markup and place the point in
between.
The style of the header will be atx (hash marks) unless
SETEXT is non-nil, in which case a setext-style (underlined)
header will be inserted."
  (interactive "p\nsHeader text: ")
  (setq level (min (max (or level 1) 1) (if setext 2 6)))
  ;; Determine header text if not given
  (when (null text)
    (if (use-region-p)
        ;; Active region
        (setq text (delete-and-extract-region (region-beginning) (region-end)))
      ;; No active region
      (markdown-remove-header)
      (setq text (delete-and-extract-region
                  (line-beginning-position) (line-end-position)))
      (when (and setext (string-match-p "^[ \t]*$" text))
        (setq text (read-string "Header text: "))))
    (setq text (markdown-compress-whitespace-string text)))
  ;; Insertion with given text
  (markdown-ensure-blank-line-before)
  (let (hdr)
    (cond (setext
           (setq hdr (make-string (string-width text) (if (= level 2) ?- ?=)))
           (insert text "\n" hdr))
          (t
           (setq hdr (make-string level ?#))
           (insert hdr " " text)
           (when (null markdown-asymmetric-header) (insert " " hdr)))))
  (markdown-ensure-blank-line-after)
  ;; Leave point at end of text
  (cond (setext
         (backward-char (1+ (string-width text))))
        ((null markdown-asymmetric-header)
         (backward-char (1+ level)))))

(defun markdown-insert-header-dwim (&optional arg setext)
  "Insert or replace header markup.
The level and type of the header are determined automatically by
the type and level of the previous header, unless a prefix
argument is given via ARG.
With a numeric prefix valued 1 to 6, insert a header of the given
level, with the type being determined automatically (note that
only level 1 or 2 setext headers are possible).

With a \\[universal-argument] prefix (i.e., when ARG is (4)),
promote the heading by one level.
With two \\[universal-argument] prefixes (i.e., when ARG is (16)),
demote the heading by one level.
When SETEXT is non-nil, prefer setext-style headers when
possible (levels one and two).

When there is an active region, use it for the header text.  When
the point is at an existing header, change the type and level
according to the rules above.
Otherwise, if the line is not empty, create a header using the
text on the current line as the header text.
Finally, if the point is on a blank line, insert empty header
markup (atx) or prompt for text (setext).
See `markdown-insert-header' for more details about how the
header text is determined."
  (interactive "*P")
  (let (level)
    (save-excursion
      (when (or (thing-at-point-looking-at markdown-regex-header)
                (re-search-backward markdown-regex-header nil t))
        ;; level of current or previous header
        (setq level (markdown-outline-level))
        ;; match group 1 indicates a setext header
        (setq setext (match-end 1))))
    ;; check prefix argument
    (cond
     ((and (equal arg '(4)) level (> level 1)) ;; C-u
      (cl-decf level))
     ((and (equal arg '(16)) level (< level 6)) ;; C-u C-u
      (cl-incf level))
     (arg ;; numeric prefix
      (setq level (prefix-numeric-value arg))))
    ;; setext headers must be level one or two
    (and level (setq setext (and setext (<= level 2))))
    ;; insert the heading
    (markdown-insert-header level nil setext)))

(defun markdown-insert-header-setext-dwim (&optional arg)
  "Insert or replace header markup, with preference for setext.
See `markdown-insert-header-dwim' for details, including how ARG is handled."
  (interactive "*P")
  (markdown-insert-header-dwim arg t))

(defun markdown-insert-header-atx-1 ()
  "Insert a first level atx-style (hash mark) header.
See `markdown-insert-header'."
  (interactive "*")
  (markdown-insert-header 1 nil nil))

(defun markdown-insert-header-atx-2 ()
  "Insert a level two atx-style (hash mark) header.
See `markdown-insert-header'."
  (interactive "*")
  (markdown-insert-header 2 nil nil))

(defun markdown-insert-header-atx-3 ()
  "Insert a level three atx-style (hash mark) header.
See `markdown-insert-header'."
  (interactive "*")
  (markdown-insert-header 3 nil nil))

(defun markdown-insert-header-atx-4 ()
  "Insert a level four atx-style (hash mark) header.
See `markdown-insert-header'."
  (interactive "*")
  (markdown-insert-header 4 nil nil))

(defun markdown-insert-header-atx-5 ()
  "Insert a level five atx-style (hash mark) header.
See `markdown-insert-header'."
  (interactive "*")
  (markdown-insert-header 5 nil nil))

(defun markdown-insert-header-atx-6 ()
  "Insert a sixth level atx-style (hash mark) header.
See `markdown-insert-header'."
  (interactive "*")
  (markdown-insert-header 6 nil nil))

(defun markdown-insert-header-setext-1 ()
  "Insert a setext-style (underlined) first-level header.
See `markdown-insert-header'."
  (interactive "*")
  (markdown-insert-header 1 nil t))

(defun markdown-insert-header-setext-2 ()
  "Insert a setext-style (underlined) second-level header.
See `markdown-insert-header'."
  (interactive "*")
  (markdown-insert-header 2 nil t))

(defun markdown-blockquote-indentation (loc)
  "Return string containing necessary indentation for a blockquote at LOC.
Also see `markdown-pre-indentation'."
  (save-excursion
    (goto-char loc)
    (let* ((list-level (length (markdown-calculate-list-levels)))
           (indent ""))
      (dotimes (_ list-level indent)
        (setq indent (concat indent "    "))))))

(defun markdown-insert-blockquote ()
  "Start a blockquote section (or blockquote the region).
If Transient Mark mode is on and a region is active, it is used as
the blockquote text."
  (interactive)
  (if (use-region-p)
      (markdown-blockquote-region (region-beginning) (region-end))
    (markdown-ensure-blank-line-before)
    (insert (markdown-blockquote-indentation (point)) "> ")
    (markdown-ensure-blank-line-after)))

(defun markdown-block-region (beg end prefix)
  "Format the region using a block prefix.
Arguments BEG and END specify the beginning and end of the
region.  The characters PREFIX will appear at the beginning
of each line."
  (save-excursion
    (let* ((end-marker (make-marker))
           (beg-marker (make-marker))
           (prefix-without-trailing-whitespace
            (replace-regexp-in-string (rx (+ blank) eos) "" prefix)))
      ;; Ensure blank line after and remove extra whitespace
      (goto-char end)
      (skip-syntax-backward "-")
      (set-marker end-marker (point))
      (delete-horizontal-space)
      (markdown-ensure-blank-line-after)
      ;; Ensure blank line before and remove extra whitespace
      (goto-char beg)
      (skip-syntax-forward "-")
      (delete-horizontal-space)
      (markdown-ensure-blank-line-before)
      (set-marker beg-marker (point))
      ;; Insert PREFIX before each line
      (goto-char beg-marker)
      (while (and (< (line-beginning-position) end-marker)
                  (not (eobp)))
        ;; Don’t insert trailing whitespace.
        (insert (if (eolp) prefix-without-trailing-whitespace prefix))
        (forward-line)))))

(defun markdown-blockquote-region (beg end)
  "Blockquote the region.
Arguments BEG and END specify the beginning and end of the region."
  (interactive "*r")
  (markdown-block-region
   beg end (concat (markdown-blockquote-indentation
                    (max (point-min) (1- beg))) "> ")))

(defun markdown-pre-indentation (loc)
  "Return string containing necessary whitespace for a pre block at LOC.
Also see `markdown-blockquote-indentation'."
  (save-excursion
    (goto-char loc)
    (let* ((list-level (length (markdown-calculate-list-levels)))
           indent)
      (dotimes (_ (1+ list-level) indent)
        (setq indent (concat indent "    "))))))

(defun markdown-insert-pre ()
  "Start a preformatted section (or apply to the region).
If Transient Mark mode is on and a region is active, it is marked
as preformatted text."
  (interactive)
  (if (use-region-p)
      (markdown-pre-region (region-beginning) (region-end))
    (markdown-ensure-blank-line-before)
    (insert (markdown-pre-indentation (point)))
    (markdown-ensure-blank-line-after)))

(defun markdown-pre-region (beg end)
  "Format the region as preformatted text.
Arguments BEG and END specify the beginning and end of the region."
  (interactive "*r")
  (let ((indent (markdown-pre-indentation (max (point-min) (1- beg)))))
    (markdown-block-region beg end indent)))

(defun markdown-electric-backquote (arg)
  "Insert a backquote.
The numeric prefix argument ARG says how many times to repeat the insertion.
Call `markdown-insert-gfm-code-block' interactively
if three backquotes inserted at the beginning of line."
  (interactive "*P")
  (self-insert-command (prefix-numeric-value arg))
  (when (and markdown-gfm-use-electric-backquote (looking-back "^```" nil))
    (replace-match "")
    (call-interactively #'markdown-insert-gfm-code-block)))

(defconst markdown-gfm-recognized-languages
  ;; To reproduce/update, evaluate the let-form in
  ;; scripts/get-recognized-gfm-languages.el. that produces a single long sexp,
  ;; but with appropriate use of a keyboard macro, indenting and filling it
  ;; properly is pretty fast.
  '("1C-Enterprise" "4D" "ABAP" "ABNF" "AGS-Script" "AMPL" "ANTLR"
    "API-Blueprint" "APL" "ASN.1" "ASP" "ATS" "ActionScript" "Ada"
    "Adobe-Font-Metrics" "Agda" "Alloy" "Alpine-Abuild" "Altium-Designer"
    "AngelScript" "Ant-Build-System" "ApacheConf" "Apex"
    "Apollo-Guidance-Computer" "AppleScript" "Arc" "AsciiDoc" "AspectJ" "Assembly"
    "Asymptote" "Augeas" "AutoHotkey" "AutoIt" "Awk" "Ballerina" "Batchfile"
    "Befunge" "BibTeX" "Bison" "BitBake" "Blade" "BlitzBasic" "BlitzMax"
    "Bluespec" "Boo" "Brainfuck" "Brightscript" "C#" "C++" "C-ObjDump"
    "C2hs-Haskell" "CLIPS" "CMake" "COBOL" "COLLADA" "CSON" "CSS" "CSV" "CWeb"
    "Cabal-Config" "Cap'n-Proto" "CartoCSS" "Ceylon" "Chapel" "Charity" "ChucK"
    "Cirru" "Clarion" "Clean" "Click" "Clojure" "Closure-Templates"
    "Cloud-Firestore-Security-Rules" "CoNLL-U" "CodeQL" "CoffeeScript"
    "ColdFusion" "ColdFusion-CFC" "Common-Lisp" "Common-Workflow-Language"
    "Component-Pascal" "Cool" "Coq" "Cpp-ObjDump" "Creole" "Crystal" "Csound"
    "Csound-Document" "Csound-Score" "Cuda" "Cycript" "Cython" "D-ObjDump"
    "DIGITAL-Command-Language" "DM" "DNS-Zone" "DTrace" "Dafny" "Darcs-Patch"
    "Dart" "DataWeave" "Dhall" "Diff" "DirectX-3D-File" "Dockerfile" "Dogescript"
    "Dylan" "EBNF" "ECL" "ECLiPSe" "EJS" "EML" "EQ" "Eagle" "Easybuild"
    "Ecere-Projects" "EditorConfig" "Edje-Data-Collection" "Eiffel" "Elixir" "Elm"
    "Emacs-Lisp" "EmberScript" "Erlang" "F#" "F*" "FIGlet-Font" "FLUX" "Factor"
    "Fancy" "Fantom" "Faust" "Filebench-WML" "Filterscript" "Formatted" "Forth"
    "Fortran" "Fortran-Free-Form" "FreeMarker" "Frege" "G-code" "GAML" "GAMS"
    "GAP" "GCC-Machine-Description" "GDB" "GDScript" "GEDCOM" "GLSL" "GN"
    "Game-Maker-Language" "Genie" "Genshi" "Gentoo-Ebuild" "Gentoo-Eclass"
    "Gerber-Image" "Gettext-Catalog" "Gherkin" "Git-Attributes" "Git-Config"
    "Glyph" "Glyph-Bitmap-Distribution-Format" "Gnuplot" "Go" "Golo" "Gosu"
    "Grace" "Gradle" "Grammatical-Framework" "Graph-Modeling-Language" "GraphQL"
    "Graphviz-(DOT)" "Groovy" "Groovy-Server-Pages" "HAProxy" "HCL" "HLSL" "HTML"
    "HTML+Django" "HTML+ECR" "HTML+EEX" "HTML+ERB" "HTML+PHP" "HTML+Razor" "HTTP"
    "HXML" "Hack" "Haml" "Handlebars" "Harbour" "Haskell" "Haxe" "HiveQL" "HolyC"
    "Hy" "HyPhy" "IDL" "IGOR-Pro" "INI" "IRC-log" "Idris" "Ignore-List" "Inform-7"
    "Inno-Setup" "Io" "Ioke" "Isabelle" "Isabelle-ROOT" "JFlex" "JSON"
    "JSON-with-Comments" "JSON5" "JSONLD" "JSONiq" "JSX" "Jasmin" "Java"
    "Java-Properties" "Java-Server-Pages" "JavaScript" "JavaScript+ERB" "Jison"
    "Jison-Lex" "Jolie" "Jsonnet" "Julia" "Jupyter-Notebook" "KRL" "KiCad-Layout"
    "KiCad-Legacy-Layout" "KiCad-Schematic" "Kit" "Kotlin" "LFE" "LLVM" "LOLCODE"
    "LSL" "LTspice-Symbol" "LabVIEW" "Lasso" "Latte" "Lean" "Less" "Lex"
    "LilyPond" "Limbo" "Linker-Script" "Linux-Kernel-Module" "Liquid"
    "Literate-Agda" "Literate-CoffeeScript" "Literate-Haskell" "LiveScript"
    "Logos" "Logtalk" "LookML" "LoomScript" "Lua" "M4" "M4Sugar" "MATLAB"
    "MAXScript" "MLIR" "MQL4" "MQL5" "MTML" "MUF" "Macaulay2" "Makefile" "Mako"
    "Markdown" "Marko" "Mask" "Mathematica" "Maven-POM" "Max" "MediaWiki"
    "Mercury" "Meson" "Metal" "Microsoft-Developer-Studio-Project" "MiniD" "Mirah"
    "Modelica" "Modula-2" "Modula-3" "Module-Management-System" "Monkey" "Moocode"
    "MoonScript" "Motorola-68K-Assembly" "Muse" "Myghty" "NASL" "NCL" "NEON" "NL"
    "NPM-Config" "NSIS" "Nearley" "Nemerle" "NetLinx" "NetLinx+ERB" "NetLogo"
    "NewLisp" "Nextflow" "Nginx" "Nim" "Ninja" "Nit" "Nix" "Nu" "NumPy" "OCaml"
    "ObjDump" "Object-Data-Instance-Notation" "ObjectScript" "Objective-C"
    "Objective-C++" "Objective-J" "Odin" "Omgrofl" "Opa" "Opal"
    "Open-Policy-Agent" "OpenCL" "OpenEdge-ABL" "OpenQASM" "OpenRC-runscript"
    "OpenSCAD" "OpenStep-Property-List" "OpenType-Feature-File" "Org" "Ox"
    "Oxygene" "Oz" "P4" "PHP" "PLSQL" "PLpgSQL" "POV-Ray-SDL" "Pan" "Papyrus"
    "Parrot" "Parrot-Assembly" "Parrot-Internal-Representation" "Pascal" "Pawn"
    "Pep8" "Perl" "Pic" "Pickle" "PicoLisp" "PigLatin" "Pike" "PlantUML" "Pod"
    "Pod-6" "PogoScript" "Pony" "PostCSS" "PostScript" "PowerBuilder" "PowerShell"
    "Prisma" "Processing" "Proguard" "Prolog" "Propeller-Spin" "Protocol-Buffer"
    "Public-Key" "Pug" "Puppet" "Pure-Data" "PureBasic" "PureScript" "Python"
    "Python-console" "Python-traceback" "QML" "QMake" "Quake" "RAML" "RDoc"
    "REALbasic" "REXX" "RHTML" "RMarkdown" "RPC" "RPM-Spec" "RUNOFF" "Racket"
    "Ragel" "Raku" "Rascal" "Raw-token-data" "Readline-Config" "Reason" "Rebol"
    "Red" "Redcode" "Regular-Expression" "Ren'Py" "RenderScript"
    "Rich-Text-Format" "Ring" "Riot" "RobotFramework" "Roff" "Roff-Manpage"
    "Rouge" "Ruby" "Rust" "SAS" "SCSS" "SMT" "SPARQL" "SQF" "SQL" "SQLPL"
    "SRecode-Template" "SSH-Config" "STON" "SVG" "SWIG" "Sage" "SaltStack" "Sass"
    "Scala" "Scaml" "Scheme" "Scilab" "Self" "ShaderLab" "Shell" "ShellSession"
    "Shen" "Slash" "Slice" "Slim" "SmPL" "Smali" "Smalltalk" "Smarty" "Solidity"
    "SourcePawn" "Spline-Font-Database" "Squirrel" "Stan" "Standard-ML" "Starlark"
    "Stata" "Stylus" "SubRip-Text" "SugarSS" "SuperCollider" "Svelte" "Swift"
    "SystemVerilog" "TI-Program" "TLA" "TOML" "TSQL" "TSX" "TXL" "Tcl" "Tcsh"
    "TeX" "Tea" "Terra" "Texinfo" "Text" "Textile" "Thrift" "Turing" "Turtle"
    "Twig" "Type-Language" "TypeScript" "Unified-Parallel-C" "Unity3D-Asset"
    "Unix-Assembly" "Uno" "UnrealScript" "UrWeb" "VBA" "VBScript" "VCL" "VHDL"
    "Vala" "Verilog" "Vim-Snippet" "Vim-script" "Visual-Basic-.NET" "Volt" "Vue"
    "Wavefront-Material" "Wavefront-Object" "Web-Ontology-Language" "WebAssembly"
    "WebIDL" "WebVTT" "Wget-Config" "Windows-Registry-Entries" "Wollok"
    "World-of-Warcraft-Addon-Data" "X-BitMap" "X-Font-Directory-Index" "X-PixMap"
    "X10" "XC" "XCompose" "XML" "XML-Property-List" "XPages" "XProc" "XQuery" "XS"
    "XSLT" "Xojo" "Xtend" "YAML" "YANG" "YARA" "YASnippet" "Yacc" "ZAP" "ZIL"
    "Zeek" "ZenScript" "Zephir" "Zig" "Zimpl" "cURL-Config" "desktop" "dircolors"
    "eC" "edn" "fish" "mIRC-Script" "mcfunction" "mupad" "nanorc" "nesC" "ooc"
    "reStructuredText" "sed" "wdl" "wisp" "xBase")
  "Language specifiers recognized by GitHub's syntax highlighting features.")

(defvar-local markdown-gfm-used-languages nil
  "Language names used in GFM code blocks.")

(defun markdown-trim-whitespace (str)
  (replace-regexp-in-string
   "\\(?:[[:space:]\r\n]+\\'\\|\\`[[:space:]\r\n]+\\)" "" str))

(defun markdown-clean-language-string (str)
  (replace-regexp-in-string
   "{\\.?\\|}" "" (markdown-trim-whitespace str)))

(defun markdown-validate-language-string (widget)
  (let ((str (widget-value widget)))
    (unless (string= str (markdown-clean-language-string str))
      (widget-put widget :error (format "Invalid language spec: '%s'" str))
      widget)))

(defun markdown-gfm-get-corpus ()
  "Create corpus of recognized GFM code block languages for the given buffer."
  (let ((given-corpus (append markdown-gfm-additional-languages
                              markdown-gfm-recognized-languages)))
    (append
     markdown-gfm-used-languages
     (if markdown-gfm-downcase-languages (cl-mapcar #'downcase given-corpus)
       given-corpus))))

(defun markdown-gfm-add-used-language (lang)
  "Clean LANG and add to list of used languages."
  (setq markdown-gfm-used-languages
        (cons lang (remove lang markdown-gfm-used-languages))))

(defcustom markdown-spaces-after-code-fence 1
  "Number of space characters to insert after a code fence.
\\<gfm-mode-map>\\[markdown-insert-gfm-code-block] inserts this many spaces between an
opening code fence and an info string."
  :group 'markdown
  :type 'integer
  :safe #'natnump
  :package-version '(markdown-mode . "2.3"))

(defcustom markdown-code-block-braces nil
  "When non-nil, automatically insert braces for GFM code blocks."
  :group 'markdown
  :type 'boolean)

(defun markdown-insert-gfm-code-block (&optional lang edit)
  "Insert GFM code block for language LANG.
If LANG is nil, the language will be queried from user.  If a
region is active, wrap this region with the markup instead.  If
the region boundaries are not on empty lines, these are added
automatically in order to have the correct markup.  When EDIT is
non-nil (e.g., when \\[universal-argument] is given), edit the
code block in an indirect buffer after insertion."
  (interactive
   (list (let ((completion-ignore-case nil))
           (condition-case nil
               (markdown-clean-language-string
                (completing-read
                 "Programming language: "
                 (markdown-gfm-get-corpus)
                 nil 'confirm (car markdown-gfm-used-languages)
                 'markdown-gfm-language-history))
             (quit "")))
         current-prefix-arg))
  (unless (string= lang "") (markdown-gfm-add-used-language lang))
  (when (and (> (length lang) 0)
             (not markdown-code-block-braces))
    (setq lang (concat (make-string markdown-spaces-after-code-fence ?\s)
                       lang)))
  (let ((gfm-open-brace (if markdown-code-block-braces "{" ""))
        (gfm-close-brace (if markdown-code-block-braces "}" "")))
    (if (use-region-p)
        (let* ((b (region-beginning)) (e (region-end)) end
               (indent (progn (goto-char b) (current-indentation))))
          (goto-char e)
          ;; if we're on a blank line, don't newline, otherwise the ```
          ;; should go on its own line
          (unless (looking-back "\n" nil)
            (newline))
          (indent-to indent)
          (insert "```")
          (markdown-ensure-blank-line-after)
          (setq end (point))
          (goto-char b)
          ;; if we're on a blank line, insert the quotes here, otherwise
          ;; add a new line first
          (unless (looking-at-p "\n")
            (newline)
            (forward-line -1))
          (markdown-ensure-blank-line-before)
          (indent-to indent)
          (insert "```" gfm-open-brace lang gfm-close-brace)
          (markdown-syntax-propertize-fenced-block-constructs (line-beginning-position) end))
      (let ((indent (current-indentation))
            start-bol)
        (delete-horizontal-space :backward-only)
        (markdown-ensure-blank-line-before)
        (indent-to indent)
        (setq start-bol (line-beginning-position))
        (insert "```" gfm-open-brace lang gfm-close-brace "\n")
        (indent-to indent)
        (unless edit (insert ?\n))
        (indent-to indent)
        (insert "```")
        (markdown-ensure-blank-line-after)
        (markdown-syntax-propertize-fenced-block-constructs start-bol (point)))
      (end-of-line 0)
      (when edit (markdown-edit-code-block)))))

(defun markdown-code-block-lang (&optional pos-prop)
  "Return the language name for a GFM or tilde fenced code block.
The beginning of the block may be described by POS-PROP,
a cons of (pos . prop) giving the position and property
at the beginning of the block."
  (or pos-prop
      (setq pos-prop
            (markdown-max-of-seq
             #'car
             (cl-remove-if
              #'null
              (cl-mapcar
               #'markdown-find-previous-prop
               (markdown-get-fenced-block-begin-properties))))))
  (when pos-prop
    (goto-char (car pos-prop))
    (set-match-data (get-text-property (point) (cdr pos-prop)))
    ;; Note: Hard-coded group number assumes tilde
    ;; and GFM fenced code regexp groups agree.
    (let ((begin (match-beginning 3))
          (end (match-end 3)))
      (when (and begin end)
        ;; Fix language strings beginning with periods, like ".ruby".
        (when (eq (char-after begin) ?.)
          (setq begin (1+ begin)))
        (buffer-substring-no-properties begin end)))))

(defun markdown-gfm-parse-buffer-for-languages (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (save-excursion
      (goto-char (point-min))
      (cl-loop
       with prop = 'markdown-gfm-block-begin
       for pos-prop = (markdown-find-next-prop prop)
       while pos-prop
       for lang = (markdown-code-block-lang pos-prop)
       do (progn (when lang (markdown-gfm-add-used-language lang))
                 (goto-char (next-single-property-change (point) prop)))))))

(defun markdown-insert-foldable-block ()
  "Insert details disclosure element to make content foldable.
If a region is active, wrap this region with the disclosure
element. More details here https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details."
  (interactive)
  (let ((details-open-tag "<details>")
        (details-close-tag "</details>")
        (summary-open-tag "<summary>")
        (summary-close-tag " </summary>"))
    (if (use-region-p)
        (let* ((b (region-beginning))
               (e (region-end))
               (indent (progn (goto-char b) (current-indentation))))
          (goto-char e)
          ;; if we're on a blank line, don't newline, otherwise the tags
          ;; should go on its own line
          (unless (looking-back "\n" nil)
            (newline))
          (indent-to indent)
          (insert details-close-tag)
          (markdown-ensure-blank-line-after)
          (goto-char b)
          ;; if we're on a blank line, insert the quotes here, otherwise
          ;; add a new line first
          (unless (looking-at-p "\n")
            (newline)
            (forward-line -1))
          (markdown-ensure-blank-line-before)
          (indent-to indent)
          (insert details-open-tag "\n")
          (insert summary-open-tag summary-close-tag)
          (search-backward summary-close-tag))
      (let ((indent (current-indentation)))
        (delete-horizontal-space :backward-only)
        (markdown-ensure-blank-line-before)
        (indent-to indent)
        (insert details-open-tag "\n")
        (insert summary-open-tag summary-close-tag "\n")
        (insert details-close-tag)
        (indent-to indent)
        (markdown-ensure-blank-line-after)
        (search-backward summary-close-tag)))))


;;; Footnotes =================================================================

(defun markdown-footnote-counter-inc ()
  "Increment `markdown-footnote-counter' and return the new value."
  (when (= markdown-footnote-counter 0) ; hasn't been updated in this buffer yet.
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward (concat "^\\[\\^\\(" markdown-footnote-chars "*?\\)\\]:")
                                (point-max) t)
        (let ((fn (string-to-number (match-string 1))))
          (when (> fn markdown-footnote-counter)
            (setq markdown-footnote-counter fn))))))
  (cl-incf markdown-footnote-counter))

(defun markdown-insert-footnote ()
  "Insert footnote with a new number and move point to footnote definition."
  (interactive)
  (let ((fn (markdown-footnote-counter-inc)))
    (insert (format "[^%d]" fn))
    (push-mark (point) t)
    (markdown-footnote-text-find-new-location)
    (markdown-ensure-blank-line-before)
    (unless (markdown-cur-line-blank-p)
      (insert "\n"))
    (insert (format "[^%d]: " fn))
    (markdown-ensure-blank-line-after)))

(defun markdown-footnote-text-find-new-location ()
  "Position the point at the proper location for a new footnote text."
  (cond
   ((eq markdown-footnote-location 'end) (goto-char (point-max)))
   ((eq markdown-footnote-location 'immediately) (markdown-end-of-text-block))
   ((eq markdown-footnote-location 'subtree) (markdown-end-of-subtree))
   ((eq markdown-footnote-location 'header) (markdown-end-of-defun))))

(defun markdown-footnote-kill ()
  "Kill the footnote at point.
The footnote text is killed (and added to the kill ring), the
footnote marker is deleted.  Point has to be either at the
footnote marker or in the footnote text."
  (interactive)
  (let ((marker-pos nil)
        (skip-deleting-marker nil)
        (starting-footnote-text-positions
         (markdown-footnote-text-positions)))
    (when starting-footnote-text-positions
      ;; We're starting in footnote text, so mark our return position and jump
      ;; to the marker if possible.
      (let ((marker-pos (markdown-footnote-find-marker
                         (cl-first starting-footnote-text-positions))))
        (if marker-pos
            (goto-char (1- marker-pos))
          ;; If there isn't a marker, we still want to kill the text.
          (setq skip-deleting-marker t))))
    ;; Either we didn't start in the text, or we started in the text and jumped
    ;; to the marker. We want to assume we're at the marker now and error if
    ;; we're not.
    (unless skip-deleting-marker
      (let ((marker (markdown-footnote-delete-marker)))
        (unless marker
          (error "Not at a footnote"))
        ;; Even if we knew the text position before, it changed when we deleted
        ;; the label.
        (setq marker-pos (cl-second marker))
        (let ((new-text-pos (markdown-footnote-find-text (cl-first marker))))
          (unless new-text-pos
            (error "No text for footnote `%s'" (cl-first marker)))
          (goto-char new-text-pos))))
    (let ((pos (markdown-footnote-kill-text)))
      (goto-char (if starting-footnote-text-positions
                     pos
                   marker-pos)))))

(defun markdown-footnote-delete-marker ()
  "Delete a footnote marker at point.
Returns a list (ID START) containing the footnote ID and the
start position of the marker before deletion.  If no footnote
marker was deleted, this function returns NIL."
  (let ((marker (markdown-footnote-marker-positions)))
    (when marker
      (delete-region (cl-second marker) (cl-third marker))
      (butlast marker))))

(defun markdown-footnote-kill-text ()
  "Kill footnote text at point.
Returns the start position of the footnote text before deletion,
or NIL if point was not inside a footnote text.

The killed text is placed in the kill ring (without the footnote
number)."
  (let ((fn (markdown-footnote-text-positions)))
    (when fn
      (let ((text (delete-and-extract-region (cl-second fn) (cl-third fn))))
        (string-match (concat "\\[\\" (cl-first fn) "\\]:[[:space:]]*\\(\\(.*\n?\\)*\\)") text)
        (kill-new (match-string 1 text))
        (when (and (markdown-cur-line-blank-p)
                   (markdown-prev-line-blank-p)
                   (not (bobp)))
          (delete-region (1- (point)) (point)))
        (cl-second fn)))))

(defun markdown-footnote-goto-text ()
  "Jump to the text of the footnote at point."
  (interactive)
  (let ((fn (car (markdown-footnote-marker-positions))))
    (unless fn
      (user-error "Not at a footnote marker"))
    (let ((new-pos (markdown-footnote-find-text fn)))
      (unless new-pos
        (error "No definition found for footnote `%s'" fn))
      (goto-char new-pos))))

(defun markdown-footnote-return ()
  "Return from a footnote to its footnote number in the main text."
  (interactive)
  (let ((fn (save-excursion
              (car (markdown-footnote-text-positions)))))
    (unless fn
      (user-error "Not in a footnote"))
    (let ((new-pos (markdown-footnote-find-marker fn)))
      (unless new-pos
        (error "Footnote marker `%s' not found" fn))
      (goto-char new-pos))))

(defun markdown-footnote-find-marker (id)
  "Find the location of the footnote marker with ID.
The actual buffer position returned is the position directly
following the marker's closing bracket.  If no marker is found,
NIL is returned."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "\\[" id "\\]\\([^:]\\|\\'\\)") nil t)
      (skip-chars-backward "^]")
      (point))))

(defun markdown-footnote-find-text (id)
  "Find the location of the text of footnote ID.
The actual buffer position returned is the position of the first
character of the text, after the footnote's identifier.  If no
footnote text is found, NIL is returned."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward (concat "^ \\{0,3\\}\\[" id "\\]:") nil t)
      (skip-chars-forward "[ \t]")
      (point))))

(defun markdown-footnote-marker-positions ()
  "Return the position and ID of the footnote marker point is on.
The return value is a list (ID START END).  If point is not on a
footnote, NIL is returned."
  ;; first make sure we're at a footnote marker
  (if (or (looking-back (concat "\\[\\^" markdown-footnote-chars "*\\]?") (line-beginning-position))
          (looking-at-p (concat "\\[?\\^" markdown-footnote-chars "*?\\]")))
      (save-excursion
        ;; move point between [ and ^:
        (if (looking-at-p "\\[")
            (forward-char 1)
          (skip-chars-backward "^["))
        (looking-at (concat "\\(\\^" markdown-footnote-chars "*?\\)\\]"))
        (list (match-string 1) (1- (match-beginning 1)) (1+ (match-end 1))))))

(defun markdown-footnote-text-positions ()
  "Return the start and end positions of the footnote text point is in.
The exact return value is a list of three elements: (ID START END).
The start position is the position of the opening bracket
of the footnote id.  The end position is directly after the
newline that ends the footnote.  If point is not in a footnote,
NIL is returned instead."
  (save-excursion
    (let (result)
      (move-beginning-of-line 1)
      ;; Try to find the label. If we haven't found the label and we're at a blank
      ;; or indented line, back up if possible.
      (while (and
              (not (and (looking-at markdown-regex-footnote-definition)
                        (setq result (list (match-string 1) (point)))))
              (and (not (bobp))
                   (or (markdown-cur-line-blank-p)
                       (>= (current-indentation) 4))))
        (forward-line -1))
      (when result
        ;; Advance if there is a next line that is either blank or indented.
        ;; (Need to check if we're on the last line, because
        ;; markdown-next-line-blank-p returns true for last line in buffer.)
        (while (and (/= (line-end-position) (point-max))
                    (or (markdown-next-line-blank-p)
                        (>= (markdown-next-line-indent) 4)))
          (forward-line))
        ;; Move back while the current line is blank.
        (while (markdown-cur-line-blank-p)
          (forward-line -1))
        ;; Advance to capture this line and a single trailing newline (if there
        ;; is one).
        (forward-line)
        (append result (list (point)))))))

(defun markdown-get-defined-footnotes ()
  "Return a list of all defined footnotes.
Result is an alist of pairs (MARKER . LINE), where MARKER is the
footnote marker, a string, and LINE is the line number containing
the footnote definition.

For example, suppose the following footnotes are defined at positions
448 and 475:

\[^1]: First footnote here.
\[^marker]: Second footnote.

Then the returned list is: ((\"^1\" . 478) (\"^marker\" . 475))"
  (save-excursion
    (goto-char (point-min))
    (let (footnotes)
      (while (markdown-search-until-condition
              (lambda () (and (not (markdown-code-block-at-point-p))
                              (not (markdown-inline-code-at-point-p))
                              (not (markdown-in-comment-p))))
              markdown-regex-footnote-definition nil t)
        (let ((marker (match-string-no-properties 1))
              (pos (match-beginning 0)))
          (unless (zerop (length marker))
            (cl-pushnew (cons marker pos) footnotes :test #'equal))))
      (reverse footnotes))))


;;; Element Removal ===========================================================

(defun markdown-kill-thing-at-point ()
  "Kill thing at point and add important text, without markup, to kill ring.
Possible things to kill include (roughly in order of precedence):
inline code, headers, horizontal rules, links (add link text to
kill ring), images (add alt text to kill ring), angle uri, email
addresses, bold, italics, reference definition (add URI to kill
ring), footnote markers and text (kill both marker and text, add
text to kill ring), and list items."
  (interactive "*")
  (let (val)
    (cond
     ;; Inline code
     ((markdown-inline-code-at-point)
      (kill-new (match-string 2))
      (delete-region (match-beginning 0) (match-end 0)))
     ;; ATX header
     ((thing-at-point-looking-at markdown-regex-header-atx)
      (kill-new (match-string 2))
      (delete-region (match-beginning 0) (match-end 0)))
     ;; Setext header
     ((thing-at-point-looking-at markdown-regex-header-setext)
      (kill-new (match-string 1))
      (delete-region (match-beginning 0) (match-end 0)))
     ;; Horizontal rule
     ((thing-at-point-looking-at markdown-regex-hr)
      (kill-new (match-string 0))
      (delete-region (match-beginning 0) (match-end 0)))
     ;; Inline link or image (add link or alt text to kill ring)
     ((thing-at-point-looking-at markdown-regex-link-inline)
      (kill-new (match-string 3))
      (delete-region (match-beginning 0) (match-end 0)))
     ;; Reference link or image (add link or alt text to kill ring)
     ((thing-at-point-looking-at markdown-regex-link-reference)
      (kill-new (match-string 3))
      (delete-region (match-beginning 0) (match-end 0)))
     ;; Angle URI (add URL to kill ring)
     ((thing-at-point-looking-at markdown-regex-angle-uri)
      (kill-new (match-string 2))
      (delete-region (match-beginning 0) (match-end 0)))
     ;; Email address in angle brackets (add email address to kill ring)
     ((thing-at-point-looking-at markdown-regex-email)
      (kill-new (match-string 1))
      (delete-region (match-beginning 0) (match-end 0)))
     ;; Wiki link (add alias text to kill ring)
     ((and markdown-enable-wiki-links
           (thing-at-point-looking-at markdown-regex-wiki-link))
      (kill-new (markdown-wiki-link-alias))
      (delete-region (match-beginning 1) (match-end 1)))
     ;; Bold
     ((thing-at-point-looking-at markdown-regex-bold)
      (kill-new (match-string 4))
      (delete-region (match-beginning 2) (match-end 2)))
     ;; Italics
     ((thing-at-point-looking-at markdown-regex-italic)
      (kill-new (match-string 3))
      (delete-region (match-beginning 1) (match-end 1)))
     ;; Strikethrough
     ((thing-at-point-looking-at markdown-regex-strike-through)
      (kill-new (match-string 4))
      (delete-region (match-beginning 2) (match-end 2)))
     ;; Footnote marker (add footnote text to kill ring)
     ((thing-at-point-looking-at markdown-regex-footnote)
      (markdown-footnote-kill))
     ;; Footnote text (add footnote text to kill ring)
     ((setq val (markdown-footnote-text-positions))
      (markdown-footnote-kill))
     ;; Reference definition (add URL to kill ring)
     ((thing-at-point-looking-at markdown-regex-reference-definition)
      (kill-new (match-string 5))
      (delete-region (match-beginning 0) (match-end 0)))
     ;; List item
     ((setq val (markdown-cur-list-item-bounds))
      (kill-new (delete-and-extract-region (cl-first val) (cl-second val))))
     (t
      (user-error "Nothing found at point to kill")))))

(defun markdown-kill-outline ()
  "Kill visible heading and add it to `kill-ring'."
  (interactive)
  (save-excursion
    (markdown-outline-previous)
    (kill-region (point) (progn (markdown-outline-next) (point)))))

(defun markdown-kill-block ()
  "Kill visible code block, list item, or blockquote and add it to `kill-ring'."
  (interactive)
  (save-excursion
    (markdown-backward-block)
    (kill-region (point) (progn (markdown-forward-block) (point)))))


;;; Indentation ===============================================================

(defun markdown-indent-find-next-position (cur-pos positions)
  "Return the position after the index of CUR-POS in POSITIONS.
Positions are calculated by `markdown-calc-indents'."
  (while (and positions
              (not (equal cur-pos (car positions))))
    (setq positions (cdr positions)))
  (or (cadr positions) 0))

(defun markdown-outdent-find-next-position (cur-pos positions)
  "Return the maximal element that precedes CUR-POS from POSITIONS.
Positions are calculated by `markdown-calc-indents'."
  (let ((result 0))
    (dolist (i positions)
      (when (< i cur-pos)
        (setq result (max result i))))
    result))

(defun markdown-indent-line ()
  "Indent the current line using some heuristics.
If the _previous_ command was either `markdown-enter-key' or
`markdown-cycle', then we should cycle to the next
reasonable indentation position.  Otherwise, we could have been
called directly by `markdown-enter-key', by an initial call of
`markdown-cycle', or indirectly by `auto-fill-mode'.  In
these cases, indent to the default position.
Positions are calculated by `markdown-calc-indents'."
  (interactive)
  (let ((positions (markdown-calc-indents))
        (point-pos (current-column))
        (_ (back-to-indentation))
        (cur-pos (current-column)))
    (if (not (equal this-command 'markdown-cycle))
        (indent-line-to (car positions))
      (setq positions (sort (delete-dups positions) '<))
      (let* ((next-pos (markdown-indent-find-next-position cur-pos positions))
             (new-point-pos (max (+ point-pos (- next-pos cur-pos)) 0)))
        (indent-line-to next-pos)
        (move-to-column new-point-pos)))))

(defun markdown-calc-indents ()
  "Return a list of indentation columns to cycle through.
The first element in the returned list should be considered the
default indentation level.  This function does not worry about
duplicate positions, which are handled up by calling functions."
  (let (pos prev-line-pos positions)

    ;; Indentation of previous line
    (setq prev-line-pos (markdown-prev-line-indent))
    (setq positions (cons prev-line-pos positions))

    ;; Indentation of previous non-list-marker text
    (when (setq pos (save-excursion
                      (forward-line -1)
                      (when (looking-at markdown-regex-list)
                        (- (match-end 3) (match-beginning 0)))))
      (setq positions (cons pos positions)))

    ;; Indentation required for a pre block in current context
    (setq pos (length (markdown-pre-indentation (point))))
    (setq positions (cons pos positions))

    ;; Indentation of the previous line + tab-width
    (if prev-line-pos
        (setq positions (cons (+ prev-line-pos tab-width) positions))
      (setq positions (cons tab-width positions)))

    ;; Indentation of the previous line - tab-width
    (if (and prev-line-pos (> prev-line-pos tab-width))
        (setq positions (cons (- prev-line-pos tab-width) positions)))

    ;; Indentation of all preceding list markers (when in a list)
    (when (setq pos (markdown-calculate-list-levels))
      (setq positions (append pos positions)))

    ;; First column
    (setq positions (cons 0 positions))

    ;; Return reversed list
    (reverse positions)))

(defun markdown-enter-key ()        ;FIXME: Partly obsoleted by electric-indent
  "Handle RET depending on the context.
If the point is at a table, move to the next row.  Otherwise,
indent according to value of `markdown-indent-on-enter'.
When it is nil, simply call `newline'.  Otherwise, indent the next line
following RET using `markdown-indent-line'.  Furthermore, when it
is set to \\='indent-and-new-item and the point is in a list item,
start a new item with the same indentation. If the point is in an
empty list item, remove it (so that pressing RET twice when in a
list simply adds a blank line)."
  (interactive)
  (cond
   ;; Table
   ((markdown-table-at-point-p)
    (call-interactively #'markdown-table-next-row))
   ;; Indent non-table text
   (markdown-indent-on-enter
    (let (bounds)
      (if (and (memq markdown-indent-on-enter '(indent-and-new-item))
               (setq bounds (markdown-cur-list-item-bounds)))
          (let ((beg (cl-first bounds))
                (end (cl-second bounds))
                (nonlist-indent (cl-fourth bounds))
                (checkbox (cl-sixth bounds)))
            ;; Point is in a list item
            (if (= (- end beg) (+ nonlist-indent (length checkbox)))
                ;; Delete blank list
                (progn
                  (delete-region beg end)
                  (newline)
                  (markdown-indent-line))
              (call-interactively #'markdown-insert-list-item)))
        ;; Point is not in a list
        (newline)
        (markdown-indent-line))))
   ;; Insert a raw newline
   (t (newline))))

(defun markdown-outdent-or-delete (arg)
  "Handle BACKSPACE by cycling through indentation points.
When BACKSPACE is pressed, if there is only whitespace
before the current point, then outdent the line one level.
Otherwise, do normal delete by repeating
`backward-delete-char-untabify' ARG times."
  (interactive "*p")
  (if (use-region-p)
      (backward-delete-char-untabify arg)
    (let ((cur-pos (current-column))
          (start-of-indention (save-excursion
                                (back-to-indentation)
                                (current-column)))
          (positions (markdown-calc-indents)))
      (if (and (> cur-pos 0) (= cur-pos start-of-indention))
          (indent-line-to (markdown-outdent-find-next-position cur-pos positions))
        (backward-delete-char-untabify arg)))))

(defun markdown-find-leftmost-column (beg end)
  "Find the leftmost column in the region from BEG to END."
  (let ((mincol 1000))
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (back-to-indentation)
        (unless (looking-at-p "[ \t]*$")
          (setq mincol (min mincol (current-column))))
        (forward-line 1)
        ))
    mincol))

(defun markdown-indent-region (beg end arg)
  "Indent the region from BEG to END using some heuristics.
When ARG is non-nil, outdent the region instead.
See `markdown-indent-line' and `markdown-indent-line'."
  (interactive "*r\nP")
  (let* ((positions (sort (delete-dups (markdown-calc-indents)) '<))
         (leftmostcol (markdown-find-leftmost-column beg end))
         (next-pos (if arg
                       (markdown-outdent-find-next-position leftmostcol positions)
                     (markdown-indent-find-next-position leftmostcol positions))))
    (indent-rigidly beg end (- next-pos leftmostcol))
    (setq deactivate-mark nil)))

(defun markdown-outdent-region (beg end)
  "Call `markdown-indent-region' on region from BEG to END with prefix."
  (interactive "*r")
  (markdown-indent-region beg end t))

(defun markdown--indent-region (start end)
  (let ((deactivate-mark nil))
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (when (bolp)
        (forward-line 1))
      (while (< (point) end)
        (unless (or (markdown-code-block-at-point-p) (and (bolp) (eolp)))
          (indent-according-to-mode))
        (forward-line 1))
      (move-marker end nil))))


;;; Markup Completion =========================================================

(defconst markdown-complete-alist
  '((markdown-regex-header-atx . markdown-complete-atx)
    (markdown-regex-header-setext . markdown-complete-setext)
    (markdown-regex-hr . markdown-complete-hr))
  "Association list of form (regexp . function) for markup completion.")

(defun markdown-incomplete-atx-p ()
  "Return t if ATX header markup is incomplete and nil otherwise.
Assumes match data is available for `markdown-regex-header-atx'.
Checks that the number of trailing hash marks equals the number of leading
hash marks, that there is only a single space before and after the text,
and that there is no extraneous whitespace in the text."
  (or
   ;; Number of starting and ending hash marks differs
   (not (= (length (match-string 1)) (length (match-string 3))))
   ;; When the header text is not empty...
   (and (> (length (match-string 2)) 0)
        ;; ...if there are extra leading, trailing, or interior spaces
        (or (not (= (match-beginning 2) (1+ (match-end 1))))
            (not (= (match-beginning 3) (1+ (match-end 2))))
            (string-match-p "[ \t\n]\\{2\\}" (match-string 2))))
   ;; When the header text is empty...
   (and (= (length (match-string 2)) 0)
        ;; ...if there are too many or too few spaces
        (not (= (match-beginning 3) (+ (match-end 1) 2))))))

(defun markdown-complete-atx ()
  "Complete and normalize ATX headers.
Add or remove hash marks to the end of the header to match the
beginning.  Ensure that there is only a single space between hash
marks and header text.  Removes extraneous whitespace from header text.
Assumes match data is available for `markdown-regex-header-atx'.
Return nil if markup was complete and non-nil if markup was completed."
  (when (markdown-incomplete-atx-p)
    (let* ((new-marker (make-marker))
           (new-marker (set-marker new-marker (match-end 2))))
      ;; Hash marks and spacing at end
      (goto-char (match-end 2))
      (delete-region (match-end 2) (match-end 3))
      (insert " " (match-string 1))
      ;; Remove extraneous whitespace from title
      (replace-match (markdown-compress-whitespace-string (match-string 2))
                     t t nil 2)
      ;; Spacing at beginning
      (goto-char (match-end 1))
      (delete-region (match-end 1) (match-beginning 2))
      (insert " ")
      ;; Leave point at end of text
      (goto-char new-marker))))

(defun markdown-incomplete-setext-p ()
  "Return t if setext header markup is incomplete and nil otherwise.
Assumes match data is available for `markdown-regex-header-setext'.
Checks that length of underline matches text and that there is no
extraneous whitespace in the text."
  (or (not (= (length (match-string 1)) (length (match-string 2))))
      (string-match-p "[ \t\n]\\{2\\}" (match-string 1))))

(defun markdown-complete-setext ()
  "Complete and normalize setext headers.
Add or remove underline characters to match length of header
text.  Removes extraneous whitespace from header text.  Assumes
match data is available for `markdown-regex-header-setext'.
Return nil if markup was complete and non-nil if markup was completed."
  (when (markdown-incomplete-setext-p)
    (let* ((text (markdown-compress-whitespace-string (match-string 1)))
           (char (char-after (match-beginning 2)))
           (level (if (char-equal char ?-) 2 1)))
      (goto-char (match-beginning 0))
      (delete-region (match-beginning 0) (match-end 0))
      (markdown-insert-header level text t)
      t)))

(defun markdown-incomplete-hr-p ()
  "Return non-nil if hr is not in `markdown-hr-strings' and nil otherwise.
Assumes match data is available for `markdown-regex-hr'."
  (not (member (match-string 0) markdown-hr-strings)))

(defun markdown-complete-hr ()
  "Complete horizontal rules.
If horizontal rule string is a member of `markdown-hr-strings',
do nothing.  Otherwise, replace with the car of
`markdown-hr-strings'.
Assumes match data is available for `markdown-regex-hr'.
Return nil if markup was complete and non-nil if markup was completed."
  (when (markdown-incomplete-hr-p)
    (replace-match (car markdown-hr-strings))
    t))

(defun markdown-complete ()
  "Complete markup of object near point or in region when active.
Handle all objects in `markdown-complete-alist', in order.
See `markdown-complete-at-point' and `markdown-complete-region'."
  (interactive "*")
  (if (use-region-p)
      (markdown-complete-region (region-beginning) (region-end))
    (markdown-complete-at-point)))

(defun markdown-complete-at-point ()
  "Complete markup of object near point.
Handle all elements of `markdown-complete-alist' in order."
  (interactive "*")
  (let ((list markdown-complete-alist) found changed)
    (while list
      (let ((regexp (eval (caar list) t)) ;FIXME: Why `eval'?
            (function (cdar list)))
        (setq list (cdr list))
        (when (thing-at-point-looking-at regexp)
          (setq found t)
          (setq changed (funcall function))
          (setq list nil))))
    (if found
        (or changed (user-error "Markup at point is complete"))
      (user-error "Nothing to complete at point"))))

(defun markdown-complete-region (beg end)
  "Complete markup of objects in region from BEG to END.
Handle all objects in `markdown-complete-alist', in order.  Each
match is checked to ensure that a previous regexp does not also
match."
  (interactive "*r")
  (let ((end-marker (set-marker (make-marker) end))
        previous)
    (dolist (element markdown-complete-alist)
      (let ((regexp (eval (car element) t)) ;FIXME: Why `eval'?
            (function (cdr element)))
        (goto-char beg)
        (while (re-search-forward regexp end-marker 'limit)
          (when (match-string 0)
            ;; Make sure this is not a match for any of the preceding regexps.
            ;; This prevents mistaking an HR for a Setext subheading.
            (let (match)
              (save-match-data
                (dolist (prev-regexp previous)
                  (or match (setq match (looking-back prev-regexp nil)))))
              (unless match
                (save-excursion (funcall function))))))
        (cl-pushnew regexp previous :test #'equal)))
    previous))

(defun markdown-complete-buffer ()
  "Complete markup for all objects in the current buffer."
  (interactive "*")
  (markdown-complete-region (point-min) (point-max)))


;;; Markup Cycling ============================================================

(defun markdown-cycle-atx (arg &optional remove)
  "Cycle ATX header markup.
Promote header (decrease level) when ARG is 1 and demote
header (increase level) if arg is -1.  When REMOVE is non-nil,
remove the header when the level reaches zero and stop cycling
when it reaches six.  Otherwise, perform a proper cycling through
levels one through six.  Assumes match data is available for
`markdown-regex-header-atx'."
  (let* ((old-level (length (match-string 1)))
         (new-level (+ old-level arg))
         (text (match-string 2)))
    (when (not remove)
      (setq new-level (% new-level 6))
      (setq new-level (cond ((= new-level 0) 6)
                            ((< new-level 0) (+ new-level 6))
                            (t new-level))))
    (cond
     ((= new-level 0)
      (markdown-unwrap-thing-at-point nil 0 2))
     ((<= new-level 6)
      (goto-char (match-beginning 0))
      (delete-region (match-beginning 0) (match-end 0))
      (markdown-insert-header new-level text nil)))))

(defun markdown-cycle-setext (arg &optional remove)
  "Cycle setext header markup.
Promote header (increase level) when ARG is 1 and demote
header (decrease level or remove) if arg is -1.  When demoting a
level-two setext header, replace with a level-three atx header.
When REMOVE is non-nil, remove the header when the level reaches
zero.  Otherwise, cycle back to a level six atx header.  Assumes
match data is available for `markdown-regex-header-setext'."
  (let* ((char (char-after (match-beginning 2)))
         (old-level (if (char-equal char ?=) 1 2))
         (new-level (+ old-level arg)))
    (when (and (not remove) (= new-level 0))
      (setq new-level 6))
    (cond
     ((= new-level 0)
      (markdown-unwrap-thing-at-point nil 0 1))
     ((<= new-level 2)
      (markdown-insert-header new-level nil t))
     ((<= new-level 6)
      (markdown-insert-header new-level nil nil)))))

(defun markdown-cycle-hr (arg &optional remove)
  "Cycle string used for horizontal rule from `markdown-hr-strings'.
When ARG is 1, cycle forward (demote), and when ARG is -1, cycle
backwards (promote).  When REMOVE is non-nil, remove the hr instead
of cycling when the end of the list is reached.
Assumes match data is available for `markdown-regex-hr'."
  (let* ((strings (if (= arg -1)
                      (reverse markdown-hr-strings)
                    markdown-hr-strings))
         (tail (member (match-string 0) strings))
         (new (or (cadr tail)
                  (if remove
                      (if (= arg 1)
                          ""
                        (car tail))
                    (car strings)))))
    (replace-match new)))

(defun markdown-cycle-bold ()
  "Cycle bold markup between underscores and asterisks.
Assumes match data is available for `markdown-regex-bold'."
  (save-excursion
    (let* ((old-delim (match-string 3))
           (new-delim (if (string-equal old-delim "**") "__" "**")))
      (replace-match new-delim t t nil 3)
      (replace-match new-delim t t nil 5))))

(defun markdown-cycle-italic ()
  "Cycle italic markup between underscores and asterisks.
Assumes match data is available for `markdown-regex-italic'."
  (save-excursion
    (let* ((old-delim (match-string 2))
           (new-delim (if (string-equal old-delim "*") "_" "*")))
      (replace-match new-delim t t nil 2)
      (replace-match new-delim t t nil 4))))


;;; Keymap ====================================================================

(defun markdown--style-map-prompt ()
  "Return a formatted prompt for Markdown markup insertion."
  (when markdown-enable-prefix-prompts
    (concat
     "Markdown: "
     (propertize "bold" 'face 'markdown-bold-face) ", "
     (propertize "italic" 'face 'markdown-italic-face) ", "
     (propertize "code" 'face 'markdown-inline-code-face) ", "
     (propertize "C = GFM code" 'face 'markdown-code-face) ", "
     (propertize "pre" 'face 'markdown-pre-face) ", "
     (propertize "footnote" 'face 'markdown-footnote-text-face) ", "
     (propertize "F = foldable" 'face 'markdown-bold-face) ", "
     (propertize "q = blockquote" 'face 'markdown-blockquote-face) ", "
     (propertize "h & 1-6 = heading" 'face 'markdown-header-face) ", "
     (propertize "- = hr" 'face 'markdown-hr-face) ", "
     "C-h = more")))

(defun markdown--command-map-prompt ()
  "Return prompt for Markdown buffer-wide commands."
  (when markdown-enable-prefix-prompts
    (concat
     "Command: "
     (propertize "m" 'face 'markdown-bold-face) "arkdown, "
     (propertize "p" 'face 'markdown-bold-face) "review, "
     (propertize "o" 'face 'markdown-bold-face) "pen, "
     (propertize "e" 'face 'markdown-bold-face) "xport, "
     "export & pre" (propertize "v" 'face 'markdown-bold-face) "iew, "
     (propertize "c" 'face 'markdown-bold-face) "heck refs, "
     (propertize "u" 'face 'markdown-bold-face) "nused refs, "
     "C-h = more")))

(defvar markdown-mode-style-map
  (let ((map (make-keymap (markdown--style-map-prompt))))
    (define-key map (kbd "1") 'markdown-insert-header-atx-1)
    (define-key map (kbd "2") 'markdown-insert-header-atx-2)
    (define-key map (kbd "3") 'markdown-insert-header-atx-3)
    (define-key map (kbd "4") 'markdown-insert-header-atx-4)
    (define-key map (kbd "5") 'markdown-insert-header-atx-5)
    (define-key map (kbd "6") 'markdown-insert-header-atx-6)
    (define-key map (kbd "!") 'markdown-insert-header-setext-1)
    (define-key map (kbd "@") 'markdown-insert-header-setext-2)
    (define-key map (kbd "b") 'markdown-insert-bold)
    (define-key map (kbd "c") 'markdown-insert-code)
    (define-key map (kbd "C") 'markdown-insert-gfm-code-block)
    (define-key map (kbd "f") 'markdown-insert-footnote)
    (define-key map (kbd "F") 'markdown-insert-foldable-block)
    (define-key map (kbd "h") 'markdown-insert-header-dwim)
    (define-key map (kbd "H") 'markdown-insert-header-setext-dwim)
    (define-key map (kbd "i") 'markdown-insert-italic)
    (define-key map (kbd "k") 'markdown-insert-kbd)
    (define-key map (kbd "l") 'markdown-insert-link)
    (define-key map (kbd "p") 'markdown-insert-pre)
    (define-key map (kbd "P") 'markdown-pre-region)
    (define-key map (kbd "q") 'markdown-insert-blockquote)
    (define-key map (kbd "s") 'markdown-insert-strike-through)
    (define-key map (kbd "t") 'markdown-insert-table)
    (define-key map (kbd "Q") 'markdown-blockquote-region)
    (define-key map (kbd "w") 'markdown-insert-wiki-link)
    (define-key map (kbd "-") 'markdown-insert-hr)
    (define-key map (kbd "[") 'markdown-insert-gfm-checkbox)
    ;; Deprecated keys that may be removed in a future version
    (define-key map (kbd "e") 'markdown-insert-italic)
    map)
  "Keymap for Markdown text styling commands.")

(defvar markdown-mode-command-map
  (let ((map (make-keymap (markdown--command-map-prompt))))
    (define-key map (kbd "m") 'markdown-other-window)
    (define-key map (kbd "p") 'markdown-preview)
    (define-key map (kbd "e") 'markdown-export)
    (define-key map (kbd "v") 'markdown-export-and-preview)
    (define-key map (kbd "o") 'markdown-open)
    (define-key map (kbd "l") 'markdown-live-preview-mode)
    (define-key map (kbd "w") 'markdown-kill-ring-save)
    (define-key map (kbd "c") 'markdown-check-refs)
    (define-key map (kbd "u") 'markdown-unused-refs)
    (define-key map (kbd "n") 'markdown-cleanup-list-numbers)
    (define-key map (kbd "]") 'markdown-complete-buffer)
    (define-key map (kbd "^") 'markdown-table-sort-lines)
    (define-key map (kbd "|") 'markdown-table-convert-region)
    (define-key map (kbd "t") 'markdown-table-transpose)
    map)
  "Keymap for Markdown buffer-wide commands.")

(defvar markdown-mode-map
  (let ((map (make-keymap)))
    ;; Markup insertion & removal
    (define-key map (kbd "C-c C-s") markdown-mode-style-map)
    (define-key map (kbd "C-c C-l") 'markdown-insert-link)
    (define-key map (kbd "C-c C-k") 'markdown-kill-thing-at-point)
    ;; Promotion, demotion, and cycling
    (define-key map (kbd "C-c C--") 'markdown-promote)
    (define-key map (kbd "C-c C-=") 'markdown-demote)
    (define-key map (kbd "C-c C-]") 'markdown-complete)
    ;; Following and doing things
    (define-key map (kbd "C-c C-o") 'markdown-follow-thing-at-point)
    (define-key map (kbd "C-c C-d") 'markdown-do)
    (define-key map (kbd "C-c '") 'markdown-edit-code-block)
    ;; Indentation
    (define-key map (kbd "RET") 'markdown-enter-key)
    (define-key map (kbd "DEL") 'markdown-outdent-or-delete)
    (define-key map (kbd "C-c >") 'markdown-indent-region)
    (define-key map (kbd "C-c <") 'markdown-outdent-region)
    ;; Visibility cycling
    (define-key map (kbd "TAB") 'markdown-cycle)
    ;; S-iso-lefttab and S-tab should both be mapped to `backtab' by
    ;; (local-)function-key-map.
    ;;(define-key map (kbd "<S-iso-lefttab>") 'markdown-shifttab)
    ;;(define-key map (kbd "<S-tab>")  'markdown-shifttab)
    (define-key map (kbd "<backtab>") 'markdown-shifttab)
    ;; Heading and list navigation
    (define-key map (kbd "C-c C-n") 'markdown-outline-next)
    (define-key map (kbd "C-c C-p") 'markdown-outline-previous)
    (define-key map (kbd "C-c C-f") 'markdown-outline-next-same-level)
    (define-key map (kbd "C-c C-b") 'markdown-outline-previous-same-level)
    (define-key map (kbd "C-c C-u") 'markdown-outline-up)
    ;; Buffer-wide commands
    (define-key map (kbd "C-c C-c") markdown-mode-command-map)
    ;; Subtree, list, and table editing
    (define-key map (kbd "C-c <up>") 'markdown-move-up)
    (define-key map (kbd "C-c <down>") 'markdown-move-down)
    (define-key map (kbd "C-c <left>") 'markdown-promote)
    (define-key map (kbd "C-c <right>") 'markdown-demote)
    (define-key map (kbd "C-c S-<up>") 'markdown-table-delete-row)
    (define-key map (kbd "C-c S-<down>") 'markdown-table-insert-row)
    (define-key map (kbd "C-c S-<left>") 'markdown-table-delete-column)
    (define-key map (kbd "C-c S-<right>") 'markdown-table-insert-column)
    (define-key map (kbd "C-c C-M-h") 'markdown-mark-subtree)
    (define-key map (kbd "C-x n s") 'markdown-narrow-to-subtree)
    (define-key map (kbd "M-RET") 'markdown-insert-list-item)
    (define-key map (kbd "C-c C-j") 'markdown-insert-list-item)
    ;; Paragraphs (Markdown context aware)
    (define-key map [remap backward-paragraph] 'markdown-backward-paragraph)
    (define-key map [remap forward-paragraph] 'markdown-forward-paragraph)
    (define-key map [remap mark-paragraph] 'markdown-mark-paragraph)
    ;; Blocks (one or more paragraphs)
    (define-key map (kbd "C-M-{") 'markdown-backward-block)
    (define-key map (kbd "C-M-}") 'markdown-forward-block)
    (define-key map (kbd "C-c M-h") 'markdown-mark-block)
    (define-key map (kbd "C-x n b") 'markdown-narrow-to-block)
    ;; Pages (top-level sections)
    (define-key map [remap backward-page] 'markdown-backward-page)
    (define-key map [remap forward-page] 'markdown-forward-page)
    (define-key map [remap mark-page] 'markdown-mark-page)
    (define-key map [remap narrow-to-page] 'markdown-narrow-to-page)
    ;; Link Movement
    (define-key map (kbd "M-n") 'markdown-next-link)
    (define-key map (kbd "M-p") 'markdown-previous-link)
    ;; Toggling functionality
    (define-key map (kbd "C-c C-x C-e") 'markdown-toggle-math)
    (define-key map (kbd "C-c C-x C-f") 'markdown-toggle-fontify-code-blocks-natively)
    (define-key map (kbd "C-c C-x C-i") 'markdown-toggle-inline-images)
    (define-key map (kbd "C-c C-x C-l") 'markdown-toggle-url-hiding)
    (define-key map (kbd "C-c C-x C-m") 'markdown-toggle-markup-hiding)
    ;; Alternative keys (in case of problems with the arrow keys)
    (define-key map (kbd "C-c C-x u") 'markdown-move-up)
    (define-key map (kbd "C-c C-x d") 'markdown-move-down)
    (define-key map (kbd "C-c C-x l") 'markdown-promote)
    (define-key map (kbd "C-c C-x r") 'markdown-demote)
    ;; Deprecated keys that may be removed in a future version
    (define-key map (kbd "C-c C-a L") 'markdown-insert-link) ;; C-c C-l
    (define-key map (kbd "C-c C-a l") 'markdown-insert-link) ;; C-c C-l
    (define-key map (kbd "C-c C-a r") 'markdown-insert-link) ;; C-c C-l
    (define-key map (kbd "C-c C-a u") 'markdown-insert-uri) ;; C-c C-l
    (define-key map (kbd "C-c C-a f") 'markdown-insert-footnote)
    (define-key map (kbd "C-c C-a w") 'markdown-insert-wiki-link)
    (define-key map (kbd "C-c C-t 1") 'markdown-insert-header-atx-1)
    (define-key map (kbd "C-c C-t 2") 'markdown-insert-header-atx-2)
    (define-key map (kbd "C-c C-t 3") 'markdown-insert-header-atx-3)
    (define-key map (kbd "C-c C-t 4") 'markdown-insert-header-atx-4)
    (define-key map (kbd "C-c C-t 5") 'markdown-insert-header-atx-5)
    (define-key map (kbd "C-c C-t 6") 'markdown-insert-header-atx-6)
    (define-key map (kbd "C-c C-t !") 'markdown-insert-header-setext-1)
    (define-key map (kbd "C-c C-t @") 'markdown-insert-header-setext-2)
    (define-key map (kbd "C-c C-t h") 'markdown-insert-header-dwim)
    (define-key map (kbd "C-c C-t H") 'markdown-insert-header-setext-dwim)
    (define-key map (kbd "C-c C-t s") 'markdown-insert-header-setext-2)
    (define-key map (kbd "C-c C-t t") 'markdown-insert-header-setext-1)
    (define-key map (kbd "C-c C-i") 'markdown-insert-image)
    (define-key map (kbd "C-c C-x m") 'markdown-insert-list-item) ;; C-c C-j
    (define-key map (kbd "C-c C-x C-x") 'markdown-toggle-gfm-checkbox) ;; C-c C-d
    (define-key map (kbd "C-c -") 'markdown-insert-hr)
    map)
  "Keymap for Markdown major mode.")

(defvar markdown-mode-mouse-map
  (when markdown-mouse-follow-link
    (let ((map (make-sparse-keymap)))
      (define-key map [follow-link] 'mouse-face)
      (define-key map [mouse-2] #'markdown-follow-thing-at-point)
      map))
  "Keymap for following links with mouse.")

(defvar gfm-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map markdown-mode-map)
    (define-key map (kbd "C-c C-s d") 'markdown-insert-strike-through)
    (define-key map "`" 'markdown-electric-backquote)
    map)
  "Keymap for `gfm-mode'.
See also `markdown-mode-map'.")


;;; Menu ======================================================================

(easy-menu-define markdown-mode-menu markdown-mode-map
  "Menu for Markdown mode."
  '("Markdown"
    "---"
    ("Movement"
     ["Jump" markdown-do]
     ["Follow Link" markdown-follow-thing-at-point]
     ["Next Link" markdown-next-link]
     ["Previous Link" markdown-previous-link]
     "---"
     ["Next Heading or List Item" markdown-outline-next]
     ["Previous Heading or List Item" markdown-outline-previous]
     ["Next at Same Level" markdown-outline-next-same-level]
     ["Previous at Same Level" markdown-outline-previous-same-level]
     ["Up to Parent" markdown-outline-up]
     "---"
     ["Forward Paragraph" markdown-forward-paragraph]
     ["Backward Paragraph" markdown-backward-paragraph]
     ["Forward Block" markdown-forward-block]
     ["Backward Block" markdown-backward-block])
    ("Show & Hide"
     ["Cycle Heading Visibility" markdown-cycle
      :enable (markdown-on-heading-p)]
     ["Cycle Heading Visibility (Global)" markdown-shifttab]
     "---"
     ["Narrow to Region" narrow-to-region]
     ["Narrow to Block" markdown-narrow-to-block]
     ["Narrow to Section" narrow-to-defun]
     ["Narrow to Subtree" markdown-narrow-to-subtree]
     ["Widen" widen (buffer-narrowed-p)]
     "---"
     ["Toggle Markup Hiding" markdown-toggle-markup-hiding
      :keys "C-c C-x C-m"
      :style radio
      :selected markdown-hide-markup])
    "---"
    ("Headings & Structure"
     ["Automatic Heading" markdown-insert-header-dwim
      :keys "C-c C-s h"]
     ["Automatic Heading (Setext)" markdown-insert-header-setext-dwim
      :keys "C-c C-s H"]
     ("Specific Heading (atx)"
      ["First Level atx" markdown-insert-header-atx-1
       :keys "C-c C-s 1"]
      ["Second Level atx" markdown-insert-header-atx-2
       :keys "C-c C-s 2"]
      ["Third Level atx" markdown-insert-header-atx-3
       :keys "C-c C-s 3"]
      ["Fourth Level atx" markdown-insert-header-atx-4
       :keys "C-c C-s 4"]
      ["Fifth Level atx" markdown-insert-header-atx-5
       :keys "C-c C-s 5"]
      ["Sixth Level atx" markdown-insert-header-atx-6
       :keys "C-c C-s 6"])
     ("Specific Heading (Setext)"
      ["First Level Setext" markdown-insert-header-setext-1
       :keys "C-c C-s !"]
      ["Second Level Setext" markdown-insert-header-setext-2
       :keys "C-c C-s @"])
     ["Horizontal Rule" markdown-insert-hr
      :keys "C-c C-s -"]
     "---"
     ["Move Subtree Up" markdown-move-up
      :keys "C-c <up>"]
     ["Move Subtree Down" markdown-move-down
      :keys "C-c <down>"]
     ["Promote Subtree" markdown-promote
      :keys "C-c <left>"]
     ["Demote Subtree" markdown-demote
      :keys "C-c <right>"])
    ("Region & Mark"
     ["Indent Region" markdown-indent-region]
     ["Outdent Region" markdown-outdent-region]
     "--"
     ["Mark Paragraph" mark-paragraph]
     ["Mark Block" markdown-mark-block]
     ["Mark Section" mark-defun]
     ["Mark Subtree" markdown-mark-subtree])
    ("Tables"
     ["Move Row Up" markdown-move-up
      :enable (markdown-table-at-point-p)
      :keys "C-c <up>"]
     ["Move Row Down" markdown-move-down
      :enable (markdown-table-at-point-p)
      :keys "C-c <down>"]
     ["Move Column Left" markdown-promote
      :enable (markdown-table-at-point-p)
      :keys "C-c <left>"]
     ["Move Column Right" markdown-demote
      :enable (markdown-table-at-point-p)
      :keys "C-c <right>"]
     ["Delete Row" markdown-table-delete-row
      :enable (markdown-table-at-point-p)]
     ["Insert Row" markdown-table-insert-row
      :enable (markdown-table-at-point-p)]
     ["Delete Column" markdown-table-delete-column
      :enable (markdown-table-at-point-p)]
     ["Insert Column" markdown-table-insert-column
      :enable (markdown-table-at-point-p)]
     ["Insert Table" markdown-insert-table]
     "--"
     ["Convert Region to Table" markdown-table-convert-region]
     ["Sort Table Lines" markdown-table-sort-lines
      :enable (markdown-table-at-point-p)]
     ["Transpose Table" markdown-table-transpose
      :enable (markdown-table-at-point-p)])
    ("Lists"
     ["Insert List Item" markdown-insert-list-item]
     ["Move Subtree Up" markdown-move-up
      :keys "C-c <up>"]
     ["Move Subtree Down" markdown-move-down
      :keys "C-c <down>"]
     ["Indent Subtree" markdown-demote
      :keys "C-c <right>"]
     ["Outdent Subtree" markdown-promote
      :keys "C-c <left>"]
     ["Renumber List" markdown-cleanup-list-numbers]
     ["Insert Task List Item" markdown-insert-gfm-checkbox
      :keys "C-c C-x ["]
     ["Toggle Task List Item" markdown-toggle-gfm-checkbox
      :enable (markdown-gfm-task-list-item-at-point)
      :keys "C-c C-d"])
    ("Links & Images"
     ["Insert Link" markdown-insert-link]
     ["Insert Image" markdown-insert-image]
     ["Insert Footnote" markdown-insert-footnote
      :keys "C-c C-s f"]
     ["Insert Wiki Link" markdown-insert-wiki-link
      :keys "C-c C-s w"]
     "---"
     ["Check References" markdown-check-refs]
     ["Find Unused References" markdown-unused-refs]
     ["Toggle URL Hiding" markdown-toggle-url-hiding
      :style radio
      :selected markdown-hide-urls]
     ["Toggle Inline Images" markdown-toggle-inline-images
      :keys "C-c C-x C-i"
      :style radio
      :selected markdown-inline-image-overlays]
     ["Toggle Wiki Links" markdown-toggle-wiki-links
      :style radio
      :selected markdown-enable-wiki-links])
    ("Styles"
     ["Bold" markdown-insert-bold]
     ["Italic" markdown-insert-italic]
     ["Code" markdown-insert-code]
     ["Strikethrough" markdown-insert-strike-through]
     ["Keyboard" markdown-insert-kbd]
     "---"
     ["Blockquote" markdown-insert-blockquote]
     ["Preformatted" markdown-insert-pre]
     ["GFM Code Block" markdown-insert-gfm-code-block]
     ["Edit Code Block" markdown-edit-code-block
      :enable (markdown-code-block-at-point-p)]
     ["Foldable Block" markdown-insert-foldable-block]
     "---"
     ["Blockquote Region" markdown-blockquote-region]
     ["Preformatted Region" markdown-pre-region]
     "---"
     ["Fontify Code Blocks Natively"
      markdown-toggle-fontify-code-blocks-natively
      :style radio
      :selected markdown-fontify-code-blocks-natively]
     ["LaTeX Math Support" markdown-toggle-math
      :style radio
      :selected markdown-enable-math])
    "---"
    ("Preview & Export"
     ["Compile" markdown-other-window]
     ["Preview" markdown-preview]
     ["Export" markdown-export]
     ["Export & View" markdown-export-and-preview]
     ["Open" markdown-open]
     ["Live Export" markdown-live-preview-mode
      :style radio
      :selected markdown-live-preview-mode]
     ["Kill ring save" markdown-kill-ring-save])
    ("Markup Completion and Cycling"
     ["Complete Markup" markdown-complete]
     ["Promote Element" markdown-promote
      :keys "C-c C--"]
     ["Demote Element" markdown-demote
      :keys "C-c C-="])
    "---"
    ["Kill Element" markdown-kill-thing-at-point]
    "---"
    ("Documentation"
     ["Version" markdown-show-version]
     ["Homepage" markdown-mode-info]
     ["Describe Mode" (describe-function 'markdown-mode)]
     ["Guide" (browse-url "https://leanpub.com/markdown-mode")])))


;;; imenu =====================================================================

(defun markdown-imenu-create-nested-index ()
  "Create and return a nested imenu index alist for the current buffer.
See `imenu-create-index-function' and `imenu--index-alist' for details."
  (let* ((root (list nil))
         (min-level 9999)
         hashes headers)
    (save-excursion
      ;; Headings
      (goto-char (point-min))
      (while (re-search-forward markdown-regex-header (point-max) t)
        (unless (or (markdown-code-block-at-point-p)
                    (and (match-beginning 3)
                         (get-text-property (match-beginning 3) 'markdown-yaml-metadata-end)))
          (cond
           ((match-string-no-properties 2) ;; level 1 setext
            (setq min-level 1)
            (push (list :heading (match-string-no-properties 1)
                        :point (match-beginning 1)
                        :level 1) headers))
           ((match-string-no-properties 3) ;; level 2 setext
            (setq min-level (min min-level 2))
            (push (list :heading (match-string-no-properties 1)
                        :point (match-beginning 1)
                        :level (- 2 (1- min-level))) headers))
           ((setq hashes (markdown-trim-whitespace
                          (match-string-no-properties 4)))
            (setq min-level (min min-level (length hashes)))
            (push (list :heading (match-string-no-properties 5)
                        :point (match-beginning 4)
                        :level (- (length hashes) (1- min-level))) headers)))))
      (cl-loop with cur-level = 0
               with cur-alist = nil
               with empty-heading = "-"
               with self-heading = "."
               for header in (reverse headers)
               for level = (plist-get header :level)
               do
               (let ((alist (list (cons (plist-get header :heading) (plist-get header :point)))))
                 (cond
                  ((= cur-level level)  ; new sibling
                   (setcdr cur-alist alist)
                   (setq cur-alist alist))
                  ((< cur-level level)  ; first child
                   (dotimes (_ (- level cur-level 1))
                     (setq alist (list (cons empty-heading alist))))
                   (if cur-alist
                       (let* ((parent (car cur-alist))
                              (self-pos (cdr parent)))
                         (setcdr parent (cons (cons self-heading self-pos) alist)))
                     (setcdr root alist)) ; primogenitor
                   (setq cur-alist alist)
                   (setq cur-level level))
                  (t                    ; new sibling of an ancestor
                   (let ((sibling-alist (last (cdr root))))
                     (dotimes (_ (1- level))
                       (setq sibling-alist (last (cdar sibling-alist))))
                     (setcdr sibling-alist alist)
                     (setq cur-alist alist))
                   (setq cur-level level)))))
      (setq root (copy-tree root))
      ;; Footnotes
      (let ((fn (markdown-get-defined-footnotes)))
        (if (or (zerop (length fn))
                (null markdown-add-footnotes-to-imenu))
            (cdr root)
          (nconc (cdr root) (list (cons "Footnotes" fn))))))))

(defun markdown-imenu-create-flat-index ()
  "Create and return a flat imenu index alist for the current buffer.
See `imenu-create-index-function' and `imenu--index-alist' for details."
  (let* ((empty-heading "-") index heading pos)
    (save-excursion
      ;; Headings
      (goto-char (point-min))
      (while (re-search-forward markdown-regex-header (point-max) t)
        (when (and (not (markdown-code-block-at-point-p (line-beginning-position)))
                   (not (markdown-text-property-at-point 'markdown-yaml-metadata-begin)))
          (cond
           ((setq heading (match-string-no-properties 1))
            (setq pos (match-beginning 1)))
           ((setq heading (match-string-no-properties 5))
            (setq pos (match-beginning 4))))
          (or (> (length heading) 0)
              (setq heading empty-heading))
          (setq index (append index (list (cons heading pos))))))
      ;; Footnotes
      (when markdown-add-footnotes-to-imenu
        (nconc index (markdown-get-defined-footnotes)))
      index)))


;;; References ================================================================

(defun markdown-reference-goto-definition ()
  "Jump to the definition of the reference at point or create it."
  (interactive)
  (when (thing-at-point-looking-at markdown-regex-link-reference)
    (let* ((text (match-string-no-properties 3))
           (reference (match-string-no-properties 6))
           (target (downcase (if (string= reference "") text reference)))
           (loc (cadr (save-match-data (markdown-reference-definition target)))))
      (if loc
          (goto-char loc)
        (goto-char (match-beginning 0))
        (markdown-insert-reference-definition target)))))

(defun markdown-reference-find-links (reference)
  "Return a list of all links for REFERENCE.
REFERENCE should not include the surrounding square brackets.
Elements of the list have the form (text start line), where
text is the link text, start is the location at the beginning of
the link, and line is the line number on which the link appears."
  (let* ((ref-quote (regexp-quote reference))
         (regexp (format "!?\\(?:\\[\\(%s\\)\\][ ]?\\[\\]\\|\\[\\([^]]+?\\)\\][ ]?\\[%s\\]\\)"
                         ref-quote ref-quote))
         links)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward regexp nil t)
        (let* ((text (or (match-string-no-properties 1)
                         (match-string-no-properties 2)))
               (start (match-beginning 0))
               (line (markdown-line-number-at-pos)))
          (cl-pushnew (list text start line) links :test #'equal))))
    links))

(defmacro markdown-for-all-refs (f)
  `(let ((result))
     (save-excursion
       (goto-char (point-min))
       (while
           (re-search-forward markdown-regex-link-reference nil t)
         (let* ((text (match-string-no-properties 3))
                (reference (match-string-no-properties 6))
                (target (downcase (if (string= reference "") text reference))))
           (,f text target result))))
     (reverse result)))

(defmacro markdown-collect-always (_ target result)
  `(cl-pushnew ,target ,result :test #'equal))

(defmacro markdown-collect-undefined (text target result)
  `(unless (markdown-reference-definition target)
     (let ((entry (assoc ,target ,result)))
       (if (not entry)
           (cl-pushnew
            (cons ,target (list (cons ,text (markdown-line-number-at-pos))))
            ,result :test #'equal)
         (setcdr entry
                 (append (cdr entry) (list (cons ,text (markdown-line-number-at-pos)))))))))

(defun markdown-get-all-refs ()
  "Return a list of all Markdown references."
  (markdown-for-all-refs markdown-collect-always))

(defun markdown-get-undefined-refs ()
  "Return a list of undefined Markdown references.
Result is an alist of pairs (reference . occurrences), where
occurrences is itself another alist of pairs (label . line-number).
For example, an alist corresponding to [Nice editor][Emacs] at line 12,
\[GNU Emacs][Emacs] at line 45 and [manual][elisp] at line 127 is
\((\"emacs\" (\"Nice editor\" . 12) (\"GNU Emacs\" . 45)) (\"elisp\" (\"manual\" . 127)))."
  (markdown-for-all-refs markdown-collect-undefined))

(defun markdown-get-unused-refs ()
  (cl-sort
   (cl-set-difference
    (markdown-get-defined-references) (markdown-get-all-refs)
    :test (lambda (e1 e2) (equal (car e1) e2)))
   #'< :key #'cdr))

(defmacro defun-markdown-buffer (name docstring)
  "Define a function to name and return a buffer.

By convention, NAME must be a name of a string constant with
%buffer% placeholder used to name the buffer, and will also be
used as a name of the function defined.

DOCSTRING will be used as the first part of the docstring."
  `(defun ,name (&optional buffer-name)
     ,(concat docstring "\n\nBUFFER-NAME is the name of the main buffer being visited.")
     (or buffer-name (setq buffer-name (buffer-name)))
     (let ((refbuf (get-buffer-create (replace-regexp-in-string
                                       "%buffer%" buffer-name
                                       ,name))))
       (with-current-buffer refbuf
         (when view-mode
           (View-exit-and-edit))
         (use-local-map button-buffer-map)
         (erase-buffer))
       refbuf)))

(defconst markdown-reference-check-buffer
  "*Undefined references for %buffer%*"
  "Pattern for name of buffer for listing undefined references.
The string %buffer% will be replaced by the corresponding
`markdown-mode' buffer name.")

(defun-markdown-buffer
  markdown-reference-check-buffer
  "Name and return buffer for reference checking.")

(defconst markdown-unused-references-buffer
  "*Unused references for %buffer%*"
  "Pattern for name of buffer for listing unused references.
The string %buffer% will be replaced by the corresponding
`markdown-mode' buffer name.")

(defun-markdown-buffer
  markdown-unused-references-buffer
  "Name and return buffer for unused reference checking.")

(defconst markdown-reference-links-buffer
  "*Reference links for %buffer%*"
  "Pattern for name of buffer for listing references.
The string %buffer% will be replaced by the corresponding buffer name.")

(defun-markdown-buffer
  markdown-reference-links-buffer
  "Name, setup, and return a buffer for listing links.")

;; Add an empty Markdown reference definition to buffer
;; specified in the 'target-buffer property.  The reference name is
;; the button's label.
(define-button-type 'markdown-undefined-reference-button
  'help-echo "mouse-1, RET: create definition for undefined reference"
  'follow-link t
  'face 'bold
  'action (lambda (b)
            (let ((buffer (button-get b 'target-buffer))
                  (line (button-get b 'target-line))
                  (label (button-label b)))
              (switch-to-buffer-other-window buffer)
              (goto-char (point-min))
              (forward-line line)
              (markdown-insert-reference-definition label)
              (markdown-check-refs t))))

;; Jump to line in buffer specified by 'target-buffer property.
;; Line number is button's 'target-line property.
(define-button-type 'markdown-goto-line-button
  'help-echo "mouse-1, RET: go to line"
  'follow-link t
  'face 'italic
  'action (lambda (b)
            (switch-to-buffer-other-window (button-get b 'target-buffer))
            ;; use call-interactively to silence compiler
            (let ((current-prefix-arg (button-get b 'target-line)))
              (call-interactively 'goto-line))))

;; Kill a line in buffer specified by 'target-buffer property.
;; Line number is button's 'target-line property.
(define-button-type 'markdown-kill-line-button
  'help-echo "mouse-1, RET: kill line"
  'follow-link t
  'face 'italic
  'action (lambda (b)
            (switch-to-buffer-other-window (button-get b 'target-buffer))
            ;; use call-interactively to silence compiler
            (let ((current-prefix-arg (button-get b 'target-line)))
              (call-interactively 'goto-line))
            (kill-line 1)
            (markdown-unused-refs t)))

;; Jumps to a particular link at location given by 'target-char
;; property in buffer given by 'target-buffer property.
(define-button-type 'markdown-location-button
  'help-echo "mouse-1, RET: jump to location of link"
  'follow-link t
  'face 'bold
  'action (lambda (b)
            (let ((target (button-get b 'target-buffer))
                  (loc (button-get b 'target-char)))
              (kill-buffer-and-window)
              (switch-to-buffer target)
              (goto-char loc))))

(defun markdown-insert-undefined-reference-button (reference oldbuf)
  "Insert a button for creating REFERENCE in buffer OLDBUF.
REFERENCE should be a list of the form (reference . occurrences),
as returned by `markdown-get-undefined-refs'."
  (let ((label (car reference)))
    ;; Create a reference button
    (insert-button label
                   :type 'markdown-undefined-reference-button
                   'target-buffer oldbuf
                   'target-line (cdr (car (cdr reference))))
    (insert " (")
    (dolist (occurrence (cdr reference))
      (let ((line (cdr occurrence)))
        ;; Create a line number button
        (insert-button (number-to-string line)
                       :type 'markdown-goto-line-button
                       'target-buffer oldbuf
                       'target-line line)
        (insert " ")))
    (delete-char -1)
    (insert ")")
    (newline)))

(defun markdown-insert-unused-reference-button (reference oldbuf)
  "Insert a button for creating REFERENCE in buffer OLDBUF.
REFERENCE must be a pair of (ref . line-number)."
  (let ((label (car reference))
        (line (cdr reference)))
    ;; Create a reference button
    (insert-button label
                   :type 'markdown-goto-line-button
                   'face 'bold
                   'target-buffer oldbuf
                   'target-line line)
    (insert (format " (%d) [" line))
    (insert-button "X"
                   :type 'markdown-kill-line-button
                   'face 'bold
                   'target-buffer oldbuf
                   'target-line line)
    (insert "]")
    (newline)))

(defun markdown-insert-link-button (link oldbuf)
  "Insert a button for jumping to LINK in buffer OLDBUF.
LINK should be a list of the form (text char line) containing
the link text, location, and line number."
  (let ((label (cl-first link))
        (char (cl-second link))
        (line (cl-third link)))
    ;; Create a reference button
    (insert-button label
                   :type 'markdown-location-button
                   'target-buffer oldbuf
                   'target-char char)
    (insert (format " (line %d)\n" line))))

(defun markdown-reference-goto-link (&optional reference)
  "Jump to the location of the first use of REFERENCE."
  (interactive)
  (unless reference
    (if (thing-at-point-looking-at markdown-regex-reference-definition)
        (setq reference (match-string-no-properties 2))
      (user-error "No reference definition at point")))
  (let ((links (markdown-reference-find-links reference)))
    (cond ((= (length links) 1)
           (goto-char (cadr (car links))))
          ((> (length links) 1)
           (let ((oldbuf (current-buffer))
                 (linkbuf (markdown-reference-links-buffer)))
             (with-current-buffer linkbuf
               (insert "Links using reference " reference ":\n\n")
               (dolist (link (reverse links))
                 (markdown-insert-link-button link oldbuf)))
             (view-buffer-other-window linkbuf)
             (goto-char (point-min))
             (forward-line 2)))
          (t
           (error "No links for reference %s" reference)))))

(defmacro defun-markdown-ref-checker
    (name docstring checker-function buffer-function none-message buffer-header insert-reference)
  "Define a function NAME acting on result of CHECKER-FUNCTION.

DOCSTRING is used as a docstring for the defined function.

BUFFER-FUNCTION should name and return an auxiliary buffer to put
results in.

NONE-MESSAGE is used when CHECKER-FUNCTION returns no results.

BUFFER-HEADER is put into the auxiliary buffer first, followed by
calling INSERT-REFERENCE for each element in the list returned by
CHECKER-FUNCTION."
  `(defun ,name (&optional silent)
     ,(concat
       docstring
       "\n\nIf SILENT is non-nil, do not message anything when no
such references found.")
     (interactive "P")
     (unless (derived-mode-p 'markdown-mode)
       (user-error "Not available in current mode"))
     (let ((oldbuf (current-buffer))
           (refs (,checker-function))
           (refbuf (,buffer-function)))
       (if (null refs)
           (progn
             (when (not silent)
               (message ,none-message))
             (kill-buffer refbuf))
         (with-current-buffer refbuf
           (insert ,buffer-header)
           (dolist (ref refs)
             (,insert-reference ref oldbuf))
           (view-buffer-other-window refbuf)
           (goto-char (point-min))
           (forward-line 2))))))

(defun-markdown-ref-checker
  markdown-check-refs
  "Show all undefined Markdown references in current `markdown-mode' buffer.

Links which have empty reference definitions are considered to be
defined."
  markdown-get-undefined-refs
  markdown-reference-check-buffer
  "No undefined references found"
  "The following references are undefined:\n\n"
  markdown-insert-undefined-reference-button)


(defun-markdown-ref-checker
  markdown-unused-refs
  "Show all unused Markdown references in current `markdown-mode' buffer."
  markdown-get-unused-refs
  markdown-unused-references-buffer
  "No unused references found"
  "The following references are unused:\n\n"
  markdown-insert-unused-reference-button)



;;; Lists =====================================================================

(defun markdown-insert-list-item (&optional arg)
  "Insert a new list item.
If the point is inside unordered list, insert a bullet mark.  If
the point is inside ordered list, insert the next number followed
by a period.  Use the previous list item to determine the amount
of whitespace to place before and after list markers.

With a \\[universal-argument] prefix (i.e., when ARG is (4)),
decrease the indentation by one level.

With two \\[universal-argument] prefixes (i.e., when ARG is (16)),
increase the indentation by one level."
  (interactive "p")
  (let (bounds cur-indent marker indent new-indent new-loc)
    (save-match-data
      ;; Look for a list item on current or previous non-blank line
      (save-excursion
        (while (and (not (setq bounds (markdown-cur-list-item-bounds)))
                    (not (bobp))
                    (markdown-cur-line-blank-p))
          (forward-line -1)))
      (when bounds
        (cond ((save-excursion
                 (skip-chars-backward " \t")
                 (looking-at-p markdown-regex-list))
               (beginning-of-line)
               (insert "\n")
               (forward-line -1))
              ((not (markdown-cur-line-blank-p))
               (newline)))
        (setq new-loc (point)))
      ;; Look ahead for a list item on next non-blank line
      (unless bounds
        (save-excursion
          (while (and (null bounds)
                      (not (eobp))
                      (markdown-cur-line-blank-p))
            (forward-line)
            (setq bounds (markdown-cur-list-item-bounds))))
        (when bounds
          (setq new-loc (point))
          (unless (markdown-cur-line-blank-p)
            (newline))))
      (if (not bounds)
          ;; When not in a list, start a new unordered one
          (progn
            (unless (markdown-cur-line-blank-p)
              (insert "\n"))
            (insert markdown-unordered-list-item-prefix))
        ;; Compute indentation and marker for new list item
        (setq cur-indent (nth 2 bounds))
        (setq marker (nth 4 bounds))
        ;; If current item is a GFM checkbox, insert new unchecked checkbox.
        (when (nth 5 bounds)
          (setq marker
                (concat marker
                        (replace-regexp-in-string "[Xx]" " " (nth 5 bounds)))))
        (cond
         ;; Dedent: decrement indentation, find previous marker.
         ((= arg 4)
          (setq indent (max (- cur-indent markdown-list-indent-width) 0))
          (let ((prev-bounds
                 (save-excursion
                   (goto-char (nth 0 bounds))
                   (when (markdown-up-list)
                     (markdown-cur-list-item-bounds)))))
            (when prev-bounds
              (setq marker (nth 4 prev-bounds)))))
         ;; Indent: increment indentation by 4, use same marker.
         ((= arg 16) (setq indent (+ cur-indent markdown-list-indent-width)))
         ;; Same level: keep current indentation and marker.
         (t (setq indent cur-indent)))
        (setq new-indent (make-string indent 32))
        (goto-char new-loc)
        (cond
         ;; Ordered list
         ((string-match-p "[0-9]" marker)
          (if (= arg 16) ;; starting a new column indented one more level
              (insert (concat new-indent "1. "))
            ;; Don't use previous match-data
            (set-match-data nil)
            ;; travel up to the last item and pick the correct number.  If
            ;; the argument was nil, "new-indent = cur-indent" is the same,
            ;; so we don't need special treatment. Neat.
            (save-excursion
              (while (and (not (looking-at (concat new-indent "\\([0-9]+\\)\\(\\.[ \t]*\\)")))
                          (>= (forward-line -1) 0))))
            (let* ((old-prefix (match-string 1))
                   (old-spacing (match-string 2))
                   (new-prefix (if (and old-prefix markdown-ordered-list-enumeration)
                                   (int-to-string (1+ (string-to-number old-prefix)))
                                 "1"))
                   (space-adjust (- (length old-prefix) (length new-prefix)))
                   (new-spacing (if (and (match-string 2)
                                         (not (string-match-p "\t" old-spacing))
                                         (< space-adjust 0)
                                         (> space-adjust (- 1 (length (match-string 2)))))
                                    (substring (match-string 2) 0 space-adjust)
                                  (or old-spacing ". "))))
              (insert (concat new-indent new-prefix new-spacing)))))
         ;; Unordered list, GFM task list, or ordered list with hash mark
         ((string-match-p "[\\*\\+-]\\|#\\." marker)
          (insert new-indent marker))))
      ;; Propertize the newly inserted list item now
      (markdown-syntax-propertize-list-items (line-beginning-position) (line-end-position)))))

(defun markdown-move-list-item-up ()
  "Move the current list item up in the list when possible.
In nested lists, move child items with the parent item."
  (interactive)
  (let (cur prev old)
    (when (setq cur (markdown-cur-list-item-bounds))
      (setq old (point))
      (goto-char (nth 0 cur))
      (if (markdown-prev-list-item (nth 3 cur))
          (progn
            (setq prev (markdown-cur-list-item-bounds))
            (condition-case nil
                (progn
                  (transpose-regions (nth 0 prev) (nth 1 prev)
                                     (nth 0 cur) (nth 1 cur) t)
                  (goto-char (+ (nth 0 prev) (- old (nth 0 cur)))))
              ;; Catch error in case regions overlap.
              (error (goto-char old))))
        (goto-char old)))))

(defun markdown-move-list-item-down ()
  "Move the current list item down in the list when possible.
In nested lists, move child items with the parent item."
  (interactive)
  (let (cur next old)
    (when (setq cur (markdown-cur-list-item-bounds))
      (setq old (point))
      (if (markdown-next-list-item (nth 3 cur))
          (progn
            (setq next (markdown-cur-list-item-bounds))
            (condition-case nil
                (progn
                  (transpose-regions (nth 0 cur) (nth 1 cur)
                                     (nth 0 next) (nth 1 next) nil)
                  (goto-char (+ old (- (nth 1 next) (nth 1 cur)))))
              ;; Catch error in case regions overlap.
              (error (goto-char old))))
        (goto-char old)))))

(defun markdown-demote-list-item (&optional bounds)
  "Indent (or demote) the current list item.
Optionally, BOUNDS of the current list item may be provided if available.
In nested lists, demote child items as well."
  (interactive)
  (when (or bounds (setq bounds (markdown-cur-list-item-bounds)))
    (save-excursion
      (let* ((item-start (set-marker (make-marker) (nth 0 bounds)))
             (item-end (set-marker (make-marker) (nth 1 bounds)))
             (list-start (progn (markdown-beginning-of-list)
                                (set-marker (make-marker) (point))))
             (list-end (progn (markdown-end-of-list)
                              (set-marker (make-marker) (point)))))
        (goto-char item-start)
        (while (< (point) item-end)
          (unless (markdown-cur-line-blank-p)
            (insert (make-string markdown-list-indent-width ? )))
          (forward-line))
        (markdown-syntax-propertize-list-items list-start list-end)))))

(defun markdown-promote-list-item (&optional bounds)
  "Unindent (or promote) the current list item.
Optionally, BOUNDS of the current list item may be provided if available.
In nested lists, demote child items as well."
  (interactive)
  (when (or bounds (setq bounds (markdown-cur-list-item-bounds)))
    (save-excursion
      (save-match-data
        (let ((item-start (set-marker (make-marker) (nth 0 bounds)))
              (item-end (set-marker (make-marker) (nth 1 bounds)))
              (list-start (progn (markdown-beginning-of-list)
                                 (set-marker (make-marker) (point))))
              (list-end (progn (markdown-end-of-list)
                               (set-marker (make-marker) (point))))
              num regexp)
          (goto-char item-start)
          (when (looking-at (format "^[ ]\\{1,%d\\}"
                                    markdown-list-indent-width))
            (setq num (- (match-end 0) (match-beginning 0)))
            (setq regexp (format "^[ ]\\{1,%d\\}" num))
            (while (and (< (point) item-end)
                        (re-search-forward regexp item-end t))
              (replace-match "" nil nil)
              (forward-line))
            (markdown-syntax-propertize-list-items list-start list-end)))))))

(defun markdown-cleanup-list-numbers-level (&optional pfx prev-item)
  "Update the numbering for level PFX (as a string of spaces) and PREV-ITEM.
PREV-ITEM is width of previous-indentation and list number

Assume that the previously found match was for a numbered item in
a list."
  (let ((cpfx pfx)
        (cur-item nil)
        (idx 0)
        (continue t)
        (step t)
        (sep nil))
    (while (and continue (not (eobp)))
      (setq step t)
      (cond
       ((looking-at "^\\(\\([\s-]*\\)[0-9]+\\)\\. ")
        (setq cpfx (match-string-no-properties 2))
        (setq cur-item (match-string-no-properties 1)) ;; indentation and list marker
        (cond
         ((or (= (length cpfx) (length pfx))
              (= (length cur-item) (length prev-item)))
          (save-excursion
            (replace-match
             (if (not markdown-ordered-list-enumeration)
                 (concat pfx "1. ")
               (cl-incf idx)
               (concat pfx (number-to-string idx) ". "))))
          (setq sep nil))
         ;; indented a level
         ((< (length pfx) (length cpfx))
          (setq sep (markdown-cleanup-list-numbers-level cpfx cur-item))
          (setq step nil))
         ;; exit the loop
         (t
          (setq step nil)
          (setq continue nil))))

       ((looking-at "^\\([\s-]*\\)[^ \t\n\r].*$")
        (setq cpfx (match-string-no-properties 1))
        (cond
         ;; reset if separated before
         ((string= cpfx pfx) (when sep (setq idx 0)))
         ((string< cpfx pfx)
          (setq step nil)
          (setq continue nil))))
       (t (setq sep t)))

      (when step
        (beginning-of-line)
        (setq continue (= (forward-line) 0))))
    sep))

(defun markdown-cleanup-list-numbers ()
  "Update the numbering of ordered lists."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (markdown-cleanup-list-numbers-level "")))


;;; Movement ==================================================================

(defun markdown-beginning-of-defun (&optional arg)
  "`beginning-of-defun-function' for Markdown.
This is used to find the beginning of the defun and should behave
like ‘beginning-of-defun’, returning non-nil if it found the
beginning of a defun.  It moves the point backward, right before a
heading which defines a defun.  When ARG is non-nil, repeat that
many times.  When ARG is negative, move forward to the ARG-th
following section."
  (or arg (setq arg 1))
  (when (< arg 0) (end-of-line))
  ;; Adjust position for setext headings.
  (when (and (thing-at-point-looking-at markdown-regex-header-setext)
             (not (= (point) (match-beginning 0)))
             (not (markdown-code-block-at-point-p)))
    (goto-char (match-end 0)))
  (let (found)
    ;; Move backward with positive argument.
    (while (and (not (bobp)) (> arg 0))
      (setq found nil)
      (while (and (not found)
                  (not (bobp))
                  (re-search-backward markdown-regex-header nil 'move))
        (markdown-code-block-at-pos (match-beginning 0))
        (setq found (match-beginning 0)))
      (setq arg (1- arg)))
    ;; Move forward with negative argument.
    (while (and (not (eobp)) (< arg 0))
      (setq found nil)
      (while (and (not found)
                  (not (eobp))
                  (re-search-forward markdown-regex-header nil 'move))
        (markdown-code-block-at-pos (match-beginning 0))
        (setq found (match-beginning 0)))
      (setq arg (1+ arg)))
    (when found
      (beginning-of-line)
      t)))

(defun markdown-end-of-defun ()
  "`end-of-defun-function’ for Markdown.
This is used to find the end of the defun at point.
It is called with no argument, right after calling ‘beginning-of-defun-raw’,
so it can assume that point is at the beginning of the defun body.
It should move point to the first position after the defun."
  (or (eobp) (forward-char 1))
  (let (found)
    (while (and (not found)
                (not (eobp))
                (re-search-forward markdown-regex-header nil 'move))
      (when (not (markdown-code-block-at-pos (match-beginning 0)))
        (setq found (match-beginning 0))))
    (when found
      (goto-char found)
      (skip-syntax-backward "-"))))

(defun markdown-beginning-of-text-block ()
  "Move backward to previous beginning of a plain text block.
This function simply looks for blank lines without considering
the surrounding context in light of Markdown syntax.  For that, see
`markdown-backward-block'."
  (interactive)
  (let ((start (point)))
    (if (re-search-backward markdown-regex-block-separator nil t)
        (goto-char (match-end 0))
      (goto-char (point-min)))
    (when (and (= start (point)) (not (bobp)))
      (forward-line -1)
      (if (re-search-backward markdown-regex-block-separator nil t)
          (goto-char (match-end 0))
        (goto-char (point-min))))))

(defun markdown-end-of-text-block ()
  "Move forward to next beginning of a plain text block.
This function simply looks for blank lines without considering
the surrounding context in light of Markdown syntax.  For that, see
`markdown-forward-block'."
  (interactive)
  (beginning-of-line)
  (skip-chars-forward " \t\n")
  (when (= (point) (point-min))
    (forward-char))
  (if (re-search-forward markdown-regex-block-separator nil t)
      (goto-char (match-end 0))
    (goto-char (point-max)))
  (skip-chars-backward " \t\n")
  (forward-line))

(defun markdown-backward-paragraph (&optional arg)
  "Move the point to the start of the current paragraph.
With argument ARG, do it ARG times; a negative argument ARG = -N
means move forward N blocks."
  (interactive "^p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (markdown-forward-paragraph (- arg))
    (dotimes (_ arg)
      ;; Skip over whitespace in between paragraphs when moving backward.
      (skip-chars-backward " \t\n")
      (beginning-of-line)
      ;; Skip over code block endings.
      (when (markdown-range-properties-exist
             (line-beginning-position) (line-end-position)
             '(markdown-gfm-block-end
               markdown-tilde-fence-end))
        (forward-line -1))
      ;; Skip over blank lines inside blockquotes.
      (while (and (not (eobp))
                  (looking-at markdown-regex-blockquote)
                  (= (length (match-string 3)) 0))
        (forward-line -1))
      ;; Proceed forward based on the type of block of paragraph.
      (let (bounds skip)
        (cond
         ;; Blockquotes
         ((looking-at markdown-regex-blockquote)
          (while (and (not (bobp))
                      (looking-at markdown-regex-blockquote)
                      (> (length (match-string 3)) 0)) ;; not blank
            (forward-line -1))
          (forward-line))
         ;; List items
         ((setq bounds (markdown-cur-list-item-bounds))
          (goto-char (nth 0 bounds)))
         ;; Other
         (t
          (while (and (not (bobp))
                      (not skip)
                      (not (markdown-cur-line-blank-p))
                      (not (looking-at markdown-regex-blockquote))
                      (not (markdown-range-properties-exist
                            (line-beginning-position) (line-end-position)
                            '(markdown-gfm-block-end
                              markdown-tilde-fence-end))))
            (setq skip (markdown-range-properties-exist
                        (line-beginning-position) (line-end-position)
                        '(markdown-gfm-block-begin
                          markdown-tilde-fence-begin)))
            (forward-line -1))
          (unless (bobp)
            (forward-line 1))))))))

(defun markdown-forward-paragraph (&optional arg)
  "Move forward to the next end of a paragraph.
With argument ARG, do it ARG times; a negative argument ARG = -N
means move backward N blocks."
  (interactive "^p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (markdown-backward-paragraph (- arg))
    (dotimes (_ arg)
      ;; Skip whitespace in between paragraphs.
      (when (markdown-cur-line-blank-p)
        (skip-syntax-forward "-")
        (beginning-of-line))
      ;; Proceed forward based on the type of block.
      (let (bounds skip)
        (cond
         ;; Blockquotes
         ((looking-at markdown-regex-blockquote)
          ;; Skip over blank lines inside blockquotes.
          (while (and (not (eobp))
                      (looking-at markdown-regex-blockquote)
                      (= (length (match-string 3)) 0))
            (forward-line))
          ;; Move to end of quoted text block
          (while (and (not (eobp))
                      (looking-at markdown-regex-blockquote)
                      (> (length (match-string 3)) 0)) ;; not blank
            (forward-line)))
         ;; List items
         ((and (markdown-cur-list-item-bounds)
               (setq bounds (markdown-next-list-item-bounds)))
          (goto-char (nth 0 bounds)))
         ;; Other
         (t
          (forward-line)
          (while (and (not (eobp))
                      (not skip)
                      (not (markdown-cur-line-blank-p))
                      (not (looking-at markdown-regex-blockquote))
                      (not (markdown-range-properties-exist
                            (line-beginning-position) (line-end-position)
                            '(markdown-gfm-block-begin
                              markdown-tilde-fence-begin))))
            (setq skip (markdown-range-properties-exist
                        (line-beginning-position) (line-end-position)
                        '(markdown-gfm-block-end
                          markdown-tilde-fence-end)))
            (forward-line))))))))

(defun markdown-backward-block (&optional arg)
  "Move the point to the start of the current Markdown block.
Moves across complete code blocks, list items, and blockquotes,
but otherwise stops at blank lines, headers, and horizontal
rules.  With argument ARG, do it ARG times; a negative argument
ARG = -N means move forward N blocks."
  (interactive "^p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (markdown-forward-block (- arg))
    (dotimes (_ arg)
      ;; Skip over whitespace in between blocks when moving backward,
      ;; unless at a block boundary with no whitespace.
      (skip-syntax-backward "-")
      (beginning-of-line)
      ;; Proceed forward based on the type of block.
      (cond
       ;; Code blocks
       ((and (markdown-code-block-at-pos (point)) ;; this line
             (markdown-code-block-at-pos (line-beginning-position 0))) ;; previous line
        (forward-line -1)
        (while (and (markdown-code-block-at-point-p) (not (bobp)))
          (forward-line -1))
        (forward-line))
       ;; Headings
       ((markdown-heading-at-point)
        (goto-char (match-beginning 0)))
       ;; Horizontal rules
       ((looking-at markdown-regex-hr))
       ;; Blockquotes
       ((looking-at markdown-regex-blockquote)
        (forward-line -1)
        (while (and (looking-at markdown-regex-blockquote)
                    (not (bobp)))
          (forward-line -1))
        (forward-line))
       ;; List items
       ((markdown-cur-list-item-bounds)
        (markdown-beginning-of-list))
       ;; Other
       (t
        ;; Move forward in case it is a one line regular paragraph.
        (unless (markdown-next-line-blank-p)
          (forward-line))
        (unless (markdown-prev-line-blank-p)
          (markdown-backward-paragraph)))))))

(defun markdown-forward-block (&optional arg)
  "Move forward to the next end of a Markdown block.
Moves across complete code blocks, list items, and blockquotes,
but otherwise stops at blank lines, headers, and horizontal
rules.  With argument ARG, do it ARG times; a negative argument
ARG = -N means move backward N blocks."
  (interactive "^p")
  (or arg (setq arg 1))
  (if (< arg 0)
      (markdown-backward-block (- arg))
    (dotimes (_ arg)
      ;; Skip over whitespace in between blocks when moving forward.
      (if (markdown-cur-line-blank-p)
          (skip-syntax-forward "-")
        (beginning-of-line))
      ;; Proceed forward based on the type of block.
      (cond
       ;; Code blocks
       ((markdown-code-block-at-point-p)
        (forward-line)
        (while (and (markdown-code-block-at-point-p) (not (eobp)))
          (forward-line)))
       ;; Headings
       ((looking-at markdown-regex-header)
        (goto-char (or (match-end 4) (match-end 2) (match-end 3)))
        (forward-line))
       ;; Horizontal rules
       ((looking-at markdown-regex-hr)
        (forward-line))
       ;; Blockquotes
       ((looking-at markdown-regex-blockquote)
        (forward-line)
        (while (and (looking-at markdown-regex-blockquote) (not (eobp)))
          (forward-line)))
       ;; List items
       ((markdown-cur-list-item-bounds)
        (markdown-end-of-list)
        (forward-line))
       ;; Other
       (t (markdown-forward-paragraph))))
    (skip-syntax-backward "-")
    (unless (eobp)
      (forward-char 1))))

(defun markdown-backward-page (&optional count)
  "Move backward to boundary of the current toplevel section.
With COUNT, repeat, or go forward if negative."
  (interactive "p")
  (or count (setq count 1))
  (if (< count 0)
      (markdown-forward-page (- count))
    (skip-syntax-backward "-")
    (or (markdown-back-to-heading-over-code-block t t)
        (goto-char (point-min)))
    (when (looking-at markdown-regex-header)
      (let ((level (markdown-outline-level)))
        (when (> level 1) (markdown-up-heading level))
        (when (> count 1)
          (condition-case nil
              (markdown-backward-same-level (1- count))
            (error (goto-char (point-min)))))))))

(defun markdown-forward-page (&optional count)
  "Move forward to boundary of the current toplevel section.
With COUNT, repeat, or go backward if negative."
  (interactive "p")
  (or count (setq count 1))
  (if (< count 0)
      (markdown-backward-page (- count))
    (if (markdown-back-to-heading-over-code-block t t)
        (let ((level (markdown-outline-level)))
          (when (> level 1) (markdown-up-heading level))
          (condition-case nil
              (markdown-forward-same-level count)
            (error (goto-char (point-max)))))
      (markdown-next-visible-heading 1))))

(defun markdown-next-link ()
  "Jump to next inline, reference, or wiki link.
If successful, return point.  Otherwise, return nil.
See `markdown-wiki-link-p' and `markdown-previous-wiki-link'."
  (interactive)
  (let ((opoint (point)))
    (when (or (markdown-link-p) (markdown-wiki-link-p))
      ;; At a link already, move past it.
      (goto-char (+ (match-end 0) 1)))
    ;; Search for the next wiki link and move to the beginning.
    (while (and (re-search-forward (markdown-make-regex-link-generic) nil t)
                (markdown-code-block-at-point-p)
                (< (point) (point-max))))
    (if (and (not (eq (point) opoint))
             (or (markdown-link-p) (markdown-wiki-link-p)))
        ;; Group 1 will move past non-escape character in wiki link regexp.
        ;; Go to beginning of group zero for all other link types.
        (goto-char (or (match-beginning 1) (match-beginning 0)))
      (goto-char opoint)
      nil)))

(defun markdown-previous-link ()
  "Jump to previous wiki link.
If successful, return point.  Otherwise, return nil.
See `markdown-wiki-link-p' and `markdown-next-wiki-link'."
  (interactive)
  (let ((opoint (point)))
    (while (and (re-search-backward (markdown-make-regex-link-generic) nil t)
                (markdown-code-block-at-point-p)
                (> (point) (point-min))))
    (if (and (not (eq (point) opoint))
             (or (markdown-link-p) (markdown-wiki-link-p)))
        (goto-char (or (match-beginning 1) (match-beginning 0)))
      (goto-char opoint)
      nil)))


;;; Outline ===================================================================

(defun markdown-move-heading-common (move-fn &optional arg adjust)
  "Wrapper for `outline-mode' functions to skip false positives.
MOVE-FN is a function and ARG is its argument. For example,
headings inside preformatted code blocks may match
`outline-regexp' but should not be considered as headings.
When ADJUST is non-nil, adjust the point for interactive calls
to avoid leaving the point at invisible markup.  This adjustment
generally should only be done for interactive calls, since other
functions may expect the point to be at the beginning of the
regular expression."
  (let ((prev -1) (start (point)))
    (if arg (funcall move-fn arg) (funcall move-fn))
    (while (and (/= prev (point)) (markdown-code-block-at-point-p))
      (setq prev (point))
      (if arg (funcall move-fn arg) (funcall move-fn)))
    ;; Adjust point for setext headings and invisible text.
    (save-match-data
      (when (and adjust (thing-at-point-looking-at markdown-regex-header))
        (if markdown-hide-markup
            ;; Move to beginning of heading text if markup is hidden.
            (goto-char (or (match-beginning 1) (match-beginning 5)))
          ;; Move to beginning of markup otherwise.
          (goto-char (or (match-beginning 1) (match-beginning 4))))))
    (if (= (point) start) nil (point))))

(defun markdown-next-visible-heading (arg)
  "Move to the next visible heading line of any level.
With argument, repeats or can move backward if negative. ARG is
passed to `outline-next-visible-heading'."
  (interactive "p")
  (markdown-move-heading-common #'outline-next-visible-heading arg 'adjust))

(defun markdown-previous-visible-heading (arg)
  "Move to the previous visible heading line of any level.
With argument, repeats or can move backward if negative. ARG is
passed to `outline-previous-visible-heading'."
  (interactive "p")
  (markdown-move-heading-common #'outline-previous-visible-heading arg 'adjust))

(defun markdown-next-heading ()
  "Move to the next heading line of any level."
  (markdown-move-heading-common #'outline-next-heading))

(defun markdown-previous-heading ()
  "Move to the previous heading line of any level."
  (markdown-move-heading-common #'outline-previous-heading))

(defun markdown-back-to-heading-over-code-block (&optional invisible-ok no-error)
  "Move back to the beginning of the previous heading.
Returns t if the point is at a heading, the location if a heading
was found, and nil otherwise.
Only visible heading lines are considered, unless INVISIBLE-OK is
non-nil.  Throw an error if there is no previous heading unless
NO-ERROR is non-nil.
Leaves match data intact for `markdown-regex-header'."
  (beginning-of-line)
  (or (and (markdown-heading-at-point)
           (not (markdown-code-block-at-point-p)))
      (let (found)
        (save-excursion
          (while (and (not found)
                      (re-search-backward markdown-regex-header nil t))
            (when (and (or invisible-ok (not (outline-invisible-p)))
                       (not (markdown-code-block-at-point-p)))
              (setq found (point))))
          (if (not found)
              (unless no-error (user-error "Before first heading"))
            (setq found (point))))
        (when found (goto-char found)))))

(defun markdown-forward-same-level (arg)
  "Move forward to the ARG'th heading at same level as this one.
Stop at the first and last headings of a superior heading."
  (interactive "p")
  (markdown-back-to-heading-over-code-block)
  (markdown-move-heading-common #'outline-forward-same-level arg 'adjust))

(defun markdown-backward-same-level (arg)
  "Move backward to the ARG'th heading at same level as this one.
Stop at the first and last headings of a superior heading."
  (interactive "p")
  (markdown-back-to-heading-over-code-block)
  (while (> arg 0)
    (let ((point-to-move-to
           (save-excursion
             (markdown-move-heading-common #'outline-get-last-sibling nil 'adjust))))
      (if point-to-move-to
          (progn
            (goto-char point-to-move-to)
            (setq arg (1- arg)))
        (user-error "No previous same-level heading")))))

(defun markdown-up-heading (arg &optional interactive)
  "Move to the visible heading line of which the present line is a subheading.
With argument, move up ARG levels.  When called interactively (or
INTERACTIVE is non-nil), also push the mark."
  (interactive "p\np")
  (and interactive (not (eq last-command 'markdown-up-heading))
       (push-mark))
  (markdown-move-heading-common #'outline-up-heading arg 'adjust))

(defun markdown-back-to-heading (&optional invisible-ok)
  "Move to previous heading line, or beg of this line if it's a heading.
Only visible heading lines are considered, unless INVISIBLE-OK is non-nil."
  (interactive)
  (markdown-move-heading-common #'outline-back-to-heading invisible-ok))

(defalias 'markdown-end-of-heading 'outline-end-of-heading)

(defun markdown-on-heading-p ()
  "Return non-nil if point is on a heading line."
  (get-text-property (line-beginning-position) 'markdown-heading))

(defun markdown-end-of-subtree (&optional invisible-OK)
  "Move to the end of the current subtree.
Only visible heading lines are considered, unless INVISIBLE-OK is
non-nil.
Derived from `org-end-of-subtree'."
  (markdown-back-to-heading invisible-OK)
  (let ((first t)
        (level (markdown-outline-level)))
    (while (and (not (eobp))
                (or first (> (markdown-outline-level) level)))
      (setq first nil)
      (markdown-next-heading))
    (if (memq (preceding-char) '(?\n ?\^M))
        (progn
          ;; Go to end of line before heading
          (forward-char -1)
          (if (memq (preceding-char) '(?\n ?\^M))
              ;; leave blank line before heading
              (forward-char -1)))))
  (point))

(defun markdown-outline-fix-visibility ()
  "Hide any false positive headings that should not be shown.
For example, headings inside preformatted code blocks may match
`outline-regexp' but should not be shown as headings when cycling.
Also, the ending --- line in metadata blocks appears to be a
setext header, but should not be folded."
  (save-excursion
    (goto-char (point-min))
    ;; Unhide any false positives in metadata blocks
    (when (markdown-text-property-at-point 'markdown-yaml-metadata-begin)
      (let ((body (progn (forward-line)
                         (markdown-text-property-at-point
                          'markdown-yaml-metadata-section))))
        (when body
          (let ((end (progn (goto-char (cl-second body))
                            (markdown-text-property-at-point
                             'markdown-yaml-metadata-end))))
            (outline-flag-region (point-min) (1+ (cl-second end)) nil)))))
    ;; Hide any false positives in code blocks
    (unless (outline-on-heading-p)
      (outline-next-visible-heading 1))
    (while (< (point) (point-max))
      (when (markdown-code-block-at-point-p)
        (outline-flag-region (1- (line-beginning-position)) (line-end-position) t))
      (outline-next-visible-heading 1))))

(defvar markdown-cycle-global-status 1)
(defvar markdown-cycle-subtree-status nil)

(defun markdown-next-preface ()
  (let (finish)
    (while (and (not finish) (re-search-forward (concat "\n\\(?:" outline-regexp "\\)")
                                                nil 'move))
      (unless (markdown-code-block-at-point-p)
        (goto-char (match-beginning 0))
        (setq finish t))))
  (when (and (bolp) (or outline-blank-line (eobp)) (not (bobp)))
    (forward-char -1)))

(defun markdown-show-entry ()
  (save-excursion
    (outline-back-to-heading t)
    (outline-flag-region (1- (point))
                         (progn
                           (markdown-next-preface)
                           (if (= 1 (- (point-max) (point)))
                               (point-max)
                             (point)))
                         nil)))

;; This function was originally derived from `org-cycle' from org.el.
(defun markdown-cycle (&optional arg)
  "Visibility cycling for Markdown mode.
This function is called with a `\\[universal-argument]' or if ARG is t, perform
global visibility cycling.  If the point is at an atx-style header, cycle
visibility of the corresponding subtree.  Otherwise, indent the current line
 or insert a tab, as appropriate, by calling `indent-for-tab-command'."
  (interactive "P")
  (cond

   ;; Global cycling
   (arg
    (cond
     ;; Move from overview to contents
     ((and (eq last-command this-command)
           (eq markdown-cycle-global-status 2))
      (outline-hide-sublevels 1)
      (message "CONTENTS")
      (setq markdown-cycle-global-status 3)
      (markdown-outline-fix-visibility))
     ;; Move from contents to all
     ((and (eq last-command this-command)
           (eq markdown-cycle-global-status 3))
      (outline-show-all)
      (message "SHOW ALL")
      (setq markdown-cycle-global-status 1))
     ;; Defaults to overview
     (t
      (outline-hide-body)
      (message "OVERVIEW")
      (setq markdown-cycle-global-status 2)
      (markdown-outline-fix-visibility))))

   ;; At a heading: rotate between three different views
   ((save-excursion (beginning-of-line 1) (markdown-on-heading-p))
    (markdown-back-to-heading)
    (let ((goal-column 0) eoh eol eos)
      ;; Determine boundaries
      (save-excursion
        (markdown-back-to-heading)
        (save-excursion
          (beginning-of-line 2)
          (while (and (not (eobp)) ;; this is like `next-line'
                      (get-char-property (1- (point)) 'invisible))
            (beginning-of-line 2)) (setq eol (point)))
        (markdown-end-of-heading)   (setq eoh (point))
        (markdown-end-of-subtree t)
        (skip-chars-forward " \t\n")
        (beginning-of-line 1) ; in case this is an item
        (setq eos (1- (point))))
      ;; Find out what to do next and set `this-command'
      (cond
       ;; Nothing is hidden behind this heading
       ((= eos eoh)
        (message "EMPTY ENTRY")
        (setq markdown-cycle-subtree-status nil))
       ;; Entire subtree is hidden in one line: open it
       ((>= eol eos)
        (markdown-show-entry)
        (outline-show-children)
        (message "CHILDREN")
        (setq markdown-cycle-subtree-status 'children))
       ;; We just showed the children, now show everything.
       ((and (eq last-command this-command)
             (eq markdown-cycle-subtree-status 'children))
        (outline-show-subtree)
        (message "SUBTREE")
        (setq markdown-cycle-subtree-status 'subtree))
       ;; Default action: hide the subtree.
       (t
        (outline-hide-subtree)
        (message "FOLDED")
        (setq markdown-cycle-subtree-status 'folded)))))

   ;; In a table, move forward by one cell
   ((markdown-table-at-point-p)
    (call-interactively #'markdown-table-forward-cell))

   ;; Otherwise, indent as appropriate
   (t
    (indent-for-tab-command))))

(defun markdown-shifttab ()
  "Handle S-TAB keybinding based on context.
When in a table, move backward one cell.
Otherwise, cycle global heading visibility by calling
`markdown-cycle' with argument t."
  (interactive)
  (cond ((markdown-table-at-point-p)
         (call-interactively #'markdown-table-backward-cell))
        (t (markdown-cycle t))))

(defun markdown-outline-level ()
  "Return the depth to which a statement is nested in the outline."
  (cond
   ((and (match-beginning 0)
         (markdown-code-block-at-pos (match-beginning 0)))
    7) ;; Only 6 header levels are defined.
   ((match-end 2) 1)
   ((match-end 3) 2)
   ((match-end 4)
    (length (markdown-trim-whitespace (match-string-no-properties 4))))))

(defun markdown-promote-subtree (&optional arg)
  "Promote the current subtree of ATX headings.
Note that Markdown does not support heading levels higher than
six and therefore level-six headings will not be promoted
further. If ARG is non-nil promote the heading, otherwise
demote."
  (interactive "*P")
  (save-excursion
    (when (and (or (thing-at-point-looking-at markdown-regex-header-atx)
                   (re-search-backward markdown-regex-header-atx nil t))
               (not (markdown-code-block-at-point-p)))
      (let ((level (length (match-string 1)))
            (promote-or-demote (if arg 1 -1))
            (remove 't))
        (markdown-cycle-atx promote-or-demote remove)
        (catch 'end-of-subtree
          (while (and (markdown-next-heading)
                      (looking-at markdown-regex-header-atx))
            ;; Exit if this not a higher level heading; promote otherwise.
            (if (and (looking-at markdown-regex-header-atx)
                     (<= (length (match-string-no-properties 1)) level))
                (throw 'end-of-subtree nil)
              (markdown-cycle-atx promote-or-demote remove))))))))

(defun markdown-demote-subtree ()
  "Demote the current subtree of ATX headings."
  (interactive)
  (markdown-promote-subtree t))

(defun markdown-move-subtree-up ()
  "Move the current subtree of ATX headings up."
  (interactive)
  (outline-move-subtree-up 1))

(defun markdown-move-subtree-down ()
  "Move the current subtree of ATX headings down."
  (interactive)
  (outline-move-subtree-down 1))

(defun markdown-outline-next ()
  "Move to next list item, when in a list, or next visible heading."
  (interactive)
  (let ((bounds (markdown-next-list-item-bounds)))
    (if bounds
        (goto-char (nth 0 bounds))
      (markdown-next-visible-heading 1))))

(defun markdown-outline-previous ()
  "Move to previous list item, when in a list, or previous visible heading."
  (interactive)
  (let ((bounds (markdown-prev-list-item-bounds)))
    (if bounds
        (goto-char (nth 0 bounds))
      (markdown-previous-visible-heading 1))))

(defun markdown-outline-next-same-level ()
  "Move to next list item or heading of same level."
  (interactive)
  (let ((bounds (markdown-cur-list-item-bounds)))
    (if bounds
        (markdown-next-list-item (nth 3 bounds))
      (markdown-forward-same-level 1))))

(defun markdown-outline-previous-same-level ()
  "Move to previous list item or heading of same level."
  (interactive)
  (let ((bounds (markdown-cur-list-item-bounds)))
    (if bounds
        (markdown-prev-list-item (nth 3 bounds))
      (markdown-backward-same-level 1))))

(defun markdown-outline-up ()
  "Move to previous list item, when in a list, or previous heading."
  (interactive)
  (unless (markdown-up-list)
    (markdown-up-heading 1)))


;;; Marking and Narrowing =====================================================

(defun markdown-mark-paragraph ()
  "Put mark at end of this block, point at beginning.
The block marked is the one that contains point or follows point.

Interactively, if this command is repeated or (in Transient Mark
mode) if the mark is active, it marks the next block after the
ones already marked."
  (interactive)
  (if (or (and (eq last-command this-command) (mark t))
          (and transient-mark-mode mark-active))
      (set-mark
       (save-excursion
         (goto-char (mark))
         (markdown-forward-paragraph)
         (point)))
    (let ((beginning-of-defun-function #'markdown-backward-paragraph)
          (end-of-defun-function #'markdown-forward-paragraph))
      (mark-defun))))

(defun markdown-mark-block ()
  "Put mark at end of this block, point at beginning.
The block marked is the one that contains point or follows point.

Interactively, if this command is repeated or (in Transient Mark
mode) if the mark is active, it marks the next block after the
ones already marked."
  (interactive)
  (if (or (and (eq last-command this-command) (mark t))
          (and transient-mark-mode mark-active))
      (set-mark
       (save-excursion
         (goto-char (mark))
         (markdown-forward-block)
         (point)))
    (let ((beginning-of-defun-function #'markdown-backward-block)
          (end-of-defun-function #'markdown-forward-block))
      (mark-defun))))

(defun markdown-narrow-to-block ()
  "Make text outside current block invisible.
The current block is the one that contains point or follows point."
  (interactive)
  (let ((beginning-of-defun-function #'markdown-backward-block)
        (end-of-defun-function #'markdown-forward-block))
    (narrow-to-defun)))

(defun markdown-mark-text-block ()
  "Put mark at end of this plain text block, point at beginning.
The block marked is the one that contains point or follows point.

Interactively, if this command is repeated or (in Transient Mark
mode) if the mark is active, it marks the next block after the
ones already marked."
  (interactive)
  (if (or (and (eq last-command this-command) (mark t))
          (and transient-mark-mode mark-active))
      (set-mark
       (save-excursion
         (goto-char (mark))
         (markdown-end-of-text-block)
         (point)))
    (let ((beginning-of-defun-function #'markdown-beginning-of-text-block)
          (end-of-defun-function #'markdown-end-of-text-block))
      (mark-defun))))

(defun markdown-mark-page ()
  "Put mark at end of this top level section, point at beginning.
The top level section marked is the one that contains point or
follows point.

Interactively, if this command is repeated or (in Transient Mark
mode) if the mark is active, it marks the next page after the
ones already marked."
  (interactive)
  (if (or (and (eq last-command this-command) (mark t))
          (and transient-mark-mode mark-active))
      (set-mark
       (save-excursion
         (goto-char (mark))
         (markdown-forward-page)
         (point)))
    (let ((beginning-of-defun-function #'markdown-backward-page)
          (end-of-defun-function #'markdown-forward-page))
      (mark-defun))))

(defun markdown-narrow-to-page ()
  "Make text outside current top level section invisible.
The current section is the one that contains point or follows point."
  (interactive)
  (let ((beginning-of-defun-function #'markdown-backward-page)
        (end-of-defun-function #'markdown-forward-page))
    (narrow-to-defun)))

(defun markdown-mark-subtree ()
  "Mark the current subtree.
This puts point at the start of the current subtree, and mark at the end."
  (interactive)
  (let ((beg))
    (if (markdown-heading-at-point)
        (beginning-of-line)
      (markdown-previous-visible-heading 1))
    (setq beg (point))
    (markdown-end-of-subtree)
    (push-mark (point) nil t)
    (goto-char beg)))

(defun markdown-narrow-to-subtree ()
  "Narrow buffer to the current subtree."
  (interactive)
  (save-excursion
    (save-match-data
      (narrow-to-region
       (progn (markdown-back-to-heading-over-code-block t) (point))
       (progn (markdown-end-of-subtree)
              (if (and (markdown-heading-at-point) (not (eobp)))
                  (backward-char 1))
              (point))))))


;;; Generic Structure Editing, Completion, and Cycling Commands ===============

(defun markdown-move-up ()
  "Move thing at point up.
When in a list item, call `markdown-move-list-item-up'.
When in a table, call `markdown-table-move-row-up'.
Otherwise, move the current heading subtree up with
`markdown-move-subtree-up'."
  (interactive)
  (cond
   ((markdown-list-item-at-point-p)
    (call-interactively #'markdown-move-list-item-up))
   ((markdown-table-at-point-p)
    (call-interactively #'markdown-table-move-row-up))
   (t
    (call-interactively #'markdown-move-subtree-up))))

(defun markdown-move-down ()
  "Move thing at point down.
When in a list item, call `markdown-move-list-item-down'.
Otherwise, move the current heading subtree up with
`markdown-move-subtree-down'."
  (interactive)
  (cond
   ((markdown-list-item-at-point-p)
    (call-interactively #'markdown-move-list-item-down))
   ((markdown-table-at-point-p)
    (call-interactively #'markdown-table-move-row-down))
   (t
    (call-interactively #'markdown-move-subtree-down))))

(defun markdown-promote ()
  "Promote or move element at point to the left.
Depending on the context, this function will promote a heading or
list item at the point, move a table column to the left, or cycle
markup."
  (interactive)
  (let (bounds)
    (cond
     ;; Promote atx heading subtree
     ((thing-at-point-looking-at markdown-regex-header-atx)
      (markdown-promote-subtree))
     ;; Promote setext heading
     ((thing-at-point-looking-at markdown-regex-header-setext)
      (markdown-cycle-setext -1))
     ;; Promote horizontal rule
     ((thing-at-point-looking-at markdown-regex-hr)
      (markdown-cycle-hr -1))
     ;; Promote list item
     ((setq bounds (markdown-cur-list-item-bounds))
      (markdown-promote-list-item bounds))
     ;; Move table column to the left
     ((markdown-table-at-point-p)
      (call-interactively #'markdown-table-move-column-left))
     ;; Promote bold
     ((thing-at-point-looking-at markdown-regex-bold)
      (markdown-cycle-bold))
     ;; Promote italic
     ((thing-at-point-looking-at markdown-regex-italic)
      (markdown-cycle-italic))
     (t
      (user-error "Nothing to promote at point")))))

(defun markdown-demote ()
  "Demote or move element at point to the right.
Depending on the context, this function will demote a heading or
list item at the point, move a table column to the right, or cycle
or remove markup."
  (interactive)
  (let (bounds)
    (cond
     ;; Demote atx heading subtree
     ((thing-at-point-looking-at markdown-regex-header-atx)
      (markdown-demote-subtree))
     ;; Demote setext heading
     ((thing-at-point-looking-at markdown-regex-header-setext)
      (markdown-cycle-setext 1))
     ;; Demote horizontal rule
     ((thing-at-point-looking-at markdown-regex-hr)
      (markdown-cycle-hr 1))
     ;; Demote list item
     ((setq bounds (markdown-cur-list-item-bounds))
      (markdown-demote-list-item bounds))
     ;; Move table column to the right
     ((markdown-table-at-point-p)
      (call-interactively #'markdown-table-move-column-right))
     ;; Demote bold
     ((thing-at-point-looking-at markdown-regex-bold)
      (markdown-cycle-bold))
     ;; Demote italic
     ((thing-at-point-looking-at markdown-regex-italic)
      (markdown-cycle-italic))
     (t
      (user-error "Nothing to demote at point")))))


;;; Commands ==================================================================

(defun markdown (&optional output-buffer-name)
  "Run `markdown-command' on buffer, sending output to OUTPUT-BUFFER-NAME.
The output buffer name defaults to `markdown-output-buffer-name'.
Return the name of the output buffer used."
  (interactive)
  (save-window-excursion
    (let* ((commands (cond ((stringp markdown-command) (split-string markdown-command))
                           ((listp markdown-command) markdown-command)))
           (command (car-safe commands))
           (command-args (cdr-safe commands))
           begin-region end-region)
      (if (use-region-p)
          (setq begin-region (region-beginning)
                end-region (region-end))
        (setq begin-region (point-min)
              end-region (point-max)))

      (unless output-buffer-name
        (setq output-buffer-name markdown-output-buffer-name))
      (when (and (stringp command) (not (executable-find command)))
        (user-error "Markdown command %s is not found" command))
      (let ((exit-code
             (cond
              ;; Handle case when `markdown-command' does not read from stdin
              ((and (stringp command) markdown-command-needs-filename)
               (if (not buffer-file-name)
                   (user-error "Must be visiting a file")
                 ;; Don’t use ‘shell-command’ because it’s not guaranteed to
                 ;; return the exit code of the process.
                 (let ((command (if (listp markdown-command)
                                    (string-join markdown-command " ")
                                  markdown-command)))
                   (shell-command-on-region
                    ;; Pass an empty region so that stdin is empty.
                    (point) (point)
                    (concat command " "
                            (shell-quote-argument buffer-file-name))
                    output-buffer-name))))
              ;; Pass region to `markdown-command' via stdin
              (t
               (let ((buf (get-buffer-create output-buffer-name)))
                 (with-current-buffer buf
                   (setq buffer-read-only nil)
                   (erase-buffer))
                 (if (stringp command)
                     (if (not (null command-args))
                         (apply #'call-process-region begin-region end-region command nil buf nil command-args)
                       (call-process-region begin-region end-region command nil buf))
                   (if markdown-command-needs-filename
                       (if (not buffer-file-name)
                           (user-error "Must be visiting a file")
                         (funcall markdown-command begin-region end-region buf buffer-file-name))
                     (funcall markdown-command begin-region end-region buf))
                   ;; If the ‘markdown-command’ function didn’t signal an
                   ;; error, assume it succeeded by binding ‘exit-code’ to 0.
                   0))))))
        ;; The exit code can be a signal description string, so don’t use ‘=’
        ;; or ‘zerop’.
        (unless (eq exit-code 0)
          (user-error "%s failed with exit code %s"
                      markdown-command exit-code))))
    output-buffer-name))

(defun markdown-standalone (&optional output-buffer-name)
  "Special function to provide standalone HTML output.
Insert the output in the buffer named OUTPUT-BUFFER-NAME."
  (interactive)
  (setq output-buffer-name (markdown output-buffer-name))
  (with-current-buffer output-buffer-name
    (set-buffer output-buffer-name)
    (unless (markdown-output-standalone-p)
      (markdown-add-xhtml-header-and-footer output-buffer-name))
    (goto-char (point-min))
    (html-mode))
  output-buffer-name)

(defun markdown-other-window (&optional output-buffer-name)
  "Run `markdown-command' on current buffer and display in other window.
When OUTPUT-BUFFER-NAME is given, insert the output in the buffer with
that name."
  (interactive)
  (markdown-display-buffer-other-window
   (markdown-standalone output-buffer-name)))

(defun markdown-output-standalone-p ()
  "Determine whether `markdown-command' output is standalone XHTML.
Standalone XHTML output is identified by an occurrence of
`markdown-xhtml-standalone-regexp' in the first five lines of output."
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (re-search-forward
       markdown-xhtml-standalone-regexp
       (save-excursion (goto-char (point-min)) (forward-line 4) (point))
       t))))

(defun markdown-stylesheet-link-string (stylesheet-path)
  (concat "<link rel=\"stylesheet\" type=\"text/css\" media=\"all\" href=\""
          (or (and (string-prefix-p "~" stylesheet-path)
                   (expand-file-name stylesheet-path))
              stylesheet-path)
          "\"  />"))

(defun markdown-escape-title (title)
  "Escape a minimum set of characters in TITLE so they don't clash with html."
  (replace-regexp-in-string ">" "&gt;"
    (replace-regexp-in-string "<" "&lt;"
      (replace-regexp-in-string "&" "&amp;" title))))

(defun markdown-add-xhtml-header-and-footer (title)
  "Wrap XHTML header and footer with given TITLE around current buffer."
  (goto-char (point-min))
  (insert "<?xml version=\"1.0\" encoding=\"UTF-8\" ?>\n"
          "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n"
          "\t\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">\n\n"
          "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n\n"
          "<head>\n<title>")
  (insert (markdown-escape-title title))
  (insert "</title>\n")
  (unless (= (length markdown-content-type) 0)
    (insert
     (format
      "<meta http-equiv=\"Content-Type\" content=\"%s;charset=%s\"/>\n"
      markdown-content-type
      (or (and markdown-coding-system
               (coding-system-get markdown-coding-system
                                  'mime-charset))
          (coding-system-get buffer-file-coding-system
                             'mime-charset)
          "utf-8"))))
  (if (> (length markdown-css-paths) 0)
      (insert (mapconcat #'markdown-stylesheet-link-string
                         markdown-css-paths "\n")))
  (when (> (length markdown-xhtml-header-content) 0)
    (insert markdown-xhtml-header-content))
  (insert "\n</head>\n\n"
          "<body>\n\n")
  (when (> (length markdown-xhtml-body-preamble) 0)
    (insert markdown-xhtml-body-preamble "\n"))
  (goto-char (point-max))
  (when (> (length markdown-xhtml-body-epilogue) 0)
    (insert "\n" markdown-xhtml-body-epilogue))
  (insert "\n"
          "</body>\n"
          "</html>\n"))

(defun markdown-preview (&optional output-buffer-name)
  "Run `markdown-command' on the current buffer and view output in browser.
When OUTPUT-BUFFER-NAME is given, insert the output in the buffer with
that name."
  (interactive)
  (browse-url-of-buffer
   (markdown-standalone (or output-buffer-name markdown-output-buffer-name))))

(defun markdown-export-file-name (&optional extension)
  "Attempt to generate a filename for Markdown output.
The file extension will be EXTENSION if given, or .html by default.
If the current buffer is visiting a file, we construct a new
output filename based on that filename.  Otherwise, return nil."
  (when (buffer-file-name)
    (unless extension
      (setq extension ".html"))
    (let ((candidate
           (concat
            (cond
             ((buffer-file-name)
              (file-name-sans-extension (buffer-file-name)))
             (t (buffer-name)))
            extension)))
      (cond
       ((equal candidate (buffer-file-name))
        (concat candidate extension))
       (t
        candidate)))))

(defun markdown-export (&optional output-file)
  "Run Markdown on the current buffer, save to file, and return the filename.
If OUTPUT-FILE is given, use that as the filename.  Otherwise, use the filename
generated by `markdown-export-file-name', which will be constructed using the
current filename, but with the extension removed and replaced with .html."
  (interactive)
  (unless output-file
    (setq output-file (markdown-export-file-name ".html")))
  (when output-file
    (let* ((init-buf (current-buffer))
           (init-point (point))
           (init-buf-string (buffer-string))
           (output-buffer (find-file-noselect output-file))
           (output-buffer-name (buffer-name output-buffer)))
      (run-hooks 'markdown-before-export-hook)
      (markdown-standalone output-buffer-name)
      (with-current-buffer output-buffer
        (run-hooks 'markdown-after-export-hook)
        (save-buffer)
        (when markdown-export-kill-buffer (kill-buffer)))
      ;; if modified, restore initial buffer
      (when (buffer-modified-p init-buf)
        (erase-buffer)
        (insert init-buf-string)
        (save-buffer)
        (goto-char init-point))
      output-file)))

(defun markdown-export-and-preview ()
  "Export to XHTML using `markdown-export' and browse the resulting file."
  (interactive)
  (browse-url-of-file (markdown-export)))

(defvar-local markdown-live-preview-buffer nil
  "Buffer used to preview markdown output in `markdown-live-preview-export'.")

(defvar-local markdown-live-preview-source-buffer nil
  "Source buffer from which current buffer was generated.
This is the inverse of `markdown-live-preview-buffer'.")

(defvar markdown-live-preview-currently-exporting nil)

(defun markdown-live-preview-get-filename ()
  "Standardize the filename exported by `markdown-live-preview-export'."
  (markdown-export-file-name ".html"))

(defun markdown-live-preview-window-eww (file)
  "Preview FILE with eww.
To be used with `markdown-live-preview-window-function'."
  (when (and (bound-and-true-p eww-auto-rename-buffer)
             markdown-live-preview-buffer)
    (kill-buffer markdown-live-preview-buffer))
  (eww-open-file file)
  ;; #737 if `eww-auto-rename-buffer' is non-nil, the buffer name is not  "*eww*"
  ;; Try to find the buffer whose name ends with "eww*"
  (if (bound-and-true-p eww-auto-rename-buffer)
      (cl-loop for buf in (buffer-list)
               when (string-match-p "eww\\*\\'" (buffer-name buf))
               return buf)
    (get-buffer "*eww*")))

(defun markdown-visual-lines-between-points (beg end)
  (save-excursion
    (goto-char beg)
    (cl-loop with count = 0
             while (progn (end-of-visual-line)
                          (and (< (point) end) (line-move-visual 1 t)))
             do (cl-incf count)
             finally return count)))

(defun markdown-live-preview-window-serialize (buf)
  "Get window point and scroll data for all windows displaying BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (mapcar
       (lambda (win)
         (with-selected-window win
           (let* ((start (window-start))
                  (pt (window-point))
                  (pt-or-sym (cond ((= pt (point-min)) 'min)
                                   ((= pt (point-max)) 'max)
                                   (t pt)))
                  (diff (markdown-visual-lines-between-points
                         start pt)))
             (list win pt-or-sym diff))))
       (get-buffer-window-list buf)))))

(defun markdown-get-point-back-lines (pt num-lines)
  (save-excursion
    (goto-char pt)
    (line-move-visual (- num-lines) t)
    ;; in testing, can occasionally overshoot the number of lines to traverse
    (let ((actual-num-lines (markdown-visual-lines-between-points (point) pt)))
      (when (> actual-num-lines num-lines)
        (line-move-visual (- actual-num-lines num-lines) t)))
    (point)))

(defun markdown-live-preview-window-deserialize (window-posns)
  "Apply window point and scroll data from WINDOW-POSNS.
WINDOW-POSNS is provided by `markdown-live-preview-window-serialize'."
  (cl-destructuring-bind (win pt-or-sym diff) window-posns
    (when (window-live-p win)
      (with-current-buffer markdown-live-preview-buffer
        (set-window-buffer win (current-buffer))
        (cl-destructuring-bind (actual-pt actual-diff)
            (cl-case pt-or-sym
              (min (list (point-min) 0))
              (max (list (point-max) diff))
              (t   (list pt-or-sym diff)))
          (set-window-start
           win (markdown-get-point-back-lines actual-pt actual-diff))
          (set-window-point win actual-pt))))))

(defun markdown-live-preview-export ()
  "Export to XHTML using `markdown-export'.
Browse the resulting file within Emacs using
`markdown-live-preview-window-function' Return the buffer
displaying the rendered output."
  (interactive)
  (let ((filename (markdown-live-preview-get-filename)))
    (when filename
      (let* ((markdown-live-preview-currently-exporting t)
             (cur-buf (current-buffer))
             (export-file (markdown-export filename))
             ;; get positions in all windows currently displaying output buffer
             (window-data
              (markdown-live-preview-window-serialize
               markdown-live-preview-buffer)))
        (save-window-excursion
          (let ((output-buffer
                 (funcall markdown-live-preview-window-function export-file)))
            (with-current-buffer output-buffer
              (setq markdown-live-preview-source-buffer cur-buf)
              (add-hook 'kill-buffer-hook
                        #'markdown-live-preview-remove-on-kill t t))
            (with-current-buffer cur-buf
              (setq markdown-live-preview-buffer output-buffer))))
        (with-current-buffer cur-buf
          ;; reset all windows displaying output buffer to where they were,
          ;; now with the new output
          (mapc #'markdown-live-preview-window-deserialize window-data)
          ;; delete html editing buffer
          (let ((buf (get-file-buffer export-file))) (when buf (kill-buffer buf)))
          (when (and export-file (file-exists-p export-file)
                     (eq markdown-live-preview-delete-export
                         'delete-on-export))
            (delete-file export-file))
          markdown-live-preview-buffer)))))

(defun markdown-live-preview-remove ()
  (when (buffer-live-p markdown-live-preview-buffer)
    (kill-buffer markdown-live-preview-buffer))
  (setq markdown-live-preview-buffer nil)
  ;; if set to 'delete-on-export, the output has already been deleted
  (when (eq markdown-live-preview-delete-export 'delete-on-destroy)
    (let ((outfile-name (markdown-live-preview-get-filename)))
      (when (and outfile-name (file-exists-p outfile-name))
        (delete-file outfile-name)))))

(defun markdown-get-other-window ()
  "Find another window to display preview or output content."
  (cond
   ((memq markdown-split-window-direction '(vertical below))
    (or (window-in-direction 'below) (split-window-vertically)))
   ((memq markdown-split-window-direction '(horizontal right))
    (or (window-in-direction 'right) (split-window-horizontally)))
   (t (split-window-sensibly (get-buffer-window)))))

(defun markdown-display-buffer-other-window (buf)
  "Display preview or output buffer BUF in another window."
  (if (and display-buffer-alist (eq markdown-split-window-direction 'any))
      (display-buffer buf)
    (let ((cur-buf (current-buffer))
          (window (markdown-get-other-window)))
      (set-window-buffer window buf)
      (set-buffer cur-buf))))

(defun markdown-live-preview-if-markdown ()
  (when (and (derived-mode-p 'markdown-mode)
             markdown-live-preview-mode)
    (unless markdown-live-preview-currently-exporting
      (if (buffer-live-p markdown-live-preview-buffer)
          (markdown-live-preview-export)
        (markdown-display-buffer-other-window
         (markdown-live-preview-export))))))

(defun markdown-live-preview-remove-on-kill ()
  (cond ((and (derived-mode-p 'markdown-mode)
              markdown-live-preview-mode)
         (markdown-live-preview-remove))
        (markdown-live-preview-source-buffer
         (with-current-buffer markdown-live-preview-source-buffer
           (setq markdown-live-preview-buffer nil))
         (setq markdown-live-preview-source-buffer nil))))

(defun markdown-live-preview-switch-to-output ()
  "Turn on `markdown-live-preview-mode' and switch to output buffer.
The output buffer is opened in another window."
  (interactive)
  (if markdown-live-preview-mode
      (markdown-display-buffer-other-window (markdown-live-preview-export)))
  (markdown-live-preview-mode))

(defun markdown-live-preview-re-export ()
  "Re-export the current live previewed content.
If the current buffer is a buffer displaying the exported version of a
`markdown-live-preview-mode' buffer, call `markdown-live-preview-export' and
update this buffer's contents."
  (interactive)
  (when markdown-live-preview-source-buffer
    (with-current-buffer markdown-live-preview-source-buffer
      (markdown-live-preview-export))))

(defun markdown-open ()
  "Open file for the current buffer with `markdown-open-command'."
  (interactive)
  (unless markdown-open-command
    (user-error "Variable `markdown-open-command' must be set"))
  (if (stringp markdown-open-command)
      (if (not buffer-file-name)
          (user-error "Must be visiting a file")
        (save-buffer)
        (let ((exit-code (call-process markdown-open-command nil nil nil
                                       buffer-file-name)))
          ;; The exit code can be a signal description string, so don’t use ‘=’
          ;; or ‘zerop’.
          (unless (eq exit-code 0)
            (user-error "%s failed with exit code %s"
                        markdown-open-command exit-code))))
    (funcall markdown-open-command))
  nil)

(defun markdown-kill-ring-save ()
  "Run Markdown on file and store output in the kill ring."
  (interactive)
  (save-window-excursion
    (markdown)
    (with-current-buffer markdown-output-buffer-name
      (kill-ring-save (point-min) (point-max)))))


;;; Links =====================================================================

(defun markdown-backward-to-link-start ()
  "Backward link start position if current position is in link title."
  ;; Issue #305
  (when (eq (get-text-property (point) 'face) 'markdown-link-face)
    (skip-chars-backward "^[")
    (forward-char -1)))

(defun markdown-link-p ()
  "Return non-nil when `point' is at a non-wiki link.
See `markdown-wiki-link-p' for more information."
  (save-excursion
    (let ((case-fold-search nil))
      (when (and (not (markdown-wiki-link-p)) (not (markdown-code-block-at-point-p)))
        (markdown-backward-to-link-start)
        (or (thing-at-point-looking-at markdown-regex-link-inline)
            (thing-at-point-looking-at markdown-regex-link-reference)
            (thing-at-point-looking-at markdown-regex-uri)
            (thing-at-point-looking-at markdown-regex-angle-uri))))))

(defun markdown-link-at-pos (pos)
  "Return properties of link or image at position POS.
Value is a list of elements describing the link:
 0. beginning position
 1. end position
 2. link text
 3. URL
 4. reference label
 5. title text
 6. bang (nil or \"!\")"
  (save-excursion
    (goto-char pos)
    (markdown-backward-to-link-start)
    (let (begin end text url reference title bang)
      (cond
       ;; Inline image or link at point.
       ((thing-at-point-looking-at markdown-regex-link-inline)
        (setq bang (match-string-no-properties 1)
              begin (match-beginning 0)
              text (match-string-no-properties 3)
              url (match-string-no-properties 6))
        ;; consider nested parentheses
        ;; if link target contains parentheses, (match-end 0) isn't correct end position of the link
        (let* ((close-pos (scan-sexps (match-beginning 5) 1))
               (destination-part (string-trim (buffer-substring-no-properties (1+ (match-beginning 5)) (1- close-pos)))))
          (setq end close-pos)
          ;; A link can contain spaces if it is wrapped with angle brackets
          (cond ((string-match "\\`<\\(.+\\)>\\'" destination-part)
                 (setq url (match-string-no-properties 1 destination-part)))
                ((string-match "\\([^ ]+\\)\\s-+\\(.+\\)" destination-part)
                 (setq url (match-string-no-properties 1 destination-part)
                       title (substring (match-string-no-properties 2 destination-part) 1 -1)))
                (t (setq url destination-part)))
          (setq url (url-unhex-string url))))
       ;; Reference link at point.
       ((thing-at-point-looking-at markdown-regex-link-reference)
        (setq bang (match-string-no-properties 1)
              begin (match-beginning 0)
              end (match-end 0)
              text (match-string-no-properties 3))
        (when (char-equal (char-after (match-beginning 5)) ?\[)
          (setq reference (match-string-no-properties 6))))
       ;; Angle bracket URI at point.
       ((thing-at-point-looking-at markdown-regex-angle-uri)
        (setq begin (match-beginning 0)
              end (match-end 0)
              url (match-string-no-properties 2)))
       ;; Plain URI at point.
       ((thing-at-point-looking-at markdown-regex-uri)
        (setq begin (match-beginning 0)
              end (match-end 0)
              url (match-string-no-properties 1))))
      (list begin end text url reference title bang))))

(defun markdown-link-url ()
  "Return the URL part of the regular (non-wiki) link at point.
Works with both inline and reference style links, and with images.
If point is not at a link or the link reference is not defined
returns nil."
  (let* ((values (markdown-link-at-pos (point)))
         (text (nth 2 values))
         (url (nth 3 values))
         (ref (nth 4 values)))
    (or url (and ref (car (markdown-reference-definition
                           (downcase (if (string= ref "") text ref))))))))

(defun markdown--browse-url (url)
  (let* ((struct (url-generic-parse-url url))
         (full (url-fullness struct))
         (file url))
    ;; Parse URL, determine fullness, strip query string
    (setq file (car (url-path-and-query struct)))
    ;; Open full URLs in browser, files in Emacs
    (if full
        (browse-url url)
      (when (and file (> (length file) 0))
        (let ((link-file (funcall markdown-translate-filename-function file)))
          (if (and markdown-open-image-command (string-match-p (image-file-name-regexp) link-file))
              (if (functionp markdown-open-image-command)
                  (funcall markdown-open-image-command link-file)
                (process-file markdown-open-image-command nil nil nil link-file))
            (find-file link-file)))))))

(defun markdown-follow-link-at-point (&optional event)
  "Open the non-wiki link at point or EVENT.
If the link is a complete URL, open in browser with `browse-url'.
Otherwise, open with `find-file' after stripping anchor and/or query string.
Translate filenames using `markdown-filename-translate-function'."
  (interactive (list last-command-event))
  (if event (posn-set-point (event-start event)))
  (if (markdown-link-p)
      (or (run-hook-with-args-until-success 'markdown-follow-link-functions (markdown-link-url))
          (markdown--browse-url (markdown-link-url)))
    (user-error "Point is not at a Markdown link or URL")))

(defun markdown-fontify-inline-links (last)
  "Add text properties to next inline link from point to LAST."
  (when (markdown-match-generic-links last nil)
    (let* ((link-start (match-beginning 3))
           (link-end (match-end 3))
           (url-start (match-beginning 6))
           (url-end (match-end 6))
           (url (match-string-no-properties 6))
           (title-start (match-beginning 7))
           (title-end (match-end 7))
           (title (match-string-no-properties 7))
           ;; Markup part
           (mp (list 'invisible 'markdown-markup
                     'rear-nonsticky t
                     'font-lock-multiline t))
           ;; Link part (without face)
           (lp (list 'keymap markdown-mode-mouse-map
                     'mouse-face 'markdown-highlight-face
                     'font-lock-multiline t
                     'help-echo (if title (concat title "\n" url) url)))
           ;; URL part
           (up (list 'keymap markdown-mode-mouse-map
                     'invisible 'markdown-markup
                     'mouse-face 'markdown-highlight-face
                     'font-lock-multiline t))
           ;; URL composition character
           (url-char (markdown--first-displayable markdown-url-compose-char))
           ;; Title part
           (tp (list 'invisible 'markdown-markup
                     'font-lock-multiline t)))
      (dolist (g '(1 2 4 5 8))
        (when (match-end g)
          (add-text-properties (match-beginning g) (match-end g) mp)
          (add-face-text-property (match-beginning g) (match-end g) 'markdown-markup-face)))
      ;; Preserve existing faces applied to link part (e.g., inline code)
      (when link-start
        (add-text-properties link-start link-end lp)
        (add-face-text-property link-start link-end 'markdown-link-face))
      (when url-start
        (add-text-properties url-start url-end up)
        (add-face-text-property url-start url-end 'markdown-url-face))
      (when title-start
        (add-text-properties url-end title-end tp)
        (add-face-text-property url-end title-end 'markdown-link-title-face))
      (when (and markdown-hide-urls url-start)
        (compose-region url-start (or title-end url-end) url-char))
      t)))

(defun markdown-fontify-reference-links (last)
  "Add text properties to next reference link from point to LAST."
  (when (markdown-match-generic-links last t)
    (let* ((link-start (match-beginning 3))
           (link-end (match-end 3))
           (ref-start (match-beginning 6))
           (ref-end (match-end 6))
           ;; Markup part
           (mp (list 'invisible 'markdown-markup
                     'rear-nonsticky t
                     'font-lock-multiline t))
           ;; Link part
           (lp (list 'keymap markdown-mode-mouse-map
                     'mouse-face 'markdown-highlight-face
                     'font-lock-multiline t
                     'help-echo (lambda (_ __ pos)
                                  (save-match-data
                                    (save-excursion
                                      (goto-char pos)
                                      (or (markdown-link-url)
                                          "Undefined reference"))))))
           ;; URL composition character
           (url-char (markdown--first-displayable markdown-url-compose-char))
           ;; Reference part
           (rp (list 'invisible 'markdown-markup
                     'font-lock-multiline t)))
      (dolist (g '(1 2 4 5 8))
        (when (match-end g)
          (add-text-properties (match-beginning g) (match-end g) mp)
          (add-face-text-property (match-beginning g) (match-end g) 'markdown-markup-face)))
      (when link-start
        (add-text-properties link-start link-end lp)
        (add-face-text-property link-start link-end 'markdown-link-face))
      (when ref-start
        (add-text-properties ref-start ref-end rp)
        (add-face-text-property ref-start ref-end 'markdown-reference-face)
        (when (and markdown-hide-urls (> (- ref-end ref-start) 2))
          (compose-region ref-start ref-end url-char)))
      t)))

(defun markdown-fontify-angle-uris (last)
  "Add text properties to angle URIs from point to LAST."
  (when (markdown-match-angle-uris last)
    (let* ((url-start (match-beginning 2))
           (url-end (match-end 2))
           ;; Markup part
           (mp (list 'face 'markdown-markup-face
                     'invisible 'markdown-markup
                     'rear-nonsticky t
                     'font-lock-multiline t))
           ;; URI part
           (up (list 'keymap markdown-mode-mouse-map
                     'face 'markdown-plain-url-face
                     'mouse-face 'markdown-highlight-face
                     'font-lock-multiline t)))
      (dolist (g '(1 3))
        (add-text-properties (match-beginning g) (match-end g) mp))
      (add-text-properties url-start url-end up)
      t)))

(defun markdown-fontify-plain-uris (last)
  "Add text properties to plain URLs from point to LAST."
  (when (markdown-match-plain-uris last)
    (let* ((start (match-beginning 0))
           (end (match-end 0))
           (props (list 'keymap markdown-mode-mouse-map
                        'face 'markdown-plain-url-face
                        'mouse-face 'markdown-highlight-face
                        'rear-nonsticky t
                        'font-lock-multiline t)))
      (add-text-properties start end props)
      t)))

(defun markdown-toggle-url-hiding (&optional arg)
  "Toggle the display or hiding of URLs.
With a prefix argument ARG, enable URL hiding if ARG is positive,
and disable it otherwise."
  (interactive (list (or current-prefix-arg 'toggle)))
  (setq markdown-hide-urls
        (if (eq arg 'toggle)
            (not markdown-hide-urls)
          (> (prefix-numeric-value arg) 0)))
  (if markdown-hide-urls
      (message "markdown-mode URL hiding enabled")
    (message "markdown-mode URL hiding disabled"))
  (markdown-reload-extensions))


;;; Wiki Links ================================================================

(defun markdown-wiki-link-p ()
  "Return non-nil if wiki links are enabled and `point' is at a true wiki link.
A true wiki link name matches `markdown-regex-wiki-link' but does
not match the current file name after conversion.  This modifies
the data returned by `match-data'.  Note that the potential wiki
link name must be available via `match-string'."
  (when markdown-enable-wiki-links
    (let ((case-fold-search nil))
      (and (thing-at-point-looking-at markdown-regex-wiki-link)
           (not (markdown-code-block-at-point-p))
           (or (not buffer-file-name)
               (not (string-equal (buffer-file-name)
                                  (markdown-convert-wiki-link-to-filename
                                   (markdown-wiki-link-link)))))))))

(defun markdown-wiki-link-link ()
  "Return the link part of the wiki link using current match data.
The location of the link component depends on the value of
`markdown-wiki-link-alias-first'."
  (if markdown-wiki-link-alias-first
      (or (match-string-no-properties 5) (match-string-no-properties 3))
    (match-string-no-properties 3)))

(defun markdown-wiki-link-alias ()
  "Return the alias or text part of the wiki link using current match data.
The location of the alias component depends on the value of
`markdown-wiki-link-alias-first'."
  (if markdown-wiki-link-alias-first
      (match-string-no-properties 3)
    (or (match-string-no-properties 5) (match-string-no-properties 3))))

(defun markdown--wiki-link-search-types ()
  (let ((ret (and markdown-wiki-link-search-type
                  (cl-copy-list markdown-wiki-link-search-type))))
    (when (and markdown-wiki-link-search-subdirectories
               (not (memq 'sub-directories markdown-wiki-link-search-type)))
      (push 'sub-directories ret))
    (when (and markdown-wiki-link-search-parent-directories
               (not (memq 'parent-directories markdown-wiki-link-search-type)))
      (push 'parent-directories ret))
    ret))

(defun markdown--project-root ()
  (or (cl-loop for dir in '(".git" ".hg" ".svn")
               when (locate-dominating-file default-directory dir)
               return it)
      (progn
        (require 'project)
        (let ((project (project-current t)))
          (with-no-warnings
            (if (fboundp 'project-root)
                (project-root project)
              (car (project-roots project))))))))

(defun markdown-convert-wiki-link-to-filename (name)
  "Generate a filename from the wiki link NAME.
Spaces in NAME are replaced with `markdown-link-space-sub-char'.
When in `gfm-mode', follow GitHub's conventions where [[Test Test]]
and [[test test]] both map to Test-test.ext.  Look in the current
directory first, then in subdirectories if
`markdown-wiki-link-search-subdirectories' is non-nil, and then
in parent directories if
`markdown-wiki-link-search-parent-directories' is non-nil."
  (save-match-data
    ;; This function must not overwrite match data(PR #590)
    (let* ((basename (replace-regexp-in-string
                      "[[:space:]\n]" markdown-link-space-sub-char name))
           (basename (if (derived-mode-p 'gfm-mode)
                         (concat (upcase (substring basename 0 1))
                                 (downcase (substring basename 1 nil)))
                       basename))
           (search-types (markdown--wiki-link-search-types))
           directory extension default candidates dir)
      (when buffer-file-name
        (setq directory (file-name-directory buffer-file-name)
              extension (file-name-extension buffer-file-name)))
      (setq default (concat basename
                            (when extension (concat "." extension))))
      (cond
       ;; Look in current directory first.
       ((or (null buffer-file-name)
            (file-exists-p default))
        default)
       ;; Possibly search in subdirectories, next.
       ((and (memq 'sub-directories search-types)
             (setq candidates
                   (directory-files-recursively
                    directory (concat "^" default "$"))))
        (car candidates))
       ;; Possibly search in parent directories as a last resort.
       ((and (memq 'parent-directories search-types)
             (setq dir (locate-dominating-file directory default)))
        (concat dir default))
       ((and (memq 'project search-types)
             (setq candidates
                   (directory-files-recursively
                    (markdown--project-root) (concat "^" default "$"))))
        (car candidates))
       ;; If nothing is found, return default in current directory.
       (t default)))))

(defun markdown-follow-wiki-link (name &optional other)
  "Follow the wiki link NAME.
Convert the name to a file name and call `find-file'.  Ensure that
the new buffer remains in `markdown-mode'.  Open the link in another
window when OTHER is non-nil."
  (let ((filename (markdown-convert-wiki-link-to-filename name))
        (wp (when buffer-file-name
              (file-name-directory buffer-file-name))))
    (if (not wp)
        (user-error "Must be visiting a file")
      (when other (other-window 1))
      (let ((default-directory wp))
        (find-file filename)))
    (unless (derived-mode-p 'markdown-mode)
      (markdown-mode))))

(defun markdown-follow-wiki-link-at-point (&optional arg)
  "Find Wiki Link at point.
With prefix argument ARG, open the file in other window.
See `markdown-wiki-link-p' and `markdown-follow-wiki-link'."
  (interactive "P")
  (if (markdown-wiki-link-p)
      (markdown-follow-wiki-link (markdown-wiki-link-link) arg)
    (user-error "Point is not at a Wiki Link")))

(defun markdown-highlight-wiki-link (from to face)
  "Highlight the wiki link in the region between FROM and TO using FACE."
  (put-text-property from to 'font-lock-face face))

(defun markdown-unfontify-region-wiki-links (from to)
  "Remove wiki link faces from the region specified by FROM and TO."
  (interactive "*r")
  (let ((modified (buffer-modified-p)))
    (remove-text-properties from to '(font-lock-face markdown-link-face))
    (remove-text-properties from to '(font-lock-face markdown-missing-link-face))
    ;; remove-text-properties marks the buffer modified in emacs 24.3,
    ;; undo that if it wasn't originally marked modified
    (set-buffer-modified-p modified)))

(defun markdown-fontify-region-wiki-links (from to)
  "Search region given by FROM and TO for wiki links and fontify them.
If a wiki link is found check to see if the backing file exists
and highlight accordingly."
  (goto-char from)
  (save-match-data
    (while (re-search-forward markdown-regex-wiki-link to t)
      (when (not (markdown-code-block-at-point-p))
        (let ((highlight-beginning (match-beginning 1))
              (highlight-end (match-end 1))
              (file-name
               (markdown-convert-wiki-link-to-filename
                (markdown-wiki-link-link))))
          (if (condition-case nil (file-exists-p file-name) (error nil))
              (markdown-highlight-wiki-link
               highlight-beginning highlight-end 'markdown-link-face)
            (markdown-highlight-wiki-link
             highlight-beginning highlight-end 'markdown-missing-link-face)))))))

(defun markdown-extend-changed-region (from to)
  "Extend region given by FROM and TO so that we can fontify all links.
The region is extended to the first newline before and the first
newline after."
  ;; start looking for the first new line before 'from
  (goto-char from)
  (re-search-backward "\n" nil t)
  (let ((new-from (point-min))
        (new-to (point-max)))
    (if (not (= (point) from))
        (setq new-from (point)))
    ;; do the same thing for the first new line after 'to
    (goto-char to)
    (re-search-forward "\n" nil t)
    (if (not (= (point) to))
        (setq new-to (point)))
    (cl-values new-from new-to)))

(defun markdown-check-change-for-wiki-link (from to)
  "Check region between FROM and TO for wiki links and re-fontify as needed."
  (interactive "*r")
  (let* ((modified (buffer-modified-p))
         (buffer-undo-list t)
         (inhibit-read-only t)
         deactivate-mark
         buffer-file-truename)
    (unwind-protect
        (save-excursion
          (save-match-data
            (save-restriction
              (cursor-intangible-mode +1) ;; inhibit-point-motion-hooks is obsoleted since Emacs 29
              ;; Extend the region to fontify so that it starts
              ;; and ends at safe places.
              (cl-multiple-value-bind (new-from new-to)
                  (markdown-extend-changed-region from to)
                (goto-char new-from)
                ;; Only refontify when the range contains text with a
                ;; wiki link face or if the wiki link regexp matches.
                (when (or (markdown-range-property-any
                           new-from new-to 'font-lock-face
                           '(markdown-link-face markdown-missing-link-face))
                          (re-search-forward
                           markdown-regex-wiki-link new-to t))
                  ;; Unfontify existing fontification (start from scratch)
                  (markdown-unfontify-region-wiki-links new-from new-to)
                  ;; Now do the fontification.
                  (markdown-fontify-region-wiki-links new-from new-to))))))
      (cursor-intangible-mode -1)
      (and (not modified)
           (buffer-modified-p)
           (set-buffer-modified-p nil)))))

(defun markdown-check-change-for-wiki-link-after-change (from to _)
  "Check region between FROM and TO for wiki links and re-fontify as needed.
Designed to be used with the `after-change-functions' hook."
  (markdown-check-change-for-wiki-link from to))

(defun markdown-fontify-buffer-wiki-links ()
  "Refontify all wiki links in the buffer."
  (interactive)
  (markdown-check-change-for-wiki-link (point-min) (point-max)))

(defun markdown-toggle-wiki-links (&optional arg)
  "Toggle support for wiki links.
With a prefix argument ARG, enable wiki link support if ARG is positive,
and disable it otherwise."
  (interactive (list (or current-prefix-arg 'toggle)))
  (setq markdown-enable-wiki-links
        (if (eq arg 'toggle)
            (not markdown-enable-wiki-links)
          (> (prefix-numeric-value arg) 0)))
  (if markdown-enable-wiki-links
      (message "markdown-mode wiki link support enabled")
    (message "markdown-mode wiki link support disabled"))
  (markdown-reload-extensions))

(defun markdown-setup-wiki-link-hooks ()
  "Add or remove hooks for fontifying wiki links.
These are only enabled when `markdown-wiki-link-fontify-missing' is non-nil."
  ;; Anytime text changes make sure it gets fontified correctly
  (if (and markdown-enable-wiki-links
           markdown-wiki-link-fontify-missing)
      (add-hook 'after-change-functions
                #'markdown-check-change-for-wiki-link-after-change t t)
    (remove-hook 'after-change-functions
                 #'markdown-check-change-for-wiki-link-after-change t))
  ;; If we left the buffer there is a really good chance we were
  ;; creating one of the wiki link documents. Make sure we get
  ;; refontified when we come back.
  (if (and markdown-enable-wiki-links
           markdown-wiki-link-fontify-missing)
      (progn
        (add-hook 'window-configuration-change-hook
                  #'markdown-fontify-buffer-wiki-links t t)
        (markdown-fontify-buffer-wiki-links))
    (remove-hook 'window-configuration-change-hook
                 #'markdown-fontify-buffer-wiki-links t)
    (markdown-unfontify-region-wiki-links (point-min) (point-max))))


;;; Following & Doing =========================================================

(defun markdown-follow-thing-at-point (arg)
  "Follow thing at point if possible, such as a reference link or wiki link.
Opens inline and reference links in a browser.  Opens wiki links
to other files in the current window, or the another window if
ARG is non-nil.
See `markdown-follow-link-at-point' and
`markdown-follow-wiki-link-at-point'."
  (interactive "P")
  (cond ((markdown-link-p)
         (markdown-follow-link-at-point))
        ((markdown-wiki-link-p)
         (markdown-follow-wiki-link-at-point arg))
        (t
         (let* ((values (markdown-link-at-pos (point)))
                (url (nth 3 values)))
           (unless url
             (user-error "Nothing to follow at point"))
           (markdown--browse-url url)))))

(defun markdown-do ()
  "Do something sensible based on context at point.
Jumps between reference links and definitions; between footnote
markers and footnote text."
  (interactive)
  (cond
   ;; Footnote definition
   ((markdown-footnote-text-positions)
    (markdown-footnote-return))
   ;; Footnote marker
   ((markdown-footnote-marker-positions)
    (markdown-footnote-goto-text))
   ;; Reference link
   ((thing-at-point-looking-at markdown-regex-link-reference)
    (markdown-reference-goto-definition))
   ;; Reference definition
   ((thing-at-point-looking-at markdown-regex-reference-definition)
    (markdown-reference-goto-link (match-string-no-properties 2)))
   ;; Link
   ((or (markdown-link-p) (markdown-wiki-link-p))
    (markdown-follow-thing-at-point nil))
   ;; GFM task list item
   ((markdown-gfm-task-list-item-at-point)
    (markdown-toggle-gfm-checkbox))
   ;; Align table
   ((markdown-table-at-point-p)
    (call-interactively #'markdown-table-align))
   ;; Otherwise
   (t
    (markdown-insert-gfm-checkbox))))


;;; Miscellaneous =============================================================

(defun markdown-compress-whitespace-string (str)
  "Compress whitespace in STR and return result.
Leading and trailing whitespace is removed.  Sequences of multiple
spaces, tabs, and newlines are replaced with single spaces."
  (replace-regexp-in-string "\\(^[ \t\n]+\\|[ \t\n]+$\\)" ""
                            (replace-regexp-in-string "[ \t\n]+" " " str)))

(defun markdown--substitute-command-keys (string)
  "Like `substitute-command-keys' but, but prefers control characters.
First pass STRING to `substitute-command-keys' and then
substitute `C-i` for `TAB` and `C-m` for `RET`."
  (replace-regexp-in-string
   "\\<TAB\\>" "C-i"
   (replace-regexp-in-string
    "\\<RET\\>" "C-m" (substitute-command-keys string) t) t))

(defun markdown-line-number-at-pos (&optional pos)
  "Return (narrowed) buffer line number at position POS.
If POS is nil, use current buffer location.
This is an exact copy of `line-number-at-pos' for use in emacs21."
  (let ((opoint (or pos (point))) start)
    (save-excursion
      (goto-char (point-min))
      (setq start (point))
      (goto-char opoint)
      (forward-line 0)
      (1+ (count-lines start (point))))))

(defun markdown-inside-link-p ()
  "Return t if point is within a link."
  (save-match-data
    (thing-at-point-looking-at (markdown-make-regex-link-generic))))

(defun markdown-line-is-reference-definition-p ()
  "Return whether the current line is a (non-footnote) reference definition."
  (save-excursion
    (move-beginning-of-line 1)
    (and (looking-at-p markdown-regex-reference-definition)
         (not (looking-at-p "[ \t]*\\[^")))))

(defun markdown-adaptive-fill-function ()
  "Return prefix for filling paragraph or nil if not determined."
  (cond
   ;; List item inside blockquote
   ((looking-at "^[ \t]*>[ \t]*\\(\\(?:[0-9]+\\|#\\)\\.\\|[*+:-]\\)[ \t]+")
    (replace-regexp-in-string
     "[0-9\\.*+-]" " " (match-string-no-properties 0)))
   ;; Blockquote
   ((looking-at markdown-regex-blockquote)
    (buffer-substring-no-properties (match-beginning 0) (match-end 2)))
   ;; List items
   ((looking-at markdown-regex-list)
    (match-string-no-properties 0))
   ;; Footnote definition
   ((looking-at-p markdown-regex-footnote-definition)
    "    ") ; four spaces
   ;; No match
   (t nil)))

(defun markdown-fill-paragraph (&optional justify)
  "Fill paragraph at or after point.
This function is like \\[fill-paragraph], but it skips Markdown
code blocks.  If the point is in a code block, or just before one,
do not fill.  Otherwise, call `fill-paragraph' as usual. If
JUSTIFY is non-nil, justify text as well.  Since this function
handles filling itself, it always returns t so that
`fill-paragraph' doesn't run."
  (interactive "P")
  (unless (or (markdown-code-block-at-point-p)
              (save-excursion
                (back-to-indentation)
                (skip-syntax-forward "-")
                (markdown-code-block-at-point-p)))
    (let ((fill-prefix (save-excursion
                         (goto-char (line-beginning-position))
                         (when (looking-at "\\([ \t]*>[ \t]*\\(?:>[ \t]*\\)+\\)")
                           (match-string-no-properties 1)))))
      (fill-paragraph justify)))
  t)

(defun markdown-fill-forward-paragraph (&optional arg)
  "Function used by `fill-paragraph' to move over ARG paragraphs.
This is a `fill-forward-paragraph-function' for `markdown-mode'.
It is called with a single argument specifying the number of
paragraphs to move.  Just like `forward-paragraph', it should
return the number of paragraphs left to move."
  (or arg (setq arg 1))
  (if (> arg 0)
      ;; With positive ARG, move across ARG non-code-block paragraphs,
      ;; one at a time.  When passing a code block, don't decrement ARG.
      (while (and (not (eobp))
                  (> arg 0)
                  (= (forward-paragraph 1) 0)
                  (or (markdown-code-block-at-pos (line-beginning-position 0))
                      (setq arg (1- arg)))))
    ;; Move backward by one paragraph with negative ARG (always -1).
    (let ((start (point)))
      (setq arg (forward-paragraph arg))
      (while (and (not (eobp))
                  (progn (move-to-left-margin) (not (eobp)))
                  (looking-at-p paragraph-separate))
        (forward-line 1))
      (cond
       ;; Move point past whitespace following list marker.
       ((looking-at markdown-regex-list)
        (goto-char (match-end 0)))
       ;; Move point past whitespace following pipe at beginning of line
       ;; to handle Pandoc line blocks.
       ((looking-at "^|\\s-*")
        (goto-char (match-end 0)))
       ;; Return point if the paragraph passed was a code block.
       ((markdown-code-block-at-pos (line-beginning-position 2))
        (goto-char start)))))
  arg)

(defun markdown--inhibit-electric-quote ()
  "Function added to `electric-quote-inhibit-functions'.
Return non-nil if the quote has been inserted inside a code block
or span."
  (let ((pos (1- (point))))
    (or (markdown-inline-code-at-pos pos)
        (markdown-code-block-at-pos pos))))


;;; Extension Framework =======================================================

(defun markdown-reload-extensions ()
  "Check settings, update font-lock keywords and hooks, and re-fontify buffer."
  (interactive)
  (when (derived-mode-p 'markdown-mode)
    ;; Refontify buffer
    (font-lock-flush)
    ;; Add or remove hooks related to extensions
    (markdown-setup-wiki-link-hooks)))

(defun markdown-handle-local-variables ()
  "Run in `hack-local-variables-hook' to update font lock rules.
Checks to see if there is actually a ‘markdown-mode’ file local variable
before regenerating font-lock rules for extensions."
  (when (or (assoc 'markdown-enable-wiki-links file-local-variables-alist)
            (assoc 'markdown-enable-math file-local-variables-alist))
    (when (assoc 'markdown-enable-math file-local-variables-alist)
      (markdown-toggle-math markdown-enable-math))
    (markdown-reload-extensions)))


;;; Math Support ==============================================================

(defconst markdown-mode-font-lock-keywords-math
  (list
   ;; Equation reference (eq:foo)
   '("\\((eq:\\)\\([[:alnum:]:_]+\\)\\()\\)" . ((1 markdown-markup-face)
                                                (2 markdown-reference-face)
                                                (3 markdown-markup-face)))
   ;; Equation reference \eqref{foo}
   '("\\(\\\\eqref{\\)\\([[:alnum:]:_]+\\)\\(}\\)" . ((1 markdown-markup-face)
                                                      (2 markdown-reference-face)
                                                      (3 markdown-markup-face))))
  "Font lock keywords to add and remove when toggling math support.")

(defun markdown-toggle-math (&optional arg)
  "Toggle support for inline and display LaTeX math expressions.
With a prefix argument ARG, enable math mode if ARG is positive,
and disable it otherwise.  If called from Lisp, enable the mode
if ARG is omitted or nil."
  (interactive (list (or current-prefix-arg 'toggle)))
  (setq markdown-enable-math
        (if (eq arg 'toggle)
            (not markdown-enable-math)
          (> (prefix-numeric-value arg) 0)))
  (if markdown-enable-math
      (progn
        (font-lock-add-keywords
         'markdown-mode markdown-mode-font-lock-keywords-math)
        (message "markdown-mode math support enabled"))
    (font-lock-remove-keywords
     'markdown-mode markdown-mode-font-lock-keywords-math)
    (message "markdown-mode math support disabled"))
  (markdown-reload-extensions))


;;; GFM Checkboxes ============================================================

(define-button-type 'markdown-gfm-checkbox-button
  'follow-link t
  'face 'markdown-gfm-checkbox-face
  'mouse-face 'markdown-highlight-face
  'action #'markdown-toggle-gfm-checkbox-button)

(defun markdown-gfm-task-list-item-at-point (&optional bounds)
  "Return non-nil if there is a GFM task list item at the point.
Optionally, the list item BOUNDS may be given if available, as
returned by `markdown-cur-list-item-bounds'.  When a task list item
is found, the return value is the same value returned by
`markdown-cur-list-item-bounds'."
  (unless bounds
    (setq bounds (markdown-cur-list-item-bounds)))
  (> (length (nth 5 bounds)) 0))

(defun markdown-insert-gfm-checkbox ()
  "Add GFM checkbox at point.
Returns t if added.
Returns nil if non-applicable."
  (interactive)
  (let ((bounds (markdown-cur-list-item-bounds)))
    (if bounds
        (unless (cl-sixth bounds)
          (let ((pos (+ (cl-first bounds) (cl-fourth bounds)))
                (markup "[ ] "))
            (if (< pos (point))
                (save-excursion
                  (goto-char pos)
                  (insert markup))
              (goto-char pos)
              (insert markup))
            (syntax-propertize (+ (cl-second bounds) 4))
            t))
      (unless (save-excursion
                (back-to-indentation)
                (or (markdown-list-item-at-point-p)
                    (markdown-heading-at-point)
                    (markdown-in-comment-p)
                    (markdown-code-block-at-point-p)))
        (let ((pos (save-excursion
                     (back-to-indentation)
                     (point)))
              (markup (concat (or (save-excursion
                                    (beginning-of-line 0)
                                    (cl-fifth (markdown-cur-list-item-bounds)))
                                  markdown-unordered-list-item-prefix)
                              "[ ] ")))
          (if (< pos (point))
              (save-excursion
                (goto-char pos)
                (insert markup))
            (goto-char pos)
            (insert markup))
          (syntax-propertize (line-end-position))
          t)))))

(defun markdown-toggle-gfm-checkbox ()
  "Toggle GFM checkbox at point.
Returns the resulting status as a string, either \"[x]\" or \"[ ]\".
Returns nil if there is no task list item at the point."
  (interactive)
  (save-match-data
    (save-excursion
      (let ((bounds (markdown-cur-list-item-bounds)))
        (when bounds
          ;; Move to beginning of task list item
          (goto-char (cl-first bounds))
          ;; Advance to column of first non-whitespace after marker
          (forward-char (cl-fourth bounds))
          (cond ((looking-at "\\[ \\]")
                 (replace-match
                  (if markdown-gfm-uppercase-checkbox "[X]" "[x]")
                  nil t)
                 (match-string-no-properties 0))
                ((looking-at "\\[[xX]\\]")
                 (replace-match "[ ]" nil t)
                 (match-string-no-properties 0))))))))

(defun markdown-toggle-gfm-checkbox-button (button)
  "Toggle GFM checkbox BUTTON on click."
  (save-match-data
    (save-excursion
      (goto-char (button-start button))
      (markdown-toggle-gfm-checkbox))))

(defun markdown-make-gfm-checkboxes-buttons (start end)
  "Make GFM checkboxes buttons in region between START and END."
  (save-excursion
    (goto-char start)
    (let ((case-fold-search t))
      (save-excursion
        (while (re-search-forward markdown-regex-gfm-checkbox end t)
          (make-button (match-beginning 1) (match-end 1)
                       :type 'markdown-gfm-checkbox-button))))))

;; Called when any modification is made to buffer text.
(defun markdown-gfm-checkbox-after-change-function (beg end _)
  "Add to `after-change-functions' to setup GFM checkboxes as buttons.
BEG and END are the limits of scanned region."
  (save-excursion
    (save-match-data
      ;; Rescan between start of line from `beg' and start of line after `end'.
      (markdown-make-gfm-checkboxes-buttons
       (progn (goto-char beg) (beginning-of-line) (point))
       (progn (goto-char end) (forward-line 1) (point))))))

(defun markdown-remove-gfm-checkbox-overlays ()
  "Remove all GFM checkbox overlays in buffer."
  (save-excursion
    (save-restriction
      (widen)
      (remove-overlays nil nil 'face 'markdown-gfm-checkbox-face))))


;;; Display inline image ======================================================

(defvar-local markdown-inline-image-overlays nil)

(defun markdown-remove-inline-images ()
  "Remove inline image overlays from image links in the buffer.
This can be toggled with `markdown-toggle-inline-images'
or \\[markdown-toggle-inline-images]."
  (interactive)
  (mapc #'delete-overlay markdown-inline-image-overlays)
  (setq markdown-inline-image-overlays nil)
  (when (fboundp 'clear-image-cache) (clear-image-cache)))

(defcustom markdown-display-remote-images nil
  "If non-nil, download and display remote images.
See also `markdown-inline-image-overlays'.

Only image URLs specified with a protocol listed in
`markdown-remote-image-protocols' are displayed."
  :group 'markdown
  :type 'boolean)

(defcustom markdown-remote-image-protocols '("https")
  "List of protocols to use to download remote images.
See also `markdown-display-remote-images'."
  :group 'markdown
  :type '(repeat string))

(defvar markdown--remote-image-cache
  (make-hash-table :test 'equal)
  "A map from URLs to image paths.")

(defun markdown--get-remote-image (url)
  "Retrieve the image path for a given URL."
  (or (gethash url markdown--remote-image-cache)
      (let ((dl-path (make-temp-file "markdown-mode--image")))
        (require 'url)
        (url-copy-file url dl-path t)
        (puthash url dl-path markdown--remote-image-cache))))

(defun markdown-display-inline-images ()
  "Add inline image overlays to image links in the buffer.
This can be toggled with `markdown-toggle-inline-images'
or \\[markdown-toggle-inline-images]."
  (interactive)
  (unless (display-images-p)
    (error "Cannot show images"))
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (while (re-search-forward markdown-regex-link-inline nil t)
        (let* ((start (match-beginning 0))
               (imagep (match-beginning 1))
               (end (match-end 0))
               (file (match-string-no-properties 6)))
          (when (and imagep
                     (not (zerop (length file))))
            (unless (file-exists-p file)
              (let* ((download-file (funcall markdown-translate-filename-function file))
                     (valid-url (ignore-errors
                                  (member (downcase (url-type (url-generic-parse-url download-file)))
                                          markdown-remote-image-protocols))))
                (if (and markdown-display-remote-images valid-url)
                    (setq file (markdown--get-remote-image download-file))
                  (when (not valid-url)
                    ;; strip query parameter
                    (setq file (replace-regexp-in-string "?.+\\'" "" file))
                    (unless (file-exists-p file)
                      (setq file (url-unhex-string file)))))))
            (when (file-exists-p file)
              (let* ((abspath (if (file-name-absolute-p file)
                                  file
                                (concat default-directory file)))
                     (image
                      (cond ((and markdown-max-image-size
                                  (image-type-available-p 'imagemagick))
                             (create-image
                              abspath 'imagemagick nil
                              :max-width (car markdown-max-image-size)
                              :max-height (cdr markdown-max-image-size)))
                            (markdown-max-image-size
                             (create-image abspath nil nil
                                           :max-width (car markdown-max-image-size)
                                           :max-height (cdr markdown-max-image-size)))
                            (t (create-image abspath)))))
                (when image
                  (let ((ov (make-overlay start end)))
                    (overlay-put ov 'display image)
                    (overlay-put ov 'face 'default)
                    (push ov markdown-inline-image-overlays)))))))))))

(defun markdown-toggle-inline-images ()
  "Toggle inline image overlays in the buffer."
  (interactive)
  (if markdown-inline-image-overlays
      (markdown-remove-inline-images)
    (markdown-display-inline-images)))


;;; GFM Code Block Fontification ==============================================

(defcustom markdown-fontify-code-blocks-natively nil
  "When non-nil, fontify code in code blocks using the native major mode.
This only works for fenced code blocks where the language is
specified where we can automatically determine the appropriate
mode to use.  The language to mode mapping may be customized by
setting the variable `markdown-code-lang-modes'."
  :group 'markdown
  :type 'boolean
  :safe #'booleanp
  :package-version '(markdown-mode . "2.3"))

(defcustom markdown-fontify-code-block-default-mode nil
  "Default mode to use to fontify code blocks.
This mode is used when automatic detection fails, such as for GFM
code blocks with no language specified."
  :group 'markdown
  :type '(choice function (const :tag "None" nil))
  :package-version '(markdown-mode . "2.4"))

(defun markdown-toggle-fontify-code-blocks-natively (&optional arg)
  "Toggle the native fontification of code blocks.
With a prefix argument ARG, enable if ARG is positive,
and disable otherwise."
  (interactive (list (or current-prefix-arg 'toggle)))
  (setq markdown-fontify-code-blocks-natively
        (if (eq arg 'toggle)
            (not markdown-fontify-code-blocks-natively)
          (> (prefix-numeric-value arg) 0)))
  (if markdown-fontify-code-blocks-natively
      (message "markdown-mode native code block fontification enabled")
    (message "markdown-mode native code block fontification disabled"))
  (markdown-reload-extensions))

;; This is based on `org-src-lang-modes' from org-src.el
(defcustom markdown-code-lang-modes
  '(("ocaml" . tuareg-mode) ("elisp" . emacs-lisp-mode) ("ditaa" . artist-mode)
    ("asymptote" . asy-mode) ("dot" . fundamental-mode) ("sqlite" . sql-mode)
    ("calc" . fundamental-mode) ("C" . c-mode) ("cpp" . c++-mode)
    ("C++" . c++-mode) ("screen" . shell-script-mode) ("shell" . sh-mode)
    ("bash" . sh-mode))
  "Alist mapping languages to their major mode.
The key is the language name, the value is the major mode.  For
many languages this is simple, but for language where this is not
the case, this variable provides a way to simplify things on the
user side.  For example, there is no ocaml-mode in Emacs, but the
mode to use is `tuareg-mode'."
  :group 'markdown
  :type '(repeat
          (cons
           (string "Language name")
           (symbol "Major mode")))
  :package-version '(markdown-mode . "2.3"))

(defun markdown-get-lang-mode (lang)
  "Return major mode that should be used for LANG.
LANG is a string, and the returned major mode is a symbol."
  (cl-find-if
   #'markdown--lang-mode-predicate
   (nconc (list (cdr (assoc lang markdown-code-lang-modes))
                (cdr (assoc (downcase lang) markdown-code-lang-modes)))
          (and (fboundp 'treesit-language-available-p)
               (list (and (treesit-language-available-p (intern lang))
                          (intern (concat lang "-ts-mode")))
                     (and (treesit-language-available-p (intern (downcase lang)))
                          (intern (concat (downcase lang) "-ts-mode")))))
          (list
           (intern (concat lang "-mode"))
           (intern (concat (downcase lang) "-mode"))))))

(defun markdown--lang-mode-predicate (mode)
  (and mode
       (fboundp mode)
       (or
        ;; https://github.com/jrblevin/markdown-mode/issues/787
        ;; major-mode-remap-alist was introduced at Emacs 29.1
        (cl-loop for pair in (bound-and-true-p major-mode-remap-alist)
                 for func = (cdr pair)
                 thereis (and (atom func) (eq mode func)))
        ;; https://github.com/jrblevin/markdown-mode/issues/761
        (cl-loop for pair in auto-mode-alist
                 for func = (cdr pair)
                 thereis (and (atom func) (eq mode func))))))

(defun markdown-fontify-code-blocks-generic (matcher last)
  "Add text properties to next code block from point to LAST.
Use matching function MATCHER."
  (when (funcall matcher last)
    (save-excursion
      (save-match-data
        (let* ((start (match-beginning 0))
               (end (match-end 0))
               ;; Find positions outside opening and closing backquotes.
               (bol-prev (progn (goto-char start)
                                (if (bolp) (line-beginning-position 0) (line-beginning-position))))
               (eol-next (progn (goto-char end)
                                (if (bolp) (line-beginning-position 2) (line-beginning-position 3))))
               lang)
          (if (and markdown-fontify-code-blocks-natively
                   (or (setq lang (markdown-code-block-lang))
                       markdown-fontify-code-block-default-mode))
              (markdown-fontify-code-block-natively lang start end)
            (add-text-properties start end '(face markdown-pre-face)))
          ;; Set background for block as well as opening and closing lines.
          (font-lock-append-text-property
           bol-prev eol-next 'face 'markdown-code-face)
          ;; Set invisible property for lines before and after, including newline.
          (add-text-properties bol-prev start '(invisible markdown-markup))
          (add-text-properties end eol-next '(invisible markdown-markup)))))
    t))

(defun markdown-fontify-gfm-code-blocks (last)
  "Add text properties to next GFM code block from point to LAST."
  (markdown-fontify-code-blocks-generic 'markdown-match-gfm-code-blocks last))

(defun markdown-fontify-fenced-code-blocks (last)
  "Add text properties to next tilde fenced code block from point to LAST."
  (markdown-fontify-code-blocks-generic 'markdown-match-fenced-code-blocks last))

;; Based on `org-src-font-lock-fontify-block' from org-src.el.
(defun markdown-fontify-code-block-natively (lang start end)
  "Fontify given GFM or fenced code block.
This function is called by Emacs for automatic fontification when
`markdown-fontify-code-blocks-natively' is non-nil.  LANG is the
language used in the block. START and END specify the block
position."
  (let ((lang-mode (if lang (markdown-get-lang-mode lang)
                     markdown-fontify-code-block-default-mode)))
    (when (fboundp lang-mode)
      (let ((string (buffer-substring-no-properties start end))
            (modified (buffer-modified-p))
            (markdown-buffer (current-buffer)) pos next)
        (remove-text-properties start end '(face nil))
        (with-current-buffer
            (get-buffer-create
             (concat " markdown-code-fontification:" (symbol-name lang-mode)))
          ;; Make sure that modification hooks are not inhibited in
          ;; the org-src-fontification buffer in case we're called
          ;; from `jit-lock-function' (Bug#25132).
          (let ((inhibit-modification-hooks nil))
            (delete-region (point-min) (point-max))
            (insert string " ")) ;; so there's a final property change
          (unless (eq major-mode lang-mode) (funcall lang-mode))
          (font-lock-ensure)
          (setq pos (point-min))
          (while (setq next (next-single-property-change pos 'face))
            (let ((val (get-text-property pos 'face)))
              (when val
                (put-text-property
                 (+ start (1- pos)) (1- (+ start next)) 'face
                 val markdown-buffer)))
            (setq pos next)))
        (add-text-properties
         start end
         '(font-lock-fontified t fontified t font-lock-multiline t))
        (set-buffer-modified-p modified)))))

(require 'edit-indirect nil t)
(defvar edit-indirect-guess-mode-function)
(defvar edit-indirect-after-commit-functions)

(defun markdown--edit-indirect-after-commit-function (beg end)
  "Corrective logic run on code block content from lines BEG to END.
Restores code block indentation from BEG to END, and ensures trailing newlines
at the END of code blocks."
  ;; ensure trailing newlines
  (goto-char end)
  (unless (eq (char-before) ?\n)
    (insert "\n"))
  ;; restore code block indentation
  (goto-char (- beg 1))
  (let ((block-indentation (current-indentation)))
    (when (> block-indentation 0)
      (indent-rigidly beg end block-indentation)))
  (font-lock-ensure))

(defun markdown-edit-code-block ()
  "Edit Markdown code block in an indirect buffer."
  (interactive)
  (save-excursion
    (if (fboundp 'edit-indirect-region)
        (let* ((bounds (markdown-get-enclosing-fenced-block-construct))
               (begin (and bounds (not (null (nth 0 bounds))) (goto-char (nth 0 bounds)) (line-beginning-position 2)))
               (end (and bounds(not (null (nth 1 bounds)))  (goto-char (nth 1 bounds)) (line-beginning-position 1))))
          (if (and begin end)
              (let* ((indentation (and (goto-char (nth 0 bounds)) (current-indentation)))
                     (lang (markdown-code-block-lang))
                     (mode (or (and lang (markdown-get-lang-mode lang))
                               markdown-edit-code-block-default-mode))
                     (edit-indirect-guess-mode-function
                      (lambda (_parent-buffer _beg _end)
                        (funcall mode)))
                     (indirect-buf (edit-indirect-region begin end 'display-buffer)))
                ;; reset `sh-shell' when indirect buffer
                (when (and (not (member system-type '(ms-dos windows-nt)))
                           (member mode '(shell-script-mode sh-mode))
                           (member lang (append
                                         (mapcar (lambda (e) (symbol-name (car e)))
                                                 sh-ancestor-alist)
                                         '("csh" "rc" "sh"))))
                  (with-current-buffer indirect-buf
                    (sh-set-shell lang)))
                (when (> indentation 0) ;; un-indent in edit-indirect buffer
                  (with-current-buffer indirect-buf
                    (indent-rigidly (point-min) (point-max) (- indentation)))))
            (user-error "Not inside a GFM or tilde fenced code block")))
      (when (y-or-n-p "Package edit-indirect needed to edit code blocks. Install it now? ")
        (progn (package-refresh-contents)
               (package-install 'edit-indirect)
               (markdown-edit-code-block))))))


;;; Table Editing =============================================================

;; These functions were originally adapted from `org-table.el'.

;; General helper functions

(defmacro markdown--with-gensyms (symbols &rest body)
  (declare (debug (sexp body)) (indent 1))
  `(let ,(mapcar (lambda (s)
                   `(,s (make-symbol (concat "--" (symbol-name ',s)))))
                 symbols)
     ,@body))

(defun markdown--split-string (string &optional separators)
  "Splits STRING into substrings at SEPARATORS.
SEPARATORS is a regular expression. If nil it defaults to
`split-string-default-separators'. This version returns no empty
strings if there are matches at the beginning and end of string."
  (let ((start 0) notfirst list)
    (while (and (string-match
                 (or separators split-string-default-separators)
                 string
                 (if (and notfirst
                          (= start (match-beginning 0))
                          (< start (length string)))
                     (1+ start) start))
                (< (match-beginning 0) (length string)))
      (setq notfirst t)
      (or (eq (match-beginning 0) 0)
          (and (eq (match-beginning 0) (match-end 0))
               (eq (match-beginning 0) start))
          (push (substring string start (match-beginning 0)) list))
      (setq start (match-end 0)))
    (or (eq start (length string))
        (push (substring string start) list))
    (nreverse list)))

(defun markdown--string-width (s)
  "Return width of string S.
This version ignores characters with invisibility property
`markdown-markup'."
  (let (b)
    (when (or (eq t buffer-invisibility-spec)
              (member 'markdown-markup buffer-invisibility-spec))
      (while (setq b (text-property-any
                      0 (length s)
                      'invisible 'markdown-markup s))
        (setq s (concat
                 (substring s 0 b)
                 (substring s (or (next-single-property-change
                                   b 'invisible s)
                                  (length s))))))))
  (string-width s))

(defun markdown--remove-invisible-markup (s)
  "Remove Markdown markup from string S.
This version removes characters with invisibility property
`markdown-markup'."
  (let (b)
    (while (setq b (text-property-any
                    0 (length s)
                    'invisible 'markdown-markup s))
      (setq s (concat
               (substring s 0 b)
               (substring s (or (next-single-property-change
                                 b 'invisible s)
                                (length s)))))))
  s)

;; Functions for maintaining tables

(defvar markdown-table-at-point-p-function #'markdown--table-at-point-p
  "Function to decide if point is inside a table.

The indirection serves to differentiate between standard markdown
tables and gfm tables which are less strict about the markup.")

(defconst markdown-table-line-regexp "^[ \t]*|"
  "Regexp matching any line inside a table.")

(defconst markdown-table-hline-regexp "^[ \t]*|[-:]"
  "Regexp matching hline inside a table.")

(defconst markdown-table-dline-regexp "^[ \t]*|[^-:]"
  "Regexp matching dline inside a table.")

(defun markdown-table-at-point-p ()
  "Return non-nil when point is inside a table."
  (funcall markdown-table-at-point-p-function))

(defun markdown--table-at-point-p ()
  "Return non-nil when point is inside a table."
  (save-excursion
    (beginning-of-line)
    (and (looking-at-p markdown-table-line-regexp)
         (not (markdown-code-block-at-point-p)))))

(defconst gfm-table-line-regexp "^.?*|"
  "Regexp matching any line inside a table.")

(defconst gfm-table-hline-regexp "^-+\\(|-\\)+"
  "Regexp matching hline inside a table.")

;; GFM simplified tables syntax is as follows:
;; - A header line for the column names, this is any text
;;   separated by `|'.
;; - Followed by a string -|-|- ..., the number of dashes is optional
;;   but must be higher than 1. The number of separators should match
;;   the number of columns.
;; - Followed by the rows of data, which has the same format as the
;;   header line.
;; Example:
;;
;; foo | bar
;; ------|---------
;; bar | baz
;; bar | baz
(defun gfm--table-at-point-p ()
  "Return non-nil when point is inside a gfm-compatible table."
  (or (markdown--table-at-point-p)
      (save-excursion
        (beginning-of-line)
        (when (looking-at-p gfm-table-line-regexp)
          ;; we might be at the first line of the table, check if the
          ;; line below is the hline
          (or (save-excursion
                (forward-line 1)
                (looking-at-p gfm-table-hline-regexp))
              ;; go up to find the header
              (catch 'done
                (while (looking-at-p gfm-table-line-regexp)
                  (cond
                   ((looking-at-p gfm-table-hline-regexp)
                    (throw 'done t))
                   ((bobp)
                    (throw 'done nil)))
                  (forward-line -1))
                nil))))))

(defun markdown-table-hline-at-point-p ()
  "Return non-nil when point is on a hline in a table.
This function assumes point is on a table."
  (save-excursion
    (beginning-of-line)
    (looking-at-p markdown-table-hline-regexp)))

(defun markdown-table-begin ()
  "Find the beginning of the table and return its position.
This function assumes point is on a table."
  (save-excursion
    (while (and (not (bobp))
                (markdown-table-at-point-p))
      (forward-line -1))
    (unless (or (eobp)
                (markdown-table-at-point-p))
      (forward-line 1))
    (point)))

(defun markdown-table-end ()
  "Find the end of the table and return its position.
This function assumes point is on a table."
  (save-excursion
    (while (and (not (eobp))
                (markdown-table-at-point-p))
      (forward-line 1))
    (point)))

(defun markdown-table-get-dline ()
  "Return index of the table data line at point.
This function assumes point is on a table."
  (let ((pos (point)) (end (markdown-table-end)) (cnt 0))
    (save-excursion
      (goto-char (markdown-table-begin))
      (while (and (re-search-forward
                   markdown-table-dline-regexp end t)
                  (setq cnt (1+ cnt))
                  (< (line-end-position) pos))))
    cnt))

(defun markdown--thing-at-wiki-link (pos)
  (when markdown-enable-wiki-links
    (save-excursion
      (save-match-data
        (goto-char pos)
        (thing-at-point-looking-at markdown-regex-wiki-link)))))

(defun markdown-table-get-column ()
  "Return table column at point.
This function assumes point is on a table."
  (let ((pos (point)) (cnt 0))
    (save-excursion
      (beginning-of-line)
      (while (search-forward "|" pos t)
        (when (and (not (looking-back "\\\\|" (line-beginning-position)))
                   (not (markdown--thing-at-wiki-link (match-beginning 0))))
          (setq cnt (1+ cnt)))))
    cnt))

(defun markdown-table-get-cell (&optional n)
  "Return the content of the cell in column N of current row.
N defaults to column at point. This function assumes point is on
a table."
  (and n (markdown-table-goto-column n))
  (skip-chars-backward "^|\n") (backward-char 1)
  (if (looking-at "|[^|\r\n]*")
      (let* ((pos (match-beginning 0))
             (val (buffer-substring (1+ pos) (match-end 0))))
        (goto-char (min (line-end-position) (+ 2 pos)))
        ;; Trim whitespaces
        (setq val (replace-regexp-in-string "\\`[ \t]+" "" val)
              val (replace-regexp-in-string "[ \t]+\\'" "" val)))
    (forward-char 1) ""))

(defun markdown-table-goto-dline (n)
  "Go to the Nth data line in the table at point.
Return t when the line exists, nil otherwise. This function
assumes point is on a table."
  (goto-char (markdown-table-begin))
  (let ((end (markdown-table-end)) (cnt 0))
    (while (and (re-search-forward
                 markdown-table-dline-regexp end t)
                (< (setq cnt (1+ cnt)) n)))
    (= cnt n)))

(defun markdown-table-goto-column (n &optional on-delim)
  "Go to the Nth column in the table line at point.
With optional argument ON-DELIM, stop with point before the left
delimiter of the cell. If there are less than N cells, just go
beyond the last delimiter. This function assumes point is on a
table."
  (beginning-of-line 1)
  (when (> n 0)
    (while (and (> n 0) (search-forward "|" (line-end-position) t))
      (when (and (not (looking-back "\\\\|" (line-beginning-position)))
                 (not (markdown--thing-at-wiki-link (match-beginning 0))))
        (cl-decf n)))
    (if on-delim
        (backward-char 1)
      (when (looking-at " ") (forward-char 1)))))

(defmacro markdown-table-save-cell (&rest body)
  "Save cell at point, execute BODY and restore cell.
This function assumes point is on a table."
  (declare (debug (body)))
  (markdown--with-gensyms (line column)
    `(let ((,line (copy-marker (line-beginning-position)))
           (,column (markdown-table-get-column)))
       (unwind-protect
           (progn ,@body)
         (goto-char ,line)
         (markdown-table-goto-column ,column)
         (set-marker ,line nil)))))

(defun markdown-table-blank-line (s)
  "Convert a table line S into a line with blank cells."
  (if (string-match "^[ \t]*|-" s)
      (setq s (mapconcat
               (lambda (x) (if (member x '(?| ?+)) "|" " "))
               s ""))
    (with-temp-buffer
      (insert s)
      (goto-char (point-min))
      (when (re-search-forward "|" nil t)
        (let ((cur (point))
              ret)
          (while (re-search-forward "|" nil t)
            (when (and (not (eql (char-before (match-beginning 0)) ?\\))
                       (not (markdown--thing-at-wiki-link (match-beginning 0))))
              (push (make-string (- (match-beginning 0) cur) ? ) ret)
              (setq cur (match-end 0))))
          (format "|%s|" (string-join (nreverse ret) "|")))))))

(defun markdown-table-colfmt (fmtspec)
  "Process column alignment specifier FMTSPEC for tables."
  (when (stringp fmtspec)
    (mapcar (lambda (x)
              (cond ((string-match-p "^:.*:$" x) 'c)
                    ((string-match-p "^:"     x) 'l)
                    ((string-match-p ":$"     x) 'r)
                    (t 'd)))
            (markdown--split-string fmtspec "\\s-*|\\s-*"))))

(defun markdown--first-column-p (bar-pos)
  (save-excursion
    (save-match-data
      (goto-char bar-pos)
      (looking-back "^\\s-*" (line-beginning-position)))))

(defun markdown--table-line-to-columns (line)
  (with-temp-buffer
    (insert line)
    (goto-char (point-min))
    (let ((cur (point))
          ret)
      (while (and (re-search-forward "\\s-*\\(|\\)\\s-*" nil t))
        (when (not (markdown--face-p (match-beginning 1) '(markdown-inline-code-face)))
          (if (markdown--first-column-p (match-beginning 1))
              (setq cur (match-end 0))
            (cond ((eql (char-before (match-beginning 1)) ?\\)
                   ;; keep spaces
                   (goto-char (match-end 1)))
                  ((markdown--thing-at-wiki-link (match-beginning 1))) ;; do nothing
                  (t
                   (push (buffer-substring-no-properties cur (match-beginning 0)) ret)
                   (setq cur (match-end 0)))))))
      (when (< cur (length line))
        (push (buffer-substring-no-properties cur (point-max)) ret))
      (nreverse ret))))

(defsubst markdown--is-delimiter-row (line)
  (and (string-match-p "\\`[ \t]*|[ \t]*[-:]" line)
       (cl-loop for c across line
                always (member c '(?| ?- ?: ?\t ? )))))

(defun markdown-table-align ()
  "Align table at point.
This function assumes point is on a table."
  (interactive)
  (let ((begin (markdown-table-begin))
        (end (copy-marker (markdown-table-end))))
    (markdown-table-save-cell
     (goto-char begin)
     (let* (fmtspec
            ;; Store table indent
            (indent (progn (looking-at "[ \t]*") (match-string 0)))
            ;; Split table in lines and save column format specifier
            (lines (mapcar (lambda (line)
                             (if (markdown--is-delimiter-row line)
                                 (progn (setq fmtspec (or fmtspec line)) nil)
                               line))
                           (markdown--split-string (buffer-substring begin end) "\n")))
            ;; Split lines in cells
            (cells (mapcar (lambda (l) (markdown--table-line-to-columns l))
                           (remq nil lines)))
            ;; Calculate maximum number of cells in a line
            (maxcells (if cells
                          (apply #'max (mapcar #'length cells))
                        (user-error "Empty table")))
            ;; Empty cells to fill short lines
            (emptycells (make-list maxcells ""))
            maxwidths)
       ;; Calculate maximum width for each column
       (dotimes (i maxcells)
         (let ((column (mapcar (lambda (x) (or (nth i x) "")) cells)))
           (push (apply #'max 1 (mapcar #'markdown--string-width column))
                 maxwidths)))
       (setq maxwidths (nreverse maxwidths))
       ;; Process column format specifier
       (setq fmtspec (markdown-table-colfmt fmtspec))
       ;; Compute formats needed for output of table lines
       (let ((hfmt (concat indent "|"))
             (rfmt (concat indent "|"))
             hfmt1 rfmt1 fmt)
         (dolist (width maxwidths (setq hfmt (concat (substring hfmt 0 -1) "|")))
           (setq fmt (pop fmtspec))
           (cond ((equal fmt 'l) (setq hfmt1 ":%s-|" rfmt1 " %%-%ds |"))
                 ((equal fmt 'r) (setq hfmt1 "-%s:|" rfmt1  " %%%ds |"))
                 ((equal fmt 'c) (setq hfmt1 ":%s:|" rfmt1 " %%-%ds |"))
                 (t              (setq hfmt1 "-%s-|" rfmt1 " %%-%ds |")))
           (setq rfmt (concat rfmt (format rfmt1 width)))
           (setq hfmt (concat hfmt (format hfmt1 (make-string width ?-)))))
         ;; Replace modified lines only
         (dolist (line lines)
           (let ((line (if line
                           (apply #'format rfmt (append (pop cells) emptycells))
                         hfmt))
                 (previous (buffer-substring (point) (line-end-position))))
             (if (equal previous line)
                 (forward-line)
               (insert line "\n")
               (delete-region (point) (line-beginning-position 2))))))
       (set-marker end nil)))))

(defun markdown-table-insert-row (&optional arg)
  "Insert a new row above the row at point into the table.
With optional argument ARG, insert below the current row."
  (interactive "P")
  (unless (markdown-table-at-point-p)
    (user-error "Not at a table"))
  (let* ((line (buffer-substring
                (line-beginning-position) (line-end-position)))
         (new (markdown-table-blank-line line)))
    (beginning-of-line (if arg 2 1))
    (unless (bolp) (insert "\n"))
    (insert-before-markers new "\n")
    (beginning-of-line 0)
    (re-search-forward "| ?" (line-end-position) t)))

(defun markdown-table-delete-row ()
  "Delete row or horizontal line at point from the table."
  (interactive)
  (unless (markdown-table-at-point-p)
    (user-error "Not at a table"))
  (let ((col (current-column)))
    (kill-region (line-beginning-position)
                 (min (1+ (line-end-position)) (point-max)))
    (unless (markdown-table-at-point-p) (beginning-of-line 0))
    (move-to-column col)))

(defun markdown-table-move-row (&optional up)
  "Move table line at point down.
With optional argument UP, move it up."
  (interactive "P")
  (unless (markdown-table-at-point-p)
    (user-error "Not at a table"))
  (let* ((col (current-column)) (pos (point))
         (tonew (if up 0 2)) txt)
    (beginning-of-line tonew)
    (unless (markdown-table-at-point-p)
      (goto-char pos) (user-error "Cannot move row further"))
    (goto-char pos) (beginning-of-line 1) (setq pos (point))
    (setq txt (buffer-substring (point) (1+ (line-end-position))))
    (delete-region (point) (1+ (line-end-position)))
    (beginning-of-line tonew)
    (insert txt) (beginning-of-line 0)
    (move-to-column col)))

(defun markdown-table-move-row-up ()
  "Move table row at point up."
  (interactive)
  (markdown-table-move-row 'up))

(defun markdown-table-move-row-down ()
  "Move table row at point down."
  (interactive)
  (markdown-table-move-row nil))

(defun markdown-table-insert-column ()
  "Insert a new table column."
  (interactive)
  (unless (markdown-table-at-point-p)
    (user-error "Not at a table"))
  (let* ((col (max 1 (markdown-table-get-column)))
         (begin (markdown-table-begin))
         (end (copy-marker (markdown-table-end))))
    (markdown-table-save-cell
     (goto-char begin)
     (while (< (point) end)
       (markdown-table-goto-column col t)
       (if (markdown-table-hline-at-point-p)
           (insert "|---")
         (insert "|   "))
       (forward-line)))
    (set-marker end nil)
    (when markdown-table-align-p
      (markdown-table-align))))

(defun markdown-table-delete-column ()
  "Delete column at point from table."
  (interactive)
  (unless (markdown-table-at-point-p)
    (user-error "Not at a table"))
  (let ((col (markdown-table-get-column))
        (begin (markdown-table-begin))
        (end (copy-marker (markdown-table-end))))
    (markdown-table-save-cell
     (goto-char begin)
     (while (< (point) end)
       (markdown-table-goto-column col t)
       (and (looking-at "|\\(?:\\\\|\\|[^|\n]\\)+|")
            (replace-match "|"))
       (forward-line)))
    (set-marker end nil)
    (markdown-table-goto-column (max 1 (1- col)))
    (when markdown-table-align-p
      (markdown-table-align))))

(defun markdown-table-move-column (&optional left)
  "Move table column at point to the right.
With optional argument LEFT, move it to the left."
  (interactive "P")
  (unless (markdown-table-at-point-p)
    (user-error "Not at a table"))
  (let* ((col (markdown-table-get-column))
         (col1 (if left (1- col) col))
         (colpos (if left (1- col) (1+ col)))
         (begin (markdown-table-begin))
         (end (copy-marker (markdown-table-end))))
    (when (and left (= col 1))
      (user-error "Cannot move column further left"))
    (when (and (not left) (looking-at "[^|\n]*|[^|\n]*$"))
      (user-error "Cannot move column further right"))
    (markdown-table-save-cell
     (goto-char begin)
     (while (< (point) end)
       (markdown-table-goto-column col1 t)
       (when (looking-at "|\\(\\(?:\\\\|\\|[^|\n]\\|\\)+\\)|\\(\\(?:\\\\|\\|[^|\n]\\|\\)+\\)|")
         (replace-match "|\\2|\\1|"))
       (forward-line)))
    (set-marker end nil)
    (markdown-table-goto-column colpos)
    (when markdown-table-align-p
      (markdown-table-align))))

(defun markdown-table-move-column-left ()
  "Move table column at point to the left."
  (interactive)
  (markdown-table-move-column 'left))

(defun markdown-table-move-column-right ()
  "Move table column at point to the right."
  (interactive)
  (markdown-table-move-column nil))

(defun markdown-table-next-row ()
  "Go to the next row (same column) in the table.
Create new table lines if required."
  (interactive)
  (unless (markdown-table-at-point-p)
    (user-error "Not at a table"))
  (if (or (looking-at "[ \t]*$")
          (save-excursion (skip-chars-backward " \t") (bolp)))
      (newline)
    (when markdown-table-align-p
      (markdown-table-align))
    (let ((col (markdown-table-get-column)))
      (beginning-of-line 2)
      (if (or (not (markdown-table-at-point-p))
              (markdown-table-hline-at-point-p))
          (progn
            (beginning-of-line 0)
            (markdown-table-insert-row 'below)))
      (markdown-table-goto-column col)
      (skip-chars-backward "^|\n\r")
      (when (looking-at " ") (forward-char 1)))))

(defun markdown-table-forward-cell ()
  "Go to the next cell in the table.
Create new table lines if required."
  (interactive)
  (unless (markdown-table-at-point-p)
    (user-error "Not at a table"))
  (when markdown-table-align-p
    (markdown-table-align))
  (let ((end (markdown-table-end)))
    (when (markdown-table-hline-at-point-p) (end-of-line 1))
    (condition-case nil
        (progn
          (re-search-forward "\\(?:^\\|[^\\]\\)|" end)
          (when (looking-at "[ \t]*$")
            (re-search-forward "\\(?:^\\|[^\\]:\\)|" end))
          (when (and (looking-at "[-:]")
                     (re-search-forward "^\\(?:[ \t]*\\|[^\\]\\)|\\([^-:]\\)" end t))
            (goto-char (match-beginning 1)))
          (if (looking-at "[-:]")
              (progn
                (beginning-of-line 0)
                (markdown-table-insert-row 'below))
            (when (looking-at " ") (forward-char 1))))
      (error (markdown-table-insert-row 'below)))))

(defun markdown-table-backward-cell ()
  "Go to the previous cell in the table."
  (interactive)
  (unless (markdown-table-at-point-p)
    (user-error "Not at a table"))
  (when markdown-table-align-p
    (markdown-table-align))
  (when (markdown-table-hline-at-point-p) (beginning-of-line 1))
  (condition-case nil
      (progn
        (re-search-backward "\\(?:^\\|[^\\]\\)|" (markdown-table-begin))
        ;; When this function is called while in the first cell in a
        ;; table, the point will now be at the beginning of a line. In
        ;; this case, we need to move past one additional table
        ;; boundary, the end of the table on the previous line.
        (when (= (point) (line-beginning-position))
          (re-search-backward "\\(?:^\\|[^\\]\\)|" (markdown-table-begin)))
        (re-search-backward "\\(?:^\\|[^\\]\\)|" (markdown-table-begin)))
    (error (user-error "Cannot move to previous table cell")))
  (when (looking-at "\\(?:^\\|[^\\]\\)| ?") (goto-char (match-end 0)))

  ;; This may have dropped point on the hline.
  (when (markdown-table-hline-at-point-p)
    (markdown-table-backward-cell)))

(defun markdown-table-transpose ()
  "Transpose table at point.
Horizontal separator lines will be eliminated."
  (interactive)
  (unless (markdown-table-at-point-p)
    (user-error "Not at a table"))
  (let* ((table (buffer-substring-no-properties
                 (markdown-table-begin) (markdown-table-end)))
         ;; Convert table to Lisp structure
         (table (delq nil
                      (mapcar
                       (lambda (x)
                         (unless (string-match-p
                                  markdown-table-hline-regexp x)
                           (markdown--table-line-to-columns x)))
                       (markdown--split-string table "[ \t]*\n[ \t]*"))))
         (dline_old (markdown-table-get-dline))
         (col_old (markdown-table-get-column))
         (contents (mapcar (lambda (_)
                             (let ((tp table))
                               (mapcar
                                (lambda (_)
                                  (prog1
                                      (pop (car tp))
                                    (setq tp (cdr tp))))
                                table)))
                           (car table))))
    (goto-char (markdown-table-begin))
    (save-excursion
      (re-search-forward "|") (backward-char)
      (delete-region (point) (markdown-table-end))
      (insert (mapconcat
               (lambda(x)
                 (concat "| " (mapconcat 'identity x " | " ) " |\n"))
               contents "")))
    (markdown-table-goto-dline col_old)
    (markdown-table-goto-column dline_old))
  (when markdown-table-align-p
    (markdown-table-align)))

(defun markdown-table-sort-lines (&optional sorting-type)
  "Sort table lines according to the column at point.

The position of point indicates the column to be used for
sorting, and the range of lines is the range between the nearest
horizontal separator lines, or the entire table of no such lines
exist. If point is before the first column, user will be prompted
for the sorting column. If there is an active region, the mark
specifies the first line and the sorting column, while point
should be in the last line to be included into the sorting.

The command then prompts for the sorting type which can be
alphabetically or numerically. Sorting in reverse order is also
possible.

If SORTING-TYPE is specified when this function is called from a
Lisp program, no prompting will take place. SORTING-TYPE must be
a character, any of (?a ?A ?n ?N) where the capital letters
indicate that sorting should be done in reverse order."
  (interactive)
  (unless (markdown-table-at-point-p)
    (user-error "Not at a table"))
  ;; Set sorting type and column used for sorting
  (let ((column (let ((c (markdown-table-get-column)))
                  (cond ((> c 0) c)
                        ((called-interactively-p 'any)
                         (read-number "Use column N for sorting: "))
                        (t 1))))
        (sorting-type
         (or sorting-type
             (progn
               ;; workaround #641
               ;; Emacs < 28 hides prompt message by another message. This erases it.
               (message "")
               (read-char-exclusive
                "Sort type: [a]lpha [n]umeric (A/N means reversed): ")))))
    (save-restriction
      ;; Narrow buffer to appropriate sorting area
      (if (region-active-p)
          (narrow-to-region
           (save-excursion
             (progn
               (goto-char (region-beginning)) (line-beginning-position)))
           (save-excursion
             (progn
               (goto-char (region-end)) (line-end-position))))
        (let ((start (markdown-table-begin))
              (end (markdown-table-end)))
          (narrow-to-region
           (save-excursion
             (if (re-search-backward
                  markdown-table-hline-regexp start t)
                 (line-beginning-position 2)
               start))
           (if (save-excursion (re-search-forward
                                markdown-table-hline-regexp end t))
               (match-beginning 0)
             end))))
      ;; Determine arguments for `sort-subr'
      (let* ((extract-key-from-cell
              (cl-case sorting-type
                ((?a ?A) #'markdown--remove-invisible-markup) ;; #'identity)
                ((?n ?N) #'string-to-number)
                (t (user-error "Invalid sorting type: %c" sorting-type))))
             (predicate
              (cl-case sorting-type
                ((?n ?N) #'<)
                ((?a ?A) #'string<))))
        ;; Sort selected area
        (goto-char (point-min))
        (sort-subr (memq sorting-type '(?A ?N))
                   (lambda ()
                     (forward-line)
                     (while (and (not (eobp))
                                 (not (looking-at
                                       markdown-table-dline-regexp)))
                       (forward-line)))
                   #'end-of-line
                   (lambda ()
                     (funcall extract-key-from-cell
                              (markdown-table-get-cell column)))
                   nil
                   predicate)
        (goto-char (point-min))))))

(defun markdown-table-convert-region (begin end &optional separator)
  "Convert region from BEGIN to END to table with SEPARATOR.

If every line contains at least one TAB character, the function
assumes that the material is tab separated (TSV). If every line
contains a comma, comma-separated values (CSV) are assumed. If
not, lines are split at whitespace into cells.

You can use a prefix argument to force a specific separator:
\\[universal-argument] once forces CSV, \\[universal-argument]
twice forces TAB, and \\[universal-argument] three times will
prompt for a regular expression to match the separator, and a
numeric argument N indicates that at least N consecutive
spaces, or alternatively a TAB should be used as the separator."

  (interactive "r\nP")
  (let* ((begin (min begin end)) (end (max begin end)) re)
    (goto-char begin) (beginning-of-line 1)
    (setq begin (point-marker))
    (goto-char end)
    (if (bolp) (backward-char 1) (end-of-line 1))
    (setq end (point-marker))
    (when (equal separator '(64))
      (setq separator (read-regexp "Regexp for cell separator: ")))
    (unless separator
      ;; Get the right cell separator
      (goto-char begin)
      (setq separator
            (cond
             ((not (re-search-forward "^[^\n\t]+$" end t)) '(16))
             ((not (re-search-forward "^[^\n,]+$" end t)) '(4))
             (t 1))))
    (goto-char begin)
    (if (equal separator '(4))
        ;; Parse CSV
        (while (< (point) end)
          (cond
           ((looking-at "^") (insert "| "))
           ((looking-at "[ \t]*$") (replace-match " |") (beginning-of-line 2))
           ((looking-at "[ \t]*\"\\([^\"\n]*\\)\"")
            (replace-match "\\1") (if (looking-at "\"") (insert "\"")))
           ((looking-at "[^,\n]+") (goto-char (match-end 0)))
           ((looking-at "[ \t]*,") (replace-match " | "))
           (t (beginning-of-line 2))))
      (setq re
            (cond
             ((equal separator '(4))  "^\\|\"?[ \t]*,[ \t]*\"?")
             ((equal separator '(16)) "^\\|\t")
             ((integerp separator)
              (if (< separator 1)
                  (user-error "Cell separator must contain one or more spaces")
                (format "^ *\\| *\t *\\| \\{%d,\\}\\|$" separator)))
             ((stringp separator) (format "^ *\\|%s" separator))
             (t (error "Invalid cell separator"))))
      (let (finish)
        (while (and (not finish) (re-search-forward re end t))
          (if (eolp)
              (progn
                (replace-match "|" t t)
                (forward-line 1)
                (when (eobp)
                  (setq finish t)))
            (replace-match "| " t t)))))
    (goto-char begin)
    (when markdown-table-align-p
      (markdown-table-align))))

(defun markdown-insert-table (&optional rows columns align)
  "Insert an empty pipe table.
Optional arguments ROWS, COLUMNS, and ALIGN specify number of
rows and columns and the column alignment."
  (interactive)
  (let* ((rows (or rows (read-number "Number of Rows: ")))
         (columns (or columns (read-number "Number of Columns: ")))
         (align (or align (read-string "Alignment ([l]eft, [r]ight, [c]enter, or RET for default): ")))
         (align (cond ((equal align "l") ":--")
                      ((equal align "r") "--:")
                      ((equal align "c") ":-:")
                      (t "---")))
         (pos (point))
         (indent (make-string (current-column) ?\ ))
         (line (concat
                (apply 'concat indent "|"
                       (make-list columns "   |")) "\n"))
         (hline (apply 'concat indent "|"
                       (make-list columns (concat align "|")))))
    (if (string-match
         "^[ \t]*$" (buffer-substring-no-properties
                     (line-beginning-position) (point)))
        (beginning-of-line 1)
      (newline))
    (dotimes (_ rows) (insert line))
    (goto-char pos)
    (if (> rows 1)
        (progn
          (end-of-line 1) (insert (concat "\n" hline)) (goto-char pos)))
    (markdown-table-forward-cell)))


;;; ElDoc Support =============================================================

(defun markdown-eldoc-function (&rest _ignored)
  "Return a helpful string when appropriate based on context.
* Report URL when point is at a hidden URL.
* Report language name when point is a code block with hidden markup."
  (cond
   ;; Hidden URL or reference for inline link
   ((and (or (thing-at-point-looking-at markdown-regex-link-inline)
             (thing-at-point-looking-at markdown-regex-link-reference))
         (or markdown-hide-urls markdown-hide-markup))
    (let* ((imagep (string-equal (match-string 1) "!"))
           (referencep (string-equal (match-string 5) "["))
           (link (match-string-no-properties 6))
           (edit-keys (markdown--substitute-command-keys
                       (if imagep
                           "\\[markdown-insert-image]"
                         "\\[markdown-insert-link]")))
           (edit-str (propertize edit-keys 'face 'font-lock-constant-face))
           (object (if referencep "reference" "URL")))
      (format "Hidden %s (%s to edit): %s" object edit-str
              (if referencep
                  (concat
                   (propertize "[" 'face 'markdown-markup-face)
                   (propertize link 'face 'markdown-reference-face)
                   (propertize "]" 'face 'markdown-markup-face))
                (propertize link 'face 'markdown-url-face)))))
   ;; Hidden language name for fenced code blocks
   ((and (markdown-code-block-at-point-p)
         (not (get-text-property (point) 'markdown-pre))
         markdown-hide-markup)
    (let ((lang (save-excursion (markdown-code-block-lang))))
      (unless lang (setq lang "[unspecified]"))
      (format "Hidden code block language: %s (%s to toggle markup)"
              (propertize lang 'face 'markdown-language-keyword-face)
              (markdown--substitute-command-keys
               "\\[markdown-toggle-markup-hiding]"))))))

(defun markdown--image-media-handler (mimetype data)
  (let* ((ext (symbol-name (mailcap-mime-type-to-extension mimetype)))
         (filename (read-string "Insert filename for image: "))
         (link-text (read-string "Link text: "))
         (filepath (file-name-with-extension filename ext))
         (dir (file-name-directory filepath)))
    (when (and dir (not (file-directory-p dir)))
      (make-directory dir t))
    (with-temp-file filepath
      (insert data))
    (when (string-match-p "\\s-" filepath)
      (setq filepath (concat "<" filepath ">")))
    (markdown-insert-inline-image link-text filepath)))

(defun markdown--file-media-handler (_mimetype data)
  (let* ((data (split-string data "[\0\r\n]" t "^file://"))
         (files (cdr data)))
    (while (not (null files))
      (let* ((file (url-unhex-string (car files)))
             (file (file-relative-name file))
             (prompt (format "Link text(%s): " (file-name-nondirectory file)))
             (link-text (read-string prompt)))
        (when (string-match-p "\\s-" file)
          (setq file (concat "<" file ">")))
        (markdown-insert-inline-image link-text file)
        (when (not (null (cdr files)))
          (insert " "))
        (setq files (cdr files))))))

(defun markdown--dnd-local-file-handler (url _action)
  (require 'mailcap)
  (require 'dnd)
  (let* ((filename (dnd-get-local-file-name url))
         (mimetype (mailcap-file-name-to-mime-type filename))
         (file (file-relative-name filename))
         (link-text "link text"))
    (when (string-match-p "\\s-" file)
      (setq file (concat "<" file ">")))
    (if (string-prefix-p "image/" mimetype)
        (markdown-insert-inline-image link-text file)
      (markdown-insert-inline-link link-text file))))


;;; Mode Definition  ==========================================================

(defun markdown-show-version ()
  "Show the version number in the minibuffer."
  (interactive)
  (message "markdown-mode, version %s" markdown-mode-version))

(defun markdown-mode-info ()
  "Open the `markdown-mode' homepage."
  (interactive)
  (browse-url "https://jblevins.org/projects/markdown-mode/"))

;;;###autoload
(define-derived-mode markdown-mode text-mode "Markdown"
  "Major mode for editing Markdown files."
  (when buffer-read-only
    (when (or (not (buffer-file-name)) (file-writable-p (buffer-file-name)))
      (setq-local buffer-read-only nil)))
  ;; Natural Markdown tab width
  (setq tab-width 4)
  ;; Comments
  (setq-local comment-start "<!-- ")
  (setq-local comment-end " -->")
  (setq-local comment-start-skip "<!--[ \t]*")
  (setq-local comment-column 0)
  (setq-local comment-auto-fill-only-comments nil)
  (setq-local comment-use-syntax t)
  ;; Sentence
  (setq-local sentence-end-base "[.?!…‽][]\"'”’)}»›*_`~]*")
  ;; Syntax
  (add-hook 'syntax-propertize-extend-region-functions
            #'markdown-syntax-propertize-extend-region nil t)
  (add-hook 'jit-lock-after-change-extend-region-functions
            #'markdown-font-lock-extend-region-function t t)
  (setq-local syntax-propertize-function #'markdown-syntax-propertize)
  (syntax-propertize (point-max)) ;; Propertize before hooks run, etc.
  ;; Font lock.
  (setq font-lock-defaults
        '(markdown-mode-font-lock-keywords
          nil nil nil nil
          (font-lock-multiline . t)
          (font-lock-syntactic-face-function . markdown-syntactic-face)
          (font-lock-extra-managed-props
           . (composition display invisible rear-nonsticky
                          keymap help-echo mouse-face))))
  (if markdown-hide-markup
      (add-to-invisibility-spec 'markdown-markup)
    (remove-from-invisibility-spec 'markdown-markup))
  ;; Wiki links
  (markdown-setup-wiki-link-hooks)
  ;; Math mode
  (when markdown-enable-math (markdown-toggle-math t))
  ;; Add a buffer-local hook to reload after file-local variables are read
  (add-hook 'hack-local-variables-hook #'markdown-handle-local-variables nil t)
  ;; For imenu support
  (setq imenu-create-index-function
        (if markdown-nested-imenu-heading-index
            #'markdown-imenu-create-nested-index
          #'markdown-imenu-create-flat-index))

  ;; Defun movement
  (setq-local beginning-of-defun-function #'markdown-beginning-of-defun)
  (setq-local end-of-defun-function #'markdown-end-of-defun)
  ;; Paragraph filling
  (setq-local fill-paragraph-function #'markdown-fill-paragraph)
  (setq-local paragraph-start
              ;; Should match start of lines that start or separate paragraphs
              (mapconcat #'identity
                         '(
                           "\f" ; starts with a literal line-feed
                           "[ \t\f]*$" ; space-only line
                           "\\(?:[ \t]*>\\)+[ \t\f]*$"; empty line in blockquote
                           "[ \t]*[*+-][ \t]+" ; unordered list item
                           "[ \t]*\\(?:[0-9]+\\|#\\)\\.[ \t]+" ; ordered list item
                           "[ \t]*\\[\\S-*\\]:[ \t]+" ; link ref def
                           "[ \t]*:[ \t]+" ; definition
                           "^|" ; table or Pandoc line block
                           )
                         "\\|"))
  (setq-local paragraph-separate
              ;; Should match lines that separate paragraphs without being
              ;; part of any paragraph:
              (mapconcat #'identity
                         '("[ \t\f]*$" ; space-only line
                           "\\(?:[ \t]*>\\)+[ \t\f]*$"; empty line in blockquote
                           ;; The following is not ideal, but the Fill customization
                           ;; options really only handle paragraph-starting prefixes,
                           ;; not paragraph-ending suffixes:
                           ".*  $" ; line ending in two spaces
                           "^#+"
                           "^\\(?:   \\)?[-=]+[ \t]*$" ;; setext
                           "[ \t]*\\[\\^\\S-*\\]:[ \t]*$") ; just the start of a footnote def
                         "\\|"))
  (setq-local adaptive-fill-first-line-regexp "\\`[ \t]*[A-Z]?>[ \t]*?\\'")
  (setq-local adaptive-fill-regexp "\\s-*")
  (setq-local adaptive-fill-function #'markdown-adaptive-fill-function)
  (setq-local fill-forward-paragraph-function #'markdown-fill-forward-paragraph)
  ;; Outline mode
  (setq-local outline-regexp markdown-regex-header)
  (setq-local outline-level #'markdown-outline-level)
  ;; Cause use of ellipses for invisible text.
  (add-to-invisibility-spec '(outline . t))
  ;; ElDoc support
  (if (boundp 'eldoc-documentation-functions)
      (add-hook 'eldoc-documentation-functions #'markdown-eldoc-function nil t)
    (add-function :before-until (local 'eldoc-documentation-function)
                  #'markdown-eldoc-function))
  ;; Inhibiting line-breaking:
  ;; Separating out each condition into a separate function so that users can
  ;; override if desired (with remove-hook)
  (add-hook 'fill-nobreak-predicate
            #'markdown-line-is-reference-definition-p nil t)
  (add-hook 'fill-nobreak-predicate
            #'markdown-pipe-at-bol-p nil t)

  ;; Indentation
  (setq-local indent-line-function markdown-indent-function)
  (setq-local indent-region-function #'markdown--indent-region)

  ;; Flyspell
  (setq-local flyspell-generic-check-word-predicate
              #'markdown-flyspell-check-word-p)

  ;; Electric quoting
  (add-hook 'electric-quote-inhibit-functions
            #'markdown--inhibit-electric-quote nil :local)

  ;; drag and drop handler
  (setq-local dnd-protocol-alist  (cons '("^file:///" . markdown--dnd-local-file-handler)
                                        dnd-protocol-alist))

  ;; media handler
  (when (version< "29" emacs-version)
    (yank-media-handler "image/.*" #'markdown--image-media-handler)
    ;; TODO support other than GNOME, like KDE etc
    (yank-media-handler "x-special/gnome-copied-files" #'markdown--file-media-handler))

  ;; Make checkboxes buttons
  (when markdown-make-gfm-checkboxes-buttons
    (markdown-make-gfm-checkboxes-buttons (point-min) (point-max))
    (add-hook 'after-change-functions #'markdown-gfm-checkbox-after-change-function t t)
    (add-hook 'change-major-mode-hook #'markdown-remove-gfm-checkbox-overlays t t))

  ;; edit-indirect
  (add-hook 'edit-indirect-after-commit-functions
            #'markdown--edit-indirect-after-commit-function
            nil 'local)

  ;; Marginalized headings
  (when markdown-marginalize-headers
    (add-hook 'window-configuration-change-hook
              #'markdown-marginalize-update-current nil t))

  ;; add live preview export hook
  (add-hook 'after-save-hook #'markdown-live-preview-if-markdown t t)
  (add-hook 'kill-buffer-hook #'markdown-live-preview-remove-on-kill t t))

;;;###autoload
(add-to-list 'auto-mode-alist
             '("\\.\\(?:md\\|markdown\\|mkd\\|mdown\\|mkdn\\|mdwn\\)\\'" . markdown-mode))


;;; GitHub Flavored Markdown Mode  ============================================

(defun gfm--electric-pair-fence-code-block ()
  (when (and electric-pair-mode
             (not markdown-gfm-use-electric-backquote)
             (eql last-command-event ?`)
             (let ((count 0))
               (while (eql (char-before (- (point) count)) ?`)
                 (cl-incf count))
               (= count 3))
             (eql (char-after) ?`))
    (save-excursion (insert (make-string 2 ?`)))))

(defvar gfm-mode-hook nil
  "Hook run when entering GFM mode.")

;;;###autoload
(define-derived-mode gfm-mode markdown-mode "GFM"
  "Major mode for editing GitHub Flavored Markdown files."
  (setq markdown-link-space-sub-char "-")
  (setq markdown-wiki-link-search-subdirectories t)
  (setq-local markdown-table-at-point-p-function #'gfm--table-at-point-p)
  (add-hook 'post-self-insert-hook #'gfm--electric-pair-fence-code-block 'append t)
  (markdown-gfm-parse-buffer-for-languages))


;;; Viewing modes =============================================================

(defcustom markdown-hide-markup-in-view-modes t
  "Enable hidden markup mode in `markdown-view-mode' and `gfm-view-mode'."
  :group 'markdown
  :type 'boolean
  :safe #'booleanp)

(defvar markdown-view-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "p") #'markdown-outline-previous)
    (define-key map (kbd "n") #'markdown-outline-next)
    (define-key map (kbd "f") #'markdown-outline-next-same-level)
    (define-key map (kbd "b") #'markdown-outline-previous-same-level)
    (define-key map (kbd "u") #'markdown-outline-up)
    (define-key map (kbd "DEL") #'scroll-down-command)
    (define-key map (kbd "SPC") #'scroll-up-command)
    (define-key map (kbd ">") #'end-of-buffer)
    (define-key map (kbd "<") #'beginning-of-buffer)
    (define-key map (kbd "q") #'kill-this-buffer)
    (define-key map (kbd "?") #'describe-mode)
    map)
  "Keymap for `markdown-view-mode'.")

(defun markdown--filter-visible (beg end &optional delete)
  (let ((result "")
        (invisible-faces '(markdown-header-delimiter-face markdown-header-rule-face)))
    (while (< beg end)
      (when (markdown--face-p beg invisible-faces)
        (cl-incf beg)
        (while (and (markdown--face-p beg invisible-faces) (< beg end))
          (cl-incf beg)))
      (let ((next (next-single-char-property-change beg 'invisible)))
        (unless (get-char-property beg 'invisible)
          (setq result (concat result (buffer-substring beg (min end next)))))
        (setq beg next)))
    (prog1 result
      (when delete
        (let ((inhibit-read-only t))
          (delete-region beg end))))))

;;;###autoload
(define-derived-mode markdown-view-mode markdown-mode "Markdown-View"
  "Major mode for viewing Markdown content."
  (setq-local markdown-hide-markup markdown-hide-markup-in-view-modes)
  (add-to-invisibility-spec 'markdown-markup)
  (setq-local filter-buffer-substring-function #'markdown--filter-visible)
  (read-only-mode 1))

(defvar gfm-view-mode-map
  markdown-view-mode-map
  "Keymap for `gfm-view-mode'.")

;;;###autoload
(define-derived-mode gfm-view-mode gfm-mode "GFM-View"
  "Major mode for viewing GitHub Flavored Markdown content."
  (setq-local markdown-hide-markup markdown-hide-markup-in-view-modes)
  (setq-local markdown-fontify-code-blocks-natively t)
  (setq-local filter-buffer-substring-function #'markdown--filter-visible)
  (add-to-invisibility-spec 'markdown-markup)
  (read-only-mode 1))


;;; Live Preview Mode  ========================================================
;;;###autoload
(define-minor-mode markdown-live-preview-mode
  "Toggle native previewing on save for a specific markdown file."
  :lighter " MD-Preview"
  (if markdown-live-preview-mode
      (if (markdown-live-preview-get-filename)
          (markdown-display-buffer-other-window (markdown-live-preview-export))
        (markdown-live-preview-mode -1)
        (user-error "Buffer %s does not visit a file" (current-buffer)))
    (markdown-live-preview-remove)))


(provide 'markdown-mode)

;; Local Variables:
;; indent-tabs-mode: nil
;; coding: utf-8
;; End:
;;; markdown-mode.el ends here
