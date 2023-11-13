;;; htmlize.el --- Convert buffer text and decorations to HTML. -*- lexical-binding: t -*-

;; Copyright (C) 1997-2003,2005,2006,2009,2011,2012,2014,2017,2018,2020 Hrvoje Niksic

;; Author: Hrvoje Niksic <hniksic@gmail.com>
;; Homepage: https://github.com/hniksic/emacs-htmlize
;; Keywords: hypermedia, extensions
;; Version: 1.57

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This package converts the buffer text and the associated
;; decorations to HTML.  Mail to <hniksic@gmail.com> to discuss
;; features and additions.  All suggestions are more than welcome.

;; To use it, just switch to the buffer you want HTML-ized and type
;; `M-x htmlize-buffer'.  You will be switched to a new buffer that
;; contains the resulting HTML code.  You can edit and inspect this
;; buffer, or you can just save it with C-x C-w.  `M-x htmlize-file'
;; will find a file, fontify it, and save the HTML version in
;; FILE.html, without any additional intervention.  `M-x
;; htmlize-many-files' allows you to htmlize any number of files in
;; the same manner.  `M-x htmlize-many-files-dired' does the same for
;; files marked in a dired buffer.

;; htmlize supports three types of HTML output, selected by setting
;; `htmlize-output-type': `css', `inline-css', and `font'.  In `css'
;; mode, htmlize uses cascading style sheets to specify colors; it
;; generates classes that correspond to Emacs faces and uses <span
;; class=FACE>...</span> to color parts of text.  In this mode, the
;; produced HTML is valid under the 4.01 strict DTD, as confirmed by
;; the W3C validator.  `inline-css' is like `css', except the CSS is
;; put directly in the STYLE attribute of the SPAN element, making it
;; possible to paste the generated HTML into existing HTML documents.
;; In `font' mode, htmlize uses <font color="...">...</font> to
;; colorize HTML, which is not standard-compliant, but works better in
;; older browsers.  `css' mode is the default.

;; You can also use htmlize from your Emacs Lisp code.  When called
;; non-interactively, `htmlize-buffer' and `htmlize-region' will
;; return the resulting HTML buffer, but will not change current
;; buffer or move the point.  htmlize will do its best to work on
;; non-windowing Emacs sessions but the result will be limited to
;; colors supported by the terminal.

;; htmlize aims for compatibility with older Emacs versions.  Please
;; let me know if it doesn't work on the version of GNU Emacs that you
;; are using.  The package relies on the presence of CL extensions;
;; please don't try to remove that dependency.  I see no practical
;; problems with using the full power of the CL extensions, except
;; that one might learn to like them too much.

;; The latest version is available at:
;;
;;        <https://github.com/hniksic/emacs-htmlize>
;;        <https://code.orgmode.org/mirrors/emacs-htmlize>
;;

;; Thanks go to the many people who have sent reports and contributed
;; comments, suggestions, and fixes.  They include Ron Gut, Bob
;; Weiner, Toni Drabik, Peter Breton, Ville Skytta, Thomas Vogels,
;; Juri Linkov, Maciek Pasternacki, and many others.

;; User quotes: "You sir, are a sick, sick, _sick_ person. :)"
;;                  -- Bill Perry, author of Emacs/W3


;;; Code:

(require 'cl-lib)
(eval-when-compile
  (defvar font-lock-auto-fontify)
  (defvar font-lock-support-mode)
  (defvar global-font-lock-mode))

(defconst htmlize-version "1.57")

(defgroup htmlize nil
  "Convert buffer text and faces to HTML."
  :group 'hypermedia)

(defcustom htmlize-head-tags ""
  "Additional tags to insert within HEAD of the generated document."
  :type 'string
  :group 'htmlize)

(defcustom htmlize-output-type 'css
  "Output type of generated HTML, one of `css', `inline-css', or `font'.
When set to `css' (the default), htmlize will generate a style sheet
with description of faces, and use it in the HTML document, specifying
the faces in the actual text with <span class=\"FACE\">.

When set to `inline-css', the style will be generated as above, but
placed directly in the STYLE attribute of the span ELEMENT: <span
style=\"STYLE\">.  This makes it easier to paste the resulting HTML to
other documents.

When set to `font', the properties will be set using layout tags
<font>, <b>, <i>, <u>, and <strike>.

`css' output is normally preferred, but `font' is still useful for
supporting old, pre-CSS browsers, and both `inline-css' and `font' for
easier embedding of colorized text in foreign HTML documents (no style
sheet to carry around)."
  :type '(choice (const css) (const inline-css) (const font))
  :group 'htmlize)

(defcustom htmlize-use-images t
  "Whether htmlize generates `img' for images attached to buffer contents."
  :type 'boolean
  :group 'htmlize)

(defcustom htmlize-force-inline-images nil
  "Non-nil means generate all images inline using data URLs.
Normally htmlize converts image descriptors with :file properties to
relative URIs, and those with :data properties to data URIs.  With this
flag set, the images specified as a file name are loaded into memory and
embedded in the HTML as data URIs."
  :type 'boolean
  :group 'htmlize)

(defcustom htmlize-max-alt-text 100
  "Maximum size of text to use as ALT text in images.

Normally when htmlize encounters text covered by the `display' property
that specifies an image, it generates an `alt' attribute containing the
original text.  If the text is larger than `htmlize-max-alt-text' characters,
this will not be done."
  :type 'integer
  :group 'htmlize)

(defcustom htmlize-transform-image 'htmlize-default-transform-image
  "Function called to modify the image descriptor.

The function is called with the image descriptor found in the buffer and
the text the image is supposed to replace.  It should return a (possibly
different) image descriptor property list or a replacement string to use
instead of of the original buffer text.

Returning nil is the same as returning the original text."
  :type 'boolean
  :group 'htmlize)

(defcustom htmlize-generate-hyperlinks t
  "Non-nil means auto-generate the links from URLs and mail addresses in buffer.

This is on by default; set it to nil if you don't want htmlize to
autogenerate such links.  Note that this option only turns off automatic
search for contents that looks like URLs and converting them to links.
It has no effect on whether htmlize respects the `htmlize-link' property."
  :type 'boolean
  :group 'htmlize)

(defcustom htmlize-hyperlink-style "
      a {
        color: inherit;
        background-color: inherit;
        font: inherit;
        text-decoration: inherit;
      }
      a:hover {
        text-decoration: underline;
      }
"
  "The CSS style used for hyperlinks when in CSS mode."
  :type 'string
  :group 'htmlize)

(defcustom htmlize-replace-form-feeds t
  "Non-nil means replace form feeds in source code with HTML separators.
Form feeds are the ^L characters at line beginnings that are sometimes
used to separate sections of source code.  If this variable is set to
`t', form feed characters are replaced with the <hr> separator.  If this
is a string, it specifies the replacement to use.  Note that <pre> is
temporarily closed before the separator is inserted, so the default
replacement is effectively \"</pre><hr /><pre>\".  If you specify
another replacement, don't forget to close and reopen the <pre> if you
want the output to remain valid HTML.

If you need more elaborate processing, set this to nil and use
htmlize-after-hook."
  :type 'boolean
  :group 'htmlize)

(defcustom htmlize-html-charset nil
  "The charset declared by the resulting HTML documents.
When non-nil, causes htmlize to insert the following in the HEAD section
of the generated HTML:

  <meta http-equiv=\"Content-Type\" content=\"text/html; charset=CHARSET\">

where CHARSET is the value you've set for htmlize-html-charset.  Valid
charsets are defined by MIME and include strings like \"iso-8859-1\",
\"iso-8859-15\", \"utf-8\", etc.

If you are using non-Latin-1 charsets, you might need to set this for
your documents to render correctly.  Also, the W3C validator requires
submitted HTML documents to declare a charset.  So if you care about
validation, you can use this to prevent the validator from bitching.

Needless to say, if you set this, you should actually make sure that
the buffer is in the encoding you're claiming it is in.  (This is
normally achieved by using the correct file coding system for the
buffer.)  If you don't understand what that means, you should probably
leave this option in its default setting."
  :type '(choice (const :tag "Unset" nil)
		 string)
  :group 'htmlize)

(defcustom htmlize-convert-nonascii-to-entities t
  "Whether non-ASCII characters should be converted to HTML entities.

When this is non-nil, characters with codes in the 128-255 range will be
considered Latin 1 and rewritten as \"&#CODE;\".  Characters with codes
above 255 will be converted to \"&#UCS;\", where UCS denotes the Unicode
code point of the character.  If the code point cannot be determined,
the character will be copied unchanged, as would be the case if the
option were nil.

When the option is nil, the non-ASCII characters are copied to HTML
without modification.  In that case, the web server and/or the browser
must be set to understand the encoding that was used when saving the
buffer.  (You might also want to specify it by setting
`htmlize-html-charset'.)

Note that in an HTML entity \"&#CODE;\", CODE is always a UCS code point,
which has nothing to do with the charset the page is in.  For example,
\"&#169;\" *always* refers to the copyright symbol, regardless of charset
specified by the META tag or the charset sent by the HTTP server.  In
other words, \"&#169;\" is exactly equivalent to \"&copy;\".

For most people htmlize will work fine with this option left at the
default setting; don't change it unless you know what you're doing."
  :type 'sexp
  :group 'htmlize)

(defcustom htmlize-ignore-face-size 'absolute
  "Whether face size should be ignored when generating HTML.
If this is nil, face sizes are used.  If set to t, sizes are ignored
If set to `absolute', only absolute size specifications are ignored.
Please note that font sizes only work with CSS-based output types."
  :type '(choice (const :tag "Don't ignore" nil)
		 (const :tag "Ignore all" t)
		 (const :tag "Ignore absolute" absolute))
  :group 'htmlize)

(defcustom htmlize-css-name-prefix ""
  "The prefix used for CSS names.
The CSS names that htmlize generates from face names are often too
generic for CSS files; for example, `font-lock-type-face' is transformed
to `type'.  Use this variable to add a prefix to the generated names.
The string \"htmlize-\" is an example of a reasonable prefix."
  :type 'string
  :group 'htmlize)

(defcustom htmlize-use-rgb-txt t
  "Whether `rgb.txt' should be used to convert color names to RGB.

This conversion means determining, for instance, that the color
\"IndianRed\" corresponds to the (205, 92, 92) RGB triple.  `rgb.txt'
is the X color database that maps hundreds of color names to such RGB
triples.  When this variable is non-nil, `htmlize' uses `rgb.txt' to
look up color names.

If this variable is nil, htmlize queries Emacs for RGB components of
colors using `color-instance-rgb-components' and `color-values'.
This can yield incorrect results on non-true-color displays.

If the `rgb.txt' file is not found (which will be the case if you're
running Emacs on non-X11 systems), this option is ignored."
  :type 'boolean
  :group 'htmlize)

(defvar htmlize-face-overrides nil
  "Overrides for face definitions.

Normally face definitions are taken from Emacs settings for fonts
in the current frame.  For faces present in this plist, the
definitions will be used instead.  Keys in the plist are symbols
naming the face and values are the overriding definitions.  For
example:

  (setq htmlize-face-overrides
        '(font-lock-warning-face \"black\"
          font-lock-function-name-face \"red\"
          font-lock-comment-face \"blue\"
          default (:foreground \"dark-green\" :background \"yellow\")))

This variable can be also be `let' bound when running `htmlize-buffer'.")

(defcustom htmlize-untabify t
  "Non-nil means untabify buffer contents during htmlization."
  :type 'boolean
  :group 'htmlize)

(defcustom htmlize-html-major-mode nil
  "The mode the newly created HTML buffer will be put in.
Set this to nil if you prefer the default (fundamental) mode."
  :type '(radio (const :tag "No mode (fundamental)" nil)
		 (function-item html-mode)
		 (function :tag "User-defined major mode"))
  :group 'htmlize)

(defcustom htmlize-pre-style nil
  "When non-nil, `<pre>' tags will be decorated with style
information in `font' and `inline-css' modes. This allows a
consistent background for captures of regions."
  :type 'boolean
  :group 'htmlize)

(defvar htmlize-before-hook nil
  "Hook run before htmlizing a buffer.
The hook functions are run in the source buffer (not the resulting HTML
buffer).")

(defvar htmlize-after-hook nil
  "Hook run after htmlizing a buffer.
Unlike `htmlize-before-hook', these functions are run in the generated
HTML buffer.  You may use them to modify the outlook of the final HTML
output.")

(defvar htmlize-file-hook nil
  "Hook run by `htmlize-file' after htmlizing a file, but before saving it.")

(defvar htmlize-buffer-places)

;;; Some cross-Emacs compatibility.

;; We need a function that efficiently finds the next change of a
;; property regardless of whether the change occurred because of a
;; text property or an extent/overlay.
(defun htmlize-next-change (pos prop &optional limit)
  (if prop
      (next-single-char-property-change pos prop nil limit)
    (next-char-property-change pos limit)))

(defun htmlize-overlay-faces-at (pos)
  (delq nil (mapcar (lambda (o) (overlay-get o 'face)) (overlays-at pos))))

(defun htmlize-next-face-change (pos &optional limit)
  ;; (htmlize-next-change pos 'face limit) would skip over entire
  ;; overlays that specify the `face' property, even when they
  ;; contain smaller text properties that also specify `face'.
  ;; Emacs display engine merges those faces, and so must we.
  (or limit
      (setq limit (point-max)))
  (let ((next-prop (next-single-property-change pos 'face nil limit))
        (overlay-faces (htmlize-overlay-faces-at pos)))
    (while (progn
             (setq pos (next-overlay-change pos))
             (and (< pos next-prop)
                  (equal overlay-faces (htmlize-overlay-faces-at pos)))))
    (setq pos (min pos next-prop))
    ;; Additionally, we include the entire region that specifies the
    ;; `display' property.
    (when (get-char-property pos 'display)
      (setq pos (next-single-char-property-change pos 'display nil limit)))
    pos))

(defmacro htmlize-lexlet (&rest letforms)
  (declare (indent 1) (debug let))
  (if (and (boundp 'lexical-binding)
           lexical-binding)
      `(let ,@letforms)
    ;; cl extensions have a macro implementing lexical let
    `(lexical-let ,@letforms)))


;;; Transformation of buffer text: HTML escapes, untabification, etc.

(defvar htmlize-basic-character-table
  ;; Map characters in the 0-127 range to either one-character strings
  ;; or to numeric entities.
  (let ((table (make-vector 128 ?\0)))
    ;; Map characters in the 32-126 range to themselves, others to
    ;; &#CODE entities;
    (dotimes (i 128)
      (setf (aref table i) (if (and (>= i 32) (<= i 126))
			       (char-to-string i)
			     (format "&#%d;" i))))
    ;; Set exceptions manually.
    (setf
     ;; Don't escape newline, carriage return, and TAB.
     (aref table ?\n) "\n"
     (aref table ?\r) "\r"
     (aref table ?\t) "\t"
     ;; Escape &, <, and >.
     (aref table ?&) "&amp;"
     (aref table ?<) "&lt;"
     (aref table ?>) "&gt;"
     ;; Not escaping '"' buys us a measurable speedup.  It's only
     ;; necessary to quote it for strings used in attribute values,
     ;; which htmlize doesn't typically do.
     ;(aref table ?\") "&quot;"
     )
    table))

;; A cache of HTML representation of non-ASCII characters.  Depending
;; on the setting of `htmlize-convert-nonascii-to-entities', this maps
;; non-ASCII characters to either "&#<code>;" or "<char>" (mapconcat's
;; mapper must always return strings).  It's only filled as characters
;; are encountered, so that in a buffer with e.g. French text, it will
;; only ever contain French accented characters as keys.  It's cleared
;; on each entry to htmlize-buffer-1 to allow modifications of
;; `htmlize-convert-nonascii-to-entities' to take effect.
(defvar htmlize-extended-character-cache (make-hash-table :test 'eq))

(defun htmlize-protect-string (string)
  "HTML-protect string, escaping HTML metacharacters and I18N chars."
  ;; Only protecting strings that actually contain unsafe or non-ASCII
  ;; chars removes a lot of unnecessary funcalls and consing.
  (if (not (string-match "[^\r\n\t -%'-;=?-~]" string))
      string
    (mapconcat (lambda (char)
		 (cond
		  ((< char 128)
		   ;; ASCII: use htmlize-basic-character-table.
		   (aref htmlize-basic-character-table char))
		  ((gethash char htmlize-extended-character-cache)
		   ;; We've already seen this char; return the cached
		   ;; string.
		   )
		  ((not htmlize-convert-nonascii-to-entities)
		   ;; If conversion to entities is not desired, always
		   ;; copy the char literally.
		   (setf (gethash char htmlize-extended-character-cache)
			 (char-to-string char)))
		  ((< char 256)
		   ;; Latin 1: no need to call encode-char.
		   (setf (gethash char htmlize-extended-character-cache)
			 (format "&#%d;" char)))
		  ((encode-char char 'ucs)
                   ;; Must check if encode-char works for CHAR;
                   ;; it fails for Arabic and possibly elsewhere.
		   (setf (gethash char htmlize-extended-character-cache)
			 (format "&#%d;" (encode-char char 'ucs))))
		  (t
		   ;; encode-char doesn't work for this char.  Copy it
		   ;; unchanged and hope for the best.
		   (setf (gethash char htmlize-extended-character-cache)
			 (char-to-string char)))))
	       string "")))

(defun htmlize-attr-escape (string)
  ;; Like htmlize-protect-string, but also escapes double-quoted
  ;; strings to make it usable in attribute values.
  (setq string (htmlize-protect-string string))
  (if (not (string-match "\"" string))
      string
    (mapconcat (lambda (char)
                 (if (eql char ?\")
                     "&quot;"
                   (char-to-string char)))
               string "")))

(defsubst htmlize-concat (list)
  (if (and (consp list) (null (cdr list)))
      ;; Don't create a new string in the common case where the list only
      ;; consists of one element.
      (car list)
    (apply #'concat list)))

(defun htmlize-format-link (linkprops text)
  (let ((uri (if (stringp linkprops)
                 linkprops
               (plist-get linkprops :uri)))
        (escaped-text (htmlize-protect-string text)))
    (if uri
        (format "<a href=\"%s\">%s</a>" (htmlize-attr-escape uri) escaped-text)
      escaped-text)))

(defun htmlize-escape-or-link (string)
  ;; Escape STRING and/or add hyperlinks.  STRING comes from a
  ;; `display' property.
  (let ((pos 0) (end (length string)) outlist)
    (while (< pos end)
      (let* ((link (get-char-property pos 'htmlize-link string))
             (next-link-change (next-single-property-change
                                pos 'htmlize-link string end))
             (chunk (substring string pos next-link-change)))
        (push
         (cond (link
                (htmlize-format-link link chunk))
               ((get-char-property 0 'htmlize-literal chunk)
                chunk)
               (t
                (htmlize-protect-string chunk)))
         outlist)
        (setq pos next-link-change)))
    (htmlize-concat (nreverse outlist))))

(defun htmlize-display-prop-to-html (display text)
  (let (desc)
    (cond ((stringp display)
           ;; Emacs ignores recursive display properties.
           (htmlize-escape-or-link display))
          ((not (eq (car-safe display) 'image))
           (htmlize-protect-string text))
          ((null (setq desc (funcall htmlize-transform-image
                                     (cdr display) text)))
           (htmlize-escape-or-link text))
          ((stringp desc)
           (htmlize-escape-or-link desc))
          (t
           (htmlize-generate-image desc text)))))

(defun htmlize-string-to-html (string)
  ;; Convert the string to HTML, including images attached as
  ;; `display' property and links as `htmlize-link' property.  In a
  ;; string without images or links, this is equivalent to
  ;; `htmlize-protect-string'.
  (let ((pos 0) (end (length string)) outlist)
    (while (< pos end)
      (let* ((display (get-char-property pos 'display string))
             (next-display-change (next-single-property-change
                                   pos 'display string end))
             (chunk (substring string pos next-display-change)))
        (push
         (if display
             (htmlize-display-prop-to-html display chunk)
           (htmlize-escape-or-link chunk))
         outlist)
        (setq pos next-display-change)))
    (htmlize-concat (nreverse outlist))))

(defun htmlize-default-transform-image (imgprops _text)
  "Default transformation of image descriptor to something usable in HTML.

If `htmlize-use-images' is nil, the function always returns nil, meaning
use original text.  Otherwise, it tries to find the image for images that
specify a file name.  If `htmlize-force-inline-images' is non-nil, it also
converts the :file attribute to :data and returns the modified property
list."
  (when htmlize-use-images
    (when (plist-get imgprops :file)
      (let ((location (plist-get (cdr (find-image (list imgprops))) :file)))
        (when location
          (setq imgprops (plist-put (cl-copy-list imgprops) :file location)))))
    (if htmlize-force-inline-images
        (let ((location (plist-get imgprops :file))
              data)
          (when location
            (with-temp-buffer
              (condition-case nil
                  (progn
                    (insert-file-contents-literally location)
                    (setq data (buffer-string)))
                (error nil))))
          ;; if successful, return the new plist, otherwise return
          ;; nil, which will use the original text
          (and data
               (plist-put (plist-put imgprops :file nil)
                          :data data)))
      imgprops)))

(defun htmlize-alt-text (_imgprops origtext)
  (and (/= (length origtext) 0)
       (<= (length origtext) htmlize-max-alt-text)
       (not (string-match "[\0-\x1f]" origtext))
       origtext))

(defun htmlize-generate-image (imgprops origtext)
  (let* ((alt-text (htmlize-alt-text imgprops origtext))
         (alt-attr (if alt-text
                       (format " alt=\"%s\"" (htmlize-attr-escape alt-text))
                     "")))
    (cond ((plist-get imgprops :file)
           ;; Try to find the image in image-load-path
           (let* ((found-props (cdr (find-image (list imgprops))))
                  (file (or (plist-get found-props :file)
                            (plist-get imgprops :file))))
             (format "<img src=\"%s\"%s />"
                     (htmlize-attr-escape (file-relative-name file))
                     alt-attr)))
          ((plist-get imgprops :data)
           (format "<img src=\"data:image/%s;base64,%s\"%s />"
                   (or (plist-get imgprops :type) "")
                   (base64-encode-string (plist-get imgprops :data))
                   alt-attr)))))

(defconst htmlize-ellipsis "...")
(put-text-property 0 (length htmlize-ellipsis) 'htmlize-ellipsis t htmlize-ellipsis)

(defun htmlize-match-inv-spec (inv)
  (cl-member inv buffer-invisibility-spec
             :key (lambda (i)
                    (if (symbolp i) i (car i)))))

(defun htmlize-decode-invisibility-spec (invisible)
  ;; Return t, nil, or `ellipsis', depending on how invisible text should be inserted.

  (if (not (listp buffer-invisibility-spec))
      ;; If buffer-invisibility-spec is not a list, then all
      ;; characters with non-nil `invisible' property are visible.
      (not invisible)

    ;; Otherwise, the value of a non-nil `invisible' property can be:
    ;; 1. a symbol -- make the text invisible if it matches
    ;;    buffer-invisibility-spec.
    ;; 2. a list of symbols -- make the text invisible if
    ;;    any symbol in the list matches
    ;;    buffer-invisibility-spec.
    ;; If the match of buffer-invisibility-spec has a non-nil
    ;; CDR, replace the invisible text with an ellipsis.
    (let ((match (if (symbolp invisible)
                     (htmlize-match-inv-spec invisible)
                   (cl-some #'htmlize-match-inv-spec invisible))))
      (cond ((null match) t)
            ((cdr-safe (car match)) 'ellipsis)
            (t nil)))))

(defun htmlize-add-before-after-strings (beg end text)
  ;; Find overlays specifying before-string and after-string in [beg,
  ;; pos).  If any are found, splice them into TEXT and return the new
  ;; text.
  (let (additions)
    (dolist (overlay (overlays-in beg end))
      (let ((before (overlay-get overlay 'before-string))
            (after (overlay-get overlay 'after-string)))
        (when after
          (push (cons (- (overlay-end overlay) beg)
                      after)
                additions))
        (when before
          (push (cons (- (overlay-start overlay) beg)
                      before)
                additions))))
    (if additions
        (let ((textlist nil)
              (strpos 0))
          (dolist (add (cl-stable-sort additions #'< :key #'car))
            (let ((addpos (car add))
                  (addtext (cdr add)))
              (push (substring text strpos addpos) textlist)
              (push addtext textlist)
              (setq strpos addpos)))
          (push (substring text strpos) textlist)
          (apply #'concat (nreverse textlist)))
      text)))

(defun htmlize-copy-prop (prop beg end string)
  ;; Copy the specified property from the specified region of the
  ;; buffer to the target string.  We cannot rely on Emacs to copy the
  ;; property because we want to handle properties coming from both
  ;; text properties and overlays.
  (let ((pos beg))
    (while (< pos end)
      (let ((value (get-char-property pos prop))
            (next-change (htmlize-next-change pos prop end)))
        (when value
          (put-text-property (- pos beg) (- next-change beg)
                             prop value string))
        (setq pos next-change)))))

(defun htmlize-get-text-with-display (beg end)
  ;; Like buffer-substring-no-properties, except it copies the
  ;; `display' property from the buffer, if found.
  (let ((text (buffer-substring-no-properties beg end)))
    (htmlize-copy-prop 'display beg end text)
    (htmlize-copy-prop 'htmlize-link beg end text)
    (setq text (htmlize-add-before-after-strings beg end text))
    text))

(defun htmlize-buffer-substring-no-invisible (beg end)
  ;; Like buffer-substring-no-properties, but don't copy invisible
  ;; parts of the region.  Where buffer-substring-no-properties
  ;; mandates an ellipsis to be shown, htmlize-ellipsis is inserted.
  (let ((pos beg)
	visible-list invisible show last-show next-change)
    ;; Iterate over the changes in the `invisible' property and filter
    ;; out the portions where it's non-nil, i.e. where the text is
    ;; invisible.
    (while (< pos end)
      (setq invisible (get-char-property pos 'invisible)
	    next-change (htmlize-next-change pos 'invisible end)
            show (htmlize-decode-invisibility-spec invisible))
      (cond ((eq show t)
	     (push (htmlize-get-text-with-display pos next-change)
                   visible-list))
            ((and (eq show 'ellipsis)
                  (not (eq last-show 'ellipsis))
                  ;; Conflate successive ellipses.
                  (push htmlize-ellipsis visible-list))))
      (setq pos next-change last-show show))
    (htmlize-concat (nreverse visible-list))))

(defun htmlize-trim-ellipsis (text)
  ;; Remove htmlize-ellipses ("...") from the beginning of TEXT if it
  ;; starts with it.  It checks for the special property of the
  ;; ellipsis so it doesn't work on ordinary text that begins with
  ;; "...".
  (if (get-text-property 0 'htmlize-ellipsis text)
      (substring text (length htmlize-ellipsis))
    text))

(defconst htmlize-tab-spaces
  ;; A table of strings with spaces.  (aref htmlize-tab-spaces 5) is
  ;; like (make-string 5 ?\ ), except it doesn't cons.
  (let ((v (make-vector 32 nil)))
    (dotimes (i (length v))
      (setf (aref v i) (make-string i ?\ )))
    v))

(defun htmlize-untabify-string (text start-column)
  "Untabify TEXT, assuming it starts at START-COLUMN."
  (let ((column start-column)
	(last-match 0)
	(chunk-start 0)
	chunks match-pos tab-size)
    (while (string-match "[\t\n]" text last-match)
      (setq match-pos (match-beginning 0))
      (cond ((eq (aref text match-pos) ?\t)
	     ;; Encountered a tab: create a chunk of text followed by
	     ;; the expanded tab.
	     (push (substring text chunk-start match-pos) chunks)
	     ;; Increase COLUMN by the length of the text we've
	     ;; skipped since last tab or newline.  (Encountering
	     ;; newline resets it.)
	     (cl-incf column (- match-pos last-match))
	     ;; Calculate tab size based on tab-width and COLUMN.
	     (setq tab-size (- tab-width (% column tab-width)))
	     ;; Expand the tab, carefully recreating the `display'
	     ;; property if one was on the TAB.
             (let ((display (get-text-property match-pos 'display text))
                   (expanded-tab (aref htmlize-tab-spaces tab-size)))
               (when display
                 (put-text-property 0 tab-size 'display display expanded-tab))
               (push expanded-tab chunks))
	     (cl-incf column tab-size)
	     (setq chunk-start (1+ match-pos)))
	    (t
	     ;; Reset COLUMN at beginning of line.
	     (setq column 0)))
      (setq last-match (1+ match-pos)))
    ;; If no chunks have been allocated, it means there have been no
    ;; tabs to expand.  Return TEXT unmodified.
    (if (null chunks)
	text
      (when (< chunk-start (length text))
	;; Push the remaining chunk.
	(push (substring text chunk-start) chunks))
      ;; Generate the output from the available chunks.
      (htmlize-concat (nreverse chunks)))))

(defun htmlize-extract-text (beg end trailing-ellipsis)
  ;; Extract buffer text, sans the invisible parts.  Then
  ;; untabify it and escape the HTML metacharacters.
  (let ((text (htmlize-buffer-substring-no-invisible beg end)))
    (when trailing-ellipsis
      (setq text (htmlize-trim-ellipsis text)))
    ;; If TEXT ends up empty, don't change trailing-ellipsis.
    (when (> (length text) 0)
      (setq trailing-ellipsis
            (get-text-property (1- (length text))
                               'htmlize-ellipsis text)))
    (when htmlize-untabify
      (setq text (htmlize-untabify-string text (current-column))))
    (setq text (htmlize-string-to-html text))
    (cl-values text trailing-ellipsis)))

(defun htmlize-despam-address (string)
  "Replace every occurrence of '@' in STRING with %40.
This is used to protect mailto links without modifying their meaning."
  ;; Suggested by Ville Skytta.
  (while (string-match "@" string)
    (setq string (replace-match "%40" nil t string)))
  string)

(defun htmlize-make-tmp-overlay (beg end props)
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'htmlize-tmp-overlay t)
    (while props
      (overlay-put overlay (pop props) (pop props)))
    overlay))

(defun htmlize-delete-tmp-overlays ()
  (dolist (overlay (overlays-in (point-min) (point-max)))
    (when (overlay-get overlay 'htmlize-tmp-overlay)
      (delete-overlay overlay))))

(defun htmlize-make-link-overlay (beg end uri)
  (htmlize-make-tmp-overlay beg end `(htmlize-link (:uri ,uri))))

(defun htmlize-create-auto-links ()
  "Add `htmlize-link' property to all mailto links in the buffer."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            "<\\(\\(mailto:\\)?\\([-=+_.a-zA-Z0-9]+@[-_.a-zA-Z0-9]+\\)\\)>"
            nil t)
      (let* ((address (match-string 3))
             (beg (match-beginning 0)) (end (match-end 0))
             (uri (concat "mailto:" (htmlize-despam-address address))))
        (htmlize-make-link-overlay beg end uri)))
    (goto-char (point-min))
    (while (re-search-forward "<\\(\\(URL:\\)?\\([a-zA-Z]+://[^;]+\\)\\)>"
                              nil t)
      (htmlize-make-link-overlay
       (match-beginning 0) (match-end 0) (match-string 3)))))

;; Tests for htmlize-create-auto-links:

;; <mailto:hniksic@xemacs.org>
;; <http://fly.srk.fer.hr>
;; <URL:http://www.xemacs.org>
;; <http://www.mail-archive.com/bbdb-info@xemacs.org/>
;; <hniksic@xemacs.org>
;; <xalan-dev-sc.10148567319.hacuhiucknfgmpfnjcpg-john=doe.com@xml.apache.org>

(defun htmlize-shadow-form-feeds ()
  (let ((s "\n<hr />"))
    (put-text-property 0 (length s) 'htmlize-literal t s)
    (let ((disp `(display ,s)))
      (while (re-search-forward "\n\^L" nil t)
        (let* ((beg (match-beginning 0))
               (end (match-end 0))
               (form-feed-pos (1+ beg))
               ;; don't process ^L if invisible or covered by `display'
               (show (and (htmlize-decode-invisibility-spec
                           (get-char-property form-feed-pos 'invisible))
                          (not (get-char-property form-feed-pos 'display)))))
          (when show
            (htmlize-make-tmp-overlay beg end disp)))))))

(defun htmlize-defang-local-variables ()
  ;; Juri Linkov reports that an HTML-ized "Local variables" can lead
  ;; visiting the HTML to fail with "Local variables list is not
  ;; properly terminated".  He suggested changing the phrase to
  ;; syntactically equivalent HTML that Emacs doesn't recognize.
  (goto-char (point-min))
  (while (search-forward "Local Variables:" nil t)
    (replace-match "Local Variables&#58;" nil t)))
  

;;; Color handling.

(defvar htmlize-x-library-search-path
  `(,data-directory
    "/etc/X11/rgb.txt"
    "/usr/share/X11/rgb.txt"
    ;; the remainder of this list really belongs in a museum
    "/usr/X11R6/lib/X11/"
    "/usr/X11R5/lib/X11/"
    "/usr/lib/X11R6/X11/"
    "/usr/lib/X11R5/X11/"
    "/usr/local/X11R6/lib/X11/"
    "/usr/local/X11R5/lib/X11/"
    "/usr/local/lib/X11R6/X11/"
    "/usr/local/lib/X11R5/X11/"
    "/usr/X11/lib/X11/"
    "/usr/lib/X11/"
    "/usr/local/lib/X11/"
    "/usr/X386/lib/X11/"
    "/usr/x386/lib/X11/"
    "/usr/XFree86/lib/X11/"
    "/usr/unsupported/lib/X11/"
    "/usr/athena/lib/X11/"
    "/usr/local/x11r5/lib/X11/"
    "/usr/lpp/Xamples/lib/X11/"
    "/usr/openwin/lib/X11/"
    "/usr/openwin/share/lib/X11/"))

(defun htmlize-get-color-rgb-hash (&optional rgb-file)
  "Return a hash table mapping X color names to RGB values.
The keys in the hash table are X11 color names, and the values are the
#rrggbb RGB specifications, extracted from `rgb.txt'.

If RGB-FILE is nil, the function will try hard to find a suitable file
in the system directories.

If no rgb.txt file is found, return nil."
  (let ((rgb-file (or rgb-file (locate-file
				"rgb.txt"
				htmlize-x-library-search-path)))
	(hash nil))
    (when rgb-file
      (with-temp-buffer
	(insert-file-contents rgb-file)
	(setq hash (make-hash-table :test 'equal))
	(while (not (eobp))
	  (cond ((looking-at "^\\s-*\\([!#]\\|$\\)")
		 ;; Skip comments and empty lines.
		 )
		((looking-at
		  "[ \t]*\\([0-9]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\([0-9]+\\)[ \t]+\\(.*\\)")
		 (setf (gethash (downcase (match-string 4)) hash)
		       (format "#%02x%02x%02x"
			       (string-to-number (match-string 1))
			       (string-to-number (match-string 2))
			       (string-to-number (match-string 3)))))
		(t
		 (error
		  "Unrecognized line in %s: %s"
		  rgb-file
		  (buffer-substring (point) (progn (end-of-line) (point))))))
	  (forward-line 1))))
    hash))

;; Compile the RGB map when loaded.  On systems where rgb.txt is
;; missing, the value of the variable will be nil, and rgb.txt will
;; not be used.
(defvar htmlize-color-rgb-hash (htmlize-get-color-rgb-hash))

;;; Face handling.

(defun htmlize-face-color-internal (face fg)
  ;; Used only under GNU Emacs.  Return the color of FACE, but don't
  ;; return "unspecified-fg" or "unspecified-bg".  If the face is
  ;; `default' and the color is unspecified, look up the color in
  ;; frame parameters.
  (let* ((function (if fg #'face-foreground #'face-background))
	 (color (funcall function face nil t)))
    (when (and (eq face 'default) (null color))
      (setq color (cdr (assq (if fg 'foreground-color 'background-color)
			     (frame-parameters)))))
    (when (or (eq color 'unspecified)
	      (equal color "unspecified-fg")
	      (equal color "unspecified-bg"))
      (setq color nil))
    (when (and (eq face 'default)
	       (null color))
      ;; Assuming black on white doesn't seem right, but I can't think
      ;; of anything better to do.
      (setq color (if fg "black" "white")))
    color))

(defun htmlize-face-foreground (face)
  ;; Return the name of the foreground color of FACE.  If FACE does
  ;; not specify a foreground color, return nil.
  (htmlize-face-color-internal face t))

(defun htmlize-face-background (face)
  ;; Return the name of the background color of FACE.  If FACE does
  ;; not specify a background color, return nil.
  ;; GNU Emacs.
  (htmlize-face-color-internal face nil))

;; Convert COLOR to the #RRGGBB string.  If COLOR is already in that
;; format, it's left unchanged.

(defun htmlize-color-to-rgb (color)
  (let ((rgb-string nil))
    (cond ((null color)
	   ;; Ignore nil COLOR because it means that the face is not
	   ;; specifying any color.  Hence (htmlize-color-to-rgb nil)
	   ;; returns nil.
	   )
	  ((string-match "\\`#" color)
	   ;; The color is already in #rrggbb format.
	   (setq rgb-string color))
	  ((and htmlize-use-rgb-txt
		htmlize-color-rgb-hash)
	   ;; Use of rgb.txt is requested, and it's available on the
	   ;; system.  Use it.
	   (setq rgb-string (gethash (downcase color) htmlize-color-rgb-hash)))
	  (t
	   ;; We're getting the RGB components from Emacs.
	   (let ((rgb (mapcar (lambda (arg)
                                (/ arg 256))
                              (color-values color))))
	     (when rgb
	       (setq rgb-string (apply #'format "#%02x%02x%02x" rgb))))))
    ;; If RGB-STRING is still nil, it means the color cannot be found,
    ;; for whatever reason.  In that case just punt and return COLOR.
    ;; Most browsers support a decent set of color names anyway.
    (or rgb-string color)))

;; We store the face properties we care about into an
;; `htmlize-fstruct' type.  That way we only have to analyze face
;; properties, which can be time consuming, once per each face.  The
;; mapping between Emacs faces and htmlize-fstructs is established by
;; htmlize-make-face-map.  The name "fstruct" refers to variables of
;; type `htmlize-fstruct', while the term "face" is reserved for Emacs
;; faces.

(cl-defstruct htmlize-fstruct
  foreground				; foreground color, #rrggbb
  background				; background color, #rrggbb
  size					; size
  boldp					; whether face is bold
  italicp				; whether face is italic
  underlinep				; whether face is underlined
  overlinep				; whether face is overlined
  strikep				; whether face is struck through
  css-name				; CSS name of face
  )

(defun htmlize-face-set-from-keyword-attr (fstruct attr value)
  ;; For ATTR and VALUE, set the equivalent value in FSTRUCT.
  (cl-case attr
    (:foreground
     (setf (htmlize-fstruct-foreground fstruct) (htmlize-color-to-rgb value)))
    (:background
     (setf (htmlize-fstruct-background fstruct) (htmlize-color-to-rgb value)))
    (:height
     (setf (htmlize-fstruct-size fstruct) value))
    (:weight
     (when (string-match (symbol-name value) "bold")
       (setf (htmlize-fstruct-boldp fstruct) t)))
    (:slant
     (setf (htmlize-fstruct-italicp fstruct) (or (eq value 'italic)
						 (eq value 'oblique))))
    (:bold
     (setf (htmlize-fstruct-boldp fstruct) value))
    (:italic
     (setf (htmlize-fstruct-italicp fstruct) value))
    (:underline
     (setf (htmlize-fstruct-underlinep fstruct) value))
    (:overline
     (setf (htmlize-fstruct-overlinep fstruct) value))
    (:strike-through
     (setf (htmlize-fstruct-strikep fstruct) value))))

(defun htmlize-face-size (face)
  ;; The size (height) of FACE, taking inheritance into account.
  ;; Only works in Emacs 21 and later.
  (let* ((face-list (list face))
         (head face-list)
         (tail face-list))
    (while head
      (let ((inherit (face-attribute (car head) :inherit)))
        (cond ((listp inherit)
               (setcdr tail (cl-copy-list inherit))
               (setq tail (last tail)))
              ((eq inherit 'unspecified))
              (t
               (setcdr tail (list inherit))
               (setq tail (cdr tail)))))
      (pop head))
    (let ((size-list
           (cl-loop
            for f in face-list
            for h = (and (facep f) (face-attribute f :height))
            collect (if (eq h 'unspecified) nil h))))
      (cl-reduce 'htmlize-merge-size (cons nil size-list)))))

(defun htmlize-face-css-name (face)
  ;; Generate the css-name property for the given face.  Emacs places
  ;; no restrictions on the names of symbols that represent faces --
  ;; any characters may be in the name, even control chars.  We try
  ;; hard to beat the face name into shape, both esthetically and
  ;; according to CSS1 specs.
  (let ((name (downcase (symbol-name face))))
    (when (string-match "\\`font-lock-" name)
      ;; font-lock-FOO-face -> FOO.
      (setq name (replace-match "" t t name)))
    (when (string-match "-face\\'" name)
      ;; Drop the redundant "-face" suffix.
      (setq name (replace-match "" t t name)))
    (while (string-match "[^-a-zA-Z0-9]" name)
      ;; Drop the non-alphanumerics.
      (setq name (replace-match "X" t t name)))
    (when (string-match "\\`[-0-9]" name)
      ;; CSS identifiers may not start with a digit.
      (setq name (concat "X" name)))
    ;; After these transformations, the face could come out empty.
    (when (equal name "")
      (setq name "face"))
    ;; Apply the prefix.
    (concat htmlize-css-name-prefix name)))

(defun htmlize-face-to-fstruct-1 (face)
  "Convert Emacs face FACE to fstruct, internal."
  (let ((fstruct (make-htmlize-fstruct
		  :foreground (htmlize-color-to-rgb
			       (htmlize-face-foreground face))
		  :background (htmlize-color-to-rgb
			       (htmlize-face-background face)))))
    ;; GNU Emacs
    (dolist (attr '(:weight :slant :underline :overline :strike-through))
      (let ((value (face-attribute face attr nil t)))
        (when (and value (not (eq value 'unspecified)))
          (htmlize-face-set-from-keyword-attr fstruct attr value))))
    (let ((size (htmlize-face-size face)))
      (unless (eql size 1.0)            ; ignore non-spec
        (setf (htmlize-fstruct-size fstruct) size)))
    (setf (htmlize-fstruct-css-name fstruct) (htmlize-face-css-name face))
    fstruct))

(defun htmlize-face-to-fstruct (face)
  (let* ((face-list (or (and (symbolp face)
                             (cdr (assq face face-remapping-alist)))
                        (list face)))
         (fstruct (htmlize-merge-faces
                   (mapcar (lambda (face)
                             (if (symbolp face)
                                 (or (htmlize-get-override-fstruct face)
                                     (htmlize-face-to-fstruct-1 face))
                               (htmlize-attrlist-to-fstruct face)))
                           (nreverse face-list)))))
    (when (symbolp face)
      (setf (htmlize-fstruct-css-name fstruct) (htmlize-face-css-name face)))
    fstruct))

(defmacro htmlize-copy-attr-if-set (attr-list dest source)
  ;; Generate code with the following pattern:
  ;; (progn
  ;;   (when (htmlize-fstruct-ATTR source)
  ;;     (setf (htmlize-fstruct-ATTR dest) (htmlize-fstruct-ATTR source)))
  ;;   ...)
  ;; for the given list of boolean attributes.
  (cons 'progn
	(cl-loop for attr in attr-list
	         for attr-sym = (intern (format "htmlize-fstruct-%s" attr))
	         collect `(when (,attr-sym ,source)
                            (setf (,attr-sym ,dest) (,attr-sym ,source))))))

(defun htmlize-merge-size (merged next)
  ;; Calculate the size of the merge of MERGED and NEXT.
  (cond ((null merged)     next)
	((integerp next)   next)
	((null next)       merged)
	((floatp merged)   (* merged next))
	((integerp merged) (round (* merged next)))))

(defun htmlize-merge-two-faces (merged next)
  (htmlize-copy-attr-if-set
   (foreground background boldp italicp underlinep overlinep strikep)
   merged next)
  (setf (htmlize-fstruct-size merged)
	(htmlize-merge-size (htmlize-fstruct-size merged)
			    (htmlize-fstruct-size next)))
  merged)

(defun htmlize-merge-faces (fstruct-list)
  (cond ((null fstruct-list)
	 ;; Nothing to do, return a dummy face.
	 (make-htmlize-fstruct))
	((null (cdr fstruct-list))
	 ;; Optimize for the common case of a single face, simply
	 ;; return it.
	 (car fstruct-list))
	(t
	 (cl-reduce #'htmlize-merge-two-faces
		    (cons (make-htmlize-fstruct) fstruct-list)))))

;; GNU Emacs 20+ supports attribute lists in `face' properties.  For
;; example, you can use `(:foreground "red" :weight bold)' as an
;; overlay's "face", or you can even use a list of such lists, etc.
;; We call those "attrlists".
;;
;; htmlize supports attrlist by converting them to fstructs, the same
;; as with regular faces.

(defun htmlize-attrlist-to-fstruct (attrlist &optional name)
  ;; Like htmlize-face-to-fstruct, but accepts an ATTRLIST as input.
  (let ((fstruct (make-htmlize-fstruct)))
    (cond ((eq (car attrlist) 'foreground-color)
	   ;; ATTRLIST is (foreground-color . COLOR)
	   (setf (htmlize-fstruct-foreground fstruct)
		 (htmlize-color-to-rgb (cdr attrlist))))
	  ((eq (car attrlist) 'background-color)
	   ;; ATTRLIST is (background-color . COLOR)
	   (setf (htmlize-fstruct-background fstruct)
		 (htmlize-color-to-rgb (cdr attrlist))))
	  (t
	   ;; ATTRLIST is a plist.
	   (while attrlist
	     (let ((attr (pop attrlist))
		   (value (pop attrlist)))
	       (when (and value (not (eq value 'unspecified)))
		 (htmlize-face-set-from-keyword-attr fstruct attr value))))))
    (setf (htmlize-fstruct-css-name fstruct) (or name "custom"))
    fstruct))

(defun htmlize-decode-face-prop (prop)
  "Turn face property PROP into a list of face-like objects."
  ;; PROP can be a symbol naming a face, a string naming such a
  ;; symbol, a cons (foreground-color . COLOR) or (background-color
  ;; COLOR), a property list (:attr1 val1 :attr2 val2 ...), or a list
  ;; of any of those.
  ;;
  ;; (htmlize-decode-face-prop 'face) -> (face)
  ;; (htmlize-decode-face-prop '(face1 face2)) -> (face1 face2)
  ;; (htmlize-decode-face-prop '(:attr "val")) -> ((:attr "val"))
  ;; (htmlize-decode-face-prop '((:attr "val") face (foreground-color "red")))
  ;;   -> ((:attr "val") face (foreground-color "red"))
  ;;
  ;; Unrecognized atoms or non-face symbols/strings are silently
  ;; stripped away.
  (cond ((null prop)
         nil)
        ((symbolp prop)
         (and (facep prop)
              (list prop)))
        ((stringp prop)
         (and (facep (intern-soft prop))
              (list prop)))
        ((atom prop)
         nil)
        ((and (symbolp (car prop))
              (eq ?: (aref (symbol-name (car prop)) 0)))
         (list prop))
        ((or (eq (car prop) 'foreground-color)
             (eq (car prop) 'background-color))
         (list prop))
        (t
         (apply #'nconc (mapcar #'htmlize-decode-face-prop prop)))))

(defun htmlize-get-override-fstruct (face)
  (let* ((raw-def (plist-get htmlize-face-overrides face))
         (def (cond ((stringp raw-def) (list :foreground raw-def))
                    ((listp raw-def) raw-def)
                    (t
                     (error (format (concat "face override must be an "
                                            "attribute list or string, got %s")
                                    raw-def))))))
    (and def
         (htmlize-attrlist-to-fstruct def (symbol-name face)))))

(defun htmlize-make-face-map (faces)
  ;; Return a hash table mapping Emacs faces to htmlize's fstructs.
  ;; The keys are either face symbols or attrlists, so the test
  ;; function must be `equal'.
  (let ((face-map (make-hash-table :test 'equal))
	css-names)
    (dolist (face faces)
      (unless (gethash face face-map)
	;; Haven't seen FACE yet; convert it to an fstruct and cache
	;; it.
	(let ((fstruct (htmlize-face-to-fstruct face)))
	  (setf (gethash face face-map) fstruct)
	  (let* ((css-name (htmlize-fstruct-css-name fstruct))
		 (new-name css-name)
		 (i 0))
	    ;; Uniquify the face's css-name by using NAME-1, NAME-2,
	    ;; etc.
	    (while (member new-name css-names)
	      (setq new-name (format "%s-%s" css-name (cl-incf i))))
	    (unless (equal new-name css-name)
	      (setf (htmlize-fstruct-css-name fstruct) new-name))
	    (push new-name css-names)))))
    face-map))

(defun htmlize-unstringify-face (face)
  "If FACE is a string, return it interned, otherwise return it unchanged."
  (if (stringp face)
      (intern face)
    face))

(defun htmlize-faces-in-buffer ()
  "Return a list of faces used in the current buffer.
This is the set of faces specified by the `face' text property and by buffer
overlays that specify `face'."
  (let (faces)
    ;; Faces used by text properties.
    (let ((pos (point-min)) face-prop next)
      (while (< pos (point-max))
        (setq face-prop (get-text-property pos 'face)
              next (or (next-single-property-change pos 'face) (point-max)))
        (setq faces (cl-nunion (htmlize-decode-face-prop face-prop)
                               faces :test 'equal))
        (setq pos next)))
    ;; Faces used by overlays.
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (let ((face-prop (overlay-get overlay 'face)))
        (setq faces (cl-nunion (htmlize-decode-face-prop face-prop)
                               faces :test 'equal))))
    faces))

;; htmlize-faces-at-point returns the faces in use at point.  The
;; faces are sorted by increasing priority, i.e. the last face takes
;; precedence.
;;
;; This returns all the faces in the `face' property and all the faces
;; in the overlays at point.

(defun htmlize-faces-at-point ()
  (let (all-faces)
    ;; Faces from text properties.
    (let ((face-prop (get-text-property (point) 'face)))
      ;; we need to reverse the `face' prop because we want
      ;; more specific faces to come later
      (setq all-faces (nreverse (htmlize-decode-face-prop face-prop))))
    ;; Faces from overlays.
    (let ((overlays
           ;; Collect overlays at point that specify `face'.
           (cl-delete-if-not (lambda (o)
                               (overlay-get o 'face))
                             (nreverse (overlays-at (point) t))))
          list face-prop)
      (dolist (overlay overlays)
        (setq face-prop (overlay-get overlay 'face)
              list (nconc (htmlize-decode-face-prop face-prop) list)))
      ;; Under "Merging Faces" the manual explicitly states
      ;; that faces specified by overlays take precedence over
      ;; faces specified by text properties.
      (setq all-faces (nconc all-faces list)))
    all-faces))

;; htmlize supports generating HTML in several flavors, some of which
;; use CSS, and others the <font> element.  We take an OO approach and
;; define "methods" that indirect to the functions that depend on
;; `htmlize-output-type'.  The currently used methods are `doctype',
;; `insert-head', `body-tag', `pre-tag', and `text-markup'.  Not all
;; output types define all methods.
;;
;; Methods are called either with (htmlize-method METHOD ARGS...) 
;; special form, or by accessing the function with
;; (htmlize-method-function 'METHOD) and calling (funcall FUNCTION).
;; The latter form is useful in tight loops because `htmlize-method'
;; conses.

(defmacro htmlize-method (method &rest args)
  ;; Expand to (htmlize-TYPE-METHOD ...ARGS...).  TYPE is the value of
  ;; `htmlize-output-type' at run time.
  `(funcall (htmlize-method-function ',method) ,@args))

(defun htmlize-method-function (method)
  ;; Return METHOD's function definition for the current output type.
  ;; The returned object can be safely funcalled.
  (let ((sym (intern (format "htmlize-%s-%s" htmlize-output-type method))))
    (indirect-function (if (fboundp sym)
			   sym
			 (let ((default (intern (concat "htmlize-default-"
							(symbol-name method)))))
			   (if (fboundp default)
			       default
			     'ignore))))))

(defvar htmlize-memoization-table (make-hash-table :test 'equal))

(defmacro htmlize-memoize (key generator)
  "Return the value of GENERATOR, memoized as KEY.
That means that GENERATOR will be evaluated and returned the first time
it's called with the same value of KEY.  All other times, the cached
\(memoized) value will be returned."
  (let ((value (cl-gensym)))
    `(let ((,value (gethash ,key htmlize-memoization-table)))
       (unless ,value
	 (setq ,value ,generator)
	 (setf (gethash ,key htmlize-memoization-table) ,value))
       ,value)))

;;; Default methods.

(defun htmlize-default-doctype ()
  nil					; no doc-string
  ;; Note that the `font' output is technically invalid under this DTD
  ;; because the DTD doesn't allow embedding <font> in <pre>.
  "<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01//EN\">"
  )

(defun htmlize-default-body-tag (face-map)
  nil					; no doc-string
  face-map ; shut up the byte-compiler
  "<body>")

(defun htmlize-default-pre-tag (face-map)
  nil					; no doc-string
  face-map ; shut up the byte-compiler
  "<pre>")


;;; CSS based output support.

;; Internal function; not a method.
(defun htmlize-css-specs (fstruct)
  (let (result)
    (when (htmlize-fstruct-foreground fstruct)
      (push (format "color: %s;" (htmlize-fstruct-foreground fstruct))
	    result))
    (when (htmlize-fstruct-background fstruct)
      (push (format "background-color: %s;"
		    (htmlize-fstruct-background fstruct))
	    result))
    (let ((size (htmlize-fstruct-size fstruct)))
      (when (and size (not (eq htmlize-ignore-face-size t)))
	(cond ((floatp size)
	       (push (format "font-size: %d%%;" (* 100 size)) result))
	      ((not (eq htmlize-ignore-face-size 'absolute))
	       (push (format "font-size: %spt;" (/ size 10.0)) result)))))
    (when (htmlize-fstruct-boldp fstruct)
      (push "font-weight: bold;" result))
    (when (htmlize-fstruct-italicp fstruct)
      (push "font-style: italic;" result))
    (when (htmlize-fstruct-underlinep fstruct)
      (push "text-decoration: underline;" result))
    (when (htmlize-fstruct-overlinep fstruct)
      (push "text-decoration: overline;" result))
    (when (htmlize-fstruct-strikep fstruct)
      (push "text-decoration: line-through;" result))
    (nreverse result)))

(defun htmlize-css-insert-head (buffer-faces face-map)
  (insert "    <style type=\"text/css\">\n    <!--\n")
  (insert "      body {\n        "
	  (mapconcat #'identity
		     (htmlize-css-specs (gethash 'default face-map))
		     "\n        ")
	  "\n      }\n")
  (dolist (face (cl-sort (cl-copy-list buffer-faces) #'string-lessp
		         :key (lambda (f)
			        (htmlize-fstruct-css-name (gethash f face-map)))))
    (let* ((fstruct (gethash face face-map))
	   (cleaned-up-face-name
	    (let ((s
		   ;; Use `prin1-to-string' rather than `symbol-name'
		   ;; to get the face name because the "face" can also
		   ;; be an attrlist, which is not a symbol.
		   (prin1-to-string face)))
	      ;; If the name contains `--' or `*/', remove them.
	      (while (string-match "--" s)
		(setq s (replace-match "-" t t s)))
	      (while (string-match "\\*/" s)
		(setq s (replace-match "XX" t t s)))
	      s))
	   (specs (htmlize-css-specs fstruct)))
      (insert "      ." (htmlize-fstruct-css-name fstruct))
      (if (null specs)
	  (insert " {")
	(insert " {\n        /* " cleaned-up-face-name " */\n        "
		(mapconcat #'identity specs "\n        ")))
      (insert "\n      }\n")))
  (insert htmlize-hyperlink-style
	  "    -->\n    </style>\n"))

(defun htmlize-css-text-markup (fstruct-list buffer)
  ;; Open the markup needed to insert text colored with FACES into
  ;; BUFFER.  Return the function that closes the markup.

  ;; In CSS mode, this is easy: just nest the text in one <span
  ;; class=...> tag for each face in FSTRUCT-LIST.
  (dolist (fstruct fstruct-list)
    (princ "<span class=\"" buffer)
    (princ (htmlize-fstruct-css-name fstruct) buffer)
    (princ "\">" buffer))
  (htmlize-lexlet ((fstruct-list fstruct-list) (buffer buffer))
    (lambda ()
      (dolist (fstruct fstruct-list)
        (ignore fstruct)                ; shut up the byte-compiler
        (princ "</span>" buffer)))))

;; `inline-css' output support.

(defun htmlize-inline-css-body-tag (face-map)
  (format "<body style=\"%s\">"
	  (mapconcat #'identity (htmlize-css-specs (gethash 'default face-map))
		     " ")))

(defun htmlize-inline-css-pre-tag (face-map)
  (if htmlize-pre-style
      (format "<pre style=\"%s\">"
              (mapconcat #'identity (htmlize-css-specs (gethash 'default face-map))
                         " "))
    (format "<pre>")))

(defun htmlize-inline-css-text-markup (fstruct-list buffer)
  (let* ((merged (htmlize-merge-faces fstruct-list))
	 (style (htmlize-memoize
		 merged
		 (let ((specs (htmlize-css-specs merged)))
		   (and specs
			(mapconcat #'identity (htmlize-css-specs merged) " "))))))
    (when style
      (princ "<span style=\"" buffer)
      (princ style buffer)
      (princ "\">" buffer))
    (htmlize-lexlet ((style style) (buffer buffer))
      (lambda ()
        (when style
          (princ "</span>" buffer))))))

;;; `font' tag based output support.

(defun htmlize-font-body-tag (face-map)
  (let ((fstruct (gethash 'default face-map)))
    (format "<body text=\"%s\" bgcolor=\"%s\">"
	    (htmlize-fstruct-foreground fstruct)
	    (htmlize-fstruct-background fstruct))))

(defun htmlize-font-pre-tag (face-map)
  (if htmlize-pre-style
      (let ((fstruct (gethash 'default face-map)))
        (format "<pre text=\"%s\" bgcolor=\"%s\">"
                (htmlize-fstruct-foreground fstruct)
                (htmlize-fstruct-background fstruct)))
    (format "<pre>")))
       
(defun htmlize-font-text-markup (fstruct-list buffer)
  ;; In `font' mode, we use the traditional HTML means of altering
  ;; presentation: <font> tag for colors, <b> for bold, <u> for
  ;; underline, and <strike> for strike-through.
  (let* ((merged (htmlize-merge-faces fstruct-list))
	 (markup (htmlize-memoize
		  merged
		  (cons (concat
			 (and (htmlize-fstruct-foreground merged)
			      (format "<font color=\"%s\">" (htmlize-fstruct-foreground merged)))
			 (and (htmlize-fstruct-boldp merged)      "<b>")
			 (and (htmlize-fstruct-italicp merged)    "<i>")
			 (and (htmlize-fstruct-underlinep merged) "<u>")
			 (and (htmlize-fstruct-strikep merged)    "<strike>"))
			(concat
			 (and (htmlize-fstruct-strikep merged)    "</strike>")
			 (and (htmlize-fstruct-underlinep merged) "</u>")
			 (and (htmlize-fstruct-italicp merged)    "</i>")
			 (and (htmlize-fstruct-boldp merged)      "</b>")
			 (and (htmlize-fstruct-foreground merged) "</font>"))))))
    (princ (car markup) buffer)
    (htmlize-lexlet ((markup markup) (buffer buffer))
      (lambda ()
        (princ (cdr markup) buffer)))))

(defun htmlize-buffer-1 ()
  ;; Internal function; don't call it from outside this file.  Htmlize
  ;; current buffer, writing the resulting HTML to a new buffer, and
  ;; return it.  Unlike htmlize-buffer, this doesn't change current
  ;; buffer or use switch-to-buffer.
  (save-excursion
    ;; Protect against the hook changing the current buffer.
    (save-excursion
      (run-hooks 'htmlize-before-hook))
    ;; Convince font-lock support modes to fontify the entire buffer
    ;; in advance.
    (htmlize-ensure-fontified)
    (clrhash htmlize-extended-character-cache)
    (clrhash htmlize-memoization-table)
    ;; It's important that the new buffer inherits default-directory
    ;; from the current buffer.
    (let ((htmlbuf (generate-new-buffer (if (buffer-file-name)
                                            (htmlize-make-file-name
                                             (file-name-nondirectory
                                              (buffer-file-name)))
                                          "*html*")))
          (completed nil))
      (unwind-protect
          (let* ((buffer-faces (htmlize-faces-in-buffer))
                 (face-map (htmlize-make-face-map (cl-adjoin 'default buffer-faces)))
                 (places (cl-gensym))
                 (title (if (buffer-file-name)
                            (file-name-nondirectory (buffer-file-name))
                          (buffer-name))))
            (when htmlize-generate-hyperlinks
              (htmlize-create-auto-links))
            (when htmlize-replace-form-feeds
              (htmlize-shadow-form-feeds))

            ;; Initialize HTMLBUF and insert the HTML prolog.
            (with-current-buffer htmlbuf
              (buffer-disable-undo)
              (insert (htmlize-method doctype) ?\n
                      (format "<!-- Created by htmlize-%s in %s mode. -->\n"
                              htmlize-version htmlize-output-type)
                      "<html>\n  ")
              (put places 'head-start (point-marker))
              (insert "<head>\n"
                      "    <title>" (htmlize-protect-string title) "</title>\n"
                      (if htmlize-html-charset
                          (format (concat "    <meta http-equiv=\"Content-Type\" "
                                          "content=\"text/html; charset=%s\">\n")
                                  htmlize-html-charset)
                        "")
                      htmlize-head-tags)
              (htmlize-method insert-head buffer-faces face-map)
              (insert "  </head>")
              (put places 'head-end (point-marker))
              (insert "\n  ")
              (put places 'body-start (point-marker))
              (insert (htmlize-method body-tag face-map)
                      "\n    ")
              (put places 'content-start (point-marker))
              (insert (htmlize-method pre-tag face-map) "\n"))
            (let ((text-markup
                   ;; Get the inserter method, so we can funcall it inside
                   ;; the loop.  Not calling `htmlize-method' in the loop
                   ;; body yields a measurable speed increase.
                   (htmlize-method-function 'text-markup))
                  ;; Declare variables used in loop body outside the loop
                  ;; because it's faster to establish `let' bindings only
                  ;; once.
                  next-change text face-list trailing-ellipsis
                  fstruct-list last-fstruct-list
                  (close-markup (lambda ())))
              ;; This loop traverses and reads the source buffer, appending
              ;; the resulting HTML to HTMLBUF.  This method is fast
              ;; because: 1) it doesn't require examining the text
              ;; properties char by char (htmlize-next-face-change is used
              ;; to move between runs with the same face), and 2) it doesn't
              ;; require frequent buffer switches, which are slow because
              ;; they rebind all buffer-local vars.
              (goto-char (point-min))
              (while (not (eobp))
                (setq next-change (htmlize-next-face-change (point)))
                ;; Get faces in use between (point) and NEXT-CHANGE, and
                ;; convert them to fstructs.
                (setq face-list (htmlize-faces-at-point)
                      fstruct-list (delq nil (mapcar (lambda (f)
                                                       (gethash f face-map))
                                                     face-list)))
                (cl-multiple-value-setq (text trailing-ellipsis)
                  (htmlize-extract-text (point) next-change trailing-ellipsis))
                ;; Don't bother writing anything if there's no text (this
                ;; happens in invisible regions).
                (when (> (length text) 0)
                  ;; Open the new markup if necessary and insert the text.
                  (when (not (cl-equalp fstruct-list last-fstruct-list))
                    (funcall close-markup)
                    (setq last-fstruct-list fstruct-list
                          close-markup (funcall text-markup fstruct-list htmlbuf)))
                  (princ text htmlbuf))
                (goto-char next-change))

              ;; We've gone through the buffer; close the markup from
              ;; the last run, if any.
              (funcall close-markup))

            ;; Insert the epilog and post-process the buffer.
            (with-current-buffer htmlbuf
              (insert "</pre>")
              (put places 'content-end (point-marker))
              (insert "\n  </body>")
              (put places 'body-end (point-marker))
              (insert "\n</html>\n")
              (htmlize-defang-local-variables)
              (goto-char (point-min))
              (when htmlize-html-major-mode
                ;; What sucks about this is that the minor modes, most notably
                ;; font-lock-mode, won't be initialized.  Oh well.
                (funcall htmlize-html-major-mode))
              (set (make-local-variable 'htmlize-buffer-places)
                   (symbol-plist places))
              (run-hooks 'htmlize-after-hook)
              (buffer-enable-undo))
            (setq completed t)
            htmlbuf)

        (when (not completed)
          (kill-buffer htmlbuf))
        (htmlize-delete-tmp-overlays)))))

;; Utility functions.

(defmacro htmlize-with-fontify-message (&rest body)
  ;; When forcing fontification of large buffers in
  ;; htmlize-ensure-fontified, inform the user that he is waiting for
  ;; font-lock, not for htmlize to finish.
  `(progn
     (if (> (buffer-size) 65536)
	 (message "Forcing fontification of %s..."
		  (buffer-name (current-buffer))))
     ,@body
     (if (> (buffer-size) 65536)
	 (message "Forcing fontification of %s...done"
		  (buffer-name (current-buffer))))))

(defun htmlize-ensure-fontified ()
  ;; If font-lock is being used, ensure that the "support" modes
  ;; actually fontify the buffer.  If font-lock is not in use, we
  ;; don't care because, except in htmlize-file, we don't force
  ;; font-lock on the user.
  (when font-lock-mode
    ;; In part taken from ps-print-ensure-fontified in GNU Emacs 21.
    (when (and (boundp 'jit-lock-mode)
               (symbol-value 'jit-lock-mode))
      (htmlize-with-fontify-message
       (jit-lock-fontify-now (point-min) (point-max))))

    (if (fboundp 'font-lock-ensure)
        (font-lock-ensure)
      ;; Emacs prior to 25.1
      (with-no-warnings
        (font-lock-mode 1)
        (font-lock-fontify-buffer)))))


;;;###autoload
(defun htmlize-buffer (&optional buffer)
  "Convert BUFFER to HTML, preserving colors and decorations.

The generated HTML is available in a new buffer, which is returned.
When invoked interactively, the new buffer is selected in the current
window.  The title of the generated document will be set to the buffer's
file name or, if that's not available, to the buffer's name.

Note that htmlize doesn't fontify your buffers, it only uses the
decorations that are already present.  If you don't set up font-lock or
something else to fontify your buffers, the resulting HTML will be
plain.  Likewise, if you don't like the choice of colors, fix the mode
that created them, or simply alter the faces it uses."
  (interactive)
  (let ((htmlbuf (with-current-buffer (or buffer (current-buffer))
		   (htmlize-buffer-1))))
    (when (interactive-p)
      (switch-to-buffer htmlbuf))
    htmlbuf))

;;;###autoload
(defun htmlize-region (beg end)
  "Convert the region to HTML, preserving colors and decorations.
See `htmlize-buffer' for details."
  (interactive "r")
  ;; Don't let zmacs region highlighting end up in HTML.
  (when (fboundp 'zmacs-deactivate-region)
    (zmacs-deactivate-region))
  (let ((htmlbuf (save-restriction
		   (narrow-to-region beg end)
		   (htmlize-buffer-1))))
    (when (interactive-p)
      (switch-to-buffer htmlbuf))
    htmlbuf))

(defun htmlize-region-for-paste (beg end)
  "Htmlize the region and return just the HTML as a string.
This forces the `inline-css' style and only returns the HTML body,
but without the BODY tag.  This should make it useful for inserting
the text to another HTML buffer."
  (let* ((htmlize-output-type 'inline-css)
	 (htmlbuf (htmlize-region beg end)))
    (unwind-protect
	(with-current-buffer htmlbuf
	  (buffer-substring (plist-get htmlize-buffer-places 'content-start)
			    (plist-get htmlize-buffer-places 'content-end)))
      (kill-buffer htmlbuf))))

(defun htmlize-region-save-screenshot (beg end)
  "Save the htmlized (see `htmlize-region-for-paste') region in
the kill ring. Uses `inline-css', with style information in
`<pre>' tags, so that the rendering of the marked up text
approximates the buffer as closely as possible."
  (interactive "r")
  (let ((htmlize-pre-style t))
    (kill-new (htmlize-region-for-paste beg end)))
  (deactivate-mark))

(defun htmlize-make-file-name (file)
  "Make an HTML file name from FILE.

In its default implementation, this simply appends `.html' to FILE.
This function is called by htmlize to create the buffer file name, and
by `htmlize-file' to create the target file name.

More elaborate transformations are conceivable, such as changing FILE's
extension to `.html' (\"file.c\" -> \"file.html\").  If you want them,
overload this function to do it and htmlize will comply."
  (concat file ".html"))

;; Older implementation of htmlize-make-file-name that changes FILE's
;; extension to ".html".
;(defun htmlize-make-file-name (file)
;  (let ((extension (file-name-extension file))
;	(sans-extension (file-name-sans-extension file)))
;    (if (or (equal extension "html")
;	    (equal extension "htm")
;	    (equal sans-extension ""))
;	(concat file ".html")
;      (concat sans-extension ".html"))))

;;;###autoload
(defun htmlize-file (file &optional target)
  "Load FILE, fontify it, convert it to HTML, and save the result.

Contents of FILE are inserted into a temporary buffer, whose major mode
is set with `normal-mode' as appropriate for the file type.  The buffer
is subsequently fontified with `font-lock' and converted to HTML.  Note
that, unlike `htmlize-buffer', this function explicitly turns on
font-lock.  If a form of highlighting other than font-lock is desired,
please use `htmlize-buffer' directly on buffers so highlighted.

Buffers currently visiting FILE are unaffected by this function.  The
function does not change current buffer or move the point.

If TARGET is specified and names a directory, the resulting file will be
saved there instead of to FILE's directory.  If TARGET is specified and
does not name a directory, it will be used as output file name."
  (interactive (list (read-file-name
		      "HTML-ize file: "
		      nil nil nil (and (buffer-file-name)
				       (file-name-nondirectory
					(buffer-file-name))))))
  (let ((output-file (if (and target (not (file-directory-p target)))
			 target
		       (expand-file-name
			(htmlize-make-file-name (file-name-nondirectory file))
			(or target (file-name-directory file)))))
	;; Try to prevent `find-file-noselect' from triggering
	;; font-lock because we'll fontify explicitly below.
	(font-lock-mode nil)
	(font-lock-auto-fontify nil)
	(global-font-lock-mode nil)
	;; Ignore the size limit for the purposes of htmlization.
	(font-lock-maximum-size nil))
    (with-temp-buffer
      ;; Insert FILE into the temporary buffer.
      (insert-file-contents file)
      ;; Set the file name so normal-mode and htmlize-buffer-1 pick it
      ;; up.  Restore it afterwards so with-temp-buffer's kill-buffer
      ;; doesn't complain about killing a modified buffer.
      (let ((buffer-file-name file))
	;; Set the major mode for the sake of font-lock.
	(normal-mode)
	;; htmlize the buffer and save the HTML.
	(with-current-buffer (htmlize-buffer-1)
	  (unwind-protect
	      (progn
		(run-hooks 'htmlize-file-hook)
		(write-region (point-min) (point-max) output-file))
	    (kill-buffer (current-buffer)))))))
  ;; I haven't decided on a useful return value yet, so just return
  ;; nil.
  nil)

;;;###autoload
(defun htmlize-many-files (files &optional target-directory)
  "Convert FILES to HTML and save the corresponding HTML versions.

FILES should be a list of file names to convert.  This function calls
`htmlize-file' on each file; see that function for details.  When
invoked interactively, you are prompted for a list of files to convert,
terminated with RET.

If TARGET-DIRECTORY is specified, the HTML files will be saved to that
directory.  Normally, each HTML file is saved to the directory of the
corresponding source file."
  (interactive
   (list
    (let (list file)
      ;; Use empty string as DEFAULT because setting DEFAULT to nil
      ;; defaults to the directory name, which is not what we want.
      (while (not (equal (setq file (read-file-name
				     "HTML-ize file (RET to finish): "
				     (and list (file-name-directory
						(car list)))
				     "" t))
			 ""))
	(push file list))
      (nreverse list))))
  ;; Verify that TARGET-DIRECTORY is indeed a directory.  If it's a
  ;; file, htmlize-file will use it as target, and that doesn't make
  ;; sense.
  (and target-directory
       (not (file-directory-p target-directory))
       (error "target-directory must name a directory: %s" target-directory))
  (dolist (file files)
    (htmlize-file file target-directory)))

;;;###autoload
(defun htmlize-many-files-dired (arg &optional target-directory)
  "HTMLize dired-marked files."
  (interactive "P")
  (htmlize-many-files (dired-get-marked-files nil arg) target-directory))

(provide 'htmlize)

;; Local Variables:
;; byte-compile-warnings: (not unresolved obsolete)
;; End:

;;; htmlize.el ends here
