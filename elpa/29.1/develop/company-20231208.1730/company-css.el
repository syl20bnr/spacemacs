;;; company-css.el --- company-mode completion backend for css-mode  -*- lexical-binding: t -*-

;; Copyright (C) 2009-2011, 2013-2015, 2018  Free Software Foundation, Inc.

;; Author: Nikolaj Schumacher

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; In Emacs >= 26, company-capf is used instead.

;;; Code:

(require 'company)
(require 'cl-lib)

(declare-function web-mode-language-at-pos "web-mode" (&optional pos))

(defconst company-css-property-alist
  ;; see http://www.w3.org/TR/CSS21/propidx.html
  '(("azimuth" angle "left-side" "far-left" "left" "center-left" "center"
     "center-right" "right" "far-right" "right-side" "behind" "leftwards"
     "rightwards")
    ("background" background-color background-image background-repeat
     background-attachment background-position
     background-clip background-origin background-size)
    ("background-attachment" "scroll" "fixed")
    ("background-color" color "transparent")
    ("background-image" uri "none")
    ("background-position" percentage length "left" "center" "right" percentage
     length "top" "center" "bottom" "left" "center" "right" "top" "center"
     "bottom")
    ("background-repeat" "repeat" "repeat-x" "repeat-y" "no-repeat")
    ("border" border-width border-style border-color)
    ("border-bottom" border)
    ("border-bottom-color" border-color)
    ("border-bottom-style" border-style)
    ("border-bottom-width" border-width)
    ("border-collapse" "collapse" "separate")
    ("border-color" color "transparent")
    ("border-left" border)
    ("border-left-color" border-color)
    ("border-left-style" border-style)
    ("border-left-width" border-width)
    ("border-right" border)
    ("border-right-color" border-color)
    ("border-right-style" border-style)
    ("border-right-width" border-width)
    ("border-spacing" length length)
    ("border-style" border-style)
    ("border-top" border)
    ("border-top-color" border-color)
    ("border-top-style" border-style)
    ("border-top-width" border-width)
    ("border-width" border-width)
    ("bottom" length percentage "auto")
    ("caption-side" "top" "bottom")
    ("clear" "none" "left" "right" "both")
    ("clip" shape "auto")
    ("color" color)
    ("content" "normal" "none" string uri counter "attr()" "open-quote"
     "close-quote" "no-open-quote" "no-close-quote")
    ("counter-increment" identifier integer "none")
    ("counter-reset" identifier integer "none")
    ("cue" cue-before cue-after)
    ("cue-after" uri "none")
    ("cue-before" uri "none")
    ("cursor" uri "*" "auto" "crosshair" "default" "pointer" "move" "e-resize"
     "ne-resize" "nw-resize" "n-resize" "se-resize" "sw-resize" "s-resize"
     "w-resize" "text" "wait" "help" "progress")
    ("direction" "ltr" "rtl")
    ("display" "inline" "block" "list-item" "run-in" "inline-block" "table"
     "inline-table" "table-row-group" "table-header-group" "table-footer-group"
     "table-row" "table-column-group" "table-column" "table-cell"
     "table-caption" "none")
    ("elevation" angle "below" "level" "above" "higher" "lower")
    ("empty-cells" "show" "hide")
    ("float" "left" "right" "none")
    ("font" font-style font-weight font-size "/" line-height
     font-family "caption" "icon" "menu" "message-box" "small-caption"
     "status-bar" "normal" "small-caps"
     ;; CSS3
     font-stretch)
    ("font-family" family-name generic-family)
    ("font-size" absolute-size relative-size length percentage)
    ("font-style" "normal" "italic" "oblique")
    ("font-weight" "normal" "bold" "bolder" "lighter" "100" "200" "300" "400"
     "500" "600" "700" "800" "900")
    ("height" length percentage "auto")
    ("left" length percentage "auto")
    ("letter-spacing" "normal" length)
    ("line-height" "normal" number length percentage)
    ("list-style" list-style-type list-style-position list-style-image)
    ("list-style-image" uri "none")
    ("list-style-position" "inside" "outside")
    ("list-style-type" "disc" "circle" "square" "decimal" "decimal-leading-zero"
     "lower-roman" "upper-roman" "lower-greek" "lower-latin" "upper-latin"
     "armenian" "georgian" "lower-alpha" "upper-alpha" "none")
    ("margin" margin-width)
    ("margin-bottom" margin-width)
    ("margin-left" margin-width)
    ("margin-right" margin-width)
    ("margin-top" margin-width)
    ("max-height" length percentage "none")
    ("max-width" length percentage "none")
    ("min-height" length percentage)
    ("min-width" length percentage)
    ("orphans" integer)
    ("outline" outline-color outline-style outline-width)
    ("outline-color" color "invert")
    ("outline-style" border-style)
    ("outline-width" border-width)
    ("overflow" "visible" "hidden" "scroll" "auto"
     ;; CSS3:
     "no-display" "no-content")
    ("padding" padding-width)
    ("padding-bottom" padding-width)
    ("padding-left" padding-width)
    ("padding-right" padding-width)
    ("padding-top" padding-width)
    ("page-break-after" "auto" "always" "avoid" "left" "right")
    ("page-break-before" "auto" "always" "avoid" "left" "right")
    ("page-break-inside" "avoid" "auto")
    ("pause" time percentage)
    ("pause-after" time percentage)
    ("pause-before" time percentage)
    ("pitch" frequency "x-low" "low" "medium" "high" "x-high")
    ("pitch-range" number)
    ("play-during" uri "mix" "repeat" "auto" "none")
    ("position" "static" "relative" "absolute" "fixed")
    ("quotes" string string "none")
    ("richness" number)
    ("right" length percentage "auto")
    ("speak" "normal" "none" "spell-out")
    ("speak-header" "once" "always")
    ("speak-numeral" "digits" "continuous")
    ("speak-punctuation" "code" "none")
    ("speech-rate" number "x-slow" "slow" "medium" "fast" "x-fast" "faster"
     "slower")
    ("stress" number)
    ("table-layout" "auto" "fixed")
    ("text-align" "left" "right" "center" "justify")
    ("text-indent" length percentage)
    ("text-transform" "capitalize" "uppercase" "lowercase" "none")
    ("top" length percentage "auto")
    ("unicode-bidi" "normal" "embed" "bidi-override")
    ("vertical-align" "baseline" "sub" "super" "top" "text-top" "middle"
     "bottom" "text-bottom" percentage length)
    ("visibility" "visible" "hidden" "collapse")
    ("voice-family" specific-voice generic-voice "*" specific-voice
     generic-voice)
    ("volume" number percentage "silent" "x-soft" "soft" "medium" "loud"
     "x-loud")
    ("white-space" "normal" "pre" "nowrap" "pre-wrap" "pre-line")
    ("widows" integer)
    ("width" length percentage "auto")
    ("word-spacing" "normal" length)
    ("z-index" "auto" integer)
    ;; CSS3
    ("align-content" align-stretch "space-between" "space-around")
    ("align-items" align-stretch "baseline")
    ("align-self" align-items "auto")
    ("animation" animation-name animation-duration animation-timing-function
     animation-delay animation-iteration-count animation-direction
     animation-fill-mode)
    ("animation-delay" time)
    ("animation-direction" "normal" "reverse" "alternate" "alternate-reverse")
    ("animation-duration" time)
    ("animation-fill-mode" "none" "forwards" "backwards" "both")
    ("animation-iteration-count" integer "infinite")
    ("animation-name" "none")
    ("animation-play-state" "paused" "running")
    ("animation-timing-function" transition-timing-function
     "step-start" "step-end" "steps(,)")
    ("backface-visibility" "visible" "hidden")
    ("background-clip" background-origin)
    ("background-origin" "border-box" "padding-box" "content-box")
    ("background-size" length percentage "auto" "cover" "contain")
    ("border-image" border-image-outset border-image-repeat border-image-source
     border-image-slice border-image-width)
    ("border-image-outset" length)
    ("border-image-repeat" "stretch" "repeat" "round" "space")
    ("border-image-source" uri "none")
    ("border-image-slice" length)
    ("border-image-width" length percentage)
    ("border-radius" length)
    ("border-top-left-radius" length)
    ("border-top-right-radius" length)
    ("border-bottom-left-radius" length)
    ("border-bottom-right-radius" length)
    ("box-decoration-break" "slice" "clone")
    ("box-shadow" length color)
    ("box-sizing" "content-box" "border-box")
    ("break-after" "auto" "always" "avoid" "left" "right" "page" "column"
     "avoid-page" "avoid-column")
    ("break-before" break-after)
    ("break-inside" "avoid" "auto")
    ("columns" column-width column-count)
    ("column-count" integer)
    ("column-fill" "auto" "balance")
    ("column-gap" length "normal")
    ("column-rule" column-rule-width column-rule-style column-rule-color)
    ("column-rule-color" color)
    ("column-rule-style" border-style)
    ("column-rule-width" border-width)
    ("column-span" "all" "none")
    ("column-width" length "auto")
    ("filter" url "blur()" "brightness()" "contrast()" "drop-shadow()"
     "grayscale()" "hue-rotate()" "invert()" "opacity()" "saturate()" "sepia()")
    ("flex" flex-grow flex-shrink flex-basis)
    ("flex-basis" percentage length "auto")
    ("flex-direction" "row" "row-reverse" "column" "column-reverse")
    ("flex-flow" flex-direction flex-wrap)
    ("flex-grow" number)
    ("flex-shrink" number)
    ("flex-wrap" "nowrap" "wrap" "wrap-reverse")
    ("font-feature-setting" normal string number)
    ("font-kerning" "auto" "normal" "none")
    ("font-language-override" "normal" string)
    ("font-size-adjust" "none" number)
    ("font-stretch" "normal" "ultra-condensed" "extra-condensed" "condensed"
     "semi-condensed" "semi-expanded" "expanded" "extra-expanded" "ultra-expanded")
    ("font-synthesis" "none" "weight" "style")
    ("font-variant" font-variant-alternates font-variant-caps
     font-variant-east-asian font-variant-ligatures font-variant-numeric
     font-variant-position)
    ("font-variant-alternates" "normal" "historical-forms" "stylistic()"
     "styleset()" "character-variant()" "swash()" "ornaments()" "annotation()")
    ("font-variant-caps" "normal" "small-caps" "all-small-caps" "petite-caps"
     "all-petite-caps" "unicase" "titling-caps")
    ("font-variant-east-asian" "jis78" "jis83" "jis90" "jis04" "simplified"
     "traditional" "full-width" "proportional-width" "ruby")
    ("font-variant-ligatures" "normal" "none" "common-ligatures"
     "no-common-ligatures" "discretionary-ligatures" "no-discretionary-ligatures"
     "historical-ligatures" "no-historical-ligatures" "contextual" "no-contextual")
    ("font-variant-numeric" "normal" "ordinal" "slashed-zero"
     "lining-nums" "oldstyle-nums" "proportional-nums" "tabular-nums"
     "diagonal-fractions" "stacked-fractions")
    ("font-variant-position" "normal" "sub" "super")
    ("hyphens" "none" "manual" "auto")
    ("justify-content" align-common "space-between" "space-around")
    ("line-break" "auto" "loose" "normal" "strict")
    ("marquee-direction" "forward" "reverse")
    ("marquee-play-count" integer "infinite")
    ("marquee-speed" "slow" "normal" "fast")
    ("marquee-style" "scroll" "slide" "alternate")
    ("opacity" number)
    ("order" number)
    ("outline-offset" length)
    ("overflow-x" overflow)
    ("overflow-y" overflow)
    ("overflow-style" "auto" "marquee-line" "marquee-block")
    ("overflow-wrap" "normal" "break-word")
    ("perspective" "none" length)
    ("perspective-origin" percentage length "left" "center" "right" "top" "bottom")
    ("resize" "none" "both" "horizontal" "vertical")
    ("tab-size" integer length)
    ("text-align-last" "auto" "start" "end" "left" "right" "center" "justify")
    ("text-decoration" text-decoration-color text-decoration-line text-decoration-style)
    ("text-decoration-color" color)
    ("text-decoration-line" "none" "underline" "overline" "line-through" "blink")
    ("text-decoration-style" "solid" "double" "dotted" "dashed" "wavy")
    ("text-overflow" "clip" "ellipsis")
    ("text-shadow" color length)
    ("text-underline-position" "auto" "under" "left" "right")
    ("transform" "matrix(,,,,,)" "translate(,)" "translateX()" "translateY()"
     "scale()" "scaleX()" "scaleY()" "rotate()" "skewX()" "skewY()" "none")
    ("transform-origin" perspective-origin)
    ("transform-style" "flat" "preserve-3d")
    ("transition" transition-property transition-duration
     transition-timing-function transition-delay)
    ("transition-delay" time)
    ("transition-duration" time)
    ("transition-timing-function"
     "ease" "linear" "ease-in" "ease-out" "ease-in-out" "cubic-bezier(,,,)")
    ("transition-property" "none" "all" identifier)
    ("word-wrap" overflow-wrap)
    ("word-break" "normal" "break-all" "keep-all"))
  "A list of CSS properties and their possible values.")

(defconst company-css-value-classes
  '((absolute-size "xx-small" "x-small" "small" "medium" "large" "x-large"
                   "xx-large")
    (align-common "flex-start" "flex-end" "center")
    (align-stretch align-common "stretch")
    (border-style "none" "hidden" "dotted" "dashed" "solid" "double" "groove"
                  "ridge" "inset" "outset")
    (border-width "thick" "medium" "thin")
    (color "aqua" "black" "blue" "fuchsia" "gray" "green" "lime" "maroon" "navy"
           "olive" "orange" "purple" "red" "silver" "teal" "white" "yellow")
    (counter "counter(,)")
    (family-name "Courier" "Helvetica" "Times")
    (generic-family "serif" "sans-serif" "cursive" "fantasy" "monospace")
    (generic-voice "male" "female" "child")
    (margin-width "auto") ;; length percentage
    (relative-size "larger" "smaller")
    (shape "rect(,,,)")
    (uri "url()"))
  "A list of CSS property value classes and their contents.")
;; missing, because not completable
;; <angle><frequency><identifier><integer><length><number><padding-width>
;; <percentage><specific-voice><string><time><uri>

(defconst company-css-html-tags
  '("a" "abbr" "acronym" "address" "applet" "area" "b" "base" "basefont" "bdo"
    "big" "blockquote" "body" "br" "button" "caption" "center" "cite" "code"
    "col" "colgroup" "dd" "del" "dfn" "dir" "div" "dl" "dt" "em" "fieldset"
    "font" "form" "frame" "frameset" "h1" "h2" "h3" "h4" "h5" "h6" "head" "hr"
    "html" "i" "iframe" "img" "input" "ins" "isindex" "kbd" "label" "legend"
    "li" "link" "map" "menu" "meta" "noframes" "noscript" "object" "ol"
    "optgroup" "option" "p" "param" "pre" "q" "s" "samp" "script" "select"
    "small" "span" "strike" "strong" "style" "sub" "sup" "table" "tbody" "td"
    "textarea" "tfoot" "th" "thead" "title" "tr" "tt" "u" "ul" "var"
    ;; HTML5
    "section" "article" "aside" "header" "footer" "nav" "figure" "figcaption"
    "time" "mark" "main")
  "A list of HTML tags for use in CSS completion.")

(defconst company-css-pseudo-classes
  '("active" "after" "before" "first" "first-child" "first-letter" "first-line"
    "focus" "hover" "lang" "left" "link" "right" "visited")
  "Identifiers for CSS pseudo-elements and pseudo-classes.")

(defconst company-css-property-cache (make-hash-table :size 115 :test 'equal))

(defun company-css-property-values (attribute)
  "Access the `company-css-property-alist' cached and flattened."
  (or (gethash attribute company-css-property-cache)
      (let (results)
        (dolist (value (cdr (assoc attribute company-css-property-alist)))
          (if (symbolp value)
              (dolist (child (or (cdr (assoc value company-css-value-classes))
                                 (company-css-property-values
                                  (symbol-name value))))
                (push child results))
            (push value results)))
        (setq results (sort results 'string<))
        (puthash attribute
                 (if (fboundp 'delete-consecutive-dups)
                     (delete-consecutive-dups results)
                   (delete-dups results))
                 company-css-property-cache)
        results)))

;;; bracket detection

(defconst company-css-braces-syntax-table
  (let ((table (make-syntax-table)))
    (setf (aref table ?{) '(4 . 125))
    (setf (aref table ?}) '(5 . 123))
    table)
  "A syntax table giving { and } paren syntax.")

(defun company-css-inside-braces-p ()
  "Return non-nil, if point is within matched { and }."
  (ignore-errors
    (with-syntax-table company-css-braces-syntax-table
      (let ((parse-sexp-ignore-comments t))
        (scan-lists (point) -1 1)))))

;;; tags
(defconst company-css-tag-regexp
  (concat "\\(?:\\`\\|}\\)[[:space:]]*"
          ;; multiple
          "\\(?:"
          ;; previous tags:
          "\\(?:#\\|\\_<[[:alpha:]]\\)[[:alnum:]-#]*\\(?:\\[[^]]*\\]\\)?"
          ;; space or selectors
          "\\(?:[[:space:]]+\\|[[:space:]]*[+,>][[:space:]]*\\)"
          "\\)*"
          "\\(\\(?:#\\|\\_<[[:alpha:]]\\)\\(?:[[:alnum:]-#]*\\_>\\)?\\_>\\|\\)"
          "\\=")
  "A regular expression matching CSS tags.")

;;; pseudo id
(defconst company-css-pseudo-regexp
  (concat "\\(?:\\`\\|}\\)[[:space:]]*"
          ;; multiple
          "\\(?:"
          ;; previous tags:
          "\\(?:#\\|\\_<[[:alpha:]]\\)[[:alnum:]-#]*\\(?:\\[[^]]*\\]\\)?"
          ;; space or delimiters
          "\\(?:[[:space:]]+\\|[[:space:]]*[+,>][[:space:]]*\\)"
          "\\)*"
          "\\(?:\\(?:\\#\\|\\_<[[:alpha:]]\\)[[:alnum:]-#]*\\):"
          "\\([[:alpha:]-]+\\_>\\|\\)\\_>\\=")
  "A regular expression matching CSS pseudo classes.")

;;; properties

(defun company-css-grab-property ()
  "Return the CSS property before point, if any.
Returns \"\" if no property found, but feasible at this position."
  (when (company-css-inside-braces-p)
    (company-grab-symbol)))

;;; values
(defconst company-css-property-value-regexp
  "\\_<\\([[:alpha:]-]+\\):\\(?:[^{};]*[[:space:]]+\\)?\\([^{};]*\\_>\\|\\)\\="
  "A regular expression matching CSS tags.")

;;;###autoload
(defun company-css (command &optional arg &rest _ignored)
  "`company-mode' completion backend for `css-mode'."
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-css))
    (prefix (and (or (derived-mode-p 'css-mode)
                     (and (derived-mode-p 'web-mode)
                          (string= (web-mode-language-at-pos) "css")))
                 (or (company-grab company-css-tag-regexp 1)
                     (company-grab company-css-pseudo-regexp 1)
                     (company-grab company-css-property-value-regexp 2
                                   (line-beginning-position))
                     (company-css-grab-property))))
    (candidates
     (cond
      ((company-grab company-css-tag-regexp 1)
       (all-completions arg company-css-html-tags))
      ((company-grab company-css-pseudo-regexp 1)
       (all-completions arg company-css-pseudo-classes))
      ((company-grab company-css-property-value-regexp 2
                     (line-beginning-position))
       (all-completions arg
                        (company-css-property-values
                         (company-grab company-css-property-value-regexp 1))))
      ((company-css-grab-property)
       (all-completions arg company-css-property-alist))))
    (sorted t)))

(provide 'company-css)
;;; company-css.el ends here
