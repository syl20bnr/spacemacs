;;; org-re-reveal.el --- Org export to reveal.js presentations  -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Copyright (C) 2013-2018 Yujie Wen and contributors to org-reveal, see:
;;                         https://github.com/yjwen/org-reveal/commits/master
;; Copyright (C) 2019      Naoya Yamashita <conao3@gmail.com>
;; Copyright (C) 2019      Ayush Goyal <perfectayush@gmail.com>
;; SPDX-FileCopyrightText: 2017-2024 Jens Lechtenbörger

;; URL: https://gitlab.com/oer/org-re-reveal
;; Version: 3.25.1
;; Package-Requires: ((emacs "24.4") (org "8.3") (htmlize "1.34"))
;; Keywords: tools, outlines, hypermedia, slideshow, presentation, OER

;; This file is not part of GNU Emacs.

;;; License:
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.
;; If not, see http://www.gnu.org/licenses/ or write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;; This package provides Org export functionality to generate HTML
;; presentations with the presentation framework reveal.js.
;; In conjunction with other packages (see comments on emacs-reveal
;; below), this is an excellent approach to generate Open Educational
;; Resources (OER).
;;
;; Quickstart:
;; 0. Install reveal.js: https://revealjs.com/
;; 1. Activate org-re-reveal.
;;    (a) Place this directory into your load path or install it from MELPA
;;        (https://melpa.org/#/getting-started).
;;    (b) Load package manually ("M-x load-library" followed by
;;        "org-re-reveal") or place "(require 'org-re-reveal)" into your
;;        ~/.emacs and restart or customize org-export-backends by adding
;;        the symbol re-reveal.
;; 2. Load an Org file and export it to HTML.
;;    (a) Make sure that reveal.js is available in your current directory
;;        (e.g., as sub-directory or symbolic link).
;;    (b) Load "Readme.org" (coming with org-re-reveal).
;;    (c) Export to HTML: Press "C-c C-e v v" (write HTML file) or
;;        "C-c C-e v b" (write HTML file and open in browser)
;; See "Readme.org" for introduction, details, and features added to
;; org-reveal:
;; https://gitlab.com/oer/org-re-reveal/-/blob/main/Readme.org
;; The Readme is also available as reveal.js presentation that is
;; generated with org-re-reveal in a CI/CD infrastructure on GitLab:
;; https://oer.gitlab.io/org-re-reveal/Readme.html
;;
;; Note that emacs-reveal offers a project that embeds org-re-reveal,
;; reveal.js, and various reveal.js plugins:
;; https://gitlab.com/oer/emacs-reveal
;; Its howto, generated from Org source file with emacs-reveal:
;; https://oer.gitlab.io/emacs-reveal-howto/howto.html
;; As a real-life example, maybe check out the OER presentations
;; (HTML with audio, different PDF variants, references into
;; bibliography, index terms, management of metadata including
;; license information and attribution, Docker image for publication
;; as GitLab Pages with CI/CD) for a course on Operating Systems:
;; https://oer.gitlab.io/OS/
;;
;; The package org-re-reveal grew out of a forked version of org-reveal
;; when upstream development stopped:
;; https://github.com/yjwen/org-reveal/issues/349
;; https://github.com/yjwen/org-reveal/issues/342

;;; Code:

(require 'ox-ascii)
(require 'ox-html)
(require 'cl-lib)   ; cl-mapcar and autoloads for:
                    ; cl-loop, cl-letf, cl-assert, cl-case, cl-every,
                    ; cl-delete-duplicates, cl-remove-if
(require 'subr-x)   ; string-trim
(require 'iso-cvt)
(require 'url-parse)
(require 'url-util)

(defvar org-re-reveal-keys) ; Silence byte compiler

(defun org-re-reveal-define-backend ()
  "Define the back-end for export as reveal.js presentation."
  (org-export-define-derived-backend 're-reveal 'html

    :menu-entry
    `(,(nth 0 org-re-reveal-keys) "Export to reveal.js HTML Presentation"
      ((,(nth 1 org-re-reveal-keys)
        "To file" org-re-reveal-export-to-html)
       (,(nth 2 org-re-reveal-keys)
        "To file and browse" org-re-reveal-export-to-html-and-browse)
       (,(nth 3 org-re-reveal-keys)
        "Current subtree to file" org-re-reveal-export-current-subtree)))

    :options-alist ; See org-export-options-alist for meaning of parts.
    ;; First options, then keywords; in both cases with variables for
    ;; default values.  Final values indicate how multiple occurrences
    ;; of keywords are treated, including nil, t, newline:
    ;; nil: Keep old value, discard new one
    ;; t: Use new value, discard old one
    ;; space: Concatenate with space character
    ;; newline: Concatenate with newlines
    '((:reveal-center nil "reveal_center" org-re-reveal-center t)
      (:reveal-control nil "reveal_control" org-re-reveal-control t)
      (:reveal-defaulttiming nil "reveal_defaulttiming" org-re-reveal-defaulttiming t)
      (:reveal-embed-local-resources nil "reveal_embed_local_resources" org-re-reveal-embed-local-resources t)
      (:reveal-fragmentinurl nil "reveal_fragmentinurl" org-re-reveal-fragmentinurl t)
      (:reveal-generate-ids nil "reveal_generate_ids" org-re-reveal-generate-custom-ids t)
      (:reveal-hashonebasedindex nil "reveal_hashonebasedindex" org-re-reveal-hashonebasedindex t)
      (:reveal-height nil "reveal_height" org-re-reveal-height t)
      (:reveal-history nil  "reveal_history" org-re-reveal-history t)
      (:reveal-inter-presentation-links nil "reveal_inter_presentation_links" org-re-reveal-inter-presentation-links t)
      (:reveal-keyboard nil "reveal_keyboard" org-re-reveal-keyboard t)
      (:reveal-klipsify-src nil "reveal_klipsify_src" org-re-reveal-klipsify-src t)
      (:reveal-mobile-app nil "reveal_mobile_app" org-re-reveal-mobile-app t)
      (:reveal-mousewheel nil "reveal_mousewheel" org-re-reveal-mousewheel t)
      (:reveal-overview nil "reveal_overview" org-re-reveal-overview t)
      (:reveal-pdfseparatefragments nil "reveal_pdfseparatefragments" org-re-reveal-pdfseparatefragments t)
      (:reveal-progress nil "reveal_progress" org-re-reveal-progress t)
      (:reveal-rolling-links nil "reveal_rolling_links" org-re-reveal-rolling-links t)
      (:reveal-show-notes nil "reveal_show_notes" org-re-reveal-show-notes t)
      (:reveal-single-file nil "reveal_single_file" org-re-reveal-single-file t)
      (:reveal-slide-global-footer nil "reveal_global_footer" org-re-reveal-global-footer t)
      (:reveal-slide-global-header nil "reveal_global_header" org-re-reveal-global-header t)
      (:reveal-slide-number nil "reveal_slide_number" org-re-reveal-slide-number t)
      (:reveal-slide-toc-footer nil "reveal_toc_footer" org-re-reveal-toc-footer t)
      (:reveal-subtree-with-title-slide nil "reveal_subtree_with_title_slide" org-re-reveal-subtree-with-title-slide t)
      (:reveal-totaltime nil "reveal_totaltime" org-re-reveal-totaltime t)
      (:reveal-width nil "reveal_width" org-re-reveal-width t)
      (:reveal-with-tts nil "reveal_with_tts" org-re-reveal-with-tts t)
      (:reveal-academic-title "REVEAL_ACADEMIC_TITLE" nil nil t)
      (:reveal-add-plugin "REVEAL_ADD_PLUGIN" nil nil newline)
      (:reveal-codemirror-config "REVEAL_CODEMIRROR_CONFIG" nil org-re-reveal-klipse-codemirror newline)
      (:reveal-default-frag-style "REVEAL_DEFAULT_FRAG_STYLE" nil org-re-reveal-default-frag-style t)
      (:reveal-default-slide-background "REVEAL_DEFAULT_SLIDE_BACKGROUND" nil nil t)
      (:reveal-default-slide-background-opacity "REVEAL_DEFAULT_SLIDE_BACKGROUND_OPACITY" nil nil t)
      (:reveal-default-slide-background-position "REVEAL_DEFAULT_SLIDE_BACKGROUND_POSITION" nil nil t)
      (:reveal-default-slide-background-repeat "REVEAL_DEFAULT_SLIDE_BACKGROUND_REPEAT" nil nil t)
      (:reveal-default-slide-background-size "REVEAL_DEFAULT_SLIDE_BACKGROUND_SIZE" nil nil t)
      (:reveal-default-slide-background-transition "REVEAL_DEFAULT_SLIDE_BACKGROUND_TRANSITION" nil nil t)
      (:reveal-export-notes-to-pdf "REVEAL_EXPORT_NOTES_TO_PDF" nil
                                   org-re-reveal-export-notes-to-pdf t)
      (:reveal-external-plugins "REVEAL_EXTERNAL_PLUGINS" nil org-re-reveal-external-plugins t)
      (:reveal-extra-attr "REVEAL_EXTRA_ATTR" nil org-re-reveal-extra-attr nil)
      (:reveal-extra-css "REVEAL_EXTRA_CSS" nil org-re-reveal-extra-css newline)
      (:reveal-extra-options "REVEAL_EXTRA_OPTIONS" nil org-re-reveal-extra-options t)
      (:reveal-extra-scripts "REVEAL_EXTRA_SCRIPTS" nil org-re-reveal-extra-scripts t)
      (:reveal-head-preamble "REVEAL_HEAD_PREAMBLE" nil org-re-reveal-head-preamble newline)
      (:reveal-highlight-css "REVEAL_HIGHLIGHT_CSS" nil org-re-reveal-highlight-css t)
      (:reveal-highlight-url "REVEAL_HIGHLIGHT_URL" nil org-re-reveal-highlight-url t)
      (:reveal-hlevel "REVEAL_HLEVEL" nil nil t)
      (:reveal-init-script "REVEAL_INIT_SCRIPT" nil org-re-reveal-init-script space)
      (:reveal-klipse-css-url "REVEAL_KLIPSE_CSS_URL" nil org-re-reveal-klipse-css t)
      (:reveal-klipse-extra-config "REVEAL_KLIPSE_EXTRA_CONFIG" nil org-re-reveal-klipse-extra-config newline)
      (:reveal-klipse-js-url "REVEAL_KLIPSE_JS_URL" nil org-re-reveal-klipse-js t)
      (:reveal-klipse-setup "REVEAL_KLIPSE_SETUP" nil org-re-reveal-klipse-setup t)
      (:reveal-margin "REVEAL_MARGIN" nil org-re-reveal-margin t)
      (:reveal-mathjax-url "REVEAL_MATHJAX_URL" nil org-re-reveal-mathjax-url t)
      (:reveal-max-scale "REVEAL_MAX_SCALE" nil org-re-reveal-max-scale t)
      (:reveal-min-scale "REVEAL_MIN_SCALE" nil org-re-reveal-min-scale t)
      (:reveal-miscinfo "REVEAL_MISCINFO" nil nil t)
      (:reveal-multiplex-id "REVEAL_MULTIPLEX_ID" nil org-re-reveal-multiplex-id t)
      (:reveal-multiplex-secret "REVEAL_MULTIPLEX_SECRET" nil org-re-reveal-multiplex-secret t)
      (:reveal-multiplex-socketio-url "REVEAL_MULTIPLEX_SOCKETIO_URL" nil org-re-reveal-multiplex-socketio-url t)
      (:reveal-multiplex-url "REVEAL_MULTIPLEX_URL" nil org-re-reveal-multiplex-url t)
      (:reveal-plugins "REVEAL_PLUGINS" nil org-re-reveal-plugins t)
      (:reveal-postamble "REVEAL_POSTAMBLE" nil org-re-reveal-postamble t)
      (:reveal-postscript "REVEAL_POSTSCRIPT" nil org-re-reveal-postscript t)
      (:reveal-preamble "REVEAL_PREAMBLE" nil org-re-reveal-preamble t)
      (:reveal-root "REVEAL_ROOT" nil org-re-reveal-root t)
      (:reveal-slide-container "REVEAL_SLIDE_CONTAINER" nil org-re-reveal-slide-container t)
      (:reveal-slide-grid-div "REVEAL_SLIDE_GRID_DIV" nil org-re-reveal-slide-grid-div newline)
      (:reveal-slide-footer "REVEAL_SLIDE_FOOTER" nil org-re-reveal-slide-footer t)
      (:reveal-slide-header "REVEAL_SLIDE_HEADER" nil org-re-reveal-slide-header t)
      (:reveal-speed "REVEAL_SPEED" nil org-re-reveal-transition-speed t)
      (:reveal-talk-qr-code "REVEAL_TALK_QR_CODE" nil nil t)
      (:reveal-talk-url "REVEAL_TALK_URL" nil nil t)
      (:reveal-tdm-reservation "REVEAL_TDM_RESERVATION" nil org-re-reveal-tdm-reservation t)
      (:reveal-theme "REVEAL_THEME" nil org-re-reveal-theme t)
      (:reveal-title-slide "REVEAL_TITLE_SLIDE" nil org-re-reveal-title-slide newline)
      (:reveal-title-slide-background "REVEAL_TITLE_SLIDE_BACKGROUND" nil nil t)
      (:reveal-title-slide-background-opacity "REVEAL_TITLE_SLIDE_BACKGROUND_OPACITY" nil nil t)
      (:reveal-title-slide-background-position "REVEAL_TITLE_SLIDE_BACKGROUND_POSITION" nil nil t)
      (:reveal-title-slide-background-repeat "REVEAL_TITLE_SLIDE_BACKGROUND_REPEAT" nil nil t)
      (:reveal-title-slide-background-size "REVEAL_TITLE_SLIDE_BACKGROUND_SIZE" nil nil t)
      (:reveal-title-slide-background-transition "REVEAL_TITLE_SLIDE_BACKGROUND_TRANSITION" nil nil t)
      (:reveal-title-slide-notes "REVEAL_TITLE_SLIDE_NOTES" nil org-re-reveal-title-slide-notes t)
      (:reveal-title-slide-extra-attr "REVEAL_TITLE_SLIDE_EXTRA_ATTR" nil nil space)
      (:reveal-title-slide-state "REVEAL_TITLE_SLIDE_STATE" nil nil t)
      (:reveal-title-slide-timing "REVEAL_TITLE_SLIDE_TIMING" nil nil t)
      (:reveal-toc-slide-class "REVEAL_TOC_SLIDE_CLASS" nil nil t)
      (:reveal-toc-slide-state "REVEAL_TOC_SLIDE_STATE" nil nil t)
      (:reveal-toc-slide-title "REVEAL_TOC_SLIDE_TITLE" nil org-re-reveal-toc-slide-title t)
      (:reveal-toc-slide-background "REVEAL_TOC_SLIDE_BACKGROUND" nil nil t)
      (:reveal-toc-slide-background-opacity "REVEAL_TOC_SLIDE_BACKGROUND_OPACITY" nil nil t)
      (:reveal-toc-slide-background-position "REVEAL_TOC_SLIDE_BACKGROUND_POSITION" nil nil t)
      (:reveal-toc-slide-background-repeat "REVEAL_TOC_SLIDE_BACKGROUND_REPEAT" nil nil t)
      (:reveal-toc-slide-background-size "REVEAL_TOC_SLIDE_BACKGROUND_SIZE" nil nil t)
      (:reveal-toc-slide-background-transition "REVEAL_TOC_SLIDE_BACKGROUND_TRANSITION" nil nil t)
      (:reveal-toc-slide-extra-attr "REVEAL_TOC_SLIDE_EXTRA_ATTR" nil nil space)
      (:reveal-toc-slide-state "REVEAL_TOC_SLIDE_STATE" nil nil t)
      (:reveal-toc-slide-timing "REVEAL_TOC_SLIDE_TIMING" nil nil t)
      (:reveal-trans "REVEAL_TRANS" nil org-re-reveal-transition t)
      (:reveal-tts-dir "REVEAL_TTS_DIR" nil org-re-reveal-tts-dir t)
      (:reveal-tts-name-prefix "REVEAL_TTS_NAME_PREFIX" nil org-re-reveal-tts-name-prefix t)
      (:reveal-tts-sentence-gap "REVEAL_TTS_SENTENCE_GAP" nil org-re-reveal-tts-sentence-gap t)
      (:reveal-tts-start-slide-gap "REVEAL_TTS_START_SLIDE_GAP" nil org-re-reveal-tts-start-slide-gap t)
      (:reveal-tts-end-slide-gap "REVEAL_TTS_END_SLIDE_GAP" nil org-re-reveal-tts-end-slide-gap t)
      (:reveal-version "REVEAL_VERSION" nil org-re-reveal-revealjs-version t)
      (:reveal-viewport "REVEAL_VIEWPORT" nil org-re-reveal-viewport t))

    :translate-alist
    '((headline . org-re-reveal-headline)
      (inner-template . org-re-reveal-inner-template)
      (item . org-re-reveal-item)
      (keyword . org-re-reveal-keyword)
      (link . org-re-reveal-link)
      (latex-environment . org-re-reveal-latex-environment)
      (latex-fragment . org-re-reveal-latex-fragment)
      (plain-list . org-re-reveal-plain-list)
      (section . org-re-reveal-section)
      (src-block . org-re-reveal-src-block)
      (special-block . org-re-reveal-special-block)
      (template . org-re-reveal-template))

    :filters-alist '((:filter-parse-tree . org-re-reveal-filter-parse-tree))))

(defun org-re-reveal-define-menu (symbol value)
  "Define back-end with (new) key bindings.
SYMBOL must be `org-re-reveal-keys' and VALUE its new value."
  (let ((standard (eval (car (get symbol 'standard-value)))))
    (cl-assert
     (eq symbol 'org-re-reveal-keys) nil
     (format "Symbol in org-re-reveal-define-menu unexpected: %s" symbol))
    (cl-assert
     (= (length standard) (length value))
     (format "Value for org-re-reveal-keys must have length %s (same as standard), not %s"
             (length standard) (length value)))
    (set-default symbol value)
    (org-re-reveal-define-backend)))

(defgroup org-export-re-reveal nil
  "Options for exporting Org files to reveal.js HTML pressentations.
See URL `https://oer.gitlab.io/org-re-reveal/Readme.html' for the
Readme of org-re-reveal as reveal.js presentation that is generated
from its Org mode source file in a CI/CD infrastructure on GitLab."
  :tag "Org Export Reveal"
  :group 'org-export)

(defcustom org-re-reveal-keys '(?v ?v ?b ?s)
  "Define keys for export with org-re-reveal.
This list must contain four characters: The first one triggers export
with org-re-reveal (after \\<org-mode-map> \\[org-export-dispatch]).
The remaining three charaters each invoke a different export variant.
One of those characters must be typed after the first one; the
variants are, in sequence: Export to file, export to file followed by
browsing that file, subtree export to file."
  :group 'org-export-re-reveal
  :type '(list (character :tag "Key to trigger export with org-re-reveal")
               (character :tag "Key for export to file")
               (character :tag "Key to browse file after export")
               (character :tag "Key for subtree export to file"))
  :set #'org-re-reveal-define-menu)

(defconst org-re-reveal-revealjs-4-file "dist/reveal.js")
(defconst org-re-reveal-revealjs-3-file "js/reveal.js")
(defconst org-re-reveal-revealjs-pre-3.8-file "lib/js/head.min.js")
(defconst org-re-reveal-file-drive-uri-re "\\`file:///[a-zA-Z]:")

(defun org-re-reveal--file-uri-to-path (string)
  "Extract filesystem path from STRING, which may be a file URI.
Return STRING unchanged if it is no file URI.
Otherwise, extract absolute path, treating URIs with drive letters
separately."
  (if (string-prefix-p "file:///" string)
      (if (string-match org-re-reveal-file-drive-uri-re string)
          ;; Paths with drive letter start without slash.
          (string-remove-prefix "file:///" string)
        ;; Keep leading slash for normal directory names.
        (string-remove-prefix "file://" string))
    string))

(defun org-re-reveal--guess-revealjs-version (info)
  "Guess version of reveal.js with INFO.
Cache guessed version of reveal.js as `:reveal-guessed-revealjs-version':
Use `org-re-reveal-revealjs-version' if it is non-nil.
Otherwise, check for existence of files under `org-re-reveal-root' and
- assign \"4\" if `org-re-reveal-revealjs-4-file' exists;
- otherwise, if `org-re-reveal-revealjs-3-file' exists and
  `org-re-reveal-revealjs-pre-3.8-file' does not exist, assign \"3.8\";
- otherwise, assign \"3\".
Return guessed version string."
  (let ((cached (plist-get info :reveal-guessed-revealjs-version)))
    (if cached
        cached
      (let ((version (plist-get info :reveal-version))
            (root-path
             (file-name-as-directory
              (org-re-reveal--file-uri-to-path (plist-get info :reveal-root)))))
        (when (and (org-re-reveal--remote-file-p root-path)
                   (not version))
          (org-re-reveal--abort-with-message-box
           "Remote URL for reveal.js does not work with version guessing.  Customize `org-re-reveal-revealjs-version'."))
        (let ((guessed
               (if version
		   version
		 (if (file-exists-p
                      (concat root-path org-re-reveal-revealjs-4-file))
                     "4"
		   (if (and
			(file-exists-p
			 (concat root-path org-re-reveal-revealjs-3-file))
			(not (file-exists-p
                              (concat root-path
                                      org-re-reveal-revealjs-pre-3.8-file))))
                       "3.8"
                     "3")))))
          (plist-put info :reveal-guessed-revealjs-version guessed)
          guessed)))))

(defun org-re-reveal--setup-paths (info)
  "Setup paths for reveal.js based in INFO."
  (org-re-reveal--guess-revealjs-version info)
  (let ((revealjs-version (org-re-reveal--guess-revealjs-version info)))
    (cond ((string= revealjs-version "4")
	   (plist-put info :reveal-script-files '("dist/reveal.js"))
	   (plist-put info :reveal-css-path "dist"))
          ((string= revealjs-version "3.8")
	   (plist-put info :reveal-script-files '("js/reveal.js"))
	   (plist-put info :reveal-css-path "css"))
          (t (plist-put info :reveal-script-files '("lib/js/head.min.js" "js/reveal.js"))
	     (plist-put info :reveal-css-path "css")))))

(defcustom org-re-reveal-revealjs-version nil
  "Specify version of reveal.js.
If nil, `org-re-reveal' tries to guess the version, which works if
`org-re-reveal-root' is a local directory.
You can specify the version per file with keyword REVEAL_VERSION."
  :group 'org-export-re-reveal
  :type '(choice (const :tag "reveal.js 4.0 and later" "4")
                 (const :tag "reveal.js 3.8 and 3.9" "3.8")
                 (const :tag "reveal.js before 3.8" "3")
                 (const :tag "Guess" nil))
  :package-version '(org-re-reveal . "3.0.0"))

(defcustom org-re-reveal-root "./reveal.js"
  "Specify root directory of reveal.js.
The root directory is the one containing dist/reveal.js (reveal.js 4.x)
or js/reveal.js (earlier versions).
If you set this to a CDN location, make sure that
`org-re-reveal-revealjs-version' is set properly as well."
  :group 'org-export-re-reveal
  :type 'string
  :package-version '(org-re-reveal . "3.0.3"))

(defcustom org-re-reveal-tdm-reservation nil
  "If non-nil, implement TDM Reservation Protocol (TDMRep).
This protocol enables European rightsholders to restrict the use of their
contents for text and data mining (TDM) purposes in a machine readable way,
see URL `https://www.w3.org/2022/tdmrep/'.
This variable can be nil, t, or a string.
If non-nil, add a \"tdm-reservation\" meta element.
In addition, if it is a string starting with \"http\", use it as URL
for a \"tdm-policy\" meta element."
  :group 'org-export-re-reveal
  :type '(choice (const nil) string)
  :package-version '(org-re-reveal . "3.22.0"))

(defcustom org-re-reveal-hlevel 1
  "Specify minimum level of headings for grouping into vertical slides."
  :group 'org-export-re-reveal
  :type 'integer)

(defun org-re-reveal--get-hlevel (info)
  "Get HLevel value safely for INFO.
If option \"REVEAL_HLEVEL\" is set, retrieve integer value from it,
else get value from custom variable `org-re-reveal-hlevel'."
  (let ((hlevel-str (plist-get info :reveal-hlevel)))
    (if hlevel-str (string-to-number hlevel-str)
      org-re-reveal-hlevel)))

(defcustom org-re-reveal-title-slide 'auto
  "If nil or empty string, do not insert a title slide.
Otherwise (`auto' or non-empty string), insert title slide.
When `auto', generate minimal automatic title slide with
`org-re-reveal--auto-title-slide-template'.
When set to a string, use this string as format string for the title
slide, where the following %-sequences are allowed:

  %t for the title.
  %s for the subtitle.
  %a for the author's name.
  %e for the author's email.
  %d for the date.
  %A for the author's academic title (set with #+REVEAL_ACADEMIC_TITLE).
  %q for the name of a file to a QR code (set with #+REVEAL_TALK_QR_CODE).
  %u for the URL of the presentation (set with #+REVEAL_TALK_URL).
  %m for misc information (set with #+REVEAL_MISCINFO).
  %n for notes on the title slide (see `org-re-reveal-title-slide-notes');
     obsolete since org-re-reveal 3.25.0; a notes block before the first
     headline defines notes on the title slide now.
  %% for a literal %.

Alternatively, the string can also be the name of a file with the title
slide's HTML code (containing the above escape sequences)."
  :group 'org-export-re-reveal
  :type '(choice (const :tag "No title slide" nil)
                 (const :tag "Auto title slide" auto)
                 (string :tag "Custom title slide")))

(defcustom org-re-reveal-transition "convex"
  "Reveal transistion style."
  :group 'org-export-re-reveal
  :type '(radio (const "none")
                (const "fade")
                (const "slide")
                (const "convex")
                (const "concave")
                (const "zoom")
                (string :tag "Other transition")))

(defcustom org-re-reveal-transition-speed "default"
  "Reveal transistion speed."
  :group 'org-export-re-reveal
  :type '(radio (const "default")
                (const "fast")
                (const "slow")
                (string :tag "Other transition speed")))

(defcustom org-re-reveal-theme "black"
  "Reveal theme.
Note that most themes load fonts from remote servers, which (a)
obviously limits offline use of presentations and (b) is not privacy
friendly.  See URL `https://github.com/hakimel/reveal.js/issues/2491'
and URL `https://github.com/google/fonts/issues/1495'.
In addition to built-in themes, you can use a path ending in \".css\"
to specify a custom theme."
  :group 'org-export-re-reveal
  :type '(radio (const "beige")
                (const "black")
                (const "blood")
                (const "league")
                (const "moon")
                (const "night")
                (const "serif")
                (const "simple")
                (const "sky")
                (const "solarized")
                (const "white")
                (string :tag "Other theme"))
  :package-version '(org-re-reveal . "3.7.0"))

(defcustom org-re-reveal-show-notes nil
  "Control `showNotes' option for presentations.
This variable is meant to show notes when viewing presentations.
It is combined with `org-re-reveal-export-notes-to-pdf' for PDF export."
  :group 'org-export-re-reveal
  :type 'boolean
  :package-version '(org-re-reveal . "3.23.0"))

(defcustom org-re-reveal-export-notes-to-pdf nil
  "Control `showNotes' option for PDF export.
With the default value nil, default reveal.js behavior applies without
an option `showNotes'; see URL `https://revealjs.com/pdf-export/'.
With t, notes appear in an overlay box.
Any string value is copied literally to the `showNotes' option; meant
to be used with \"separate-page\"."
  :group 'org-export-re-reveal
  :type '(choice
          (const :tag "Do not add \"showNotes\" option" nil)
          (const :tag "Add \"showNotes: true\"" t)
          (const :tag "Add \"showNotes: 'separate-page'\"" "separate-page"))
  :package-version '(org-re-reveal . "3.1.0"))

(defcustom org-re-reveal-extra-scripts nil
  "List of extra scripts.
Each list element can be the filename or URL of a JavaScript file or an
entire HTML script element.
If relative filenames are used, they must be relative to the presentation's
HTML file."
  :group 'org-export-re-reveal
  :type '(repeat string)
  :package-version '(org-re-reveal . "2.10.0"))

(defcustom org-re-reveal-extra-attr nil
  "Global Reveal Extra Attrs for all slides."
  :group 'org-export-re-reveal
  :type '(choice
          string
          (const nil)))

(defcustom org-re-reveal-extra-css ""
  "Newline separated names (or remote URLs) for extra CSS files."
  :group 'org-export-re-reveal
  :type 'string)

(defcustom org-re-reveal-multiplex-id ""
  "The ID to use for multiplexing.
E.g., per README of reveal.js 3.8.0, generate id and secrete by visiting
URL `https://reveal-js-multiplex-ccjbegmaii.now.sh/'.
To enable multiplex, see `org-re-reveal-plugins'."
  :group 'org-export-re-reveal
  :type 'string)

(defcustom org-re-reveal-multiplex-secret ""
  "The secret to use for the main presentation.
E.g., per README of reveal.js 3.8.0, generate id and secrete by visiting
URL `https://reveal-js-multiplex-ccjbegmaii.now.sh/'.
To enable multiplex, see `org-re-reveal-plugins'."
  :group 'org-export-re-reveal
  :type 'string)

(defcustom org-re-reveal-multiplex-url
  "https://reveal-js-multiplex-ccjbegmaii.now.sh"
  "The url of the socketio server.
To enable multiplex, see `org-re-reveal-plugins'."
  :group 'org-export-re-reveal
  :type 'string
  :package-version '(org-re-reveal . "2.1.0"))

(defcustom org-re-reveal-multiplex-socketio-url
  "https://cdn.socket.io/socket.io-1.3.5.js"
  "The url of the socketio.js library.
To enable multiplex, see `org-re-reveal-plugins'."
  :group 'org-export-re-reveal
  :type 'string
  :package-version '(org-re-reveal . "2.1.0"))

(defcustom org-re-reveal-multiplex-client-ext "_client"
  "Extension to insert in names of multiplex client presentations.
The name of the multiplex main presentation is derived from
`org-html-extension'.  For client presentations, the value of this
variable is inserted before the HTML extension."
  :group 'org-export-re-reveal
  :type 'string
  :package-version '(org-re-reveal . "3.2.0"))

(defcustom org-re-reveal-client-multiplex-filter nil
  "If non-nil, a regular expression to filter multiplex client publication.
When using `org-re-reveal-publish-to-reveal-client', by default all Org
files are also published as multiplex client files (which roughly
doubles the amount of time necessary for publication).  If you have got
a mix of Org files that use multiplexing and that do not, set to this
variable to a regular expression matching files for which a multiplex
client file should be generated."
  :group 'org-export-re-reveal
  :type '(choice
          regexp
          (const nil))
  :package-version '(org-re-reveal . "2.12.0"))

(defcustom org-re-reveal-control t
  "If nil, hide presentation control arrows.
See URL `https://revealjs.com/config/'."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-progress t
  "If nil, progress bar.
See URL `https://revealjs.com/config/'."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-history nil
  "If non-nil, push each slide change to the browser history.
See URL `https://revealjs.com/config/'."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-center t
  "If nil, do not center slides vertically.
See URL `https://revealjs.com/config/'."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-rolling-links nil
  "Option to show links with rolling effect for reveal.js 3.x."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-slide-number "c"
  "Choose slide number format.
See URL `https://revealjs.com/config/'."
  :group 'org-export-re-reveal
  :type '(radio (const :tag "horizontal . vertical slide number" "h.v")
                (const :tag "horizontal / vertical slide number" "h/v")
                (const :tag "flattened slide number" "c")
                (const :tag "flattened slide number / total slides" "c/t")
                (string :tag "Other slide number format")))

(defcustom org-re-reveal-keyboard t
  "If nil, disable keyboard navigation.
See URL `https://revealjs.com/config/'."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-mobile-app nil
  "If t, add meta tags to indicate mobile web app capabilities.
Specifically, add \"mobile-web-app-capable\" and
\"apple-mobile-web-app-capable\".  Then, users can add presentations
to their home screen to have them open without address bar.
For backward compatibility, this option is nil.  With `oer-reveal', this
is activated by default (and, thus, also with `emacs-reveal')."
  :group 'org-export-re-reveal
  :type 'boolean
  :package-version '(org-re-reveal . "3.13.0"))

(defcustom org-re-reveal-viewport nil
  "If non-nil, add string as viewport meta tag.
The given choice makes the presentation user-scalable (e.g.,
pinch-to-zoom on mobile devices).
For backward compatibility, this option is nil.  With `oer-reveal', this
is activated by default (and, thus, also with `emacs-reveal')."
  :group 'org-export-re-reveal
  :type '(choice
          (const :tag "Do not add viewport tag" nil)
          (const :tag "Make presentation user-scalable" "width=device-width, initial-scale=1.0, minimum-scale=0.1, maximum-scale=10.0, user-scalable=yes")
          (string :tag "Custom configuration"))
  :package-version '(org-re-reveal . "3.24.0"))

(defcustom org-re-reveal-mousewheel nil
  "If t, enable mousewheel navigation.
See URL `https://revealjs.com/config/'."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-fragmentinurl nil
  "If t, enable fragmentInURL setting.
See URL `https://revealjs.com/config/'."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-hashonebasedindex nil
  "If t, enable hashOneBasedIndex setting.
See URL `https://revealjs.com/config/'."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-pdfseparatefragments t
  "If nil, disable pdfSeparateFragments setting.
See URL `https://revealjs.com/config/'."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-defaulttiming nil
  "If non-nil, specify defaultTiming for speaker notes view.
See URL `https://revealjs.com/speaker-view/'.
For indivual timing of specific slides, use \"REVEAL_TITLE_SLIDE_TIMING\" for
the title slide, and assign data-timing attributes to other headlines/slides.
Alternatively, use `org-re-reveal-totaltime'."
  :group 'org-export-re-reveal
  :type '(choice integer (const nil)))

(defcustom org-re-reveal-totaltime nil
  "If non-nil, specify totalTime for speaker notes view.
See URL `https://revealjs.com/speaker-view/'.
For indivual timing of specific slides, use \"REVEAL_TITLE_SLIDE_TIMING\" for
the title slide, and assign data-timing attributes to other headlines/slides.
If this variable is set, reveal.js ignores defaultTiming."
  :group 'org-export-re-reveal
  :type '(choice integer (const nil)))

(defcustom org-re-reveal-overview t
  "If nil, disable slide overview mode.
See URL `https://revealjs.com/config/'."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-subtree-with-title-slide nil
  "If t, export title slide also for subtree exports."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-width nil
  "Slide width as positive integer (pixels) or string (percentage) or nil."
  :group 'org-export-re-reveal
  :type '(choice integer string (const nil))
  :package-version '(org-re-reveal . "1.1.4"))

(defcustom org-re-reveal-height nil
  "Slide height as positive integer (pixels) or string (percentage) or nil."
  :group 'org-export-re-reveal
  :type '(choice integer string (const nil))
  :package-version '(org-re-reveal . "1.1.4"))

(defcustom org-re-reveal-margin "-1"
  "Slide margin (in a string)."
  :group 'org-export-re-reveal
  :type 'string)

(defcustom org-re-reveal-min-scale "-1"
  "Minimum bound for scaling slide (in a string)."
  :group 'org-export-re-reveal
  :type 'string)

(defcustom org-re-reveal-max-scale "-1"
  "Maximum bound for scaling slide (in a string)."
  :group 'org-export-re-reveal
  :type 'string)

(defcustom org-re-reveal-extra-options nil
  "Extra options to be passed to Reveal.initialize().
Useful to specify options without keyword in org-re-reveal, e.g.,
\"controlsTutorial: false, controlsLayout: \\='edges\\='\".
Individual options are separated by comma.
For the current list of reveal.js options, see URL
`https://github.com/hakimel/reveal.js/#configuration.'"
  :group 'org-export-re-reveal
  :type '(choice string (const nil)))

(defcustom org-re-reveal-mathjax-url
  "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
  "Default MathJax URL.
Set to empty string to avoid loading of MathJax even if LaTeX formulas are used.
This variable is ignored if the math plugin of reveal.js is activated via
`org-re-reveal-plugins'."
  :group 'org-export-re-reveal
  :type 'string
  :package-version '(org-re-reveal . "3.16.0"))

(defcustom org-re-reveal-preamble nil
  "Preamble contents."
  :group 'org-export-re-reveal
  :type '(choice (const nil) string))

(defcustom org-re-reveal-head-preamble nil
  "Preamble contents for head part."
  :group 'org-export-re-reveal
  :type '(choice (const nil) string))

(defcustom org-re-reveal-postamble nil
  "Postamble contents."
  :group 'org-export-re-reveal
  :type '(choice (const nil) string))

(defcustom org-re-reveal-postscript nil
  "Postscript contents.
Insert after reveal.js initialization before closing body tag."
  :group 'org-export-re-reveal
  :type '(choice (const nil) string)
  :package-version '(org-re-reveal . "3.9.0"))

(defcustom org-re-reveal-body-attrs nil
  "Attribute string to assign to body element.
By default, no attributes are assigned."
  :group 'org-export-re-reveal
  :type '(choice (const nil) string))

(defcustom org-re-reveal-slide-header nil
  "Specify HTML content for slide header, or nil.
If non-nil, embed inside `org-re-reveal-slide-header-html' to create header."
  :group 'org-export-re-reveal
  :type '(choice (const nil) string))

(defcustom org-re-reveal-slide-header-html
  "<div class=\"slide-header\">%s</div>\n"
  "HTML format string as container for slide header.
This format string must contain a single \"%s\" sequence, which is replaced by
`org-re-reveal-slide-header'."
  :group 'org-export-re-reveal
  :type 'string)

(defcustom org-re-reveal-global-header nil
  "If non nil, display slide header also on title and toc slide.
Header is defined by `org-re-reveal-slide-header'."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-global-footer nil
  "If non-nil, display slide footer also on title and toc slide.
Footer is defined by `org-re-reveal-slide-footer'."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-toc-footer nil
  "If non-nil, display slide footer also on toc slide.
Footer is defined by `org-re-reveal-slide-footer'."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-slide-container "%s"
  "Specify HTML container for slide contents.
See `org-re-reveal-slide-grid-div' for a variant that wraps not only slide
contents but also the headline (and header and footer if they exist).
This must be a format string with a single \"%s\" sequence, e.g.,
\"<div attr=value>%s</div>\".
See issue URL `https://gitlab.com/oer/org-re-reveal/-/issues/69'."
  :group 'org-export-re-reveal
  :type 'string
  :package-version '(org-re-reveal . "3.11.0"))

(defcustom org-re-reveal-slide-grid-div ""
  "Specify HTML container for grid layouts.
Maybe use \"<div class=\"grid-wrapper\">\n\" to wrap header, headline,
slide contents, and footer in div element.  As closing div tags are
hardcoded, this must be an opening div tag.
See issue URL `https://gitlab.com/oer/org-re-reveal/-/issues/69'."
  :group 'org-export-re-reveal
  :type 'string
  :package-version '(org-re-reveal . "3.13.0"))

(defcustom org-re-reveal-slide-footer nil
  "Specify HTML content for slide footer, or nil.
If non-nil, embed inside `org-re-reveal-slide-footer-html' to create footer."
  :group 'org-export-re-reveal
  :type '(choice (const nil) string))

(defcustom org-re-reveal-slide-footer-html
  "<div class=\"slide-footer\">%s</div>\n"
  "HTML format string as container for slide footer.
This format string must contain a single \"%s\" sequence, which is replaced by
`org-re-reveal-slide-footer'."
  :group 'org-export-re-reveal
  :type 'string)

(defcustom org-re-reveal-toc-slide-title nil
  "If non-nil, string to display as title of toc slide.
When nil or empty string, use a language-specific translation
of \"Table of Contents\".
Otherwise, use given string without translation."
  :group 'org-export-re-reveal
  :type '(choice (const nil) string)
  :package-version '(org-re-reveal . "3.12.0"))

(defcustom org-re-reveal-notes-format-string
  "<aside class=\"notes\">\n%s\n</aside>\n"
  "HTML format string to construct aside element for notes.
Must constain exactly one %-sequence \"%s\"."
  :group 'org-export-re-reveal
  :type 'string
  :package-version '(org-re-reveal . "3.3.0"))

(defcustom org-re-reveal-title-slide-notes nil
  "Name of file to define speaker notes on title slide or nil.
If non-nil, export contents of the file as speaker notes for the title slide.
To insert speaker notes into the title slide, use \"%n\" as specified
for `org-re-reveal-title-slide'."
  :group 'org-export-re-reveal
  :type '(choice (const nil) file)
  :package-version '(org-re-reveal . "3.3.0"))
(make-obsolete-variable
 'org-re-reveal-title-slide-notes
 "insert a notes block before the first headline instead."
 "3.25.0")

(defcustom org-re-reveal-default-frag-style nil
  "Default fragment style."
  :group 'org-export-re-reveal
  :type '(choice (const nil) string))

(defcustom org-re-reveal-plugins
  '(markdown notes search zoom)
  "Default builtin plugins.

By default, multiplex is not enabled.  With reveal.js 4.x, multiplex
needs to be downloaded separately.
Include `multiplex' in this variable to enable it.

This variable, like lots of other variables, can be overridden
in the org buffer comments as follows:
  #+REVEAL_PLUGINS: (markdown zoom notes multiplex)"
  :group 'org-export-re-reveal
  :type '(set
          (const markdown)
          (const highlight)
          (const zoom)
          (const notes)
          (const search)
          (const :tag "math (reveal.js 4.x)" math)
          (const :tag "multiplex (not included in reveal.js 4.x)" multiplex)
          (const :tag "classList (absent from modern reveal.js)" classList)
          (const :tag "remotes (absent from modern reveal.js)" remotes))
  :package-version '(org-re-reveal . "3.0.0"))

(defcustom org-re-reveal-plugin-config
  '((highlight "RevealHighlight" "plugin/highlight/highlight.js")
    (markdown "RevealMarkdown" "plugin/markdown/markdown.js")
    (math "RevealMath" "plugin/math/math.js")
    (notes "RevealNotes" "plugin/notes/notes.js")
    (search "RevealSearch" "plugin/search/search.js")
    (zoom "RevealZoom" "plugin/zoom/zoom.js"))
  "Initialization for reveal.js 4.x plugins.
This is a list of lists.  Each list consists of
- the plugin name, listed in `org-re-reveal-plugins',
- the JavaScript name for the plugin,
- the name of the JavaScript file,
- zero or more names of CSS files.

Starting with version 3.6.0 or org-re-reveal, the name of the
JavaScript file can also be a URL."
  :group 'org-export-oer-reveal
  :type '(repeat
          (list
           (symbol :tag "Plugin name")
           (string :tag "JavaScript plugin name")
           (string :tag "JavaScript file name")
           (repeat (string :tag "CSS file name"))))
  :package-version '(org-re-reveal . "3.0.0"))

(defcustom org-re-reveal-external-plugins nil
  "Additional third-party plugins to load with reveal.js.
This is either an alist or a filename.
In case of an alist, each entry should contain a name and an expression
of the following form:
\"{src: '%srelative/path/from/reveal/root', async:true/false,
   condition: jscallbackfunction(){}}\"
In case of a file, its lines must be expressions of the above form.
Note that some plugins have dependencies such as jquery; these must be
included here as well, BEFORE the plugins that depend on them."
  :group 'org-export-re-reveal
  :type '(choice alist file))

(defcustom org-re-reveal-single-file nil
  "Export presentation into one single HTML file.
That file embeds JS scripts and pictures.  Export aborts if necessary
resources are not available locally.
See also `org-re-reveal-embed-local-resources'."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-embed-local-resources nil
  "Export local resources into HTML file of presentation.
In contrast to `org-re-reveal-single-file', this option only embeds locally
available resources.  Thus, it can also be used with a remote reveal.js
installation."
  :group 'org-export-re-reveal
  :type 'boolean
  :package-version '(org-re-reveal . "3.10.0"))

(defcustom org-re-reveal-inter-presentation-links nil
  "If non nil, try to convert links between presentations."
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-init-script nil
  "Custom script to be passed to Reveal.initialize."
  :group 'org-export-re-reveal
  :type '(choice (const nil) string))

(defcustom org-re-reveal-highlight-css 'zenburn
  "Highlight.js CSS style.
Styles distributed with reveal.js are monokai and zenburn.
Alternatively, use any file path or URL.  The file path may contain the
placeholder \"%r\", to be replaced with the root directory of reveal.js."
  :group 'org-export-re-reveal
  :type '(choice (const monokai)
                 (const zenburn)
                 (string :tag "Other CSS file or URL"))
  :package-version '(org-re-reveal . "3.0.0"))

(defcustom org-re-reveal-highlight-url nil
  "Location of Highlight.js.
If nil (default), the local plugin file is used."
  :group 'org-export-re-reveal
  :type '(choice (const nil) string))

(defcustom org-re-reveal-note-key-char "n"
  "If not nil, register key for Org structure completion for speaker notes.
When `<' followed by the key character are
typed and then the completion key is pressed, which is usually
`TAB', \"#+BEGIN_NOTES\" and \"#+END_NOTES\" is inserted (maybe in
lower-case).  See \"Readme.org\" how to make this work with Org version
9.2 or later.

The default value is \"n\".  Set the variable to nil to disable
registering the completion."
  :group 'org-export-re-reveal
  :type '(choice (const nil) string))

(defcustom org-re-reveal-no-htmlize-src nil
  "For syntax highlighting with org-re-reveal, three options exist:
1. Use reveal.js plugin highlight for syntax highlighting with highlight.js.
   This applies to all source code blocks.
2. If plugin highlight is not enabled, by default the library htmlize
   is used.
3. Do not use highlight and customize this variable to t.
   This disables syntax highlighting but you can activate htmlize for
   individual source code blocks with attributes:
\"#+ATTR_REVEAL: :htmlize t\""
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-klipsify-src nil
  "Set to non-nil to enable live code execution with klipse.
See test-cases/test-klipsify.org in the source repository for examples.
To export a source code block without klipse, use the following:
\"#+ATTR_REVEAL: :no-klipsify t\""
  :group 'org-export-re-reveal
  :type 'boolean)

(defcustom org-re-reveal-klipse-css "https://storage.googleapis.com/app.klipse.tech/css/codemirror.css"
  "Location of the codemirror css file for use with klipse."
  :group 'org-export-re-reveal
  :type 'string)

(defcustom org-re-reveal-klipse-extra-css "<style>
/* Position computations of klipse get confused by reveal.js's scaling.
   Hence, scaling should be disabled with this code.  Fix height of code area
   with scrollbar (use overflow instead of overflow-y to restore CodeMirror
   setting afterwards): */
.reveal section pre { max-height: 70vh; height: auto; overflow: auto; }
/* Reset some reveal.js and oer-reveal settings: */
.reveal section pre .CodeMirror pre { font-size: 2em; box-shadow: none; width: auto; padding: 0.4em; display: block; overflow: visible; }
/* Enlarge cursor: */
.CodeMirror-cursor { border-left: 3px solid black; }
</style>\n"
  "CSS string to ensure compatibility between klipse and reveal.js."
  :group 'org-export-re-reveal
  :type 'string
  :package-version '(org-re-reveal . "2.0.1"))

(defcustom org-re-reveal-klipse-codemirror nil
  "If not nil, a string to pass as CodeMirror options to \"klipse_setting\"."
  :group 'org-export-re-reveal
  :type '(choice (const nil) string))

(defcustom org-re-reveal-klipse-js
  "https://storage.googleapis.com/app.klipse.tech/plugin/js/klipse_plugin.js"
  "Location of the klipse js source code.
The minified version may not work, see URL
`https://github.com/viebel/klipse/issues/334'."
  :group 'org-export-re-reveal
  :type 'string
  :package-version '(org-re-reveal . "1.1.11"))

(defcustom org-re-reveal-klipse-setup
  '(("clojure" "selector" "language-klipse")
    ("html" "selector_eval_html" "language-klipse-html")
    ("javascript" "selector_eval_js" "language-klipse-javascript")
    ("js" "selector_eval_js" "language-klipse-js")
    ("php" "selector_eval_php" "language-klipse-php")
    ("python" "selector_eval_python_client" "language-klipse-python")
    ("ruby" "selector_eval_ruby" "language-klipse-ruby")
    ("scheme" "selector_eval_scheme" "language-klipse-scheme")
    ("sql" "selector_sql" "language-klipse-sql"))
  "Supported klipse languages with selectors.
This is a list of triples (language  selectorname selectorvalue).
Each language needs to be the language of an Org source block.
For existing names of klipse selectors, see URL
`https://github.com/viebel/klipse/blob/master/README.md#page-level-configuration'.
If additional languages work for you, maybe you could report that in issue #23
at URL `https://gitlab.com/oer/org-re-reveal/issues/23'?"
  :group 'org-export-re-reveal
  :type '(repeat
          (list
           (string :tag "Language")
           (string :tag "Selector name")
           (string :tag "CSS class")))
  :package-version '(org-re-reveal . "1.1.11"))

(defvar org-re-reveal-klipse-languages
  (mapcar #'car org-re-reveal-klipse-setup)
  "List of languages supported by org-re-reveal.")

(defcustom org-re-reveal-klipse-extra-config nil
  "If not nil, extra JavaScript string to execute for klipse initialization.
E.g., window.klipse_settings.editor_type = \"html\"; for SQL."
  :group 'org-export-re-reveal
  :type '(choice (const nil) string))

(defcustom org-re-reveal-generate-custom-ids t
  "If t, generate CUSTOM_IDs for headings that don't have one.
Set to nil to revert to old behavior, where HTML section elements have
content hashes as \"id\" attributes, which change when slide contents
change.  With the default of t, generate CUSTOM_ID for headlines
missing such a property, by using the value of the headline's number.
This results in more stable URLs when working on presentations and
reloading slides.  You may want to set \"#+OPTIONS: reveal_history:t\"
to see the section identifiers as URL fragments in the address bar,
and you should not disable section numbering (for unnumbered
headlines, hash IDs are used unless a CUSTOM_ID is present).
For CSS code to hide section numbers if necessary, see
URL `https://github.com/yjwen/org-reveal/pull/284'."
  :group 'org-export-re-reveal
  :type 'boolean
  :package-version '(org-re-reveal . "1.1.3"))

(defvar org-re-reveal--slide-id-prefix "slide-"
  "Prefix to use in ID attributes of slide elements.")

(defvar org-re-reveal--href-fragment-prefix
  (concat "/" org-re-reveal--slide-id-prefix)
  "Prefix to use when linking to specific slides.
The default uses a slash between hash sign and slide ID,
which leads to broken links that are not understood outside reveal.js.
See there: https://github.com/hakimel/reveal.js/issues/2276")

(defcustom org-re-reveal-with-tts nil
  "If non-nil, specify voice and create text files for TTS generation.
Please see the test case test-notes-for-tts.org for an example.

Note that org-re-reveal only produces text files for a text-to-speech process
that needs to be implemented elsewhere, e.g., in emacs-reveal.
See URL `https://oer.gitlab.io/emacs-reveal-howto/tts-howto.html' for a
demo presentation.

Emacs-reveal activates the audio slideshow plugin, see
URL `https://github.com/rajgoel/reveal.js-plugins/tree/master/audio-slideshow',
to play generated audio.  That plugin has an option `defaultAudios'
where audio file names are derived from slide and fragment indices.
When section numbers are enabled (see `org-export-with-section-numbers'),
org-re-reveal prepares file names according to the format of
`defaultAudios'.

If you disable slide numbers (e.g., with Org option `num:nil') but if
you still want to use the audio slideshow plugin, you *must* provide
file names for generated audio yourself, using `:audio-name' as
attribute for each block of notes.  If you disable slide numbers but do
not provide audio names, export fails with an error message.
Manually chosen audio names can again mimic the default names of the
slideshow plugin or be names of your choice (to be used with
\":reveal_extra_attr: data-audio-src=...\")."
  :group 'org-export-re-reveal
  :type '(choice (const :tag "No TTS" nil)
                 (const :tag "Speechbrain (US female)" speechbrain)
                 (const :tag "CLB (SpeechT5, US female)" CLB)
                 (const :tag "SLT (SpeechT5, US female)" SLT)
                 (const :tag "BDL (SpeechT5, US male)" BDL)
                 (const :tag "RMS (SpeechT5, US male)" RMS)
                 (const :tag "KSP (SpeechT5, Indian female)" KSP))
  :package-version '(org-re-reveal . "3.19.0"))

(defvar org-re-reveal-pub-dir nil
  "Record publishing directory.")

(defcustom org-re-reveal-tts-dir
  (file-name-as-directory "tts")
  "Target directory for text files as basis for TTS.
When publishing projects, this directory is a child of the
publishing directory."
  :group 'org-export-re-reveal
  :type 'directory
  :package-version '(org-re-reveal . "3.19.1"))

(defun org-re-reveal--tts-dir (info)
  "Return directory under which to create text files from INFO."
  (let ((tts-dir (plist-get info :reveal-tts-dir)))
    (if org-re-reveal-pub-dir
        (concat org-re-reveal-pub-dir tts-dir)
      tts-dir)))

(defcustom org-re-reveal-tts-name-prefix "presentation"
  "Prefix to use for names related to TTS.
This string is used in `org-re-reveal--tts-index-name' and should be passed
to the process creating audio files as well as to the audio slideshow plugin.
Different prefixes are necessary for projects/courses with multiple
presentations."
  :group 'org-export-re-reveal
  :type 'string
  :package-version '(org-re-reveal . "3.19.0"))

(defcustom org-re-reveal-tts-sentence-gap 1.0
  "Gap/silence to add between sentences."
  :group 'org-export-re-reveal
  :type 'number
  :package-version '(org-re-reveal . "3.19.0"))

(defcustom org-re-reveal-tts-start-slide-gap 2.0
  "Gap/silence at beginning of slide."
  :group 'org-export-re-reveal
  :type 'number
  :package-version '(org-re-reveal . "3.21.0"))

(defcustom org-re-reveal-tts-end-slide-gap 1.0
  "Gap/silence at end of slide."
  :group 'org-export-re-reveal
  :type 'number
  :package-version '(org-re-reveal . "3.21.0"))

(defcustom org-re-reveal-tts-normalize-table
  '(("[ \t][ \t]+" " ") ; Replace horizontal whitespace.
    ("[ \t]+\n" "\n")   ; Remove trailing whitespace.
    ("’" "'")           ; Replace curly apostrophe.
    ;; If a space precedes a break element, replace with newline:
    ("[ ]+\\(<break time=[^>]+>\\)" "\n\\1")
    ;; If something else precedes a break element, keep it:
    ("\\([^\n]\\)\\(<break time=[^>]+>\\)" "\\1\n\\2")
    ;; Similarly for (non-) space following break elements:
    ("\\(<break time=[^>]+>\\)[ ]+" "\\1\n")
    ("\\(<break time=[^>]+>\\)\\([^\n]\\)" "\\1\n\\2"))
    "Normalization table understood by `iso-translate-conventions'.
Such a table contains a list of 2-element lists.  Both elements are regular
expressions, where occurrences of the first one are replaced by the second
one.
Currently:
- Replace several whitespaces with one.
- Avoiding some UTF symbols.
- Make sure that SSML break elements appear on lines of their own.

TODO: What about current limitations of TTS?  Where should preprocessing be
applied?  Here or in the TTS implementation?
- Deal with abbreviations.  CPU, CPUs, TTS, ...
  In contrast, RAM sounds right.
- Numbers are not read."
  :group 'org-export-re-reveal
  :type '(repeat (list string string))
  :package-version '(org-re-reveal . "3.20.0"))

(defun org-re-reveal--if-format (fmt val)
  "Apply `format' to FMT and VAL if VAL is a number or non-empty string.
Otherwise, return empty string."
  (if (or (numberp val)
          (and (stringp val) (> (length val) 0)))
      (format fmt val)
    ""))

(defun org-re-reveal--sentences-in-lines (text)
  "Return string for TEXT where each sentence starts on a new line.
Replace each newline (which, assuming wrapped texts, may or may not end
sentences or paragraphs) with two space characters (which should not hurt).
Then, insert a newline after each sentence (determined by `forward-sentence').
Also perform replacements based on `org-re-reveal-tts-normalize-table'."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (while (re-search-forward "[ ]*\n[ ]*" nil t)
      (replace-match "  "))
    (goto-char (point-min))
    (let* ((prev (point))
           (next (forward-sentence)))
      (while (and (not (null next))
                  (not (= prev next)))
        ;; End sentence with newline, remove whitespace at bol.
        (insert "\n")
        (when (re-search-forward "[ \t\n]+" nil t)
          (replace-match ""))
        (setq prev (point)
              next (ignore-errors (forward-sentence)))))
    (iso-translate-conventions (point-min) (point-max)
                               org-re-reveal-tts-normalize-table)
    (buffer-string)))

(defun org-re-reveal--notes-to-tts-text (block)
  "Return notes BLOCK as (mostly) ASCII text:
- Remove bold, code, italics, underline, verbatim.
- Replace hyperlinks with their texts.
- Create one sentence per line."
  (cl-letf (((symbol-function 'org-ascii-bold)
             (lambda (_ contents _) (format "%s" contents)))
            ((symbol-function 'org-ascii-italic)
             (lambda (_ contents _) (format "%s" contents)))
            ((symbol-function 'org-ascii-underline)
             (lambda (_ contents _) (format "%s" contents)))
            ((symbol-function 'org-ascii-link)
             (lambda (_ contents _) (format "%s" contents))))
    (let* ((org-ascii-links-to-notes nil) ; Avoid footnotes section.
           (org-ascii-verbatim-format "%s") ; Avoid quotation marks for code.
           ;; We suppose that sentence-end-double-space is true and also end
           ;; sentences at colons:
           (sentence-end
            (concat
	     "\\("
             "[.?!…‽:][]\"'”’)}»›]*"
             "\\($\\|[ \u00a0]$\\|\t\\|[ \u00a0][ \u00a0]\\)"
             "\\|[" sentence-end-without-space "]+"
	     "\\)"
             "[ \u00a0\t\n]*"))
           (text (org-export-string-as
                  (org-element-interpret-data block) 'ascii t)))
      (when (string-match "[^.?!…‽:>]\n\n" text)
        ;; Negated set includes > to avoid warnings about SSML elements.
        (display-warning
         'org-export-re-reveal
         (concat "For TTS, full sentences should be used.  Following text contains at least one paragraph without end character:\n"
                 text)
         :warning))
      (org-re-reveal--sentences-in-lines text))))

(defun org-re-reveal--get-headline-number (headline pnumbers info)
  "Get number of HEADLINE based on PNUMBERS and INFO.
If users use UNNUMBERED, we guess here and warn.
This does not work for fragments!"
  (let ((numbers (org-export-get-headline-number headline info))
        (level (org-export-get-relative-level headline info)))
    (if numbers
        (let ((title-slide (plist-get info :reveal-title-slide)))
          (if (and title-slide
                   (or (not (stringp title-slide))
                       (< 0 (length title-slide))))
              numbers
            ;; Without title slide, reduce section number by 1
            ;; for 0-based indexing of reveal.js.
            (cons (- (car numbers) 1) (cdr numbers))))
      (if pnumbers
          (let ((result (if (= 1 level)
                            (list (+ 1 (car pnumbers)) 0)
                          (list (car pnumbers) (+ 1 (cadr pnumbers))))))
            (message-box
             "[org-re-reveal] No numbers for headline after %s found.  Do you use UNNUMBERED?  Guessed numbers: %s  (Numbers for subsequent slides on the same level of nesting and for fragments are likely wrong.)"
             pnumbers result)
            result)
        (error "[org-re-reveal] This should not happen.  Unable to guess number for headline: %s" headline)))))

(defun org-re-reveal--tts-audio-name (block info)
  "Create audio name for BLOCK with INFO."
  (let ((sec-num (plist-get info :section-numbers))
        (headline (org-export-get-parent-headline block))
        (audio-name
         (org-export-read-attribute :attr_reveal block :audio-name)))
    (when (and (not sec-num) (not audio-name))
      (org-re-reveal--abort-with-message-box "[org-re-reveal] You must use attribute :audio-name on TTS notes if you disable section numbers!"))
    (if sec-num
        (if headline
            (let* ((hnum (plist-get info :reveal-tts-hnum))
                   (vnum (plist-get info :reveal-tts-vnum))
                   (frag (plist-get info :reveal-tts-frag))
                   (split-p (plist-get info :reveal-tts-split-p))
                   (pnumbers (plist-get info :reveal-tts-prev-numbers))
                   (numbers (org-re-reveal--get-headline-number
                             headline pnumbers info)))
              (if (equal pnumbers numbers)
                  ;; Same numbers, so either split or new fragment on slide.
                  (if (not split-p)
                      ;; Increment fragment counter if no split.
                      (if frag
                          (plist-put info :reveal-tts-frag (+ 1 frag))
                        (plist-put info :reveal-tts-frag 0))
                    ;; Reset split-p and increment vertical index.
                    (plist-put info :reveal-tts-split-p nil)
                    (plist-put info :reveal-tts-vnum (+ 1 vnum)))
                ;; Different numbers, on new slide.
                (plist-put info :reveal-tts-prev-numbers numbers)
                (plist-put info :reveal-tts-frag -1)
                (plist-put info :reveal-tts-split-p nil)
                (if (eq hnum (car numbers))
                    ;; Same horizontal index.  Thus, increment vertical index.
                    (plist-put info :reveal-tts-vnum (+ 1 vnum))
                  ;; Reset vertical number to 0 for new section.
                  (plist-put info :reveal-tts-hnum (car numbers))
                  (plist-put info :reveal-tts-vnum 0)))
              (let ((prefix (plist-get info :reveal-tts-name-prefix))
                    (hnumstr (number-to-string (plist-get info :reveal-tts-hnum)))
                    (vnumstr (number-to-string (plist-get info :reveal-tts-vnum)))
                    (frag (plist-get info :reveal-tts-frag)))
                (or audio-name
                    (concat prefix hnumstr "." vnumstr
                            (when (and frag (< -1 frag))
                              (concat "." (number-to-string frag)))))))
          ;; No headline; thus, notes for title slide.
          (or audio-name
              (let ((prefix (plist-get info :reveal-tts-name-prefix)))
                (concat prefix "0.0"))))
      audio-name)))

(defun org-re-reveal--write-tts-files (block voice info &optional audio-name)
  "Write TTS files for notes BLOCK with VOICE and INFO.
Add a line to the index file, and create a text file for the notes.
Function `org-re-reveal--tts-audio-name' determines the name of the text
file unless optional AUDIO-NAME is present."
  (let* ((gap (plist-get info :reveal-tts-sentence-gap))
         (text (org-re-reveal--notes-to-tts-text block))
         (hash (md5 text))
         (tts-dir (org-re-reveal--tts-dir info))
         (audio-name
          (or audio-name (org-re-reveal--tts-audio-name block info))))
    (org-re-reveal--add-to-tts-index voice gap audio-name hash info)
    (org-re-reveal--create-tts-text hash text tts-dir)))

(defun org-re-reveal--notes-to-html (contents)
  "Transcode notes CONTENTS to HTML.
Create HTML notes according to `org-re-reveal-notes-format-string' and
strip potential SSML elements (added for TTS purposes but not meant for
the audience)."
  (if (stringp contents)
      (replace-regexp-in-string
       "&lt;break time=[^/]+/&gt;" ""
       (format org-re-reveal-notes-format-string contents))
    ""))

(defun org-re-reveal--notes-to-tts (block contents info)
  "Transcode a notes BLOCK from Org to Reveal.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.
If TTS is configured, also create text file and add it to index."
  (let ((voice (plist-get info :reveal-with-tts))
        (headline (org-export-get-parent-headline block)))
    (when voice
      (org-re-reveal--write-tts-files block voice info))
    (if headline
        (org-re-reveal--notes-to-html contents)
      ;; Notes for title slide.  Store for later retrieval.
      (plist-put info :title-notes (org-re-reveal--notes-to-html contents))
      "")))

(defun org-re-reveal-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to Reveal.
CONTENTS holds the contents of the block.  INFO is a plist
holding contextual information.

If the block type is \"NOTES\" (case-insensitive), transcode the block
into a Reveal.js slide note.  Otherwise, export the block as by the HTML
exporter."
  (let ((block-type (org-element-property :type special-block)))
    (if (string= (downcase block-type) "notes")
        (org-re-reveal--notes-to-tts special-block contents info)
      (org-html-special-block special-block contents info))))

(defun org-re-reveal--html-header-add-class (elem value)
  "Add VALUE as \"class\" attribute in HTML header element ELEM.
Do nothing if \"class\" attribute is already present."
  (let ((match (string-match "\\`<h[1-9][^>]+>" elem)))
    (unless match (error "[org-re-reveal] Element no headline: %s" elem))
    (let ((tag (match-string 0 elem)))
      (if (string-match "class" tag)
          elem
        (replace-regexp-in-string "\\`\\(<h[1-9][^>]+\\)>"
                                  (format "\\1 class=\"%s\">" value)
                                  elem)))))

(defun org-re-reveal--fix-html-headline (headline contents info)
  "Convert HEADLINE with CONTENTS and INFO to HTML.
Call `org-html-headline' to generate initial HTML, remove surrounding
\"div\" tags, and add class attribute to h-element if
\":HTML_HEADLINE_CLASS\" property is present.

Adding a class attribute in ox-reveal.el is a hack which is only
necessary until that functionality has arrived in ox-html.el:
https://lists.gnu.org/archive/html/emacs-orgmode/2018-12/msg00016.html
As that patch has been accepted, the property is called
\":HTML_HEADLINE_CLASS\".  Otherwise, \":REVEAL_HEADLINE_CLASS\" would
have been appropriate..."
  (let* ((class (org-element-property :HTML_HEADLINE_CLASS headline))
         (html (org-html-headline headline contents info))
         (nodiv
          (if (string-prefix-p "<div" html)
              ;; Remove the first <div> and the last </div> tags from html
              (concat "<"
                      (mapconcat 'identity
                                 (butlast (cdr (split-string html "<" t)))
                                 "<"))
            ;; Return the HTML content unchanged
            html)))
    (if class
        (org-re-reveal--html-header-add-class nodiv class)
      nodiv)))

(defun org-re-reveal--generate-data-uri (path)
  "Generate data URI for image at PATH.
Return PATH unchanged if it starts with \"data:image/\"."
  (if (string-prefix-p "data:image/" path)
      path
    (let ((ext (downcase (file-name-extension path)))
          (clean-path (org-re-reveal--file-url-to-path path)))
      (concat
       "data:image/"
       ;; Image type
       ext
       ";base64,"
       ;; Base64 content
       (with-temp-buffer
         ;; Use insert-file-contents-literally here as base64-encode-region
         ;; requires bytes, not text.
         (insert-file-contents-literally clean-path)
         (base64-encode-region 1 (point-max) 'no-line-break)
         (buffer-string))))))

(defun org-re-reveal--maybe-encode-with-data-uri (path info)
  "Encode image at PATH as data URI if INFO indicates single-file export."
  (when (< 0 (length path))
    (if (plist-get info :reveal-single-file)
        (if (org-re-reveal--remote-file-p path)
            (org-re-reveal--abort-with-message-box
             "Single file export requires local background image, not %s." path)
          (org-re-reveal--generate-data-uri path))
      path)))

(defun org-re-reveal--maybe-replace-background (attr info)
  "Maybe replace background image in ATTR based on INFO.
If ATTR specifies a background image, call
`org-re-reveal--maybe-encode-with-data-uri' to potentially generate a data
URI for single-file export."
  (if (and (< 0 (length attr))
           (string-match "^data-background\\(-image\\)?=" attr))
      (let* ((parts (split-string attr "[=\"]" t))
             (name (nth 0 parts))
             (path (nth 1 parts))
             (new-path (org-re-reveal--maybe-encode-with-data-uri path info)))
        (format "%s=\"%s\"" name new-path))
    attr))

(defun org-re-reveal--section-attrs (headline info)
  "Compute attributes for section element of HEADLINE with INFO.
Return empty string or one starting with a space character."
  (let* ((default-slide-background (plist-get info :reveal-default-slide-background))
         (default-slide-background-size (plist-get info :reveal-default-slide-background-size))
         (default-slide-background-position (plist-get info :reveal-default-slide-background-position))
         (default-slide-background-repeat (plist-get info :reveal-default-slide-background-repeat))
         (default-slide-background-transition (plist-get info :reveal-default-slide-background-transition))
         (default-slide-background-opacity (plist-get info :reveal-default-slide-background-opacity))
         (slide-background (org-export-get-node-property :REVEAL_BACKGROUND headline org-use-property-inheritance))
         (attrs (org-html--make-attribute-string
                 `(:data-transition ,(org-export-get-node-property :REVEAL_DATA_TRANSITION headline)
                                    :data-state ,(org-export-get-node-property :REVEAL_DATA_STATE headline)
                                    ;; Allow empty slide background to override default one.
                                    :data-background ,(if slide-background
                                                          (when (< 0 (length slide-background))
                                                            slide-background)
                                                        default-slide-background)
                                    :data-background-size ,(or (org-export-get-node-property :REVEAL_BACKGROUND_SIZE headline org-use-property-inheritance)
                                                               default-slide-background-size)
                                    :data-background-position ,(or (org-export-get-node-property :REVEAL_BACKGROUND_POSITION headline org-use-property-inheritance)
                                                                   default-slide-background-position)
                                    :data-background-repeat ,(or (org-export-get-node-property :REVEAL_BACKGROUND_REPEAT headline org-use-property-inheritance)
                                                                 default-slide-background-repeat)
                                    :data-background-opacity ,(or (org-export-get-node-property :REVEAL_BACKGROUND_OPACITY headline org-use-property-inheritance)
                                                                 default-slide-background-opacity)
                                    :data-background-transition ,(or (org-export-get-node-property :REVEAL_BACKGROUND_TRANS headline org-use-property-inheritance)
                                                                     default-slide-background-transition)))))
    (if (> (length attrs) 0) (format " %s" attrs) "")))

;; Copied from org-html-headline and modified to embed org-re-reveal
;; specific attributes.
(defun org-re-reveal-headline (headline contents info)
  "Transcode a HEADLINE element from Org to HTML.
CONTENTS holds the contents of the headline.  INFO is a plist
holding contextual information."
  (unless (org-element-property :footnote-section-p headline)
    (if (or (org-export-low-level-p headline info)
            (org-element-property :NOSLIDE headline))
        ;; This is a deep sub-tree or a subheading; do not create slide.
        (if (< 0 (length (plist-get info :reveal-slide-grid-div)))
            ;; For grid layouts, remove div element created by ox-html.
            (org-re-reveal--fix-html-headline headline contents info)
          ;; Just use ox-html.  This is kept here for backwards
          ;; compatibility.  Not sure whether anyone relies on the div.
          (org-html-headline headline contents info))
      ;; Standard headline.  Export it as a slide
      (let* ((level (org-export-get-relative-level headline info))
             (preferred-id (or (org-element-property :CUSTOM_ID headline)
                               (and (fboundp 'org-export-get-reference)
                                    (org-export-get-reference headline info))
                               (org-element-property :ID headline)))
             (hlevel (org-re-reveal--get-hlevel info))
             (header (plist-get info :reveal-slide-header))
             (header-div (org-re-reveal--if-format
                          org-re-reveal-slide-header-html header))
             (first-sibling (org-export-first-sibling-p headline info))
             (attrs (org-re-reveal--section-attrs headline info))
             (extra-attrs (org-re-reveal--maybe-replace-background
                           (or (org-element-property :REVEAL_EXTRA_ATTR headline)
                               (plist-get info :reveal-extra-attr))
                           info))
             (slide-section-tag
              (format "<section id=\"%s\"%s%s>\n"
                      (format "%s%s" org-re-reveal--slide-id-prefix preferred-id)
                      attrs
                      (org-re-reveal--if-format " %s" extra-attrs)))
             (slide-grid-div (plist-get info :reveal-slide-grid-div))
             (ret (concat
                   (if (or (/= level 1) (not first-sibling))
                       ;; Not the first heading. Close previous slide.
                       (concat
                        ;; Close previous slide.
                        "</section>\n"
                        (if (<= level hlevel)
                            ;; Close previous vertical slide group.
                            "</section>\n")))
                   (if (<= level hlevel)
                       ;; Add an extra "<section>" to group following slides
                       ;; into vertical slide group. Transition override
                       ;; attributes are attached at this level, too.
                       (let ((attrs
                              (org-html--make-attribute-string
                               `(:data-transition ,(org-element-property :REVEAL_DATA_TRANSITION headline)))))
                         (if (string= attrs "")
                             "<section>\n"
                           (format "<section %s>\n" attrs))))
                   ;; Start a new slide.
                   slide-section-tag
                   ;; Grid div if any.
                   slide-grid-div
                   ;; Slide header if any.
                   header-div
                   ;; The HTML content of the headline
                   (org-re-reveal--fix-html-headline headline contents info)
                   (when (and (= level 1)
                              (org-export-last-sibling-p headline info))
                     ;; Last head 1. Close all slides.
                     "</section>\n</section>\n"))))
        ret))))

(defun org-re-reveal--read-list (thing)
  "Return THING if it is a list.
Return nil if THING is the empty string.
Otherwise, `read' THING and return value if it is a list.
Otherwise, raise an error."
  (if (listp thing)
      thing
    (if (and (stringp thing) (= 0 (length thing)))
        nil
      (let ((lthing (read thing)))
        (if (listp lthing)
            lthing
          (error "[org-re-reveal] Expected a list, but got: %s" thing))))))

(defun org-re-reveal--parse-listoption (info option)
  "Parse and return OPTION in INFO.
That value for OPTION may be a list or a string representing a list."
  (org-re-reveal--read-list (plist-get info option)))

(defun org-re-reveal--read-file (file)
  "Return the content of FILE.

Note that this function uses `insert-file-contents-literally', which
may lead to encoding problems."
  (declare (obsolete 'org-re-reveal--read-file-as-string "3.18.1"))
  (with-temp-buffer
    (insert-file-contents-literally file)
    (buffer-string)))

(defun org-re-reveal--file-url-to-path (url)
  "Convert URL that points to local files to file path."
  (replace-regexp-in-string
   (if (string-equal system-type "windows-nt") "^file:///" "^file://")
   "" url))

(defun org-re-reveal--abort-with-message-box (msg &rest args)
  "Call `message' and `message-box' with MSG and ARGS, then raise error."
  (message msg args)
  (message-box msg args)
  (user-error "[org-re-reveal] Aborted"))

(defun org-re-reveal--css-label (in-single-file file-name style-id)
  "Generate HTML code to include CSS file FILE-NAME.
If IN-SINGLE-FILE is non-nil, embed contents of FILE-NAME with an
error if SINGLE-FILE is `must' and FILE-NAME is not a readable file;
otherwise, generate `<link>' label, with a non-nil STYLE-ID as
`id' attribute."
  (when (and file-name (not (string= file-name "")))
    (let ((local-file-name (org-re-reveal--file-url-to-path file-name)))
      (cond ((and in-single-file (file-readable-p local-file-name))
             (concat "<style type=\"text/css\">\n"
                     (org-re-reveal--read-file-as-string local-file-name t)
                     "\n</style>\n"))
            ((eq in-single-file 'must)
             (org-re-reveal--abort-with-message-box
              "CSS file not readable for single-file embedding: %s" file-name))
            (t (concat "<link rel=\"stylesheet\" href=\"" file-name "\""
                       (org-re-reveal--if-format " id=\"%s\"" style-id)
                       "/>\n"))))))

(defun org-re-reveal--klipsify-header (info)
  "Return code (CSS and JavaScript) to activate klipse when indicated by INFO."
  (if (plist-get info :reveal-klipsify-src)
      (concat (format "<link rel=\"stylesheet\" href=\"%s\"/>\n"
                      (plist-get info :reveal-klipse-css-url))
              org-re-reveal-klipse-extra-css
              (format "<script>
    window.klipse_settings = {
%s%s
    };\n"
                      (org-re-reveal--if-format
                       "%s,\n" (plist-get info :reveal-codemirror-config))
                      (mapconcat (lambda (elem)
                                   (format "        %s: '.%s'"
                                           (nth 1 elem) (nth 2 elem)))
                                 (org-re-reveal--parse-listoption
                                  info :reveal-klipse-setup)
                                 ",\n"))
              (org-re-reveal--if-format
               "    %s\n" (plist-get info :reveal-klipse-extra-config))
              "</script>\n")
    ""))

(defun org-re-reveal--klipsify-script (info)
  "Return script element for klipse when indicated by INFO."
  (if (plist-get info :reveal-klipsify-src)
      (format "<script src=\"%s\"></script>
<script>
/* Recompute layout upon changes by klipse.  Code fragment from
   asciidoctor-revealjs-klipse by Timothy Pratley under GPLv3:
   https://github.com/timothypratley/asciidoctor-revealjs-klipse/blob/master/docs/docinfo-footer.html */
Reveal.addEventListener( 'slidechanged', function( event ) {
    window.dispatchEvent( new Event('resize') );
} );
</script>\n" (plist-get info :reveal-klipse-js-url))
    ""))


(defun org-re-reveal--highlight-css-path (info)
  "Return location of CSS for highlight plugin with INFO."
  (let ((highlight-css (plist-get info :reveal-highlight-css))
        (version (org-re-reveal--guess-revealjs-version info)))
    (if (symbolp highlight-css)
        (concat "%r/" (format "%s/%s.css"
                              (if (version< version "4")
                                  "lib/css"
                                "plugin/highlight")
                              highlight-css))
      highlight-css)))

(defun org-re-reveal--theme-path (theme css-path)
  "Return the path to the themes CSS file.
If the theme already is a path (e.g. THEME is '/css/my-theme.css', detected by
checking for the suffix '.css') then return THEME, else locate THEME within
CSS-PATH for built in themes."
  (if (string-suffix-p ".css" theme t)
      theme
    (let* ((css-path (file-name-as-directory css-path))
           (theme-path (file-name-as-directory (concat css-path "theme")))
           (theme-css (concat theme-path theme ".css")))
      theme-css)))

(unless (fboundp 'mapcan)
  (defun mapcan (func sequence)
    "Apply FUNC to each element of SEQUENCE.
Concatenate the results by altering them (using `nconc')."
  (apply #'nconc (mapcar func sequence))))

(defun org-re-reveal-stylesheets (info)
  "Return HTML code for reveal stylesheets using INFO and `org-re-reveal-root'."
  (let* ((root-path (file-name-as-directory (plist-get info :reveal-root)))
         (reveal-version (org-re-reveal--guess-revealjs-version info))
         (css-path (file-name-as-directory
                    (concat root-path (plist-get info :reveal-css-path))))
         (reveal-css (concat css-path "reveal.css"))
         (theme (plist-get info :reveal-theme))
         (theme-css (org-re-reveal--theme-path theme css-path))
         (extra-css (plist-get info :reveal-extra-css))
         (enabled-plugins (org-re-reveal--enabled-plugins info))
         (plugin-css (mapcan
                      (lambda (plugin)
                        (nthcdr 3 (org-re-reveal--plugin-config plugin info)))
                      enabled-plugins))
         (in-single-file (if (plist-get info :reveal-single-file)
                             'must
                           (plist-get info :reveal-embed-local-resources))))
    (concat
     ;; Default embedded style sheets
     "<style type=\"text/css\">
.underline { text-decoration: underline; }
</style>
"
     ;; stylesheets
     (mapconcat (lambda (elem) (org-re-reveal--css-label
                                in-single-file (car elem) (cdr elem)))
                (append (list (cons reveal-css nil)
                              (cons theme-css "theme"))
                        (mapcar (lambda (path) (cons path nil))
                                (cl-delete-duplicates
                                 (split-string extra-css "\n" t)
                                 :test #'equal))
                        (mapcar (lambda (path)
                                  (cons (org-re-reveal--reveal-path
                                         path root-path)
                                        nil))
                                plugin-css))
                "\n")

     ;; Include CSS for highlight.js if necessary
     (if (org-re-reveal--using-highlight.js info)
         (format "<link rel=\"stylesheet\" href=\"%s\"/>\n"
                 (format-spec (org-re-reveal--highlight-css-path info)
                              `((?r . ,(directory-file-name root-path)))))
       "")

     ;; Include CSS for klipse if necessary
     (org-re-reveal--klipsify-header info)

     ;; print-pdf
     (if (or in-single-file
             (version< "3.9" reveal-version))
         ""
       (format "\n<!-- If the query includes 'print-pdf', include the PDF print sheet -->
<script>
    if( window.location.search.match( /print-pdf/gi ) ) {
        var link = document.createElement( 'link' );
        link.rel = 'stylesheet';
        link.type = 'text/css';
        link.href = '%scss/print/pdf.css';
        document.getElementsByTagName( 'head' )[0].appendChild( link );
    }
</script>
"
               root-path)))))

(defun org-re-reveal-mathjax-scripts (info)
  "Return HTML code for declaring MathJax scripts for INFO.
Only do this if MathJax is enabled and `org-re-reveal-mathjax-url' is
not the empty string."
  (if (and (plist-get info :reveal-mathjax)
           (< 0 (length (plist-get info :reveal-mathjax-url))))
      (format "<script type=\"text/javascript\" src=\"%s\"></script>\n"
              (plist-get info :reveal-mathjax-url))))

(defun org-re-reveal--read-file-as-string (filename &optional raise-error)
  "If FILENAME exists as file, return its contents as string.
Otherwise, return nil unless FILENAME and RAISE-ERROR are non-nil, which
raises an error.

Note that this function does not use `insert-file-contents-literally'
any longer."
  (if (and (stringp filename)
           (file-readable-p filename)
           (not (file-directory-p filename)))
      (with-temp-buffer
        ;; With Emacs 30.0.50, Org mode cannot deal with undecoded file
        ;; contents any more, see:
        ;; https://lists.gnu.org/archive/html/emacs-orgmode/2023-02/msg00501.html
        ;; Thus, do not use `insert-file-contents-literally' any more.
        ;; Instead, use its code to inhibit further processing and call
        ;; `insert-file-contents'.
        (let ((format-alist nil)
	      (after-insert-file-functions nil)
              (inhibit-file-name-handlers
               '(jka-compr-handler image-file-handler epa-file-handler)))
          (insert-file-contents filename)
          (buffer-string)))
    (when (and filename raise-error)
      (user-error "[org-re-reveal] File not found --read-file-as-string: %s" filename))))

(defun org-re-reveal--external-plugins-maybe-from-file (info)
  "Create list of plugin dependencies from INFO.
In INFO, `:reveal-external-plugins' can be a list or a filename.
If it is a filename, split lines to produce a list."
  (let* ((external-plugins (plist-get info :reveal-external-plugins))
         (file-contents
          (org-re-reveal--read-file-as-string external-plugins)))
    (if file-contents
        (mapcar (lambda (line) (cons 'dummy line))
                (split-string (string-trim file-contents) "\n"))
      (org-re-reveal--read-list external-plugins))))

(defun org-re-reveal--external-plugin-init (info root-path)
  "Build initialization strings for plugins of INFO under ROOT-PATH.
Parameter INFO determines plugins and their initializations
based on `org-re-reveal-external-plugins'."
  (let ((plugins (org-re-reveal--external-plugins-maybe-from-file info)))
    (cl-loop for (nil . value) in plugins
             collect (format value root-path))))

(defvar org-re-reveal-client-multiplex nil
  "Used to cause generation of client html file for multiplex.")

(defun org-re-reveal--add-plugins (info)
  "Retrieve configuration for plugins with reveal.js 4 and later with INFO.
Parse keywords \"REVEAL_ADD_PLUGIN\" and return list of items."
  (let ((additional (split-string
                     (or (plist-get info :reveal-add-plugin) "") "\n" t " ")))
    (mapcar (lambda (line)
              (split-string line " " t " "))
            additional)))

(defun org-re-reveal--plugin-config (plugin info)
  "Retrieve configuration for PLUGIN with reveal.js 4 and later with INFO.
This retrieves the configuration for PLUGIN from `org-re-reveal-plugin-config'
or after keyword \"REVEAL_ADD_PLUGIN\"."
  (assoc plugin (append org-re-reveal-plugin-config (org-re-reveal--add-plugins info))))

(defun org-re-reveal--enabled-plugins (info)
  "Return enabled plugins based on INFO.
Plugins can be enabled
- with keyword \"REVEAL_PLUGINS\" (or variable `org-re-reveal-plugins') and
- with keyword \"REVEAL_ADD_PLUGIN\" (only reveal.js version 4 and later).
For reveal.js before version 4.0, no plugin is enabled with single file
export."
  (let ((in-single-file (plist-get info :reveal-single-file))
        (reveal-version (org-re-reveal--guess-revealjs-version info)))
    (when (or (not in-single-file)
              (version< "3.9" reveal-version))
      (append
       (org-re-reveal--parse-listoption info :reveal-plugins)
       (mapcar (lambda (config) (nth 0 config))
               (org-re-reveal--add-plugins info))))))

(defun org-re-reveal--reveal-path (path root-path)
  "Return location of PATH given ROOT-PATH.
If PATH is a remote URL, return it unchanged.
Otherwise, concatenate ROOT-PATH and PATH."
  (if (org-re-reveal--remote-file-p path)
      path
    (concat root-path path)))

(defun org-re-reveal--plugin-path (plugin root-path info)
  "Return location of PLUGIN given ROOT-PATH and INFO.
If path is a remote URL, return it unchanged.
Otherwise, concatenate ROOT-PATH with path of PLUGIN configuration in INFO."
  (let ((path (nth 2 (org-re-reveal--plugin-config plugin info))))
    (org-re-reveal--reveal-path path root-path)))

(defun org-re-reveal-scripts--libraries (info)
  "Internal function to generate script tags with INFO.
This includes reveal.js libraries in `:reveal-script-files' under
`org-re-reveal-root', and libraries in `org-re-reveal-extra-scripts'."
  (let* ((root-path (file-name-as-directory (plist-get info :reveal-root)))
         (script-files (org-re-reveal--parse-listoption
                        info :reveal-script-files))
         (root-libs (mapcar (lambda (file) (concat root-path file))
                            script-files))
         (reveal-version (org-re-reveal--guess-revealjs-version info))
         (in-single-file (plist-get info :reveal-single-file))
         (embed-local-resources (plist-get info :reveal-embed-local-resources))
         ;; Plugin config for reveal.js 4.x
         (enabled-plugins
          (when (version< "3.9" reveal-version)
            ;; Multiplex is no builtin in 4.x.
            (cl-remove 'multiplex (org-re-reveal--enabled-plugins info))))
         (plugin-libs
          (mapcar
           (lambda (plugin)
             (org-re-reveal--plugin-path plugin root-path info))
           enabled-plugins))
         (extra-scripts (org-re-reveal--parse-listoption
                         info :reveal-extra-scripts))
         ;; Treat extra scripts not starting with <script> as filenames.
         (extra-script-files
          (cl-remove-if (lambda (s) (string-prefix-p "<script>" s))
                        extra-scripts))
         ;; Treat extra scripts starting with <script> as elements.
         (extra-script-elements
          (cl-remove-if-not (lambda (s) (string-prefix-p "<script>" s))
                            extra-scripts)))
    (concat
     (let* ((local-root-path (org-re-reveal--file-url-to-path root-path))
            (local-libs (append (mapcar (lambda (file)
                                          (concat local-root-path file))
                                        script-files)
                                (mapcar (lambda (file)
					  (org-re-reveal--file-url-to-path file))
					plugin-libs)
                                extra-script-files))
            (local-libs-exist-p (cl-every #'file-readable-p local-libs)))
       (if (and in-single-file (not local-libs-exist-p))
           (org-re-reveal--abort-with-message-box
            "Subsequently listed libraries not readable for single-file embedding.  %s"
            (mapconcat 'identity
                       (cl-remove-if #'file-readable-p local-libs)
                       ", "))
         (if (or in-single-file embed-local-resources)
             ;; Embed contents or link to files.
             (mapconcat (lambda (file)
                          (if (file-readable-p file)
                              (format "<script>\n%s\n</script>\n"
                                      (org-re-reveal--read-file-as-string
                                       file t))
                            (concat "<script src=\"" file "\"></script>\n")))
                        local-libs "")
           ;; Embed script files with src.
           (mapconcat (lambda (file)
                        (concat "<script src=\"" file "\"></script>\n"))
                      (append root-libs plugin-libs extra-script-files) ""))))
     ;; Embed script tags.
     (mapconcat 'identity extra-script-elements "\n")
     (if extra-script-elements "\n" ""))))

(defun org-re-reveal-scripts--reveal-options (info)
  "Internal function for `org-re-reveal-scripts' with INFO."
  (format "
controls: %s,
progress: %s,
history: %s,
center: %s,
slideNumber: %s,
rollingLinks: %s,
keyboard: %s,
mouseWheel: %s,
fragmentInURL: %s,
hashOneBasedIndex: %s,
pdfSeparateFragments: %s,
%s%soverview: %s,
"
          (if (plist-get info :reveal-control) "true" "false")
          (if (plist-get info :reveal-progress) "true" "false")
          (if (plist-get info :reveal-history) "true" "false")
          (if (plist-get info :reveal-center) "true" "false")
          (let ((slide-number (plist-get info :reveal-slide-number)))
            (if slide-number (format "'%s'" slide-number)
              "false"))
          (if (plist-get info :reveal-rolling-links) "true" "false")
          (if (plist-get info :reveal-keyboard) "true" "false")
          (if (plist-get info :reveal-mousewheel) "true" "false")
          (if (plist-get info :reveal-fragmentinurl) "true" "false")
          (if (plist-get info :reveal-hashonebasedindex) "true" "false")
          (if (plist-get info :reveal-pdfseparatefragments) "true" "false")
          (org-re-reveal--if-format "defaultTiming: %s,\n"
                                    (plist-get info :reveal-defaulttiming))
          (org-re-reveal--if-format "totalTime: %s,\n"
                                    (plist-get info :reveal-totaltime))
          (if (plist-get info :reveal-overview) "true" "false")))

(defun org-re-reveal--to-string (option)
  "Return OPTION as string.
If OPTION is an integer > 0, return as string.
If OPTION is a string, embed in quotation marks.
If OPTION is nil, return nil (not the empty string).
Otherwise, raise error."
  (cond ((and (integerp option) (> option 0)) (format "%d" option))
        ((stringp option) (format "\"%s\"" option))
        ((eq option nil) nil)
        (t (user-error "[org-re-reveal] Option »%s« must be string, positive integer, or nil; not %s"
                  option (type-of option)))))

(defun org-re-reveal-scripts--main-configures (info)
  "Internal function for `org-re-reveal-scripts' with INFO."
  (concat
   ;; slide width
   (let ((width (plist-get info :reveal-width)))
     (org-re-reveal--if-format "width: %s,\n"
                               (org-re-reveal--to-string width)))

   ;; slide height
   (let ((height (plist-get info :reveal-height)))
     (org-re-reveal--if-format "height: %s,\n"
                               (org-re-reveal--to-string height)))

   ;; slide margin
   (let ((margin (string-to-number (plist-get info :reveal-margin))))
     (if (>= margin 0) (format "margin: %.2f,\n" margin) ""))

   ;; slide minimum scaling factor
   (let ((min-scale (string-to-number (plist-get info :reveal-min-scale))))
     (if (> min-scale 0) (format "minScale: %.2f,\n" min-scale) ""))

   ;; slide maximux scaling factor
   (let ((max-scale (string-to-number (plist-get info :reveal-max-scale))))
     (if (> max-scale 0) (format "maxScale: %.2f,\n" max-scale) ""))

   ;; themes and transitions
   (let ((reveal-version (org-re-reveal--guess-revealjs-version info)))
     (format (if (version< reveal-version "4")
                 "
theme: Reveal.getQueryHash().theme, // available themes are in /css/theme
transition: Reveal.getQueryHash().transition || '%s', // see README of reveal.js for options
transitionSpeed: '%s',\n"
               "
transition: '%s',
transitionSpeed: '%s',\n")
           (plist-get info :reveal-trans)
           (plist-get info :reveal-speed)))

   ;; notes in PDF export and in presentation
   (let ((export-notes (plist-get info :reveal-export-notes-to-pdf))
         (show-notes (plist-get info :reveal-show-notes)))
    (if export-notes
        (format "showNotes: window.location.search.match( /print-pdf/gi ) ? %s : %s,\n"
                (if (booleanp export-notes)
                    "true"
                  (format "'%s'" export-notes))
                (if show-notes
                    "true"
                  "false"))
      (when show-notes
        "showNotes: true,\n")))

   ;; extra options
   (let ((options (plist-get info :reveal-extra-options)))
     (org-re-reveal--if-format "%s,\n" options))))

(defun org-re-reveal-scripts--multiplex (info)
  "Internal function for `org-re-reveal-scripts' with INFO."
  (let ((enabled-plugins (org-re-reveal--enabled-plugins info)))
    (when (memq 'multiplex enabled-plugins)
      (format
       "multiplex: {
    secret: %s, // null if client
    id: '%s', // id, obtained from socket.io server
    url: '%s' // Location of socket.io server
},\n"
       (if (eq org-re-reveal-client-multiplex nil)
           (format "'%s'" (plist-get info :reveal-multiplex-secret))
         (format "null"))
       (plist-get info :reveal-multiplex-id)
       (plist-get info :reveal-multiplex-url)))))

(defun org-re-reveal-scripts--dependencies (info)
  "Internal function for `org-re-reveal-scripts' with INFO."
  (let* ((root-path (file-name-as-directory (plist-get info :reveal-root)))
         (in-single-file (plist-get info :reveal-single-file))
         (reveal-version (org-re-reveal--guess-revealjs-version info))
         (enabled-plugins (org-re-reveal--enabled-plugins info)))
    ;; optional JS library heading
    (if (and (version< reveal-version "4") in-single-file) ""
      (concat
       (if (version< reveal-version "4")
           ""
         (format "\n// Plugins with reveal.js 4.x
plugins: [ %s ],\n"
                 (mapconcat
                  (lambda (plugin)
                    (nth 1 (org-re-reveal--plugin-config plugin info)))
                  enabled-plugins
                  ", ")))
       "\n// Optional libraries used to extend reveal.js
dependencies: [\n"
       ;; JS libraries
       (let* ((highlight-url (plist-get info :reveal-highlight-url))
              (builtin-multiplex (format " { src: '%s', async: true },\n%s"
                                         (plist-get info :reveal-multiplex-socketio-url)
                                         ;; following ensures that either client.js or master.js is included depending on defvar org-re-reveal-client-multiplex value state
                                         (if (not org-re-reveal-client-multiplex)
                                             (progn
                                               (if (not (string= "" (plist-get info :reveal-multiplex-secret)))
                                                   (setq org-re-reveal-client-multiplex t))
                                               (format " { src: '%splugin/multiplex/master.js', async: true }" root-path))
                                           (format " { src: '%splugin/multiplex/client.js', async: true }" root-path))))
              (builtins-v3
               `(classList ,(format " { src: '%slib/js/classList.js', condition: function() { return !document.body.classList; } }" root-path)
                           markdown ,(format " { src: '%splugin/markdown/marked.js', condition: function() { return !!document.querySelector( '[data-markdown]' ); } },
 { src: '%splugin/markdown/markdown.js', condition: function() { return !!document.querySelector( '[data-markdown]' ); } }" root-path root-path)
                           highlight ,(if highlight-url
                                          (format " { src: '%s', async: true, callback: function() { hljs.initHighlightingOnLoad(); } }" highlight-url)
                                        (format " { src: '%splugin/highlight/highlight.js', async: true, callback: function() { hljs.initHighlightingOnLoad(); } }" root-path))
                           zoom ,(format " { src: '%splugin/zoom-js/zoom.js', async: true, condition: function() { return !!document.body.classList; } }" root-path)
                           notes ,(format " { src: '%splugin/notes/notes.js', async: true, condition: function() { return !!document.body.classList; } }" root-path)
                           search ,(format " { src: '%splugin/search/search.js', async: true, condition: function() { return !!document.body.classList; } }" root-path)
                           remotes ,(format " { src: '%splugin/remotes/remotes.js', async: true, condition: function() { return !!document.body.classList; } }" root-path)
                           multiplex ,builtin-multiplex))
              (builtin-codes
               (if (version< reveal-version "4")
                   (mapcar (lambda (p) (plist-get builtins-v3 p))
                           enabled-plugins)
                 ;; Multiplex plugin is a dependency with version 4.x
                 (when (memq 'multiplex enabled-plugins)
                   (list builtin-multiplex))))
              (external-plugins
               (org-re-reveal--external-plugin-init info root-path))
              (all-plugins
               (if external-plugins
                   (append external-plugins builtin-codes)
                 builtin-codes)))
         (mapconcat 'identity all-plugins ",\n"))
       "]\n\n"))))

(defun org-re-reveal-scripts--init-script (info)
  "Internal function for `org-re-reveal-scripts' with INFO."
  (let ((init-script (plist-get info :reveal-init-script))
        (in-single-file (plist-get info :reveal-single-file)))
    (if (and (stringp init-script) (> (length init-script) 0))
        (concat (if in-single-file "" ",") init-script)
      "")))

(defun org-re-reveal-scripts (info)
  "Return necessary scripts to initialize reveal.js.
Use INFO and custom variable `org-re-reveal-root'."
  (concat
   ;; Libraries in script tags, including reveal.js itself.
   (org-re-reveal-scripts--libraries info)

   ;; Create <script> tag for Reveal.initialize(...).
   "<script>
// Full list of configuration options available here:
// https://github.com/hakimel/reveal.js#configuration
Reveal.initialize({
"
   ;; plugin configures/frags
   (org-re-reveal-scripts--reveal-options info)

   ;; reveal.js main configures
   (org-re-reveal-scripts--main-configures info)

   ;; multiplexing - depends on defvar 'org-re-reveal-client-multiplex'
   (org-re-reveal-scripts--multiplex info)

   ;; load dependency js
   (org-re-reveal-scripts--dependencies info)

   ;; init-script
   (org-re-reveal-scripts--init-script info)

   ;; end of <script> tag
   "});\n</script>\n"))

(defun org-re-reveal--footer (info &optional object check-parent)
  "Return footer given INFO.
If optional OBJECT is non-nil use it to try to obtain a footer property
from its parent headline.  If optional CHECK-PARENT is non-nil, only
return a footer if OBJECT has a parent headline."
  (let* ((parent (when object (org-export-get-parent-headline object)))
	 (footer (or (and parent
                          (org-element-property :REVEAL_SLIDE_FOOTER parent))
		     (plist-get info :reveal-slide-footer))))
    (if (and (or (not check-parent) parent)
             footer)
        (format org-re-reveal-slide-footer-html footer)
      "")))

(defun org-re-reveal--slide-common-attrs (type info)
  "Return string for attributes of slide with TYPE from INFO.
TYPE specifies \"toc\" or \"title\"."
  (let ((background
         (plist-get info (intern
                          (concat ":reveal-" type
                                  "-slide-background"))))
        (background-size
         (plist-get info (intern
                          (concat ":reveal-" type
                                  "-slide-background-size"))))
        (background-position
         (plist-get info (intern
                          (concat ":reveal-" type
                                  "-slide-background-position"))))
        (background-repeat
         (plist-get info (intern
                          (concat ":reveal-" type
                                  "-slide-background-repeat"))))
        (background-opacity
         (plist-get info (intern
                          (concat ":reveal-" type
                                  "-slide-background-opacity"))))
        (background-transition
         (plist-get info (intern
                          (concat ":reveal-" type
                                  "-slide-background-transition"))))
        (extra-attr
         (org-re-reveal--maybe-replace-background
          (plist-get info (intern
                           (concat ":reveal-" type "-slide-extra-attr")))
          info))
        (state
         (plist-get info (intern
                          (concat ":reveal-" type "-slide-state"))))
        (timing
         (plist-get info (intern
                          (concat ":reveal-" type "-slide-timing")))))
    (concat
     (when (< 0 (length background))
       (concat " data-background=\"" background "\""))
     (when (< 0 (length background-size))
       (concat " data-background-size=\"" background-size "\""))
     (when (< 0 (length background-position))
       (concat " data-background-position=\"" background-position "\""))
     (when (< 0 (length background-repeat))
       (concat " data-background-repeat=\"" background-repeat "\""))
     (when (< 0 (length background-opacity))
       (concat " data-background-opacity=\"" background-opacity "\""))
     (when (< 0 (length background-transition))
       (concat " data-background-transition=\"" background-transition "\""))
     (when (< 0 (length extra-attr))
       (concat " " (org-re-reveal--maybe-replace-background
                    extra-attr info)))
     (when (< 0 (length state)) (concat " data-state=\"" state "\""))
     (when (< 0 (length timing)) (concat " data-timing=\"" timing "\"")))))

(defun org-re-reveal--wrap-div-grid (contents info)
  "Wrap CONTENTS depending on INFO.
Check `org-re-reveal-slide-grid-div'."
  (let ((slide-grid-div (plist-get info :reveal-slide-grid-div)))
    (concat slide-grid-div
            contents
            (when (< 0 (length slide-grid-div))
              "</div>\n"))))

(defun org-re-reveal-toc (depth info)
  "Build a slide of table of contents with DEPTH and INFO."
  (let ((toc (org-html-toc depth info)))
    (org-re-reveal-toc-1 toc info)))

(defun org-re-reveal-toc-1 (toc info)
  "Build table of contents with TOC and INFO."
  (when toc
    (let* ((toc-slide-with-header (plist-get info :reveal-slide-global-header))
           (toc-slide-with-footer (or
                                   (plist-get info :reveal-slide-global-footer)
                                   (plist-get info :reveal-slide-toc-footer)))
           (toc-slide-attrs (org-re-reveal--slide-common-attrs "toc" info))
           (toc-slide-class (plist-get info :reveal-toc-slide-class))
           (toc-slide-title (plist-get info :reveal-toc-slide-title))
           (toc (replace-regexp-in-string
                 "<a href=\"#"
                 (concat "<a href=\"#" org-re-reveal--href-fragment-prefix) toc))
           (toc (if (< 0 (length toc-slide-title))
                    (replace-regexp-in-string
                     (org-html--translate "Table of Contents" info)
                     toc-slide-title toc)
                  toc)))
      (concat "<section id=\"table-of-contents-section\""
              (or toc-slide-attrs "")
              ">\n"
              (org-re-reveal--wrap-div-grid
               (concat
                (when toc-slide-with-header
                  (let ((header (plist-get info :reveal-slide-header)))
                    (when header (format org-re-reveal-slide-header-html header))))
                (if toc-slide-class
                    (replace-regexp-in-string
                     "<h\\([1-3]\\)>"
                     (format "<h\\1 class=\"%s\">" toc-slide-class)
                     toc)
                  toc)
                (when toc-slide-with-footer
                  (org-re-reveal--footer info)))
               info)
              "</section>\n"))))

(defun org-re-reveal-inner-template (contents info)
  "Return body of document string after HTML conversion.
CONTENTS is the transcoded contents string.  INFO is a plist
holding export options."
  (concat
   ;; Table of contents.
   (let ((depth (plist-get info :with-toc)))
     (when (and depth
                (not (plist-get info :reveal-subtree)))
       (org-re-reveal-toc depth info)))
   ;; Document contents.
   contents))

(defun org-re-reveal-parse-keyword-value (value footer keyword info)
  "According to VALUE of KEYWORD and INFO, return HTML tags to split slides.
Currently, only the keyword \"split\" is implemented, and VALUE must
start with \"split\".  Any following text is inserted literally into
the section tag.
The possibly empty FOOTER is inserted at the end of the slide."
  (cl-assert (string-prefix-p "split" value) nil
             (format "Unknown REVEAL keyword.  Expected \"split\", got: %s"
                     value))
  (let* ((headline (org-export-get-parent-headline keyword))
         (split-attrs (substring value 5)) ; Everything after "split"
         (real-attrs (if (< 0 (length split-attrs))
                         split-attrs
                       (org-re-reveal--section-attrs headline info))))
    (when (plist-get info :reveal-with-tts)
      ;; A split acts like a new slide.
      ;; Thus, remember split and reset fragment counter for TTS.
      (plist-put info :reveal-tts-split-p t)
      (plist-put info :reveal-tts-frag -1))
    (format "%s</section>\n<section%s>" footer real-attrs)))

;; Copied from org-html-format-list-item. Overwrite HTML class
;; attribute when there is attr_html attributes.
(defun org-re-reveal-format-list-item (contents type checkbox attributes info
                                                &optional term-counter-id
                                                headline)
  "Format a list item into HTML based on INFO.
Item has CONTENTS, TYPE, may be a CHECKBOX, have ATTRIBUTES, and may have
TERM-COUNTER-ID and HEADLINE."
  (let ((attr-html (cond (attributes (format " %s" (org-html--make-attribute-string attributes)))
                         (checkbox (format " class=\"%s\"" (symbol-name checkbox)))
                         (t "")))
        (checkbox (concat (org-html-checkbox checkbox info)
                          (and checkbox " ")))
        (br (org-html-close-tag "br" nil info)))
    (concat
     (cl-case type
       (ordered
        (let* ((counter term-counter-id)
               (extra (if counter (format " value=\"%s\"" counter) "")))
          (concat
           (format "<li%s%s>" attr-html extra)
           (when headline (concat headline br)))))
       (unordered
        (let* ((id term-counter-id)
               (extra (if id (format " id=\"%s\"" id) "")))
          (concat
           (format "<li%s%s>" attr-html extra)
           (when headline (concat headline br)))))
       (descriptive
        (let* ((term term-counter-id))
          (setq term (or term "(no term)"))
          ;; Check-boxes in descriptive lists are associated to tag.
          (concat (format "<dt%s>%s</dt>"
                          attr-html (concat checkbox term))
                  (format "<dd%s>" attr-html)))))
     (unless (eq type 'descriptive) checkbox)
     (and contents (org-trim contents))
     (cl-case type
       (ordered "</li>")
       (unordered "</li>")
       (descriptive "</dd>")))))

;; Copied from org-html-item, changed to call
;; org-re-reveal-format-list-item.
(defun org-re-reveal-item (item contents info)
  "Transcode an ITEM element from Org to Reveal.
CONTENTS holds the contents of the item.  INFO is a plist holding
contextual information."
  (let* ((plain-list (org-export-get-parent item))
         (type (org-element-property :type plain-list))
         (counter (org-element-property :counter item))
         (attributes (org-export-read-attribute :attr_html item))
         (checkbox (org-element-property :checkbox item))
         (tag (let ((tag (org-element-property :tag item)))
                (and tag (org-export-data tag info)))))
    (org-re-reveal-format-list-item
     contents type checkbox attributes info (or tag counter))))

(defun org-re-reveal-keyword (keyword contents info)
  "Transcode a KEYWORD element from Org to Reveal.
May change custom variables as SIDE EFFECT.
CONTENTS is nil.  INFO is a plist holding contextual information."
  (let* ((key (org-element-property :key keyword))
         (value (org-element-property :value keyword))
         (footer-div (org-re-reveal--footer info keyword)))
    (cl-case (intern key)
      (REVEAL (org-re-reveal-parse-keyword-value value footer-div keyword info))
      (REVEAL_HTML value)
      (HTML value)
      ;; Handling of TOC at arbitrary position is a hack.
      ;; We end the previous section by inserting a closing section tag,
      ;; which will *break* the presentation if other tags are still open.
      ;; To avoid unbalanced tags, remove the TOC's closing tag.
      ;; If slide footers are used, insert it before closing the section.
      ;; In any case, if footers are used, the one of the closed section
      ;; is sufficient, and the one contained in the TOC needs to be removed.
      (TOC (message "Please use #+REVEAL_TOC instead of #+TOC.  See Readme.")
           (sit-for 2)
           (concat footer-div
                   "</section>\n"
                   (replace-regexp-in-string
                    (format "</section>\\|%s"
                            (format org-re-reveal-slide-footer-html ".*"))
                    ""
                    (org-re-reveal-toc-1
                     (org-html-keyword keyword contents info) info))))
      (REVEAL_TOC
       ;; Following code stiched together with snippets from
       ;; org-html-keyword and org-html-toc.
       (when (string-match "\\<headlines\\>" value)
	 (let* ((depth (and (string-match "\\<[0-9]+\\>" value)
			    (string-to-number (match-string 0 value))))
	        (toc-entries
	         (mapcar (lambda (headline)
		           (cons (org-html--format-toc-headline headline info)
			         (org-export-get-relative-level headline info)))
		         (org-export-collect-headlines info depth))))
           (when toc-entries
             (let ((toc (concat "<div id=\"text-table-of-contents\" role=\"doc-toc\">"
			        (org-html--toc-text toc-entries)
			        "</div>\n")))
               ;; Use link format of reveal.js.
	       (replace-regexp-in-string
                "<a href=\"#"
                (concat "<a href=\"#" org-re-reveal--href-fragment-prefix)
                toc)))))))))

(defun org-re-reveal-embedded-svg (path)
  "Embed the SVG content at PATH into Reveal HTML."
  (with-temp-buffer
    (insert-file-contents path)
    (let ((start (re-search-forward "<[ \t\n]*svg[ \t\n]"))
          (end (re-search-forward "<[ \t\n]*/svg[ \t\n]*>")))
      (concat "<svg " (buffer-substring-no-properties start end)))))

(defun org-re-reveal--format-image-data-uri (link path info)
  "Generate HTML code for embedded image referenced by LINK at PATH with INFO.
For an svg image, return its svg element.  For other images, return an
img element that embeds the image as data URI."
  (let ((ext (downcase (file-name-extension path))))
    (if (string= ext "svg")
        (org-re-reveal-embedded-svg path)
      (org-html-close-tag
       "img"
       (org-html--make-attribute-string
        (org-combine-plists
         (list :src (org-re-reveal--generate-data-uri path))
         ;; Get attribute list from parent element
         ;; Copied from ox-html.el
         (let* ((parent (org-export-get-parent-element link))
                (link (let ((container (org-export-get-parent link)))
                        (if (and (eq (org-element-type container) 'link)
                                 (org-html-inline-image-p link info))
                            container
                          link))))
           (and (eq (org-element-map parent 'link 'identity info t) link)
                (org-export-read-attribute :attr_html parent)))))
       info))))

(defun org-re-reveal--maybe-replace-in-link (link allow-inter-link)
  "Replace hash sign in LINK, affected by ALLOW-INTER-LINK.

If ALLOW-INTER-LINK is nil, only replace hash signs if URL in LINK starts
with it.  Otherwise, also replace if the URL does not contain a hostname;
such links are assumed to point into other presentations."
  (if (and allow-inter-link
           (string-match "<a href=\"\\([^\"]*\\)\"" link))
      (let* ((url (match-string 1 link))
             (obj (url-generic-parse-url url))
             (host (url-host obj)))
        (if host
            link
          (replace-regexp-in-string
           "<a href=\"\\([^#]*\\)#"
           (concat "<a href=\"\\1#" org-re-reveal--href-fragment-prefix)
           link)))
    (replace-regexp-in-string
     "<a href=\"#"
     (concat "<a href=\"#" org-re-reveal--href-fragment-prefix)
     link)))

(defun org-re-reveal--add-class (elem class)
  "Add CLASS to the class attribute of ELEM.
Merge CLASS with any previous classes in the :attr_html :class attribute"
  (let* ((attrs (org-export-read-attribute :attr_html elem))
         (oldclass (plist-get attrs :class))
         (newclass (if oldclass (concat class " " oldclass) class))
         (newattrs (mapconcat (lambda (elem) (format "%s" elem))
                              (plist-put attrs :class newclass)
                              " ")))
    (org-element-put-property elem :attr_html (list newattrs))))

(defun org-re-reveal--internal-link-class (link info)
  "Check if LINK is internal, given INFO, and maybe assign class.
The direction of the link is assigned as class attribute to the link
and to its parent via \"attr_html\":
If link points backward (to previous content), class \"backwardlink\"
is assigned, else \"forwardlink\".
Assigning the class to \"attr_html\" of parent is based on a hack in
`org-html-link', while use of \"attr_html\" of the link itself
requires a version of org-mode as of 2018-12-08 or newer."
  (let ((target (or (ignore-errors (org-export-resolve-id-link link info))
                    (ignore-errors (org-export-resolve-fuzzy-link link info)))))
    (when target
      (let* ((lbegin (org-element-property :begin link))
             (tbegin (org-element-property :begin target))
             (direction (if (< tbegin lbegin)
                            "backwardlink"
                          "forwardlink"))
             (parent (org-export-get-parent-element link)))
        (org-re-reveal--add-class parent direction)
        (org-re-reveal--add-class link direction)))))

(defun org-re-reveal-link (link desc info)
  "Transcode a LINK object with DESC and INFO from Org to Reveal.
The result is identical to ox-html except for image links.
When `org-re-reveal-single-file' is t,
the result is the Data URI of the referenced image."
  (let* ((must-embed-image (and (plist-get info :reveal-single-file)
                                (org-export-inline-image-p
                                 link (plist-get info :html-inline-image-rules))))
         (want-embed-image (and (or must-embed-image
                                    (plist-get info :reveal-embed-local-resources))
                                (plist-get info :html-inline-images)
                                (string= "file" (org-element-property :type link))
                                (org-export-inline-image-p
                                 link (plist-get info :html-inline-image-rules))))
         (allow-inter-link (plist-get info :reveal-inter-presentation-links))
         (raw-path (org-element-property :path link))
         (clean-path (org-re-reveal--file-url-to-path raw-path))
         (can-embed-image (and want-embed-image
                               (file-readable-p clean-path))))
    (if can-embed-image
        (org-re-reveal--format-image-data-uri link clean-path info)
      (if must-embed-image
          (org-re-reveal--abort-with-message-box
           "Image not readable for single-file embedding: %s" raw-path)
        (org-re-reveal--internal-link-class link info)
        (org-re-reveal--maybe-replace-in-link (org-html-link link desc info)
                                              allow-inter-link)))))

(defun org-re-reveal--math-enabled-p (info)
  "Return non-nil if the math plugin is enabled with INFO."
  (or (member 'math (org-re-reveal--enabled-plugins info))
      (member "math" (org-re-reveal--enabled-plugins info))))

(defun org-re-reveal-latex-environment (latex-env contents info)
  "Transcode a LaTeX environment from Org to Reveal.
LATEX-ENV is the Org element.  CONTENTS is the contents of the environment.
INFO is a plist holding contextual information.
Before version 3.16.0, org-re-reveal enabled MathJax for LaTeX environments
by adding a script element to load the library.
Since version 3.16.0, the script element is not added if the math plugin
of reveal.js is activated."
  (unless (org-re-reveal--math-enabled-p info)
    (setq info (plist-put info :reveal-mathjax t)))
  (let ((attrs (org-export-read-attribute :attr_html latex-env)))
    (format "<div%s>\n%s\n</div>\n"
            (if attrs (concat " " (org-html--make-attribute-string attrs)) "")
            (org-html-latex-environment latex-env contents info))))

(defun org-re-reveal-latex-fragment (frag contents info)
  "Transcode a LaTeX fragment from Org to Reveal.
FRAG is the Org element.  CONTENTS is the contents of the fragment.
INFO is a plist holding contextual information."
  (unless (org-re-reveal--math-enabled-p info)
    (setq info (plist-put info :reveal-mathjax t)))
  (org-html-latex-fragment frag contents info))

(defun org-re-reveal-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to Reveal.
CONTENTS is the contents of the list.  INFO is a plist holding
contextual information.
Extract and set `attr_html' to plain-list tag attributes."
  (ignore info) ; Silence byte compiler
  (let ((tag (cl-case (org-element-property :type plain-list)
               (ordered "ol")
               (unordered "ul")
               (descriptive "dl")))
        (attrs (org-export-read-attribute :attr_html plain-list)))
    (format "%s<%s%s>\n%s\n</%s>%s"
            (if (string= org-html-checkbox-type 'html) "<form>" "")
            tag
            (if attrs (concat " " (org-html--make-attribute-string attrs)) "")
            contents
            tag
            (if (string= org-html-checkbox-type 'html) "</form>" ""))))

(defun org-re-reveal-format-spec (info)
  "Return format specification with INFO.
Formatting extends `org-html-format-spec' such that
%-sequences for `org-re-reveal-title-slide' are available.
Speaker notes on the title slide with \"%n\" make use of
`org-re-reveal-notes-format-string'."
  (let* ((notes (org-re-reveal--read-file-as-string
                 (plist-get info :reveal-title-slide-notes)))
         (voice (plist-get info :reveal-with-tts))
         (prefix (plist-get info :reveal-tts-name-prefix))
         (html-notes (when notes
                       (when voice
                         (org-re-reveal--write-tts-files
                          notes voice info (concat prefix "0.0")))
                       (org-export-string-as notes 're-reveal t))))
    (append (org-html-format-spec info)
            `((?A . ,(org-export-data
                      (plist-get info :reveal-academic-title) info))
              (?m . ,(org-export-data
                      (plist-get info :reveal-miscinfo) info))
              (?n . ,(org-re-reveal--notes-to-html html-notes))
              (?q . ,(url-encode-url
                      (org-export-data
                       (plist-get info :reveal-talk-qr-code) info)))
              (?u . ,(url-encode-url
                      (org-export-data
                       (plist-get info :reveal-talk-url) info)))))))

(defun org-re-reveal--build-pre-postamble (type info spec)
  "Depending on TYPE, return preamble, postamble, postscript, or nil.
Use plist INFO and format specification SPEC."
  (let ((section (plist-get info (intern (format ":reveal-%s" type)))))
    (when section
      (let ((section-contents
             (if (functionp (intern section)) (funcall (intern section) info)
               ;; else section is a string.
               (format-spec section spec))))
        (when (org-string-nw-p section-contents)
          (org-element-normalize-string section-contents))))))

(defun org-re-reveal-section (section contents info)
  "Transcode a SECTION element from Org to Reveal.
CONTENTS holds the contents of the section.  INFO is a plist
holding contextual information."
  (let ((footer (org-re-reveal--footer info section t))
        (slide-container (plist-get info :reveal-slide-container))
        (slide-grid-div (plist-get info :reveal-slide-grid-div))
        (parent (org-export-get-parent-element section)))
    (if parent
      (concat (format slide-container (or contents ""))
              footer
              (if (and parent (< 0 (length slide-grid-div)))
                  "</div>\n"
                ""))
      ;; The first section (without parent, before first headline)
      ;; should not be important at all.  We keep it here for backward
      ;; compatibility.  In test cases, this produces an empty line
      ;; after the title slide.
      (concat contents footer))))

(defun org-re-reveal--using-highlight.js (info)
  "Check with INFO whether highlight.js plugin is enabled."
  (let ((reveal-plugins
         (org-re-reveal--enabled-plugins info)))
    (memq 'highlight reveal-plugins)))

(defun org-re-reveal--buffer-substring-html-escape (start end)
  "Convert buffer substring characters from plain text to HTML equivalent.
START and END are character positions as used by `buffer-substring'.
Conversion is done by escaping special HTML chars."
  (org-html-encode-plain-text (buffer-substring start end)))

(defun org-re-reveal-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to Reveal.
INFO is a plist holding contextual information.  CONTENTS is unused."
  (ignore contents) ; Silence byte compiler
  (if (org-export-read-attribute :attr_html src-block :textarea)
      (org-html--textarea-block src-block)
    (let* ((use-highlight (org-re-reveal--using-highlight.js info))
           (use-htmlize (or (not org-re-reveal-no-htmlize-src)
                            (org-export-read-attribute
                             :attr_reveal src-block :htmlize)))
           (lang (org-element-property :language src-block))
           (caption (org-export-get-caption src-block))
           (code (if (and (not use-highlight) use-htmlize)
                     (org-html-format-code src-block info)
                   (cl-letf (((symbol-function
                               'org-html-htmlize-region-for-paste)
                              #'org-re-reveal--buffer-substring-html-escape))
                     (org-html-format-code src-block info))))
           (code-attribs (or (org-export-read-attribute
                              :attr_reveal src-block :code_attribs) ""))
           (label (let ((lbl (org-element-property :name src-block)))
                    (if (not lbl) ""
                      (format " id=\"%s\"" lbl))))
           (klipse-setup (org-re-reveal--parse-listoption
                          info :reveal-klipse-setup))
           (klipsify (and (member lang (mapcar #'car klipse-setup))
                          (plist-get info :reveal-klipsify-src)
                          (not (org-export-read-attribute
                                :attr_reveal src-block
                                :no-klipsify)))))
      (let* ((attr-html (org-export-read-attribute :attr_html src-block))
             (attr-string (if attr-html
                              (concat
                               " " (org-html--make-attribute-string attr-html))
                            ""))
             (pre-class (if (or klipsify use-highlight)
                            ;; With klipsify and highlight.js, lang is
                            ;; part of code tag.  Do not repeat on pre tag.
                            ""
                          (if lang
                              (format " class=\"src src-%s\"" lang)
                            " class=\"example\"")))
             (pre-tag (format "<pre%s%s>"
                              (if attr-html attr-string pre-class)
                              label)))
        (if (not lang)
            (format "%s\n%s</pre>" pre-tag code)
          (if klipsify
              (let* ((triple (assoc lang klipse-setup))
                     (selectorclass (nth 2 triple)))
                (concat
                 (format "%s<code class=\"%s\" %s>\n"
                         pre-tag selectorclass code-attribs)
                 (if (string= lang "html")
                     (replace-regexp-in-string
                      "'" "&#39;"
                      (replace-regexp-in-string
                       "<" "&lt;"
                       (replace-regexp-in-string
                        ">" "&gt;"
                        (replace-regexp-in-string
                         "&" "&amp;"
                         (cl-letf (((symbol-function
                                     'org-html-htmlize-region-for-paste)
                                    #'buffer-substring))
                           (org-html-format-code src-block info))))))
                   (replace-regexp-in-string "'" "&#39;" code))
                 "</code></pre>\n"))
            (format
             "<div class=\"org-src-container\">\n%s%s\n</div>"
             (if (not caption) ""
               (format "<label class=\"org-src-name\">%s</label>"
                       (org-export-data caption info)))
             (if use-highlight
                 (format "\n%s<code class=\"%s\" %s>%s</code></pre>"
                         pre-tag lang code-attribs code)
               (format "\n%s%s</pre>" pre-tag code)))))))))

(defun org-re-reveal--auto-title-slide-template (info spec)
  "Generate an automatic title slide template with INFO and SPEC.
Add title, author, e-mail, date, and timestamp (if enabled by
org-export options)."
  (let* ((title (org-export-data (plist-get info :title) info))
         (author (cdr (assq ?a spec)))
         (email (cdr (assq ?e spec)))
         (date (cdr (assq ?d spec))))
    (concat
     (when (and (plist-get info :with-title)
                (org-string-nw-p title))
       (concat "<h1 class=\"title\">" title "</h1>"))
     (when (and (plist-get info :with-author)
                (org-string-nw-p author))
       (concat "<h2 class=\"author\">" author "</h2>"))
     (when (and (plist-get info :with-email)
                (org-string-nw-p email))
       (concat "<h2 class=\"email\">" email "</h2>"))
     (when (and (plist-get info :with-date)
                (org-string-nw-p date))
       (concat "<h2 class=\"date\">" date "</h2>"))
     (when (plist-get info :time-stamp-file)
       (concat "<p class=\"date\">"
               (org-html--translate "Created" info)
               ": "
               (format-time-string
                (plist-get info :html-metadata-timestamp-format))
               "</p>")))))

(defun org-re-reveal--remote-file-p (file-name)
  "Return t if FILE-NAME is an HTTP or FTP URL."
  (string-match-p "^\\(https?\\|ftp\\)://" file-name))

(defun org-re-reveal--check-single-file (info)
  "Raise error if INFO violates single file export requirements."
  (let ((root-path (plist-get info :reveal-root))
        (in-single-file (plist-get info :reveal-single-file)))
    (when (and in-single-file
               (org-re-reveal--remote-file-p root-path))
      (org-re-reveal--abort-with-message-box
       "Single file export requires local reveal.js resources (no CDN).  See Readme.org and customize `org-re-reveal-root' or set \"REVEAL_ROOT\"."))))

(defun org-re-reveal--maybe-title-notes (info)
  "Return notes for title slide from INFO."
  (plist-get info :title-notes))

(defun org-re-reveal-template (contents info)
  "Return complete document string after HTML conversion.
CONTENTS is the transcoded contents string.
INFO is a plist holding export options."
  (let ((spec (org-re-reveal-format-spec info))
        (tdm (plist-get info :reveal-tdm-reservation)))
    (org-re-reveal--check-single-file info)
    (org-re-reveal--setup-paths info)
    (concat
     (format "<!DOCTYPE html>\n<html%s>\n<head>\n"
             (org-re-reveal--if-format " lang=\"%s\"" (plist-get info :language)))
     "<meta charset=\"utf-8\"/>\n"
     (org-re-reveal--if-format "<title>%s</title>\n"
                               (org-export-data (plist-get info :title) info))
     (org-re-reveal--if-format "<meta name=\"author\" content=\"%s\"/>\n"
                               (org-element-interpret-data (plist-get info :author)))
     (org-re-reveal--if-format "<meta name=\"description\" content=\"%s\"/>\n"
                               (plist-get info :description))
     (org-re-reveal--if-format "<meta name=\"keywords\" content=\"%s\"/>\n"
                               (plist-get info :keywords))
     (org-re-reveal--if-format "<meta name=\"viewport\" content=\"%s\"/>\n"
                               (plist-get info :reveal-viewport))
     (if (plist-get info :reveal-mobile-app)
         "<meta name=\"mobile-web-app-capable\" content=\"yes\">\n<meta name=\"apple-mobile-web-app-capable\" content=\"yes\">\n"
       "")
     (if tdm
         (concat "<meta name=\"tdm-reservation\" content=\"1\">\n"
                 (when (and (stringp tdm)
                            (string-prefix-p "http" tdm))
                   (format "<meta name=\"tdm-policy\" content=\"%s\">\n" tdm)))
       "")
     (org-re-reveal-stylesheets info)
     (org-re-reveal--build-pre-postamble 'head-preamble info spec)
     (org-re-reveal-mathjax-scripts info)
     (org-element-normalize-string (plist-get info :html-head))
     (org-element-normalize-string (plist-get info :html-head-extra))
     "</head>\n<body"
     (org-re-reveal--if-format " %s" org-re-reveal-body-attrs)
     ">\n"
     (org-re-reveal--build-pre-postamble 'preamble info spec)
     "<div class=\"reveal\">
<div class=\"slides\">\n"
     ;; Title slides
     (let ((title-slide (plist-get info :reveal-title-slide)))
       (when (and (or (eq 'auto title-slide)
                      (and (stringp title-slide) (< 0 (length title-slide))))
                  (or (not (plist-get info :reveal-subtree))
                      (plist-get info :reveal-subtree-with-title-slide)))
         (let ((slide-attrs (org-re-reveal--slide-common-attrs "title" info))
               (title-slide-with-header (plist-get info :reveal-slide-global-header))
               (title-slide-with-footer (plist-get info :reveal-slide-global-footer)))
           (concat "<section id=\"sec-title-slide\""
                   (or slide-attrs "")
                   ">\n"
                   (org-re-reveal--wrap-div-grid
                    (concat
                     (when title-slide-with-header
                       (let ((header (plist-get info :reveal-slide-header)))
                         (when header (format org-re-reveal-slide-header-html header))))
                     (cond ((eq title-slide nil) nil)
                           ((stringp title-slide)
                            (let* ((file-contents
                                    (org-re-reveal--read-file-as-string title-slide))
                                   (title-string (or file-contents title-slide)))
                              (format-spec title-string spec)))
                           ((eq title-slide 'auto)
                            (org-re-reveal--auto-title-slide-template info spec)))
                     "\n"
                     (when title-slide-with-footer
                       (org-re-reveal--footer info)))
                    info)
                   (org-re-reveal--maybe-title-notes info)
                   "</section>\n"))))
     contents
     "</div>
</div>\n"
     (org-re-reveal--build-pre-postamble 'postamble info spec)
     (org-re-reveal-scripts info)
     (org-re-reveal--klipsify-script info)
     (org-re-reveal--build-pre-postamble 'postscript info spec)
     "</body>
</html>\n")))

(defun org-re-reveal-filter-parse-tree (tree backend info)
  "Do filtering before parsing TREE.
TREE is the parse tree being exported.  BACKEND is the export
back-end used.  INFO is a plist-used as a communication channel.
BACKEND must be (or be derived from) `re-reveal'.
Modify the TREE in two ways:
First, map each `attr_reveal' attribute to corresponding
`attr_html' attributes.
Second, if `org-re-reveal-generate-custom-ids' is t (or option
\"reveal_generate_ids\" is t), generate \"CUSTOM_ID\" values for
section headings that do not have one already."
  (cl-assert (org-export-derived-backend-p backend 're-reveal) nil
             (format "Function org-re-reveal-filter-parse-tree called on unexpected backend: %s" backend))
  (let ((default-frag-style (plist-get info :reveal-default-frag-style)))
    (org-element-map tree (remq 'item org-element-all-elements)
      (lambda (elem) (org-re-reveal-append-frag elem default-frag-style))))
  (when (plist-get info :reveal-generate-ids)
    (let ((numbering (org-export--collect-headline-numbering tree info)))
      (dolist (pair numbering nil)
        (let ((headline (car pair))
              (number (cdr pair)))
          (when (org-export-numbered-headline-p headline info)
            (let ((section-number (mapconcat #'number-to-string number "-")))
              (when (and (> (length section-number) 0)
                         (not (org-element-property :CUSTOM_ID headline)))
                (org-element-put-property headline :CUSTOM_ID section-number))))))))
  ;; Return the updated tree.
  tree)

(defun org-re-reveal--update-attr-html (elem frag default-style
                                             &optional frag-index frag-audio)
  "Update ELEM's attr_html attribute with reveal's fragment attributes.
FRAG is the fragment style, a DEFAULT-STYLE may be used;
optional FRAG-INDEX and FRAG-AUDIO may indicate fragment positions
and audio files."
  (when (and frag (not (string= frag "none")))
    (org-re-reveal--add-class
     elem
     (if (string= frag t)
         (if default-style
             (format "fragment %s" default-style)
           "fragment")
       (format "fragment %s" frag)))
    (let ((attr-html (org-element-property :attr_html elem)))
      (when frag-index
        ;; Index positions should be numbers or the minus sign.
        (cl-assert (or (integerp frag-index)
                       (eq frag-index '-)
                       (and (not (listp frag-index))
                            (not (char-equal
                                  (string-to-char frag-index) ?\())))
                   nil "Index cannot be a list: %s" frag-index)
        (push (format ":data-fragment-index %s" frag-index) attr-html))
      (when (and frag-audio (not (string= frag-audio "none")))
        (push (format ":data-audio-src %s" frag-audio) attr-html))
      (org-element-put-property elem :attr_html attr-html))))

(defun org-re-reveal-append-frag (elem default-style)
  "Append transformed fragment from ELEM with DEFAULT-STYLE.
Read fragment from ELEM and append transformed fragment attribute to ELEM's
attr_html plist."
  (let ((frag (org-export-read-attribute :attr_reveal elem :frag))
        (frag-index (org-export-read-attribute :attr_reveal elem :frag_idx))
        (frag-audio (org-export-read-attribute :attr_reveal elem :audio)))
    (when frag
      (if (and (string= (org-element-type elem) 'plain-list)
               (char-equal (string-to-char frag) ?\())
          (let* ((items (org-element-contents elem))
                 (frag-list (car (read-from-string frag)))
                 (frag-list (if default-style
                                (mapcar (lambda (s)
                                          "Replace t with default-style"
                                          (if (string= s t) default-style
                                            s))
                                        frag-list)
                              frag-list))
                 (itemno (length items))
                 (style-list (make-list itemno default-style))
                 ;; Make sure that we have enough fragments.  Duplicate the
                 ;; last element of frag-list so that frag-list and items
                 ;; have the same length.
                 (last-frag (car (last frag-list)))
                 (tail-list (make-list
                             (- itemno (length frag-list)) last-frag))
                 (frag-list (append frag-list tail-list))
                 ;; Concerning index positions and audio files, check later
                 ;; that their number is OK.
                 (frag-index (if frag-index
                                 (car (read-from-string frag-index))
                               (make-list itemno nil)))
                 (frag-audio (when frag-audio
                               (car (read-from-string frag-audio)))))
            ;; As we are looking at fragments in lists, we make sure
            ;; that other specs are lists of proper length.
            (cl-assert (listp frag-index) t
                       "Must use list for index positions, not: %s")
            (when frag-index
              (cl-assert (= (length frag-index) itemno) nil
                         "Use one index per item!  %s has %d, need %d"
                         frag-index (length frag-index) (length items)))
            (cl-assert (listp frag-audio) t
                       "Must use list for audio files! %s")
            (when frag-audio
              (cl-assert (= (length frag-audio) itemno) nil
                         "Use one audio file per item!  %s has %d, need %d"
                         frag-audio (length frag-audio) itemno))
            (if frag-audio
                (cl-mapc 'org-re-reveal--update-attr-html
                         items frag-list style-list frag-index frag-audio)
              (cl-mapc 'org-re-reveal--update-attr-html
                       items frag-list style-list frag-index)))
        (org-re-reveal--update-attr-html
         elem frag default-style frag-index frag-audio))
      elem)))

(defun org-re-reveal--tts-index-name (info)
  "Construct name of index file from INFO."
  (let ((tts-dir (org-re-reveal--tts-dir info))
        (prefix (plist-get info :reveal-tts-name-prefix)))
    (concat tts-dir prefix ".tts")))

(defun org-re-reveal--add-to-tts-index (voice gap name hash info)
  "Add NAME with HASH of text for TTS to index file with INFO.
If the index file does not exist, create it, writing VOICE, GAP, as
well as the start and the end slide gaps as first line."
  (let ((index (org-re-reveal--tts-index-name info))
        (start-gap (plist-get info :reveal-tts-start-slide-gap))
        (end-gap (plist-get info :reveal-tts-end-slide-gap)))
    (unless (file-exists-p index)
      (append-to-file
       (format "%s %s %s %s\n" voice gap start-gap end-gap) nil index))
    (append-to-file (format "%s %s\n" name hash) nil index)))

(defun org-re-reveal--create-tts-text (hash text dir)
  "Create file under name HASH in DIR with TEXT for TTS.
If the file exists already, do nothing."
  (let ((filename (concat dir hash)))
    (unless (file-exists-p filename)
      (append-to-file text nil filename))))

(defun org-re-reveal-prepare-tts (backend)
  "For BACKEND re-reveal, create tts directory, remove outdated index file."
  (when (org-export-derived-backend-p backend 're-reveal)
    (let* ((info (org-export-get-environment 're-reveal))
           (with-tts (plist-get info :reveal-with-tts))
           (dir (org-re-reveal--tts-dir info))
           (index (org-re-reveal--tts-index-name info)))
      (when with-tts
        (when (not (file-directory-p dir))
          (make-directory dir t))
        (when (file-exists-p index)
          (delete-file index))))))

(defun org-re-reveal-export-to-html
    (&optional async subtreep visible-only body-only ext-plist backend)
  "Export current buffer to a reveal.js HTML file.
Optional ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, EXT-PLIST are passed
to `org-export-to-file'.
Optional BACKEND must be `re-reveal' or a backend derived from it."
  (interactive)
  (let* ((backend (or backend 're-reveal))
         (extension (concat "." org-html-extension))
         (client-ext (concat org-re-reveal-multiplex-client-ext extension))
         (file (org-export-output-file-name extension subtreep))
         (clientfile (org-export-output-file-name client-ext subtreep))
         (org-html-container-element "div"))

    (setq org-re-reveal-client-multiplex nil)
    (org-export-to-file backend file
      async subtreep visible-only body-only ext-plist)

    ;; Export the client HTML file if org-re-reveal-client-multiplex is set true
    ;; by previous call to org-export-to-file
    (if org-re-reveal-client-multiplex
        (org-export-to-file backend clientfile
          async subtreep visible-only body-only ext-plist))
    file))

(defun org-re-reveal-export-to-html-and-browse
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current buffer to a reveal.js and browse HTML file.
Optional ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, EXT-PLIST are passed
to `org-re-reveal-export-to-html'."
  (interactive)
  (browse-url-of-file
   (expand-file-name
    (org-re-reveal-export-to-html
     async subtreep visible-only body-only ext-plist))))

(defun org-re-reveal-export-current-subtree
    (&optional async subtreep visible-only body-only ext-plist)
  "Export current subtree to a Reveal.js HTML file.
Optional ASYNC, SUBTREEP, VISIBLE-ONLY, BODY-ONLY, EXT-PLIST are passed
to `org-re-reveal-export-to-html'."
  (interactive)
  (org-narrow-to-subtree)
  (let ((ret (org-re-reveal-export-to-html
              async subtreep visible-only body-only
              (plist-put ext-plist :reveal-subtree t))))
    (widen)
    ret))

;;;###autoload
(defun org-re-reveal-publish-to-reveal
    (plist filename pub-dir &optional backend)
  "Publish an Org file to HTML.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.  Optional BACKEND may specify a derived export
backend.
Return output file name."
  (let ((org-re-reveal-client-multiplex nil)
        (org-re-reveal-pub-dir pub-dir)
        (org-html-container-element "div"))
    (org-publish-org-to
     (or backend 're-reveal) filename
     (concat "." org-html-extension) plist pub-dir)))

;;;###autoload
(defun org-re-reveal-publish-to-reveal-client
    (plist filename pub-dir &optional backend)
  "Publish an Org file to HTML as multiplex client.
FILENAME is the filename of the Org file to be published.  PLIST
is the property list for the given project.  PUB-DIR is the
publishing directory.  Optional BACKEND may specify a derived export
backend.
If `org-re-reveal-client-multiplex-filter' is non-nil, use it as regular
expression to only publish FILENAME if it matches this regular expression.
Return output file name."
  (if (or (not org-re-reveal-client-multiplex-filter)
          (string-match org-re-reveal-client-multiplex-filter filename))
      (let ((org-re-reveal-client-multiplex t)
            (org-re-reveal-pub-dir pub-dir)
            (org-html-container-element "div")
            ;; TODO Disable TTS code paths?  Probably multiplex
            ;; presentations do not use TTS anyways?
            (client-ext (concat org-re-reveal-multiplex-client-ext
                                "." org-html-extension)))
        (org-publish-org-to
         (or backend 're-reveal) filename client-ext plist pub-dir))
    (message "File '%s' not published (not matched by '%s')."
             filename org-re-reveal-client-multiplex-filter)
    nil)
  :package-version '(org-re-reveal . "2.12.0"))

;; Register auto-completion for speaker notes.
(when org-re-reveal-note-key-char
  (add-to-list 'org-structure-template-alist
               (if (version< org-version "9.2")
                   (list org-re-reveal-note-key-char "#+BEGIN_NOTES\n\?\n#+END_NOTES")
                 (cons org-re-reveal-note-key-char "notes"))))

;; Make sure that TTS directory exists.
(add-hook 'org-export-before-parsing-hook #'org-re-reveal-prepare-tts)

;;; Extract version string.
;;;###autoload
(defun org-re-reveal-version ()
  "Display version string for org-re-reveal from Lisp file."
  (interactive)
  (let ((lisp-file
         (concat (file-name-sans-extension (locate-library "org-re-reveal"))
                 ".el")))
    (with-temp-buffer
      (insert-file-contents lisp-file)
      (goto-char (point-min))
      (re-search-forward "^;; Version: \\([0-9.]+\\)$")
      (message "org-re-reveal version %s" (match-string 1)))))

(provide 'org-re-reveal)
;;; org-re-reveal.el ends here
