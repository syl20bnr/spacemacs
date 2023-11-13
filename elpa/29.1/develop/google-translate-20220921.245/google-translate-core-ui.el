;;; google-translate-core-ui.el --- The google translate core UI

;; Copyright (C) 2012 Oleksandr Manzyuk <manzyuk@gmail.com>

;; Author: Oleksandr Manzyuk <manzyuk@gmail.com>
;; Maintainer: Andrey Tykhonov <atykhonov@gmail.com>
;; URL: https://github.com/atykhonov/google-translate
;; Package-Requires: ((emacs "24.3") (popup "0.5.8"))
;; Version: 0.12.0
;; Keywords: convenience

;; Contributors:
;;   Tassilo Horn <tsdh@gnu.org>
;;   Bernard Hurley <bernard@marcade.biz>
;;   Chris Bilson <cbilson@pobox.com>
;;   Takumi Kinjo <takumi.kinjo@gmail.com>
;;   momomo5717 <momomo5717@gmail.com>
;;   stardiviner <numbchild@gmail.com>

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This script provides the most common functions and variables for
;; UI. It does not contain any interactive functions and overall is
;; not going to be used directly by means of
;; `execute-extended-command' (M-x). Its purpose to provide the most
;; valuable and useful functionality for packages and scripts which
;; provide UI.
;;
;; The most important functions are the following:
;;
;; - `google-translate-translate'
;;
;; - `google-translate-read-source-language'
;;
;; - `google-translate-read-target-language'
;;
;; `google-translate-translate' translates the given text from source
;; language to target language and shows a translation.
;;
;; `google-translate-read-source-language' reads source language from
;; minibuffer and returns language
;; abbreviation. `google-translate-read-target-language' reads target
;; language from minibuffer and returns language abbreviation.

;; Customization:

;; You can customize the following variables:
;;
;; - `google-translate-output-destination'
;;
;; - `google-translate-enable-ido-completion'
;;
;; - `google-translate-show-phonetic'
;;
;; - `google-translate-listen-program'
;;
;; - `google-translate-pop-up-buffer-set-focus'
;;
;; - `google-translate-preferable-input-methods-alist'
;;
;; `google-translate-output-destination' determines translation output
;; destination. If `nil' the translation output will be displayed in
;; the pop up buffer. If value equal to `echo-area' then translation
;; outputs in the Echo Area. In case of `popup' the translation
;; outputs to the popup tooltip using `popup' package. In case of
;; `kill-ring' the translation outputs to the kill ring. And in case
;; of `current-buffer' the translation outputs to the current
;; buffer. If you would like output translation to the Echo Area you
;; would probably like to increase it because only part of translation
;; could visible there with the default settings. To increase Echo
;; Area you could increase the value of `max-mini-window-height'
;; variable, for example: `(setq max-mini-window-height 0.5)'.
;;
;; If `google-translate-enable-ido-completion' is non-NIL, the input
;; will be read with ido-style completion.
;;
;; The variable `google-translate-show-phonetic' controls whether the
;; phonetic spelling of the original text and its translation is
;; displayed if available. If you want to see the phonetics, set this
;; variable to t.
;;
;; The variable `google-translate-listen-program' determines the
;; program to use to listen translations. By default the program looks
;; for `mplayer' in the PATH, if `mplayer' is found then listening
;; function will be available and you'll see `Listen' button in the
;; buffer with the translation. You can use any other suitable
;; program. If you use Windows please download and unpack `mplayer'
;; and add its path (directory) to the system PATH variable. Please
;; note that translation listening is not available if
;; `google-translate-output-destination' is set to `echo-area' or
;; `popup'.
;;
;; The variable `google-translate-pop-up-buffer-set-focus' determines
;; whether window (buffer) with translation gets focus when it pop
;; ups. If `nil', it doesn't get focus and focus remains in the same
;; window as was before translation. If `t', window (buffer with
;; translation) gets focus. Please note that that setting works only
;; for pop up buffer, i.e. when `google-translate-output-destination'
;; is `nil'.
;;
;; The `google-translate-input-method-auto-toggling' variable
;; determines whether input method auto toggling is enabled or not.
;;
;; While switching among languages I noticed that I change input
;; method quite often. Input method auto toggling allows switch on
;; appropriate input method while switching among languages. Auto
;; toggling will work in case of
;; `google-translate-input-method-auto-toggling' is set to `t' and
;; `google-translate-preferable-input-methods-alist' is defined
;; properly.
;;
;; This variable may be defined as follow (just for example):
;;
;; (setq google-translate-preferable-input-methods-alist '((nil . ("en"))
;;                                                         (ukrainian-programmer-dvorak . ("ru" "uk"))))
;;
;; In this way, input method is disabled (because of nil) for the
;; minibuffer when source language is English. And
;; "ukrainian-programmer-dvorak" input method is enabled when source
;; language is Russian or Ukrainian.

;; Customization of faces:

;; - `google-translate-text-face', used to display the original text
;;   (defaults to `default')
;;
;; - `google-translate-phonetic-face', used to display the phonetics
;;   (defaults to `shadow')
;;
;; - `google-translate-translation-face', used to display the highest
;;   ranking translation (defaults to `default' with the `weight'
;;   attribute set to `bold')
;;
;; - `google-translate-suggestion-label-face' used to display the
;;   label for suggestion (defaults to `default' with the `foreground'
;;   attribute set to `red')
;;
;; - `google-translate-suggestion-face' used to display the suggestion
;;   in case of word is misspelled (defaults to `default' with the
;;   `slant' attribute set to `italic' and `underline' attribute set
;;   to `t')
;;
;; - `google-translate-listen-button-face' used to display the "Listen"
;;   button (defaults to `height' 0.8).
;;
;; For example, to show the translation in a larger font change the
;; `height' attribute of the face `google-translate-translation-face'
;; like so:
;;
;;   (set-face-attribute 'google-translate-translation-face nil :height 1.4)
;;
;;
;;; Code:
;;

(eval-when-compile (require 'cl-lib))
(require 'google-translate-core)
(require 'ido)
(require 'popup)
(require 'color)
(require 'facemenu)

(defvar google-translate-supported-languages-alist
  '(("Afrikaans"           . "af")
    ("Albanian"            . "sq")
    ("Amharic"             . "am")
    ("Arabic"              . "ar")
    ("Armenian"            . "hy")
    ("Azerbaijani"         . "az")
    ("Basque"              . "eu")
    ("Belarusian"          . "be")
    ("Bengali"             . "bn")
    ("Bosnian"             . "bs")
    ("Bulgarian"           . "bg")
    ("Catalan"             . "ca")
    ("Cebuano"             . "ceb")
    ("Chichewa"            . "ny")
    ("Chinese Simplified"  . "zh-CN")
    ("Chinese Traditional" . "zh-TW")
    ("Corsican"            . "co")
    ("Croatian"            . "hr")
    ("Czech"               . "cs")
    ("Danish"              . "da")
    ("Dutch"               . "nl")
    ("English"             . "en")
    ("Esperanto"           . "eo")
    ("Estonian"            . "et")
    ("Filipino"            . "tl")
    ("Finnish"             . "fi")
    ("French"              . "fr")
    ("Frisian"             . "fy")
    ("Galician"            . "gl")
    ("Georgian"            . "ka")
    ("German"              . "de")
    ("Greek"               . "el")
    ("Gujarati"            . "gu")
    ("Haitian Creole"      . "ht")
    ("Hausa"               . "ha")
    ("Hawaiian"            . "haw")
    ("Hebrew"              . "iw")
    ("Hindi"               . "hi")
    ("Hmong"               . "hmn")
    ("Hungarian"           . "hu")
    ("Icelandic"           . "is")
    ("Igbo"                . "ig")
    ("Indonesian"          . "id")
    ("Irish"               . "ga")
    ("Italian"             . "it")
    ("Japanese"            . "ja")
    ("Javanese"            . "jw")
    ("Kannada"             . "kn")
    ("Kazakh"              . "kk")
    ("Khmer"               . "km")
    ("Korean"              . "ko")
    ("Kurdish (Kurmanji)"  . "ku")
    ("Kyrgyz"              . "ky")
    ("Lao"                 . "lo")
    ("Latin"               . "la")
    ("Latvian"             . "lv")
    ("Lithuanian"          . "lt")
    ("Luxembourgish"       . "lb")
    ("Macedonian"          . "mk")
    ("Malagasy"            . "mg")
    ("Malay"               . "ms")
    ("Malayalam"           . "ml")
    ("Maltese"             . "mt")
    ("Maori"               . "mi")
    ("Marathi"             . "mr")
    ("Mongolian"           . "mn")
    ("Myanmar (Burmese)"   . "my")
    ("Nepali"              . "ne")
    ("Norwegian"           . "no")
    ("Pashto"              . "ps")
    ("Persian"             . "fa")
    ("Polish"              . "pl")
    ("Portuguese"          . "pt")
    ("Punjabi"             . "pa")
    ("Romanian"            . "ro")
    ("Russian"             . "ru")
    ("Samoan"              . "sm")
    ("Scots Gaelic"        . "gd")
    ("Serbian"             . "sr")
    ("Sesotho"             . "st")
    ("Shona"               . "sn")
    ("Sindhi"              . "sd")
    ("Sinhala"             . "si")
    ("Slovak"              . "sk")
    ("Slovenian"           . "sl")
    ("Somali"              . "so")
    ("Spanish"             . "es")
    ("Sundanese"           . "su")
    ("Swahili"             . "sw")
    ("Swedish"             . "sv")
    ("Tajik"               . "tg")
    ("Tamil"               . "ta")
    ("Telugu"              . "te")
    ("Thai"                . "th")
    ("Turkish"             . "tr")
    ("Ukrainian"           . "uk")
    ("Urdu"                . "ur")
    ("Uzbek"               . "uz")
    ("Vietnamese"          . "vi")
    ("Welsh"               . "cy")
    ("Xhosa"               . "xh")
    ("Yiddish"             . "yi")
    ("Yoruba"              . "yo")
    ("Zulu"                . "zu"))
  "Alist of the languages supported by Google Translate.

Each element is a cons-cell of the form (NAME . CODE), where NAME
is a human-readable language name and CODE is its code used as a
query parameter in HTTP requests.")

(defvar google-translate-translation-listening-debug nil
  "For debug translation listening purposes.")

(cl-defstruct gtos
  "google translate output structure contains miscellaneous
  information which intended to be outputed to the buffer, echo
  area or popup tooltip."
  source-language target-language text
  auto-detected-language text-phonetic translation
  translation-phonetic detailed-translation suggestion detailed-definition)

(defgroup google-translate-core-ui nil
  "Emacs core UI script for the Google Translate package."
  :group 'processes)

(defcustom google-translate-enable-ido-completion nil
  "If non-NIL, use `ido-completing-read' rather than `completing-read' for reading input."
  :group 'google-translate-core-ui
  :type  '(choice (const :tag "No"  nil)
                  (other :tag "Yes" t)))

(defcustom google-translate-show-phonetic nil
  "If non-NIL, try to show the phonetic spelling."
  :group 'google-translate-core-ui
  :type '(choice (const :tag "No"  nil)
                 (const :tag "Yes" t)))

(defcustom google-translate-listen-program
  (executable-find "mplayer")
  "The program to use to listen translations.

By default the program looks for `mplayer' in the PATH, if
`mplayer' is found then listening function will be available and
you'll see `Listen' button in the buffer with the translation.
You can use any other suitable program."
  :group 'google-translate-core-ui
  :type '(string))

(defcustom google-translate-output-destination
  nil
  "Determines where translation output will be displayed.

- If it is `nil', output to temporary pop up buffer (default).
- `echo-area': output to the Echo Area.
- `popup': output to the popup tooltip using `popup' package.
- `kill-ring': the output will be added in `kill-ring'.
- `current-buffer': the output will be inserted to current buffer.
- `help': output to help buffer.
- `paragraph-overlay': output in current buffer overlay paragraph by paragraph.
- `paragraph-insert': output will be inserted in buffer paragraph by paragraph."
  :group 'google-translate-core-ui
  :type '(repeat (choice (const :tag "temporary popup buffer" nil)
                         (const :tag "Echo Area" echo-area)
                         (const :tag "popup tooltip using popup.el" popup)
                         (const :tag "kill-ring" kill-ring)
                         (const :tag "current buffer" current-buffer)
                         (const :tag "popup Help buffer" help)
                         (const :tag "paragraph-by-paragraph in overlay" paragraph-overlay)
                         (const :tag "paragraph-by-paragraph inserted in buffer" paragraph-insert))))

(defcustom google-translate-pop-up-buffer-set-focus
  nil
  "Determines whether result window (buffer) gets focus when it pop ups.

If nil, it doesn't get focus and focus remains in the same
window as was before translation. If t, window (buffer with
translation) gets focus.")

(defcustom google-translate-display-translation-phonetic t
  "Determines whether display phonetic transcription of the translating text.")

(defcustom google-translate-listen-button-label
  "[Listen]"
  "Label of the 'Listen' button."
  :group 'google-translate-core-ui
  :type 'string)

(defcustom google-translate-translation-to-kill-ring nil
  "Add translation to kill-ring after translate commands if it's `t'."
  :type 'boolean
  :safe #'booleanp
  :group 'google-translate-core-ui)

(defface google-translate-text-face
  '((t (:inherit default)))
  "Face used to display the original text."
  :group 'google-translate-core-ui)

(defface google-translate-phonetic-face
  '((t (:inherit shadow)))
  "Face used to display the phonetic spelling."
  :group 'google-translate-core-ui)

(defface google-translate-translation-face
  '((t (:weight bold)))
  "Face used to display the probable translation."
  :group 'google-translate-core-ui)

(defface google-translate-suggestion-label-face
  '((t (:foreground "red")))
  "Face used to display the suggestion label."
  :group 'google-translate-core-ui)

(defface google-translate-suggestion-face
  '((t (:slant italic :underline t)))
  "Face used to display the suggestion."
  :group 'google-translate-core-ui)

(defface google-translate-listen-button-face
  '((t (:inherit button :height 0.8)))
  "Face used to display button \"Listen\"."
  :group 'google-translate-core-ui)

(defvar google-translate-input-method-auto-toggling nil
  "When t, the current source language is compared.

Compared with the values from
`google-translate-preferable-input-methods-alist' and enables
appropriate input method for the minibuffer. So this feature may
allow to avoid switching between input methods while translating
using different languages.")

(defvar google-translate-preferable-input-methods-alist
  '((nil . nil))
  "Alist of preferable input methods for certain languages.

Each element is a cons-cell of the form (INPUT-METHOD
. LANGUAGES-LIST), where INPUT-METHOD is the input method which
will be switched on, when translation source language equals to
one of the language from the LANGUAGE-LIST.

INPUT-METHOD could be specified as nil. In such case input method
disables.

As example, this alist could looks like the following:

  '((nil . \"en\")
    (ukrainian-programmer-dvorak . (\"ru\" \"uk\")))

In this way, `ukrainian-programmer-dvorak' will be auto enabled
for the minibuffer when Russian or Ukrainian (as source language)
is active.")


(defvar google-translate-result-translation nil
  "The result translation of `google-translate-translate'.")

(defun google-translate-supported-languages ()
  "Return a list of names of languages supported by Google Translate."
  (mapcar #'car google-translate-supported-languages-alist))

(defun google-translate-language-abbreviation (language)
  "Return the abbreviation of LANGUAGE."
  (if (string-equal language "Detect language")
      "auto"
    (cdr (assoc language google-translate-supported-languages-alist))))

(defun google-translate-language-display-name (abbreviation)
  "Return a name suitable for use in prompts of the language whose abbreviation is ABBREVIATION."
  (if (string-equal abbreviation "auto")
      "unspecified language"
    (car (rassoc abbreviation google-translate-supported-languages-alist))))

(defun google-translate-paragraph (text face &optional output-format)
  "Return TEXT as a filled paragraph into the current buffer.
And apply FACE to it. Optionally use OUTPUT-FORMAT."
  (let ((beg (point))
        (output-format
         (if output-format output-format "\n%s\n")))
    (with-temp-buffer
      (insert (format output-format text))
      (facemenu-set-face face beg (point))
      (fill-region beg (point))
      (buffer-substring (point-min) (point-max)))))

(defun google-translate-setup-preferable-input-method (source-language)
  "Set input method which takes from the value of
`google-translate-preferable-input-methods-alist' variable."
  (interactive)
  (let* ((preferable-input-method
          (google-translate-find-preferable-input-method source-language)))
    (set-input-method preferable-input-method)))

(defun google-translate-find-preferable-input-method (source-language)
  "Look for the SOURCE-LANGUAGE in the
`google-translate-preferable-input-methods-alist' and return
input method for it."
  (let ((input-method nil))
    (dolist (item google-translate-preferable-input-methods-alist)
      (dolist (language (cdr item))
        (when (string-equal source-language language)
          (setq input-method (car item)))))
    input-method))

(defun google-translate--translation-title (gtos format)
  "Return translation title which contains information about used
source and target languages."
  (let ((source-language (gtos-source-language gtos))
        (target-language (gtos-target-language gtos))
        (auto-detected-language (gtos-auto-detected-language gtos)))
    (format format
            (if (string-equal source-language "auto")
                (format "%s (detected)"
                        (google-translate-language-display-name
                         auto-detected-language))
              (google-translate-language-display-name
               source-language))
            (google-translate-language-display-name
             target-language))))

(defun google-translate--translating-text (gtos format)
  "Outputs in buffer translating text."
  (let ((text (gtos-text gtos)))
    (let ((output-format format))
      (google-translate-paragraph
       text
       'google-translate-text-face
       output-format))))

(defun google-translate--text-phonetic (gtos format)
  "Outputs in buffer text phonetic in case of
`google-translate-show-phonetic' is set to t."
  (let ((text-phonetic (gtos-text-phonetic gtos)))
    (if (and google-translate-show-phonetic
             (not (string-equal text-phonetic "")))
        (google-translate-paragraph
         text-phonetic
         'google-translate-phonetic-face
         format)
      "")))

(defun google-translate--translated-text (gtos format)
  "Output in buffer translation."
  (let ((translation (gtos-translation gtos)))
    (google-translate-paragraph
     translation
     'google-translate-translation-face
     format)))

(defun google-translate--translation-phonetic (gtos format)
  "Output in buffer translation phonetic in case of
`google-translate-show-phonetic' is set to t."
  (let ((translation-phonetic (gtos-translation-phonetic gtos)))
    (if (and google-translate-show-phonetic
             (not (string-equal translation-phonetic "")))
        (google-translate-paragraph
         translation-phonetic
         'google-translate-phonetic-face
         format)
      "")))

(defun google-translate--detailed-translation (detailed-translation translation
                                                                    format1
                                                                    format2)
  "Return detailed translation."
  (with-temp-buffer
    (cl-loop for item across detailed-translation do
             (let ((index 0)
                   (label (aref item 0)))
               (unless (string-equal label "")
                 (put-text-property 0 (length label)
                                    'font-lock-face
                                    'google-translate-translation-face
                                    label)
                 (insert (format format1 label))
                 (cl-loop for translation across (aref item 2) do
                          (let ((content
                                 (format "%s (%s)"
                                         (aref translation 0)
                                         (mapconcat 'identity
                                                    (aref translation 1)
                                                    ", "))))
                            (insert (format format2
                                            (cl-incf index)
                                            content)))))))
    (buffer-substring (point-min) (point-max))))

(defun google-translate--detailed-definition (detailed-definition definition
                                                                  format1
                                                                  format2)
  "Return detailed definition."
  (with-temp-buffer
    (let ((section "DEFINITION"))
      (put-text-property 0 (length section)
                         'font-lock-face
                         'google-translate-translation-face
                         section)
      (insert (format "\n%s\n" section)))
    (cl-loop for item across detailed-definition do
             (let ((index 0)
                   (label (aref item 0)))
               (unless (string-equal label "")
                 (put-text-property 0 (length label)
                                    'font-lock-face
                                    'google-translate-translation-face
                                    label)
                 (insert (format format1 label))
                 (cl-loop for definition across (aref item 1) do
                          (insert (format format2
                                          (cl-incf index)
                                          (if (> (length definition) 2)
                                              (format "%s\n    \"%s\""
                                                      (aref definition 0)
                                                      (aref definition 2))
                                            (format "%s" (aref definition 0)))))))))
    (buffer-substring (point-min) (point-max))))

(defun google-translate--suggestion (gtos)
  "Return suggestion."
  (let ((source-language (gtos-source-language gtos))
        (target-language (gtos-target-language gtos))
        (suggestion (gtos-suggestion gtos)))
    (if suggestion
        (with-temp-buffer
          (insert "\n")
          (let ((beg (point)))
            (insert "Did you mean: ")
            (facemenu-set-face 'google-translate-suggestion-label-face
                               beg (point)))
          (goto-char (+ (point) 1))
          (let ((beg (point)))
            (insert-text-button suggestion
                                'action 'google-translate--suggestion-action
                                'follow-link t
                                'suggestion suggestion
                                'source-language source-language
                                'target-language target-language)
            (facemenu-set-face 'google-translate-suggestion-face
                               beg (point))
            (insert "\n"))
          (buffer-substring (point-min) (point-max)))
      "")))

(defun google-translate--suggestion-action (button)
  "Suggestion action which occur when suggestion button is
clicked."
  (interactive)
  (let ((suggestion (button-get button 'suggestion))
        (source-language (button-get button 'source-language))
        (target-language (button-get button 'target-language)))
    (google-translate-translate source-language
                                target-language
                                suggestion)))

(defun google-translate--listen-button (language text)
  "Return listen button."
  (with-temp-buffer
    (insert " ")
    (insert-text-button google-translate-listen-button-label
                        'action 'google-translate--listen-action
                        'face 'google-translate-listen-button-face
                        'follow-link t
                        'text text
                        'language language)
    (insert "\n")
    (buffer-substring (point-min) (point-max))))

(defun google-translate--listen-action (button)
  "Do translation listening."
  (interactive)
  (let ((text (button-get button 'text))
        (language (button-get button 'language)))
    (google-translate-listen-translation language text)))

(defun google-translate-listen-translation (language text)
  (let ((buf "*mplayer output*"))
    (message "Retrieving audio message...")
    (if google-translate-translation-listening-debug
        (with-current-buffer (get-buffer-create buf)
          (insert (format "Listen program: %s\r\n" google-translate-listen-program))
          (mapc (lambda (x) (insert (format "Listen URL: %s\r\n" x)))
                (google-translate-format-listen-urls text language))
          (apply 'call-process google-translate-listen-program nil t nil
                 (google-translate-format-listen-urls text language))
          (switch-to-buffer buf))
      (apply 'call-process google-translate-listen-program nil nil nil
             (google-translate-format-listen-urls text language)))))

(defun google-translate-translate (source-language target-language text &optional output-destination)
  "Translate TEXT from SOURCE-LANGUAGE to TARGET-LANGUAGE.

About the OUTPUT-DESTINATION, check out option
`google-translate-output-destination'.

To deal with multi-line regions, sequences of white space
are replaced with a single space. If the region contains not text, a
message is printed.

At last will save result translation to `google-translate-result-translation'."
  (let* ((json (google-translate-request source-language
                                         target-language
                                         text)))
    (if (null json)
        (message "Nothing to translate.")
      (let* ((detailed-translation
              (google-translate-json-detailed-translation json))
             (detailed-definition
              (google-translate-json-detailed-definition json))
             (gtos
              (make-gtos
               :source-language source-language
               :target-language target-language
               :auto-detected-language (aref json 2)
               :text text
               :text-phonetic (google-translate-json-text-phonetic json)
               :translation (google-translate-json-translation json)
               :translation-phonetic (if google-translate-display-translation-phonetic
                                         (google-translate-json-translation-phonetic json) "")
               :detailed-translation detailed-translation
               :detailed-definition detailed-definition
               :suggestion (when (null detailed-translation)
                             (google-translate-json-suggestion json))))
             (output-destination (if (null output-destination)
                                     google-translate-output-destination
                                   output-destination)))
        (cond
         ((null output-destination)
          (google-translate-buffer-output-translation gtos))
         ((equal output-destination 'echo-area)
          (google-translate-echo-area-output-translation gtos))
         ((equal output-destination 'popup)
          (google-translate-popup-output-translation gtos))
         ((equal output-destination 'kill-ring)
          (google-translate-kill-ring-output-translation gtos))
         ((equal output-destination 'current-buffer)
          (google-translate-current-buffer-output-translation gtos))
         ((equal output-destination 'help)
          (let ((describe-func
                 (function
                  (lambda (gtos)
                    (google-translate-help-buffer-output-translation gtos)))))
            (help-setup-xref (list 'google-translate-translate source-language target-language text) nil)
            (with-help-window (help-buffer)
              (funcall describe-func gtos))))
         ((equal output-destination 'paragraph-overlay)
          (google-translate-paragraph-overlay-output-translation gtos))
         ((equal output-destination 'paragraph-insert)
          (google-translate-paragraph-insert-output-translation gtos)))
        (setq google-translate-result-translation (gtos-translation gtos))
        (when google-translate-translation-to-kill-ring
          (kill-new google-translate-result-translation))))))

(defun google-translate-popup-output-translation (gtos)
  "Output translation to the popup tooltip using `popup'
package."
  (require 'popup)
  (popup-tip
   (with-temp-buffer
     (google-translate-insert-translation gtos)
     (google-translate--trim-string
      (buffer-substring (point-min) (point-max))))))

(defun google-translate-echo-area-output-translation (gtos)
  "Output translation to the echo area (See
http://www.gnu.org/software/emacs/manual/html_node/elisp/The-Echo-Area.html)"
  (message
   (with-temp-buffer
     (google-translate-insert-translation gtos)
     (google-translate--trim-string
      (buffer-substring (point-min) (point-max))))))

(defun google-translate-kill-ring-output-translation (gtos)
  "Output translation to the kill ring."
  (kill-new
   (with-temp-buffer
     (insert
      (gtos-translation gtos))
     (google-translate--trim-string
      (buffer-substring (point-min) (point-max)))))
  (message "Translated text was added to the kill ring."))

(defun google-translate-current-buffer-output-translation (gtos)
  "Output translation to current buffer."
  (insert
   (gtos-translation gtos))
  (message "Translated text was added to current buffer."))

(defun google-translate-insert-translation (gtos)
  "Insert translation to the current buffer."
  (let ((translation (gtos-translation gtos))
        (detailed-translation (gtos-detailed-translation gtos)))
    (insert
     (google-translate--translation-title gtos "%s -> %s:")
     (google-translate--translating-text gtos " %s")
     (google-translate--text-phonetic gtos " [%s]")
     " - "
     (google-translate--translated-text gtos "%s")
     (google-translate--translation-phonetic gtos " [%s]")
     (if detailed-translation
         (google-translate--detailed-translation
          detailed-translation translation
          "\n* %s " "%d. %s ")
       (google-translate--suggestion gtos)))))

(defun google-translate-buffer-output-translation (gtos)
  "Output translation to the temp buffer."
  (let ((buffer-name "*Google Translate*"))
    (with-output-to-temp-buffer buffer-name
      (if google-translate-pop-up-buffer-set-focus
          (select-window (display-buffer buffer-name))
        (set-buffer buffer-name))
      (google-translate-buffer-insert-translation gtos))))

(defun google-translate-paragraph-overlay-output-translation (gtos)
  "Output translation below the paragraph with overlay."
  (let ((start (save-excursion (start-of-paragraph-text) (point)))
        (end (save-excursion (end-of-paragraph-text) (point)))
        (below-paragraph (if (eq major-mode 'org-mode)
                             (save-excursion (previous-line) (point))
                           (save-excursion (end-of-paragraph-text) (forward-line) (point))))
        (translation (gtos-translation gtos)))
    (with-silent-modifications
      (put-text-property
       (1- below-paragraph) below-paragraph
       'display (propertize
                 (concat "\n\n" translation "\n")
                 'face `(:background ,(color-darken-name (face-background 'default) 4)))))))

(defun google-translate-paragraph-insert-output-translation (gtos)
  "Insert translation below the paragraph with overlay."
  (let ((start (save-excursion (start-of-paragraph-text) (point)))
        (end (save-excursion (end-of-paragraph-text) (point)))
        (below-paragraph (save-excursion (end-of-paragraph-text) (forward-line) (point)))
        (translation (gtos-translation gtos)))
    (goto-char below-paragraph)
    (insert (concat "\n" translation "\n"))))

(defun google-translate-help-buffer-output-translation (gtos)
  "Output translation to the help buffer."
  (and google-translate-pop-up-buffer-set-focus
       (select-window (display-buffer "*Help*")))
  (google-translate-buffer-insert-translation gtos))

(defun google-translate-buffer-insert-translation (gtos)
  "Insert translation to the current temp buffer."
  (let ((translation (gtos-translation gtos))
        (detailed-translation (gtos-detailed-translation gtos))
        (detailed-definition (gtos-detailed-definition gtos))
        (source-language (gtos-source-language gtos))
        (target-language (gtos-target-language gtos))
        (auto-detected-language (gtos-auto-detected-language gtos))
        (text (gtos-text gtos)))
    (insert
     (google-translate--translation-title gtos "Translate from %s to %s:\n")
     "\n"
     (google-translate--translating-text
      gtos
      (if (null google-translate-listen-program)
          "%s\n"
        "%s"))
     (if google-translate-listen-program
         (google-translate--listen-button
          (if (string-equal source-language "auto")
              auto-detected-language
            source-language) text) "")
     (google-translate--text-phonetic gtos "\n%s\n")
     "\n"
     (google-translate--translated-text
      gtos
      (if (null google-translate-listen-program)
          "%s\n"
        "%s"))
     (if google-translate-listen-program
         (google-translate--listen-button target-language translation) "")
     (google-translate--translation-phonetic gtos "\n%s\n")
     (if detailed-translation
         (google-translate--detailed-translation
          detailed-translation translation
          "\n%s\n" "%2d. %s\n")
       (google-translate--suggestion gtos))
     (if detailed-definition
         (google-translate--detailed-definition
          detailed-definition translation
          "\n%s\n" "%2d. %s\n")
       ""))))

(defun google-translate-read-source-language (&optional prompt)
  "Read a source language, with completion, and return its abbreviation.

The null input is equivalent to \"Detect language\"."
  (let ((completion-ignore-case t)
        (prompt
         (if (null prompt)
             "Translate from: "
           prompt)))
    (google-translate-language-abbreviation
     (google-translate-completing-read
      prompt
      (google-translate-supported-languages)
      "Detect language"))))

(defun google-translate-read-target-language (&optional prompt)
  "Read a target language, with completion, and return its abbreviation.

The input is guaranteed to be non-null."
  (let ((completion-ignore-case t)
        (prompt
         (if (null prompt)
             "Translate to: "
           prompt)))
    (cl-flet ((read-language ()
                             (google-translate-completing-read
                              prompt
                              (google-translate-supported-languages))))
      (let ((target-language (read-language)))
        (while (string-equal target-language "")
          (setq target-language (read-language)))
        (google-translate-language-abbreviation target-language)))))

(defun google-translate-completing-read (prompt choices &optional def)
  "Read a string in the minibuffer with completion.

If `google-translate-enable-ido-completion' is non-NIL, use
ido-style completion."
  (funcall (if google-translate-enable-ido-completion
               #'ido-completing-read
             #'completing-read)
           prompt choices nil t nil nil def))

(provide 'google-translate-core-ui)

;;; google-translate-core-ui.el ends here
