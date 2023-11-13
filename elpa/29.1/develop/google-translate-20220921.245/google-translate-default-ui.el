;;; google-translate-default-ui.el --- default UI for Google Translate
;;; package

;; Copyright (C) 2012 Oleksandr Manzyuk <manzyuk@gmail.com>

;; Author: Oleksandr Manzyuk <manzyuk@gmail.com>
;; Maintainer: Andrey Tykhonov <atykhonov@gmail.com>
;; URL: https://github.com/atykhonov/google-translate
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
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file provides default UI for the Google Translate package. It
;; was originally written by Oleksandr Manzyuk and was part of
;; google-translate.el. It was extracted to this,
;; google-translate-default-ui.el file due to refactoring (the goal of
;; which is to separate backend from UI and provide better way for
;; having different UIs for Google Translate package).
;;
;; Invoking the function `google-translate-query-translate' queries the source
;; and target languages and text to translate, and shows a buffer with
;; available translations of the text.  Invoking the function
;; `google-translate-at-point' translates the word at point or the active
;; region.

;; Customization:

;; You can customize the following variables:
;;;;
;; - `google-translate-default-source-language'
;;
;; - `google-translate-default-target-language'

;; If the variable `google-translate-default-source-language' is set
;; to a non-NIL value, the source language won't be queried and that
;; value will be used instead.  Analogously, if you set the variable
;; `google-translate-default-target-language' to some non-NIL value,
;; that value will be used without querying.

;; You can always override this behavior by supplying a `C-u' prefix
;; argument to the function `google-translate-query-translate'.

;; Here is an example.  Suppose that your native language is Russian
;; and you frequently need to translate from various languages to
;; Russian.  Then it is reasonable
;;
;; - to set the variable `google-translate-default-target-language'
;;   to "ru", and
;;
;; - to leave `google-translate-default-source-language' set to its
;;   default value, NIL.
;;
;; In this case, the function `google-translate-query-translate' is
;; only going to query the source language and text to translate.
;; If you need to translate to some language other than Russian, you
;; can override the default for the target language by supplying a
;; `C-u' prefix argument, in which case you will be queried for both
;; the source and target languages, as well as text to translate.

;; If you frequently translate from some fixed language, it is also
;; reasonable to set `google-translate-default-source-language' to
;; an appropriate value.
;;
;; If you have both the default source and target languages specified,
;; you may like to bind functions `google-translate-at-point-reverse'
;; and `google-translate-query-translate-reverse' to some keys, e.g.:
;;
;;   (global-set-key (kbd "C-c r") 'google-translate-at-point-reverse)
;;   (global-set-key (kbd "C-c R") 'google-translate-query-translate-reverse)
;;
;; This will allow you to quickly translate in the reverse direction.
;; When the default source (resp. target) language is not set, the
;; target (resp. source) language of the reverse translation will be
;; queried interactively.

;; The admitted values of `google-translate-default-source-language'
;; and `google-translate-default-target-language' are the codes of the
;; languages supported by Google Translate (like "ru" for Russian
;; above).  See `google-translate-supported-languages' for the list of
;; the supported languages, or customize the defaults using the
;; customization mechanism of Emacs.  Setting a default language to
;; NIL means that language will always be queried.  Moreover, the
;; variable `google-translate-default-source-language' can be set to a
;; special value "auto" that is interpreted as the instruction for
;; Google Translate to detect the source language.  This option is
;; also available when you are queried for the source language: simply
;; leave this parameter blank by pressing RET.  (If you have enabled
;; the ido-style completion, "Detect language" is going to be the
;; first option, which you can select simply by hitting RET.)
;;

;;; Code:


(require 'google-translate-core-ui)


(defgroup google-translate-default-ui nil
  "Default UI interface to the Google Translate package."
  :group 'processes)

(defcustom google-translate-default-source-language nil
  "Default source language.

A string designating a language supported by Google Translate.
Set this variable to NIL (the default value) if you want to
always be queried for the source language, or to \"auto\" if you
want Google Translate to always detect the source language.

See the variable `google-translate-supported-languages-alist' for
the list of available languages."
  :group 'google-translate-manzyuk-ui
  :type  `(radio ,@(mapcar #'(lambda (lang)
                               `(const :tag ,(car lang) ,(cdr lang)))
                           google-translate-supported-languages-alist)
                 (const :tag "Detect language" "auto")
                 (other :tag "Always ask" nil)))

(defcustom google-translate-default-target-language nil
  "Default target language.

A string designating a language supported by Google Translate.
Set this variable to NIL (the default value) if you want to
always be queried for the target language.

See the variable `google-translate-supported-languages-alist' for
the list of available languages."
  :group 'google-translate-manzyuk-ui
  :type  `(radio ,@(mapcar #'(lambda (lang)
                               `(const :tag ,(car lang) ,(cdr lang)))
                           google-translate-supported-languages-alist)
                 (other :tag "Always ask" nil)))

(defun google-translate-read-args (override-p reverse-p)
  "Query and return the language arguments of `google-translate-translate'.

When OVERRIDE-P is NIL, the source (resp. target) language is queried
only if the variable `google-translate-default-source-language' (resp.
`google-translate-default-target-language') is NIL.  If OVERRIDE-P is
non-NIL, both the source and target languages are queried, allowing
one to override the defaults if they are specified.

REVERSE-P is used to reverse the default direction of translation: if
it's non-NIL, the value of `google-translate-default-source-language'
becomes the default target language and vice versa."
  (let* ((default-source-language
           (if reverse-p
               google-translate-default-target-language
             google-translate-default-source-language))
         (default-target-language
           (if reverse-p
               google-translate-default-source-language
             google-translate-default-target-language))
         (source-language
          (if (and default-source-language
                   (not override-p))
              default-source-language
            (google-translate-read-source-language
             "Translate from: ")))
         (target-language
          (if (and default-target-language
                   (not override-p))
              default-target-language
            (google-translate-read-target-language
             (format "Translate from %s to: "
                     (google-translate-language-display-name
                      source-language))))))
    (list source-language target-language)))

(defun %google-translate-query-translate (override-p reverse-p)
  (let* ((langs (google-translate-read-args override-p reverse-p))
         (source-language (car langs))
         (target-language (cadr langs)))
    (google-translate-translate
     source-language target-language
     (if google-translate-input-method-auto-toggling
         (minibuffer-with-setup-hook
             (lambda ()
               (google-translate-setup-preferable-input-method source-language))
           (%google-translate-default-ui-read-from-minibuffer source-language target-language))
       (%google-translate-default-ui-read-from-minibuffer source-language target-language)))))

(defun %google-translate-default-ui-read-from-minibuffer (source-language target-language)
  (read-from-minibuffer
   (format "Translate from %s to %s: "
           (google-translate-language-display-name source-language)
           (google-translate-language-display-name target-language))))

;;;###autoload
(defun google-translate-query-translate (&optional override-p)
  "Interactively translate text with Google Translate.

Query a text (a word or a phrase), and pop up a buffer named *Google
Translate* displaying available translations of the text.

If no defaults for the source and target languages are specified (by
setting the variables `google-translate-default-source-language' and
`google-translate-default-target-language'), interactively query the
missing parts.  For example, a reasonable option may be to specify a
default for the target language and always be queried for the source
language.

With a `C-u' prefix argument, query the source and target languages,
even if any defaults are specified.  For example, you may frequently
need to translate from English to Russian, and you may choose to set
the default source and target languages to \"en\" and  \"ru\", resp.
However, occasionally you may also need to translate from Russian to
English.  With a `C-u' prefix argument you can override the defaults
and specify the source and target languages explicitly.

The languages are queried with completion, and the null input at the
source language prompt is considered as an instruction for Google
Translate to detect the source language."
  (interactive "P")
  (%google-translate-query-translate override-p nil))

;;;###autoload
(defun google-translate-query-translate-reverse (&optional override-p)
  "Like `google-translate-query-translate', but performs translation
in the reverse direction.

The value of the variable `google-translate-default-source-language'
\(if set) becomes the target language, and the value of the variable
`google-translate-default-target-language' (if also set) becomes the
source language.

In particular, when both variables are set, translation is performed
in the reverse direction."
  (interactive "P")
  (%google-translate-query-translate override-p t))

(defun %google-translate-at-point (override-p reverse-p)
  (let* ((langs (google-translate-read-args override-p reverse-p))
         (source-language (car langs))
         (target-language (cadr langs))
         (bounds nil))
    (google-translate-translate
     source-language target-language
     (cond ((string-equal major-mode "pdf-view-mode") (car (pdf-view-active-region-text)))
           ((use-region-p) (buffer-substring-no-properties (region-beginning) (region-end)))
       (t (or (and (setq bounds (bounds-of-thing-at-point 'word))
                (buffer-substring-no-properties (car bounds) (cdr bounds)))
           (error "No word at point.")))))))

;;;###autoload
(defun google-translate-at-point (&optional override-p)
  "Translate the word at point or the words in the active region.

For the meaning of OVERRIDE-P, see `google-translate-query-translate'."
  (interactive "P")
  (%google-translate-at-point override-p nil))

;;;###autoload
(defun google-translate-at-point-reverse (&optional override-p)
  "Like `google-translate-at-point', but performs translation in the
reverse direction."
  (interactive "P")
  (%google-translate-at-point override-p t))

;;;###autoload
(defun google-translate-buffer (&optional override-p reverse-p)
  "Translate current buffer.

For the meaning of OVERRIDE-P, see `google-translate-query-translate'."
  (interactive "P")
  (if (string-equal major-mode "pdf-view-mode")
      (message "In PDF, select region and use google-translate-at-point")
    (let* ((langs (google-translate-read-args override-p reverse-p))
           (source-language (car langs))
           (target-language (cadr langs)))
      (google-translate-translate
       source-language target-language
       (if (use-region-p)
           (buffer-substring-no-properties (region-beginning) (region-end))
         (or (buffer-substring-no-properties (point-min) (point-max))
             (error "Translate current buffer error.")))))))

;;;###autoload
(defun google-translate-paragraphs-overlay (&optional override-p reverse-p)
  "Translate current buffer with paragraph by paragraph and SHOW results in overlay below paragraph.
This command also specificly support org-mode."
  (interactive "P")
  (let* ((langs (google-translate-read-args override-p reverse-p))
         (source-language (car langs))
         (target-language (cadr langs))
         (last-paragraph-begin 1))
    (goto-char (point-min))
    (while (not (equal (point) (point-max))) ; reached end of buffer
      (if (eq major-mode 'org-mode)
          (if (eq (car (org-element-at-point)) 'paragraph)
              (progn
                (org-mark-element)
                (exchange-point-and-mark)
                (unless (= last-paragraph-begin (region-beginning))
                  (google-translate-translate
                   source-language target-language
                   (buffer-substring-no-properties (region-beginning) (region-end))
                   'paragraph-overlay)
                  (setq last-paragraph-begin (region-beginning)))
                (deactivate-mark))
            (forward-line 2))
        (google-translate-translate
         source-language target-language
         (save-excursion
           (mark-paragraph)
           (buffer-substring-no-properties (region-beginning) (region-end)))
         'paragraph-overlay)
        (deactivate-mark)
        (forward-paragraph)
        (forward-line)))))

;;;###autoload
(defun google-translate-paragraphs-insert (&optional override-p reverse-p)
  "Translate current buffer with paragraph by paragraph and INSERT results below paragraph.
This command does NOT support document format like org-mode."
  (interactive "P")
  (let* ((langs (google-translate-read-args override-p reverse-p))
         (source-language (car langs))
         (target-language (cadr langs)))
    (goto-char (point-min))
    (while (not (equal (point) (point-max))) ; reached end of buffer
      (google-translate-translate
       source-language target-language
       (save-excursion
         (mark-paragraph)
         (buffer-substring-no-properties (region-beginning) (region-end)))
       'paragraph-insert)
      (deactivate-mark)
      (forward-line))))

(provide 'google-translate-default-ui)


;;; google-translate-default-ui.el ends here
