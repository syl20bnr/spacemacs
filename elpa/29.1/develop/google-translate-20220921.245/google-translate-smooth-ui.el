;;; google-translate-smooth-ui.el --- Just another UI to Google
;;; Translate package

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

;; `google-translate-smooth-ui' is a just alternative to the default
;; `google-translate-default-ui'. It was written with mind to provide
;; impoved user interface and, especially, to achieve better
;; supporting of many default languages. `google-translate-default-ui'
;; supports two default languages very well but there is no space for
;; the third one.
;;
;; Invoking the function `google-translate-smooth-translate' queries
;; text and (optionally) the source and target languages to translate,
;; and shows a buffer with available translations of the text.

;; Installation:

;; Assuming that the files `google-translate.el',
;; `google-translate-core.el', `google-translate-core-ui.el' and
;; `google-translate-smooth-ui.el' are somewhere on the load path, add
;; the following lines to your .emacs file:
;;
;;   (require 'google-translate-smooth-ui)
;;   (global-set-key "\C-ct" 'google-translate-smooth-translate)
;;
;; Change the key bindings to your liking.
;;
;; Configuration:
;;
;; `google-translate-translation-directions-alist' alist is intended
;; to contain translation directions.
;;
;; For example it could be defined (in your .emacs or init.el) as:
;;
;; (setq google-translate-translation-directions-alist '(("en" . "ru"))
;;
;; in this way one translation direction ("en" > "ru") is defined and
;; when `google-translate-smooth-translate' function executes it will
;; output the prompt (in minibuffer) which will looks like as the
;; following:
;;
;; [English > Russian] Translate:
;;
;; You may set as many translation directions as you would like
;; to. For example such piece of code will define four translation
;; directions:
;;
;; (setq google-translate-translation-directions-alist
;;       '(("de" . "en") ("en" . "de") ("de" . "fr") ("fr" . "de")))
;;
;; in this way, when `google-translate-smooth-translate' function
;; executes you'll be queried by the prompt which will looks like the
;; following:
;;
;; [German > English] Translate:
;;
;; and, also in this way, you'll be able to switch between different
;; translation directions directly from minibuffer by using "C-n" and
;; "C-p" key bindings. "C-n" key binding changes current translation
;; direction to the next direction defined in the
;; `google-translate-translation-directions-alist' variable. And "C-p"
;; key binding changes current translation direction to the previous
;; one. Thus, while executing `google-translate-smooth-translate'
;; function and having in minibuffer such prompt:
;;
;; [German > English] Translate:
;;
;; then after pressing "C-n" you'll get the following prompt:
;;
;; [English > German] Translate:
;;
;; By default `google-translate-translation-directions-alist' is empty
;; and thus during execution of `google-translate-smooth-translate'
;; you'll be queried (to input a text) by the prompt:
;;
;; Translate:
;;
;; And after inputed text you'll be queried also for the source and
;; target languages. To let the package to be known which languages
;; you would like to always use and to avoid repetitive language
;; quering it is reasonable to define them in the mentioned
;; `google-translate-translation-directions-alist' variable.

;; Customization:

;; `google-translate-smooth-ui' doesn't contain any customizable
;; variables. But `google-translate-smooth-ui' extends
;; `google-translate-core-ui' and thus it could be customized via this
;; package's variables. Please read documentation for the
;; `google-translate-core-ui' package.
;;

;;; Code:


(require 'google-translate-core-ui)


(defgroup google-translate-smooth-ui nil
  "Just Another UI for Google Translate package."
  :group 'processes)

(defvar google-translate-translation-directions-alist
  '()
  "Alist of translation directions. Each of direction could be
selected directly in the minibuffer during translation.

Each element is a cons-cell of the form (SOURCE_CODE
. TARGET_CODE), where SOURCE_CODE is a source language code and
TARGET_CODE is a target language code.

Language codes are defined in
`google-translate-supported-languages-alist' variable.

As example, this alist could looks like the following:

  '((\"en\" . \"ru\")
    (\"ru\" . \"en\")
    (\"uk\" . \"ru\")
    (\"ru\" . \"uk\"))")

(defvar google-translate-current-translation-direction 0
  "Points to nth element of
`google-translate-translation-directions-alist' variable and
keeps current translation direction while changing translation
directions.")

(defvar google-translate-last-translation-direction nil
  "The last used translation direction.
Points to nth element of `google-translate-translation-directions-alist' variable.")

(defvar google-translate-translation-direction-query ""
  "Temporal variable which keeps a minibuffer text while
switching translation directions.")

(defvar google-translate-try-other-direction nil
  "Indicates that other translation direction is going to be
used.")

(defvar google-translate-minibuffer-keymap nil
  "Keymap for minibuffer for changing translation directions.")

(defun google-translate-change-translation-direction (direction)
  "Change translation direction. If DIRECTION is 'next then
change current direction by the next one. Otherwise change it to
the previous one."
  (let ((current google-translate-current-translation-direction)
        (length (length google-translate-translation-directions-alist)))
    (setq current
          (if (equal direction 'next)
              (+ current 1)
            (- current 1)))
    (when (< current 0)
      (setq current (- length 1)))
    (when (> current (- length 1))
      (setq current 0))
    (setq google-translate-current-translation-direction current)
    (setq google-translate-translation-direction-query
          (minibuffer-contents))))

(defun google-translate-next-translation-direction ()
  "Switch to the next translation direction. If current direction
is the last in the list of existing directions then switch to the
first one."
  (interactive)
  (google-translate-change-translation-direction 'next)
  (setq google-translate-try-other-direction t)
  (exit-minibuffer))

(defun google-translate-previous-translation-direction ()
  "Switch to the previous translation direction. If current
direction is the first in the list of existing directions then
switch to the last one."
  (interactive)
  (google-translate-change-translation-direction 'previous)
  (setq google-translate-try-other-direction t)
  (exit-minibuffer))

(defun google-translate-query-translate-using-directions ()
  "Tranlate query using translation directions described by
`google-translate-translation-directions-alist' variable.

This function allows to select desired translation direction
directly in the minibuffer while translating a word or a
sentence.

This function defines two key bindings for the minibuffer which
allow to select direction:
C-p - to select previous direction,
C-n - to select next direction."
  (interactive)
  (let ((text ""))
    (setq google-translate-try-other-direction nil)
    (setq text
          (if google-translate-input-method-auto-toggling
              (minibuffer-with-setup-hook
                  (lambda ()
                    (google-translate-setup-preferable-input-method
                     (google-translate--current-direction-source-language)))
                'google-translate-setup-preferable-input-method
                (google-translate--read-from-minibuffer))
            (google-translate--read-from-minibuffer)))
    (if google-translate-try-other-direction
        (call-interactively 'google-translate-query-translate-using-directions)
      text)))

(defun google-translate--setup-minibuffer-keymap ()
  "Setup additional key bindings for minibuffer."
  (unless google-translate-minibuffer-keymap
    (setq google-translate-minibuffer-keymap
          (let ((map (make-sparse-keymap)))
            (define-key map "\C-p" 'google-translate-previous-translation-direction)
            (define-key map "\C-n" 'google-translate-next-translation-direction)
            (define-key map "\C-l" 'google-translate-clear-minibuffer)
            (set-keymap-parent map minibuffer-local-map)
            map))))

(defun google-translate-clear-minibuffer ()
  "Delete minibuffer contents."
  (interactive)
  (delete-minibuffer-contents))

(defun google-translate--read-from-minibuffer ()
  "Read string from minibuffer."
  (let* ((source-language
          (google-translate--current-direction-source-language))
         (target-language
          (google-translate--current-direction-target-language))
         (prompt (if (or (null source-language)
                         (null target-language))
                     "Translate: "
                   (format "[%s > %s] Translate: "
                           (google-translate-language-display-name source-language)
                           (google-translate-language-display-name target-language)))))
    (google-translate--setup-minibuffer-keymap)
    (read-from-minibuffer
     prompt
     google-translate-translation-direction-query
     google-translate-minibuffer-keymap nil nil
     google-translate-translation-direction-query t)))

(defun google-translate--current-direction-source-language ()
  "Retrieve source language from the current translation
direction."
  (car (nth google-translate-current-translation-direction
            google-translate-translation-directions-alist)))

(defun google-translate--current-direction-target-language ()
  "Retrieve target language from the current translation
direction."
  (cdr (nth google-translate-current-translation-direction
            google-translate-translation-directions-alist)))

;;;###autoload
(defun google-translate-smooth-translate ()
  "Translate a text using translation directions.

Make a prompt in minibuffer for a text to translate. Default text
is word at point.

In case of `google-translate-translation-directions-alist' is
empty list then after inputed translating text prompts for source
language and then for target languages.

In case of `google-translate-translation-directions-alist' is not
empty list takes current translation direction and makes
appropriate translation. Current translation direction indicates
in the minibuffers' prompt.

A current translation direction could be changed directly in the
minibuffer by means of key bindings such as C-n and C-p for
changing to the next translation direction and to the previous
one respectively."
  (interactive)

  (setq google-translate-translation-direction-query
        (if (use-region-p)
            (google-translate--strip-string
             (buffer-substring-no-properties (region-beginning) (region-end)))
          (unless current-prefix-arg
            (current-word t t))))

  (setq google-translate-current-translation-direction
        (or google-translate-last-translation-direction 0))

  (let* ((text (google-translate-query-translate-using-directions))
         (source-language (google-translate--current-direction-source-language))
         (target-language (google-translate--current-direction-target-language)))
    (when (null source-language)
      (setq source-language (google-translate-read-source-language)))
    (when (null target-language)
      (setq target-language (google-translate-read-target-language)))
    (google-translate-translate source-language target-language text)
    (setq google-translate-last-translation-direction
          google-translate-current-translation-direction)))


(provide 'google-translate-smooth-ui)

;;; google-translate-smooth-ui.el ends here
