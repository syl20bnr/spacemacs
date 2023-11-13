; greader-espeak.el
;; Copyright (C) 2017-2023  Free Software Foundation, Inc.
(defgroup greader-espeak
  nil
  "Back-end of espeak for greader."
  :group 'greader
  )

(defcustom greader-espeak-language "en"
  "Specifies the language of this back-end.
For a comprehensive list of languages and voices available in espeak
type in a terminal: espeak --list-languages"
  :tag "greader espeak language"
  :type 'string)

(defcustom greader-espeak-rate 200
  "Specifies the rate os speech in words per minute."
  :tag "greader espeak rate"
  :type 'integer)

(defcustom greader-espeak-executable-name "espeak"
  "File name of espeak executable.
this variable determines authomatically if espeak is present in your PATH environment, then if this variable is nil, it means that you must first install espeak."
  :tag "espeak executable"
  :type 'string)

(defcustom greader-espeak-punctuation nil
  "Espeak punctuation switch."
  :tag "espeak punctuation"
  :type 'boolean)

(defun greader-espeak-set-rate (&optional rate)
  "Return a string suitable for setting espeak RATE."
  (if (not rate)
      (concat "-s" (number-to-string greader-espeak-rate))
    (progn
      (setq-local greader-espeak-rate rate)
      (concat "-s" (number-to-string rate)))))

(defun greader-espeak-set-language (&optional lang)
  "Return a suitable string for espeak language.
LANG must be recognized by espeak or espeak-ng."
  (if (not lang)
      (concat "-v" greader-espeak-language)
    (progn
      (setq-local greader-espeak-language lang)
      (concat "-v " lang))))
;;;###autoload
(defun greader-espeak (command &optional arg &rest _)
  "Back-end main function of greader-espeak.
COMMAND must be a string suitable for `make-process'."
  (pcase command
    ('executable
     greader-espeak-executable-name)
    ('lang
     (greader-espeak-set-language arg))
    ('rate
     (cond
      ((equal arg 'value)
       greader-espeak-rate)
      (t
       (greader-espeak-set-rate arg))))
    ('punctuation
     (pcase arg
       ('yes
	(setq-local greader-espeak-punctuation t)
	"--punct")
       ('no
	(setq-local greader-espeak-punctuation nil)
	nil)
       ('nil
	(if greader-espeak-punctuation
	    "--punct"
	  nil))))
    ('get-language
     greader-espeak-language)
    (_
     'not-implemented)))
(put 'greader-espeak 'greader-backend-name "greader-espeak")

(provide 'greader-espeak)
