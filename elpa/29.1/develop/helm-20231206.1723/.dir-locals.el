;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((bug-reference-bug-regexp . "\\(\\b\\(?:[Ii]ssue ?#?\\|[Bb]ug ?#?\\|[Pp]atch ?#\\|RFE ?#\\|PR [a-z+-]+/\\)\\([0-9]+\\(?:#[0-9]+\\)?\\)\\)")
         (bug-reference-url-format . "https://github.com/emacs-helm/helm/issues/%s")
         (byte-compile-warnings . (not obsolete docstrings docstrings-non-ascii-quotes))))
 (emacs-lisp-mode . ((mode . bug-reference-prog)
                     (indent-tabs-mode . nil)
                     (fill-column . 80))))
