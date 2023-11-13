;;; google-translate-backend.el --- Backend methods for url retrieve.

;; Copyright (C) 2019 Tomotaka SUWA <tomotaka.suwa@gmail.com>

;; Author: Oleksandr Manzyuk <manzyuk@gmail.com>
;; Maintainer: Andrey Tykhonov <atykhonov@gmail.com>
;; URL: https://github.com/atykhonov/google-translate
;; Version: 0.11.17
;; Keywords: convenience

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

;; Provide backend facilities to cope with google translate.

;;; Code:

(defvar google-translate-backend-method 'emacs
  "The backend method for URL retrieve.

Valid symbols are below:

 - emacs: use built in `url-retrieve-synchronously'
 - curl: invoke curl command
 - wget: invoke wget command

and any other keys of `google-translate-backend-commands'.")

(defvar google-translate-backend-user-agent "Emacs"
  "The user agent string for HTTP request header.")

(defvar google-translate-backend-commands
  '((curl :args ("-s" "-L" "-A"))
    (wget :args ("-q" "-O" "-" "-U")))
  "An alist of external program specifications.

The form of each element is (KEY P-LIST).  The variable
`google-translate-backend-method' may have one of KEYs and is
used for determine the command to execute.  The P-LIST of each
element represents command specific information.

Available properties:

 - Property `:name': the program name(optional)
 - Property `:args': a list of arguments passed to the program

If you omit the `:name' property, (symbol-name KEY) will be used
as the program name.  The `:args' property must be composed to
satisfy all the following conditions:

 - Output content to standard output
 - Suppress non-content(HTTP headers, progress messages, etc)
 - Handle location response header
 - Place User-Agent option at last

So if you would like to use another program \"foo\", for example:

\(push \\='(foo :name \"foo-x86\"
             :args (\"-q\" \"--agent\"))
       google-translate-backend-commands)

\(setq google-translate-backend-method \\='foo)

And the command line looks like:

foo-x86 -q --agent ['google-translate-backend-user-agent] [URL]")

(defvar google-translate-backend-debug nil
  "Non-nil means log http activities to the *google-translate-debug* buffer.")

(defun google-translate-backend--log (&rest args)
  "Log http activities to the *google-translate-debug* buffer along with ARGS.

Disabled if `google-translate-backend-debug' is nil."
  (when google-translate-backend-debug
    (let ((message (mapconcat 'identity
                              (list (current-time-string)
                                    (prin1-to-string args)
                                    "-- begin --"
                                    (buffer-string)
                                    "-- end --")
                              "\n")))
      (with-current-buffer
          (get-buffer-create "*google-translate-backend-debug*")
        (goto-char (point-max))
        (insert message)
        (newline)))))

(defun google-translate-backend--emacs (url)
  "Get URL contents by `url-retrieve-synchronously'."
  (insert
   (let ((url-user-agent google-translate-backend-user-agent))
     (with-current-buffer (url-retrieve-synchronously url nil nil 15)
       (set-buffer-multibyte t)
       (google-translate-backend--log url 'emacs)
       (goto-char (point-min))
       (re-search-forward "\n\n")
       (prog1 (buffer-substring (point)
                                (point-max))
         (kill-buffer))))))

(defun google-translate-backend--process (url key spec)
  "Get URL contents by `call-process'.

\(KEY SPEC) would be exist in `google-translate-backend-commands'."
  (let ((name (or (plist-get spec :name)
                  (symbol-name key)))
        (args (plist-get spec :args))
        (agent google-translate-backend-user-agent))
    (apply 'call-process
           (append (list name nil t nil)
                   args
                   (list agent url)))
    (google-translate-backend--log url key spec)))

(defun google-translate-backend-retrieve (url)
  "Get URL contents via `google-translate-backend-method'."
  (let ((method google-translate-backend-method))
    (if (eq method 'emacs)
        (google-translate-backend--emacs url)
      (let ((spec (cdr (assq method
                             google-translate-backend-commands))))
        (if (null spec)
            (error "Unknown backend method: %s" method)
          (google-translate-backend--process url method spec))))))

(provide 'google-translate-backend)
;;; google-translate-backend.el ends here
