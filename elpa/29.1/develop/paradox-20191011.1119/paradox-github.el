;;; paradox-github.el --- interacting with the Github API -*- lexical-binding:t -*-

;; Copyright (C) 2014-2015 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; Prefix: paradox
;; Separator: -

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;


;;; Code:
(require 'cl-lib)
(require 'package)
(require 'paradox-core)

(defgroup paradox-github nil
  "Paradox Github configurations."
  :prefix "paradox-"
  :package-version '(paradox . "2.0")
  :group 'paradox)

(defvar paradox--user-starred-list nil)
(make-obsolete-variable
 'paradox--user-starred-list 'paradox--user-starred-repos "2.1")
(defvar paradox--user-starred-repos (make-hash-table))


;;; Github token
(defcustom paradox-github-token nil
  "Access token to use for github actions.
Currently, that means (un)starring repos.

To generate an access token:
  1. Visit the page https://github.com/settings/tokens/new?scopes=public_repo&description=Paradox
     and login to github (if asked).
  2. Click on \"Generate Token\", copy the generated token, and
     save it to this variable by writing
         (setq paradox-github-token TOKEN)
     somewhere in your configuration and evaluating it (or just
     restart emacs).

This is similar to how erc or jabber handle authentication in
emacs, but the following disclaimer always worth reminding.

DISCLAIMER
When you save this variable, DON'T WRITE IT ANYWHERE PUBLIC. This
token grants (very) limited access to your account.
END DISCLAIMER

One way to make this variable safer is to set it from your
authinfo.gpg file. See this issue for instructions on how to do that:
https://github.com/Malabarba/paradox/issues/147#issuecomment-409336111

Paradox will ask you whether you want github integration the
first time you start it. If you answer \"no\", it will remember
your choice via `customize-save-variable'. You can do this
manually by setting this variable to t. Setting it to nil means
it hasn't been configured yet."
  :type '(choice (string :tag "Token")
                 (const :tag "Disable" t)
                 (const :tag "Ask me next time" nil))
  :group 'paradox-github
  :package-version '(paradox . "0.2"))

(defcustom paradox-automatically-star 'unconfigured
  "When you install new packages, should they be automatically starred?
This variable has no effect if `paradox-github-token' isn't set
to a string.

Paradox is capable of automatically starring packages when you
install them, and unstarring when you delete them. This only
applies to actual installation/deletion, i.e. Paradox doesn't
auto (un)star packages that were simply upgraded.

If this variable is nil, this behaviour is disabled. \\<paradox-menu-mode-map>

On the Package Menu, you can always manually star packages with \\[paradox-menu-mark-star-unstar]."
  :type '(choice (const :tag "Yes" t)
                 (const :tag "No" nil)
                 (const :tag "Ask later" unconfigured))
  :group 'paradox-github
  :package-version '(paradox . "0.2"))

(defmacro paradox--enforce-github-token (&rest forms)
  "If a token is defined, perform FORMS, otherwise ignore forms ask for it be defined."
  `(if (stringp paradox-github-token)
       (progn ,@forms)
     (setq paradox-github-token nil)
     (paradox--check-github-token)))

(defun paradox--check-github-token ()
  "Check that the user has either set or refused the github token.
If neither has happened, ask the user now whether he'd like to
configure or refuse the token."
  (if (stringp paradox-github-token) t
    (if paradox-github-token
        t
      (if (not (y-or-n-p "Would you like to set up GitHub integration?
This will allow you to star/unstar packages from the Package Menu. "))
          (customize-save-variable 'paradox-github-token t)
        (describe-variable 'paradox-github-token)
        (when (get-buffer "*Help*")
          (switch-to-buffer "*Help*")
          (delete-other-windows))
        (if (y-or-n-p "Follow the instructions on the `paradox-github-token' variable.
May I take you to the token generation page? ")
            (browse-url "https://github.com/settings/tokens/new?scopes=public_repo&description=Paradox"))
        (message "Once you're finished, simply call `paradox-list-packages' again.")
        nil))))


;;; Starring
(defun paradox-star-all-installed-packages ()
  "Star all of your currently installed packages.
No questions asked."
  (interactive)
  (paradox--enforce-github-token
   (mapc (lambda (x) (paradox--star-package-safe (car-safe x))) package-alist)))

(defun paradox--starred-repo-p (repo)
  "Non-nil if REPO is starred by the user."
  (gethash repo paradox--user-starred-repos))

(defun paradox--star-package-safe (pkg &optional delete query)
  "Star PKG without throwing errors, unless DELETE is non-nil, then unstar.
If QUERY is non-nil, ask the user first."
  (let ((repo (gethash pkg paradox--package-repo-list)))
    (when (and repo (paradox--starred-repo-p repo))
      (paradox--star-repo repo delete query))))

(defun paradox--star-repo (repo &optional delete query)
  "Star REPO, unless DELETE is non-nil, then unstar.
If QUERY is non-nil, ask the user first.

Throws error if repo is malformed."
  (when (or (not query)
            (y-or-n-p (format "Really %sstar %s? "
                              (if delete "un" "") repo)))
    (paradox--github-action-star repo delete)
    (message "%starred %s." (if delete "Uns" "S") repo)
    (if delete
        (remhash repo paradox--user-starred-repos)
      (puthash repo t paradox--user-starred-repos))))

(defun paradox--unstar-repo (repo &optional delete query)
  "Unstar REPO.
Calls (paradox--star-repo REPO (not DELETE) QUERY)."
  (paradox--star-repo repo (not delete) query))

(defun paradox--full-name-reader ()
  "Return all \"full_name\" properties in the buffer.
Much faster than `json-read'."
  (let (out)
    (while (search-forward-regexp
            "^ *\"full_name\" *: *\"\\(.*\\)\", *$" nil t)
      (push (match-string-no-properties 1) out))
    (goto-char (point-max))
    out))

(defun paradox--refresh-user-starred-list (&optional async)
  "Fetch the user's list of starred repos."
  (paradox--github-action "user/starred?per_page=100"
    :async    (when async 'refresh)
    :callback (lambda (res)
                (setq paradox--user-starred-repos
                      (make-hash-table :size (length res)
                                       :test #'equal))
                (dolist (it res)
                  (puthash it t paradox--user-starred-repos)))
    :reader   #'paradox--full-name-reader))

(defun paradox--github-action-star (repo &optional delete)
  "Call `paradox--github-action' with \"user/starred/REPO\" as the action.
DELETE and NO-RESULT are passed on."
  (paradox--github-action (concat "user/starred/" repo)
    :async t
    :method (if (stringp delete) delete
              (if delete "DELETE" "PUT"))))


;;; The Base (generic) function
(defun paradox--github-report (&rest text)
  "Write TEXT to the *Paradox Github* buffer."
  (with-current-buffer (get-buffer-create "*Paradox Report*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (apply #'insert text))
    (goto-char (point-min))))

(defun paradox--github-error (format &rest args)
  "Throw an error using FORMAT and ARGS.
Also print contents of current buffer to *Paradox Github*."
  (declare (indent 1))
  (paradox--github-report (buffer-string))
  (apply #'error
         (concat format "  See *Paradox Github* buffer for the full result")
         args))

(defvar paradox--github-errors-to-ignore nil
  "List of numbers to ignore when parsing the HTML return code.
`paradox--github-parse-response-code' normally returns nil on
200, t on 204, and emits messages or errors on other values.
Adding values to this list makes them be treated as a 200.")

(defun paradox--github-parse-response-code ()
  "Non-nil if this reponse buffer looks ok.
Leave point at the return code on the first line."
  (goto-char (point-min))
  (unless (search-forward " " nil t)
    (paradox--github-report (buffer-string))
    (error "Tried contacting Github, but I can't understand the result.  See *Paradox Github* buffer for the full result"))
  (pcase (thing-at-point 'number)
    ((pred (lambda (n) (member n paradox--github-errors-to-ignore))) nil)
    (`204 nil) ;; OK, but no content.
    (`200 t)   ;; OK, with content.
    ;; I'll implement redirection if anyone ever reports this.
    ;; For now, I haven't found a place where it's used.
    ((or `301 `302 `303 `304 `305 `306 `307)
     (paradox--github-report "Redirect received:\n\n" (buffer-string))
     ;; (message "Received a redirect reply, please file a bug report (M-x `paradox-bug-report')")
     nil)
    ((or `404) ;; Not found.
     (paradox--github-report (buffer-string))
     (message "This repo doesn't seem to exist, Github replied with: %s"
              (substring (thing-at-point 'line) 0 -1))
     nil)
    ((or `403) ;; Forbidden
     (paradox--github-error
         "Github wouldn't let me do this - does your token have the right permissions? They're here: https://github.com/settings/tokens"))
    ((or `400 `422) ;; Bad request.
     (paradox--github-error
         "Github didn't understand my request, please file a bug report (M-x `paradox-bug-report')"))
    (`401 (paradox--github-error
              (if (stringp paradox-github-token)
                  "Github says you're not authenticated, try creating a new Github token"
                "Github says you're not authenticated, you need to configure `paradox-github-token'")))
    (_ (paradox--github-error "Github returned: %S"
         (substring (thing-at-point 'line) 0 -1)))))

(defvar paradox--github-next-page nil)

(defun paradox--https-proxy ()
  "Get https proxy if url-proxy-services has been defined."
  (if (and (boundp 'url-proxy-services)
           (assoc "https" url-proxy-services))
      (cdr (assoc "https" url-proxy-services))
    ""))

(defmacro paradox--with-github-buffer (method action async unwind-form
                                              &rest body)
  "Run BODY in a Github request buffer.
UNWIND-FORM is run no matter what, and doesn't affect the return
value."
  (declare (indent 4)
           (debug t))
  (let ((call-name (make-symbol "callback")))
    `(let ((,call-name
            (lambda (&optional process event)
              (cond
               ((or (not event)
                    (string-match "\\`finished" event))
                (with-current-buffer (if (processp process)
                                         (process-buffer process)
                                       (current-buffer))
                  (unwind-protect
                      (when (paradox--github-parse-response-code)
                        (let ((next-page))
                          (when (search-forward-regexp
                                 "^Link: .*<\\([^>]+\\)>; rel=\"next\"" nil t)
                            (setq next-page (match-string-no-properties 1))
                            (setq paradox--github-next-page next-page))
                          (ignore next-page)
                          (search-forward-regexp "^\r?$")
                          (skip-chars-forward "[:blank:]\n\r")
                          (delete-region (point-min) (point))
                          ,@body))
                    ,unwind-form
                    (kill-buffer (current-buffer)))))
               ((string-match "\\`exited abnormally" event)
                ,unwind-form
                (paradox--github-report (buffer-string))
                (message "async curl command %s\n  method: %s\n  action: %s"
                         event ,method ,action))))))
       (if ,async
           (condition-case nil
               (set-process-sentinel
                (apply #'start-process "paradox-github"
                       (generate-new-buffer "*Paradox http*")
                       "curl"
                       "-x" (paradox-https-proxy)
                       "-s" "-i" "-d" "" "-X" ,method ,action
                       (when (stringp paradox-github-token)
                         (list "-u" (concat paradox-github-token ":x-oauth-basic"))))
                ,call-name)
             (error ,unwind-form))
         (with-temp-buffer
           ;; Make the request.
           (condition-case nil
               (apply #'call-process
                      "curl" nil t nil
                      "-x" (paradox--https-proxy)
                      "-s" "-i" "-d" "" "-X" ,method ,action
                      (when (stringp paradox-github-token)
                        (list "-u" (concat paradox-github-token ":x-oauth-basic"))))
             (error ,unwind-form))
           ;; Do the processing.
           (funcall ,call-name))))))

(cl-defun paradox--github-action (action &key
                                  (method "GET")
                                  reader
                                  max-pages
                                  (callback #'identity)
                                  async)
  "Contact the github api performing ACTION with METHOD.
Default METHOD is \"GET\".

Action can be anything such as \"user/starred?per_page=100\". If
it's not a full url, it will be prepended with
\"https://api.github.com/\". The action might not work if
`paradox-github-token' isn't set.

This function also handles the pagination used in github results,
results of each page are appended together. Use MAX-PAGES to
limit the number of pages that are fetched.

Return value is always a list.
- If READER is nil, the result of the action is completely
  ignored (no pagination is performed on this case, making it
  much faster).
- Otherwise, READER is called as a function with point right
  after the headers and should always return a list. As a special
  exception, if READER is t, it is equivalent to a function that
  returns (t).

CALLBACK, if provided, is a function to be called with the read
data as an argument. If the request succeeds with no data, it
will be given nil as an argument. Its return value is returned by
this function.

ASYNC determines to run the command asynchronously. In this case,
the function's return value is undefined. In particular, if ASYNC
is the symbol refresh, it means the package-menu should be
refreshed after the operation is done."
  (declare (indent 1))
  ;; Make sure the token's configured.
  (unless (string-match "\\`https?://" action)
    (setq action (concat "https://api.github.com/" action)))
  (let ((do-update (when (eq async 'refresh)
                     (make-symbol "paradox-github"))))
    (when do-update
      (add-to-list 'package--downloads-in-progress do-update))
    (paradox--with-github-buffer method action async
                          (paradox--update-downloads-in-progress
                           do-update)
      (cond
       ((not reader)
        (funcall callback nil))
       ((or (not next-page)
            (and max-pages (< max-pages 2)))
        (funcall callback
                 (unless (eobp) (funcall reader))))
       (t
        (let ((result (unless (eobp) (funcall reader))))
          (paradox--github-action next-page
            :method method
            :reader reader
            :async  async
            :max-pages (when max-pages (1- max-pages))
            :callback (lambda (res)
                        (funcall callback
                                 (append result res))))))))))

(provide 'paradox-github)
;;; paradox-github.el ends here
