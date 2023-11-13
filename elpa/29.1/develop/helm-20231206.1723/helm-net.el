;;; helm-net.el --- helm browse url and search web. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2023 Thierry Volpiatto 

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-help)
(require 'url)
(require 'xml)
(require 'browse-url)

(declare-function helm-comp-read "helm-mode")


(defgroup helm-net nil
  "Net related applications and libraries for Helm."
  :group 'helm)

(defcustom helm-google-suggest-default-browser-function nil
  "The browse url function you prefer to use with Google suggest.
When nil, use the first browser function available
See `helm-browse-url-default-browser-alist'."
  :group 'helm-net
  :type 'symbol)

(defcustom helm-home-url "https://www.google.com"
  "Default url to use as home url."
  :group 'helm-net
  :type 'string)

(defcustom helm-surfraw-default-browser-function nil
  "The browse url function you prefer to use with surfraw.
When nil, fallback to `browse-url-browser-function'."
  :group 'helm-net
  :type 'symbol)

(defcustom helm-google-suggest-url
  "https://encrypted.google.com/complete/search?output=toolbar&q=%s"
  "URL used for looking up Google suggestions.
This is a format string, don't forget the `%s'."
  :type 'string
  :group 'helm-net)

(defcustom helm-google-suggest-search-url
  "https://encrypted.google.com/search?ie=utf-8&oe=utf-8&q=%s"
  "URL used for Google searching.
This is a format string, don't forget the `%s'."
  :type 'string
  :group 'helm-net)

(defvaralias 'helm-google-suggest-use-curl-p 'helm-net-prefer-curl)
(make-obsolete-variable 'helm-google-suggest-use-curl-p 'helm-net-prefer-curl "1.7.7")

(defcustom helm-net-prefer-curl nil
  "When non--nil use CURL external program to fetch data.
Otherwise `url-retrieve-synchronously' is used."
  :type 'boolean
  :group 'helm-net)

(defcustom helm-surfraw-duckduckgo-url
  "https://duckduckgo.com/lite/?q=%s&kp=1"
  "The Duckduckgo url.
This is a format string, don't forget the `%s'.
If you have personal settings saved on duckduckgo you should have
a personal url, see your settings on duckduckgo."
  :type 'string
  :group 'helm-net)

(defcustom helm-search-suggest-action-wikipedia-url
  "https://en.wikipedia.org/wiki/Special:Search?search=%s"
  "The Wikipedia search url.
This is a format string, don't forget the `%s'."
  :type 'string
  :group 'helm-net)

(defcustom helm-search-suggest-action-youtube-url
  "https://www.youtube.com/results?aq=f&search_query=%s"
  "The Youtube search url.
This is a format string, don't forget the `%s'."
  :type 'string
  :group 'helm-net)

(defcustom helm-search-suggest-action-imdb-url
  "http://www.imdb.com/find?s=all&q=%s"
  "The IMDb search url.
This is a format string, don't forget the `%s'."
  :type 'string
  :group 'helm-net)

(defcustom helm-search-suggest-action-google-maps-url
  "https://maps.google.com/maps?f=q&source=s_q&q=%s"
  "The Google Maps search url.
This is a format string, don't forget the `%s'."
  :type 'string
  :group 'helm-net)

(defcustom helm-search-suggest-action-google-news-url
  "https://www.google.com/search?safe=off&prmd=nvlifd&source=lnms&tbs=nws:1&q=%s"
  "The Google News search url.
This is a format string, don't forget the `%s'."
  :type 'string
  :group 'helm-net)

(defcustom helm-google-suggest-actions
  '(("Google Search" . helm-google-suggest-action)
    ("Wikipedia" . (lambda (candidate)
                     (helm-search-suggest-perform-additional-action
                      helm-search-suggest-action-wikipedia-url
                      candidate)))
    ("Youtube" . (lambda (candidate)
                   (helm-search-suggest-perform-additional-action
                    helm-search-suggest-action-youtube-url
                    candidate)))
    ("IMDb" . (lambda (candidate)
                (helm-search-suggest-perform-additional-action
                 helm-search-suggest-action-imdb-url
                 candidate)))
    ("Google Maps" . (lambda (candidate)
                       (helm-search-suggest-perform-additional-action
                        helm-search-suggest-action-google-maps-url
                        candidate)))
    ("Google News" . (lambda (candidate)
                       (helm-search-suggest-perform-additional-action
                        helm-search-suggest-action-google-news-url
                        candidate))))
  "List of actions for google suggest sources."
  :group 'helm-net
  :type '(alist :key-type string :value-type function))

(defcustom helm-browse-url-firefox-new-window "--new-tab"
  "Allow choosing to browse url in new window or new tab.
Can be \"--new-tab\" (default), \"--new-window\" or \"--private-window\"."
  :group 'helm-net
  :type '(radio
          (const :tag "New tab" "--new-tab")
          (const :tag "New window" "--new-window")
          (const :tag "New private window" "--private-window")))

(defcustom helm-net-curl-switches '("-s" "-L")
  "Arguments list passed to curl when using `helm-net-prefer-curl'."
  :group 'helm-net
  :type '(repeat string))

;;; Additional actions for search suggestions
;;
;;
;; Internal
(defvar helm-net-curl-log-file (expand-file-name "helm-curl.log" user-emacs-directory))
(defun helm-search-suggest-perform-additional-action (url query)
  "Perform the search via URL using QUERY as input."
  (browse-url (format url (url-hexify-string query))))

(defun helm-net--url-retrieve-sync (request parser)
  (if helm-net-prefer-curl
      (with-temp-buffer
        (apply #'call-process "curl"
               nil `(t ,helm-net-curl-log-file) nil request helm-net-curl-switches)
        (funcall parser))
      (with-current-buffer (url-retrieve-synchronously request)
        (funcall parser))))


;;; Google Suggestions
;;
;;
(defun helm-google-suggest-parser ()
  (cl-loop
   with result-alist = (xml-get-children
                        (car (xml-parse-region
                              (point-min) (point-max)))
                        'CompleteSuggestion)
   for i in result-alist collect
   (cdr (cl-caadr (assq 'suggestion i)))))

(defun helm-google-suggest-fetch (input)
  "Fetch suggestions for INPUT from XML buffer."
  (let ((request (format helm-google-suggest-url
                         (url-hexify-string input))))
    (helm-net--url-retrieve-sync
     request #'helm-google-suggest-parser)))

(defun helm-google-suggest-set-candidates (&optional request-prefix)
  "Set candidates with result and number of Google results found."
  (let ((suggestions (helm-google-suggest-fetch
                      (or (and request-prefix
                               (concat request-prefix
                                       " " helm-pattern))
                          helm-pattern))))
    (if (member helm-pattern suggestions)
        suggestions
        ;; if there is no suggestion exactly matching the input then
        ;; prepend a Search on Google item to the list
        (append
         suggestions
         (list (cons (format "Search for '%s' on Google" helm-input)
                     helm-input))))))

(defun helm-ggs-set-number-result (num)
  (if num
      (progn
        (and (numberp num) (setq num (number-to-string num)))
        (cl-loop for i in (reverse (split-string num "" t))
              for count from 1
              append (list i) into C
              when (= count 3)
              append (list ",") into C
              and do (setq count 0)
              finally return
              (replace-regexp-in-string
               "^," "" (mapconcat 'identity (reverse C) ""))))
    "?"))

(defun helm-google-suggest-action (candidate)
  "Default action to jump to a Google suggested candidate."
  (let ((arg (format helm-google-suggest-search-url
                     (url-hexify-string candidate))))
    (helm-aif helm-google-suggest-default-browser-function
        (funcall it arg)
      (helm-browse-url arg))))

(defvar helm-google-suggest-default-function
  'helm-google-suggest-set-candidates
  "Default function to use in `helm-google-suggest'.")

(defvar helm-source-google-suggest
  (helm-build-sync-source "Google Suggest"
    :candidates (lambda ()
                  (funcall helm-google-suggest-default-function))
    :action 'helm-google-suggest-actions
    :match-dynamic t
    :keymap helm-map
    :requires-pattern 3))

(defun helm-google-suggest-emacs-lisp ()
  "Try to emacs lisp complete with Google suggestions."
  (helm-google-suggest-set-candidates "emacs lisp"))


;;; Web browser functions.
;;
;;
;; If default setting of `w3m-command' is not
;; what you want and you modify it, you will have to reeval
;; also `helm-browse-url-default-browser-alist'.

(defvar helm-browse-url-chromium-program "chromium-browser")
(defvar helm-browse-url-uzbl-program "uzbl-browser")
(defvar helm-browse-url-nyxt-program "nyxt")
(defvar helm-browse-url-conkeror-program "conkeror")
(defvar helm-browse-url-opera-program "opera")
(defvar helm-browse-url-w3m-program (or (and (boundp 'w3m-command) w3m-command)
                                        (executable-find "w3m")))
(defvar helm-browse-url-default-browser-alist
  '((helm-browse-url-w3m-program . w3m-browse-url)
    (browse-url-firefox-program . browse-url-firefox)
    (helm-browse-url-chromium-program . helm-browse-url-chromium)
    (helm-browse-url-conkeror-program . helm-browse-url-conkeror)
    (helm-browse-url-opera-program . helm-browse-url-opera)
    (helm-browse-url-uzbl-program . helm-browse-url-uzbl)
    (helm-browse-url-nyxt-program . helm-browse-url-nyxt)
    (browse-url-kde-program . browse-url-kde)
    (browse-url-gnome-moz-program . browse-url-gnome-moz)
    (browse-url-mozilla-program . browse-url-mozilla)
    (browse-url-galeon-program . browse-url-galeon)
    (browse-url-netscape-program . browse-url-netscape)
    (browse-url-xterm-program . browse-url-text-xterm)
    ("emacs" . eww-browse-url))
  "Alist of (browse_url_variable . function) to try to find a suitable url browser.")

(cl-defun helm-generic-browser (url cmd-name &rest args)
  "Browse URL with NAME browser."
  (let ((proc (concat cmd-name " " url)))
    (message "Starting %s..." cmd-name)
    (apply 'start-process proc nil cmd-name
           (append args (list url)))
    (set-process-sentinel
     (get-process proc)
     (lambda (process event)
         (when (string= event "finished\n")
           (message "%s process %s" process event))))))

;;;###autoload
(defun helm-browse-url-firefox (url &optional _ignore)
  "Same as `browse-url-firefox' but detach from Emacs.

So when you quit Emacs you can keep your Firefox session open and
not be prompted to kill the Firefox process.

NOTE: Probably not supported on some systems (e.g., Windows)."
  (interactive (list (read-string "URL: " (browse-url-url-at-point))
                     nil))
  (setq url (browse-url-encode-url url))
  (let ((process-environment (browse-url-process-environment)))
    (call-process-shell-command
     (format "(%s %s %s &)"
             browse-url-firefox-program
             helm-browse-url-firefox-new-window
             (shell-quote-argument url)))))

;;;###autoload
(defun helm-browse-url-opera (url &optional _ignore)
  "Browse URL with Opera browser and detach from Emacs.

So when you quit Emacs you can keep your Opera session open and
not be prompted to kill the Opera process.

NOTE: Probably not supported on some systems (e.g., Windows)."
  (interactive (list (read-string "URL: " (browse-url-url-at-point))
                     nil))
  (setq url (browse-url-encode-url url))
  (let ((process-environment (browse-url-process-environment)))
    (call-process-shell-command
     (format "(%s %s &)"
             helm-browse-url-opera-program (shell-quote-argument url)))))

;;;###autoload
(defun helm-browse-url-chromium (url &optional _ignore)
  "Browse URL with Google Chrome browser."
  (interactive "sURL: ")
  (helm-generic-browser
   url helm-browse-url-chromium-program))

;;;###autoload
(defun helm-browse-url-uzbl (url &optional _ignore)
  "Browse URL with uzbl browser."
  (interactive "sURL: ")
  (helm-generic-browser url helm-browse-url-uzbl-program "-u"))

;;;###autoload
(defun helm-browse-url-conkeror (url &optional _ignore)
  "Browse URL with conkeror browser."
  (interactive "sURL: ")
  (helm-generic-browser url helm-browse-url-conkeror-program))

;;;###autoload
(defun helm-browse-url-nyxt (url &optional _ignore)
  "Browse URL with nyxt browser."
  (interactive "sURL: ")
  (helm-generic-browser url helm-browse-url-nyxt-program))

(defun helm-browse-url-default-browser (url &rest args)
  "Find the first available browser and ask it to load URL."
  (let ((default-browser-fn
         (cl-loop for (var . fn) in helm-browse-url-default-browser-alist
                  for exe = (if (stringp var)
                                var
                              (and (boundp var) (symbol-value var)))
                  thereis (and exe (executable-find exe) (fboundp fn) fn))))
    (if default-browser-fn
        (apply default-browser-fn url args)
      (error "No usable browser found"))))

(defun helm-browse-url (url &rest args)
  "Default command to browse URL."
  (if browse-url-browser-function
      (browse-url url args)
    (helm-browse-url-default-browser url args)))


;;; Surfraw
;;
;; Need external program surfraw.
;; <http://surfraw.alioth.debian.org/>

;; Internal
(defvar helm-surfraw-engines-history nil)
(defvar helm-surfraw-input-history nil)
(defvar helm-surfraw--elvi-cache nil)

(defun helm-build-elvi-list ()
  "Return list of all engines and descriptions handled by surfraw."
  (or helm-surfraw--elvi-cache
      (setq helm-surfraw--elvi-cache
            (cdr (with-temp-buffer
                   (call-process "surfraw" nil t nil "-elvi")
                   (split-string (buffer-string) "\n"))))))

;;;###autoload
(defun helm-surfraw (pattern engine)
  "Preconfigured `helm' to search PATTERN with search ENGINE."
  (interactive
   (list
    (let* ((default (if (use-region-p)
                        (buffer-substring-no-properties
                         (region-beginning) (region-end))
                      (thing-at-point 'symbol)))
           (prompt (if default
                       (format "SearchFor (default %s): " default)
                     "SearchFor: ")))
      (read-string prompt nil 'helm-surfraw-input-history default))
    (helm-comp-read
     "Engine: "
     (helm-build-elvi-list)
     :must-match t
     :name "Surfraw Search Engines"
     :history 'helm-surfraw-engines-history)))
  (let* ((engine-nodesc (car (split-string engine)))
         (url (if (string= engine-nodesc "duckduckgo")
                  ;; "sr duckduckgo -p foo" is broken, workaround.
                  (format helm-surfraw-duckduckgo-url
                          (url-hexify-string pattern))
                (with-temp-buffer
                  (apply 'call-process "surfraw" nil t nil
                         (append  (list engine-nodesc "-p") (split-string pattern)))
                  (replace-regexp-in-string
                   "\n" "" (buffer-string)))))
         (browse-url-browser-function (or helm-surfraw-default-browser-function
                                          browse-url-browser-function)))
    (if (string= engine-nodesc "W")
        (helm-browse-url helm-home-url)
      (helm-browse-url url))))

;;;###autoload
(defun helm-google-suggest ()
  "Preconfigured `helm' for Google search with Google suggest."
  (interactive)
  (helm-other-buffer 'helm-source-google-suggest "*helm google*"))

(provide 'helm-net)

;;; helm-net.el ends here
