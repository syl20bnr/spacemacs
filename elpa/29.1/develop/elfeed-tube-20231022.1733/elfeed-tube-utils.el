;;; elfeed-tube-utils.el --- utilities for elfeed-tube  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: multimedia, convenience

;; SPDX-License-Identifier: UNLICENSE

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; Utilities for Elfeed Tube.
;;
;;; Code:
(require 'rx)
(require 'aio)
(require 'elfeed)

(declare-function elfeed-tube--with-label "elfeed-tube")
(declare-function elfeed-tube--fetch-1 "elfeed-tube")
(declare-function elfeed-tube-show "elfeed-tube")
(declare-function elfeed-tube-curl-enqueue "elfeed-tube")
(declare-function elfeed-tube--attempt-log "elfeed-tube")
(declare-function elfeed-tube-log "elfeed-tube")
(declare-function elfeed-tube--get-invidious-url "elfeed-tube")
(declare-function elfeed-tube--nrotate-invidious-servers "elfeed-tube")

(defvar elfeed-tube-youtube-regexp)
(defvar elfeed-tube--api-videos-path)
(defvar elfeed-tube--max-retries)

(defsubst elfeed-tube--ensure-list (var)
  "Ensure VAR is a list."
  (if (listp var) var (list var)))

(cl-defstruct (elfeed-tube-channel (:constructor elfeed-tube-channel-create)
                                   (:copier nil))
  "Struct to hold youtube channel information."
  query author url feed)

;;;###autoload (autoload 'elfeed-tube-add-feeds "elfeed-tube-utils" "Add youtube feeds to the Elfeed database by QUERIES." t nil)
(aio-defun elfeed-tube-add-feeds (queries &optional _)
  "Add youtube feeds to the Elfeed database by QUERIES.

Each query can be a video, playlist or channel URL and the
corresponding channel feed will be selected. It can also be a
search term and the best match will be found. You will be asked
to finalize the results before committing them to Elfeed.

When called interactively, multiple queries can be provided by
separating them with the `crm-separator', typically
comma (\",\"). Search terms cannot include the `crm-separator'.

When called noninteractively, it accepts a query or a list of
queries."
  (interactive
   (list (completing-read-multiple
          "Video, Channel, Playlist URLs or search queries: "
          #'ignore)
         current-prefix-arg))
  (message "Finding RSS feeds, hold tight!")
  (let ((channels (aio-await (elfeed-tube-add--get-channels queries))))
    (elfeed-tube-add--display-channels channels)))

(defsubst elfeed-tube--video-p (cand)
  "Check if CAND is a Youtube video URL."
  (string-match
   (concat
    elfeed-tube-youtube-regexp
    (rx (zero-or-one "watch?v=")
        (group (1+ (not "&")))))
   cand))

(defsubst elfeed-tube--playlist-p (cand)
  "Check if CAND is a Youtube playlist URL."
  (string-match
   (concat
    elfeed-tube-youtube-regexp
    "playlist\\?list="
    (rx (group (1+ (not "&")))))
   cand))

(defsubst elfeed-tube--channel-p (cand)
  "Check if CAND is a Youtube channel URL."
  (string-match
   (concat
    elfeed-tube-youtube-regexp
    (rx "channel/"
        (group (1+ (not "&")))))
   cand))

(aio-defun elfeed-tube-add--get-channels (queries)
  (let* ((fetches (aio-make-select))
         (queries (elfeed-tube--ensure-list queries))
         (playlist-base-url
          "https://www.youtube.com/feeds/videos.xml?playlist_id=")
         (channel-base-url
          "https://www.youtube.com/feeds/videos.xml?channel_id=")
         channels)

    ;; Add all promises to fetches, an aio-select
    (dolist (q queries channels)
      (setq q (string-trim q))
      (cond
       ((elfeed-tube--channel-p q)
        (let* ((chan-id (match-string 1 q))
               (api-url (concat (aio-await (elfeed-tube--get-invidious-url))
                                "/api/v1/channels/"
                                chan-id
                                "?fields=author,authorUrl"))
               (feed (concat channel-base-url chan-id)))
          (aio-select-add fetches
                          (elfeed-tube--with-label
                           `(:type channel :feed ,feed :query ,q)
                           #'elfeed-tube--aio-fetch
                           api-url #'elfeed-tube--nrotate-invidious-servers))))
        
       ((string-match
         (concat elfeed-tube-youtube-regexp "\\(?:@\\|c/\\)" "\\([^?&]+\\)") q)
        ;; Interpret channel url as search query
        (let* ((search-url "/api/v1/search")
               (api-url (concat (aio-await (elfeed-tube--get-invidious-url))
                                search-url
                                "?q=" (url-hexify-string (match-string 1 q))
                                "&type=channel&page=1")))
          (aio-select-add fetches
                          (elfeed-tube--with-label
                           `(:type search :query ,q)
                           #'elfeed-tube--aio-fetch
                           api-url #'elfeed-tube--nrotate-invidious-servers))))

       ((elfeed-tube--playlist-p q)
        (let* ((playlist-id (match-string 1 q))
               (api-url (concat (aio-await (elfeed-tube--get-invidious-url))
                                "/api/v1/playlists/"
                                playlist-id
                                "?fields=title,author"))
               (feed (concat playlist-base-url playlist-id)))
            (aio-select-add fetches
                            (elfeed-tube--with-label
                             `(:type playlist :feed ,feed :query ,q)
                             #'elfeed-tube--aio-fetch
                             api-url #'elfeed-tube--nrotate-invidious-servers))))
       
       ((elfeed-tube--video-p q)
        (if-let* ((video-id (match-string 1 q))
                  (videos-url "/api/v1/videos/")
                  (api-url (concat (aio-await (elfeed-tube--get-invidious-url))
                                   videos-url
                                   video-id
                                   "?fields=author,authorUrl,authorId")))
            (aio-select-add fetches
                            (elfeed-tube--with-label
                             `(:type video :query ,q)
                             #'elfeed-tube--aio-fetch
                             api-url #'elfeed-tube--nrotate-invidious-servers))
          (push (elfeed-tube-channel-create :query q)
                channels)))
       
       (t ;interpret as search query
        (let* ((search-url "/api/v1/search")
               (api-url (concat (aio-await (elfeed-tube--get-invidious-url))
                                search-url
                                "?q=" (url-hexify-string q)
                                "&type=channel&page=1")))
            (aio-select-add fetches
                            (elfeed-tube--with-label
                             `(:type search :query ,q)
                             #'elfeed-tube--aio-fetch
                             api-url #'elfeed-tube--nrotate-invidious-servers))))))
    
    ;; Resolve all promises in the aio-select
    (while (aio-select-promises fetches)
      (pcase-let* ((`(,label . ,data)
                    (aio-await (aio-await (aio-select fetches))))
                   (q (plist-get label :query))
                   (feed (plist-get label :feed)))
        (pcase (plist-get label :type)
          ('channel
           (if-let ((author (plist-get data :author))
                    (author-url (plist-get data :authorUrl)))
               (push (elfeed-tube-channel-create
                      :query q :author author
                      :url  q
                      :feed feed)
                     channels)
             (push (elfeed-tube-channel-create :query q :feed feed)
                   channels)))
          
          ('playlist
           (if-let ((title (plist-get data :title))
                    (author (plist-get data :author)))
               (push (elfeed-tube-channel-create
                      :query q :author title :url q
                      :feed feed)
                     channels)
             (push (elfeed-tube-channel-create
                    :query q :url q
                    :feed feed)
                   channels)))
          ('video
           (if-let* ((author (plist-get data :author))
                     (author-id (plist-get data :authorId))
                     (author-url (plist-get data :authorUrl))
                     (feed (concat channel-base-url author-id)))
               (push (elfeed-tube-channel-create
                      :query q :author author
                      :url (concat "https://www.youtube.com" author-url)
                      :feed feed)
                     channels)
             (push (elfeed-tube-channel-create :query (plist-get label :query))
                   channels)))
          ('search
           (if-let* ((chan-1 (and (> (length data) 0)
                                  (aref data 0)))
                     (author (plist-get chan-1 :author))
                     (author-id (plist-get chan-1 :authorId))
                     (author-url (plist-get chan-1 :authorUrl))
                     (feed (concat channel-base-url author-id)))
               (push (elfeed-tube-channel-create
                      :query q :author author
                      :url (concat "https://www.youtube.com" author-url)
                      :feed feed)
                     channels)
             (push (elfeed-tube-channel-create :query q)
                   channels))))))
    
    (nreverse channels)))

(defun elfeed-tube-add--display-channels (channels)
  "Summarize found Youtube channel feeds CHANNELS."
  (let ((buffer (get-buffer-create "*Elfeed-Tube Channels*"))
        (notfound (propertize "Not found!" 'face 'error)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)) (erase-buffer))
      (elfeed-tube-channels-mode)
      (setq
       tabulated-list-entries
       (cl-loop for channel in channels
                for n upfrom 1
                for author = (if-let ((url (elfeed-tube-channel-url channel)))
                                 (list (elfeed-tube-channel-author channel)
                                       'mouse-face 'highlight
                                       'action
                                       #'elfeed-tube-add--visit-channel
                                       'follow-link t
                                       'help-echo (elfeed-tube-channel-url channel))
                                 notfound)
                for feed = (or (elfeed-tube-channel-feed channel) notfound)
                collect
                `(,n
                  [,author
                   ,(replace-regexp-in-string
                     elfeed-tube-youtube-regexp ""
                     (elfeed-tube-channel-query channel))
                   ,feed])))
      (setq tabulated-list-format
            '[("Channel" 22 t)
              ("Query" 32 t)
              ("Feed URL" 30 nil)])

      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-address-mode 1)
      
      (goto-char (point-max))
      
      (let ((inhibit-read-only t)
            (fails (cl-reduce
                    (lambda (sum ch)
                      (+ sum
                         (or (and (elfeed-tube-channel-feed ch) 0) 1)))
                    channels :initial-value 0))
            (continue (propertize "C-c C-c" 'face 'help-key-binding))
            (continue-extra (propertize "C-u C-c C-c" 'face 'help-key-binding))
            (cancel-q (propertize "q" 'face 'help-key-binding))
            (cancel   (propertize "C-c C-k" 'face 'help-key-binding))
            (copy     (propertize "C-c C-w" 'face 'help-key-binding)))
        
        (let ((inhibit-message t))
          (toggle-truncate-lines 1))
        (insert "\n")
        (when (> fails 0)
          (insert (propertize
                   (format "%d queries could not be resolved.\n\n" fails)
                   'face 'error)
                  "     " continue ": Add found feeds to the Elfeed database, ignoring the failures.\n"
                  " " continue-extra ": Add found feeds, fetch entries from them and open Elfeed.\n"))
        (when (= fails 0)
          (insert
           (propertize
            "All queries resolved successfully.\n\n"
            'face 'success)
           "     " continue ": Add all feeds to the Elfeed database.\n"
           " " continue-extra ": Add all feeds, fetch entries from them and open Elfeed.\n"
           "     " copy ": Copy the list of feed URLs as a list\n"))
        (insert "\n" cancel-q " or " cancel ": Quit and cancel this operation."))
      
      (goto-char (point-min))
      
      (use-local-map (copy-keymap elfeed-tube-channels-mode-map))
      (local-set-key (kbd "C-c C-c") #'elfeed-tube-add--confirm)
      (local-set-key (kbd "C-c C-w") #'elfeed-tube-add--copy)
      
      (funcall
       (if (bound-and-true-p demo-mode)
           #'switch-to-buffer
         #'display-buffer)
       buffer))))

(defun elfeed-tube-add--visit-channel (button)
  "Activate BUTTON."
  (browse-url (button-get button 'help-echo)))

;; (elfeed-tube-add--display-channels my-channels)

(defun elfeed-tube-add--confirm (&optional arg)
  "Confirm the addition of visible Youtube feeds to the Elfeed database.

With optional prefix argument ARG, update these feeds and open Elfeed
afterwards."
  (interactive "P")
  (cl-assert (derived-mode-p 'elfeed-tube-channels-mode))
  (let* ((channels tabulated-list-entries))
    (let ((inhibit-message t))
      (cl-loop for channel in channels
               for (_ _ feed) = (append (cadr channel) nil)
               do (elfeed-add-feed feed :save t)))
    (message "Added to elfeed-feeds.")
    (when arg (elfeed))))

(defvar elfeed-tube-channels-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-k") (lambda () (interactive) (quit-window 'kill-buffer)))
    map))

(define-derived-mode elfeed-tube-channels-mode tabulated-list-mode
  "Elfeed Tube Channels"
  (setq tabulated-list-use-header-line t ; default to no header
        ;; tabulated-list--header-string nil
        header-line-format nil))

(defun elfeed-tube-add--copy ()
  "Copy visible Youtube feeds to the kill ring as a list.

With optional prefix argument ARG, update these feeds and open Elfeed
afterwards."
  (interactive)
  (cl-assert (derived-mode-p 'elfeed-tube-channels-mode))
  (let* ((channels tabulated-list-entries))
    (cl-loop for channel in channels
             for (_ _ feed) = (append (cadr channel) nil)
             collect feed into feeds
             finally (kill-new (prin1-to-string feeds)))
    (message "Feed URLs saved to kill-ring.")))

(aio-defun elfeed-tube--aio-fetch (url &optional next desc attempts)
  "Fetch URL asynchronously using `elfeed-curl-retrieve'.

If successful (HTTP 200), return the JSON-parsed result as a
plist.

Otherwise, call the function NEXT (with no arguments) and try
ATTEMPTS more times. Return nil if all attempts fail. DESC is a
description string to print to the elfeed-tube log allong with
any other error messages.

This function returns a promise."
  (let ((attempts (or attempts (1+ elfeed-tube--max-retries))))
    (when (> attempts 0)
      (let* ((response
              (aio-await (elfeed-tube-curl-enqueue url :method "GET")))
             (content (plist-get response :content))
             (status (plist-get response :status-code))
             (error-msg (plist-get response :error-message)))
        (cond
         ((equal status 200)
          (condition-case nil
              (json-parse-string content :object-type 'plist)
            ((json-parse-error error)
             (elfeed-tube-log 'error "[Search] JSON malformed (%s)"
                              (elfeed-tube--attempt-log attempts))
             (and (functionp next) (funcall next))
             (aio-await
              (elfeed-tube--aio-fetch url next desc (1- attempts))))))
         (t (elfeed-tube-log 'error "[Search][%s]: %s (%s)" error-msg url
                             (elfeed-tube--attempt-log attempts))
            (and (functionp next) (funcall next))
            (aio-await
             (elfeed-tube--aio-fetch url next desc (1- attempts)))))))))

(defun elfeed-tube--entry-create (feed-id entry-data)
  "Create an Elfeed entry from ENTRY-DATA for feed with id FEED-ID.

FEED-ID is the id of the feed in the Elfeed database. ENTRY-DATA
is a plist of video metadata."
  (cl-assert (listp entry-data))
  (cl-assert (plist-get entry-data :videoId))

  (let* ((video-id (plist-get entry-data :videoId))
         (link (format "https://www.youtube.com/watch?v=%s" video-id))
         (title (plist-get entry-data :title))
         (published (plist-get entry-data :published))
         (author `((:name ,(plist-get entry-data :author)
                    :uri ,feed-id))))
    (elfeed-entry--create
     :link link
     :title title
     :id `("www.youtube.com" . ,(concat "yt:video:" video-id))
     :date published
     :tags '(unread)
     :content-type 'html
     :meta `(:authors ,author)
     :feed-id feed-id)))

(aio-defun elfeed-tube--fake-entry (url &optional force-fetch)
  (string-match (concat elfeed-tube-youtube-regexp
                        (rx (zero-or-one "watch?v=")
                            (group (1+ (not (or "&" "?"))))))
                url)
  (if-let ((video-id (match-string 1 url)))
      (progn
        (message "Creating a video summary...")
        (cl-letf* ((elfeed-show-unique-buffers t)
                   (elfeed-show-entry-switch #'display-buffer)
                   (elfeed-tube-save-indicator nil)
                   (elfeed-tube-auto-save-p nil)
                   (api-data (aio-await
                              (elfeed-tube--aio-fetch
                               (concat (aio-await (elfeed-tube--get-invidious-url))
                                       elfeed-tube--api-videos-path
                                       video-id
                                       "?fields="
                                       ;; "videoThumbnails,descriptionHtml,lengthSeconds,"
                                       "title,author,authorUrl,published,videoId")
                               #'elfeed-tube--nrotate-invidious-servers)))
                   (feed-id (concat "https://www.youtube.com/feeds/videos.xml?channel_id="
                                    (nth 1 (split-string (plist-get api-data :authorUrl)
                                                         "/" t))))
                   (author `((:name ,(plist-get api-data :author)
                                    :uri ,feed-id)))
                   (entry (elfeed-tube--entry-create feed-id api-data))
                   ((symbol-function 'elfeed-entry-feed)
                    (lambda (_)
                      (elfeed-feed--create
                       :id feed-id
                       :url feed-id
                       :title (plist-get api-data :author)
                       :author author))))
          (aio-await (elfeed-tube--fetch-1 entry force-fetch))
          (with-selected-window (elfeed-show-entry entry)
            (message "Summary created for video: \"%s\""
                     (elfeed-entry-title entry))
            (setq-local elfeed-show-refresh-function
                        (lambda () (interactive)
                          (elfeed-tube-show))
                        elfeed-tube-save-indicator nil)
            (use-local-map (copy-keymap elfeed-show-mode-map))
            (local-set-key (kbd "q") 'quit-window))))
    (message "Not a youtube video URL, aborting.")))

(defsubst elfeed-tube--line-at-point ()
  "Get line around point."
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun elfeed-tube-next-heading (&optional arg)
  "Jump to the next heading in an Elfeed entry.

With numeric prefix argument ARG, jump forward that many times.
If ARG is negative, jump backwards instead."
  (interactive "p")
  (unless arg (setq arg 1))
  (catch 'return
    (dotimes (_ (abs arg))
      (when (> arg 0) (end-of-line))
      (if-let ((match
                (funcall (if (> arg 0)
                             #'text-property-search-forward
                           #'text-property-search-backward)
                         'face `(shr-h1 shr-h2 shr-h3
                                        message-header-name elfeed-tube-chapter-face)
                         (lambda (tags face)
                           (cl-loop for x in (if (consp face) face (list face))
                                    thereis (memq x tags)))
                         t)))
          (goto-char
           (if (> arg 0) (prop-match-beginning match) (prop-match-end match)))
        (throw 'return nil))
      (when (< arg 0) (beginning-of-line)))
    (beginning-of-line)
    (point)))

(defun elfeed-tube-prev-heading (&optional arg)
  "Jump to the previous heading in an Elfeed entry.

With numeric prefix argument ARG, jump backward that many times.
If ARG is negative, jump forward instead."
  (interactive "p")
  (elfeed-tube-next-heading (- (or arg 1))))

(provide 'elfeed-tube-utils)
;;; elfeed-tube-utils.el ends here
