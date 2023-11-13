;;; elfeed-tube-fill.el --- Back-fill elfeed-tube feeds  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; Keywords: multimedia, convenience

;; SPDX-License-Identifier: UNLICENSE

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; This file contains commands to back-fill Elfeed YouTube feeds. Back-filling a
;; feed fetches all historical entries for the corresponding YouTube channel or
;; playlist and adds them to the Elfeed database. Youtube RSS feeds generally
;; contain only the latest 15 entries.
;;
;; Call `elfeed-tube-fill-feeds' in an Elfeed search or entry buffer to
;; back-fill entries for the corresponding feed. You can select a region of
;; entries to fill all the corresponding feeds.
;;
;;; Code:

(require 'elfeed-tube)

(declare-function elfeed-tube--get-entries "elfeed-tube")
(defvar elfeed-tube--api-channels-videos-path "/api/v1/channels/%s/videos")
(defvar elfeed-tube--api-playlists-videos-path "/api/v1/playlists/%s")
(defvar elfeed-tube--fill-tags nil
  "Alist of Elfeed feed-ids and tags to add.

These tags (list of symbols) will be added when back-filling the
corresponding feed.")


(cl-deftype elfeed-tube--fill-api-data ()
  `(satisfies
   (lambda (coll)
     (and (vectorp coll)
          (or (= (length coll) 0)
              (cl-every (lambda (vd) (and (plist-get vd :videoId)
                                     (plist-get vd :title)))
                        coll))))))

;; Progress reporting
(defvar elfeed-tube--fill-header nil)

(defun elfeed-tube--fill-header ()
  "Display a progress header when filling feeds with elfeed-tube."
  (apply #'concat `("Filling Feed: " ,@elfeed-tube--fill-header)))

(defun elfeed-tube--fill-progress-update (pending &optional force-update)
  "Update report of PENDING fetch jobs for `elfeed-tube-fill-feeds'.

Optional argument FORCE-UPDATE will force redisplay of the header line."
  (setf (cadr elfeed-tube--fill-header)
        (cl-typecase pending
          (integer
           (concat (propertize " ...fetch metadata (" 'face 'shadow)
                   (propertize (format "%d pending" pending)
                               'face 'elfeed-search-unread-count-face)
                   (propertize ")" 'face 'shadow)))
          (vector (format " ...%d entries to fetch" (length pending)))
          (string pending)))
  (when force-update
      (when-let* ((win (get-buffer-window "*elfeed-search*"))
                  ((window-live-p win)))
        (with-selected-window win (force-mode-line-update))))
  pending)

;; Feed filling

;;;###autoload (autoload 'elfeed-tube-fill-feeds "elfeed-tube-fill" "Fetch and add all channel videos for ENTRIES' feeds." t nil)
(aio-defun elfeed-tube-fill-feeds (entries &optional interactive-p)
  "Fetch and add all channel videos for ENTRIES' feeds.

This feature is currently disabled, pending changes to upstream
APIs. Sorry about that!

YouTube RSS feeds generally contain only the latest 15 entries.
Use this command to fetch and add to Elfeed all videos
corresponding a channel or playlist.

ENTRIES is the entry at point or visited entry, or the list of
selected entries if the region is active.

When called interactively, INTERACTIVE-P is t and a summary
window will be shown before taking any action."
  (interactive (list (elfeed-tube--ensure-list (elfeed-tube--get-entries))
                     t))
  (let ((feeds (cl-reduce
                  (lambda (accum entry)
                    (if-let* ((feed (elfeed-entry-feed entry))
                              ((memq feed accum)))
                        accum
                      (cons feed accum)))
                  entries
                  :initial-value nil)))
      (if interactive-p
          (elfeed-tube--fill-display-feeds feeds)
        (aio-await (elfeed-tube--fill-feeds feeds)))))

(aio-defun elfeed-tube--fill-feeds (feeds)
  "Find videos corresponding to the channels/playlists for Elfeed feeds FEEDS.

Videos not already present will be added to the Elfeed database."
  (cl-check-type feeds (and (not null) (not atom)))
  (cl-check-type (car feeds) elfeed-feed)

  (advice-add elfeed-search-header-function :override
              #'elfeed-tube--fill-header)
  
  (unwind-protect
      (dolist (feed feeds)
        (setq elfeed-tube--fill-header
              (list (propertize (elfeed-feed-title feed) 'face 'elfeed-search-feed-face)
                    ""))
        (elfeed-tube-log 'debug "[(fill-feeds): Backfilling feed: %s]"
                         (elfeed-feed-title feed))
        (let ((elfeed-tube-auto-fetch-p nil)
              (feed-url (elfeed-feed-url feed))
              (feed-id  (elfeed-feed-id feed))
              (feed-title (elfeed-feed-title feed))
              (add-count)
              (feed-entries-to-add
               (thread-first
                 (elfeed-tube--fill-feed feed)
                 (aio-await)
                 (cl-delete-duplicates :key (lambda (x) (plist-get x :videoId)) :test #'string=)
                 (vconcat)
                 (elfeed-tube--fill-progress-update 'redisplay)
                 (elfeed-tube--fill-feed-dates)
                 (aio-await))))

          (cl-check-type feed-entries-to-add elfeed-tube--fill-api-data
                         "Missing video attributes (ID, Title or Publish Date).")

          (if (= (length feed-entries-to-add) 0)
              (prog1 (elfeed-tube--fill-progress-update
                      (propertize " ...nothing to retrieve." 'face 'shadow)
                      'redisplay)
                (message "Nothing to retrieve for feed \"%s\" (%s)" feed-title feed-url))
            (setq add-count (length feed-entries-to-add))
            (condition-case error
                (prog1
                    (thread-last
                      feed-entries-to-add
                      (cl-map 'list (apply-partially #'elfeed-tube--entry-create feed-id))
                      (cl-map 'list (lambda (entry)
                                      (setf (elfeed-entry-tags entry)
                                            (or (alist-get feed-id elfeed-tube--fill-tags
                                                           nil nil #'equal)
                                                '(unread)))
                                      entry))
                      (elfeed-db-add))
                  (elfeed-tube--fill-progress-update
                   (concat
                    (propertize " ...done (" 'face 'shadow)
                    (propertize (format "added %d entries" add-count) 'face 'success)
                    (propertize ")" 'face 'shadow))
                   'redisplay)
                  (elfeed-tube-log 'debug "[(elfeed-db): Backfilling feed: %s][Added %d videos]"
                                   feed-title add-count)
                  (message "Retrieved %d missing videos for feed \"%s\" (%s)"
                           add-count feed-title feed-url))
              (error (elfeed-handle-parse-error feed-url error)))
            (run-hook-with-args 'elfeed-update-hooks feed-url))
          (aio-await (aio-sleep 0.8 nil))))

    (run-at-time 1.5 nil
                 (lambda () 
                   (advice-remove elfeed-search-header-function
                                  #'elfeed-tube--fill-header)
                   (setf elfeed-tube--fill-header nil)))))

;; feed: elfeed-feed struct, page: int or nil -> vector(plist entries for feed videos not in db)
(aio-defun elfeed-tube--fill-feed (feed &optional page)
  "Find videos corresponding to the channel/playlist for Elfeed feed FEED.

Return video metadata as a vector of plists. Metadata
corresponding to videos already in the Elfeed database are
filtered out.

PAGE corresponds to the page number of results requested from the API,
or a continuation string included in the previous response."
  (cl-check-type feed elfeed-feed "An Elfeed Feed")
  (cl-check-type page (or null (integer 0 *) string) "A positive integer or string.")

  (if-let* ((feed-url (elfeed-feed-url feed))
            (feed-title (elfeed-feed-title feed))
            (api-path (cond ((string-match "playlist_id=\\(.*?\\)/*$" feed-url)
                             (concat
                              (format elfeed-tube--api-playlists-videos-path
                                      (match-string 1 feed-url))
                              "?fields=videos(title,videoId,author)"
                              "&page=" (number-to-string (or page 1))))
                            ((string-match "channel_id=\\(.*?\\)/*$" feed-url)
                             (concat
                              (format elfeed-tube--api-channels-videos-path
                                      (match-string 1 feed-url))
                              "?fields="
                              "videos(title,videoId,author,published),continuation"
                              "&sort_by=newest"
                              ;; PAGE here is actually a continuation string
                              (when (stringp page) (concat "&continuation=" page))))
                            (t (elfeed-tube-log 'error "[Malformed/Not YouTube feed: %s][%s]"
                                                feed-title feed-url)
                               nil)))
            (feed-type (cond ((string-match "playlist_id=\\(.*?\\)/*$" feed-url) 'playlist)
                             ((string-match "channel_id=\\(.*?\\)/*$" feed-url)  'channel))))
      (let ((feed-entry-video-ids
             (mapcar (lambda (e) (elfeed-tube--url-video-id (elfeed-entry-link e)))
                     (elfeed-feed-entries feed)))
            (feed-id (elfeed-feed-id feed))
            (next-page))
        (if-let*
            ((api-data-full
              (aio-await
               (elfeed-tube--aio-fetch
                (concat (aio-await (elfeed-tube--get-invidious-url)) api-path)
                #'elfeed-tube--nrotate-invidious-servers)))
             (api-data (progn (cl-check-type api-data-full (and (not null) list))
                              (plist-get api-data-full :videos)))
             ((> (length api-data) 0)))
            (progn
              (setq next-page
                    (pcase feed-type
                      ('channel (plist-get api-data-full :continuation))
                      ('playlist (if page (1+ page) 2))))
              ;; FIXME this type check fails
              ;; (cl-check-type api-data elfeed-tube--fill-api-data)
              (elfeed-tube-log 'debug "[Backfilling...][Fetched: %d entries]" (length api-data))
              (vconcat
               (cl-delete-if   ;remove entries already in db
                (lambda (elt) (member (plist-get elt :videoId) feed-entry-video-ids))
                api-data)
               (if next-page
                   (aio-await (elfeed-tube--fill-feed feed next-page))
                 (make-vector 0 0))))
          (make-vector 0 0)))
    (elfeed-tube-log 'error "[Malformed/Not Youtube feed: %s][%s]" feed-title feed-url)))

;; api-data: vector(plist entries for feed videos) -> vector(plist entries for
;; feed videos with correct dates.)
(aio-defun elfeed-tube--fill-feed-dates (api-data)
  "Add or correct dates for videos in API-DATA.

API-DATA is a vector of plists, one per video. This function
returns a vector of plists with video publish dates
corrected/added as the value of the plist's :published key."
  (cl-check-type api-data elfeed-tube--fill-api-data)
  (let ((date-queries)
        (feed-videos-map (make-hash-table :test 'equal))
        (fix-count 0)
        (total-count (length api-data)))

    (if (= total-count 0)
        api-data
      (progn
        (elfeed-tube-log 'debug "[Fixing publish dates]")
        (cl-loop for video-plist across api-data
                 for num upfrom 0
                 for video-id = (plist-get video-plist :videoId)
                 do (puthash video-id video-plist feed-videos-map)
                 when (= (mod num 10) 0) do (aio-await (aio-sleep 1 nil))
                 do (elfeed-tube--fill-progress-update num)
                 do (push (elfeed-tube--with-label
                           video-id #'elfeed-tube--aio-fetch
                           (concat (aio-wait-for (elfeed-tube--get-invidious-url))
                                   elfeed-tube--api-videos-path
                                   video-id "?fields=published"))
                          date-queries))

        (dolist (promise (nreverse date-queries))
          (pcase-let* ((`(,video-id . ,corrected-date) (aio-await promise))
                       (video-plist (gethash video-id feed-videos-map)))
            
            (elfeed-tube--fill-progress-update (- total-count fix-count) 'redisplay)
            (cl-incf fix-count)
            (plist-put video-plist :published (plist-get corrected-date :published))))

        (elfeed-tube-log 'debug "[Fixed publish dates for %d videos]" fix-count)

        (vconcat (hash-table-values feed-videos-map))))))

;; Back-fill GUI

(defsubst elfeed-tube--fill-tags-strings (taglist)
  "Convert a list of tags TAGLIST to a comma separated string."
  (mapconcat
   (lambda (s) (propertize (symbol-name s)
                      'face 'elfeed-search-tag-face))
   taglist ","))

(defun elfeed-tube--fill-display-feeds (feeds)
  "Produce a summary of Elfeed FEEDS to be back-filled.

Back-filling a YouTube feed will fetch all its videos not
presently available in its RSS feed or in the Elfeed database."
  (let ((buffer (get-buffer-create "*Elfeed-Tube Channels*")))
    (with-current-buffer buffer
      (let ((inhibit-read-only t)) (erase-buffer))
      (elfeed-tube-channels-mode)

      (setq tabulated-list-use-header-line t ; default to no header
            header-line-format nil
            ;; tabulated-list--header-string nil
            tabulated-list-format
            '[("Channel" 22 t)
              ("#Entries" 10 t)
              ("Tags to apply" 30 nil)
              ("Feed URL" 30 nil)])

      (setq
       tabulated-list-entries
       (cl-loop for feed in feeds
                for n upfrom 1
                for feed-url = (elfeed-feed-url feed)
                for channel-id = (progn (string-match "=\\(.*?\\)$" feed-url)
                                        (match-string 1 feed-url))
                for feed-title = (list (propertize (elfeed-feed-title feed)
                                                   'feed feed)
                                       'mouse-face 'highlight
                                       'action
                                       #'elfeed-tube-add--visit-channel
                                       'follow-link t
                                       'help-echo
                                       (or (and channel-id
                                                (concat
                                                 "https://www.youtube.com/channel/"
                                                 channel-id))
                                           ""))
                for feed-count = (number-to-string (length (elfeed-feed-entries feed)))
                for feed-tags = (if-let ((taglist
                                          (alist-get (elfeed-feed-id feed)
                                                     elfeed-tube--fill-tags nil t #'equal)))
                                    (elfeed-tube--fill-tags-strings taglist)
                                  (propertize "unread" 'face 'elfeed-search-tag-face))
                collect
                `(,n
                  [,feed-title
                   ,feed-count
                   ,feed-tags
                   ,feed-url])))

      (tabulated-list-init-header)
      (tabulated-list-print)
      (goto-address-mode 1)

      (goto-char (point-max))
      (let ((inhibit-read-only t)
            (continue (propertize "C-c C-c" 'face 'help-key-binding))
            (cancel-q (propertize "q" 'face 'help-key-binding))
            (cancel   (propertize "C-c C-k" 'face 'help-key-binding)))

        (let ((inhibit-message t)) (toggle-truncate-lines 1))
        (insert "\n")
        (insert
         "      " (propertize "t" 'face 'help-key-binding)
         " or "   (propertize "+" 'face 'help-key-binding)
         ": Set tags to apply to back-filled entries for feed.\n\n"
         "     " continue ": Add All (historical) videos from these channels to Elfeed.\n"
         cancel-q " or " cancel ": Quit and cancel this operation.\n"))

      (goto-char (point-min))

      (use-local-map (copy-keymap elfeed-tube-channels-mode-map))
      (local-set-key (kbd "C-c C-c") #'elfeed-tube--fill-confirm)
      (local-set-key (kbd "+")       #'elfeed-tube--fill-tags-add)
      (local-set-key (kbd "t")       #'elfeed-tube--fill-tags-add)

      (display-buffer
       buffer `(nil
                (window-height . ,#'fit-window-to-buffer)
                (body-function . ,#'select-window))))))

(defun elfeed-tube--fill-tags-add ()
  "Add tags to back-filled entries fetched for feed at point."
  (interactive)
  (when-let* ((entry (tabulated-list-get-entry))
              (feed (thread-last (aref entry 0)
                                 (car)
                                 (get-text-property 0 'feed)))
              (title (elfeed-feed-title feed))
              (id    (elfeed-feed-id feed))
              (tags  (read-from-minibuffer
                      (format "Add tags for \"%s\" (comma separated): " title)
                      (thread-last
                        (or (alist-get id elfeed-tube--fill-tags nil t #'equal) '(unread))
                        (mapcar #'symbol-name)
                        (funcall (lambda (tg) (string-join tg ","))))))
              (taglist (thread-last (split-string tags "," t "[ \f\t\n\r\v]+")
                                    (mapcar #'intern-soft)
                                    (elfeed-normalize-tags))))
    (setf (alist-get (elfeed-feed-id feed) elfeed-tube--fill-tags nil nil #'equal)
          taglist)
    (tabulated-list-set-col 2 (elfeed-tube--fill-tags-strings taglist))))

(aio-defun elfeed-tube--fill-confirm ()
  "Back-fill video entries for the displayed Elfeed feeds."
  (interactive)
  (cl-assert (derived-mode-p 'elfeed-tube-channels-mode))
  (cl-loop for table-entry in tabulated-list-entries
           for feed-title = (car (aref (cadr table-entry) 0))
           collect (get-text-property 0 'feed feed-title) into feeds
           finally do (elfeed-tube-log 'debug "[(fill-confirm-feeds): %S]"
                                       (mapcar #'elfeed-feed-title feeds))
           finally do
           (progn
             (quit-window 'kill-buffer)
             (message "Backfilling YouTube feeds...")
             (aio-await (elfeed-tube--fill-feeds feeds))
             (message "Backfilling Youtube feeds... done."))))

(provide 'elfeed-tube-fill)
;;; elfeed-tube-fill.el ends here
