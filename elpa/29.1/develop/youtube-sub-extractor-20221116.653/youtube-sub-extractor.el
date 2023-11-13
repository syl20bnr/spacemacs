;;; youtube-sub-extractor.el --- Extract YouTube video subtitles  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Ag Ibragimov
;;
;; Author: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Maintainer: Ag Ibragimov <agzam.ibragimov@gmail.com>
;; Created: October 02, 2022
;; Modified: October 02, 2022
;; Version: 0.0.1
;; Keywords: convenience multimedia
;; Homepage: https://github.com/agzam/youtube-sub-extractor.el
;; Package-Requires: ((emacs "27.1"))
;;
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Extract video subtitles using https://github.com/yt-dlp and show them in a buffer.
;;
;; This package requires https://github.com/yt-dlp cmd line tool, installed and availible in $PATH
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup youtube-sub-extractor nil
  "YouTube Subtitle Extractor."
  :prefix "youtube-sub-extractor-"
  :group 'applications)

(defcustom youtube-sub-extractor-timestamps
  'right-margin
  "Method of displaying timestamps.
`left-margin' or `right-margin' - it would display them as overlays.
`leftside-text' - inserts timestamps next to subs (so they can be
copied with timestamps)
nil - timestamps will not be displayed at all."
  :group 'youtube-sub-extractor
  :type '(choice
          (symbol 'left-margin)
          (symbol 'right-margin)
          (symbol 'left-side-text)))

(defcustom youtube-sub-extractor-executable-path
  nil
  "Path to yt-dlp (preferred) or youtube-dl executable."
  :group 'youtube-sub-extractor
  :type 'string)

(defcustom youtube-sub-extractor-min-chunk-size
  5
  "Minimum number of seconds between subs."
  :group 'youtube-sub-extractor
  :type 'number)

(defcustom youtube-sub-extractor-language-choice
  t
  "Whether to prompt for the language when multiple are available.
If nil or t - will ask.  If set to specific language string, e.g.,

\"en\" - for English,
\"es\" - Spanish,
\"ar\" - Arabic,
\"pt-PT\" - Portuguese (Portugal),
\"pt-BR\" - Portuguese (Brazil), etc.

then it will not ask and quietly download subtitles for the
specified language (if available). When there are no subtitles
for the selected language, will download auto-generated English
ones."
  :group 'youtube-sub-extractor
  :type '(choice boolean string))

(defvar youtube-sub-extractor-subtitles-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'youtube-sub-extractor-copy-ts-link)
    (define-key map (kbd "C-c C-o") #'youtube-sub-extractor-browse-ts-link)
    map)
  "Keymap for minor mode variable `youtube-sub-extractor-subtitles-mode'.")

(define-minor-mode youtube-sub-extractor-subtitles-mode
  "Minor mode for youtube-sub-extractor.
\\{youtube-sub-extractor-subtitles-mode-map}"
  :group 'youtube-sub-extractor
  :lighter " yt-subs"
  :init-value nil
  :keymap youtube-sub-extractor-subtitles-mode-map)

(defvar youtube-sub-extractor--ts-rx
  "[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}.[0-9]\\{3\\}"
  "Regexp used to locate timestamps in subtitles file.")

(defun youtube-sub-extractor--seconds (ts)
  "Return total number of seconds for given timestamp TS."
  (when (and (stringp ts)
             (string-match-p youtube-sub-extractor--ts-rx ts))
    (pcase-let* ((`(,s ,m ,h) (parse-time-string ts))
                 (ms (string-to-number (substring ts 9 12))))
      (+ (* 3600 h) (* 60 m) s (/ ms 1000.0)))))

(defun youtube-sub-extractor--duration (long-ts)
  "Calculate duration in seconds in LONG-TS string.
i.e., string containing '00:00:07.200 --> 00:00:09.830'"
  (when (string-match
         (format "\\(%1$s\\) --> \\(%1$s\\)" youtube-sub-extractor--ts-rx)
         long-ts)
    (- (youtube-sub-extractor--seconds (match-string 2 long-ts))
       (youtube-sub-extractor--seconds (match-string 1 long-ts)))))

(defun youtube-sub-extractor--add (ts1 ts2)
  "Add long timestamp TS1 to another long timestamp TS2."
  (let* ((rx (format "\\(%1$s\\) --> \\(%1$s\\)"
                     youtube-sub-extractor--ts-rx))
         (start (when (string-match rx ts1)
                  (match-string 1 ts1)))
         (end (when (string-match rx ts2)
                (match-string 2 ts2))))
    (format "%s --> %s" start end)))

(defun youtube-sub-extractor--find-exe ()
  "Attempts to find the command line util to extract subs."
  (if-let ((exe (or (executable-find (or youtube-sub-extractor-executable-path "yt-dlp"))
                    (executable-find "youtube-dl"))))
      exe
    (error "ERROR: I couldn't locate yt-dlp or youtube-dl!")))

(defun youtube-sub-extractor--process-subs (subs-string)
  "Take single SUBS-STRING and return list of tuples.
Each is a timestamp, duration and the corresponding sub."
  (let* ((ts-line-rx (format "^%1$s --> %1$s\\( .*$\\|$\\)" youtube-sub-extractor--ts-rx))
         ;; let's remove all cue and decoration tags
         (tags-n-karaoke-rx "<.>\\|</.>\\|\\(<[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}.[0-9]\\{3\\}>\\)")
         ;; first we simply parse things and gather into a list of tuples
         ;; with timestamp and list of subtitles
         (first-pass
          (seq-reduce
           (lambda (acc nxt)
             (let ((prev-el (car (last acc))))
               (cond
                ;; timestamp
                ((string-match-p ts-line-rx nxt)
                 (append acc (list (list (string-trim nxt)))))
                ;; actual sub
                ((and prev-el
                      (not (string-match-p ts-line-rx nxt))
                      (listp prev-el)
                      (string-match-p ts-line-rx (car prev-el)))
                 (append
                  (butlast acc)
                  (list
                   (append
                    prev-el
                    (list
                     (thread-last
                       nxt
                       (replace-regexp-in-string tags-n-karaoke-rx "")
                       (replace-regexp-in-string "&nbsp;" "")))))))

                (t acc))))
           (split-string subs-string "\n" :omit-nulls " *")
           ()))
         ;; group subs by chunks of the size of `youtube-sub-extractor-min-chunk-size',
         ;; otherwise it splits them into pieces too small. Also, need to take care of
         ;; duplicates
         (second-pass
          (seq-reduce
           (lambda (acc nxt)
             (let* ((ts (when (string-match
                               (format "%1$s --> %1$s" youtube-sub-extractor--ts-rx)
                               (car nxt))
                          (match-string 0 (car nxt))))
                    (prev-ts (cl-first (cl-first (last acc))))
                    (subs (cdr nxt))
                    (prev-subs (cadr (cl-first (last acc)))))
               (cond
                ((and prev-ts
                      (< (+ (youtube-sub-extractor--duration prev-ts)
                            (youtube-sub-extractor--duration ts))
                         youtube-sub-extractor-min-chunk-size))
                 (append
                  (butlast acc)
                  (list
                   (list
                    (youtube-sub-extractor--add prev-ts ts)
                    (seq-uniq (append prev-subs subs))))))

                (t (append
                    acc
                    (list
                     (list ts (seq-difference
                               (seq-uniq subs)
                               prev-subs))))))))
           first-pass
           ())))
    second-pass))

(defun youtube-sub-extractor--create-subs-buffer (subs-file vid-url)
  "Read SUBS-FILE and insert the content in a buffer.
VID-URL gets used later for browsing video at specific timestamp."
  (let* ((raw (with-temp-buffer
                (insert-file-contents subs-file)
                (buffer-string)))
         (subs-lst (youtube-sub-extractor--process-subs raw))
         (buf (generate-new-buffer (file-name-base subs-file)))
         ;; if the vid shorter than hour, no need to show hours - timestamps would be s:ms
         (mins-only? (zerop (nth 2 (parse-time-string (cl-first (cl-first (last subs-lst))))))))
    (with-current-buffer buf
      (insert (format "%s\n\n" (file-name-base subs-file)))
      (dolist (el subs-lst)
        (let* ((full-ts (nth 0 el))
               (ts (substring full-ts (if mins-only? 3 0) 8))
               (sub-text (nth 1 el))
               (pos (point))
               (_ (progn
                    (when (eq youtube-sub-extractor-timestamps 'left-side-text)
                      (insert (format "%s\t" ts)))
                    (insert (format "%s" (string-join sub-text " ")))
                    (save-excursion
                      (add-text-properties
                       (line-beginning-position)
                       (line-end-position)
                       `(help-echo ,ts timestamp ,full-ts)))
                    (insert "\n")))
               (ovrl (make-overlay (1+ pos) (point) nil t))
               (ovrl-txt (or ts ""))
               (margin (if (eq youtube-sub-extractor-timestamps 'right-margin)
                           'right-margin 'left-margin)))
          (overlay-put
           ovrl 'before-string
           (propertize ovrl-txt 'display `((margin ,margin) ,ovrl-txt)))))
      (goto-char (point-min))
      (setq-local video-url vid-url)
      (youtube-sub-extractor-subtitles-mode +1)
      (read-only-mode +1))
    (switch-to-buffer-other-window buf)
    (unless (or (eq youtube-sub-extractor-timestamps 'left-side-text)
                (null youtube-sub-extractor-timestamps))
      (set-window-margins
       nil
       (when (eq youtube-sub-extractor-timestamps 'left-margin) 9)
       (when (eq youtube-sub-extractor-timestamps 'right-margin) 9)))))

(defun youtube-sub-extractor--send-request (video-url args)
  "Call cmd-line util with ARGS and VIDEO-URL."
  (message "sending request %s \"%s\"" args video-url)
  (shell-command-to-string
   (format
    "mkdir -p \"%1$s\" && cd \"%1$s\" && %2$s %3$s %4$s"
    (temporary-file-directory)
    (youtube-sub-extractor--find-exe)
    args
    (shell-quote-argument video-url))))

(defun youtube-sub-extractor--available-langs (video-url)
  "Send a request for VIDEO-URL and get list of available languages."
  (let* ((lan-lst (seq-remove
                   #'null
                   (seq-map
                    (lambda (x)
                      (when (string-match "^\\([A-z|-]+\\)\\(.*\\)vtt," x)
                        (list (match-string 1 x)
                              (string-trim (match-string 2 x)))))
                    (seq-drop
                     (seq-drop-while
                      (lambda (x)
                        (not (string-match-p "\\[info\\] Available subtitles for.*:" x)))
                      (split-string
                       (youtube-sub-extractor--send-request
                        video-url
                        "--list-subs --no-simulate --skip-download --no-playlist")
                       "\n"  :omit-nulls))
                     2)))))
    lan-lst))

(defun youtube-sub-extractor-copy-ts-link ()
  "Construct a video url at specific timestamp.
Works only in youtube-sub-extractor-mode buffer."
  (interactive)
  (let ((ts (plist-get (text-properties-at (point)) 'timestamp)))
    (when (and (boundp 'video-url) ts
               (bound-and-true-p youtube-sub-extractor-subtitles-mode)
               (string-match "\\([^?]+\\)\\(\\?\\|\\)\\(.*\\)" video-url))
      (let* ((path (match-string 1 video-url))
             (args (url-parse-query-string (match-string 3 video-url)))
             (tp (floor (youtube-sub-extractor--seconds ts)))
             (tpoint (when (numberp tp) (number-to-string tp)))
             (t-item (alist-get "t" args nil nil #'string-equal))
             (_ (if t-item (setf (car t-item) tpoint)
                  (cl-pushnew (list "t" tpoint) args)))
             (new-path (concat path "?" (url-build-query-string args))))
        (message new-path)
        (kill-new new-path)
        new-path))))

(defun youtube-sub-extractor-browse-ts-link ()
  "Browse video url at specific timestamp.
Works only in youtube-sub-extractor-mode buffer."
  (interactive)
  (browse-url (youtube-sub-extractor-copy-ts-link)))

(defun youtube-sub-extractor-extract-subs (video-url)
  "For a given YouTube vid VIDEO-URL, extract subtitles and open them in a buffer."
  (interactive (list (read-string "Enter video URL: ")))
  (let* ((langs (youtube-sub-extractor--available-langs video-url))
         (selected (cond
                    ((null langs) 'auto)

                    ((and (stringp youtube-sub-extractor-language-choice)
                          (seq-contains-p
                           (seq-map #'car langs)
                           youtube-sub-extractor-language-choice
                           #'string-equal))
                     youtube-sub-extractor-language-choice)

                    (t (completing-read "Choose the language: " langs nil t))))
         (res (youtube-sub-extractor--send-request
               video-url
               (format "--skip-download --no-playlist %s"
                       (if (equal 'auto selected)
                           "--write-auto-subs"
                         (format "--write-subs --sub-langs \"%s\"" selected)))))
         (progress (seq-drop-while
                    (lambda (x)
                      (not (string-match-p "\\[download\\] Destination.*$" x)))
                    (split-string res "\n" :omit-nulls "^ \\| $")))
         (fname (when (seq-some
                       (lambda (x) (string-match-p "\\[download\\] 100% .*" x))
                       progress)
                  (seq-some
                   (lambda (x)
                     (when (string-match "\\[download\\] Destination: \\(.*\\)" x)
                       (match-string 1 x)))
                   progress)))
         (fpath (concat (temporary-file-directory) fname)))
    (unless fname
      (error "Failed to extract subtitles, output log:\n\n%s" res))

    (youtube-sub-extractor--create-subs-buffer fpath video-url)
    (delete-file fpath)))

(provide 'youtube-sub-extractor)
;;; youtube-sub-extractor.el ends here
