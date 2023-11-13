;;; elfeed-tube-mpv.el --- Control mpv from Elfeed  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Karthik Chikmagalur

;; Author: Karthik Chikmagalur <karthikchikmagalur@gmail.com>
;; version: 0.10
;; Keywords: news, hypermedia
;; Package-Requires: ((emacs "27.1") (elfeed-tube "0.10") (mpv "0.2.0"))
;; URL: https://github.com/karthink/elfeed-tube

;; SPDX-License-Identifier: UNLICENSE

;; This file is NOT part of GNU Emacs.

;;; Commentary:
;;
;; This package provides integration with the mpv video player for `elfeed-tube'
;; entries, which see.
;;
;; With `elfeed-tube-mpv' loaded, clicking on a transcript segment in an Elfeed
;; Youtube video feed entry will launch mpv at that time, or seek to that point
;; if already playing.
;;
;; It defines two commands and a minor mode:
;;
;; - `elfeed-tube-mpv': Start an mpv session that is "connected" to an Elfeed
;; entry corresponding to a Youtube video. You can use this command to start
;; playback, or seek in mpv to a transcript segment, or enqueue a video in mpv
;; if one is already playing. Call with a prefix argument to spawn a new
;; instance of mpv instead.
;;
;; - `elfeed-tube-mpv-where': Jump in Emacs to the transcript position
;; corresponding to the current playback time in mpv.
;;
;; - `elfeed-tube-mpv-follow-mode': Follow along in the transcript in Emacs to
;; the video playback.
;;
;;; Code:
(require 'pulse)
(require 'elfeed-tube)
(require 'mpv)

(defcustom elfeed-tube-mpv-options
  '(;; "--ytdl-format=bestvideo[height<=?480]+bestaudio/best"
    "--cache=yes"
    "--force-window=yes"
    ;; "--script-opts=osc-scalewindowed=2,osc-visibility=always"
    )
  "List of command line arguments to pass to mpv.

If the mpv library is available, these are appended to
`mpv-default-options'. Otherwise mpv is started with these options.

Each element in this list is a string. Examples:
- \"--cache=yes\"
- \"--osc=no\""
  :group 'elfeed-tube
  :type '(repeat string))
(defvar elfeed-tube-mpv--available-p
  (and (executable-find "mpv")
       (or (executable-find "youtube-dl")
           (executable-find "yt-dlp"))))
(defvar-local elfeed-tube-mpv--follow-p nil)
(defvar elfeed-tube-mpv--follow-timer nil)
(defvar-local elfeed-tube-mpv--overlay nil)
(defvar elfeed-tube-mpv-hook nil
  "Hook run before starting mpv playback in an elfeed-show buffer.

Each function must accept one argument, the current Elfeed
entry.")

(let ((map elfeed-tube-captions-map))
  (define-key map (kbd "RET") #'elfeed-tube-mpv)
  (define-key map [mouse-1] (elfeed-tube-captions-browse-with
                             #'elfeed-tube-mpv))
  (define-key map (kbd "C-<down-mouse-1>")
    (elfeed-tube-captions-browse-with
     (lambda (pos) (elfeed-tube-mpv pos t)))))

(setq-default
 elfeed-tube--captions-echo-message
 (defsubst elfeed-tube-mpv--echo-message (time)
   (format
    "  mouse-1: open at %s (mpv)
C-mouse-1: open at %s (mpv, new instance)
  mouse-2: open at %s (web browser)"
    time time time)))

(defsubst elfeed-tube-mpv--check-path (video-url)
  "Check if currently playing mpv video matches VIDEO-URL."
  (condition-case nil
      (apply #'string=
             (mapcar
              (lambda (s)
                (replace-regexp-in-string
                 "&t=[0-9.]*" "" s))
              (list (mpv-get-property "path")
                    video-url)))
    ('error nil)))

(defsubst elfeed-tube-mpv--set-timer (entry)
  "Start mpv position update timer for ENTRY."
  (setq elfeed-tube-mpv--follow-timer
        (run-with-timer
         4 1.5 #'elfeed-tube-mpv--follow entry)))

(defsubst elfeed-tube-mpv--overlay-clear ()
  "Clear mpv position overlay."
  (progn (when (timerp elfeed-tube-mpv--follow-timer)
           (cancel-timer elfeed-tube-mpv--follow-timer))
         (when (overlayp elfeed-tube-mpv--overlay)
           (delete-overlay elfeed-tube-mpv--overlay))))

;;;###autoload
(defun elfeed-tube-mpv (pos &optional arg)
  "Start or seek an mpv session connected to an Elfeed entry.

Call this command with point POS on an Elfeed entry in an Elfeed
Search buffer, or anywhere in an Elfeed Entry, to play the
corresponding video. When called with point in a transcript
segment, seek here or start a new session as appropriate. If a
connected mpv session for a different video is already running
enqueue this video instead.

With prefix argument ARG always start a new, unnconnected mpv
session."
  (interactive (list (point)
                     current-prefix-arg))
  (if (not elfeed-tube-mpv--available-p)
      (message "Could not find mpv + youtube-dl/yt-dlp in PATH.")
    (when-let* ((time (or (get-text-property pos 'timestamp) 0))
                (entry (or elfeed-show-entry
                           (elfeed-search-selected 'ignore-region)))
                (video-id (elfeed-tube--entry-video-id entry))
                (video-url (concat "https://youtube.com/watch?v="
                                   video-id
                                   "&t="
                                   (number-to-string (floor time))))
                (args (append elfeed-tube-mpv-options (list video-url))))
      (run-hook-with-args 'elfeed-tube-mpv-hook entry)
      ;; (pulse-momentary-highlight-one-line)
      (if (and (not arg) (require 'mpv nil t))
          (if (mpv-live-p)
              (if (elfeed-tube-mpv--check-path video-url)
                  (unless (= 0 time)
                    (mpv-seek time))
                (mpv--enqueue `("loadfile" ,video-url "append")
                              #'ignore)
                (message "Added to playlist: %s"
                         (elfeed-entry-title entry)))
            (apply #'mpv-start args)
            (message
             (concat "Starting mpv: "
                     (propertize "Connected to Elfeed ✓"
                                 'face 'success)))
            (when elfeed-tube-mpv--follow-p
              (elfeed-tube-mpv--set-timer entry)))
        (apply #'start-process
               (concat "elfeed-tube-mpv-"
                       (elfeed-tube--entry-video-id elfeed-show-entry))
               nil "mpv" args)
        (message (concat "Starting new mpv instance: "
                         (propertize "Not connected to Elfeed ❌"
                                     'face 'error)))))))

(defun elfeed-tube-mpv--follow (entry-playing)
  "Folllow the ENTRY-PLAYING in mpv in Emacs.

This function is intended to be run on a timer when
`elfeed-tube-mpv-follow-mode' is active."
  (if (not (mpv-live-p))
      (elfeed-tube-mpv--overlay-clear)
    (when-let ((entry-buf
                (or (let ((elfeed-show-unique-buffers t))
                      (get-buffer (elfeed-show--buffer-name
                                   entry-playing)))
                    (get-buffer
                     (elfeed-show--buffer-name
                      entry-playing)))))
      (when (and (or (derived-mode-p 'elfeed-show-mode)
		     (window-live-p (get-buffer-window entry-buf)))
		 (elfeed-tube--same-entry-p
                  (buffer-local-value 'elfeed-show-entry entry-buf)
		  entry-playing)
		 (eq (mpv-get-property "pause")
		     json-false))
	(condition-case nil
	    (when-let ((mpv-time (mpv-get-property "time-pos")))
	      (with-current-buffer entry-buf

		;; Create overlay
		(unless (overlayp elfeed-tube-mpv--overlay)
		  (save-excursion
		    (goto-char (point-min))
		    (text-property-search-forward
		     'timestamp)
		    (setq elfeed-tube-mpv--overlay
			  (make-overlay (point) (point)))
		    (overlay-put elfeed-tube-mpv--overlay
				 'face '(:inverse-video t))))
		
                ;; Handle narrowed buffers
                (when (buffer-narrowed-p)
                  (save-excursion
                    (let ((min (point-min))
                          (max (point-max))
                          beg end)
                      (goto-char min)
                      (setq beg (prop-match-value
                                 (text-property-search-forward
                                  'timestamp)))
                      (goto-char max)
                      (widen)
                      (setq end (prop-match-value
                                 (text-property-search-forward
                                  'timestamp)))
                      (narrow-to-region min max)
                      (cond
                       ((and beg (< mpv-time beg))
                        (mpv-set-property "time-pos" (1- beg)))
                       ((and end (> mpv-time end))
                        (mpv-set-property "time-pos" (1+ end))
                        (mpv-set-property "pause" t))))))
                
                ;; Update overlay
                (when-let ((next (elfeed-tube-mpv--where-internal mpv-time)))
                  (goto-char next)
                  (move-overlay elfeed-tube-mpv--overlay
				(save-excursion (beginning-of-visual-line) (point))
				(save-excursion (end-of-visual-line) (point))))))
	  ('error nil))))))

(defun elfeed-tube-mpv--where-internal (mpv-time)
  "Return the point in the Elfeed buffer that corresponds to time MPV-TIME."
  (save-excursion
      (while (not (get-text-property (point) 'timestamp))
        (goto-char (or (previous-single-property-change
		        (point) 'timestamp)
		       (next-single-property-change
		        (point) 'timestamp))))

      (if (> (get-text-property (point) 'timestamp)
	     mpv-time)
	  (let ((match (text-property-search-backward
		        'timestamp mpv-time
		        (lambda (mpv cur)
			  (< (or cur
			         (get-text-property
				  (1+ (point))
				  'timestamp))
			     (- mpv 1))))))
	    (goto-char (prop-match-end match))
	    (text-property-search-forward 'timestamp)
	    (min (1+ (point)) (point-max)))
        (let ((match (text-property-search-forward
		      'timestamp mpv-time
		      (lambda (mpv cur) (if cur (> cur (- mpv 1)))))))
          (prop-match-beginning match)))))

(defun elfeed-tube-mpv-where ()
  "Jump to the current mpv position in a video transcript."
  (interactive)
  (cond
   ((not (featurep 'mpv))
    (message "mpv-where requires the mpv package. You can install it with M-x `package-install' RET mpv RET."))
   ((not (and (derived-mode-p 'elfeed-show-mode)
              (elfeed-tube--youtube-p elfeed-show-entry)))
    (message "Not in an elfeed-show buffer for a Youtube video!"))
   ((not (mpv-live-p))
    (message "No running instance of mpv is connected to Emacs."))
   ((or (previous-single-property-change
	 (point) 'timestamp)
	(next-single-property-change
	 (point) 'timestamp))
    (goto-char (elfeed-tube-mpv--where-internal
                (mpv-get-property "time-pos")))
    (let ((pulse-delay 0.08)
          (pulse-iterations 16))
      (pulse-momentary-highlight-one-line)))
   (t (message "Transcript location not found in buffer."))))

;;;###autoload
(define-minor-mode elfeed-tube-mpv-follow-mode
  "Follow along with mpv in elfeed-show buffers.

This appliies to Youtube feed entries in Elfeed. When the video
player mpv is started from this buffer (from any location in the
transcript), turning on this minor-mode will cause the cursor to
track the currently playing segment in mpv. You can still click
anywhere in the transcript to seek to that point in the video."
  :global nil
  :version "0.10"
  :lighter " (-->)"
  :keymap (let ((map (make-sparse-keymap)))
            (prog1 map
              (define-key map " " #'mpv-pause)))
  :group 'elfeed-tube
  (if elfeed-tube-mpv-follow-mode
      (cond
       
       ((not (require 'mpv nil t))
        (message "mpv-follow-mode requires the mpv package. You can install it with M-x `package-install' RET mpv RET.")
        (elfeed-tube-mpv-follow-mode -1))
       
       ((not (derived-mode-p 'elfeed-show-mode))
        (message "mpv-follow-mode only works in elfeed-show buffers.")
        (elfeed-tube-mpv-follow-mode -1))
       
       (t (if-let* ((entry elfeed-show-entry)
                    (video-id (elfeed-tube--entry-video-id entry))
                    (video-url
                     (concat "https://youtube.com/watch?v="
                             video-id)))
              (if (and (mpv-live-p) (elfeed-tube-mpv--check-path video-url))
                  (elfeed-tube-mpv--set-timer entry)
                (setq-local elfeed-tube-mpv--follow-p t))
            (message "Not a youtube video buffer!")
            (elfeed-tube-mpv-follow-mode -1))))
    
    (setq-local elfeed-tube-mpv--follow-p nil)
    (when (timerp elfeed-tube-mpv--follow-timer)
      (cancel-timer elfeed-tube-mpv--follow-timer))
    (elfeed-tube-mpv--overlay-clear)))

(provide 'elfeed-tube-mpv)
;;; elfeed-tube-mpv.el ends here
