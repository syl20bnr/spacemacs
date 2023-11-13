;;; evil-collection-lms.el --- Evil bindings for lms.el -*- lexical-binding: t -*-
;; Copyright (C) 2021-2023 Edgar Vincent

;; Author: Edgar Vincent <e-v@posteo.net>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: evil, multimedia, music, logitechmediaserver, squeezebox

;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Evil bindings for lms.el.
;;
;;; Code:
(require 'lms nil t)
(require 'evil-collection)

(defvar lms-ui-docs)
(defvar lms-ui-playing-now-mode-map)
(defvar lms-ui-players-mode-map)
(defvar lms-ui-playlist-mode-map)
(defvar lms-ui-track-info-mode-map)
(defvar lms-ui-tracks-list-mode-map)
(defvar lms--ui-editing-p)
(defvar lms--ui-pl-tracks)

(declare-function lms-ui-playing-now-refresh "lms")

(defconst evil-collection-lms-maps '(lms-ui-players-mode-map
                                     lms-ui-playing-now-mode-map
                                     lms-ui-playlist-mode-map
                                     lms-ui-track-info-mode-map
                                     lms-ui-tracks-list-mode-map
                                     lms-ui-year-album-artist-list-mode-map))

(defconst evil-collection-lms-modes '(lms-players-mode
                                      lms-ui-playing-now-mode
                                      lms-ui-track-info-mode
                                      lms-ui-players-mode
                                      lms-ui-playlist-mode
                                      lms-ui-tracks-list-mode
                                      lms-ui-year-album-artist-list-mode))

(defun evil-collection-lms-ui-track-info-mode-quit ()
  "Exit `lms-ui-track-info-mode'."
  (interactive)
  (kill-buffer "*LMS: Track Information*")
  (lms-ui-playing-now-refresh))

(defun evil-collection-lms-ui-players-mode-quit ()
  "Exit `lms-ui-players-mode'."
  (interactive)
  (kill-buffer (format "*LMS: Players*"))
  (lms-ui-playing-now-refresh))

(defun evil-collection-lms-ui-playlist-mode-quit ()
  "Exit `lms-ui-playlist-mode'."
  (interactive)
  (kill-buffer (format "*LMS: Playlist [%d tracks]*" (length lms--ui-pl-tracks)))
  (lms-ui-playing-now-refresh))

(defun evil-collection-lms-ui-tracks-list-mode-quit ()
  "Exit `lms-ui-tracks-list-mode'."
  (interactive)
  (kill-buffer)
  (lms-ui-playing-now-refresh))

(defalias 'evil-collection-lms-ui-year-album-artist-list-mode-quit
  'evil-collection-lms-ui-tracks-list-mode-quit
  "Exit lms-ui-tracks-list-mode.")

(defun evil-collection-lms-goto-line (count)
  "Move the point to line COUNT. By default the penultimate line."
  (interactive "P")
  (evil-goto-line count)
  (unless count
    (forward-line -1)))

(defun evil-collection-lms-next-line ()
  "Move the point to the next line, except if it is at the end of the buffer."
  (interactive)
  (forward-line 1)
  (when (eobp) (forward-line -1)))

(defun evil-collection-lms-ui-playing-now ()
  "Modify the *LMS: Playing Now* buffer to reflect evil keybinding changes."
  (setq-local buffer-read-only nil)
  (setq lms--ui-editing-p t)
  (while (search-forward-regexp "Press 'h' for help" nil 'noerror)
      (replace-match "")
      (insert (propertize "Press 'gh' or 'g?' for help" 'face '(variable-pitch (:height 0.85 :slant italic :foreground "gray40")))))
  (setq-local buffer-read-only t)
  (setq lms--ui-editing-p nil))

(defvar evil-collection-lms-ui-docs
  "#+TITLE: lms.el Documentation
#+AUTHOR: Iñigo Serna
#+DATE: 2020/06/14

* Introduction
This is an *emacs* frontend to interact with Squeezebox Server / Logitech Media Server.
Released under GPL version 3 license or later.

It requires emacs version 25 or higher.

More information on what a *squeezebox controller* is at https://inigo.katxi.org/blog/2017/07/31/lms_el.html.

Quick instructions: customize some basic parameters such as 'lms-url' and run it with *lms* or *lms-ui*.
From there, you could read complete documentation after pressing *gh* or *g?*.
You can also run 'emacsclient -e \"(lms-float)\"' to display an independent small frame.

Package should appear in [[https://melpa.org][MELPA repository]], and the code is in [[https://hg.serna.eu/emacs/lms][the code repository]] as well.

* Features
This is Squeezebox controller, i.e. a program which can handle your local music library.

Some of the features:
- Display song: title, artist, album, year, cover…
- Play, pause, stop, select next / previous song
- Control players: select player, power on/off, volume, repeat and shuffle modes
- Playlist control: list, select song, delete track, clear
- Show track information and change rating

It is not aimed to be a complete controller, as it can't - and won't - manage external sources such us BBC, Deezer, Pandora, Spotify, or TuneIn Radio.

* Configuration
There are some parameters you could customize:
|--------------------------+-----------------------------------------------+------------------------------------|
| Parameter                | Description                                   | Default                            |
|--------------------------+-----------------------------------------------+------------------------------------|
| lms-url                  | Logitech Media Server hostname or ip and port | http://lms_server:9000             |
| lms-default-player       | Name of default player                        | nil  (1)                           |
| lms-ui-cover-width       | Cover image width                             | 500  (2)                           |
| lms-ui-update-interval   | Time in seconds between UI updates            | 1    (3)                           |
| lms-number-recent-albums | Number of recent albums to show               | 25                                 |
| lms-number-random-albums | Number of random albums to show               | 25                                 |
| lms-number-random-songs  | Number of random songs to show                | 50                                 |
| lms-set-rating-function  | Function to use to set song rating            | lms--set-rating-with-trackstat (4) |
|--------------------------+-----------------------------------------------+------------------------------------|
Notes:
(1) If *lms-default-player* is not defined or a player with that name does not exist, it will ask for one at start.
(2) It's recomendable not to change *lms-ui-cover-width*.
(3) Note that small values in *lms-ui-update-interval* could freeze your Emacs use while refreshing window.
(4) LMS does not have any means to set the rating of a song by itself, so it depends on an external plugin.
    TrackStat (function *lms--set-rating-with-trackstat*) is a popular one, and RatingsLight (function *lms--set-rating-with-ratingslight*) is another option.
** Faces
The colors and font attributes of text can be customized in some views:
|----------------------------+-------------------------------+---------------------|
| Face name                  | Description                   | Default             |
|----------------------------+-------------------------------+---------------------|
| lms-playing-face           | Playing symbol                | DarkTurquoise, bold |
| lms-title-face             | Song title                    | SlateGray, italic   |
| lms-artist-face            | Artist                        | RosyBrown, bold     |
| lms-year-face              | Song year                     | SteelBlue           |
| lms-album-face             | Album                         | CadetBlue           |
| lms-tracknum-face          | Track number                  | gray40              |
| lms-duration-face          | Song duration                 | gray60              |
| lms-players-selected-face  | Selected icon in players list | SteelBlue           |
| lms-players-isplaying-face | Isplaying in players list     | RosyBrown           |
| lms-players-name-face      | Player name in players list   | CadetBlue           |
| lms-players-model-face     | Player model in players list  | SlateGray           |
| lms-players-playerid-face  | Player id in players list     | gray60              |
| lms-players-ip-face        | Player IP in players list     | gray40              |
| lms-players-power-face     | Ispower in players list       | Maroon              |
|----------------------------+-------------------------------+---------------------|

* Playing now
Main window showing information about current track and player status.
The actions triggered by pressing keys refer to the current track.
** Key bindings
|-----------------+--------------------------------|
| Ctrl-p          | select player                  |
| t               | change player power state      |
| R               | change track rating            |
| Ctrl-=          | set volume                     |
| x, <enter>      | toggle play/pause              |
| p               | play                           |
| s               | stop playing                   |
| Ctrl-k, <left>  | play previous song in playlist |
| Ctrl-j, <right> | play next song in playlist     |
| m               | toggle mute volume             |
| +, =            | volume up +5                   |
| -               | volume down -5                 |
| r               | cycle repeat mode              |
| S               | cycle shuffle mode             |
| gr              | update window contents         |
| i               | display track information      |
| P               | display playlist               |
| T               | show all albums by artist      |
| L               | show all tracks of album       |
| Y               | show all albums of this year   |
| M               | browse music libray            |
| gh, g?          | show this documentation        |
| q               | quit LMS                       |
|-----------------+--------------------------------|

* Track information
Display track information.
Previous/next track only works when *Track information* window was called from a list, but not from *Playing now*.
** Key bindings
|-----------------+-------------------------|
| R               | change track rating     |
| Ctrl-k, <left>  | show previous track     |
| Ctrl-j, <right> | show next track         |
| gh, g?          | show this documentation |
| q               | close window            |
|-----------------+-------------------------|

* Players list
Players list.
** Key bindings
|--------------------+--------------------------------|
| k, <up>, j, <down> | move cursor                    |
| <enter>            | select player and close window |
| x                  | toggle player play/pause       |
| t                  | toggle player power state      |
| gh, g?             | show this documentation        |
| q                  | close window                   |
|--------------------+--------------------------------|

* Playlist
Playlist view.
The actions triggered by pressing keys refer to the track under cursor.
** Key bindings
|--------------------+------------------------------------|
| k, <up>, j, <down> | move cursor                        |
| <enter>            | play track                         |
| i                  | show track information             |
| C                  | jump to current track              |
| d, <delete>        | remove track from playlist         |
| c c                | clear playlist                     |
| c u                | remove tracks from start to cursor |
| c f                | remove tracks from cursor to end   |
| u                  | update window contents             |
| A                  | show all albums by artist          |
| T                  | show all tracks of album           |
| Y                  | show all albums of this year       |
| gh, g?             | show this documentation            |
| q                  | close window                       |
|--------------------+------------------------------------|

* Year - Album - Artist list
View all albums of an artist, sorted by date/year.
The actions triggered by pressing keys refer to the album under cursor.
** Key bindings
|--------------------+------------------------------|
| k, <up>, j, <down> | move cursor                  |
| <enter>, a         | show all tracks of album     |
| A                  | show all albums by artist    |
| Y                  | show all albums of this year |
| p                  | add album to playlist        |
| gh, g?             | show this documentation      |
| q                  | close window                 |
|--------------------+------------------------------|

* Tracks list
View list of tracks.
The actions triggered by pressing keys refer to the track under cursor.
** Key bindings
|--------------------+------------------------------|
| j, <up>, k, <down> | move cursor                  |
| <enter>, i         | display track information    |
| A                  | show all albums by artist    |
| Y                  | show all albums of this year |
| p                  | add song to playlist         |
| P                  | add all songs to playlist    |
| gh, g?             | show this documentation      |
| q                  | close window                 |
|--------------------+------------------------------|
"
  "LMS documentation.")

;;;###autoload
(defun evil-collection-lms-setup ()
  "Set up `evil' bindings for `lms'."

  (dolist (map evil-collection-lms-maps)
    (evil-collection-inhibit-insert-state map))

  (dolist (mode evil-collection-lms-modes)
    (evil-set-initial-state mode 'normal))

  (evil-collection-define-key 'normal 'lms-ui-playing-now-mode-map
    "t"              'lms-ui-playing-now-change-player-power-state
    (kbd "C-p")      'lms-ui-playing-now-players-list
    "R"              'lms-ui-playing-now-change-rating
    (kbd  "C-=")     'lms-ui-playing-now-set-volume
    (kbd "RET")      'lms-ui-playing-now-play-pause
    "x"              'lms-ui-playing-now-play-pause
    "p"              'lms-ui-playing-now-play
    "s"              'lms-ui-playing-now-stop
    (kbd "C-j")      'lms-ui-playing-now-next
    (kbd  "<right>") 'lms-ui-playing-now-next
    (kbd "C-k")      'lms-ui-playing-now-prev
    (kbd  "<left>")  'lms-ui-playing-now-prev
    "+"              'lms-ui-playing-now-volume-up
    "="              'lms-ui-playing-now-volume-up
    "-"              'lms-ui-playing-now-volume-down
    "m"              'lms-ui-playing-now-volume-mute
    "r"              'lms-ui-playing-now-cycle-repeat
    "S"              'lms-ui-playing-now-cycle-shuffle
    "gr"             'lms-ui-playing-now-refresh
    "i"              'lms-ui-playing-now-show-track-info
    "P"              'lms-ui-playing-now-show-playlist
    "T"              'lms-ui-playing-now-album-tracks-list
    "A"              'lms-ui-playing-now-artist-albums-list
    "Y"              'lms-ui-playing-now-year-albums-list
    "M"              'lms-ui-playing-now-browse-music-library
    "g?"             'lms-ui-playing-now-help
    "gh"             'lms-ui-playing-now-help
    "q"              'lms-ui-playing-now-quit)

  (evil-collection-define-key 'normal 'lms-ui-track-info-mode-map
    "R"             'lms-ui-track-info-change-rating
    (kbd "C-k")     'lms-ui-track-info-prev
    (kbd "<left>")  'lms-ui-track-info-prev
    (kbd "C-j")     'lms-ui-track-info-next
    (kbd "<right>") 'lms-ui-track-info-next
    "g?"            'lms-ui-playing-now-help
    "gh"            'lms-ui-playing-now-help
    "q"             'evil-collection-lms-ui-track-info-mode-quit)

  (evil-collection-define-key 'normal 'lms-ui-players-mode-map
    [remap evil-goto-line] 'evil-collection-lms-goto-line
    [remap end-of-defun] 'evil-collection-lms-goto-line
    [remap evil-forward-paragraph] 'evil-collection-lms-goto-line
    [remap evil-next-line] 'evil-collection-lms-next-line
    (kbd "RET") 'lms-ui-players-select
    "x" 'lms-ui-players-playpause
    "t" 'lms-ui-players-toggle-power
    "g?" 'lms-ui-playing-now-help
    "gh" 'lms-ui-playing-now-help
    "q" 'evil-collection-lms-ui-players-mode-quit)

  (evil-collection-define-key 'normal 'lms-ui-playlist-mode-map
    [remap evil-goto-line] 'evil-collection-lms-goto-line
    [remap end-of-defun] 'evil-collection-lms-goto-line
    [remap evil-forward-paragraph] 'evil-collection-lms-goto-line
    [remap evil-next-line] 'evil-collection-lms-next-line
    (kbd "RET")      'lms-ui-playlist-play
    "i"              'lms-ui-playlist-track-info
    "C"              'lms-ui-playlist-jump-to-current
    "d"              'lms-ui-playlist-delete-track
    (kbd "<delete>") 'lms-ui-playlist-delete-track
    "cc"             'lms-ui-playlist-clear
    "ct"             'lms-ui-playlist-clear-until-track
    "cf"             'lms-ui-playlist-clear-from-track
    "u"              'lms-ui-playlist
    "A"              'lms-ui-playlist-artist-albums-list
    "T"              'lms-ui-playlist-album-tracks-list
    "Y"              'lms-ui-playlist-year-albums-list
    "g?"             'lms-ui-playing-now-help
    "gh"             'lms-ui-playing-now-help
    "q"              'evil-collection-lms-ui-playlist-mode-quit)

  (evil-collection-define-key 'normal 'lms-ui-tracks-list-mode-map
    [remap evil-goto-line] 'evil-collection-lms-goto-line
    [remap end-of-defun] 'evil-collection-lms-goto-line
    [remap evil-forward-paragraph] 'evil-collection-lms-goto-line
    [remap evil-next-line] 'evil-collection-lms-next-line
    "i"         'lms-ui-tl-track-info
    (kbd "RET") 'lms-ui-tl-track-info
    "p"         'lms-ui-tl-to-playlist
    "P"         'lms-ui-tl-all-to-playlist
    "Y"         'lms-ui-tl-by-year
    "A"         'lms-ui-tl-by-artist
    "g?"        'lms-ui-playing-now-help
    "gh"        'lms-ui-playing-now-help
    "q"         'evil-collection-lms-ui-tracks-list-mode-quit)

  (evil-collection-define-key 'normal 'lms-ui-year-album-artist-list-mode-map
    [remap evil-goto-line] 'evil-collection-lms-goto-line
    [remap end-of-defun] 'evil-collection-lms-goto-line
    [remap evil-forward-paragraph] 'evil-collection-lms-goto-line
    [remap evil-next-line] 'evil-collection-lms-next-line
    "Y"         'lms-ui-yaal-by-year
    "A"         'lms-ui-yaal-by-artist
    (kbd "RET") 'lms-ui-yaal-by-album
    "a"         'lms-ui-yaal-by-album
    "p"         'lms-ui-yaal-to-playlist
    "g?"        'lms-ui-playing-now-help
    "gh"        'lms-ui-playing-now-help
    "q"         'evil-collection-lms-ui-year-album-artist-list-mode-quit)

  ;; Reflect help keybinding changes in the lms.el UI.
  (advice-add 'lms-ui-playing-now :after 'evil-collection-lms-ui-playing-now)
  (setq lms-ui-docs evil-collection-lms-ui-docs))

(provide 'evil-collection-lms)
;;; evil-collection-lms.el ends here
