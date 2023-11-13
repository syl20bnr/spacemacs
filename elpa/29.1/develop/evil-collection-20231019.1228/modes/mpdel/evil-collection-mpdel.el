;;; evil-collection-mpdel.el --- Evil bindings for mpdel -*- lexical-binding: t -*-

;; Copyright (C) 2021 Ruoyu Feng

;; Author: Ruoyu Feng <mail@vonfry.name>
;; Maintainer: James Nguyen <james@jojojames.com>
;; Pierre Neidhardt <mail@ambrevar.xyz>
;; URL: https://github.com/emacs-evil/evil-collection
;; Version: 0.0.4
;; Package-Requires: ((emacs "26.3"))
;; Keywords: evil, tools, mpd

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
;; Evil bindings for mpdel.

;;; Code:

(require 'mpdel nil t)
(require 'evil-collection)

(defvar mpdel-core-map)

(defconst evil-collection-mpdel-maps '(mpdel-core-map))

(defconst evil-collection-mpdel-modes '(mpdel-browser-mode mpdel-playlist-mode
                                        mpdel-song-mode mpdel-browser-mode
                                        mpdel-playlist-current-playlist-mode
                                        mpdel-playlist-stored-playlist-mode))

;;;###autoload
(defun evil-collection-mpdel-setup ()
  "Set up `evil' bindings for `mpdel'."

  (evil-collection-set-readonly-bindings 'mpdel-core-map)
  (dolist (mode evil-collection-mpdel-modes)
    (evil-set-initial-state mode 'normal))
  (evil-collection-define-key 'normal 'mpdel-core-map
    "p"  'libmpdel-playback-play-pause
    "{"  'libmpdel-playback-previous
    "}"  'libmpdel-playback-next
    "a"  'mpdel-core-add-to-current-playlist
    "A"  'mpdel-core-add-to-stored-playlist
    "r"  'mpdel-core-replace-current-playlist
    "R"  'mpdel-core-replace-stored-playlist
    "i"  'mpdel-core-insert-current-playlist
    "gD" 'mpdel-core-dired
    "gA" 'mpdel-core-open-artists
    "gL" 'mpdel-core-open-stored-playlists
    "ss" 'mpdel-core-search-by-title
    "sl" 'mpdel-core-search-by-album
    "sr" 'mpdel-core-search-by-artist
    "gp" 'navigel-open-parent
    "j"  'evil-next-line
    "k"  'evil-previous-line
    "+"  'mpdel-core-volume-increase
    "-"  'mpdel-core-volume-decrease
    "C"  'libmpdel-connect-profile

    "gb" 'mpdel-browser-open

    "go" 'mpdel-song-open
    "gr" 'mpdel-song-refresh
    "I"  'mpdel-song-play
    "q"  'mpdel-song-quit-window

    ">s" 'mpdel-song-small-increment
    ">n" 'mpdel-song-normal-increment
    ">l" 'mpdel-song-large-increment
    "<s" 'mpdel-song-small-decrement
    "<n" 'mpdel-song-normal-decrement
    "<l" 'mpdel-song-large-decrement

    "gl" 'mpdel-playlist-open
    "("  'mpdel-playlist-move-up
    ")"  'mpdel-playlist-move-down
    "w"  'mpdel-playlist-save
    "P"  'mpdel-playlist-play)

  (evil-collection-define-key 'normal 'mpdel-tablist-mode-map
    "D" 'tablist-do-delete))

(provide 'evil-collection-mpdel)
;;; evil-collection-mpdel.el ends here
