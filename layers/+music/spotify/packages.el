;;; packages.el --- spotify Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2022 Sylvain Benner & Contributors
;;
;; Author: Brian Hicks <brian@brianthicks.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defconst spotify-packages
  '(spotify
    (helm-spotify-plus :toggle (configuration-layer/package-usedp 'helm))
    (counsel-spotify :toggle (configuration-layer/package-usedp 'ivy))))

(defun spotify/init-spotify ()
  (use-package spotify
    :defer t
    :init
    (spacemacs/declare-prefix
      "am"  "music"
      "ams" "Spotify")
    (spacemacs/set-leader-keys
      "amsp" 'spotify-playpause
      "amsn" 'spotify-next
      "amsN" 'spotify-previous
      "amsQ" 'spotify-quit)))

(defun spotify/init-helm-spotify-plus ()
  (use-package helm-spotify-plus
    :defer t
    :init (spacemacs/set-leader-keys "amsg" 'helm-spotify-plus)))

(defun spotify/init-counsel-spotify ()
  (use-package counsel-spotify
    :defer t
    :commands (counsel-spotify-search-artist
               counsel-spotify-search-album
               counsel-spotify-search-track
               counsel-spotify-search-tracks-by-artist
               counsel-spotify-search-tracks-by-album)
    :init
    (spacemacs/declare-prefix
      "amss"  "search"
      "amssT" "tracks")
    (spacemacs/set-leader-keys
      "amssa" 'counsel-spotify-search-artist
      "amssA" 'counsel-spotify-search-album
      "amsst" 'counsel-spotify-search-track
      "amssTa" 'counsel-spotify-search-tracks-by-artist
      "amssTA" 'counsel-spotify-search-tracks-by-album)))
