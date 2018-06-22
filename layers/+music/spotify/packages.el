;;; packages.el --- spotify Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Brian Hicks <brian@brianthicks.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq spotify-packages
      '(
        (spotify :toggle (configuration-layer/package-usedp 'helm))
        (helm-spotify-plus :toggle (configuration-layer/package-usedp 'helm))
        (counsel-spotify :toggle (configuration-layer/package-usedp 'ivy))
        ))

(defun spotify/init-spotify ()
  (use-package spotify
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "am" "music")
      (spacemacs/declare-prefix "ams" "Spotify")
      (spacemacs/set-leader-keys
        "amsp" 'spotify-playpause
        "amsn" 'spotify-next
        "amsN" 'spotify-previous
        "amsQ" 'spotify-quit))))

(defun spotify/init-helm-spotify-plus ()
  (use-package helm-spotify-plus
    :defer t
    :init (spacemacs/set-leader-keys "amsg" 'helm-spotify-plus)))

(defun spotify/init-counsel-spotify ()
  (use-package counsel-spotify
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "am" "music")
      (spacemacs/declare-prefix "ams" "Spotify")
      (spacemacs/set-leader-keys
        "amsp" 'counsel-spotify-toggle-play-pause
        "amsn" 'counsel-spotify-next
        "amsN" 'counsel-spotify-previous
        "amsst" 'counsel-spotify-search-track
        "amssa" 'counsel-spotify-search-artist
        "amssA" 'counsel-spotify-search-album
        ))))

(defun spotify/post-init-counsel-spotify ()
  (load-library "counsel-spotify")
  )
