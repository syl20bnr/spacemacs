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
        spotify
        (helm-spotify-plus :requires helm)
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
