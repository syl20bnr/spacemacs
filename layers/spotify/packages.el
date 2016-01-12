;;; packages.el --- spotify Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Brian Hicks <brian@brianthicks.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar spotify-packages '(spotify helm-spotify))

(defun spotify/init-spotify ()
  (use-package spotify
    :config (spacemacs/set-leader-keys
              "amsp" 'spotify-playpause
              "amsn" 'spotify-next
              "amsN" 'spotify-previous
              "amsQ" 'spotify-quit)))

(defun spotify/init-helm-spotify ()
  (use-package helm-spotify
    :config (spacemacs/set-leader-keys
              "amsg" 'helm-spotify)))
