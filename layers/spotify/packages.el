;;; packages.el --- spotify Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Brian Hicks & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar spotify-packages '(spotify helm-spotify))

(defun spotify/init-spotify ()
  (use-package spotify
    :config (spacemacs/set-leader-keys
              "aMsp" 'spotify-playpause
              "aMsn" 'spotify-next
              "aMsN" 'spotify-previous
              "aMsQ" 'spotify-quit)))

(defun spotify/init-helm-spotify ()
  (use-package helm-spotify
    :config (spacemacs/set-leader-keys
              "aMsg" 'helm-spotify)))
