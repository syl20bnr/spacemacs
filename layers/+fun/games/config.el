;;; config.el --- Games Layer configuration File for Spacemacs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variable

(defvar spacemacs-games-cache-directory
  (concat spacemacs-cache-directory "games/")
  "Directory where to store games data.")

;; create the game cache directory
(unless (file-exists-p spacemacs-games-cache-directory)
  (make-directory spacemacs-games-cache-directory))

(setq helm-games-list nil)
