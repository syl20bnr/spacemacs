;;; config.el --- Games Layer configuration File for Space-macs
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

;; Variable

(defvar space-macs-games-cache-directory
  (concat space-macs-cache-directory "games/")
  "Directory where to store games data.")

;; create the game cache directory
(unless (file-exists-p space-macs-games-cache-directory)
  (make-directory space-macs-games-cache-directory))

(setq helm-games-list nil)


