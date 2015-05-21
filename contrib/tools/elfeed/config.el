;;; config.el --- elfeed Layer extensions File for Spacemacs
;;
;; Copyright (c) 2012-2015 Sylvain Benner
;; Copyright (c) 2015- Uri Sharf & Contributors
;;
;; Author: Uri Sharf <uri.sharf@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Variables

(defvar elfeed-feeds nil
  "List of all feeds that Elfeed should follow. You must add your
feeds to this list.")

(defvar url-queue-timeout 5
  "How long to let a job live once it's started (in seconds).")

(defvar elfeed-search-filter "@1-week-ago +unread "
  "Query string filtering shown entries.")

(defvar elfeed-web-enabled-on-emacs-startup nil
  "If true, serve web interface Elfeed with simpl-httpd.")

(defvar rmh-elfeed-org-files nil
  "The files where we look to find trees with the
  `rmh-elfeed-org-tree-id'.")
