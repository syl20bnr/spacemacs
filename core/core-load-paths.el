;;; core-load-paths.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(defun add-to-load-path (dir) (add-to-list 'load-path dir))

;; paths
(defconst spacemacs-core-directory
  (expand-file-name (concat user-emacs-directory "core/"))
  "Spacemacs core directory.")
(defconst spacemacs-info-directory
  (expand-file-name (concat spacemacs-core-directory "info/"))
  "Spacemacs info files directory")
(defconst spacemacs-release-notes-directory
  (expand-file-name (concat spacemacs-info-directory "release-notes/"))
  "Spacemacs release notes directory")
(defconst spacemacs-banner-directory
  (expand-file-name (concat spacemacs-core-directory "banners/"))
  "Spacemacs banners directory.")
(defconst spacemacs-banner-official-png
  (expand-file-name (concat spacemacs-banner-directory "img/spacemacs.png"))
  "Spacemacs official banner image.")
(defconst spacemacs-cache-directory
  (expand-file-name (concat user-emacs-directory ".cache/"))
  "Spacemacs storage area for persistent files")
(defconst spacemacs-auto-save-directory
  (expand-file-name (concat spacemacs-cache-directory "auto-save/"))
  "Spacemacs auto-save directory")
(defconst spacemacs-docs-directory
  (expand-file-name (concat user-emacs-directory "doc/"))
  "Spacemacs documentation directory.")
(defconst spacemacs-test-directory
  (expand-file-name (concat user-emacs-directory "tests/"))
  "Spacemacs tests directory.")

(defconst user-home-directory
  (expand-file-name "~/")
  "User home directory (~/).")
(defconst pcache-directory
  (concat spacemacs-cache-directory "pcache"))
(unless (file-exists-p spacemacs-cache-directory)
    (make-directory spacemacs-cache-directory))

(defconst user-dropbox-directory
  (expand-file-name (concat user-home-directory "Dropbox/"))
  "Dropbox directory.")

;; load paths
(mapc 'add-to-load-path
      `(
        ,(concat user-emacs-directory "core/")
        ,(concat user-emacs-directory "core/libs/")
        ,(concat user-dropbox-directory "emacs/")
        ))
