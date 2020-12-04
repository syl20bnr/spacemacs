;;; core-load-paths.el --- Space-macs Core File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3
(defun add-to-load-path (dir) (add-to-list 'load-path dir))

(defun add-to-load-path-if-exists (dir)
  "If DIR exists in the file system, add it to `load-path'."
  (when (file-exists-p dir)
      (add-to-load-path dir)))

;; paths
(defvar space-macs-start-directory
  user-e-macs-directory
  "Space-macs start directory.")
(defconst space-macs-core-directory
  (expand-file-name (concat space-macs-start-directory "core/"))
  "Space-macs core directory.")
(defconst space-macs-private-directory
  (expand-file-name (concat space-macs-start-directory "private/"))
  "Space-macs private directory.")
(defconst space-macs-info-directory
  (expand-file-name (concat space-macs-core-directory "info/"))
  "Space-macs info files directory")
(defconst space-macs-release-notes-directory
  (expand-file-name (concat space-macs-info-directory "release-notes/"))
  "Space-macs release notes directory")
(defconst space-macs-banner-directory
  (expand-file-name (concat space-macs-core-directory "banners/"))
  "Space-macs banners directory.")
(defconst space-macs-banner-official-png
  (expand-file-name (concat space-macs-banner-directory "img/space-macs.png"))
  "Space-macs official banner image.")
(defconst space-macs-badge-official-png
  (expand-file-name (concat space-macs-banner-directory
                            "img/space-macs-badge.png"))
  "Space-macs official badge image.")
(defconst space-macs-purple-heart-png
  (expand-file-name (concat space-macs-banner-directory "img/heart.png"))
  "Purple heart emoji.")
(defconst space-macs-cache-directory
  (expand-file-name (concat user-e-macs-directory ".cache/"))
  "Space-macs storage area for persistent files")
(defconst space-macs-auto-save-directory
  (expand-file-name (concat space-macs-cache-directory "auto-save/"))
  "Space-macs auto-save directory")
(defconst space-macs-docs-directory
  (expand-file-name (concat space-macs-start-directory "doc/"))
  "Space-macs documentation directory.")
(defconst space-macs-news-directory
  (expand-file-name (concat space-macs-start-directory "news/"))
  "Space-macs News directory.")
(defconst space-macs-assets-directory
  (expand-file-name (concat space-macs-start-directory "assets/"))
  "Space-macs assets directory.")
(defconst space-macs-test-directory
  (expand-file-name (concat space-macs-start-directory "tests/"))
  "Space-macs tests directory.")

(defconst user-home-directory
  (expand-file-name "~/")
  "User home directory (~/).")
(defconst pcache-directory
  (concat space-macs-cache-directory "pcache/"))
(unless (file-exists-p space-macs-cache-directory)
    (make-directory space-macs-cache-directory))

;; load paths
(mapc 'add-to-load-path
      `(
        ,space-macs-core-directory
        ,(concat space-macs-core-directory "libs/")
        ,(concat space-macs-core-directory "libs/space-macs-theme/")
        ;; ,(concat space-macs-core-directory "aprilfool/")
        ))

;; themes
(add-to-list 'custom-theme-load-path (concat space-macs-core-directory
                                             "libs/space-macs-theme/"))



