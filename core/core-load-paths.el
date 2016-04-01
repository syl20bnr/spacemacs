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
(defconst spacemacs-banner-directory
  (expand-file-name (concat spacemacs-core-directory "banners/"))
  "Spacemacs banners directory.")
(defconst user-home-directory
  (expand-file-name "~/")
  "User home directory (~/).")
(defconst spacemacs-directory
  (expand-file-name (concat user-emacs-directory "spacemacs/"))
  "Spacemacs base directory.")
(defconst spacemacs-cache-directory
  (expand-file-name (concat user-emacs-directory ".cache/"))
  "Spacemacs storage area for persistent files.")
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
