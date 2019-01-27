;;; config.el --- backup layer config file for Spacemacs.
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
;;
;; Author: Igor Kupczyński <igor@kupczynski.info>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; based on https://github.com/syl20bnr/spacemacs/issues/8947

;;; Variables:
(defvar backup-per-save-directory "~/.emacs.d/.cache/backups/per-save"
  "Directory to store per-save backups")

(defvar backup-per-session-directory "~/.emacs.d/.cache/backups/per-session"
  "Directory to store per-session backups")

;;; Defaults:
(setq make-backup-files t)     ;; Enable backups

(setq version-control t        ;; Use version numbers for backups.
      kept-new-versions 10     ;; Number of newest versions to keep.
      kept-old-versions 0      ;; Number of oldest versions to keep.
      delete-old-versions t    ;; Don't ask to delete excess backup versions.
      backup-by-copying t)     ;; Copy all files, don't rename them.
(setq vc-make-backup-files t)  ;; Do backups also for version controlled files

(setq backup-directory-alist
      `(("" . ,backup-per-save-directory)))

;;; config.el ends here
