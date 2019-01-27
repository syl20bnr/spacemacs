;;; funcs.el --- backup layer funcs file for Spacemacs.
;;
;; Copyright (c) 2012-2019 Sylvain Benner & Contributors
;;
;; Author: Igor Kupczyński <igor@kupczynski.info>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:
(defun backup-force-backup-of-buffer ()
  "Make sure emacs backs up buffers on every save, not just the first time.

By default, emacs backups the file only first time it is saved:

> Emacs makes a backup for a file only the first time the file is
> saved from a buffer. No matter how many times you subsequently
> save the file, its backup remains unchanged. However, if you
> kill the buffer and then visit the file again, a new backup
> file will be made.

Src: https://www.gnu.org/software/emacs/manual/html_node/emacs/Backup.html

This function, if installed as a before-save-hook, extends that
behavior to save a backup copy each time a file is
saved (per-save backup). It also preserves the first-time only
behavior (per-session backup).

per-save and per-session backups are stored in different
directories.
"
  ;; Make "per session" backup at the first save of each emacs session.
  (when (not buffer-backed-up)
    ;; Override the default parameters for per-session backups as we don't need
    ;; to keep as many copies.
    (let ((backup-directory-alist `(("" . ,backup-per-session-directory)))
          (kept-new-versions 3))
      (backup-buffer)))
  ;; Make a "per save" backup on each save. The first save results in both a
  ;; per-session and a per-save backup, to keep the numbering of per-save
  ;; backups consistent.
  (let ((buffer-backed-up nil))
    (backup-buffer)))

;;; Setup:
(add-hook 'before-save-hook  'backup-force-backup-of-buffer)
(if (eq dotspacemacs-auto-save-file-location 'original)
    (add-hook 'auto-save-hook 'backup-force-backup-of-buffer))

;;; funcs.el ends here
