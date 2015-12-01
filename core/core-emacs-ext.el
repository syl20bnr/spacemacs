;;; core-emacs-ext.el --- Spacemacs Core File
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
(require 'core-spacemacs-buffer)

;; Disclaimer:
;; The code in this file is not meant to stay for ever, they are
;; temporary fixes that we should remove as soon as a better
;; solution is found.

;; TODO remove this code as soon as we have a clean alternative.
;; A good proposal is available here:
;; https://github.com/syl20bnr/spacemacs/commit/4d87ea626dafc066d911c83538e260dd2bef762f#commitcomment-14708731
(when (and (version<= "24.3.1" emacs-version)
           (version<= emacs-version "24.5.1"))
  ;; for some reason with-eval-after-load does not work here in 24.3
  ;; maybe the backport is incorrect!
  (eval-after-load 'package
    '(progn
       (defun package-refresh-contents ()
         "Download the ELPA archive description if needed.
 This informs Emacs about the latest versions of all packages, and
 makes them available for download.

This redefinition adds a timeout of 5 seconds to contact each archive."
         (interactive)
         ;; the first part is not available before Emacs 24.4 so we just ignore
         ;; it to be safe.
         (unless (version< emacs-version "24.4")
           ;; FIXME: Do it asynchronously.
           (unless (file-exists-p package-user-dir)
             (make-directory package-user-dir t))
           (let ((default-keyring (expand-file-name "package-keyring.gpg"
                                                    data-directory)))
             (when (and package-check-signature (file-exists-p default-keyring))
               (condition-case-unless-debug error
                   (progn
                     (epg-check-configuration (epg-configuration))
                     (package-import-keyring default-keyring))
                 (error (message "Cannot import default keyring: %S" (cdr error)))))))
         (dolist (archive package-archives)
           (condition-case-unless-debug nil
               ;; add timeout here
               (with-timeout (5 (spacemacs-buffer/warning
                                 "Cannot contact archive %s (reason: timeout)"
                                 (cdr archive)))
                 (package--download-one-archive archive "archive-contents"))
             (error (message "Failed to download `%s' archive."
                             (car archive)))))
         (package-read-all-archive-contents)))))

(provide 'core-emacs-ext)
