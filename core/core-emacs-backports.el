;;; core-emacs-backports.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(when (and (version<  "25" emacs-version)
           (version< emacs-version "26.3"))
  ;; Hack to prevent TLS error with Emacs 26.1 and 26.2 and gnutls 3.6.4 and
  ;; above see https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341
  (message (concat "Testing if your Emacs version %s and GnuTLS version "
                   "need the TLS work-around...")
           emacs-version)
  (message "More info at https://debbugs.gnu.org/cgi/bugreport.cgi?bug=34341")
  (let ((stime (current-time)))
    (with-temp-buffer
      (ignore-errors (call-process "gnutls-cli" nil t nil "--version"))
      (unless (string-empty-p (buffer-string))
        (goto-char (point-min))
        (let ((gnutls-version-line (thing-at-point 'line t)))
          (when (string-match ".*\\([0-9]+\\.[0-9]+\\.[0-9]+\\).*"
                              gnutls-version-line)
            (let ((gnutls-version (match-string 1 gnutls-version-line)))
              (unless (version<= gnutls-version "3.6.3")
                (message (concat "Your Emacs version %s and GnutTLS version %s "
                                 "need the work-around, applying it...")
                         emacs-version gnutls-version)
                (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")))))))))

(provide 'core-emacs-backports)
