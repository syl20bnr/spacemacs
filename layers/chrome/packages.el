;;; packages.el --- Chrome Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Ben Hayden <hayden767@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq chrome-packages '(
                        edit-server
                        ))

(defun chrome/init-edit-server ()
  (message "init-edit-server" chrome-gmail-rich-text-format)
  (use-package edit-server
    :init (edit-server-start)))

;; gmail-message-mode autoloads code that drags the rest of the package in. So
;; use-package's :if is actually useless, and if we don't want it to steal the
;; composition buffer we need this outer cond to avoid ever reading in the file.
(cond
 (chrome-gmail-rich-text-format
  (progn
    (add-to-list 'chrome-packages 'gmail-message-mode)
    (defun chrome/init-gmail-message-mode ()
      (use-package gmail-message-mode
        :if chrome-gmail-rich-text-format
        :after edit-server
        :init (setq edit-server-default-major-mode 'markdown-mode)))))
 (t
  (progn
    (add-to-list 'chrome-packages 'edit-server-htmlize)
    (defun chrome/init-edit-server-htmlize ()
      (use-package edit-server-htmlize
        :if (not chrome-gmail-rich-text-format)
        :after edit-server
        :init
        (progn
          (defun chrome/gmail-compose-buffer-p ()
            (string-match-p "^mail\\.google\\.com/mail/u/" (buffer-name)))
          (defun chrome/gmail-compose-dehtmlize ()
            (when (chrome/gmail-compose-buffer-p)
              (edit-server-dehtmlize-buffer)))
          (defun chrome/gmail-compose-htmlize ()
            (when (chrome/gmail-compose-buffer-p)
              (edit-server-htmlize-buffer)))
          (add-hook 'edit-server-start-hook 'chrome/gmail-compose-dehtmlize)
          (add-hook 'edit-server-done-hook 'chrome/gmail-compose-htmlize)))))))
