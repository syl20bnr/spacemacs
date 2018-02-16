;;; funcs.el --- mu4e Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun mu4e/load-signature-from-file (file)
  "Load signature from FILE and strip separator if needed."
  (setq mu4e-compose-signature
        (with-temp-buffer
          (insert-file-contents file)
          (flush-lines message-signature-separator)
          (buffer-string))))

(defun mu4e/message-is-for-p (msg rx)
  "Check if to, cc or bcc field in MSG has any address in RX."
  (when (and msg rx)
    (or (mu4e-message-contact-field-matches msg :to rx)
        (mu4e-message-contact-field-matches msg :cc rx)
        (mu4e-message-contact-field-matches msg :bcc rx))))
