;; erc-sasl.el -- handle SASL PLAIN authentication

;; Copyright (C) 2012-2022 Joseph Gay

;; Author: Joseph Gay <ysph@psy.ai>
;; Keywords: comm

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file implements SASL PLAIN authentication
;; To activate:
;;
;; (require 'erc-sasl)
;;
;; (add-to-list 'erc-sasl-server-regexp-list "host\\.server\\.com")
;; e.g. irc\\.freenode\\.net, or .* for any host
;;
;; To disable:
;; (setq erc-sasl-use-sasl nil)
;;
;; NOTE: requires passing a password initially to (erc) and variants

;;; Code:

(eval-when-compile (require 'cl-lib))

(defvar erc-sasl-use-sasl t
  "Set to nil to disable SASL auth")

(defvar erc-sasl-server-regexp-list '()
  "List of regexps matching server host names for which sasl
  should be used")

(defun erc-sasl-use-sasl-p ()
  "Used internally to decide whether SASL should be used in the
current session"
  (and erc-sasl-use-sasl
       (boundp 'erc-session-server)
       (cl-loop for re in erc-sasl-server-regexp-list
             thereis (integerp (string-match re erc-session-server)))))

(define-erc-response-handler (CAP)
  "Client capability framework is used to request SASL auth, need
  to wait for ACK to begin" nil
  (let ((msg (erc-response.contents parsed)))
    (when (string-match " *sasl" msg)
      (erc-server-send "AUTHENTICATE PLAIN")
      ;; now wait for AUTHENTICATE +
      )))

(define-erc-response-handler (AUTHENTICATE)
  "Handling empty server response indicating ready to receive
  authentication." nil
  (if erc-session-password
      (let ((msg (erc-response.contents parsed)))
        (when (string= "+" msg)
          ;; plain auth
          (erc-server-send
           (format "AUTHENTICATE %s"
                   (base64-encode-string
                    (concat "\0" (erc-current-nick)
                            "\0" erc-session-password) t)))))
    (erc-display-message
     parsed 'error
     (if erc-server-connected 'active proc)
     "You must set a password in order to use SASL authentication.")
    ;; aborting SASL auth
    (erc-server-send (erc-server-send "AUTHENTICATE *"))))

(define-erc-response-handler (903)
  "Handling a successful SASL authentication." nil
  (erc-server-send "CAP END"))

(provide 'erc-sasl)

;;; erc-sasl.el ends here
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
