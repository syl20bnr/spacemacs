;;; helm-epa.el --- helm interface for epa/epg  -*- lexical-binding: t; -*-

;; Copyright (C) 2012 ~ 2023 Thierry Volpiatto <thievol@posteo.net>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

(require 'helm)

(eval-when-compile (require 'epg))
(defvar epa-protocol)
(defvar epa-last-coding-system-specified)
(defvar epg-key-validity-alist)
(defvar mail-header-separator)
(declare-function epg-list-keys             "epg")
(declare-function epg-make-context          "epg")
(declare-function epg-key-sub-key-list      "epg")
(declare-function epg-sub-key-id            "epg")
(declare-function epg-key-user-id-list      "epg")
(declare-function epg-user-id-string        "epg")
(declare-function epg-user-id-validity      "epg")
(declare-function epa-sign-region           "epa")
(declare-function epa--read-signature-type  "epa")
(declare-function epa-display-error         "epa")
(declare-function epg-export-keys-to-string "epg")
(declare-function epg-context-armor         "epg")
(declare-function epg-context-set-armor     "epg")
(declare-function epg-delete-keys           "epg")
(declare-function helm-read-file-name       "helm-mode")

(defvar helm-epa--list-only-secrets nil
  "[INTERNAL] Used to pass MODE argument to `epg-list-keys'.")

(defcustom helm-epa-actions '(("Show key" . epa--show-key)
                              ("encrypt file with key" . helm-epa-encrypt-file)
                              ("Copy keys to kill ring" . helm-epa-kill-keys-armor)
                              ("Delete keys" . helm-epa-delete-keys))
  "Actions for `helm-epa-list-keys'."
  :type '(alist :key-type string :value-type symbol)
  :group 'helm-misc)

(defclass helm-epa (helm-source-sync)
  ((init :initform (lambda ()
                     (require 'epg)
                     (require 'epa)))
   (candidates :initform 'helm-epa-get-key-list)
   (keymap :initform 'helm-comp-read-map)
   (mode-line :initform 'helm-comp-read-mode-line))
  "Allow building helm sources for GPG keys.")

(defun helm-epa-get-key-list (&optional keys)
  "Build candidate list for `helm-epa-list-keys'."
  (cl-loop with all-keys = (or keys (epg-list-keys (epg-make-context epa-protocol)
                                                   nil helm-epa--list-only-secrets))
           for key in all-keys
           for sublist = (car (epg-key-sub-key-list key))
           for subkey-id = (epg-sub-key-id sublist)
           for uid-list = (epg-key-user-id-list key)
           for uid = (epg-user-id-string (car uid-list))
           for validity = (epg-user-id-validity (car uid-list))
           collect (cons (format " %s %s %s"
                                 (helm-aif (rassq validity epg-key-validity-alist)
                                     (string (car it))
                                   "?")
                                 (propertize
                                  subkey-id
                                  'face (cl-case validity
                                          (none 'epa-validity-medium)
                                          ((revoked expired)
                                           'epa-validity-disabled)
                                          (t 'epa-validity-high)))
                                 (propertize
                                  uid 'face 'font-lock-warning-face))
                         key)))

(cl-defun helm-epa--select-keys (prompt keys)
  "A helm replacement for `epa--select-keys'."
  (let ((result (helm :sources (helm-make-source "Epa select keys" 'helm-epa
                                 :candidates (lambda ()
                                               (helm-epa-get-key-list keys))
                                 :action (lambda (_candidate)
                                           (helm-marked-candidates)))
                      :prompt (and prompt (helm-epa--format-prompt prompt))
                      :buffer "*helm epa*")))
    (if (or (equal result "") (null result))
        (cl-return-from helm-epa--select-keys
          (error "No keys selected, aborting"))
      result)))

(defun helm-epa--format-prompt (prompt)
  (let ((split (split-string prompt "\n")))
    (if (cdr split)
        (format "%s\n(%s): "
                (replace-regexp-in-string "\\.[\t ]*\\'" "" (car split))
                (replace-regexp-in-string "\\.[\t ]*\\'" "" (cadr split)))
      (format "%s: " (replace-regexp-in-string "\\.[\t ]*\\'" "" (car split))))))

(defun helm-epa--read-signature-type ()
  "A helm replacement for `epa--read-signature-type'."
  (let ((answer (helm-read-answer "Signature type:
(n - Create a normal signature)
(c - Create a cleartext signature)
(d - Create a detached signature)"
                                  '("n" "c" "d"))))
    (helm-acase answer
      ("n" 'normal)
      ("c" 'clear)
      ("d" 'detached))))

(defun helm-epa-collect-keys-from-candidates (candidates)
  (cl-loop for c in candidates
           collect (epg-sub-key-id
                    (car (epg-key-sub-key-list c)))))

(defun helm-epa-collect-id-from-candidates (candidates)
  (cl-loop for c in candidates
           collect (epg-user-id-string
                    (car (epg-key-user-id-list c)))))

(defun helm-epa-success-message (str keys ids)
  (message str
           (mapconcat (lambda (pair)
                        (concat (car pair) " " (cdr pair)))
                      (cl-loop for k in keys
                               for i in ids
                               collect (cons k i))
                      "\n")))

;;;###autoload
(define-minor-mode helm-epa-mode
  "Enable helm completion on gpg keys in epa functions."
  :group 'helm-misc
  :global t
  (require 'epa)
  (if helm-epa-mode
      (progn
        (advice-add 'epa--select-keys :override #'helm-epa--select-keys)
        (advice-add 'epa--read-signature-type :override #'helm-epa--read-signature-type))
    (advice-remove 'epa--select-keys #'helm-epa--select-keys)
    (advice-remove 'epa--read-signature-type #'helm-epa--read-signature-type)))

(defun helm-epa-action-transformer (actions _candidate)
  "Helm epa action transformer function."
  (cond ((with-helm-current-buffer
           (derived-mode-p 'message-mode 'mail-mode))
         (helm-append-at-nth
          actions '(("Sign mail with key" . helm-epa-mail-sign)
                    ("Encrypt mail with key" . helm-epa-mail-encrypt))
          3))
        (t actions)))

(defun helm-epa-delete-keys (_candidate)
  "Delete gpg marked keys from helm-epa."
  (let ((context (epg-make-context epa-protocol))
        (keys (helm-marked-candidates)))
    (message "Deleting gpg keys..")
    (condition-case error
	(epg-delete-keys context keys)
      (error
       (epa-display-error context)
       (signal (car error) (cdr error))))
    (message "Deleting gpg keys done")))
  
(defun helm-epa-encrypt-file (_candidate)
  "Select a file to encrypt with key CANDIDATE."
  (let* ((file (helm-read-file-name "Encrypt file: "))
         (cands (helm-marked-candidates))
         (keys (helm-epa-collect-keys-from-candidates cands))
         (ids  (helm-epa-collect-id-from-candidates cands)))
    (epa-encrypt-file file cands)
    (helm-epa-success-message "File encrypted with key(s):\n %s"
                              keys ids)))

(defun helm-epa-kill-keys-armor (_candidate)
  "Copy marked keys to kill ring."
  (let ((keys (helm-marked-candidates))
        (context (epg-make-context epa-protocol)))
    (with-no-warnings
      (setf (epg-context-armor context) t))
    (condition-case error
	(kill-new (epg-export-keys-to-string context keys))
      (error
       (epa-display-error context)
       (signal (car error) (cdr error))))))

(defun helm-epa-mail-sign (candidate)
  "Sign email with key CANDIDATE."
  (let ((key (epg-sub-key-id (car (epg-key-sub-key-list candidate))))
        (id  (epg-user-id-string (car (epg-key-user-id-list candidate))))
        start end mode)
    (save-excursion
      (goto-char (point-min))
      (if (search-forward mail-header-separator nil t)
	  (forward-line))
      (setq epa-last-coding-system-specified
	    (or coding-system-for-write
	        (select-safe-coding-system (point) (point-max))))
      (let ((verbose current-prefix-arg))
        (setq start (point)
              end (point-max)
              mode (if verbose
		       (epa--read-signature-type)
	             'clear))))
    ;; TODO Make non-interactive functions to replace epa-sign-region
    ;; and epa-encrypt-region and inline them.
    (with-no-warnings
      (epa-sign-region start end candidate mode))
    (message "Mail signed with key `%s %s'" key id)))

(defun helm-epa-mail-encrypt (_candidate)
  "Encrypt email with key CANDIDATE."
  (let ((cands (helm-marked-candidates))
        start end)
    (save-excursion
      (goto-char (point-min))
      (when (search-forward mail-header-separator nil t)
	(forward-line))
      (setq start (point)
            end (point-max))
      (setq epa-last-coding-system-specified
	    (or coding-system-for-write
		(select-safe-coding-system start end))))
    ;; Don't let some read-only text stop us from encrypting.
    (let ((inhibit-read-only t)
          (keys (helm-epa-collect-keys-from-candidates cands))
          (ids  (helm-epa-collect-id-from-candidates cands)))
      (with-no-warnings
        (epa-encrypt-region start end cands nil nil))
      (helm-epa-success-message "Mail encrypted with key(s):\n %s"
                                keys ids))))

;;;###autoload
(defun helm-epa-list-keys ()
  "List all gpg keys.
This is the helm interface for `epa-list-keys'."
  (interactive)
  (helm :sources
        (helm-make-source "Epg list keys" 'helm-epa
          :action-transformer 'helm-epa-action-transformer
          :action 'helm-epa-actions)
        :buffer "*helm epg list keys*"))

(provide 'helm-epa)

;;; helm-epa.el ends here
