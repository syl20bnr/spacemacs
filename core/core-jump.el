;;; core-jump.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2021 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(defvar spacemacs-default-jump-handlers '()
  "List of jump handlers available in every mode.")

(defvar-local spacemacs-jump-handlers '()
  "List of jump handlers local to this buffer.

Jump handlers in this list has the highest priority. A jump
handler jump-handler can be registered by making this call
from a mode hook:

\(add-to-list 'spacemacs-jump-handlers 'jump-handler\)

Handler in this list is called first so only dynamic handlers like
`lsp' should use this one. Conventional jump handlers should use
`spacemacs-jump-handlers-MODE' instead.")

(defmacro spacemacs|define-jump-handlers (mode &rest handlers)
  "Defines jump handlers for the given MODE.

This defines a variable `spacemacs-jump-handlers-MODE' to which
handlers can be added. MODE must be a major mode."
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (handlers-list (intern (format "spacemacs-jump-handlers-%S" mode))))
    `(progn
       (defvar ,handlers-list ',handlers
         ,(format (concat "List of mode-specific jump handlers for %S. "
                          "These take priority over those in "
                          "`spacemacs-default-jump-handlers'.")
                  mode))
       (with-eval-after-load 'bind-map
         (spacemacs/set-leader-keys-for-major-mode ',mode
           "gg" 'spacemacs/jump-to-definition
           "gG" 'spacemacs/jump-to-definition-other-window)))))

(defmacro spacemacs|define-reference-handlers (mode &rest handlers)
  "Defines reference handlers for the given MODE.
This defines a variable `spacemacs-reference-handlers-MODE' to which
handlers can be added, and a function added to MODE-hook which
sets `spacemacs-reference-handlers' in buffers of that mode."
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (func (intern (format "spacemacs//init-reference-handlers-%S" mode)))
        (handlers-list (intern (format "spacemacs-reference-handlers-%S" mode))))
    `(progn
       (defvar ,handlers-list ',handlers
         ,(format (concat "List of mode-specific reference handlers for %S. "
                          "These take priority over those in "
                          "`spacemacs-default-reference-handlers'.")
                  mode))
       (defun ,func ()
         (setq spacemacs-reference-handlers
               (append ,handlers-list
                       spacemacs-default-reference-handlers)))
       (add-hook ',mode-hook ',func)
       (with-eval-after-load 'bind-map
         (spacemacs/set-leader-keys-for-major-mode ',mode
           "gr" 'spacemacs/jump-to-reference)))))

(defun spacemacs//get-jump-handlers ()
  "Combine all jump handlers into a list.

They are in order: `spacemacs-jump-handlers',
`spacemacs-jump-handlers-MAJOR-MODE',
`spacemacs-default-jump-handlers'."
  (let ((handlers-major-mode-list (intern (format "spacemacs-jump-handlers-%S"
                                                  major-mode))))
    (append spacemacs-jump-handlers
            (if (boundp handlers-major-mode-list)
                (symbol-value handlers-major-mode-list)
              '())
            spacemacs-default-jump-handlers)))

(defun spacemacs/jump-to-definition ()
  "Jump to definition around point using the best tool for this action."
  (interactive)
  (catch 'done
    (let ((old-buffer (current-buffer))
          (old-point (point)))
      (dolist (-handler (spacemacs//get-jump-handlers))
        (let ((handler (if (listp -handler) (car -handler) -handler))
              (async (when (listp -handler)
                       (plist-get (cdr -handler) :async))))
          (ignore-errors
            (call-interactively handler))
          (when (or (eq async t)
                    (and (fboundp async) (funcall async))
                    (not (eq old-point (point)))
                    (not (equal old-buffer (current-buffer))))
            (throw 'done t)))))
    (message "No jump handler was able to find this symbol.")))

(defun spacemacs/jump-to-definition-other-window ()
  "Jump to definition around point in other window."
  (interactive)
  (let ((pos (point)))
    ;; since `spacemacs/jump-to-definition' can be asynchronous we cannot use
    ;; `save-excursion' here, so we have to bear with the jumpy behavior.
    (switch-to-buffer-other-window (current-buffer))
    (goto-char pos)
    (spacemacs/jump-to-definition)))

(defun spacemacs/jump-to-reference ()
  "Jump to reference around point using the best tool for this action."
  (interactive)
  (catch 'done
    (let ((old-window (selected-window))
          (old-buffer (current-buffer))
          (old-point (point)))
      (dolist (-handler spacemacs-reference-handlers)
        (let ((handler (if (listp -handler) (car -handler) -handler))
              (async (when (listp -handler)
                       (plist-get (cdr -handler) :async))))
          (ignore-errors
            (call-interactively handler))
          (when (or (eq async t)
                    (and (fboundp async) (funcall async))
                    (not (eq old-point (point)))
                    (not (equal old-buffer (window-buffer old-window))))
            (throw 'done t)))))
    (message "No reference handler was able to find this symbol.")))

(defun spacemacs/jump-to-reference-other-window ()
  "Jump to reference around point in other window."
  (interactive)
  (let ((pos (point)))
    ;; since `spacemacs/jump-to-reference' can be asynchronous we cannot use
    ;; `save-excursion' here, so we have to bear with the jumpy behavior.
    (switch-to-buffer-other-window (current-buffer))
    (goto-char pos)
    (spacemacs/jump-to-reference)))

;; Set the `:jump' property manually instead of just using `evil-define-motion'
;; in an `eval-after-load' macro invocation because doing that prevents
;; `describe-function' from correctly finding the source.
;;
;; See discussion on https://github.com/syl20bnr/spacemacs/pull/6771
(with-eval-after-load 'evil
  (evil-set-command-property 'spacemacs/jump-to-definition :jump t)
  (evil-set-command-property 'spacemacs/jump-to-reference :jump t))

(provide 'core-jump)
