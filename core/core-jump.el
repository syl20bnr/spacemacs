;;; core-jump.el --- Space-macs Core File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3

(defvar space-macs-default-jump-handlers '()
  "List of jump handlers available in every mode.")

(defvar-local space-macs-jump-handlers '()
  "List of jump handlers local to this buffer.")

(defmacro space-macs|define-jump-handlers (mode &rest handlers)
  "Defines jump handlers for the given MODE.
This defines a variable `space-macs-jump-handlers-MODE' to which
handlers can be added, and a function added to MODE-hook which
sets `space-macs-jump-handlers' in buffers of that mode."
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (func (intern (format "space-macs//init-jump-handlers-%S" mode)))
        (handlers-list (intern (format "space-macs-jump-handlers-%S" mode))))
    `(progn
       (defvar ,handlers-list ',handlers
         ,(format (concat "List of mode-specific jump handlers for %S. "
                          "These take priority over those in "
                          "`space-macs-default-jump-handlers'.")
                  mode))
       (defun ,func ()
         (setq space-macs-jump-handlers
               (append ,handlers-list
                       space-macs-default-jump-handlers)))
       (add-hook ',mode-hook ',func)
       (with-eval-after-load 'bind-map
         (space-macs/set-leader-keys-for-major-mode ',mode
           "gg" 'space-macs/jump-to-definition
           "gG" 'space-macs/jump-to-definition-other-window)))))

(defmacro space-macs|define-reference-handlers (mode &rest handlers)
  "Defines reference handlers for the given MODE.
This defines a variable `space-macs-reference-handlers-MODE' to which
handlers can be added, and a function added to MODE-hook which
sets `space-macs-reference-handlers' in buffers of that mode."
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (func (intern (format "space-macs//init-reference-handlers-%S" mode)))
        (handlers-list (intern (format "space-macs-reference-handlers-%S" mode))))
    `(progn
       (defvar ,handlers-list ',handlers
         ,(format (concat "List of mode-specific reference handlers for %S. "
                          "These take priority over those in "
                          "`space-macs-default-reference-handlers'.")
                  mode))
       (defun ,func ()
         (setq space-macs-reference-handlers
               (append ,handlers-list
                       space-macs-default-reference-handlers)))
       (add-hook ',mode-hook ',func)
       (with-eval-after-load 'bind-map
         (space-macs/set-leader-keys-for-major-mode ',mode
           "gr" 'space-macs/jump-to-reference)))))

(defun space-macs/jump-to-definition ()
  "Jump to definition around point using the best tool for this action."
  (interactive)
  (catch 'done
    (let ((old-buffer (current-buffer))
          (old-point (point)))
      (dolist (-handler space-macs-jump-handlers)
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

(defun space-macs/jump-to-definition-other-window ()
  "Jump to definition around point in other window."
  (interactive)
  (let ((pos (point)))
    ;; since `space-macs/jump-to-definition' can be asynchronous we cannot use
    ;; `save-excursion' here, so we have to bear with the jumpy behavior.
    (switch-to-buffer-other-window (current-buffer))
    (goto-char pos)
    (space-macs/jump-to-definition)))

(defun space-macs/jump-to-reference ()
  "Jump to reference around point using the best tool for this action."
  (interactive)
  (catch 'done
    (let ((old-window (selected-window))
          (old-buffer (current-buffer))
          (old-point (point)))
      (dolist (-handler space-macs-reference-handlers)
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

(defun space-macs/jump-to-reference-other-window ()
  "Jump to reference around point in other window."
  (interactive)
  (let ((pos (point)))
    ;; since `space-macs/jump-to-reference' can be asynchronous we cannot use
    ;; `save-excursion' here, so we have to bear with the jumpy behavior.
    (switch-to-buffer-other-window (current-buffer))
    (goto-char pos)
    (space-macs/jump-to-reference)))

;; Set the `:jump' property manually instead of just using `evil-define-motion'
;; in an `eval-after-load' macro invocation because doing that prevents
;; `describe-function' from correctly finding the source.
;;
;; See discussion on https://github.com/syl20bnr/space-macs/pull/6771
(with-eval-after-load 'evil
  (evil-set-command-property 'space-macs/jump-to-definition :jump t)
  (evil-set-command-property 'space-macs/jump-to-reference :jump t))

(provide 'core-jump)


