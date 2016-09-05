;;; core-jump.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar spacemacs-default-jump-handlers '()
  "List of jump handlers available in every mode.")

(defvar-local spacemacs-jump-handlers '()
  "List of jump handlers local to this buffer.")

(defmacro spacemacs|define-jump-handlers (mode &rest handlers)
  "Defines jump handlers for the given MODE.
This defines a variable `spacemacs-jump-handlers-MODE' to which
handlers can be added, and a function added to MODE-hook which
sets `spacemacs-jump-handlers' in buffers of that mode."
  (let ((mode-hook (intern (format "%S-hook" mode)))
        (func (intern (format "spacemacs//init-jump-handlers-%S" mode)))
        (handlers-list (intern (format "spacemacs-jump-handlers-%S" mode))))
    `(progn
       (defvar ,handlers-list ',handlers
         ,(format (concat "List of mode-specific jump handlers for %S. "
                          "These take priority over those in "
                          "`spacemacs-default-jump-handlers'.")
                  mode))
       (defun ,func ()
         (setq spacemacs-jump-handlers
               (append ,handlers-list
                       spacemacs-default-jump-handlers)))
       (add-hook ',mode-hook ',func)
       (with-eval-after-load 'bind-map
         (spacemacs/set-leader-keys-for-major-mode ',mode
           "gg" 'spacemacs/jump-to-definition)))))

(defun spacemacs/jump-to-definition ()
  (interactive)
  (catch 'done
    (let ((old-buffer (current-buffer))
          (old-point (point)))
      (dolist (-handler spacemacs-jump-handlers)
        (let ((handler (if (listp -handler) (car -handler) -handler))
              (async (when (listp -handler)
                       (plist-get (cdr -handler) :async))))
          (ignore-errors
            (call-interactively handler))
          (when (or async
                    (not (eq old-point (point)))
                    (not (equal old-buffer (current-buffer))))
            (throw 'done t)))))
    (message "No jump handler was able to find this symbol.")))

;; Set the `:jump' property manually instead of just using `evil-define-motion'
;; in an `eval-after-load' macro invocation because doing that prevents
;; `describe-function' from correctly finding the source.
;;
;; See discussion on https://github.com/syl20bnr/spacemacs/pull/6771
(with-eval-after-load 'evil
  (evil-set-command-property 'spacemacs/jump-to-definition :jump t))

(provide 'core-jump)
