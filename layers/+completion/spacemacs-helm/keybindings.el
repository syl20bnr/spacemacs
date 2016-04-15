;;; keybindings.el --- Spacemacs Base Layer key-bindings File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; describe functions ---------------------------------------------------------
(defmacro spacemacs||set-helm-key (keys func)
  "Define a key bindings for FUNC using KEYS.
Ensure that helm is required before calling FUNC."
  (let ((func-name (intern (format "spacemacs/%s" (symbol-name func)))))
    `(progn
       (defun ,func-name ()
         ,(format "Wrapper to ensure that `helm' is loaded before calling %s."
                  (symbol-name func))
         (interactive)
         (require 'helm)
         (call-interactively ',func))
       (spacemacs/set-leader-keys ,keys ',func-name))))
(spacemacs||set-helm-key "fel"  helm-locate-library)
(spacemacs||set-helm-key "hdm" describe-mode)
;; search functions -----------------------------------------------------------
(spacemacs||set-helm-key "sww" helm-wikipedia-suggest)
(spacemacs||set-helm-key "swg" helm-google-suggest)

(defun spacemacs-helm//find-files-edit (candidate)
  (dired (file-name-directory candidate))
  (dired-goto-file candidate)
  (dired-toggle-read-only))
(defun spacemacs-helm/find-files-edit ()
  "Exits helm, opens a dired buffer and immediately switches to editable mode."
  (interactive)
  (helm-exit-and-execute-action 'spacemacs-helm//find-files-edit))
(with-eval-after-load 'helm-files
  (define-key helm-find-files-map (kbd "C-c C-e") 'spacemacs-helm/find-files-edit))
