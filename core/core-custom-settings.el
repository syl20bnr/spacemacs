;;; core-custom-settings.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'core-dotspacemacs)

(defvar spacemacs--custom-file (concat spacemacs-cache-directory
                                       ".custom-settings"))

(defun spacemacs/initialize-custom-file ()
  "Initialize the custom file.
Does not initialize writing the custom file into the dotfile. To
complete that part see `spacemacs/initialize-custom-file-sync'."
  ;; setup auto-rewrite of custom settings only if custom-file
  ;; has not been set by the user
  (when (null custom-file)
    (setq custom-file spacemacs--custom-file))
  ;; initialize the cache file contents
  (unless (or (not (string-equal custom-file spacemacs--custom-file))
              (file-exists-p spacemacs--custom-file))
    (with-temp-file spacemacs--custom-file
      (let ((standard-output (current-buffer)))
        (princ ";; -*- mode: emacs-lisp -*-\n")
        (princ ";; This file is where Emacs writes custom variables.
;; Spacemacs will copy its content to your dotfile automatically in the
;; function `dotspacemacs/emacs-custom-settings'.
;; Do not alter this file, use Emacs customize interface instead.\n\n")))))

(defun spacemacs/initialize-custom-file-sync ()
  "Initialize syncing of the custom file to the dotfile."
  (when (string-equal custom-file spacemacs--custom-file)
    (advice-add 'custom-save-all :after
                #'spacemacs/write-custom-settings-to-dotfile)))

(defun spacemacs//delete-emacs-custom-settings ()
  "Delete function `dotspacemacs/emacs-custom-settings' from dotfile.

Leave point at the old location of the defun, or (if there were none) at the
end of the buffer.
 This function does not save the buffer."
  (goto-char (point-min))
  ;; Skip all whitespace and comments.
  (while (forward-comment 1))
  (let (first)
    (catch 'found
      (while t ;; We exit this loop only via throw.
        ;; Skip all whitespace and comments.
        (while (forward-comment 1))
        (let ((start (point))
              (sexp (condition-case nil
                        (read (current-buffer))
                      (end-of-file (throw 'found nil)))))
          (when (and (listp sexp)
                     (eq (car sexp) 'defun)
                     (eq (cadr sexp) 'dotspacemacs/emacs-custom-settings))
            (delete-region start (point))
            (unless first
              (setq first (point)))))))
    (if first
        (goto-char first)
      ;; Move in front of local variables, otherwise long Custom
      ;; entries would make them ineffective.
      (let ((pos (point-max))
            (case-fold-search t))
        (save-excursion
          (goto-char (point-max))
          (search-backward "\n\^L" (max (- (point-max) 3000) (point-min))
                           'move)
          (when (search-forward "Local Variables:" nil t)
            (setq pos (line-beginning-position))))
        (goto-char pos)))))

(defun spacemacs//get-custom-settings-from-cache ()
  "Returns the custom settings from `spacemacs--custom-file'."
  (with-current-buffer (let ((find-file-visit-truename t)
                             (delay-mode-hooks t))
                         (find-file-noselect spacemacs--custom-file))
    (goto-char (point-min))
    ;; Skip all whitespace and comments.
    (while (forward-comment 1))
    (buffer-substring-no-properties (point) (point-max))))

(defun spacemacs/write-custom-settings-to-dotfile ()
  "Write `dotspacemacs/emacs-custom-settings' function in the dotfile"
  (message "Writing Emacs custom settings to dotfile...")
  (with-current-buffer (let ((find-file-visit-truename t)
                             (delay-mode-hooks t))
                         (find-file-noselect (dotspacemacs/location)))
    (spacemacs//delete-emacs-custom-settings)
    (let ((standard-output (current-buffer)))
      (princ "(defun dotspacemacs/emacs-custom-settings ()\n")
      (princ "  \"Emacs custom settings.
This is an auto-generated function, do not modify its content directly, use
Emacs customize menu instead.
This function is called at the very end of Spacemacs initialization.\"\n")
      (princ (spacemacs//get-custom-settings-from-cache))
      (princ ")")
      (save-buffer)
      (kill-buffer (current-buffer)))))

(provide 'core-custom-settings)
