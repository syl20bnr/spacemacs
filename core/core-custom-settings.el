;;; core-custom-settings.el --- Space-macs Core File
;;
;; Copyright (c) 2012-2020 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/space-macs
;;
;; This file is not part of GNU e-macs.
;;
;;; License: GPLv3
(require 'core-dotspace-macs)

(defvar space-macs--custom-file (concat space-macs-cache-directory
                                       ".custom-settings"))

(defun space-macs/initialize-custom-file ()
  "Initialize the custom file.
Does not initialize writing the custom file into the dotfile. To
complete that part see `space-macs/initialize-custom-file-sync'."
  ;; setup auto-rewrite of custom settings only if custom-file
  ;; has not been set by the user
  (when (null custom-file)
    (setq custom-file space-macs--custom-file))
  ;; initialize the cache file contents
  (unless (or (not (string-equal custom-file space-macs--custom-file))
              (file-exists-p space-macs--custom-file))
    (with-temp-file space-macs--custom-file
      (let ((standard-output (current-buffer)))
        (princ ";; -*- mode: e-macs-lisp -*-\n")
        (princ ";; This file is where e-macs writes custom variables.
;; Space-macs will copy its content to your dotfile automatically in the
;; function `dotspace-macs/e-macs-custom-settings'.
;; Do not alter this file, use e-macs customize interface instead.\n\n")))))

(defun space-macs/initialize-custom-file-sync ()
  "Initialize syncing of the custom file to the dotfile."
  (when (string-equal custom-file space-macs--custom-file)
    (advice-add 'custom-save-all :after
                #'space-macs/write-custom-settings-to-dotfile)))

(defun space-macs//delete-e-macs-custom-settings ()
  "Delete function `dotspace-macs/e-macs-custom-settings' from dotfile.

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
                     (eq (cadr sexp) 'dotspace-macs/e-macs-custom-settings))
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

(defun space-macs//get-custom-settings-from-cache ()
  "Returns the custom settings from `space-macs--custom-file'."
  (with-current-buffer (let ((find-file-visit-truename t)
                             (delay-mode-hooks t))
                         (find-file-noselect space-macs--custom-file))
    (goto-char (point-min))
    ;; Skip all whitespace and comments.
    (while (forward-comment 1))
    (buffer-substring-no-properties (point) (point-max))))

(defun space-macs/write-custom-settings-to-dotfile ()
  "Write `dotspace-macs/e-macs-custom-settings' function in the dotfile"
  (message "Writing e-macs custom settings to dotfile...")
  (with-current-buffer (let ((find-file-visit-truename t)
                             (delay-mode-hooks t))
                         (find-file-noselect (dotspace-macs/location)))
    (space-macs//delete-e-macs-custom-settings)
    (let ((standard-output (current-buffer)))
      (princ "(defun dotspace-macs/e-macs-custom-settings ()\n")
      (princ "  \"e-macs custom settings.
This is an auto-generated function, do not modify its content directly, use
e-macs customize menu instead.
This function is called at the very end of Space-macs initialization.\"\n")
      (princ (space-macs//get-custom-settings-from-cache))
      (princ ")")
      (save-buffer)
      (kill-buffer (current-buffer)))))

(provide 'core-custom-settings)


