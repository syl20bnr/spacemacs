;;; sly-scratch.el  -*- lexical-binding: t; -*-

(require 'sly)
(require 'cl-lib)

(define-sly-contrib sly-scratch
  "Imitate Emacs' *scratch* buffer"
  (:authors "Helmut Eller  <heller@common-lisp.net>")
  (:on-load
   (define-key sly-selector-map (kbd "s") 'sly-scratch))
  (:license "GPL"))


;;; Code

(defvar sly-scratch-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map lisp-mode-map)
    (define-key map "\C-j" 'sly-eval-print-last-expression)
    map))

(defun sly-scratch ()
  (interactive)
  (sly-switch-to-scratch-buffer))

(defun sly-switch-to-scratch-buffer ()
  (set-buffer (sly-scratch-buffer))
  (unless (eq (current-buffer) (window-buffer))
    (pop-to-buffer (current-buffer) t)))

(defvar sly-scratch-file nil)

(defun sly-scratch-buffer ()
  "Return the scratch buffer, create it if necessary."
  (or (get-buffer (sly-buffer-name :scratch))
      (with-current-buffer (if sly-scratch-file
                               (find-file sly-scratch-file)
                             (get-buffer-create (sly-buffer-name :scratch)))
        (rename-buffer (sly-buffer-name :scratch))
	(lisp-mode)
	(use-local-map sly-scratch-mode-map)
	(sly-mode t)
	(current-buffer))))

(provide 'sly-scratch)
