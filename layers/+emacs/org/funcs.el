;;; funcs.el --- Org Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Autoload space-doc-mode
(autoload 'space-doc-mode "space-doc" nil 'interactive)

(defun org-projectile/capture (&optional arg)
  (interactive "P")
  (if arg
      (org-projectile:project-todo-completing-read nil :empty-lines 1)
    (org-projectile:capture-for-current-project nil :empty-lines 1)))

(defun org-projectile/goto-todos ()
  (interactive)
  (org-projectile:location-for-project (projectile-project-name)))



(defun spacemacs/ob-fix-inline-images ()
  "Fix redisplay of inline images after a code block evaluation."
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))



(defun spacemacs//surround-drawer ()
  (let ((dname (read-from-minibuffer "" "")))
    (cons (format ":%s:" (upcase (or dname ""))) ":END:")))

(defun spacemacs//surround-code ()
  (let ((dname (read-from-minibuffer "" "")))
    (cons (format "#+BEGIN_SRC %s" (or dname "")) "#+END_SRC")))

(defun spacemacs//org-ctrl-c-ctrl-c-counsel-org-tag ()
  "Hook for `org-ctrl-c-ctrl-c-hook' to use `counsel-org-tag'."
  (if (save-excursion (beginning-of-line) (looking-at "[ \t]*$"))
      (or (run-hook-with-args-until-success 'org-ctrl-c-ctrl-c-final-hook)
          (user-error "C-c C-c can do nothing useful at this location"))
    (let* ((context (org-element-context))
           (type (org-element-type context)))
      (case type
        ;; When at a link, act according to the parent instead.
        (link (setq context (org-element-property :parent context))
              (setq type (org-element-type context)))
        ;; Unsupported object types: refer to the first supported
        ;; element or object containing it.
        ((bold code entity export-snippet inline-babel-call inline-src-block
               italic latex-fragment line-break macro strike-through subscript
               superscript underline verbatim)
         (setq context
               (org-element-lineage
                context '(radio-target paragraph verse-block table-cell)))))
      ;; For convenience: at the first line of a paragraph on the
      ;; same line as an item, apply function on that item instead.
      (when (eq type 'paragraph)
        (let ((parent (org-element-property :parent context)))
          (when (and (eq (org-element-type parent) 'item)
                     (= (line-beginning-position)
                        (org-element-property :begin parent)))
            (setq context parent type 'item))))

      ;; Act according to type of element or object at point.
      (case type
        ((headline inlinetask)
         (save-excursion (goto-char (org-element-property :begin context))
                         (call-interactively 'counsel-org-tag)) t)))))
