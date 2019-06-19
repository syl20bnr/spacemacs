;;; funcs.el --- Neotree Layer functions File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/neotree-expand-or-open (&optional arg)
  "Expand or open a neotree node."
  (interactive "P")
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (progn
            (neo-buffer--set-expand node t)
            (neo-buffer--refresh t)
            (when neo-auto-indent-point
              (next-line)
              (neo-point-auto-indent)))
        (if arg
            (neotree-enter arg)
          (let ((mru-winum (winum-get-number (get-mru-window))))
            (apply 'neotree-enter (list mru-winum))))))))

(defun spacemacs/neotree-collapse ()
  "Collapse a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (when (file-directory-p node)
        (neo-buffer--set-expand node nil)
        (neo-buffer--refresh t))
      (when neo-auto-indent-point
        (neo-point-auto-indent)))))

(defun spacemacs/neotree-collapse-or-up ()
  "Collapse an expanded directory node or go to the parent node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (if (neo-buffer--expanded-node-p node)
              (spacemacs/neotree-collapse)
            (neotree-select-up-node))
        (neotree-select-up-node)))))

(defun neotree-find-project-root ()
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (let ((origin-buffer-file-name (buffer-file-name)))
      (neotree-find (projectile-project-root))
      (neotree-find origin-buffer-file-name))))

(defun spacemacs//neotree-maybe-attach-window ()
  (when (get-buffer-window (neo-global--get-buffer))
    (neo-global--attach)))


;; winum

(defun spacemacs//winum-neotree-assign-func ()
  "Custom number assignment for neotree."
  (when (and (boundp 'neo-buffer-name)
             (string= (buffer-name) neo-buffer-name)
             ;; in case there are two neotree windows. Example: when
             ;; invoking a transient state from neotree window, the new
             ;; window will show neotree briefly before displaying the TS,
             ;; causing an error message. the error is eliminated by
             ;; assigning 0 only to the top-left window
             (eq (selected-window) (frame-first-window)))
    0))
