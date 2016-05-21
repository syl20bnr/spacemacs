;;; funcs.el --- Spacemacs Visual UI Layer functions File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; smooth scrolling

(defun spacemacs/enable-smooth-scrolling ()
  "Enable smooth scrolling."
  (interactive)
  (setq scroll-conservatively 101))

(defun spacemacs/disable-smooth-scrolling ()
  "Enable smooth scrolling."
  (interactive)
  (setq scroll-conservatively 0))


;; neotree

(defun spacemacs/neotree-expand-or-open ()
  "Collapse a neotree node."
  (interactive)
  (let ((node (neo-buffer--get-filename-current-line)))
    (when node
      (if (file-directory-p node)
          (progn
            (neo-buffer--set-expand node t)
            (neo-buffer--refresh t)
            (when neo-auto-indent-point
              (next-line)
              (neo-point-auto-indent)))
        (call-interactively 'neotree-enter)))))

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


;; vi-tilde-fringe

(defun spacemacs/disable-vi-tilde-fringe ()
  "Disable `vi-tilde-fringe' in the current buffer."
  (vi-tilde-fringe-mode -1))

(defun spacemacs/disable-vi-tilde-fringe-read-only ()
  "Disable `vi-tilde-fringe' in the current buffer if it is read only."
  (when buffer-read-only
    (spacemacs/disable-vi-tilde-fringe)))
