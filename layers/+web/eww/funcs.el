;;; funcs.el --- eww layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Colton Kopsa <coljamkop@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar spacemacs--eww-buffers nil)

(defun spacemacs//eww-get-buffers ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and (derived-mode-p 'eww-mode)
                 (not (memq buffer spacemacs--eww-buffers)))
        (push buffer
              spacemacs--eww-buffers))))
  (unless spacemacs--eww-buffers
    (error "No eww buffers"))
  ;; remove deleted buffers maintaining order
  (dolist (buffer spacemacs--eww-buffers)
    (if (not (memq buffer (buffer-list)))
        (delq buffer spacemacs--eww-buffers)))
  spacemacs--eww-buffers)

(defun spacemacs//eww-next-buffer (buff)
  (let* ((eww-buffers (spacemacs//eww-get-buffers))
         (eww-buffer-pos (seq-position eww-buffers buff)))
    (if (eq eww-buffer-pos (1- (length eww-buffers)))
        (car eww-buffers)
      (nth (1+ eww-buffer-pos) eww-buffers))))

(defun spacemacs//eww-previous-buffer (buff)
  (let* ((eww-buffers (spacemacs//eww-get-buffers))
         (eww-buffer-pos (seq-position eww-buffers buff)))
    (if (zerop eww-buffer-pos)
        (car (last eww-buffers))
      (nth (1- eww-buffer-pos) eww-buffers))))

(defun spacemacs/eww-jump-next-buffer ()
  (interactive)
  (pop-to-buffer-same-window (spacemacs//eww-next-buffer (current-buffer))))

(defun spacemacs/eww-jump-previous-buffer ()
  (interactive)
  (pop-to-buffer-same-window (spacemacs//eww-previous-buffer (current-buffer))))
