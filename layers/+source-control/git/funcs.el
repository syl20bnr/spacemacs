;;; funcs.el --- Colors Layer functions File
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; magit

(defun spacemacs/magit-toggle-whitespace ()
  "Toggle whitespace in `magit-diff-mode'."
  (interactive)
  (if (member "-w" (if (derived-mode-p 'magit-diff-mode)
                       magit-refresh-args
                     magit-diff-section-arguments))
      (spacemacs//magit-dont-ignore-whitespace)
    (spacemacs//magit-ignore-whitespace)))

(defun spacemacs//magit-ignore-whitespace ()
  "Ignore whitespace in `magit-diff-mode'"
  (add-to-list (if (derived-mode-p 'magit-diff-mode)
                   'magit-refresh-args 'magit-diff-section-arguments) "-w")
  (magit-refresh))

(defun spacemacs//magit-dont-ignore-whitespace ()
  "Don't ignore whitespace in `magit-diff-mode'"
  (setq magit-diff-options
        (remove "-w"
                (if (derived-mode-p 'magit-diff-mode)
                    magit-refresh-args
                  magit-diff-section-arguments))) (magit-refresh))

(defun spacemacs//fullscreen-magit (buffer)
  "Display Magit status buffer in fullscreen."
  (if (or
       ;; the original should stay alive, so we can't go fullscreen
       magit-display-buffer-noselect
       ;; don't go fullscreen for certain magit buffers if current
       ;; buffer is a magit buffer (we're conforming to
       ;; `magit-display-buffer-traditional')
       (and (derived-mode-p 'magit-mode)
            (not (memq (with-current-buffer buffer major-mode)
                       '(magit-process-mode
                         magit-revision-mode
                         magit-diff-mode
                         magit-stash-mode
                         magit-status-mode)))))
      ;; open buffer according to original magit rules
      (magit-display-buffer-traditional buffer)
    ;; open buffer in fullscreen
    (delete-other-windows)
    ;; make sure the window isn't dedicated, otherwise
    ;; `set-window-buffer' throws an error
    (set-window-dedicated-p nil nil)
    (set-window-buffer nil buffer)
    ;; return buffer's window
    (get-buffer-window buffer)))
