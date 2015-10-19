;;; corelv.el --- Other echo area

;; Copyright (C) 2015  Free Software Foundation, Inc.

;; Author: Oleh Krehel

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This package provides `corelv-message' intended to be used in place of
;; `message' when semi-permanent hints are needed, in order to not
;; interfere with Echo Area.
;;
;;    "Я тихо-тихо пiдглядаю,
;;     І тiшуся собi, як бачу то,
;;     Шо страшить i не пiдпускає,
;;     А iншi п’ють тебе, як воду пiсок."
;;     --  Андрій Кузьменко, L.V.

;;; Code:

(defgroup corelv nil
  "The other echo area."
  :group 'minibuffer
  :group 'hydra)

(defcustom corelv-use-separator nil
  "Whether to draw a line between the LV window and the Echo Area."
  :group 'corelv
  :type 'boolean)

(defface corelv-separator
  '((((class color) (background light)) :background "grey80")
    (((class color) (background  dark)) :background "grey30"))
  "Face used to draw line between the corelv window and the echo area.
This is only used if option `corelv-use-separator' is non-nil.
Only the background color is significant."
  :group 'corelv)

(defvar corelv-wnd nil
  "Holds the current LV window.")

(defun corelv-window ()
  "Ensure that LV window is live and return it."
  (if (window-live-p corelv-wnd)
      corelv-wnd
    (let ((ori (selected-window))
          buf)
      (prog1 (setq corelv-wnd
                   (select-window
                    (let ((ignore-window-parameters t))
                      (split-window
                       (frame-root-window) -1 'below))))
        (if (setq buf (get-buffer "*LV*"))
            (switch-to-buffer buf)
          (switch-to-buffer "*LV*")
          (set-window-hscroll corelv-wnd 0)
          (setq window-size-fixed t)
          (setq mode-line-format nil)
          (setq cursor-type nil)
          (set-window-dedicated-p corelv-wnd t)
          (set-window-parameter corelv-wnd 'no-other-window t))
        (select-window ori)))))

(defvar golden-ratio-mode)

(defvar corelv-force-update nil
  "When non-nil, `corelv-message' will refresh even for the same string.")

(defun corelv-message (format-string &rest args)
  "Set LV window contents to (`format' FORMAT-STRING ARGS)."
  (let* ((str (apply #'format format-string args))
         (n-lines (cl-count ?\n str))
         deactivate-mark
         golden-ratio-mode)
    (with-selected-window (corelv-window)
      (unless (and (string= (buffer-string) str)
                   (null corelv-force-update))
        (delete-region (point-min) (point-max))
        (insert str)
        (when (and (window-system) corelv-use-separator)
          (unless (looking-back "\n" nil)
            (insert "\n"))
          (insert
           (propertize "__" 'face 'corelv-separator 'display '(space :height (1)))
           (propertize "\n" 'face 'corelv-separator 'line-height t)))
        (setq-local window-min-height n-lines)
        (setq truncate-lines (> n-lines 1))
        (let ((window-resize-pixelwise t)
              (window-size-fixed nil))
          (fit-window-to-buffer nil nil 1)))
      (goto-char (point-min)))))

(defun corelv-delete-window ()
  "Delete LV window and kill its buffer."
  (when (window-live-p corelv-wnd)
    (let ((buf (window-buffer corelv-wnd)))
      (delete-window corelv-wnd)
      (kill-buffer buf))))

(provide 'corelv)

;;; corelv.el ends here
