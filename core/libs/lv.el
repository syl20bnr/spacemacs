;;; lv.el --- Other echo area

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
;; This package provides `lv-message' intended to be used in place of
;; `message' when semi-permanent hints are needed, in order to not
;; interfere with Echo Area.
;;
;;    "Я тихо-тихо пiдглядаю,
;;     І тiшуся собi, як бачу то,
;;     Шо страшить i не пiдпускає,
;;     А iншi п’ють тебе, як воду пiсок."
;;     --  Андрій Кузьменко, L.V.

;;; Code:

(defvar lv-wnd nil
  "Holds the current LV window.")

(defun lv-window ()
  "Ensure that LV window is live and return it."
  (if (window-live-p lv-wnd)
      lv-wnd
    (let ((ori (selected-window))
          buf)
      (prog1 (setq lv-wnd
                   (select-window
                    (split-window
                     (frame-root-window) -1 'below)))
        (if (setq buf (get-buffer "*LV*"))
            (switch-to-buffer buf)
          (switch-to-buffer "*LV*")
          (setq truncate-lines nil)
          (setq mode-line-format nil)
          (setq cursor-type nil)
          (set-window-dedicated-p lv-wnd t))
        (select-window ori)))))

(defun lv-message (format-string &rest args)
  "Set LV window contents to (`format' FORMAT-STRING ARGS)."
  (let ((ori (selected-window))
        (str (apply #'format format-string args))
        (golden-ratio (when (boundp 'golden-ratio-mode) golden-ratio-mode))
        deactivate-mark)
    (when (bound-and-true-p golden-ratio-mode) (golden-ratio-mode -1))
    (select-window (lv-window))
    (when golden-ratio (golden-ratio-mode))
    (unless (string= (buffer-string) str)
      (delete-region (point-min) (point-max))
      (insert str)
      (fit-window-to-buffer nil nil 1))
    (goto-char (point-min)) (end-of-line)
    (select-window ori)))

(provide 'lv)

;;; lv.el ends here
