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

(require 'cl-lib)

(defgroup lv nil
  "The other echo area."
  :group 'minibuffer
  :group 'hydra)

(defcustom lv-use-separator nil
  "Whether to draw a line between the LV window and the Echo Area."
  :group 'lv
  :type 'boolean)

(defcustom lv-use-padding nil
  "Whether to use horizontal padding in the LV window."
  :group 'lv
  :type 'boolean)

(defface lv-separator
  '((((class color) (background light)) :background "grey80")
    (((class color) (background  dark)) :background "grey30"))
  "Face used to draw line between the lv window and the echo area.
This is only used if option `lv-use-separator' is non-nil.
Only the background color is significant."
  :group 'lv)

(defvar lv-wnd nil
  "Holds the current LV window.")

(defvar display-line-numbers)
(defvar display-fill-column-indicator)
(defvar tab-line-format)

(defvar lv-window-hook nil
  "Hook to run by `lv-window' when a new window is created.")

(defun lv-window ()
  "Ensure that LV window is live and return it."
  (if (window-live-p lv-wnd)
      lv-wnd
    (let ((ori (selected-window))
          buf)
      (prog1 (setq lv-wnd
                   (select-window
                    (let ((ignore-window-parameters t))
                      (split-window
                       (frame-root-window) -1 'below))
                    'norecord))
        (if (setq buf (get-buffer " *LV*"))
            (switch-to-buffer buf 'norecord)
          (switch-to-buffer " *LV*" 'norecord)
          (fundamental-mode)
          (set-window-hscroll lv-wnd 0)
          (setq window-size-fixed t)
          (setq mode-line-format nil)
          (setq header-line-format nil)
          (setq tab-line-format nil)
          (setq cursor-type nil)
          (setq display-line-numbers nil)
          (setq display-fill-column-indicator nil)
          (set-window-dedicated-p lv-wnd t)
          (set-window-parameter lv-wnd 'no-other-window t)
          (run-hooks 'lv-window-hook))
        (select-window ori 'norecord)))))

(defvar golden-ratio-mode)

(defvar lv-force-update nil
  "When non-nil, `lv-message' will refresh even for the same string.")

(defun lv--pad-to-center (str width)
  "Pad STR with spaces on the left to be centered to WIDTH."
  (let* ((strs (split-string str "\n"))
         (padding (make-string
                   (/ (- width (length (car strs))) 2)
                   ?\ )))
    (mapconcat (lambda (s) (concat padding s)) strs "\n")))

(defun lv-message (format-string &rest args)
  "Set LV window contents to (`format' FORMAT-STRING ARGS)."
  (let* ((str (apply #'format format-string args))
         (n-lines (cl-count ?\n str))
         deactivate-mark
         golden-ratio-mode)
    (with-selected-window (lv-window)
      (when lv-use-padding
        (setq str (lv--pad-to-center str (window-width))))
      (unless (and (string= (buffer-string) str)
                   (null lv-force-update))
        (delete-region (point-min) (point-max))
        (insert str)
        (when (and (window-system) lv-use-separator)
          (unless (looking-back "\n" nil)
            (insert "\n"))
          (insert
           (propertize "__" 'face 'lv-separator 'display '(space :height (1)))
           (propertize "\n" 'face 'lv-separator 'line-height t)))
        (set (make-local-variable 'window-min-height) n-lines)
        (setq truncate-lines (> n-lines 1))
        (let ((window-resize-pixelwise t)
              (window-size-fixed nil))
          (fit-window-to-buffer nil nil 1)))
      (goto-char (point-min)))))

(defun lv-delete-window ()
  "Delete LV window and kill its buffer."
  (when (window-live-p lv-wnd)
    (let ((buf (window-buffer lv-wnd)))
      (delete-window lv-wnd)
      (kill-buffer buf))))

(provide 'lv)

;;; lv.el ends here
