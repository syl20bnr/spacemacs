;;; funcs.el  --- Nav-flash Layer Functions File for Spacemacs. -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2023 Sylvain Benner & Contributors
;;
;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(defun nav-flash//blink-cursor (&rest _)
  "Blink the line containing the point.
This makes it clear where the cursor has landed (typically after a large motion,
like switching windows or jumping to another part of the file)."
  (unless (minibufferp)
    (nav-flash-show)
    ;; only show in the current window
    (when (overlayp compilation-highlight-overlay)
      (overlay-put compilation-highlight-overlay 'window (selected-window)))))

(defun nav-flash/blink-cursor-maybe (&rest _)
  "Like `nav-flash//blink-cursor', but no-ops if any following condition is met.
- If it's trigger by one of `nav-flash-exclude-commands'.
- If current major mode is one of `nav-flash-exclude-modes'.
- If `so-long-minor-mode' is on."
  (unless (or (memq this-command nav-flash-exclude-commands)
              (bound-and-true-p so-long-minor-mode)
              (apply #'derived-mode-p nav-flash-exclude-modes)
              (equal nav-flash--last-point
                     (list (selected-window)
                           (current-buffer)
                           (point))))
    (nav-flash//blink-cursor)
    (setq nav-flash--last-point (list (selected-window) (current-buffer) (point)))))

(defun nav-flash/delayed-blink-cursor-h (&rest _)
  "Like `nav-flash//blink-cursor', but blinks after a tiny pause.
Useful at run-time to ensure the point be in the correct window/buffer (like
for `org-follow-link-hook')."
  (run-at-time 0.1 nil #'nav-flash//blink-cursor))

(defun nav-flash/blink-cursor (&rest _)
  "Blink current line using `nav-flash'."
  (interactive)
  (nav-flash//blink-cursor))
