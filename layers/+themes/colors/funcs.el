;;; funcs.el --- Colors Layer functions File
;;
;; Copyright (c) 2012-2024 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
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



;; rainbow-identifiers

(defun colors//rainbow-identifiers-mode-maybe ()
  "Enable rainbow identifiers if the major mode is a prog mode."
  (when (derived-mode-p 'prog-mode)
    (rainbow-identifiers-mode)))

(defun colors//rainbow-identifiers-ignore-keywords ()
  "Do not colorize stuff with ‘font-lock-keyword-face’."
  (setq-local rainbow-identifiers-faces-to-override
              (delq 'font-lock-keyword-face
                    rainbow-identifiers-faces-to-override)))

(defun colors//tweak-theme-colors (theme)
  "Tweak color themes by adjusting rainbow-identifiers."
  (interactive)
  ;; tweak the saturation and lightness of identifier colors
  (unless (assq theme (get 'rainbow-identifiers-cie-l*a*b*-saturation
                           'theme-value))
    (let ((sat&light (assq theme colors-theme-identifiers-sat&light)))
      (if sat&light
          (setq rainbow-identifiers-cie-l*a*b*-saturation (cadr sat&light)
                rainbow-identifiers-cie-l*a*b*-lightness (caddr sat&light))
        ;; fall back to our defaults if there are no per-theme settings
        (setq rainbow-identifiers-cie-l*a*b*-saturation colors-default-rainbow-identifiers-sat
              rainbow-identifiers-cie-l*a*b*-lightness colors-default-rainbow-identifiers-light))))
  ;; it isn't enough to just update the variables! we must now refresh the "font
  ;; locking" (syntax highlighting) in all buffers that have rainbow-identifiers-mode
  ;; currently active, so that they instantly re-paint with their per-theme values.
  ;; this loops through all buffers and marks matching ones for re-painting,
  ;; starting with the current buffer first so that the user sees quick results!
  (when (featurep 'rainbow-identifiers)
    (dolist ($buf (buffer-list (current-buffer)))
      (with-current-buffer $buf
        (when (and rainbow-identifiers-mode font-lock-mode)
          (if (fboundp 'font-lock-flush)
              (font-lock-flush) ; use flush if available
            (with-no-warnings (font-lock-fontify-buffer))))))))

(defun colors//change-color-mini-mode-doc (component)
  "Display a short documentation in the mini buffer."
  (let ((var (intern (format
                      "rainbow-identifiers-cie-l*a*b*-%s" component))))
    (spacemacs/echo "Change color %s mini-mode (value: %s)
  + to increase %s
  - to decrease %s
  = to reset
Press any other key to exit." component (eval var) component component)))

(defun colors/change-color-component-overlay-map (component)
  "Set a temporary overlay map to easily change a color COMPONENT from
 rainbow-identifier mode. The color COMPONENT can be 'saturation' or
 'lightness'."
  (set-temporary-overlay-map
   (let ((map (make-sparse-keymap))
         (up-func (intern (format "colors/change-color-%s-up" component)))
         (down-func (intern (format "colors/change-color-%s-down" component)))
         (reset-func (intern (format "colors/change-color-%s-reset" component))))
     (define-key map (kbd "+") up-func)
     (define-key map (kbd "-") down-func)
     (define-key map (kbd "=") reset-func)
     map) t)
  (colors//change-color-mini-mode-doc component))

(defun colors/start-change-color-saturation ()
  "Initiate the overlay map to change the saturation."
  (interactive)
  (colors/change-color-component-overlay-map "saturation"))

(defun colors/change-color-saturation-up ()
  "Increase the saturation by 5 units."
  (interactive)
  (colors//change-color-component-func "saturation" 5))

(defun colors/change-color-saturation-down ()
  "Decrease the saturation by 5 units."
  (interactive)
  (colors//change-color-component-func "saturation" -5))

(defun colors/change-color-saturation-reset ()
  "Reset the saturation to default."
  (interactive)
  (colors//change-color-component-func "saturation" colors-default-rainbow-identifiers-sat t))

(defun colors/start-change-color-lightness ()
  "Initiate the overlay map to change the lightness."
  (interactive)
  (colors/change-color-component-overlay-map "lightness"))

(defun colors/change-color-lightness-up ()
  "Increase the lightness by 5 units."
  (interactive)
  (colors//change-color-component-func "lightness" 5))

(defun colors/change-color-lightness-down ()
  "Decrease the lightness by 5 units."
  (interactive)
  (colors//change-color-component-func "lightness" -5))

(defun colors/change-color-lightness-reset ()
  "Reset the lightness to default."
  (interactive)
  (colors//change-color-component-func "lightness" colors-default-rainbow-identifiers-light t))

(defun colors//change-color-component-func
    (component inc &optional reset)
  "Change the color component by adding INC value to it. If RESET is not
 nil the color component is set to INC."
  (let* ((var (intern (format
                       "rainbow-identifiers-cie-l*a*b*-%s" component)))
         (new-value (+ (eval var) inc)))
    (if reset
        (set var inc)
      (progn
        (if (< new-value 0)
            (setq new-value 0))
        (set var new-value)))
    (if (fboundp 'font-lock-flush)
        (font-lock-flush) ; use flush if available
      (with-no-warnings (font-lock-fontify-buffer)))
    (colors/change-color-component-overlay-map component)))

(defun colors/add-theme-sat&light (theme-name sat-light)
  "Easily add personal per-theme rainbow-identifiers
saturation & lightness settings. Your new values will
override any previous definitions for that theme.

Remember to always refresh the look of your theme after
you've added any new settings!

Example usage:
    (colors/add-theme-sat&light 'leuven '(30 50))
    (colors/add-theme-sat&light 'anothertheme '(90 20))
    (colors/refresh-theme-look)"
  (push (cons theme-name sat-light) colors-theme-identifiers-sat&light))

(defun colors/refresh-theme-look ()
  "Refresh and re-apply the look of your current theme.
Always run this after adding new per-theme settings!"
  (interactive)
  (colors//tweak-theme-colors spacemacs--cur-theme))
