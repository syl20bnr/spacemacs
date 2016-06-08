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


;; rainbow-identifiers

(defun colors//rainbow-identifiers-mode-maybe ()
  "Enable rainbow identifiers if the major mode is a prog mode."
  (when (derived-mode-p 'prog-mode)
    (rainbow-identifiers-mode)))

(defun colors//tweak-theme-colors (theme)
  "Tweak color themes by adjusting rainbow-identifiers."
  (interactive)
  ;; tweak the saturation and lightness of identifier colors
  (when (not (assq theme (get 'rainbow-identifiers-cie-l*a*b*-saturation
                              'theme-value)))
    (let ((sat&light (assq theme colors-theme-identifiers-sat&light)))
      (if sat&light
          (setq rainbow-identifiers-cie-l*a*b*-saturation (cadr sat&light)
                rainbow-identifiers-cie-l*a*b*-lightness (caddr sat&light))
        ;; default
        (setq rainbow-identifiers-cie-l*a*b*-saturation 80
              rainbow-identifiers-cie-l*a*b*-lightness 45)))))

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

(defun colors/init-rainbow-mode ()
  (use-package rainbow-mode
    :commands rainbow-mode
    :init (spacemacs/set-leader-keys "tCc" 'rainbow-mode)
    :config (spacemacs|hide-lighter rainbow-mode)))

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
  "Reset the saturation to 100."
  (interactive)
  (colors//change-color-component-func "saturation" 100 t))

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
  "Reset the lightness to 40."
  (interactive)
  (colors//change-color-component-func "lightness" 40 t))

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
    (font-lock-fontify-buffer)
    (colors/change-color-component-overlay-map component)))
