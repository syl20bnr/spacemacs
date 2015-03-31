;;; packages.el --- Colors Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar colors-packages
  '(
    ;; not working well for now
    ;; rainbow-blocks
    rainbow-identifiers
    rainbow-mode
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defun colors/init-rainbow-blocks ()
  (use-package rainbow-blocks
    :disabled t
    :init (add-hook 'emacs-lisp-mode-hook 'rainbow-blocks-mode)))

(defun colors/init-rainbow-identifiers ()
  (use-package rainbow-identifiers
    :if colors-enable-rainbow-identifiers
    :commands rainbow-identifiers-mode
    :init
    (progn
      (setq rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face
            rainbow-identifiers-cie-l*a*b*-saturation 100
            rainbow-identifiers-cie-l*a*b*-lightness 40
            ;; override theme faces
            rainbow-identifiers-faces-to-override '(highlight-quoted-symbol
                                                    font-lock-variable-name-face))

      (defun colors/toggle-rainbow-indentifiers ()
        "Toggle rainbow identifiers."
        (interactive)
        (if (and (boundp 'rainbow-identifiers-mode)
                 (symbol-value rainbow-identifiers-mode))
            (progn
              (colors//tweak-theme-colors-font-lock t)
              (rainbow-identifiers-mode -1))
          (colors//tweak-theme-colors-font-lock)
          (rainbow-identifiers-mode)))
      (evil-leader/set-key "tCi" 'colors/toggle-rainbow-indentifiers)

      (add-to-hooks 'rainbow-identifiers-mode '(prog-mode-hook
                                                erlang-mode-hook))

      (defun colors//tweak-theme-colors-font-lock (&optional restore)
        "Nilify some font locks. If RESTORE in non nil the font locks are
 restored."
        ;; To make the variables stand out, keyword coloring is disabled
        (cond
         (restore
          (set-attributes-from-alist
           'font-lock-function-name-face original-font-lock-function-name-face-attributes)
          (set-attributes-from-alist
           'font-lock-keyword-face original-font-lock-keyword-face-attributes))
         (t
          (set-face-attribute 'font-lock-function-name-face nil
                              :foreground nil :slant 'normal :weight 'normal)
          (set-face-attribute 'font-lock-keyword-face nil
                              :foreground nil :slant 'normal :weight 'bold)))
        (font-lock-fontify-buffer))

      (defun colors//tweak-theme-colors (theme)
        "Tweak color themes by adjusting rainbow-identifiers colors settings an by
disabling some faces in order to make colored identifiers stand out."
        (interactive)
        ;; tweak the saturation and lightness of identifier colors
        (pcase theme
          (`leuven (setq rainbow-identifiers-cie-l*a*b*-saturation 100
                                  rainbow-identifiers-cie-l*a*b*-lightness 40))
          (`monokai (setq rainbow-identifiers-cie-l*a*b*-saturation 55
                          rainbow-identifiers-cie-l*a*b*-lightness 60))
          (`solarized-dark (setq rainbow-identifiers-cie-l*a*b*-saturation 65
                                 rainbow-identifiers-cie-l*a*b*-lightness 55))
          (`solarized-light (setq rainbow-identifiers-cie-l*a*b*-saturation 60
                                  rainbow-identifiers-cie-l*a*b*-lightness 55))
          (`zenburn (setq rainbow-identifiers-cie-l*a*b*-saturation 40
                          rainbow-identifiers-cie-l*a*b*-lightness 65))
          (_ (setq rainbow-identifiers-cie-l*a*b*-saturation 80
                   rainbow-identifiers-cie-l*a*b*-lightness 45)))
        ;; backup to original font locks
        (let ((frame (selected-frame)))
          (setq original-font-lock-function-name-face-attributes
                (face-all-attributes font-lock-function-name-face frame))
          (setq original-font-lock-keyword-face-attributes
                (face-all-attributes font-lock-keyword-face frame)))
        ;; tweak the font locks
        (colors//tweak-theme-colors-font-lock)))
    (colors//tweak-theme-colors spacemacs--cur-theme)

    (defadvice spacemacs/post-theme-init (after colors/post-theme-init activate)
      "Adjust lightness and brightness of rainbow-identifiers on post theme init."
      (colors//tweak-theme-colors spacemacs--cur-theme))

    :config
    (progn
      ;; functions to change saturation and lightness of colors
      (defun colors//change-color-mini-mode-doc (component)
        "Display a short documentation in the mini buffer."
        (let ((var (intern (format
                            "rainbow-identifiers-cie-l*a*b*-%s" component))))
          (echo "Change color %s mini-mode (value: %s)
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
      ;; key bindings
      (evil-leader/set-key "Cis" 'colors/start-change-color-saturation)
      (evil-leader/set-key "Cil" 'colors/start-change-color-lightness))))

(defun colors/init-rainbow-mode ()
  (use-package rainbow-mode
    :commands rainbow-mode
    :init (evil-leader/set-key "tCc" 'rainbow-mode)
    :config (spacemacs|hide-lighter rainbow-mode)))
