(defvar syl20bnr-packages
  '(
    rainbow-identifiers
    )
  "List of all packages to install and/or initialize. Built-in packages
which require an initialization must be listed explicitly in the list.")

(defvar syl20bnr-excluded-packages '()
  "List of packages to exclude.")

(defun syl20bnr/init-rainbow-identifiers ()
  (use-package rainbow-identifiers
    :commands rainbow-identifiers-mode
    :init
    (progn 
      (setq rainbow-identifiers-choose-face-function 'rainbow-identifiers-cie-l*a*b*-choose-face
            rainbow-identifiers-cie-l*a*b*-saturation 100
            rainbow-identifiers-cie-l*a*b*-lightness 40
            ;; override theme faces
            rainbow-identifiers-faces-to-override '(highlight-quoted-symbol
                                                    font-lock-variable-name-face))
      (add-to-hooks 'rainbow-identifiers-mode '(prog-mode-hook
                                                erlang-mode-hook))

      (defun syl20bnr/tweak-theme-colors (theme)
        "Tweak color themes by adjusting rainbow-identifiers colors settings an by
disabling some faces in order to make colored identifiers stand out."
        (interactive)
        (pcase theme
          (`solarized-dark (setq rainbow-identifiers-cie-l*a*b*-saturation 85
                                 rainbow-identifiers-cie-l*a*b*-lightness 65))
          (`solarized-light (setq rainbow-identifiers-cie-l*a*b*-saturation 100
                                  rainbow-identifiers-cie-l*a*b*-lightness 40))
          (`monokai (setq rainbow-identifiers-cie-l*a*b*-saturation 55
                          rainbow-identifiers-cie-l*a*b*-lightness 60))
          (`zenburn (setq rainbow-identifiers-cie-l*a*b*-saturation 40
                          rainbow-identifiers-cie-l*a*b*-lightness 65))
          (_ (setq rainbow-identifiers-cie-l*a*b*-saturation 80
                   rainbow-identifiers-cie-l*a*b*-lightness 45)))
        ;; To make the variables stand out, keyword coloring is disabled
        ;; (set-face-attribute 'highlight-quoted-symbol nil
        ;;                     :foreground nil :slant 'normal :weight 'bold)
        (set-face-attribute 'font-lock-function-name-face nil
                            :foreground nil :slant 'normal :weight 'normal)
        (set-face-attribute 'font-lock-keyword-face nil
                            :foreground nil :slant 'normal :weight 'bold)
        (font-lock-fontify-buffer)))
    (syl20bnr/tweak-theme-colors spacemacs-cur-theme)

    (defadvice spacemacs/post-theme-init (after syl20bnr/post-theme-init activate)
      "Adjust lightness and brightness of rainbow-identifiers on post theme init."
      (syl20bnr/tweak-theme-colors spacemacs-cur-theme))

    :config
    (progn
      (evil-leader/set-key "tc" 'rainbow-identifiers-mode)

      ;; functions to change saturation and lightness of colors
      (defun syl20bnr/change-color-mini-mode-doc (component)
        "Display a short documentation in the mini buffer."
        (let ((var (intern (format
                            "rainbow-identifiers-cie-l*a*b*-%s" component))))
          (echo "Change color %s mini-mode (value: %s)
  + to increase %s
  - to decrease %s
  = to reset
Press any other key to exit." component (eval var) component component)))

      (defun syl20bnr/change-color-component-overlay-map (component)
        "Set a temporary overlay map to easily change a color COMPONENT from
 rainbow-identifier mode. The color COMPONENT can be 'saturation' or
 'lightness'."
        (set-temporary-overlay-map
         (let ((map (make-sparse-keymap))
               (up-func (intern (format "syl20bnr/change-color-%s-up" component)))
               (down-func (intern (format "syl20bnr/change-color-%s-down" component)))
               (reset-func (intern (format "syl20bnr/change-color-%s-reset" component))))
           (define-key map (kbd "+") up-func)
           (define-key map (kbd "-") down-func)
           (define-key map (kbd "=") reset-func)
           map) t)
           (syl20bnr/change-color-mini-mode-doc component)) 

      (defun syl20bnr/start-change-color-saturation ()
        "Initiate the overlay map to change the saturation."
        (interactive)
        (syl20bnr/change-color-component-overlay-map "saturation"))
      (defun syl20bnr/change-color-saturation-up ()
        "Increase the saturation by 5 units."
        (interactive)
        (syl20bnr/change-color-component-func "saturation" 5))
      (defun syl20bnr/change-color-saturation-down ()
        "Decrease the saturation by 5 units."
        (interactive)
        (syl20bnr/change-color-component-func "saturation" -5))
      (defun syl20bnr/change-color-saturation-reset ()
        "Reset the saturation to 100."
        (interactive)
        (syl20bnr/change-color-component-func "saturation" 100 t))
      (defun syl20bnr/start-change-color-lightness ()
        "Initiate the overlay map to change the lightness."
        (interactive)
        (syl20bnr/change-color-component-overlay-map "lightness"))
      (defun syl20bnr/change-color-lightness-up ()
        "Increase the lightness by 5 units."
        (interactive)
        (syl20bnr/change-color-component-func "lightness" 5))
      (defun syl20bnr/change-color-lightness-down ()
        "Decrease the lightness by 5 units."
        (interactive)
        (syl20bnr/change-color-component-func "lightness" -5))
      (defun syl20bnr/change-color-lightness-reset ()
        "Reset the lightness to 40."
        (interactive)
        (syl20bnr/change-color-component-func "lightness" 40 t))

      (defun syl20bnr/change-color-component-func
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
          (syl20bnr/change-color-component-overlay-map component)))
      ;; key bindings
      (evil-leader/set-key "cs" 'syl20bnr/start-change-color-saturation)
      (evil-leader/set-key "cl" 'syl20bnr/start-change-color-lightness))))

