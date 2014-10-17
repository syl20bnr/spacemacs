(defvar syl20bnr-packages
  '(
    rainbow-identifiers
    )
  "List of all packages to install and/or initialized. Built-in packages
which require an initialization must be listed explicitly in the list.")

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
      (syl20bnr/tweak-theme-colors 'solarized-light)
      (evil-leader/set-key "tc" 'rainbow-identifiers-mode)

      ;; functions to change saturation and lightness of colors
      (defun syl20bnr/change-color-mini-mode-doc (component)
        "Display a short documentation in the mini buffer."
        (let ((var (intern (format
                            "rainbow-identifiers-cie-l*a*b*-%s" component))))
          (message "Change color %s mini-mode (value: %s)
  + to increase %s
  - to decrease %s
  = to reset
Press any other key to exit." component (eval var) component component)))

      (defun syl20bnr/change-color-component
        (component inc reset)
        "Set a temporary overlay map to easily change a color COMPONENT from
 rainbow-identifier mode. The color COMPONENT can be 'saturation' or
 'lightness'. INC is the value to add to the COMPONENT. If RESET is not nil
 then INC is the new value of the COMPONENT."
        (set-temporary-overlay-map
         (let ((map (make-sparse-keymap)))
           (define-key map (kbd "+")
             `(lambda () (interactive) (syl20bnr/change-color-component-func
                                        ,component ,inc)))
           (define-key map (kbd "-")
             `(lambda () (interactive) (syl20bnr/change-color-component-func
                                        ,component ,(- inc))))
           (define-key map (kbd "=")
             `(lambda () (interactive) (syl20bnr/change-color-component-func
                                        ,component ,reset t)))
           map) t)
        (syl20bnr/change-color-mini-mode-doc component))

      (defun syl20bnr/change-color-component-func
        (component inc &optional reset)
        "Change the color component by adding INC value to it. If RESET is not
 nil the color component is set to INC."
        (interactive)
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
          (syl20bnr/change-color-mini-mode-doc component)))
      ;; key bindings
      (evil-leader/set-key "cs"
        '(lambda () (interactive)
           (syl20bnr/change-color-component "saturation" 5 100)))
      (evil-leader/set-key "cl"
        '(lambda () (interactive)
           (syl20bnr/change-color-component "lightness" 5 40))))))

