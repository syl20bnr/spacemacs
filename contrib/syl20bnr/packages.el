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
                                                erlang-mode-hook)))
    :config
    (progn
      (syl20bnr/tweak-theme-colors 'solarized-light)
      ;; functions to change saturation and lightness of colors
      (defun syl20bnr/rainbow-identifier-change-color-component
        (component inc reset)
        "Set a temporary overlay map to easily change a color COMPONENT from
 rainbow-identifier mode. The color COMPONENT can be 'saturation' or
 'lightness'. INC is the value to add to the COMPONENT. If RESET is not nil
 then INC is the new value of the COMPONENT."
        (set-temporary-overlay-map
         (let ((map (make-sparse-keymap)))
           (define-key map (kbd "+")
             `(lambda () (interactive)
                (syl20bnr/rainbow-identifier-change-color-component-func
                 ,component ,inc)))
           (define-key map (kbd "-")
             `(lambda () (interactive)
                (syl20bnr/rainbow-identifier-change-color-component-func
                 ,component ,(- inc))))
           (define-key map (kbd "=")
             `(lambda () (interactive)
                (syl20bnr/rainbow-identifier-change-color-component-func
                 ,component ,reset t)))
           map) t)
        (message "Type +/- to increase or decrease %s (= to reset)" component))

      (defun syl20bnr/rainbow-identifier-change-color-component-func
        (component inc &optional reset)
        "Change the color component by adding INC value to it. If RESET is not nil
 the color component is set to INC."
        (interactive)
        (message component)
        (let* ((var (intern (format "rainbow-identifiers-cie-l*a*b*-%s"
                                    component)))
               (new-value (+ (eval var) inc)))
          (if reset
              (set var inc)
            (progn
              (if (< new-value 0)
                  (setq new-value 0))
              (set var new-value)))
          (message "Color %s: %s" component (eval var))
          (font-lock-fontify-buffer)))
      ;; key bindings
      (evil-leader/set-key "cs"
        '(lambda () (interactive)
           (syl20bnr/rainbow-identifier-change-color-component "saturation" 5 100)))
      (evil-leader/set-key "cl"
        '(lambda () (interactive)
           (syl20bnr/rainbow-identifier-change-color-component "lightness" 5 40)))
 
      )))

