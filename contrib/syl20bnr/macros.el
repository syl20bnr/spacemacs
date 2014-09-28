(defmacro syl20bnr//rainbow-identifier-change-color-component (component
                                                               inc
                                                               reset)
  "Set a temporary overlay map to easily change a color COMPONENT from
 rainbow-identifier mode. The color COMPONENT can be 'saturation' or
 'lightness'. INC is the value to add to the COMPONENT. If RESET is not nil
 then INC is the new value of the COMPONENT."
  `(progn 
     (set-temporary-overlay-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "+")
          '(lambda () (interactive)
             (syl20bnr//rainbow-identifier-change-color-component-func
              ,component ,inc)))
        (define-key map (kbd "-")
          '(lambda () (interactive)
             (syl20bnr//rainbow-identifier-change-color-component-func
              ,component (- ,inc))))
        (define-key map (kbd "=")
          '(lambda () (interactive)
             (syl20bnr//rainbow-identifier-change-color-component-func
              ,component ,reset t)))
        map) t)
     (message ,(format "Type +/- to increase or decrease %s (= to reset)"
                       component))))

(defmacro syl20bnr//rainbow-identifier-change-color-component-func
  (component inc &optional reset)
  "Change the color component by adding INC value to it. If RESET is not nil
the color component is set to INC."
  (interactive)
  `(let* ((var (intern (format "rainbow-identifiers-cie-l*a*b*-%s"
                               ,component)))
          (new-value (+ (eval var) ,inc)))
     (if ,reset
         (set var ,inc)
       (progn
         (if (< new-value 0)
             (setq new-value 0))
         (set var new-value)))
     (message (format "Color %s: %s" ,component (eval var)))
     (font-lock-fontify-buffer)))
