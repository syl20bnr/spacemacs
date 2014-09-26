(defun syl20bnr/tweak-theme-colors (theme)
  "Tweak color themes by adjusting rainbow-identifiers colors settings an by
disabling some faces in order to make colored identifiers stand out."
  (interactive)
  (pcase theme
    (`solarized-dark (setq rainbow-identifiers-cie-l*a*b*-saturation 60
                           rainbow-identifiers-cie-l*a*b*-lightness 50))
    (`solarized-light (setq rainbow-identifiers-cie-l*a*b*-saturation 100
                            rainbow-identifiers-cie-l*a*b*-lightness 40))
    (_ (setq rainbow-identifiers-cie-l*a*b*-saturation 80
             rainbow-identifiers-cie-l*a*b*-lightness 45)))
  ;; To make the variables stand out, keyword coloring is disabled
  ;; (set-face-attribute 'highlight-quoted-symbol nil
  ;;                     :foreground nil :slant 'normal :weight 'bold)
  (set-face-attribute 'font-lock-function-name-face nil
                      :foreground nil :slant 'normal :weight 'normal)
  (set-face-attribute 'font-lock-keyword-face nil
                      :foreground nil :slant 'normal :weight 'bold))
