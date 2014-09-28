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
      (evil-leader/set-key "cs"
        '(lambda () (interactive)
           (syl20bnr//rainbow-identifier-change-color-component "saturation"
                                                                5 50)))
      (evil-leader/set-key "cl"
        '(lambda () (interactive)
           (syl20bnr//rainbow-identifier-change-color-component "lightness"
                                                                5 50)))
      (syl20bnr/tweak-theme-colors 'solarized-light))))

