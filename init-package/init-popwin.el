(use-package popwin
  :init
  (progn
    (popwin-mode 1)
    (push '("*grep*" :dedicated t :position bottom :stick t :noselect t) popwin:special-display-config)
    ;; use popwin with helm
    (push '("^\*helm.+\*$" :regexp t :position bottom) popwin:special-display-config)
    (push '("^\*helm-.+\*$" :regexp t :position bottom) popwin:special-display-config)))
