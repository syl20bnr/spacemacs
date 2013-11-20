(use-package popwin
  :init
  (progn
    (setq display-buffer-function 'popwin:display-buffer)
    ;; use popwin with helm
    (push '("^\*helm.+\*$" :regexp t) popwin:special-display-config)
    (push '("^\*helm-.+\*$" :regexp t) popwin:special-display-config)))
