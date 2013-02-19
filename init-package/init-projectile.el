(require 'projectile)

(dolist (mode '(python ruby))
(add-hook (intern (concat (symbol-name mode) "-mode-hook")) 'projectile-on))
