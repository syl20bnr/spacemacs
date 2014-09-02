;; different method used than the documented one in order to speed up the
;; loading of emacs
(use-package solarized
  :init
  (progn 
    (deftheme solarized-light "The light variant of the Solarized colour theme")
    (create-solarized-theme 'light 'solarized-light)
    (deftheme solarized-dark "The dark variant of the Solarized colour theme")
    (create-solarized-theme 'dark 'solarized-dark)
    (set-flycheck-custom-face)
))

